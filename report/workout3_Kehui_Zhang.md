workout3\_Kehui\_Zhang
================
Kehui-Zhang
12/5/2019

``` r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE)
```

``` r
library(stringr)
library(xml2)
library(rvest)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(wordcloud)
```

    ## Loading required package: RColorBrewer

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(dataMeta)
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.2.1     ✔ readr   1.3.1
    ## ✔ tibble  2.1.3     ✔ purrr   0.3.3
    ## ✔ tidyr   1.0.0     ✔ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::annotate()     masks NLP::annotate()
    ## ✖ dplyr::filter()         masks stats::filter()
    ## ✖ readr::guess_encoding() masks rvest::guess_encoding()
    ## ✖ dplyr::lag()            masks stats::lag()
    ## ✖ purrr::pluck()          masks rvest::pluck()

``` r
library(ggplot2)
```

## Extract raw data

### 1\) Extract simple information of the authors

``` r
getwd()
```

    ## [1] "/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/report"

``` r
setwd("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/data/rawdata")
getwd()
```

    ## [1] "/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/data/rawdata"

``` r
Sys.sleep(15)

abhijit_banerjee_url <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/abhijit_banerjee_GoogleScholarCitations.html"
download.file(abhijit_banerjee_url,  'abhijit_banerjee.html')
ab <- read_html('abhijit_banerjee.html')

esther_duflo_url <-"https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/esther_duflo_GoogleScholarCitations.html"
download.file(esther_duflo_url,  'esther_duflo.html')
ed <- read_html('esther_duflo.html')
```

#### Extract the names of the scholars from the HTML object

``` r
ab_name <- ab %>%
  html_nodes(xpath = '//*[@id = "gsc_prf_i"]') %>%
  html_nodes(xpath = '//*[@id = "gsc_prf_in"]')%>%
  html_text()
ab_name
```

    ## [1] "Abhijit Banerjee"

``` r
ed_name <- ed %>%
  html_nodes(xpath = '//*[@id = "gsc_prf_i"]') %>%
  html_nodes(xpath = '//*[@id = "gsc_prf_in"]')%>%
  html_text()
ed_name
```

    ## [1] "Esther Duflo"

#### Extract the scholars’ affiliated institutions from the HTML object (NA if not specified)

``` r
ab_insti <- ab %>%
  html_nodes(xpath = '//*[@class = "gsc_prf_il"]') %>%
  html_nodes(xpath = '//*[@class = "gsc_prf_ila"]')%>%
  html_text()
if(identical(ab_insti, character(0))){
  ab_insti = NA
}
ab_insti
```

    ## [1] NA

``` r
ed_insti <- ed %>%
  html_nodes(xpath = '//*[@class = "gsc_prf_il"]') %>%
  html_nodes(xpath = '//*[@class = "gsc_prf_ila"]')%>%
  html_text()
if(identical(ed_insti, character(0))){
  ed_insti = NA
}
ed_insti
```

    ## [1] "MIT"

### 2\) Extract all the papers for each author

``` r
ab_papers <- ab %>%
  html_nodes(xpath = '//*[@id="gsc_a_b"]') %>%
  html_nodes(xpath = '//*[@class="gsc_a_tr"]') %>%
  html_nodes(xpath = '//*[@class="gsc_a_at"]') %>%
  html_text()
```

``` r
ed_papers <- ed %>%
  #html_nodes(xpath = '//*[@id="gsc_a_t"]') %>%
  html_nodes(xpath = '//*[@id="gsc_a_b"]') %>%
  html_nodes(xpath = '//*[@class="gsc_a_tr"]') %>%
  html_nodes(xpath = '//*[@class="gsc_a_at"]') %>%
  html_text()
```

#### Extract information of the two scholars and save the information in data frames

``` r
ab_info_link <- ab %>% html_nodes(xpath ='//*[@id="gsc_a_b"]') %>% 
  html_nodes(xpath = '//*[@class="gsc_a_tr"]') %>% html_nodes(xpath ='td') 
ab_result = sapply(html_children(ab_info_link), html_text)
ab_result = ab_result[ab_result != '*']

ab_citation_df = data.frame(paperName = ab_result[seq(1, length(ab_result), 5)],
                         researcher = ab_result[seq(2, length(ab_result), 5)],
                         journal = ab_result[seq(3, length(ab_result), 5)],
                         citations = ab_result[seq(4, length(ab_result), 5)],
                         year = ab_result[seq(5, length(ab_result), 5)])
ab_citation_df$citations = as.integer(ab_citation_df$citations)


ed_info_link <- ed %>% html_nodes(xpath ='//*[@id="gsc_a_b"]') %>% 
  html_nodes(xpath = '//*[@class="gsc_a_tr"]') %>% html_nodes(xpath ='td') 
ed_result = sapply(html_children(ed_info_link), html_text)
ed_result = ed_result[ed_result != '*']

ed_citation_df = data.frame(paperName = ed_result[seq(1, length(ed_result), 5)],
                         researcher = ed_result[seq(2, length(ed_result), 5)],
                         journal = ed_result[seq(3, length(ed_result), 5)],
                         citations = ed_result[seq(4, length(ed_result), 5)],
                         year = ed_result[seq(5, length(ed_result), 5)])
ed_citation_df$citations = as.integer(ed_citation_df$citations)
```

#### Write the data frames into csv files and save the files in the cleandata/ folder

``` r
setwd("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/data/cleandata")
getwd()
```

    ## [1] "/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/data/cleandata"

``` r
write.csv(ab_citation_df,'ab_citation_df.csv')
write.csv(ed_citation_df,'ed_citation_df.csv')
```

### 3\) Practice with Regular Expressions

#### a) For the two scholars, how many of their paper titles begin with a word that starts with a vowel, respectively?

``` r
length(ab_papers)
```

    ## [1] 495

``` r
ab_1st <- substr(ab_papers, 1,1)
vowels <- c('A', 'E', 'I', 'O', 'U')
sum(table(ab_1st[ab_1st %in% vowels]))
```

    ## [1] 118

``` r
length(ed_papers)
```

    ## [1] 491

``` r
ed_1st <- substr(ed_papers, 1,1)
vowels <- c('A', 'E', 'I', 'O', 'U')
sum(table(ed_1st[ed_1st %in% vowels]))
```

    ## [1] 116

#### The number of their paper titles that begins with a word starting with a vowel almost accounts for 25% of the total number of papers. This makes sense since the number of vowels also accounts for 25% of the total 26 letters. And we may conclude that the starting letter has nothing to do with vowels.

#### b) For the two scholars, how many of their paper titles end with “s” respectively?

``` r
ab_last <- str_sub(ab_papers,-1,-1)
sum(table(ab_last[ab_last %in% 's']))
```

    ## [1] 78

``` r
ed_last <- str_sub(ed_papers,-1,-1)
sum(table(ed_last[ed_last %in% 's']))
```

    ## [1] 74

#### The number of their paper titles end with “s” accounts for around 15% of the total number of their paper titles, which far more than the percent for which the letter “s” accounts of the total 26 letters–3%. I think one possible reason is that the letter “s” represents the concept of plural and usually the paper focus on a general group instead of one single thing.

#### c) For the two scholars, find the longest title, respectively (“longest” in terms of number of characters).

``` r
ab_longest <- data_frame(text = ab_citation_df$paperName) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest(cols = c(tokens)) %>% 
  count(text) %>% 
  arrange(desc(n)) 
ab_longest$text[1]
```

    ## [1] Page numbers followed by the letter f or t refer to figures or tables, respectively. Adam, S., 227, 228, 237, 250, 252 Adams, P., 164, 188, 188n2, 189, 189n4, 190, 191,193,194 …
    ## 475 Levels: ¿ Cuál es tu evidencia? ...

``` r
ed_longest <- data_frame(text = ed_citation_df$paperName) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest(cols = c(tokens)) %>% 
  count(text) %>% 
  arrange(desc(n)) 
ed_longest$text[1]
```

    ## [1] Up in smoke: the influence of household behavior on the long-run impact of improved cooking stoves
    ## 461 Levels: ‘Beating the Odds’ versus ‘Changing the Odds’: Poverty, Resilience, and Family Policy. ...

#### d) For the two scholars, calculate the variable “number of punctuation symbols in the their titles”. Display summary() statistics of these variables, and the corresponding histograms.

``` r
num_punc <- str_count(ab_papers,"[:punct:]")
summary(num_punc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00    0.00    1.00    1.64    2.00   21.00

``` r
hist(num_punc)
```

![](workout3_Kehui_Zhang_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

#### The above histogram of number of punctuations shows that in the paper titles, punctuations are rarely used. Because te titles are usually simple and clear.

#### e) Remove stop words(“the”, “a”, “an”, “and”, “in”, “if”, “but”), numbers and punctuations from the titles.

``` r
afterclean_ab_papers <- str_replace_all(ab_papers, "[:punct:]", "") %>%
  str_replace_all("[:digit:]", "") 
afterclean_ab_papers <- afterclean_ab_papers[!afterclean_ab_papers == ""]

afterclean_ed_papers <- str_replace_all(ed_papers, "[:punct:]", "") %>%
  str_replace_all("[:digit:]", "") 
afterclean_ed_papers <- afterclean_ed_papers[!afterclean_ed_papers == ""]
```

#### We need to remove these stop words from the paper titles because these words do not have any clear or real meaning. Then there is no need to extract these words. Similarly, a single number or punctuations cannot give any information to us. Therefore we need to remove them from the paper titles.

#### f) Excluding stop words, numbers and punctuations, what are the 10 most frequent words in scholar A’s titles?

``` r
library(tidyverse)
data_frame(text = afterclean_ab_papers) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest(cols = c(tokens)) %>% 
  count(tokens) %>% 
  filter(!tokens %in% stopwords()) %>% 
  filter(!tokens %in% "") %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n)) %>%
  head(10)
```

    ## # A tibble: 10 x 3
    ##    tokens          n    freq
    ##    <chr>       <int>   <dbl>
    ##  1 evidence       67 0.0219 
    ##  2 india          46 0.0150 
    ##  3 development    42 0.0137 
    ##  4 economics      38 0.0124 
    ##  5 dp             30 0.00980
    ##  6 economic       30 0.00980
    ##  7 poor           28 0.00914
    ##  8 theory         26 0.00849
    ##  9 growth         25 0.00816
    ## 10 randomized     24 0.00784

#### g) Excluding stop words, numbers and punctuations, what are the 10 most frequent words in scholar B’s titles?

``` r
data_frame(text = afterclean_ed_papers) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest(cols = c(tokens)) %>% 
  count(tokens) %>% 
  filter(!tokens %in% stopwords()) %>% 
  filter(!tokens %in% "") %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n)) %>%
  head(10)
```

    ## # A tibble: 10 x 3
    ##    tokens          n    freq
    ##    <chr>       <int>   <dbl>
    ##  1 evidence       98 0.0295 
    ##  2 india          53 0.0160 
    ##  3 randomized     52 0.0157 
    ##  4 economics      49 0.0148 
    ##  5 development    48 0.0144 
    ##  6 dp             36 0.0108 
    ##  7 policy         34 0.0102 
    ##  8 economic       32 0.00963
    ##  9 experiment     32 0.00963
    ## 10 education      29 0.00873

#### From above tables, we can know that the frequencies of “evidence” and “india” are the highest and “economic” and “economic” appears a lot in their paper titles. This means that these two professors might be interested in the same topic which may be the economics of india. And they would like to provide some evidence to prove their conclusions.

### 4)Data Visualizations

#### Excluding stop words, numbers and punctuations, create two wordclouds for all the titles of scholar A and B respectively. What’s your observation from the wordcloud plots?

``` r
setwd("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images")
png('abhijit_banerjee_image.png')
ab_papers_map <- Corpus(VectorSource(ab_papers))
ab_papers_map <- tm_map(ab_papers_map, removeNumbers)
ab_papers_map <- tm_map(ab_papers_map, removePunctuation)
ab_papers_map <- tm_map(ab_papers_map, content_transformer(tolower))
ab_papers_map <- tm_map(ab_papers_map,function(x)removeWords(tolower(x),stopwords()))
wordcloud(ab_papers_map,scale=c(8,.3), colors=brewer.pal(6,"Dark2"),random.order=FALSE)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
knitr::include_graphics('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/abhijit_banerjee_image.png')
```

![](/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/abhijit_banerjee_image.png)<!-- -->

``` r
png('esther_duflo_image.png')
ed_papers_map <- Corpus(VectorSource(ed_papers))
ed_papers_map <- tm_map(ed_papers_map, removeNumbers)
ed_papers_map <- tm_map(ed_papers_map, removePunctuation)
ed_papers_map <- tm_map(ed_papers_map, content_transformer(tolower))
ed_papers_map <- tm_map(ed_papers_map,function(x)removeWords(tolower(x),stopwords()))
wordcloud(ed_papers_map,scale=c(8,.3), colors=brewer.pal(6,"Dark2"),random.order=FALSE)
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
knitr::include_graphics('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/esther_duflo_image.png')
```

![](/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/esther_duflo_image.png)<!-- -->

#### Create a line plot that displays the number of the publications for the two scholars across years. What can you observe from the plot?

``` r
setwd("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images")
png('ab_ed_citations.png')
plot(ab_citation_df$citations,type = 'l', col = "red",ylab = "publications", 
   main = "publications across years")
lines(ed_citation_df$citations, col = "blue")
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
knitr::include_graphics('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/ab_ed_citations.png')
```

![](/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/images/ab_ed_citations.png)<!-- -->

#### We can see that the trends of the amount of publications for both scholars across years are almost the same. This probably means that they have a close cooperation in researching.

#### For each author, select 3 of the top 10 most frequently used words in his/her titles. With this set of five words, create a plot with timelines that show the trend (i.e. evolution) of each word over a period of 10 to 20 years (the more years the better, so that you can see how a specific word has become more or less popular over time).

``` r
## three words "evidence", "india" and "development" are chosen.

characters <- c("evidence", "india" , "development")
#table(ab_citation_df$year)
name <- names(table(ab_citation_df$year))[-1]

#ab_citation_df$paperName[ab_citation_df$year == 1987]
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

df <- data_frame(year = name)
df$year <-  as.double(df$year)  
ab_word_fre <- list()

for (j in c(1:length(characters))){
  counts <- c()
  
  for (i in c(1:length(name)) ){
    ab_counts <- data_frame(text = ab_citation_df$paperName[ab_citation_df$year == name[i]]) %>% 
    mutate(text = tolower(text)) %>% 
    mutate(tokens = str_split(text, "\\s+")) %>%
    unnest(cols = c(tokens)) %>% 
    filter(tokens == characters[j]) %>%
    count(tokens)
  
    if(is.integer0(ab_counts$n))
      counts <- c(counts, 0)
    else
      counts <- c(counts, ab_counts$n)
  }
  ab_word_fre[[j]]<- counts
  }

#counts <- counts[-1]
df <- mutate(df,evidence = ab_word_fre[[1]], india = ab_word_fre[[2]], development = ab_word_fre[[3]])


df <- df %>%
  select(year, evidence, india, development) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("red", "blue", "black"))
```

![](workout3_Kehui_Zhang_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
## three words "evidence", "india" and "development" are chosen.

characters <- c("evidence", "india" , "development")
#table(ab_citation_df$year)
name <- names(table(ed_citation_df$year))[-1]

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

df <- data_frame(year = name)
df$year <-  as.double(df$year)  
ed_word_fre <- list()

for (j in c(1:length(characters))){
  counts <- c()
  
  for (i in c(1:length(name)) ){
    ed_counts <- data_frame(text = ed_citation_df$paperName[ed_citation_df$year == name[i]]) %>% 
    mutate(text = tolower(text)) %>% 
    mutate(tokens = str_split(text, "\\s+")) %>%
    unnest(cols = c(tokens)) %>% 
    filter(tokens == characters[j]) %>%
    count(tokens)
  
    if(is.integer0(ed_counts$n))
      counts <- c(counts, 0)
    else
      counts <- c(counts, ed_counts$n)
  }
  ed_word_fre[[j]]<- counts
  }

#counts <- counts[-1]
df <- mutate(df,evidence = ed_word_fre[[1]], india = ed_word_fre[[2]], development = ed_word_fre[[3]])


df <- df %>%
  select(year, evidence, india, development) %>%
  gather(key = "variable", value = "value", -year)

ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("red", "blue", "black"))
```

![](workout3_Kehui_Zhang_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### According to the trend (i.e. evolution) of each word which is within the most frequently 10 words in the paper titles over a period of 10 to 20 years, we can see that they appear in the paper titles more frequently in recent years which probably shows that the reseachers are interested in them in recent years.

### 5\) Report

#### 1\) On average, which scholar has more co-authors?

``` r
ab_co_author <- str_split(ab_citation_df$researcher,", ", n= Inf) %>%
  unlist() %>%
  str_split( " ", n = 2) %>%
  lapply("[", 2) %>%
  tolower() %>%
  unlist()  %>% 
  unique()
ab_co_author <- ab_co_author[!is.na(ab_co_author)]
ab_co_author <- str_replace_all(ab_co_author, "banerjee","")
ab_co_author <- ab_co_author[!ab_co_author == ""]
length(ab_co_author)
```

    ## [1] 482

``` r
ed_co_author <- str_split(ed_citation_df$researcher,", ", n= Inf) %>%
  unlist() %>%
  str_split( " ", n = 2) %>%
  lapply("[", 2) %>%
  tolower() %>%
  unlist()  %>% 
  unique()
ed_co_author <- ed_co_author[!is.na(ed_co_author)]
ed_co_author <- str_replace_all(ed_co_author, "duflo","")
ed_co_author <- ed_co_author[!ed_co_author == ""]
length(ed_co_author)
```

    ## [1] 411

#### We know that the first scholar has 482 co-authors and the second one has 411 co-authors. Therefore the number of co-authors of the second scholar is more than the first one’s.

#### 2\) Do the two scholars have mutual friends(co-authors)? If yes, print the names of their friends

``` r
intersect(ab_co_author, ed_co_author)
```

    ##   [1] "glennerster"      "kinnan"           "cole"            
    ##   [4] "linden"           "besley"           "chandrasekhar"   
    ##   [7] "jackson"          "ghatak"           "qian"            
    ##  [10] "karlan"           "zinman"           "banerji"         
    ##  [13] "khemani"          "goldberg"         "osei"            
    ##  [16] "parienté"         "shapiro"          "galiani"         
    ##  [19] "mullainathan"     "kothari"          "hanna"           
    ##  [22] "olken"            "pande"            "deaton"          
    ##  [25] "munshi"           "postel-vinay"     "watts"           
    ##  [28] "lafortune"        "zwane"            "van dusen"       
    ##  [31] "pariente"         "null"             "miguel"          
    ##  [34] "rogoff"           "benabou"          "bertrand"        
    ##  [37] "hornbeck"         "chattopadhyay"    "bardhan"         
    ##  [40] "basu"             "berry"            "kannan"          
    ##  [43] "mukerji"          "shotland"         "keniston"        
    ##  [46] "singh"            "breza"            "kremer"          
    ##  [49] "chassang"         "kenniston"        "imbert"          
    ##  [52] "mathew"           "mukherji"         "acemoglu"        
    ##  [55] "johnson"          "barnhardt"        "walton"          
    ##  [58] "gueron"           "athey"            "imbens"          
    ##  [61] "field"            "khwaja"           "angrist"         
    ##  [64] "card"             "udry"             "abhijit"         
    ##  [67] "dufio"            "delacourte"       "dastidar"        
    ##  [70] "finkelstein"      "aaron"            "robinson"        
    ##  [73] "rajasekaran"      "reyes"            "pan"             
    ##  [76] "zaff"             "donlan"           "ungar"           
    ##  [79] "adams"            "lillrank"         "abel-smith"      
    ##  [82] "aherne"           "whelton"          "balkenhol"       
    ##  [85] "akerlof"          "alamgir"          "armendariz"      
    ##  [88] "morduch"          "rabie"            "towfighian"      
    ##  [91] "clark"            "cammett"          "bai"             
    ##  [94] "zhang"            "fei"              "zhao"            
    ##  [97] "wang"             "abramovitz"       "aigner"          
    ## [100] "aboagye"          "román cedillo"    "abdulgani"       
    ## [103] "abu bakar"        "abdelal"          "abouharb"        
    ## [106] "cingranelli"      "konomi"           "dattilo"         
    ## [109] "akbulut-yuksel"   "bernanke"         "ahmed"           
    ## [112] "shuster"          "donaldson"        "abdel aziz"      
    ## [115] "berg"             "abreu"            "mendes"          
    ## [118] "ben-david"        "pezzini"          "juma"            
    ## [121] "rocca"            "rieff"            "grada"           
    ## [124] "abaluck"          "abbring"          "ameriks"         
    ## [127] "azzi"             "bach"             "baker"           
    ## [130] "becker"           "chen"             "e. adenutsi"     
    ## [133] "ahortor"          "acosta"           "calderon"        
    ## [136] "fajnzylber"       "lopez"            "chavan"          
    ## [139] "adenutsi"         "lartey"           "mandelman"       
    ## [142] "diamond"          "kounouwewa"       "chao"            
    ## [145] "almeida"          "carneiro"         "aryeetey"        
    ## [148] "adelman"          "alesina"          "barro"           
    ## [151] "morris"           "emilson adenutsi" "adams jr"        
    ## [154] "page"             "snyder"           "mashwama"        
    ## [157] "baird"            "hicks"            "learning"        
    ## [160] "david"            "aker"             "mbiti"           
    ## [163] "pischke"          "auerbach"         "mostagir"        
    ## [166] "ozdaglar"         "laibson"          "list"            
    ## [169] "anderson"         "wurgler"          "basker"          
    ## [172] "paris"            "maruani"          "meron"           
    ## [175] "duggan"           "mccleary"         "bel"             
    ## [178] "galeotti"         "rogers"           "werker"          
    ## [181] "durbin"           "michael"

#### They have many mutual friends(co-authors). This might because these scholars are interested in the same field and they would like to work together.

#### 3\) Did the two scholars once publish a paper together? If yes, please print the related information of that paper.

``` r
b <- c("banerjee")
d <- c("duflo")  
 df <- mutate(ab_citation_df, coauthor = tolower(ab_citation_df$researcher)) %>% 
  unnest(cols = c(coauthor)) 
 
co_paper <- data_frame(paper = df$paperName[grepl(b,df$coauthor) & grepl(d,df$coauthor)], author = df$coauthor[grepl(b,df$coauthor) & grepl(d,df$coauthor)])
co_paper
```

    ## # A tibble: 186 x 2
    ##    paper                                      author                       
    ##    <fct>                                      <chr>                        
    ##  1 Poor economics: A radical rethinking of t… av banerjee, a banerjee, e d…
    ##  2 The miracle of microfinance? Evidence fro… a banerjee, e duflo, r glenn…
    ##  3 The economic lives of the poor             av banerjee, e duflo         
    ##  4 Inequality and growth: What can the data … av banerjee, e duflo         
    ##  5 Remedying education: Evidence from two ra… av banerjee, s cole, e duflo…
    ##  6 Growth theory through the lens of develop… av banerjee, e duflo         
    ##  7 The diffusion of microfinance              a banerjee, ag chandrasekhar…
    ##  8 What is middle class about the middle cla… av banerjee, e duflo         
    ##  9 Do firms want to borrow more? Testing cre… av banerjee, e duflo         
    ## 10 Reputation effects and the limits of cont… av banerjee, e duflo         
    ## # … with 176 more rows

#### Now we know that they published papers together. And the number of the papers published by both of them are not small.

#### 4\) What’s the paper with the most co-authors?

``` r
  mutate(ab_citation_df, tokens = str_split(tolower(ab_citation_df$researcher), ", ")) %>%
  unnest(cols = c(tokens)) %>% 
  count(paperName) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n)) %>%
  head(1)
```

    ## # A tibble: 1 x 3
    ##   paperName                     n   freq
    ##   <fct>                     <int>  <dbl>
    ## 1 American Economic Journal    20 0.0113

#### According to the result we know that there are a total of 20 co-authors who paticipated in wrting this paper and researching.

#### 5\) Use regular expression to count the number of pages for each article (exclude books).

``` r
ab_df <- mutate(ab_citation_df, tokens = str_split(tolower(ab_citation_df$journal), ", ")) %>%
  unnest(cols = c(tokens))
ab_page <-ab_df$tokens[!grepl("[:digit:]", ab_df$tokens)] 
ab_page <- ab_page[grepl("-", ab_page)]

ab_num_page2 <- str_split(ab_page, "-") %>%
   lapply("[", 2) %>%
  as.numeric()

ab_num_page1 <- str_split(ab_page, "-") %>%
   lapply("[", 1) %>%
  as.numeric()

ab_num_page <- abs(ab_num_page2 - ab_num_page1)
ab_num_page <- ab_num_page[!is.na(ab_num_page)]

ab_num_page   
```

    ##   [1]   20   24   31   27   32   29   23   79   24   43   25   29   35   28
    ##  [15]   41   20   27   29   19   27   21   38   10   29   25   24   15   24
    ##  [29]    8 1166   19    4   14   18   13   23   32   52   27   17    4   14
    ##  [43]   39    5   22    5   19   11   50   13   16   38  396   29   37   55
    ##  [57]   41  295   56   19   16   19  194   15    4   29   29   45    2   10
    ##  [71]   35  169   40  496   33   26    7   30    8    6   34    5   20   37
    ##  [85]    5   34   23    1    2   27    1   21   42   14   27   19    4    8
    ##  [99]    4   29    3   41   24   29   23   44    1    6    3   96   36   18
    ## [113]   29    3   23   32   24    5   10    2   12   17   35   53   38    3
    ## [127]    1    6    2   15    5   17   18    1    9   10    0    0    4    3
    ## [141]   25    1    3    1   17   24   22   24

``` r
ed_df <- mutate(ed_citation_df, tokens = str_split(tolower(ed_citation_df$journal), ", ")) %>%
  unnest(cols = c(tokens))
ed_page <-ed_df$tokens[!grepl("[:digit:]", ed_df$tokens)] 
ed_page <- ed_page[grepl("-", ed_page)]

ed_num_page2 <- str_split(ed_page, "-") %>%
   lapply("[", 2) %>%
  as.numeric()

ed_num_page1 <- str_split(ed_page, "-") %>%
   lapply("[", 1) %>%
  as.numeric()

ed_num_page <- abs(ed_num_page2 - ed_num_page1)
ed_num_page <- ed_num_page[!is.na(ed_num_page)]

ed_num_page
```

    ##   [1]   26   18   31   24   27   34  972   67   32   27   29   79 2260   35
    ##  [15] 1665   43   25   35   27   28    4 1163   27   45  394   29   26    5
    ##  [29]   49   15   35   31    4   73 2660   18   34   84   39   13   44   14
    ##  [43]   39    5    7    5  289   46   19   34   28   58   53   10   55   32
    ##  [57]   19   25  194    5   29   10   16   35  196  496   26    4   37    7
    ##  [71]    8    6   14   20   37   34   41   17    7    1    3   21    8  295
    ##  [85]   27 2933   27   19   27    1    8   55   29   41   76   27   23    7
    ##  [99]    1    3   24  585    2    1    3   29   27  586    5   10    2  259
    ## [113]  595   12   17   12   10   53  697    2    1    3    1  597   25    0
    ## [127]   38    3    1    4    3    3   22    9   18  471    5   18   10   41
    ## [141]   18    0    0   30   24

#### 7\) How many distinct journals are there in the two citation tables?

``` r
str_split(ab_citation_df$journal, ", ") %>%
  lapply("[", 1) %>%
  str_replace_all("[:digit:]", "") %>%
  str_replace_all("[:punct:]", "") %>%
  unique() %>%
  length()
```

    ## [1] 264

``` r
str_split(ed_citation_df$journal, ", ") %>%
  lapply("[", 1) %>%
  str_replace_all("[:digit:]", "") %>%
  str_replace_all("[:punct:]", "") %>%
  unique() %>%
  length()
```

    ## [1] 250

#### There are 264 distinct journals in the scholar A’s citation table while 250 distinct journals in the scholar B’s citation table.
