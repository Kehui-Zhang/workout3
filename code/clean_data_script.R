### Construct a data frame for abhijit_banerjee

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

setwd("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout3/data/cleandata")
getwd()
write.csv(ab_citation_df,'ab_citation_df.csv')