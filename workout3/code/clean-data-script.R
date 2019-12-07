library(rvest)
library(xml2)
library(dplyr)
library(stringr)

#########################################################
## Section 1 Extract simple information of the authors ##
#########################################################
citation_page_1 <- read_html("/home/chaoyoung/Desktop/workouts/workout3/data/rawdata/abhijit_banerjee_GoogleScholarCitations.html")
# extract names
str_extract(citation_page_1, pattern = "[A-Z][a-z][A-Z]?[a-z]+ [A-Z]'?[A-Z]?[a-z]+")
# extract affiliated institutions
str_match(citation_page_1, pattern = "MIT")

citation_page_2 <- read_html("/home/chaoyoung/Desktop/workouts/workout3/data/rawdata/esther_duflo_GoogleScholarCitations.html")
# extract name 
str_extract(citation_page_2, pattern = "[A-Z][a-z][A-Z]?[a-z]+ [A-Z]'?[A-Z]?[a-z]+")
# extract affiliated institutions
str_match(citation_page_1, pattern = "MIT")

######################################################
## Section 2 Extract all the papers for each author ##
######################################################
# citation table of Abhijit Banerjee
info_link <- citation_page_1 %>% html_nodes(xpath ='//*[@id="gsc_a_b"]') %>% 
  html_nodes(xpath ='tr') %>% html_nodes(xpath ='td') 
result = sapply(html_children(info_link), html_text)
result = result[result != '*']

citation_df_1 = data.frame(article_title = result[seq(1, length(result), 5)],
                         author = result[seq(2, length(result), 5)],
                         journal = result[seq(3, length(result), 5)],
                         citations = result[seq(4, length(result), 5)],
                         year = result[seq(5, length(result), 5)])

# citation table of Esther Duflo
info_link <- citation_page_2 %>% html_nodes(xpath ='//*[@id="gsc_a_b"]') %>% 
  html_nodes(xpath ='tr') %>% html_nodes(xpath ='td') 
result = sapply(html_children(info_link), html_text)
result = result[result != '*']

citation_df_2 = data.frame(article_title = result[seq(1, length(result), 5)],
                          author = result[seq(2, length(result), 5)],
                          journal = result[seq(3, length(result), 5)],
                          citations = result[seq(4, length(result), 5)],
                          year = result[seq(5, length(result), 5)])

# write the data.frame as csv
write.csv(citation_df_1, '/home/chaoyoung/Desktop/workouts/workout3/data/cleandata/A.B_citations.csv', row.names = FALSE)
write.csv(citation_df_2, '/home/chaoyoung/Desktop/workouts/workout3/data/cleandata/E.D_citations.csv', row.names = FALSE)

# create the data dictionary
df <- data.frame(name = c('article_titile', 'author', 'journal', 'citations', 'year'),
                 description = c('the title of each article', 'the names of authors', 'the journal published on', 'the number of citations', 'the year when paper published'),
                 data_type = c("character", "character", "character", "integer","integer"))

write.csv(df, '/home/chaoyoung/Desktop/workouts/workout3/data/cleandata/data_dictionary.csv', row.names = FALSE)


