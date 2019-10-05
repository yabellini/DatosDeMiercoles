#Librerias

library(purrr)
library(dplyr)
library(tidyr)
library(wordcloud2)
library(tidytext)
library(cld2)
library(cld3)
library(ggplot2)
library(forcats)


#Cargo los datos

tweets_latinr_conf <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-02/tweets_latinr_conf.csv")
tweets_latinr <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-02/tweets_latinR.csv")

#preparo un listado de stopwords en varios idiomas para sacarlos de la nube de palabras
# No tengo idea si esto se puede hacer de otra manera más sencilla, pero funciona

idiomas <- list('spanish', 'portuguese', 'french', 'danish', 'dutch', 'finnish', 'german', 'hungarian', 'italian', 'norwegian', 'russian', 'swedish')

variosIdiomas_stop_words <- idiomas %>% 
  map(tm::stopwords) %>%
  flatten_dfc() %>% 
  gather(var, word) %>%
  bind_rows(stop_words) %>%
  select(word, lexicon)



# tomo el texto de los tweets y los separo en palabras 
# y saco los stop_words en varios idiomas 
# y me quedo con todas las palabras que tienen al menos 5 ocurrencias


names <- tweets_latinr_conf %>% 
  select(tweet)

#Saco los emojis
names <-  gsub("[^\x01-\x7F]", "", names$tweet)
names <- as_tibble(names)

#Saco las URL
names <-  gsub("http*.*", "", names$value)
names <- as_tibble(names)

#Saco los links a fotos
names <-  gsub("pic.twitter*.*", "", names$value)
names <- as_tibble(names)

names <- names %>%  
  unnest_tokens(word, value) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>5)

wordcloud2(names, size = .5, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor ="#223564")

names <- names %>%  
  unnest_tokens(word, value) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>5)%>%
  filter(word != 'latinr2019')

wordcloud2(names, size = .5, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor ="#223564")


names %>%
  filter(n>5) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(y=n, x=word, fill="#223564")) +
  geom_col(position = 'stack', width = .7) + 
  coord_flip() +
  xlab("cantidad apariciones") + ylab("Palabras")


#Lo mismo pero con el hastag


names <- tweets_latinr %>% 
  select(tweet)

#Saco los emojis
names <-  gsub("[^\x01-\x7F]", "", names$tweet)
names <- as_tibble(names)

#Saco las URL
names <-  gsub("http*.*", "", names$value)
names <- as_tibble(names)

#Saco los links a fotos
names <-  gsub("pic.twitter*.*", "", names$value)
names <- as_tibble(names)

names <- names %>%  
  unnest_tokens(word, value) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>5)

wordcloud2(names, size = .5, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor ="#223564")


#Ranking de palabras

names %>%
  filter(n>10) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(y=n, x=word, fill="#223564")) +
  geom_col(position = 'stack', width = .7) + 
  coord_flip() +
  xlab("cantidad apariciones") + ylab("Palabras")


names <- names %>%  
  unnest_tokens(word, value) %>%
  anti_join(variosIdiomas_stop_words) %>%
  count(word) %>%
  filter(n>10)%>%
  filter(word != 'latinr2019')

wordcloud2(names, size = .5, minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-light", backgroundColor ="#223564")


