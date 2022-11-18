#Cargamos las librerías
 
library(tidyverse)
 
#Descargo la bbss

meli <- read_csv("listado_mercado_libre_.csv")

glimpse(meli)

meli %>% count("Articulos")

#Voy a limpiar toda la columna de artículo

library(janitor)
meli$Articulos <- make_clean_names(meli$Articulos)

library("stringr")

# consider a string "Hello Geek"
# replace the character 'e' in  "Hello Geek" 
# with "E"

meli$Articulos <- str_replace_all( meli$Articulos,"_", " ") 

#Voy a quedarme solamente con aquellos articulos que dicen calor

aire_caliente <- meli %>% 
  filter(str_detect(Articulos, "calor")) %>% 
  filter(!str_detect(Articulos, "frio"))

aire_frio <- meli %>% 
  filter(str_detect(Articulos, "frio")) %>% 
  filter(!str_detect(Articulos, "calor"))


aire_frio_calor <- meli %>% 
  filter(str_detect(Articulos, "frio")) %>% 
  filter(str_detect(Articulos, "calor"))

aire_ni_ni <- meli %>% 
  filter(!str_detect(Articulos, "frio")) %>% 
  filter(!str_detect(Articulos, "calor"))

aire_split <- meli %>% 
  filter(str_detect(Articulos, "split"))

aire_inverter <- meli %>% 
  filter(str_detect(Articulos, "inverter"))

aire_no_inverter <- meli %>% 
  filter(!str_detect(Articulos, "inverter"))

aires <- rbind(aire_caliente %>% 
                     mutate(Tipo = "Caliente"),
               aire_frio %>% 
                     mutate(Tipo = "Frio"),
               aire_frio_calor %>% 
                     mutate(Tipo = "Frio y Calor"),
               aire_ni_ni %>% 
                 mutate(Tipo = "Sin aclaración"))

aires_inverter <- rbind(aire_inverter %>% 
                 mutate(Tipo = "Inverter"),
                 aire_no_inverter %>% 
                 mutate(Tipo = "No Inverter"))

#Quiero saber el precio promedio según el tipo

aires$Precios = as.integer(aires$Precios)

precio_tipo <- aires_inverter %>%
  mutate(Precios = as.integer(Precios)) %>% 
  na.omit(Precios) %>% 
  group_by(Tipo) %>%
  summarise_at(vars(Precios), list(name = mean))

max(aire_inverter$Precios)

aire_inverter %>%
  na.omit(Precios) %>% 
  mean(Precios, na.rm = TRUE)

#Frecuencia de palabras en los títulos de los artículos

my_stopwords <- read_csv("~/Documents/Ciencia de datos/R-Projects/Clases/my_stopwords.csv")

word_freq <- aires %>% unnest_tokens(word, Articulos, drop = TRUE) %>%
  anti_join(my_stopwords) %>%
  count(word, sort = TRUE)%>% 
  top_n(20)

       