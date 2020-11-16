# Prueba análisis de texto, seccion reforzamiento.

# Integrantes: Ramón Yañez, Patricio Zapata.


# Librerias requeridas ----------------------------------------------------

if(!require(epubr)) install.packages("epubr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(patchwork)) install.packages("patchwork")


library(epubr)
library(stringr)
library(tidyverse)
library(dplyr)
library(readr)
library(tidytext)
library(ggplot2)




# Funciones personalizadas para trabajo con texto --------------------------

#' limpiatexto: limpa el texto recibido de caracteres extraños.
#' 
#' @param texto el texto a limpiar.
#' @return el texto limpio.
#' limpiatexto(el_hombre_libro)
limpiatexto <- function(texto) {
  resultadotxt <-  texto %>% 
    str_remove_all("\\\\n{1,}") %>% 
    str_remove_all("`C.+=") %>% 
    str_remove_all("c\\(") %>% 
    str_remove_all("\\)") %>% 
    str_replace_all( "\\n", " ") %>% 
    str_replace_all( "\\«", " ") %>% 
    str_replace_all( "\\»", " ") %>% 
    str_remove_all("\\\\") %>% 
    str_remove_all("\\[+[:digit:]+\\]") %>%
    str_remove_all("[:digit:]+[:punct:]") %>% 
    str_remove_all("[:digit:][:space:]{0,}") %>%
    str_remove_all("<<") %>% 
    str_remove_all("<<\n") %>% 
    str_remove_all('"') %>%
    str_replace_all("[:space:]{2,}", " ")
 
  resultadotxt
}


#' freq_palabras_libros: saca frecuencias de palabras y elimina las stopwords.
#' 
#' @param libro el texto a trabajar.
#' @param stopwords las palabras no deseadas a limpiar.
#' @return tibble con frecuencias.
#' freq_palabras_libros(el_hombre_libro,all_stopwords)
freq_palabras_libros <- function(libro, stopwords) {
  # Se pasa el texto a una tibble con palabras y se cuenta la freq  --------
  libros_por_freq <- tibble(texto = libro) %>% 
    unnest_tokens(input = texto, output = palabra, strip_numeric = TRUE) %>% 
    count(palabra, sort = TRUE)
  
  # Se quitan los stopwords.
  libros_quitando_palabras <- libros_por_freq %>% 
    anti_join(stopwords)
  
  # libros_quitando_palabras <- libros_quitando_palabras %>% 
  #   slice_head(n=2000)
  
  libros_quitando_palabras
}


#' bigramas_libros: Obtiene bigramas del texto entregado.
#' 
#' @param libro_texto el texto a trabajar.
#' @return bigrama de palabras en tibble.
#' bigramas_libros(el_hombre_tibble)
bigramas_libros <- function(libro){
  
  libro_tibble <-  tibble(entrada = libro)
  bigrama_salida <-  libro_tibble %>% 
    unnest_tokens(input = entrada,
                  output = palabra,
                  token = "ngrams",
                  n = 2) %>% 
    filter(!is.na(palabra)) %>% 
    count(palabra, sort = TRUE) %>% 
    separate(col= palabra, 
             into = c("palabra_1","palabra_2"),
             sep = " ") %>% 
    filter(!palabra_1 %in% all_stopwords$palabra) %>% 
    filter(!palabra_2 %in% all_stopwords$palabra)
  
  bigrama_salida
}


#' grafica_frecuencias: Grafica los 15 primeros términos de las frecuencia de palabras de entrada
#' 
#' @param texto_tibble el texto a trabajar.
#' @param titulo título del grafico el texto a trabajar.
#' @return grafico de frecuencias.
#' grafica_frecuencias(el_hombre_tibble, "el hombre en busca de sentido")
grafica_frecuencias <- function(texto_tibble, titulo){
  texto_tibble %>% 
    slice_head(n=15) %>% 
    ggplot(aes(y= reorder(palabra,n),n)) +
    geom_col(fill = "#0057e7") + 
    geom_text(aes(label=n)) +
    labs(y=NULL,
         x="Frecuencia",
         title = titulo,
         caption = "Fuente: analisis propio.") +
    theme_minimal()
  
}


# Lectura de los textos formato EPUB --------------------------------------


# LEEMOS el EPUB El Hombre En Busca De Sentido
el_hombre <-  epub("ElHombreEnBuscaDeSentido.epub")

# Verificamos que sea los capitulos correctos del epub los que leeremos
#View(el_hombre$data[[1]])

# tomaremos de la sección 1 a la 5, esto es las filas 2 a la 6
el_hombre_texto <- paste(el_hombre$data[[1]][2:6,2], collapse = " ")



# LEEMOS el EPUB El Elemento
el_elemento <- epub("ElElemento.epub")

# Verificamos que es la info del libro sin agregar cosas demás
#View(el_elemento$data[[1]])

# tomaremos del capitulo 1 al 11, esto es filas 10 a 22
el_elemento_texto <- paste(el_elemento$data[[1]][10:22,2],collapse = " ")



# LEEMOS el EPUB Padre rico, padre pobre
padre_rico <-  epub("PadreRicoPadrePobre.epub")

# Verificamos que sea los capitulos correctos del epub los que leeremos
#View(padre_rico$data[[1]])

# tomaremos del capitulo 1 al 9 (filas 3 a la 11)
padre_rico_texto <- paste(padre_rico$data[[1]][3:11,2], collapse = " ")



# Limpiando los textos --------------------------------------

el_hombre_texto <- limpiatexto(el_hombre_texto)

el_elemento_texto <- limpiatexto(el_elemento_texto)

padre_rico_texto <- limpiatexto(padre_rico_texto)



# Preparamos para llamar a funcion de frecuencia de palabras y realizar antijoin --------

# 1:
# Se descargan algunas stopwords desde la web.
stopwords_web <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

# stopword personalizado 
# mis_stopwords <- tibble(palabra = c("va", "tenemos","value","hace", "hacer", "ser", "puede", "tiene", "dice")) # colocar mas en caso de...
mis_stopwords <- tibble(palabra = c("va", "tenemos","value","hace", "hacer", "ser",
                                    "puede", "tiene", "dice","paul","gillian","igualmente",
                                    "liverpool","mediante","londres","sesenta")) 

# unimos
all_stopwords <- bind_rows(stopwords_web, mis_stopwords)

# 2:
# llamamos al método que saca stopwords y les da frecuencia a las palabras del texto.
# Luego graficamos estas frecuencias

freq_el_hombre_texto <- freq_palabras_libros(el_hombre_texto,all_stopwords)
grafica_frecuencias(freq_el_hombre_texto, "El hombre en busca de sentido")

freq_el_elemento_texto <- freq_palabras_libros(el_elemento_texto,all_stopwords)
grafica_frecuencias(freq_el_elemento_texto, "El elemento")

freq_padre_rico_texto <- freq_palabras_libros(padre_rico_texto,all_stopwords)
grafica_frecuencias(freq_padre_rico_texto,"Padre rico padre pobre")


################ BIGRAMAS ################

bigramas_libros(el_hombre_texto)

bigramas_libros(el_elemento_texto)

bigramas_libros(padre_rico_texto)



## ¿Los resultados son los que esperaban? 
# Si. AL mirar los bigramas y relacionarlos con el autor de cada libro, corresponden con las vivencias y entorno de cada autor. 

## ¿Hay algo que les haya llamado la atención?
# Los libros Padre rico y el elemento son parecidos entre si, en la perspectiva de crecimiento personal y económica,
# mientras que "el hombre en busca de sentido" toca temas vividos en un campo de concentración y el sentido a la vida que encontró en ello.
# Si bien los 3 títulos caen en la categoría autoayuda, los bigramas demuestran que lo hacen de maneras distintas




# Hacemos el análisis TF-IDF

#Creamos la columna de libro
freq_el_hombre_texto <- freq_el_hombre_texto %>% 
  mutate(libro = 'El_hombre_en_busca_de_sentido', .before = palabra) 

freq_el_elemento_texto <- freq_el_elemento_texto %>% 
  mutate(libro = 'El_elemento', .before = palabra)

freq_padre_rico_texto <- freq_padre_rico_texto %>% 
  mutate(libro = 'Padre_rico_padre_pobre', .before = palabra)


# Juntamos libros en un solo DF
# NOTA: Como el texto de "el hombre en busca de sentido" dista mucho respecto de los otros dos 
# (ya que es una historia de vida que se basa en un campo de concentración),
# se dejará fuera para realizar este análisis TF-IDF. 
# Así podemos ver dos libros mas o menos similares y ver sus diferencias

libros <- bind_rows(freq_el_elemento_texto, freq_padre_rico_texto)

# Generamos el análisis TF-IDF
libros_tfidf <- bind_tf_idf(libros,
                               term = palabra,
                               document = libro,
                               n= n)



libros_tfidf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(libro) %>% 
  slice_head(n=15) %>% 
  ggplot(aes(y=reorder(palabra,tf_idf),x= tf_idf, fill= libro)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~libro, scales = "free") +
  labs(y=NULL) +
  scale_x_continuous(labels = scales::comma)

# En este gráfico se aprecia claramente que "el elemento" se basa mucho en temas
# orientados a la educación. En cambio "padre rico padre pobre" apunta a temas
# financieros y como mover el dinero.









# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------








# Complemento de la prueba.


# Solo como método comparativo y en vista de las conclusiones, agregaremos 
# un libro más a comparar (tus zonas erróneas) el cual, tendería a ser mas parecido al elemento.

# Entonces, hacemos los mismos procedimiento anteriores:



# LEER EPUB
zonas_erroneas <-  epub("Tuszonaserroneas.epub")
View(zonas_erroneas$data[[1]])

# tomaremos de la sección de capitulos 1 a la 12, esto es las filas 2 a la 13
zonas_erroneas_texto <- paste(zonas_erroneas$data[[1]][2:13,2], collapse = " ")

# limpiamos el texto --
zonas_erroneas_texto <- limpiatexto(zonas_erroneas_texto)

#  sacamos stopwords y graficamos

freq_zonas_erroneas_texto <- freq_palabras_libros(zonas_erroneas_texto,all_stopwords)
grafica_frecuencias(freq_zonas_erroneas_texto, "Tus zonas erroneas")

bigramas_libros(zonas_erroneas_texto)


freq_zonas_erroneas_texto <- freq_zonas_erroneas_texto %>% 
  mutate(libro = 'Tus_zonas_erroneas', .before = palabra)

# comparamos el elemento con tus zonas erróneas:


libros2 <- bind_rows(freq_el_elemento_texto, freq_zonas_erroneas_texto)

# Generamos el análisis TF-IDF
libros2_tfidf <- bind_tf_idf(libros2,
                            term = palabra,
                            document = libro,
                            n= n)



libros2_tfidf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(libro) %>% 
  slice_head(n=15) %>% 
  ggplot(aes(y=reorder(palabra,tf_idf),x= tf_idf, fill= libro)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~libro, scales = "free") +
  labs(y=NULL) +
  scale_x_continuous(labels = scales::comma)

# Con esta nueva comparación, podemos notar que los libros se diferencian en que el elemento 
# muestra quehaceres o verbos de acción (como tocar, baile, escritura, aptitudes), en los 
# cuales se busca lo que a uno le apasiona en la vida, 
# en cambio las zonas erróneas se basa en sentimientos negativos (como eliminarlos) 
# como la ira, culpabilidad, rabia).