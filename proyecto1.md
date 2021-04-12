Untitled
================

``` r
library(readxl)
library(datasets)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
```

    ## Package version: 2.1.2

    ## Parallel computing: 2 of 8 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(ggplot2)
library(utf8)




# como mirar las hojas de tu excel
ruta_excel <- "/Users/misaelfernandez/Desktop/sanguchez.xlsx"

#como mirar las hojas de tu excel 
dato_excel <-read_excel(ruta_excel)
```

    ## New names:
    ## * url -> url...1
    ## * url -> url...2
    ## * url -> url...3
    ## * url -> url...4
    ## * url -> url...5

``` r
#crear tabla nueva
datos_analisis <-dato_excel[,5:6]

datos_analisis <- as.data.frame(datos_analisis)
attach(datos_analisis)

str(datos_analisis)
```

    ## 'data.frame':    410 obs. of  2 variables:
    ##  $ url...5: chr  "Suprema de pollo dulce, espinaca, crema ·cida, repollo avinagrado y guacamole" "Carne mechada en reducciÛn de vino tinto, champiÒones salteados, cebolla caramelizada y queso derretido" "Mayonesa al olivo, champiÒones salteados, jalapeÒos, queso Mozzarella, papas hilo y cebolla morada" "Queso Mozzarella, R˙cula, ChampiÒon portobello relleno de cheddar y luego apanado en panko y frito" ...
    ##  $ nota   : chr  "3" "3" "4" "4" ...

``` r
datos_analisis$nota <- as.numeric(datos_analisis$nota)
```

    ## Warning: NAs introduced by coercion

``` r
#tabla con datos limpios
datos_analisis1 <- na.omit(datos_analisis)

#tabla solo nota mayor a 5
sanguche_final = datos_analisis1 [datos_analisis1$nota >=5,]
#cambie nombre de las columnas
names (sanguche_final)= c("ingredientes", "notas")

#se crea una tabla solo con los ingredientes de las preparaciones
tabla_ingredientes <- str_split_fixed(sanguche_final$ingredientes, pattern = ",", n=8)



#crear dataframe
tabla_ingredientes<-as.data.frame(tabla_ingredientes)
#se cambian los nombres
names (tabla_ingredientes)= c("ingrediente1", "ingrediente2","ingrediente3", "ingrediente4","ingrediente5", "ingrediente6","ingrediente7", "ingrediente8")

#conversion a tabla identidad
texto <- sanguche_final$ingredientes

#se eliminan los espacios por espacios vacios
texto<-gsub(" ","", texto)
texto <- char_tolower(texto)
texto <- iconv(texto, to = "ASCII//TRANSLIT")

#se genera la tabla con la repeticion de los nombres
matriz_ingre <- dfm(texto, remove = c(stopwords("es"), "vs", ",","´",".","num","?","(",")","!","%","/"))
```

    ## Warning: NA is replaced by empty string

``` r
#se cuenta la cantidad de veces que se repite el ingrediente
Total = colSums (matriz_ingre)

#se genera un vector con con los nombres 
Ingr<-colSums(matriz_ingre)
Ingr<-attributes(Ingr)

#se juntan los vectores en un data frame
Total<-as.data.frame(Total)
Ingr<-as.data.frame(Ingr)
Ingr$cantidad<-Total
names (Ingr)= c("ingredientes", "total")

#se seleccionan los ingredientes cantidad sobre 4 
Mejores = Ingr [Ingr$total >=4,]
Mejores<-as.data.frame(Mejores)

Ingredientes_finales<-Mejores$ingredientes
Ingredientes_finales<-as.data.frame(Ingredientes_finales)
```
