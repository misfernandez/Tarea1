Tarea1
================

## Proyecto 1

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

ruta_excel <-  "/Users/misaelfernandez/Desktop/sanguchez.xlsx"
```

## Creamos una tabla con los datos que se van a analisar, los ingredientes y las notas y los limpiamos

``` r
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
datos_analisis
```

    ##                                                                                                                                                                                                url...5
    ## 1                                                                                                                        Suprema de pollo dulce, espinaca, crema ·cida, repollo avinagrado y guacamole
    ## 2                                                                                              Carne mechada en reducciÛn de vino tinto, champiÒones salteados, cebolla caramelizada y queso derretido
    ## 3                                                                                                   Mayonesa al olivo, champiÒones salteados, jalapeÒos, queso Mozzarella, papas hilo y cebolla morada
    ## 4                                                                                                   Queso Mozzarella, R˙cula, ChampiÒon portobello relleno de cheddar y luego apanado en panko y frito
    ## 5                                                                          Tofu asado no transgÈnico, palta, tomate, champiÒones, mix de hojas verdes org·nicas,  mayonesa de zanahoria vegana casera,
    ## 6                                                                                                    Hamburguesa, queso Cheddar, cebolla caramelizada, berros, pepinillos y salsa Jack Danielís Honey.
    ## 7                                                                                     Carne de vacuno, con cebolla caramelizada, palta, queso derretido, merkÈn, tocino, champiÒones y 2 huevos fritos
    ## 8                                                                                                                       Mechada con salsa de vino tinto, cebolla caramelizada y champiÒones salteados.
    ## 9                                                                                                                                    Pl·tano verde frito en vez de pan, carne mechada, queso y r˙cula.
    ## 10                                                                                                                         Costillar de cerdo, pepinillos caseros, queso Cheddar, coleslaw y salsa BBQ
    ## 11                                                                                                                                          At˙n rojo, nori, mayonesa al wasabi, soya, palta y pepino.
    ## 12                                                                                                       Bondiola desmenuzada, braseada en cerveza y salsa BBQ, cebolla caramelizada y tocino crocante
    ## 13                                                                 Hamburguesa de 180 grs de punta picana y tocino, con queso crema saborizado, camarones panko, r˙cula, sour cream y morrones asados.
    ## 14                                                                      Tocino, Hashbrown, Huevo frito, Chorizo, Queso, Palta, Tomate, Mesclun (mix verde), Alioli casero y Salsa BBQ, en pan ciabatta
    ## 15                                                                               Cereales integrales, salmÛn a las finas hierbas, queso crema con verduras, cebolla dulce, betarraga y mostaza picante
    ## 16                                                                                                                  Burger casera de 120 gramos, quesos cheddar y mantecoso, mayonesa casera y tomate.
    ## 17                                                                                      1) Mozzarella, tocino y huevo de codorniz. 2) Mozzarella y Portobello. 3) Queso azul y manzanas caramelizadas.
    ## 18                                                                                                                JamÛn Serrano, queso de cabra, tomate, aceitunas negras, albahaca y oliva, en frica.
    ## 19                                                                                                                                      Pescado frito, palta, tomate, cebolla y alioli, en marraqueta.
    ## 20                                                                                           Pato, queso Pepper Jack, jalapeÒos, cebolla morada, espinaca, tomate y mayonesa Habanero, en pan Brioche.
    ## 21                                                                                                                                             Lomo vetado, tomate, porotos verdes, ajÌ verde y palta.
    ## 22                                                                                    Hamburguesa, Queso americano, Tocino glaseado con miel de maple, Alioli con miel de maple, Poutine, Pan de leche
    ## 23                                                                                         Pan de berlÌn,  cerdo, semillas de mostaza, repollo morado encurtido, mermelada de cebolla y KÈtchup casero
    ## 24                                                                                                                      Hamburguesa de costillas, barbecue artesanal, cebollas caramelizadas y lechuga
    ## 25                                                                                                                                                              Posta rosada y un queso del Lago Ranco
    ## 26                                                                                                                                                                                     Cordero, hummus
    ## 27                                                                                                                  Carne mechada, pastelera de choclo, tomates asados y cebolla morada, en un panini.
    ## 28                                                                                                         Pollo al cognac, palta, lechuga, papas hilo, en pan italiano. Viene con su caldo para untar
    ## 29                                                                                                            Plateada al vino tinto, cebolla caramelizada, queso fundido y champiÒones, en marraqueta
    ## 30                                                                                                                                                       Churrasco, tomate, porotos verdes y ajÌ verde
    ## 31                                                                                                                                                       Tomate, palta, chucrut, mayonesa y americana.
    ## 32                                                                 Hamburguesa de 230 grs, tocino crocante, queso Mozzarella fundido, champiÒones salteados, tomate, pepinillo dill y mayonesa merkÈn.
    ## 33                                                                                Hamburguesa de habas y garbanzos, con palta, salsa de cilantro con ajo, queso Mozzarella y aros de cebolla apanados.
    ## 34                                                                                                          Carne, zucchini grillado con un toque de ajo, pimentÛn acaramelado, champiÒones y ricotta.
    ## 35                                                                                                                Lomo sajÛn, salsa Roquefort, nueces, champiÒones laminados y r˙cula, en pan toscano.
    ## 36                                                                                            Filete de res, con relish de betarraga, raÌz picante (r·bano), aros de cebolla y champiÒones Portobellos
    ## 37                                                                                       Costillar de cerdo desmenuzado a la chilena con mix verde, mayonesa al merkÈn y ensalada chilena,pan Ciabatta
    ## 38                                                                                           Hamburguesa de 180 grs rellena con queso Cheddar, agregado cebolla, pimentÛn asado, champiÒones salteados
    ## 39                                                                                                                                                              Cerdo, vacuno, queso de campo, caldito
    ## 40                                                                                        Hamburguesa con queso mantecoso fundido, confit de tomates, palta, papas hilo y huevo frito, en pan brioche.
    ## 41                                                                                                 Hamburguesa vegetariana, queso Gruyere, palta, mayonesa a las hierbas, lechuga, tomate y pepinillos
    ## 42                                                                                                 Zapallo japonÈs rostizado, Mayonesa al chilli, R˙cula, Garbanzos, Queso grana padano, Pan focaccia.
    ## 43                                                                                                                                     Brisket de vacuno mechado con pasta de queso Cheddar ñ pimentÛn
    ## 44                                                                                                                            Mechada, cebolla caramelizada, queso, pimiento morrÛn y mayonesa al ajo.
    ## 45                                                                                                                      Mechada, champiÒones salteados, tomate, r˙cula, berros y cebolla caramelizada.
    ## 46                                                                                                     Hamburguesa de 300 gramos rellena de queso, con mayonesa, r˙cula, champiÒones salteados y palta
    ## 47                                                                                                               Pastrami, alioli, tomates cherry, r˙cula, queso Cheddar, queso Mantecoso y papas hilo
    ## 48                                                                                                  Queso cheddar fundido, Tocino, Lechuga, Tomate, Cebolla grillada, Salsa BBQ y Mayonesa de la casa.
    ## 49                                                                                                  Hamburguesa vegetariana de lentejas, zanahoria y avena, verduras al wok, palta y salsa de cilantro
    ## 50                                                                           Hamburguesa de 200 grs rellena de queso Cheddar, con queso Mozzarella fundido, tocino, cebolla morada y mayonesa jalapeÒo
    ## 51                                                                                           Hamburguesa Angus rellana con queso, acompaÒada con cebolla fresca y salsa Toto (tomate y tocino ahumado)
    ## 52                                                                                      Hamburguesa 50% carne y 50% prieta, queso parmesano, manzana verde caramelizada, palta, r˙cula y salsa al ajo.
    ## 53                                                                                                      Doble hamburguesa, doble queso Cheddar, tocino, lechuga, tomate, pepinillos, cebolla y mostaza
    ## 54                                                                                          Hamburguesa de 220 grs, queso Cheddar, lechuga, tomate, cebolla morada fresca y tocino ahumado en manzana.
    ## 55                                                                                             Hamburguesa, mayonesa de cilantro, queso Mozzarella, jalapeÒo, guacamole, nachos y mayonesa de chipotle
    ## 56                                                                                                         Queso crema, jamÛn serrano, lechuga, salsa agridulce semi picante, pepinillo dill y tomate.
    ## 57                                                                                             Lomo ahumado kassler, queso Mozzarella, tomate, lechuga, aceite de oliva, en pan Ciabatta a la plancha.
    ## 58                                                                                                                                                        Tomate, churrasco, pan casero, palta, filete
    ## 59                                                                                        SalmÛn en mantequilla aromatizada, tomates asados, queso crema saborizado, corazÛn de alcachofa y mix verde.
    ## 60                                                                                                                Lomo de cerdo, queso fundido, pepinillos, lechuga, tomate, y salsa secreta Haussmann
    ## 61                                                                                                                                                                      Pollo, lechuga y salsa t·rtara
    ## 62                                                                                                                               Pollo frito, cama de r˙cula, piÒa grillada, queso Camembert y mostaza
    ## 63                                                                                                                                  Carne mechada, mix verde, lactonesa y chutney de tomate y jengibre
    ## 64                         Burger vegetariana (de porotos negros, betarraga, arroz integral y otros secretos), vegetales grillados, aros de cebolla apanados, pepinillos, lechuga, tomate y salsa BBQ.
    ## 65                                                                                                                                                                 Pollo con Coleslaw y cebolla frita.
    ## 66                                                                                                           Hamburguesa con queso azul gratinado, manzana caramelizada, misuna y salsa de queso azul.
    ## 67                                                      Crudo de res machacado, con crema ·cida al ciboullette, alcaparras, pepinillos en vinagre, chucrut de repollo morado y lechuga en sopaipillas.
    ## 68                                                            Sierra ahumada desmenuzada, con mayonesa de ajo, perejil y limÛn, sobre una cama de lechuga costina y pebre de cochayuyo, en sopaipilla.
    ## 69                                                                                                                  Hamburguesa de Wagyu, queso Cheddar blanco, cebolla caramelizada y alioli trufado.
    ## 70                                                                                                         Filete Mignon, cebolla caramelizada, nueces, r˙cula, manzana, queso suizo, en pan ciabatta.
    ## 71                                                                                                                         Carne, lechuga, tomate, cebolla morada, panceta, queso, huevo y pepinillos.
    ## 72                                                                                                               Hamburguesa, salsa BBQ, pepinillo, cebolla morada, tocino y queso derretido, en frica
    ## 73                                                                          Hamburguesa de 200 grs, mix de hojas verdes (r˙cula, berros y albahaca), salsa Roquefort y mermelada de berries a la Stout
    ## 74                                                                                                                                            Mechada, palta, tomate y mayonesa casera, en sopaipillas
    ## 75                                                                                                              Hamburguesa, tomate, lechuga hidropÛnica, queso de cabra, papaya y salsa de queso azul
    ## 76                        Pan Brioche, hamburguesa de vacuno Hereford de 150 grs, queso de HuentelauquÈn, lechuga, tomate, cebolla caramelizada, pepinillo dill, mayonesa casera a la mostaza antigua.
    ## 77                                                                                      Hamburguesa de 200 grs, lechuga escarola, tomate, cebolla crispy, palta, pimiento morrÛn, queso gauda y tocino
    ## 78                                                                                                                                                      Lomo de cerdo, palta, tomate, mayonesa casera.
    ## 79                                                                                                                                                                                                  NA
    ## 80                                                                                                                    Ossobuco cocinado con cerveza, r˙cula, cebolla y mayonesa Stout, agregado palta.
    ## 81                                        Salchicha, salsa de cilantro picante, papas hilo, napolitana (aceituna, choclo y tomate), pepinillo, y salsa Palermo (quesillo, huevo, pimentÛn y mayonesa).
    ## 82                                                                                                                                                                Lomito, queso, pimentÛn, palta, mayo
    ## 83                                                        Lomito de cerdo, queso Mozzarella, queso Cheddar, cebolla salteada, pimentones rojos, palta, mayonesa casera, champiÒones salteados y tocino
    ## 84                                                                                                     Reineta frita, lechuga, tomate, cebolla morada y blanca, ajÌes blanqueados y salsa al cilantro.
    ## 85                                                                                                                  Longaniza de jabalÌ con cebolla, ajÌ amarillo, r˙cula y zanahoria, en pan de queso
    ## 86                                                                                                                         Churrasco, Quilicura, Querricura, Santiago, Barato, As, Completos, Lomitos.
    ## 87                                                 Lomito, tocino, pepperoni, hamburguesa picada, cebolla caramelizada al merlot, papas fritas, queso Cheddar, queso Mantecoso, salsa BBQ y jalapeÒos.
    ## 88                                                                                                                                                            Carne de cerdo, mayonesa, pan marraqueta
    ## 89                                                                                            Hamburguesa de 180 grs, mayonesa casera, lechuga, queso Azul, frutillas grilladas y nueces caramelizadas
    ## 90                                                                                                                                            Churrasco, palta, tomate, palmito, ajÌ verde y mayonesaC
    ## 91                                                                                                                                               Filete de pollo, guacamole, queso, lechuga lolo green
    ## 92                                                                                                                       Pan de masa madre, lechuga, tomate, cebolla y una salsita creo que de zapallo
    ## 93                                                                                                                                                                           Marraqueta, Filete, Queso
    ## 94                                                                                                                                       Mozzarella de b˙fala, prosciutto, pesto de albahaca y r˙cula.
    ## 95                                                                                 Queso cheddar, Pan casero con sÈsamo, Lechuga, Cebolla, Tocino ahumado, Ketchup casero, Pepinillos en vinagre, Apio
    ## 96                                                                                                      AlbÛndigas de quinoa crocante, hojas verdes, huevo frito, brotes de alfalfa y chips de camote.
    ## 97                                                                                  Prieta Desmenuzada, queso de cabra, pimentones rojos asados y berros, en baguette, con salsa de yogur y ciboulette
    ## 98                                                                                                                                   Hamburguesa de Lentejas, cebolla frita, champiÒones, huevo frito.
    ## 99                                                                                                  Carne mechada atomatada , pl·tano frito, queso Gouda rallado, ensaladita de pepino y manzana verde
    ## 100                                                                                                             Salchicha de vacuno, repollo rojo, betarraga, lingonberries, chalotas crispy y cebolla
    ## 101                                                                                            Prieta chilena artesanal a la plancha con pebre, crocante de papas hilo y nueces tostadas, en pan frica
    ## 102                             L·minas de pierna de jabalÌ macerdas en finas hierbas y vino, cebolla al Merlot, mayo al cilantro, choclo a la mantequilla (no habÌa y los cambiaron por champiÒones).
    ## 103                                                                                                        Hamburguesa al carbÛn, palta, pl·tano maduro grillado, jalea de tomate y mayonesa al curry.
    ## 104                                                                                                                   ChicharrÛn de pescado y mariscos, camote frito, sarza criolla y salsa acevichada
    ## 105                                                                                                                                        Hamburguesa con tocino, palta tempura y alioli en pan frica
    ## 106                                                                                     Hamburguesa a la parrilla, pl·tano frito, huevo frito, sarza criolla, camote al hilo acompaÒado de rica salsa.
    ## 107                                                                                  Carne mechada, cebolla morada, tomate fresco, ajÌ verde, agregado chancho en piedra y merkÈn, en marraqueta y 1/2
    ## 108                                                                                    Hamburguesa de calabaza camote, mayonesa al cilantro, queso Cheddar, cebolla grilladas y champiÒones salteados.
    ## 109                                                                                                                         Salchicha asada, chucrut, tomate, palta, mayonesa casera y salsa americana
    ## 110                                                     Lomo liso ahumado al wok, pimentÛn, cebolla morada, cilantro, papas al hilo, salsa t·rtara, queso fundido y palta en l·minas, en pan Ciabatta.
    ## 111                                                                                                                                                   Lengua, porotos verdes, mayonesa casera, tomate.
    ## 112                                                                                               Trozos de lechÛn macerado al huacatay, lechuga, yuca frita, chifles, choclo peruano y zarza criolla.
    ## 113                                                                                                                                      Pastrami caliente, con queso derretido, mostaza y pepinillos.
    ## 114                                                                              Hamburguesa de Berenjena, queso, lechuga, tomate, cebolla morada, pepinillos, mix de hojas verdes y mayonesa de soya.
    ## 115                                                                                                                         Pescado frito con tomate salteado en ajo y cebolla frita en az˙car de caÒa
    ## 116                                                                                                 Carne mechada al vino tinto, con cebolla caramelizada, pimientos asados, champiÒones y queso azul.
    ## 117                                                                                                                                               Churrasco picado, champiÒÛn salteado, queso fundido.
    ## 118                                                                                                                      Pechuga de pollo con queso, aros de cebolla, pepinillos, mayonesa y salsa BBQ
    ## 119                                                                                                                     Carne Mechada, palta, tomate, lechuga, cebolla caramelizada y mayonesa casera.
    ## 120                                                                                                                   Hamburguesa, queso suizo, champiÒones asados, cebolla grillada y mayonesa alioli
    ## 121                                                                                                                                                     Carne mechada ahumada, palta, tomate, mayonesa
    ## 122                                                                                                                              Pollo guisado, carne desmechada, chicharrÛn de cerdo, queso y salsas.
    ## 123                                                                                                                                                               Pernil, palta y queso, en marraqueta
    ## 124                                                                                                                                            Tomate, americana, palta, chucrut, poroto, queso y mayo
    ## 125                                                                                                                                    Doble lomito de cerdo, queso, tomate, palta, lechuga y mayonesa
    ## 126                                                                                                                    Hamburguesa casera de 150 grs, Chile Chilli, palta, tomate cherry y huevo frito
    ## 127                                                                                                           Cebolla caramelizada, Queso mantecoso derretido, Huevo frito, Kiss my hass (palta frita)
    ## 128                                                                                            Tomate fresquito, de buen color y bien jugoso. Buena mayonesa casera, complementada con su palta molida
    ## 129                                                                                   Hamburguesa de quinoa con toques de limÛn, pimientos frescos, champiÒones salteados, alfalfa y pesto de cilantro
    ## 130                                                                                                                                       Lomo de cerdo, palta, tomate, chucrut, americana y mayonesa.
    ## 131                                                                                                                                                            Tocino, Queso Cheddar, Tomate, Lechuga.
    ## 132                                                                                                                    Tomtate, Salsa criolla, ChampiÒones, Papas hilo, Salsa al Olivo en pan ciabatta
    ## 133                                                                                                                                                                        Poroto verde, pollo, palta.
    ## 134                                                                                                                         Hamburguesa, tocino, lechuga, tomate, palta, mayonesa y huevo a la plancha
    ## 135                                                                                                                                                                         Tomate, pesto, mozzarella.
    ## 136                                                                                                Merluza frita sin espinas, lechuga, tomate, cebolla, orÈgano y cilantro, ìy un ingrediente secretoî
    ## 137                                                                                                                                                                                 S·ndwich de lengua
    ## 138                                                                                                                                                       Panita, cebolla, huevo frito y papas fritas.
    ## 139                                                                                                                                                                   Tomate, mayonesa, chucrut, palta
    ## 140                                                                                                                Plateada desmenuzada, ajÌ verde, tomate, porotos verdes, mayonesa y queso derretido
    ## 141                                                                                                                      Mechada, con palta, tomate, ajÌ verde, cebolla caramelizada al ron y lechuga.
    ## 142                                                                                   Pechuga de pollo, tocino, queso derretido, champiÒones salteados, lechuga, tomate, papas ìhiloî y salsa al gusto
    ## 143                                                                                                          Verduras asadas (berenjenas y pimentones), con queso de cabra fundido y purÈ de aceitunas
    ## 144                                                                                                                                   SalmÛn, chorizo, cebolla caramelizada, zanahoria, queso y tomate
    ## 145                                                                                                                                              Lomo de res al ajo y aceite, con queso en pan francÈs
    ## 146                                                                                   Croquetas de garbanzos , tomate, pepino, repollo, cebolla, pimentÛn verde, hummus, tehina y picante, en pan pita
    ## 147                                                                                  Pita de Falafel (crujiente croqueta de garbanzos y aliÒos ·rabes), lechuga, tomate, cebolla y salsa merkÈn-yogurt
    ## 148                                                                                                              Sobrecostilla asada, queso de campo derretido, tomate, ajÌ verde, lechuga y mayonesa.
    ## 149                                                                                    Hamburguesa de vacuno, Cebolla frita al estilo de Kansas, Queso cheddar, ChampiÒones trufados, Mayonesa trufada
    ## 150           Carne neozelandesa premium, Tocino ìstreaky baconî, Queso cheddar, Pepinillos dulces encurtidos, Lechuga, Tomate, Cebolla morada, Alioli, Relish de tomate en pan\rcasero de masa madre.
    ## 151                                       Hambuguesa, queso Cheddar, lechuga hidropÛnica, tomate, cebolla morada, pepinillos, pimientos asados, jalapeÒos y crema de rocoto con un toque de zanahorias
    ## 152                                                                                                                  Doble hamburguesa, doble queso Cheddar, tocino, cebolla asada y salsa de carne A1
    ## 153                                                                                                             Camote asado, Queso azul Great Hill, Pesto con nueces, Manzana verde, Hojas de mostaza
    ## 154                                         Hamburguesa casera, Queso cheddar, Huevo frito, Betarraga, Lechuga, Tomate, Cebolla morada encurtida, Relish de kiwi, Salsa de la casa en pan de carbÛn.\r
    ## 155                                                                                                                                   Pan chino al vapor, relleno con trocitos de cerdo y salsa china.
    ## 156                                                                                                                Hamburguesa de wagyu de 185 grs, lechuga, tomate, cebolla morada y mayonesa picante
    ## 157                                                                                               Salsa de queso azul, pepinillos, cebolla caramelizada, queso Mantecoso, espinaca y salsa de la casa.
    ## 158                                                             Pulled chicken ahumado, queso de cabra, salsa Teriyaki, chips de betarraga frita, mayonesa de cilantro y berros en pan francÈs casero.
    ## 159                                                                                                                                   Fricandela, porotos verdes, tomate, aÌ verde, agregado mayonesa.
    ## 160                                                                                                                                                       Ajo, AjÌ, Mayonesa, Salsa de tomate, Chucrut
    ## 161                                                                                                                                                               Tomate, Chucrut, Mayonesa, Pepinillo
    ## 162                                                                                       Hamburguesa de carne de vacuno y cerdo molida condimentada con ajo y ajÌ, tomate, chucrut y mayonesa casera.
    ## 163                                                                                                                                                                 Mayonesa, Salsa de Tomate, Chucrut
    ## 164                                                                                                                                                                         Tomate, chucrut y mayonesa
    ## 165                                                                                                                                          Lomito, queso, tomate, choclo, ajÌ verde y pimentÛn verde
    ## 166                                                                                             Mechada, queso Cheddar, champiÒÛn, tocino crispy, cebolla, tomate, pepinillo, lechuga, mayonesa casera
    ## 167                                                                                                                           Mechada, palta, cebolla caramelizada, champiÒones asados, en marraqueta.
    ## 168                                                                                                                                                           Porotos verdes, tomate, queso, churrasco
    ## 169                                                                                                                                                     Carne mechada, palta, tomate, mayonesa casera.
    ## 170                                                                                                                                                                                                 NA
    ## 171                                                                                                     Gorda, queso mantecoso, cebolla morada, tomate, ajÌ verde, chucrut y mayonesa danÈs al merkÈn.
    ## 172                                                                                                                           Hamburguesa, lechuga, tocino, palta, mozzarella y mayonesa al ciboulette
    ## 173                                                                                                                                                      Palta, Churrasco con ajo, orÈgano y pimienta.
    ## 174                                                                                        Relleno de pollo a la plancha, berros, salsa de cilantro, y un mix de champiÒones con queso crema derretido
    ## 175                                                                               Hamburguesa dry-aged, Queso provolone, Tocino glaseado con miel de maple, JalapeÒo, Relish de piÒa, Mayonesa trufada
    ## 176                                                                                         Hamburguesa, huevo frito, lechuga, tomate, trozos de Roquefort, cebolla crispy y salsa de queso Roquefort.
    ## 177                                                                                     Hamburguesa de vacuno, Burrata (un queso tipo Mozzarella), pesto, r˙cula, tomates secos y mayonesa de albahaca
    ## 178                                                                                 Salchicha Super Frank (cerdo y vacuno), cebolla caramelizada, tocino crocante, salsa de queso Cheddar y pepinillo.
    ## 179                                                                                                               Hummus de garbanzos, berenjena horneada, menta, cebolla bals·mica, pickles y lechuga
    ## 180                                                                                                                                   Salchicha alemana, queso, tocino crocante y cebolla caramelizada
    ## 181                                                                                                                             Queso, Tomate, Lechuga, Cebolla, Pepinillos, Mostaza, Ketchup, Tocino.
    ## 182                                                                                                        Lomo de cerdo, tomate,champiÒones a la mantequilla, queso derretido y manzana caramelizada.
    ## 183                                                                                                                                                             Lomo saltado, cebolla morada, perejil.
    ## 184   Hamburguesa de vacuno (posta), aros de cebolla morada, salsa t·rtara, pepinillos dill, pimentones asados, tocino, queso azul, queso de cabra, queso Gouda y Parmesano, en pan Ciabatta artesanal
    ## 185                                                                          Pechuga de pollo marinada en ajÌ verde, ajo, orÈgano y merkÈn, con porotos verdes, tomate y mayonesa, en un mini baguette
    ## 186                                                                                                                          Mix punta paleta, tocino y punta ganso, Tocino, queso, encurtidos, Salsa.
    ## 187                                                                                                    Pollo asado desmenuzado con palta, queso crema, tocino, berros y pebre de tomates deshidratados
    ## 188              Costillar de cerdo al merkÈn, braseado y desmenuzado con cebollas caramelizadas, con tomates confitados al orÈgano y fonduta de quesos, montado en un exquisito pan amasado especial.
    ## 189                                                                                                        Berenjenas grilladas, tomates confitados, pesto de la casa y mozzarella gratinada en r˙cula
    ## 190                                                                                            Queso cheddar, Queso Havarti, Costillitas de vacuno desmenuzada, Cebolla caramelizada, Cebolla crunchy.
    ## 191                                                                                                                                          Churrasco, jamÛn, tomate, mayonesa y queso, en pan frica.
    ## 192                                                                         Lomito de cerdo a la cacerola con queso camembert apanado en panko, frito, con chutney y crema de papayas y mix de verdes.
    ## 193                                                                                    JamÛn serrano, r˙cula, queso parmesano, aceitunas y aceite de oliva, en pan italiano frotado con tomate rallado
    ## 194                                                                                                  Hamburguesa de quinoa, cebolla y champiÒones salteados, queso vegano, mayonesa vegana y salsa BBQ
    ## 195                                                                                                                       Queso crema, Aceitunas verdes, Queso Las ¡guilas, Sal al merkÈn, Mantequilla
    ## 196                                                   Hamburguesa, salsa BBQ, trozos de queso azul, queso Cheddar, tiras de tocino crispy, onion rings, lechuga, tomate, pepinillos, y cebolla en aros
    ## 197                                                                       Mejillas de res cocinadas por 12 horas, Mayonesa de r·bano, Queso, Cebolla morada, R˙cula, Salsa secreta, Pan de masa madre.
    ## 198                                                                                                                               Doble hamburguesa, jalapeÒos, queso, tomate, lechuga y salsa Houston
    ## 199                                                                                                                   Hamburguesa de garbanzos, tomate, r˙cula, pepinos dulces, mayonesa de aceitunas.
    ## 200                                                                                                                             Churrasco filete, tomate, porotos verdes, ajÌ verde. Agregado mayonesa
    ## 201                                                                                 Carne molida de vacuno y cerdo mezclada  con pimientos, queso Cheddar,  mayonesa, lechuga, tomate y cebolla morada
    ## 202                                                                                                                                                              Pastrami y mostaza, en pan de centeno
    ## 203                                                                                                               Pechuga de pollo, tocino, tomate, lechuga y mayonesa al ciboulette, en pan italiano.
    ## 204                                                                                                                                                              Pasta de chorizo, pan hallula, queso.
    ## 205                                                                                            Churrasco a la plancha, queso fundido, cebolla caramelizada y champiÒones a la mantequilla, en baguette
    ## 206                                                                                                                      Cordero braseado, queso azul, pepinillo, cebolla caramelizada y mostaza Dijon
    ## 207                                                             Pechito de cerdo braseado con cerveza Golden, mostaza de la casa y ensalada de repollo agridulce, manzana y semillas, en pan Ciabatta.
    ## 208                                                                      Lomo de cerdo, tomate, salsa verde, porotos verdes al dente, tocino crunchy, champiÒones Paris a la plancha y mayonesa casera
    ## 209                                                                                                     Gorda, queso mantecoso, cebolla morada, tomate, ajÌ verde, chucrut y mayonesa danÈs al merkÈn.
    ## 210                                                                                                                              Camarones fritos en panko, Palta, Queso mozzarella ,Salsa sweet chili
    ## 211                                                                                                                                          Churrasco, porotos verdes,  salsa verde y salsa de merkÈn
    ## 212                                                                                                                                                  Lomo liso,lechuga, queso derretido, huevo, tomate
    ## 213                                                                                                                   Salsa la birreria, r˙cula, queso cheddar, cebolla acaramelada, pepinillos dulces
    ## 214                                                     Salsa La BirrerÌa, Lechuga, Tomate, Queso de cabra, Queso mozzarella, ChampiÒones salteados, Tocino ahumado crocante, Aros de cebolla apanados
    ## 215                                                 Hamburguesa de 3 cortes de carne, base de lechuga y tomate, queso Cheddar, y ajÌes verdes rellenos con Mozzarella, tocino crocante, mayonesa spicy
    ## 216                                                                                                      Lechuga, Tomate, Queso Cheddar, AjÌes verdes rellenos con mozzarella y tocino, Mayonesa spicy
    ## 217                                                                                                                        Burger de champiÒones, con queso de oveja, tomate, palta y mayonesa vegana.
    ## 218                                                                                                             Choripan artesanal con 20% de cochayuyo, salsa de naranja con romero y la salsa griega
    ## 219                                                                                                    Jaiba marinada en mandarina, limÛn, mayonesa casera y cilantro, tomate, palta, pepino y lechuga
    ## 220                                                                                                        Hamburguesa de 180 grs, champiÒones salteados, queso fundido y r˙cula fresca, en marraqueta
    ## 221                                                                                                Hamburguesa de camarones, queso Mozzarella, r˙cula, aceitunas, cebolla morada, con salsa de whisky.
    ## 222                                                                                             Plateada cocinada por 5 horas, r˙cula, palta, queso azul, cebolla caramelizada y lactonesa sin lactosa
    ## 223                                                                                                                                              Mechada con pastelera de choclo, tomate y salsa verde
    ## 224                                                                                                                 Costillar de Cerdo, sarza criolla con hierbabuena, camotes fritos, en pan francÈs.
    ## 225                                                                       ChampiÒones salteados a la mantequilla y pimienta, queso de cabra, cebolla caramelizada, queso crema, palta y lechuga fresca
    ## 226                                                                                                            Hamburguesa, queso Llanero asado, tomate, lechuga, palta, palmitos y salsa chimichurri.
    ## 227                                                                                                                                      LechÛn a la leÒa con sarza criolla, y le sumÈ un huevo frito.
    ## 228                                                                            Costillar de cerdo 100% org·nico de cocciÛn lenta con salsa BBQ casera, ensaladilla criolla con palta, en pan Ciabatta.
    ## 229                                                                                                Queso crema, Tocino, JalapeÒos, Mix verde, Cebolla caramelizada, Queso cheddar, Salsa de frambuesa.
    ## 230                                                                                                                                        Arrollado de malaya, palta, tomate, mayo casera y ajÌ verde
    ## 231                                                                                                           Anillos de calamar apanado, lechuga escarola, salsa t·rtara, ajÌ verde y pepinillo dill.
    ## 232                                                                                                               Plateada de vacuno, pastelera de choclo, papas hilo, huevo frito y cebolla rebosada.
    ## 233                                                                                                                                           Carne Mechada, poroto verde, ajÌ verde, tomate, mayonesa
    ## 234                                                     Milanesa de Bife de Chorizo, queso gratinado, pimentones asados con huevo duro rallado, mayonesa de chimichurri y salsa criolla, pan de sÈsamo
    ## 235                                                            Hamburguesa casera a la parrilla, con camote caramelizado, gratin de queso Gruyere, una reducciÛn de Oporto (vino) y l·minas de tocino.
    ## 236                                                                                                                                    La Pic· de Clinton, Santiago Centro, Santiago, Carne, Churrasco
    ## 237                                                                                                                                                                          Lomito, palta, marraqueta
    ## 238                                                                                                                                              Carne Mechada, palta, tomate, mayonesa, en marraqueta
    ## 239                                                                                                                                    Merluza frita y ensalada chilena, agregado pebre, en marraqueta
    ## 240                                                                                                                                                                  Lomito con TODOS los ingredientes
    ## 241                                                                                                                        Lomo de res, cebolla morada, pimentÛn y papas hilo, en pan ciabatta peruano
    ## 242                                                                                             Pan focaccia de masa madre, Berenjenas, Tomates asados, Ricotta frÌa, Pesto de la casa y Aceite de ajo
    ## 243                                                                                                                       Finas lonjas de pulpo con salsa anticuchera, zarza criolla y chips de camote
    ## 244                                                                                                                Hamburguesa de 150 gramos, queso de oveja, salsa pomodoro, hongos y pesto de r˙cula
    ## 245                                                                                                                                 SalmÛn ahumado, palta hass, ricotta con ciboulette y salsa t·rtara
    ## 246                                                                                                                     Milanesa de vacuno, lechuga, aceitunas sevillanas, jamÛn serrano y salsa brava
    ## 247                                                                                                                  Chorizo argentino, chimichurri, huevo frito y cebolla caramelizada, en marraqueta
    ## 248                                                                                     Merluza Austral apanada y frita, con ensalada chilena, mayonesa casera y mermelada de ajÌ verde, en marraqueta
    ## 249                                                                                                                                             Lengua, tomate, palta, mayonesa (agregado salsa verde)
    ## 250                                                                     Solomillo de cerdo a la plancha,cebolla caramelizada al vino dulce, cebollÌn crujiente, y salsa de queso Azul, en Pan Ciabatta
    ## 251                                                                                                                                                   Lomo de vaca con queso derretido y cebolla frita
    ## 252                                                                          Hamburguesa de soya, l·minas de berenjenas asadas, berros frescos, pepinillo dulce sobre tomate, palta y mayonesa casera.
    ## 253                                                                                                                                                   Lomito de cerdo, palta, tomate, mayonesa y queso
    ## 254                                                                                                                                                        Hamburguesa, cebolla morada y queso Cheddar
    ## 255                                                  Malaya de cerdo macerada en jugo de limÛn, queso provoleta, mermelada de ajÌ verde, mayonesa casera, lechuga y mostaza en pan de tomate albahaca.
    ## 256                                                                                                                                                 Mechada, salsa de tomate, palta, mayonesa, tomate.
    ## 257                                                                                  Hilachas de costillar deshuesado y pulpa de cerdo marinado en soya y mostaza, con palta a la chilena, en Ciabatta
    ## 258                                                                 Alioli, mix verde, pimento asado, jamÛn serrano, ensalada chilena con tomates cherry y aroma de pesto, y papas hilo en marraqueta.
    ## 259                                                                                          Mayonesa de albahaca, Salsa de queso roquefort, Chutney de pera, Mix verde, Papas hilo, Ensalada chilena.
    ## 260                                                                                                                                                       Porotos verdes, mayonesa casera y ajÌ verde.
    ## 261                                                                                                                                              Churrasco filete, porotos verdes, ajÌ verde y tomate.
    ## 262                                      Hamburguesa de Wagyu y Angus con cebolla caramelizada al maple syrup, queso Camembert y queso Azul derretido, berros y alioli (mayo al ojo) en pan artesanal.
    ## 263                                                                                                                         Hamburguesa de 200 grs, piÒa asada, queso Cheddar, tocino y repollo morado
    ## 264                                                                                                                                           EntraÒas, chorizo, pebre, cebolla asada, mayonera casada
    ## 265                                                                                                            Queso azul, Dulce de membrillo, Almendras tostadas, Mayonesa de albahaca, Tomate asado.
    ## 266                                                                      Milanesa de posta (apanada sin harina), con queso Mozzarella, cebollita frita, salsa de tomates casera y un toque de orÈgano.
    ## 267                                                                                            Hamburguesa con queso Brie apanado, cebolla caramelizada, lechuga, mostaza Dijon y mermelada de tomate.
    ## 268                                                                                                                                                                  Choricillo, carne, pollo y queso.
    ## 269                                                                                                                                              Mechada, queso, r˙cula, salsa pomodoro, en pan frica.
    ## 270                                                                                                      1) De lomo saltado, 2) de chanchito asado y 3) de pescado frito, todos en pan chino al vapor.
    ## 271                                                                                                       Pulpa de cerdo picante al jugo, purÈ de palta, chancho en piedra y ajÌ verde, en marraqueta.
    ## 272                                                                                                                   Pollo salteado con sÈsamo tostado, salsa agridulce, queso crema, palta y lechuga
    ## 273                       ChampiÒÛn gigante apanado, queso Mantecoso fundido (puede ser reemplazado por queso vegano), palta, tomate, cebolla caramelizada, pepinillos y veganesa a base de zanahoria.
    ## 274                                                                                                                                                                Carne, queso, tomate, palta y mayo.
    ## 275                                                                                                               JamÛn crudo, tomates secos, pesto, lechuga y queso de cabra fundido, en pan Ciabatta
    ## 276                              Tres hamburguesas chiquitas, y cada una viene en un pan Brioche, con queso Azul, queso Emmental y queso Brie, adem·s de cebolla caramelizada, r˙cula y tomates asados
    ## 277                                          Pil-pil de trutro de pollo cocinado al vaciÛ, con acevichado de espinacas, cilantro y zanahoria, cebolla caramelizada, y mayonesa casera, en pan ciabatta
    ## 278                                                                                                      Hamburguesa de cordero, cebolla caramelizada, tomates asados, r˙cula, queso azul y papas hilo
    ## 279                                                                                                                                      Lomo liso, queso, huevos fritos, mayonesa casera y m·s queso.
    ## 280                                                                                                                                                                 Churrasco,huevo revuelto y tocino.
    ## 281                                                                                                                          Mechada de vacuno, cebolla frita y dos huevos planchados, en un Ciabatta.
    ## 282                                                                                                                        Cordero con mayonesa casera, lechuga y mostaza Dijon, en mini marraquetitas
    ## 283                                                                                                                                                                       Queso Brie, Tomate, Albahaca
    ## 284                                                            Hamburguesa (340 grs de carne), con doble queso Cheddar, tocino crocante, aros de cebolla apanados, mayonesa de eneldo y cebolla morada
    ## 285                                                                                                             Churrasco de lomo liso, palta, anillos de cebolla, tomate, lechuga, mayonesa y tocino.
    ## 286                             Hamburguesa, queso Cheddar, berros, tomate confitado, tocino, huevo frito, aros de cebolla morada, y crocante de Mozzarella, en pan Brioche negro, ahumada con romero.
    ## 287                                                                                                                                                                 Pan amasado, tomate, palta y carne
    ## 288                                                                                                                        Camarones, palta, cebolla caramelizada, quinoa y crema ·cida, en pan frica.
    ## 289                                                                                                                             Pollo, tocino, huevo, tomate, palta, lechuga, mayonesa, en pan de miga
    ## 290                                                                                                                                                 Res deshilachada, palta, tomate, mayonesa pale ale
    ## 291                                                           Hamburguesa de 220 grs, con costillas BBQ mechadas, cebolla grillada, queso Pepper Jack, lechuga hidropÛnica, pepinillos y salsa casera.
    ## 292                                         Hamburguesa de porotos negros, con cebolla caramelizada, veganesa de cilantro, lechuga, betarraga y zanahoria en juliana y salsa barbecue hecha ahÌ mismo.
    ## 293                                                                                                                                             Carne, queso derretido, cebolla salteada y champiÒones
    ## 294                                                                                                                      Doble hamburguesa, Doble queso, doble tocino, Donas con frosting de caramelo.
    ## 295                                                                                                                                                 Carne de vacuno, cebolla, tomate, palta y lechuga.
    ## 296                                                                                                                                                                       Hamburguesa, lechuga, tomate
    ## 297                                                                                                         Hamburguesa de porotos, y la acompaÒaban con r˙cula, berenjena, tomate y mayonesa vegetal.
    ## 298                                                                                                           Pulpa de cerdo ahumado con cebolla caramelizada, tomates asados y mayonesa al ajo/merkÈn
    ## 299                                                                                                              Churrasco en marraqueta, con palta, tomate, pebre y una salsa de yogurt al ciboulette
    ## 300                                                                                                       Milanesa de quinoa casera, pan Brioche, queso, r˙cula, tomates secos y mayonesa de zanahoria
    ## 301                                                                                                                                                                   Mantequilla y quesos venezolanos
    ## 302                                                                                                       SalmÛn ahumado, mix verde, mayonesa al ajo, tomate, palta, queso derretido, en pan hecho ahÌ
    ## 303                                                                                                                      pastrami ahumado y curado en casa, con queso y chucrut, en pan de masa madre.
    ## 304                                                                                                   Tocino, cebolla caramelizada al Calafate, queso Mantecoso, pepinillos y lactonesa, en pan casero
    ## 305                                                                                                                 Mechada en una reducciÛn de vino tinto, queso, palta, pebre, ajÌ verde y lactonesa
    ## 306                                                        Pollo frito al estilo sureÒo de EEUU, Queso, Lechuga, JalapeÒo, Tocino, Tomate, Mayonesa, Chipotle, Pan de hamburguesa de 24 kilates de oro
    ## 307                                                                                         Hamburguesa Black Angus, queso Pepper Jack, lechuga, cebolla crispy y mayonesa chipotle, en pan de pretzel
    ## 308                                                                                                                                                                    Salchicha, tomate, palta, mayo.
    ## 309                                                                                                     Hamburguesa Angus de 230 grs, pepinillos, queso derretido, tocino, cebolla morada y salsa BBQ.
    ## 310                                                                                           Croqueta de quinoa, zuccini, berenjena y pimentones asados, lechuga, queso de cabra y salsa vegetariana.
    ## 311                                                                                                                                             Carne mechada, palta, tomate y ajÌ verde en escabeche.
    ## 312                                                                                                                                                                                      Lomito, queso
    ## 313                                                                                                                                  Salchicha, tomate, palta, mayonesa, salsa verde y salsa americana
    ## 314                                                                                                                                                                 Lomito, Mayonesa, Palta, Tomate.\r
    ## 315                                                                                                        3 Hamburguesas de mezcla de wagyu con angus, cebolla caramelizada a la naranja y foie gras.
    ## 316                                                                                                                                                       Salchicha, tomate, palta, mayonesa de papa .
    ## 317                                                                                                                                                          Pollo al cognac, palta, tomate, mayonesa.
    ## 318                                                                                                                                      Lomo, choricillo, queso caliente, tomate, orÈgano y ajÌ verde
    ## 319                                                                                                  Hamburguesa de quinoa, tomate, palta, lechuga, pepinos By Maria y mayonesa de merkÈn (sin huevo).
    ## 320                                                                                                                                          Arrollado de huaso, tomate, ajÌ y mayonesa, en marraqueta
    ## 321                                                                                                                       Doble hamburguesa, queso americano, ajÌ cherry pepper, tocino y salsa Streat
    ## 322                                                                                                                                                         Mayonesa, chucrut, tomate  y carne mechada
    ## 323                                                                                            Hamburguesa de 250 grs, cebolla caramelizada, champiÒones, tocino, queso tres leches y tomate grillado.
    ## 324                                                                                                                                                                  Merluza Austral, ensalada chilena
    ## 325                                                                                                        Carne de res, ajo, especias, chimichurri casero, apio, perejil, Sriracha (Salsa Vietnamita)
    ## 326                                                                                                                Arrollado de huaso, con palta, tomate y mayonesa, en una tortilla amasada artesanal
    ## 327                                                                                                              Doble hamburguesa (220 grs), doble queso Cheddar, salsa BBQ, aros de cebolla y tocino
    ## 328                                                                                                                                             Queso Cheddar, Churrasco, Guacamole, JalapeÒo, Lechuga
    ## 329                                                                          Pollo deshilachado en curry y leche de coco, con tiras de zanahoria, zapallo italiano, pimentÛn, cebolla morada y lechuga
    ## 330                                                                                  Hamburguesa de 225 grs., cebolla morada, palta hass, queso camembert, jamÛn serrano, tomate, lechuga hidropÛnica.
    ## 331                                                                                                     Queso Pepper Jack, tocino, guacamole, lechuga, tomate, cebolla, pepinillos y salsa de la casa.
    ## 332                                                                                                             Queso cu·druple,Cristal Onion Sauce, Lechuga, Tocino grillado, Pepinillos y JalapeÒos.
    ## 333                                                   Hamburguesa de punta picana 200 grs, mostaza Dijon, cebolla caramelizada, salsa bernesa, queso Mantecoso y ensalada de r˙cula, con papas souflÈ.
    ## 334                                                                                                                                      SalmÛn ahumado y queso crema al cebollÌn, en bagel con sÈsamo
    ## 335                                                                               Carne mechada, con guacamole, jalapeÒos, pepinillos, mayonesa al ciboullette y cebolla caramelizada, en pan amasado.
    ## 336                                                                                 Churrasco de vacuno, cebolla caramelizada, queso fundido, jamÛn serrano, lechuga, champiÒones salteados y mayonesa
    ## 337                                                                                       Doble salchicha, repollo, zanahoria, cilantro, papas fritas, muchÌsimo queso, tocino, jamÛn y salsas varias.
    ## 338                                                                                                                           SalmÛn curado con sal al estilo finlandÈs, con queso crema y alcaparras.
    ## 339                                                                                                 Pan frica, queso Cheddar, tomate, cebolla caramelizada al bourbon, pesto de tomate seco y albahaca
    ## 340                                                                                                                                              Lomo liso, queso Azul, cebolla caramelizada y r˙cula.
    ## 341                                                       Milanesa, mix de lechuga y tomates del huerto de un amigo, pepinillos dulces, cebolla caramelizada y mayonesa, en un pan Ciabatta artesanal.
    ## 342                                                                                                                           Pancito tostado, tres capas de un churrasco, tocino, queso y champiÒones
    ## 343                                                                                                          Cerdo a la mostaza, cebolla caramelizada al vino tinto, queso de cabra, r˙cula y lechuga.
    ## 344                                                                                                                     Mechada de Cerdo, queso Mantecoso y repollo morado a la manzana, en pan bagel.
    ## 345                                                                                     Lechuga escarola, JamÛn planchado, Tocino, ChampiÒÛn, Salsa de queso cheddar y Salsa agridulce en pan amasado.
    ## 346                                                                                                         Soya, sÈsamo tostado, miel, jengibre y aceite de sÈsamo, con palta, lechuga y queso crema.
    ## 347                                                                                          Hamburguesa cubierta con crema de queso azul, Mermelada de cebolla, Lechuga, Pepinillos, Panceta ahumada.
    ## 348                                                                                                                             Pechuga de pollo a la crema con nueces, champiÒones y cebolla salteada
    ## 349                                                                                                                      Hamburguesa de pollo, mayonesa ahumada, coleslaw, sriracha y pickle de pepino
    ## 350                                                                                                                                                               Potito, longaniza y mayonesa casera.
    ## 351                                                                                                                  Pavo marinado y asado al horno, con zarza criolla y salsa t·rtara en pan Ciabatta
    ## 352                                                                   Pollo al cognac con pimentones asados al oliva y limÛn, r˙cula, papas hilo, ajÌ en escabeche, mostaza antigua y mayonesa casera.
    ## 353                                                                                                                 Varios. Tortilla de patatas, queso ibÈrico, calamares apanados, salmÛn, camarones.
    ## 354                                                                                                               Doble carne, queso fundido, cebolla, champiÒones, lechuga, tomate y mayonesa casera.
    ## 355                                                                                                                                    Carne de Wagy˙, queso Cheddar, cebolla frita, tocino y mayonesa
    ## 356                                                                          Hamburguesa, queso Cheddar, champiÒÛn Portobello frito relleno de quesos Cheddar y Muenster, lechuga, tomate y ShackSauce
    ## 357                                                                                                                                               S·ndwich de mechada con tomate, ajÌ verde y mayonesa
    ## 358                                                                                                                                    Pollo especiado y frito, Mayonesa, Zesty slaw, Pan potato roll.
    ## 359                                                   Hamburguesa, tocino ahumado, champiÒones, queso Cheddar, papas hilo, lechuga, cebolla caramelizada, pepinillo dill, house mayo y salsa honey bbq
    ## 360                                                                                                                                                       Pernil, mayonesa, palta, tomate, pepinillos.
    ## 361                                                                                    Langosta pequeÒa completa, mayonesa a las hierbas, tocino, lechuga y palta, en pan Brioche y papas hilo al lado
    ## 362                                                                                                                                           Queso americano, AjÌ cherry pepper, Tocino, Salsa Streat
    ## 363                                                                                                     Gorda, queso mantecoso, cebolla morada, tomate, ajÌ verde, chucrut y mayonesa danÈs al merkÈn.
    ## 364                                                         Trutro de pollo deshuesado y grillado, marinado en salsa anticuchera, con lechuga, tomate, queso Mantecoso, tocino crujiente y papas hilo.
    ## 365                                                                                                                                                                                                 NA
    ## 366                                                                                                                      Tocino ahumado, Lechuga, Palta, Sriracha, Mayonesa, Salsa tomate con cebolla.
    ## 367                                                                                                                                               SalmÛn ahumado, queso crema, lechuga, palta y berros
    ## 368                              Hamburguesa de champiÒones Portobello y legumbres, tomate y kÈtchup casero, queso Cheddar, cebollitas caramelizadas, mix verde y aderezo de zanahoria, en pan casero.
    ## 369                                                                                                                                               Arrollado de huaso, palta, mayonesa, en pan amasado.
    ## 370                                                                                                    Lomo liso ahumado en salsa de champiÒones, con lechuga, tomate y palta del valle del Aconcagua.
    ## 371                                                                                                                    Provoleta, Panceta ahumada, Aros de cebolla apanados y Salsa BBQ Jack Danielís.
    ## 372                                                                         Cheeseburger con pebre (palta, cilantro y tomate), con cebolla a la plancha, pepinillo y mayonesa, agregado de salsa Spicy
    ## 373                                                                                                                                                                   Carne rostizada,pepino agridulce
    ## 374                                                                                                 Churrasco de posta negra, brÛcoli, coliflor, salsa agridulce, tocino, cranberries y queso Cheddar.
    ## 375                                                                                                                                           JamÛn serrano,queso de HuentelauquÈn y pimientos asados.
    ## 376                                                                                                                              Hamburguesa de garbanzo en tempura de granola, pera, cebolla y r˙cula
    ## 377                                                                                                   AlbÛndigas de cerdo picante, queso Provolone y salsa Gravy con champiÒones, en baguette integral
    ## 378                                                                                                                Brisket sureÒo al BBQ, Cebolla frita, JalapeÒo, Queso cheddar, Pan de masa madre.\r
    ## 379                                                                  Hamburguesa casera de 170 gramos rellena con queso Cheddar, con quesos Mantecoso y Azul, tomates asados y albahaca, en pan frica.
    ## 380                                                                        Plateada al carmenere, champiÒones salteados, cebolla caramelizada, queso mantecoso derretido, lechuga y mayonesa al merkÈn
    ## 381                                                                                                                                                 Hamburguesa a la parrilla, queso azul y papas hilo
    ## 382                                                                                                                                 Corned Beef, queso suizo, chucrut y aliÒo ruso, en pan de centeno.
    ## 383                                                              Hamburguesa de araÒita de Wagyu, tomate, lechuga, mermelada de cebolla, tocino, pepinillos, queso mozzarella, servido en pan brioche.
    ## 384                                                     Hamburguesa Hereford, Tocino, Pulled pork, Queso, Aros de cebolla apanados y fritos, Lechuga, Tomate, Cebolla, Pepinillos, KÈtchup y Mayonesa.
    ## 385                                                                                                                Churrasco de posta rosada, queso derretido, tomate y mayonesa casera, en pan frica.
    ## 386                                                                                                                                                           Salchicha, palta, tomate, queso, chucrut
    ## 387                                                                          Tofu org·nico frito y glaseado con yangnyum, ManÌ molido, Mu encurtido, Lechuga, Mayonesa gochuyang, en un pan brioche.\r
    ## 388                                                                                                                                                      Lomo de cerdo, tomate, salsa verde y mayonesa
    ## 389                                                                                                                Churrasco Filete, lechuga, tomate, cebolla, queso, palta, pepinillos, en pan frica.
    ## 390                                                                                                                                          Lomo liso, r˙cula, grana padano, salsa CÈsar, sal de mar.
    ## 391                                                                                                                                                                  Mechada, palta, tomate, mayonesa.
    ## 392                                                                            Costillas de cerdo, pulled pork baÒado en salsa barbecue, con quesos Cheddar y Mantecoso, pepinillos y aros de cebolla.
    ## 393                                                                                     Brisket ahumado por 14 horas, Coliflor encurtida, Ensalada de kale, Alioli de ajo negro, Salsa BBQ , Pan turco
    ## 394                                                                                                     Milanesa, queso Azul, queso Parmesano, Cebolla Caramelizada, espinaca y champiÒones salteados.
    ## 395                                                                                                                                                                                      Pollo, palta.
    ## 396 Hamburguesa de vacuno Hereford de 175 grs, prieta grillada, tocino crujiente, manzanas caramelizadas, queso Mozzarella, lechuga y salsa casera de Honey Mustard, en pan brioche blanco con sÈsamo.
    ## 397                                                                                                     Queso cheddar, Onion rings, Tocino crujiente, AjÌ jalapeÒo, Lechuga, Tomate, Salsa BBQ casera.
    ## 398                                                                                                           Croquetas de Palmito con Coleslaw, Hojas Verdes y mayonesa de Sriracha, en pan Ciabatta.
    ## 399                                                                                                                    Zapallo italiano apanado, hummus, r˙cula, pimiento morrÛn y mayonesa de la casa
    ## 400                                                                  2 hamburguesas vegetarianas caseras, Salsa especial, Lechuga iceberg, Queso vegano, Cebolla,  Pepinillos, Pan brioche de 3 pisos.
    ## 401                                                                           Hamburguesa de carne de vacuno, Betarraga, Mix verde, Tocino, Huevo frito, ChampiÒÛn portobello, Queso, Relish y Alioli.
    ## 402                                                          Salchicha artesanal de 20 centÌmetros, con salsa de queso Cheddar, cebolla crispy, tocino crocante y salsa BBQ, m·s dos salsas a elecciÛn
    ## 403                                  ChampiÒones royal marinados en vinagre, especias, queso de cabra ahumado, cebolla al escabeche, tomate, lechuga hidropÛnica, pepinillos caseros y mayonesa vegana
    ## 404                                                                                                                     Mechada, porotos verdes, tomate, mayonesa casera, ajÌ verde y queso derretido.
    ## 405                                                                                                                Churrasco (posta negra), papas hilo, salsa de tocino, mermelada de cebolla y r˙cula
    ## 406                                                                                                                                           Queso Chedar, Salsa de la casa, lechuga, tomate, tocino.
    ## 407                                                                                                                                                                                                 NA
    ## 408                                                                                           Pan pita, champiÒÛn horneado en leche de coco, curry rojo, lechuga, salsa ·cida y chips de pl·tano frito
    ## 409                                                                                                                                            Lomo saltado, papas hilo, tocino crocante y huevo frito
    ## 410                                                                                                                                                                                 Churrasco y palta.
    ##     nota
    ## 1      3
    ## 2      3
    ## 3      4
    ## 4      4
    ## 5      4
    ## 6      3
    ## 7      3
    ## 8      3
    ## 9      3
    ## 10     3
    ## 11     4
    ## 12     3
    ## 13     3
    ## 14     2
    ## 15     4
    ## 16     2
    ## 17     4
    ## 18     3
    ## 19     3
    ## 20     5
    ## 21     4
    ## 22     5
    ## 23     3
    ## 24     3
    ## 25     3
    ## 26     4
    ## 27     3
    ## 28     2
    ## 29     2
    ## 30     1
    ## 31     3
    ## 32     3
    ## 33     4
    ## 34     5
    ## 35     4
    ## 36     5
    ## 37     4
    ## 38     3
    ## 39     3
    ## 40     4
    ## 41     2
    ## 42     5
    ## 43     4
    ## 44     3
    ## 45     1
    ## 46     3
    ## 47     3
    ## 48     3
    ## 49     3
    ## 50     4
    ## 51     3
    ## 52     5
    ## 53     2
    ## 54     3
    ## 55     3
    ## 56    NA
    ## 57     2
    ## 58     3
    ## 59     5
    ## 60     3
    ## 61     3
    ## 62     1
    ## 63     3
    ## 64     4
    ## 65     2
    ## 66     3
    ## 67     5
    ## 68     5
    ## 69     3
    ## 70     4
    ## 71     3
    ## 72     3
    ## 73     1
    ## 74     5
    ## 75     3
    ## 76     3
    ## 77     4
    ## 78     5
    ## 79    NA
    ## 80     4
    ## 81     4
    ## 82     3
    ## 83     3
    ## 84     5
    ## 85     5
    ## 86     3
    ## 87     3
    ## 88     4
    ## 89     4
    ## 90     3
    ## 91     2
    ## 92     3
    ## 93     4
    ## 94     3
    ## 95     3
    ## 96     5
    ## 97     4
    ## 98     3
    ## 99     3
    ## 100    3
    ## 101    4
    ## 102    4
    ## 103    3
    ## 104    3
    ## 105    3
    ## 106    2
    ## 107    5
    ## 108    2
    ## 109    2
    ## 110    3
    ## 111    5
    ## 112    3
    ## 113    1
    ## 114    4
    ## 115    4
    ## 116    3
    ## 117    2
    ## 118    3
    ## 119    3
    ## 120    4
    ## 121    1
    ## 122    2
    ## 123    3
    ## 124    1
    ## 125    3
    ## 126    4
    ## 127    5
    ## 128    4
    ## 129    2
    ## 130    3
    ## 131    1
    ## 132    3
    ## 133    2
    ## 134    1
    ## 135    4
    ## 136    3
    ## 137    2
    ## 138    4
    ## 139    2
    ## 140    3
    ## 141    2
    ## 142    1
    ## 143    3
    ## 144    4
    ## 145    2
    ## 146    3
    ## 147    3
    ## 148    1
    ## 149    2
    ## 150   NA
    ## 151    2
    ## 152    3
    ## 153    3
    ## 154    4
    ## 155    3
    ## 156    3
    ## 157    5
    ## 158    5
    ## 159    3
    ## 160    5
    ## 161    3
    ## 162    5
    ## 163    4
    ## 164    1
    ## 165    3
    ## 166    3
    ## 167    1
    ## 168    3
    ## 169    3
    ## 170   NA
    ## 171    3
    ## 172    1
    ## 173    2
    ## 174    5
    ## 175    5
    ## 176    2
    ## 177    4
    ## 178    2
    ## 179    4
    ## 180    4
    ## 181    5
    ## 182    3
    ## 183    1
    ## 184    3
    ## 185    2
    ## 186    5
    ## 187    3
    ## 188    5
    ## 189    3
    ## 190    5
    ## 191    4
    ## 192    2
    ## 193    3
    ## 194    3
    ## 195    5
    ## 196    2
    ## 197    5
    ## 198    2
    ## 199    3
    ## 200    5
    ## 201    4
    ## 202    5
    ## 203    3
    ## 204    4
    ## 205    2
    ## 206    4
    ## 207    3
    ## 208    2
    ## 209    4
    ## 210    5
    ## 211    3
    ## 212    1
    ## 213    4
    ## 214    5
    ## 215    5
    ## 216    5
    ## 217    2
    ## 218    4
    ## 219    4
    ## 220    2
    ## 221    2
    ## 222    4
    ## 223    1
    ## 224    3
    ## 225    4
    ## 226    4
    ## 227    2
    ## 228    5
    ## 229    3
    ## 230    4
    ## 231    3
    ## 232    5
    ## 233    3
    ## 234    3
    ## 235    2
    ## 236    2
    ## 237    2
    ## 238    5
    ## 239    4
    ## 240    3
    ## 241    5
    ## 242    5
    ## 243    3
    ## 244    4
    ## 245    3
    ## 246    2
    ## 247    5
    ## 248    3
    ## 249    2
    ## 250    4
    ## 251    3
    ## 252    4
    ## 253    4
    ## 254    3
    ## 255    4
    ## 256    5
    ## 257    4
    ## 258    4
    ## 259    3
    ## 260    3
    ## 261    3
    ## 262    3
    ## 263    3
    ## 264    5
    ## 265    4
    ## 266    5
    ## 267    3
    ## 268    3
    ## 269    3
    ## 270    3
    ## 271    3
    ## 272    4
    ## 273    3
    ## 274    4
    ## 275    3
    ## 276    3
    ## 277    3
    ## 278    2
    ## 279    4
    ## 280    3
    ## 281    2
    ## 282    3
    ## 283    2
    ## 284    5
    ## 285    3
    ## 286    3
    ## 287    1
    ## 288    3
    ## 289    1
    ## 290    3
    ## 291    3
    ## 292    5
    ## 293    3
    ## 294    4
    ## 295    1
    ## 296    1
    ## 297    2
    ## 298    4
    ## 299    3
    ## 300    1
    ## 301    3
    ## 302    3
    ## 303    3
    ## 304    3
    ## 305    3
    ## 306    4
    ## 307    4
    ## 308    1
    ## 309    3
    ## 310    3
    ## 311    2
    ## 312    3
    ## 313    4
    ## 314    1
    ## 315    4
    ## 316    3
    ## 317    3
    ## 318    3
    ## 319    2
    ## 320    4
    ## 321    5
    ## 322    1
    ## 323    1
    ## 324    2
    ## 325    4
    ## 326    3
    ## 327    3
    ## 328    1
    ## 329    5
    ## 330    2
    ## 331    2
    ## 332    4
    ## 333    4
    ## 334    4
    ## 335    3
    ## 336    1
    ## 337    1
    ## 338    1
    ## 339    3
    ## 340    4
    ## 341    3
    ## 342    3
    ## 343    4
    ## 344    3
    ## 345    4
    ## 346    5
    ## 347    5
    ## 348    3
    ## 349    3
    ## 350    2
    ## 351    3
    ## 352    4
    ## 353    2
    ## 354    3
    ## 355    1
    ## 356    4
    ## 357    1
    ## 358    5
    ## 359    3
    ## 360    1
    ## 361    3
    ## 362    5
    ## 363    2
    ## 364    4
    ## 365   NA
    ## 366    3
    ## 367    2
    ## 368    5
    ## 369    2
    ## 370    4
    ## 371    2
    ## 372    3
    ## 373    1
    ## 374    3
    ## 375    5
    ## 376    1
    ## 377    2
    ## 378    4
    ## 379    4
    ## 380    3
    ## 381    3
    ## 382    3
    ## 383    5
    ## 384    1
    ## 385    3
    ## 386    2
    ## 387    4
    ## 388    2
    ## 389    3
    ## 390    3
    ## 391    3
    ## 392    2
    ## 393   NA
    ## 394    2
    ## 395    3
    ## 396    3
    ## 397    3
    ## 398    4
    ## 399    5
    ## 400    3
    ## 401   NA
    ## 402    4
    ## 403    4
    ## 404    4
    ## 405    3
    ## 406    3
    ## 407   NA
    ## 408    3
    ## 409    4
    ## 410    2

## Seleccionamos solo los datos que tienen nota 5, ya que son los mejores

``` r
sanguche_final = datos_analisis1 [datos_analisis1$nota >=5,]

#cambie nombre de las columnas

names (sanguche_final)= c("ingredientes", "notas")
sanguche_final
```

    ##                                                                                                                                                                              ingredientes
    ## 20                                                                              Pato, queso Pepper Jack, jalapeÒos, cebolla morada, espinaca, tomate y mayonesa Habanero, en pan Brioche.
    ## 22                                                                       Hamburguesa, Queso americano, Tocino glaseado con miel de maple, Alioli con miel de maple, Poutine, Pan de leche
    ## 34                                                                                             Carne, zucchini grillado con un toque de ajo, pimentÛn acaramelado, champiÒones y ricotta.
    ## 36                                                                               Filete de res, con relish de betarraga, raÌz picante (r·bano), aros de cebolla y champiÒones Portobellos
    ## 42                                                                                    Zapallo japonÈs rostizado, Mayonesa al chilli, R˙cula, Garbanzos, Queso grana padano, Pan focaccia.
    ## 52                                                                         Hamburguesa 50% carne y 50% prieta, queso parmesano, manzana verde caramelizada, palta, r˙cula y salsa al ajo.
    ## 59                                                                           SalmÛn en mantequilla aromatizada, tomates asados, queso crema saborizado, corazÛn de alcachofa y mix verde.
    ## 67                                         Crudo de res machacado, con crema ·cida al ciboullette, alcaparras, pepinillos en vinagre, chucrut de repollo morado y lechuga en sopaipillas.
    ## 68                                               Sierra ahumada desmenuzada, con mayonesa de ajo, perejil y limÛn, sobre una cama de lechuga costina y pebre de cochayuyo, en sopaipilla.
    ## 74                                                                                                                               Mechada, palta, tomate y mayonesa casera, en sopaipillas
    ## 78                                                                                                                                         Lomo de cerdo, palta, tomate, mayonesa casera.
    ## 84                                                                                        Reineta frita, lechuga, tomate, cebolla morada y blanca, ajÌes blanqueados y salsa al cilantro.
    ## 85                                                                                                     Longaniza de jabalÌ con cebolla, ajÌ amarillo, r˙cula y zanahoria, en pan de queso
    ## 96                                                                                         AlbÛndigas de quinoa crocante, hojas verdes, huevo frito, brotes de alfalfa y chips de camote.
    ## 107                                                                     Carne mechada, cebolla morada, tomate fresco, ajÌ verde, agregado chancho en piedra y merkÈn, en marraqueta y 1/2
    ## 111                                                                                                                                      Lengua, porotos verdes, mayonesa casera, tomate.
    ## 127                                                                                              Cebolla caramelizada, Queso mantecoso derretido, Huevo frito, Kiss my hass (palta frita)
    ## 157                                                                                  Salsa de queso azul, pepinillos, cebolla caramelizada, queso Mantecoso, espinaca y salsa de la casa.
    ## 158                                                Pulled chicken ahumado, queso de cabra, salsa Teriyaki, chips de betarraga frita, mayonesa de cilantro y berros en pan francÈs casero.
    ## 160                                                                                                                                          Ajo, AjÌ, Mayonesa, Salsa de tomate, Chucrut
    ## 162                                                                          Hamburguesa de carne de vacuno y cerdo molida condimentada con ajo y ajÌ, tomate, chucrut y mayonesa casera.
    ## 174                                                                           Relleno de pollo a la plancha, berros, salsa de cilantro, y un mix de champiÒones con queso crema derretido
    ## 175                                                                  Hamburguesa dry-aged, Queso provolone, Tocino glaseado con miel de maple, JalapeÒo, Relish de piÒa, Mayonesa trufada
    ## 181                                                                                                                Queso, Tomate, Lechuga, Cebolla, Pepinillos, Mostaza, Ketchup, Tocino.
    ## 186                                                                                                             Mix punta paleta, tocino y punta ganso, Tocino, queso, encurtidos, Salsa.
    ## 188 Costillar de cerdo al merkÈn, braseado y desmenuzado con cebollas caramelizadas, con tomates confitados al orÈgano y fonduta de quesos, montado en un exquisito pan amasado especial.
    ## 190                                                                               Queso cheddar, Queso Havarti, Costillitas de vacuno desmenuzada, Cebolla caramelizada, Cebolla crunchy.
    ## 195                                                                                                          Queso crema, Aceitunas verdes, Queso Las ¡guilas, Sal al merkÈn, Mantequilla
    ## 197                                                          Mejillas de res cocinadas por 12 horas, Mayonesa de r·bano, Queso, Cebolla morada, R˙cula, Salsa secreta, Pan de masa madre.
    ## 200                                                                                                                Churrasco filete, tomate, porotos verdes, ajÌ verde. Agregado mayonesa
    ## 202                                                                                                                                                 Pastrami y mostaza, en pan de centeno
    ## 210                                                                                                                 Camarones fritos en panko, Palta, Queso mozzarella ,Salsa sweet chili
    ## 214                                        Salsa La BirrerÌa, Lechuga, Tomate, Queso de cabra, Queso mozzarella, ChampiÒones salteados, Tocino ahumado crocante, Aros de cebolla apanados
    ## 215                                    Hamburguesa de 3 cortes de carne, base de lechuga y tomate, queso Cheddar, y ajÌes verdes rellenos con Mozzarella, tocino crocante, mayonesa spicy
    ## 216                                                                                         Lechuga, Tomate, Queso Cheddar, AjÌes verdes rellenos con mozzarella y tocino, Mayonesa spicy
    ## 228                                                               Costillar de cerdo 100% org·nico de cocciÛn lenta con salsa BBQ casera, ensaladilla criolla con palta, en pan Ciabatta.
    ## 232                                                                                                  Plateada de vacuno, pastelera de choclo, papas hilo, huevo frito y cebolla rebosada.
    ## 238                                                                                                                                 Carne Mechada, palta, tomate, mayonesa, en marraqueta
    ## 241                                                                                                           Lomo de res, cebolla morada, pimentÛn y papas hilo, en pan ciabatta peruano
    ## 242                                                                                Pan focaccia de masa madre, Berenjenas, Tomates asados, Ricotta frÌa, Pesto de la casa y Aceite de ajo
    ## 247                                                                                                     Chorizo argentino, chimichurri, huevo frito y cebolla caramelizada, en marraqueta
    ## 256                                                                                                                                    Mechada, salsa de tomate, palta, mayonesa, tomate.
    ## 264                                                                                                                              EntraÒas, chorizo, pebre, cebolla asada, mayonera casada
    ## 266                                                         Milanesa de posta (apanada sin harina), con queso Mozzarella, cebollita frita, salsa de tomates casera y un toque de orÈgano.
    ## 284                                               Hamburguesa (340 grs de carne), con doble queso Cheddar, tocino crocante, aros de cebolla apanados, mayonesa de eneldo y cebolla morada
    ## 292                            Hamburguesa de porotos negros, con cebolla caramelizada, veganesa de cilantro, lechuga, betarraga y zanahoria en juliana y salsa barbecue hecha ahÌ mismo.
    ## 321                                                                                                          Doble hamburguesa, queso americano, ajÌ cherry pepper, tocino y salsa Streat
    ## 329                                                             Pollo deshilachado en curry y leche de coco, con tiras de zanahoria, zapallo italiano, pimentÛn, cebolla morada y lechuga
    ## 346                                                                                            Soya, sÈsamo tostado, miel, jengibre y aceite de sÈsamo, con palta, lechuga y queso crema.
    ## 347                                                                             Hamburguesa cubierta con crema de queso azul, Mermelada de cebolla, Lechuga, Pepinillos, Panceta ahumada.
    ## 358                                                                                                                       Pollo especiado y frito, Mayonesa, Zesty slaw, Pan potato roll.
    ## 362                                                                                                                              Queso americano, AjÌ cherry pepper, Tocino, Salsa Streat
    ## 368                 Hamburguesa de champiÒones Portobello y legumbres, tomate y kÈtchup casero, queso Cheddar, cebollitas caramelizadas, mix verde y aderezo de zanahoria, en pan casero.
    ## 375                                                                                                                              JamÛn serrano,queso de HuentelauquÈn y pimientos asados.
    ## 383                                                 Hamburguesa de araÒita de Wagyu, tomate, lechuga, mermelada de cebolla, tocino, pepinillos, queso mozzarella, servido en pan brioche.
    ## 399                                                                                                       Zapallo italiano apanado, hummus, r˙cula, pimiento morrÛn y mayonesa de la casa
    ##     notas
    ## 20      5
    ## 22      5
    ## 34      5
    ## 36      5
    ## 42      5
    ## 52      5
    ## 59      5
    ## 67      5
    ## 68      5
    ## 74      5
    ## 78      5
    ## 84      5
    ## 85      5
    ## 96      5
    ## 107     5
    ## 111     5
    ## 127     5
    ## 157     5
    ## 158     5
    ## 160     5
    ## 162     5
    ## 174     5
    ## 175     5
    ## 181     5
    ## 186     5
    ## 188     5
    ## 190     5
    ## 195     5
    ## 197     5
    ## 200     5
    ## 202     5
    ## 210     5
    ## 214     5
    ## 215     5
    ## 216     5
    ## 228     5
    ## 232     5
    ## 238     5
    ## 241     5
    ## 242     5
    ## 247     5
    ## 256     5
    ## 264     5
    ## 266     5
    ## 284     5
    ## 292     5
    ## 321     5
    ## 329     5
    ## 346     5
    ## 347     5
    ## 358     5
    ## 362     5
    ## 368     5
    ## 375     5
    ## 383     5
    ## 399     5

## se crea una tabla solo con los ingredientes de las preparaciones, está tabla no fue utilizada finalmente ya que no se logró aprender a codificar a tiempo

``` r
tabla_ingredientes <- str_split_fixed(sanguche_final$ingredientes, pattern = ",", n=8)


tabla_ingredientes<-as.data.frame(tabla_ingredientes)
#se cambian los nombres
names (tabla_ingredientes)= c("ingrediente1", "ingrediente2","ingrediente3", "ingrediente4","ingrediente5", "ingrediente6","ingrediente7", "ingrediente8")
tabla_ingredientes
```

    ##                                                                ingrediente1
    ## 1                                                                      Pato
    ## 2                                                               Hamburguesa
    ## 3                                                                     Carne
    ## 4                                                             Filete de res
    ## 5                                                 Zapallo japonÈs rostizado
    ## 6                                        Hamburguesa 50% carne y 50% prieta
    ## 7                                         SalmÛn en mantequilla aromatizada
    ## 8                                                    Crudo de res machacado
    ## 9                                                Sierra ahumada desmenuzada
    ## 10                                                                  Mechada
    ## 11                                                            Lomo de cerdo
    ## 12                                                            Reineta frita
    ## 13                                          Longaniza de jabalÌ con cebolla
    ## 14                                            AlbÛndigas de quinoa crocante
    ## 15                                                            Carne mechada
    ## 16                                                                   Lengua
    ## 17                                                     Cebolla caramelizada
    ## 18                                                      Salsa de queso azul
    ## 19                                                   Pulled chicken ahumado
    ## 20                                                                      Ajo
    ## 21 Hamburguesa de carne de vacuno y cerdo molida condimentada con ajo y ajÌ
    ## 22                                            Relleno de pollo a la plancha
    ## 23                                                     Hamburguesa dry-aged
    ## 24                                                                    Queso
    ## 25                                                         Mix punta paleta
    ## 26                                             Costillar de cerdo al merkÈn
    ## 27                                                            Queso cheddar
    ## 28                                                              Queso crema
    ## 29                                   Mejillas de res cocinadas por 12 horas
    ## 30                                                         Churrasco filete
    ## 31                                                       Pastrami y mostaza
    ## 32                                                Camarones fritos en panko
    ## 33                                                        Salsa La BirrerÌa
    ## 34                                         Hamburguesa de 3 cortes de carne
    ## 35                                                                  Lechuga
    ## 36   Costillar de cerdo 100% org·nico de cocciÛn lenta con salsa BBQ casera
    ## 37                                                       Plateada de vacuno
    ## 38                                                            Carne Mechada
    ## 39                                                              Lomo de res
    ## 40                                               Pan focaccia de masa madre
    ## 41                                                        Chorizo argentino
    ## 42                                                                  Mechada
    ## 43                                                                 EntraÒas
    ## 44                                   Milanesa de posta (apanada sin harina)
    ## 45                                           Hamburguesa (340 grs de carne)
    ## 46                                            Hamburguesa de porotos negros
    ## 47                                                        Doble hamburguesa
    ## 48                              Pollo deshilachado en curry y leche de coco
    ## 49                                                                     Soya
    ## 50                             Hamburguesa cubierta con crema de queso azul
    ## 51                                                  Pollo especiado y frito
    ## 52                                                          Queso americano
    ## 53                        Hamburguesa de champiÒones Portobello y legumbres
    ## 54                                                            JamÛn serrano
    ## 55                                          Hamburguesa de araÒita de Wagyu
    ## 56                                                 Zapallo italiano apanado
    ##                                          ingrediente2
    ## 1                                   queso Pepper Jack
    ## 2                                     Queso americano
    ## 3               zucchini grillado con un toque de ajo
    ## 4                             con relish de betarraga
    ## 5                                  Mayonesa al chilli
    ## 6                                     queso parmesano
    ## 7                                      tomates asados
    ## 8                      con crema ·cida al ciboullette
    ## 9                                 con mayonesa de ajo
    ## 10                                              palta
    ## 11                                              palta
    ## 12                                            lechuga
    ## 13                                       ajÌ amarillo
    ## 14                                       hojas verdes
    ## 15                                     cebolla morada
    ## 16                                     porotos verdes
    ## 17                          Queso mantecoso derretido
    ## 18                                         pepinillos
    ## 19                                     queso de cabra
    ## 20                                                AjÌ
    ## 21                                             tomate
    ## 22                                             berros
    ## 23                                    Queso provolone
    ## 24                                             Tomate
    ## 25                               tocino y punta ganso
    ## 26  braseado y desmenuzado con cebollas caramelizadas
    ## 27                                      Queso Havarti
    ## 28                                   Aceitunas verdes
    ## 29                                 Mayonesa de r·bano
    ## 30                                             tomate
    ## 31                                  en pan de centeno
    ## 32                                              Palta
    ## 33                                            Lechuga
    ## 34                           base de lechuga y tomate
    ## 35                                             Tomate
    ## 36                      ensaladilla criolla con palta
    ## 37                                pastelera de choclo
    ## 38                                              palta
    ## 39                                     cebolla morada
    ## 40                                         Berenjenas
    ## 41                                        chimichurri
    ## 42                                    salsa de tomate
    ## 43                                            chorizo
    ## 44                               con queso Mozzarella
    ## 45                            con doble queso Cheddar
    ## 46                           con cebolla caramelizada
    ## 47                                    queso americano
    ## 48                             con tiras de zanahoria
    ## 49                                     sÈsamo tostado
    ## 50                               Mermelada de cebolla
    ## 51                                           Mayonesa
    ## 52                                  AjÌ cherry pepper
    ## 53                            tomate y kÈtchup casero
    ## 54         queso de HuentelauquÈn y pimientos asados.
    ## 55                                             tomate
    ## 56                                             hummus
    ##                                              ingrediente3
    ## 1                                               jalapeÒos
    ## 2                       Tocino glaseado con miel de maple
    ## 3                                    pimentÛn acaramelado
    ## 4                                   raÌz picante (r·bano)
    ## 5                                                  R˙cula
    ## 6                              manzana verde caramelizada
    ## 7                                  queso crema saborizado
    ## 8                                              alcaparras
    ## 9                                         perejil y limÛn
    ## 10                               tomate y mayonesa casera
    ## 11                                                 tomate
    ## 12                                                 tomate
    ## 13                                     r˙cula y zanahoria
    ## 14                                            huevo frito
    ## 15                                          tomate fresco
    ## 16                                        mayonesa casera
    ## 17                                            Huevo frito
    ## 18                                   cebolla caramelizada
    ## 19                                         salsa Teriyaki
    ## 20                                               Mayonesa
    ## 21                             chucrut y mayonesa casera.
    ## 22                                      salsa de cilantro
    ## 23                      Tocino glaseado con miel de maple
    ## 24                                                Lechuga
    ## 25                                                 Tocino
    ## 26  con tomates confitados al orÈgano y fonduta de quesos
    ## 27                      Costillitas de vacuno desmenuzada
    ## 28                                      Queso Las ¡guilas
    ## 29                                                  Queso
    ## 30                                         porotos verdes
    ## 31                                                       
    ## 32                                      Queso mozzarella 
    ## 33                                                 Tomate
    ## 34                                          queso Cheddar
    ## 35                                          Queso Cheddar
    ## 36                                       en pan Ciabatta.
    ## 37                                             papas hilo
    ## 38                                                 tomate
    ## 39                                  pimentÛn y papas hilo
    ## 40                                         Tomates asados
    ## 41                     huevo frito y cebolla caramelizada
    ## 42                                                  palta
    ## 43                                                  pebre
    ## 44                                        cebollita frita
    ## 45                                        tocino crocante
    ## 46                                   veganesa de cilantro
    ## 47                                      ajÌ cherry pepper
    ## 48                                       zapallo italiano
    ## 49                                                   miel
    ## 50                                                Lechuga
    ## 51                                             Zesty slaw
    ## 52                                                 Tocino
    ## 53                                          queso Cheddar
    ## 54                                                       
    ## 55                                                lechuga
    ## 56                                                 r˙cula
    ##                                               ingrediente4
    ## 1                                           cebolla morada
    ## 2                                 Alioli con miel de maple
    ## 3                                   champiÒones y ricotta.
    ## 4                aros de cebolla y champiÒones Portobellos
    ## 5                                                Garbanzos
    ## 6                                                    palta
    ## 7                        corazÛn de alcachofa y mix verde.
    ## 8                                    pepinillos en vinagre
    ## 9   sobre una cama de lechuga costina y pebre de cochayuyo
    ## 10                                          en sopaipillas
    ## 11                                        mayonesa casera.
    ## 12                                 cebolla morada y blanca
    ## 13                                         en pan de queso
    ## 14                    brotes de alfalfa y chips de camote.
    ## 15                                               ajÌ verde
    ## 16                                                 tomate.
    ## 17                              Kiss my hass (palta frita)
    ## 18                                         queso Mantecoso
    ## 19                                chips de betarraga frita
    ## 20                                         Salsa de tomate
    ## 21                                                        
    ## 22       y un mix de champiÒones con queso crema derretido
    ## 23                                                JalapeÒo
    ## 24                                                 Cebolla
    ## 25                                                   queso
    ## 26           montado en un exquisito pan amasado especial.
    ## 27                                    Cebolla caramelizada
    ## 28                                           Sal al merkÈn
    ## 29                                          Cebolla morada
    ## 30                            ajÌ verde. Agregado mayonesa
    ## 31                                                        
    ## 32                                       Salsa sweet chili
    ## 33                                          Queso de cabra
    ## 34                  y ajÌes verdes rellenos con Mozzarella
    ## 35           AjÌes verdes rellenos con mozzarella y tocino
    ## 36                                                        
    ## 37                         huevo frito y cebolla rebosada.
    ## 38                                                mayonesa
    ## 39                                 en pan ciabatta peruano
    ## 40                                            Ricotta frÌa
    ## 41                                           en marraqueta
    ## 42                                                mayonesa
    ## 43                                           cebolla asada
    ## 44          salsa de tomates casera y un toque de orÈgano.
    ## 45                                aros de cebolla apanados
    ## 46                                                 lechuga
    ## 47                                   tocino y salsa Streat
    ## 48                                                pimentÛn
    ## 49                             jengibre y aceite de sÈsamo
    ## 50                                              Pepinillos
    ## 51                                        Pan potato roll.
    ## 52                                            Salsa Streat
    ## 53                                cebollitas caramelizadas
    ## 54                                                        
    ## 55                                    mermelada de cebolla
    ## 56                   pimiento morrÛn y mayonesa de la casa
    ##                                                           ingrediente5
    ## 1                                                             espinaca
    ## 2                                                              Poutine
    ## 3                                                                     
    ## 4                                                                     
    ## 5                                                   Queso grana padano
    ## 6                                               r˙cula y salsa al ajo.
    ## 7                                                                     
    ## 8                  chucrut de repollo morado y lechuga en sopaipillas.
    ## 9                                                       en sopaipilla.
    ## 10                                                                    
    ## 11                                                                    
    ## 12                              ajÌes blanqueados y salsa al cilantro.
    ## 13                                                                    
    ## 14                                                                    
    ## 15                                 agregado chancho en piedra y merkÈn
    ## 16                                                                    
    ## 17                                                                    
    ## 18                                        espinaca y salsa de la casa.
    ## 19                mayonesa de cilantro y berros en pan francÈs casero.
    ## 20                                                             Chucrut
    ## 21                                                                    
    ## 22                                                                    
    ## 23                                                      Relish de piÒa
    ## 24                                                          Pepinillos
    ## 25                                                          encurtidos
    ## 26                                                                    
    ## 27                                                    Cebolla crunchy.
    ## 28                                                         Mantequilla
    ## 29                                                              R˙cula
    ## 30                                                                    
    ## 31                                                                    
    ## 32                                                                    
    ## 33                                                    Queso mozzarella
    ## 34                                                     tocino crocante
    ## 35                                                      Mayonesa spicy
    ## 36                                                                    
    ## 37                                                                    
    ## 38                                                       en marraqueta
    ## 39                                                                    
    ## 40                                    Pesto de la casa y Aceite de ajo
    ## 41                                                                    
    ## 42                                                             tomate.
    ## 43                                                     mayonera casada
    ## 44                                                                    
    ## 45                                 mayonesa de eneldo y cebolla morada
    ## 46  betarraga y zanahoria en juliana y salsa barbecue hecha ahÌ mismo.
    ## 47                                                                    
    ## 48                                            cebolla morada y lechuga
    ## 49                                                           con palta
    ## 50                                                    Panceta ahumada.
    ## 51                                                                    
    ## 52                                                                    
    ## 53                                    mix verde y aderezo de zanahoria
    ## 54                                                                    
    ## 55                                                              tocino
    ## 56                                                                    
    ##                   ingrediente6             ingrediente7
    ## 1   tomate y mayonesa Habanero          en pan Brioche.
    ## 2                 Pan de leche                         
    ## 3                                                      
    ## 4                                                      
    ## 5                Pan focaccia.                         
    ## 6                                                      
    ## 7                                                      
    ## 8                                                      
    ## 9                                                      
    ## 10                                                     
    ## 11                                                     
    ## 12                                                     
    ## 13                                                     
    ## 14                                                     
    ## 15         en marraqueta y 1/2                         
    ## 16                                                     
    ## 17                                                     
    ## 18                                                     
    ## 19                                                     
    ## 20                                                     
    ## 21                                                     
    ## 22                                                     
    ## 23            Mayonesa trufada                         
    ## 24                     Mostaza                  Ketchup
    ## 25                      Salsa.                         
    ## 26                                                     
    ## 27                                                     
    ## 28                                                     
    ## 29               Salsa secreta       Pan de masa madre.
    ## 30                                                     
    ## 31                                                     
    ## 32                                                     
    ## 33       ChampiÒones salteados  Tocino ahumado crocante
    ## 34              mayonesa spicy                         
    ## 35                                                     
    ## 36                                                     
    ## 37                                                     
    ## 38                                                     
    ## 39                                                     
    ## 40                                                     
    ## 41                                                     
    ## 42                                                     
    ## 43                                                     
    ## 44                                                     
    ## 45                                                     
    ## 46                                                     
    ## 47                                                     
    ## 48                                                     
    ## 49      lechuga y queso crema.                         
    ## 50                                                     
    ## 51                                                     
    ## 52                                                     
    ## 53              en pan casero.                         
    ## 54                                                     
    ## 55                  pepinillos         queso mozzarella
    ## 56                                                     
    ##                 ingrediente8
    ## 1                           
    ## 2                           
    ## 3                           
    ## 4                           
    ## 5                           
    ## 6                           
    ## 7                           
    ## 8                           
    ## 9                           
    ## 10                          
    ## 11                          
    ## 12                          
    ## 13                          
    ## 14                          
    ## 15                          
    ## 16                          
    ## 17                          
    ## 18                          
    ## 19                          
    ## 20                          
    ## 21                          
    ## 22                          
    ## 23                          
    ## 24                   Tocino.
    ## 25                          
    ## 26                          
    ## 27                          
    ## 28                          
    ## 29                          
    ## 30                          
    ## 31                          
    ## 32                          
    ## 33  Aros de cebolla apanados
    ## 34                          
    ## 35                          
    ## 36                          
    ## 37                          
    ## 38                          
    ## 39                          
    ## 40                          
    ## 41                          
    ## 42                          
    ## 43                          
    ## 44                          
    ## 45                          
    ## 46                          
    ## 47                          
    ## 48                          
    ## 49                          
    ## 50                          
    ## 51                          
    ## 52                          
    ## 53                          
    ## 54                          
    ## 55   servido en pan brioche.
    ## 56

## Pasamos los datos a una matriz que cuenta cuantas veces se repiten las palabras, antes eliminamos los espacios, para que de esta forma no afectaran en lacomparativa

``` r
texto <- sanguche_final$ingredientes



texto<-gsub(" ","", texto)
texto <- char_tolower(texto)
texto <- iconv(texto, to = "ASCII//TRANSLIT")

#se genera la tabla con la repeticion de los nombres

matriz_ingre <- dfm(texto, remove = c(stopwords("es"), "vs", ",","´",".","num","?","(",")","!","%","/"))
```

    ## Warning: NA is replaced by empty string

``` r
matriz_ingre
```

    ## Document-feature matrix of: 56 documents, 216 features (97.7% sparse).
    ##        features
    ## docs    pato quesopepperjack jalape oos cebollamorada espinaca
    ##   text1    1               1      1   1             1        1
    ##   text2    0               0      0   0             0        0
    ##   text3    0               0      0   0             0        0
    ##   text4    0               0      0   0             0        0
    ##   text5    0               0      0   0             0        0
    ##   text6    0               0      0   0             0        0
    ##        features
    ## docs    tomateymayonesahabanero enpanbrioche hamburguesa quesoamericano
    ##   text1                       1            1           0              0
    ##   text2                       0            0           1              1
    ##   text3                       0            0           0              0
    ##   text4                       0            0           0              0
    ##   text5                       0            0           0              0
    ##   text6                       0            0           0              0
    ## [ reached max_ndoc ... 50 more documents, reached max_nfeat ... 206 more features ]

## se cuenta la cantidad de veces que se repite el ingrediente

``` r
Total = colSums (matriz_ingre)
Total
```

    ##                                                        pato 
    ##                                                           1 
    ##                                             quesopepperjack 
    ##                                                           1 
    ##                                                      jalape 
    ##                                                           2 
    ##                                                         oos 
    ##                                                           1 
    ##                                               cebollamorada 
    ##                                                           3 
    ##                                                    espinaca 
    ##                                                           1 
    ##                                     tomateymayonesahabanero 
    ##                                                           1 
    ##                                                enpanbrioche 
    ##                                                           1 
    ##                                                 hamburguesa 
    ##                                                           2 
    ##                                              quesoamericano 
    ##                                                           3 
    ##                                tocinoglaseadoconmieldemaple 
    ##                                                           2 
    ##                                        alioliconmieldemaple 
    ##                                                           1 
    ##                                                     poutine 
    ##                                                           1 
    ##                                                  pandeleche 
    ##                                                           1 
    ##                                                       carne 
    ##                                                           1 
    ##                             zucchinigrilladoconuntoquedeajo 
    ##                                                           1 
    ##                                                      piment 
    ##                                                           3 
    ##                                               unacaramelado 
    ##                                                           1 
    ##                                                      champi 
    ##                                                           2 
    ##                                               oonesyricotta 
    ##                                                           1 
    ##                                                 filetederes 
    ##                                                           1 
    ##                                        conrelishdebetarraga 
    ##                                                           1 
    ##                                                          ra 
    ##                                                           1 
    ##                                                   izpicante 
    ##                                                           1 
    ##                                                      r.bano 
    ##                                                           1 
    ##                                        arosdecebollaychampi 
    ##                                                           1 
    ##                                            oonesportobellos 
    ##                                                           1 
    ##                                                        salm 
    ##                                                           1 
    ##                                  unenmantequillaaromatizada 
    ##                                                           1 
    ##                                               tomatesasados 
    ##                                                           2 
    ##                                        quesocremasaborizado 
    ##                                                           1 
    ##                                                       coraz 
    ##                                                           1 
    ##                                      undealcachofaymixverde 
    ##                                                           1 
    ##                                         crudoderesmachacado 
    ##                                                           1 
    ##                                  concrema.cidaalciboullette 
    ##                                                           1 
    ##                                                  alcaparras 
    ##                                                           1 
    ##                                         pepinillosenvinagre 
    ##                                                           1 
    ##                 chucrutderepollomoradoylechugaensopaipillas 
    ##                                                           1 
    ##                                    sierraahumadadesmenuzada 
    ##                                                           1 
    ##                                            conmayonesadeajo 
    ##                                                           1 
    ##                                                 perejilylim 
    ##                                                           1 
    ##               sobreunacamadelechugacostinaypebredecochayuyo 
    ##                                                           1 
    ##                                                ensopaipilla 
    ##                                                           1 
    ##                                                     mechada 
    ##                                                           2 
    ##                                                       palta 
    ##                                                           5 
    ##                                       tomateymayonesacasera 
    ##                                                           1 
    ##                                               ensopaipillas 
    ##                                                           1 
    ##                                                 lomodecerdo 
    ##                                                           1 
    ##                                                      tomate 
    ##                                                          11 
    ##                                              mayonesacasera 
    ##                                                           2 
    ##                                                reinetafrita 
    ##                                                           1 
    ##                                                     lechuga 
    ##                                                           7 
    ##                                        cebollamoradayblanca 
    ##                                                           1 
    ##                                                          aj 
    ##                                                           7 
    ##                              iesblanqueadosysalsaalcilantro 
    ##                                                           1 
    ##                                                         alb 
    ##                                                           1 
    ##                                     undigasdequinoacrocante 
    ##                                                           1 
    ##                                                 hojasverdes 
    ##                                                           1 
    ##                                                  huevofrito 
    ##                                                           2 
    ##                               brotesdealfalfaychipsdecamote 
    ##                                                           1 
    ##                                                carnemechada 
    ##                                                           2 
    ##                                                tomatefresco 
    ##                                                           1 
    ##                                                      iverde 
    ##                                                           1 
    ##                                agregadochanchoenpiedraymerk 
    ##                                                           1 
    ##                                              enmarraquetay1 
    ##                                                           1 
    ##                                                      lengua 
    ##                                                           1 
    ##                                               porotosverdes 
    ##                                                           2 
    ##                                         cebollacaramelizada 
    ##                                                           3 
    ##                                     quesomantecosoderretido 
    ##                                                           1 
    ##                                                  kissmyhass 
    ##                                                           1 
    ##                                                  paltafrita 
    ##                                                           1 
    ##                                            salsadequesoazul 
    ##                                                           1 
    ##                                                  pepinillos 
    ##                                                           4 
    ##                                              quesomantecoso 
    ##                                                           1 
    ##                                      espinacaysalsadelacasa 
    ##                                                           1 
    ##                                        pulledchickenahumado 
    ##                                                           1 
    ##                                                quesodecabra 
    ##                                                           2 
    ##                                               salsateriyaki 
    ##                                                           1 
    ##                                       chipsdebetarragafrita 
    ##                                                           1 
    ##                         mayonesadecilantroyberrosenpanfranc 
    ##                                                           1 
    ##                                                    escasero 
    ##                                                           1 
    ##                                                         ajo 
    ##                                                           1 
    ##                                                    mayonesa 
    ##                                                           4 
    ##                                               salsadetomate 
    ##                                                           2 
    ##                                                     chucrut 
    ##                                                           1 
    ## hamburguesadecarnedevacunoycerdomolidacondimentadaconajoyaj 
    ##                                                           1 
    ##                                      chucrutymayonesacasera 
    ##                                                           1 
    ##                                    rellenodepolloalaplancha 
    ##                                                           1 
    ##                                                      berros 
    ##                                                           1 
    ##                                             salsadecilantro 
    ##                                                           1 
    ##                                              yunmixdechampi 
    ##                                                           1 
    ##                                 oonesconquesocremaderretido 
    ##                                                           1 
    ##                                         hamburguesadry-aged 
    ##                                                           1 
    ##                                              quesoprovolone 
    ##                                                           1 
    ##                                                          oo 
    ##                                                           1 
    ##                                                  relishdepi 
    ##                                                           1 
    ##                                                          oa 
    ##                                                           1 
    ##                                             mayonesatrufada 
    ##                                                           1 
    ##                                                       queso 
    ##                                                           2 
    ##                                                     cebolla 
    ##                                                           1 
    ##                                                     mostaza 
    ##                                                           1 
    ##                                                     ketchup 
    ##                                                           1 
    ##                                                      tocino 
    ##                                                           4 
    ##                                              mixpuntapaleta 
    ##                                                           1 
    ##                                           tocinoypuntaganso 
    ##                                                           1 
    ##                                                  encurtidos 
    ##                                                           1 
    ##                                                       salsa 
    ##                                                           1 
    ##                                      costillardecerdoalmerk 
    ##                                                           1 
    ##                braseadoydesmenuzadoconcebollascaramelizadas 
    ##                                                           1 
    ##                                    contomatesconfitadosalor 
    ##                                                           1 
    ##                                       eganoyfondutadequesos 
    ##                                                           1 
    ##                      montadoenunexquisitopanamasadoespecial 
    ##                                                           1 
    ##                                                quesocheddar 
    ##                                                           4 
    ##                                                quesohavarti 
    ##                                                           1 
    ##                              costillitasdevacunodesmenuzada 
    ##                                                           1 
    ##                                              cebollacrunchy 
    ##                                                           1 
    ##                                                  quesocrema 
    ##                                                           1 
    ##                                             aceitunasverdes 
    ##                                                           1 
    ##                                                    quesolas 
    ##                                                           1 
    ##                                                      guilas 
    ##                                                           1 
    ##                                                   salalmerk 
    ##                                                           1 
    ##                                                 mantequilla 
    ##                                                           1 
    ##                                             churrascofilete 
    ##                                                           1 
    ##                                     iverde.agregadomayonesa 
    ##                                                           1 
    ##                                            pastramiymostaza 
    ##                                                           1 
    ##                                              enpandecenteno 
    ##                                                           1 
    ##                                      camaronesfritosenpanko 
    ##                                                           1 
    ##                                             quesomozzarella 
    ##                                                           3 
    ##                                             salsasweetchili 
    ##                                                           1 
    ##                                               salsalabirrer 
    ##                                                           1 
    ##                                                          ia 
    ##                                                           2 
    ##                                              oonessalteados 
    ##                                                           1 
    ##                                       tocinoahumadocrocante 
    ##                                                           1 
    ##                                       arosdecebollaapanados 
    ##                                                           2 
    ##                                 hamburguesade3cortesdecarne 
    ##                                                           1 
    ##                                        basedelechugaytomate 
    ##                                                           1 
    ##                                                         yaj 
    ##                                                           1 
    ##                              iesverdesrellenosconmozzarella 
    ##                                                           1 
    ##                                              tocinocrocante 
    ##                                                           2 
    ##                                               mayonesaspicy 
    ##                                                           2 
    ##                       iesverdesrellenosconmozzarellaytocino 
    ##                                                           1 
    ##                                         costillardecerdo100 
    ##                                                           1 
    ##                                             org.nicodecocci 
    ##                                                           1 
    ##                                    unlentaconsalsabbqcasera 
    ##                                                           1 
    ##                                  ensaladillacriollaconpalta 
    ##                                                           1 
    ##                                               enpanciabatta 
    ##                                                           1 
    ##                                            plateadadevacuno 
    ##                                                           1 
    ##                                           pasteleradechoclo 
    ##                                                           1 
    ##                                                   papashilo 
    ##                                                           1 
    ##                                  huevofritoycebollarebosada 
    ##                                                           1 
    ##                                                enmarraqueta 
    ##                                                           2 
    ##                                                   lomoderes 
    ##                                                           1 
    ##                                                unypapashilo 
    ##                                                           1 
    ##                                        enpanciabattaperuano 
    ##                                                           1 
    ##                                      panfocacciademasamadre 
    ##                                                           1 
    ##                                                  berenjenas 
    ##                                                           1 
    ##                                                   ricottafr 
    ##                                                           1 
    ##                                   pestodelacasayaceitedeajo 
    ##                                                           1 
    ##                                            chorizoargentino 
    ##                                                           1 
    ##                                                 chimichurri 
    ##                                                           1 
    ##                              huevofritoycebollacaramelizada 
    ##                                                           1 
    ##                                                       entra 
    ##                                                           1 
    ##                                                         oas 
    ##                                                           1 
    ##                                                     chorizo 
    ##                                                           1 
    ##                                                       pebre 
    ##                                                           1 
    ##                                                cebollaasada 
    ##                                                           1 
    ##                                              mayoneracasada 
    ##                                                           1 
    ##                                             milanesadeposta 
    ##                                                           1 
    ##                                            apanadasinharina 
    ##                                                           1 
    ##                                          conquesomozzarella 
    ##                                                           1 
    ##                                              cebollitafrita 
    ##                                                           1 
    ##                            salsadetomatescaserayuntoquedeor 
    ##                                                           1 
    ##                                                       egano 
    ##                                                           1 
    ##                                               340grsdecarne 
    ##                                                           1 
    ##                                        condoblequesocheddar 
    ##                                                           1 
    ##                              mayonesadeeneldoycebollamorada 
    ##                                                           1 
    ##                                  hamburguesadeporotosnegros 
    ##                                                           1 
    ##                                      concebollacaramelizada 
    ##                                                           1 
    ##                                          veganesadecilantro 
    ##                                                           1 
    ##           betarragayzanahoriaenjulianaysalsabarbecuehechaah 
    ##                                                           1 
    ##                                                      imismo 
    ##                                                           1 
    ##                                            doblehamburguesa 
    ##                                                           1 
    ##                                               icherrypepper 
    ##                                                           2 
    ##                                          tocinoysalsastreat 
    ##                                                           1 
    ##                        pollodeshilachadoencurryylechedecoco 
    ##                                                           1 
    ##                                         contirasdezanahoria 
    ##                                                           1 
    ##                                             zapalloitaliano 
    ##                                                           1 
    ##                                       cebollamoradaylechuga 
    ##                                                           1 
    ##                                                        soya 
    ##                                                           1 
    ##                                                esamotostado 
    ##                                                           1 
    ##                                                        miel 
    ##                                                           1 
    ##                                          jengibreyaceitedes 
    ##                                                           1 
    ##                                                       esamo 
    ##                                                           1 
    ##                                                    conpalta 
    ##                                                           1 
    ##                                          lechugayquesocrema 
    ##                                                           1 
    ##                      hamburguesacubiertaconcremadequesoazul 
    ##                                                           1 
    ##                                          mermeladadecebolla 
    ##                                                           2 
    ##                                              pancetaahumada 
    ##                                                           1 
    ##                                        polloespeciadoyfrito 
    ##                                                           1 
    ##                                                   zestyslaw 
    ##                                                           1 
    ##                                               panpotatoroll 
    ##                                                           1 
    ##                                                 salsastreat 
    ##                                                           1 
    ##                                         hamburguesadechampi 
    ##                                                           1 
    ##                                   oonesportobelloylegumbres 
    ##                                                           1 
    ##                                                    tomateyk 
    ##                                                           1 
    ##                                                etchupcasero 
    ##                                                           1 
    ##                                     cebollitascaramelizadas 
    ##                                                           1 
    ##                                 mixverdeyaderezodezanahoria 
    ##                                                           1 
    ##                                                 enpancasero 
    ##                                                           1 
    ##                                                         jam 
    ##                                                           1 
    ##                                                   unserrano 
    ##                                                           1 
    ##                                          quesodehuentelauqu 
    ##                                                           1 
    ##                                          enypimientosasados 
    ##                                                           1 
    ##                                            hamburguesadeara 
    ##                                                           1 
    ##                                                 oitadewagyu 
    ##                                                           1 
    ##                                         servidoenpanbrioche 
    ##                                                           1

## se genera un vector con con los nombres

``` r
Ingr<-colSums(matriz_ingre)
Ingr<-attributes(Ingr)
Ingr
```

    ## $names
    ##   [1] "pato"                                                       
    ##   [2] "quesopepperjack"                                            
    ##   [3] "jalape"                                                     
    ##   [4] "oos"                                                        
    ##   [5] "cebollamorada"                                              
    ##   [6] "espinaca"                                                   
    ##   [7] "tomateymayonesahabanero"                                    
    ##   [8] "enpanbrioche"                                               
    ##   [9] "hamburguesa"                                                
    ##  [10] "quesoamericano"                                             
    ##  [11] "tocinoglaseadoconmieldemaple"                               
    ##  [12] "alioliconmieldemaple"                                       
    ##  [13] "poutine"                                                    
    ##  [14] "pandeleche"                                                 
    ##  [15] "carne"                                                      
    ##  [16] "zucchinigrilladoconuntoquedeajo"                            
    ##  [17] "piment"                                                     
    ##  [18] "unacaramelado"                                              
    ##  [19] "champi"                                                     
    ##  [20] "oonesyricotta"                                              
    ##  [21] "filetederes"                                                
    ##  [22] "conrelishdebetarraga"                                       
    ##  [23] "ra"                                                         
    ##  [24] "izpicante"                                                  
    ##  [25] "r.bano"                                                     
    ##  [26] "arosdecebollaychampi"                                       
    ##  [27] "oonesportobellos"                                           
    ##  [28] "salm"                                                       
    ##  [29] "unenmantequillaaromatizada"                                 
    ##  [30] "tomatesasados"                                              
    ##  [31] "quesocremasaborizado"                                       
    ##  [32] "coraz"                                                      
    ##  [33] "undealcachofaymixverde"                                     
    ##  [34] "crudoderesmachacado"                                        
    ##  [35] "concrema.cidaalciboullette"                                 
    ##  [36] "alcaparras"                                                 
    ##  [37] "pepinillosenvinagre"                                        
    ##  [38] "chucrutderepollomoradoylechugaensopaipillas"                
    ##  [39] "sierraahumadadesmenuzada"                                   
    ##  [40] "conmayonesadeajo"                                           
    ##  [41] "perejilylim"                                                
    ##  [42] "sobreunacamadelechugacostinaypebredecochayuyo"              
    ##  [43] "ensopaipilla"                                               
    ##  [44] "mechada"                                                    
    ##  [45] "palta"                                                      
    ##  [46] "tomateymayonesacasera"                                      
    ##  [47] "ensopaipillas"                                              
    ##  [48] "lomodecerdo"                                                
    ##  [49] "tomate"                                                     
    ##  [50] "mayonesacasera"                                             
    ##  [51] "reinetafrita"                                               
    ##  [52] "lechuga"                                                    
    ##  [53] "cebollamoradayblanca"                                       
    ##  [54] "aj"                                                         
    ##  [55] "iesblanqueadosysalsaalcilantro"                             
    ##  [56] "alb"                                                        
    ##  [57] "undigasdequinoacrocante"                                    
    ##  [58] "hojasverdes"                                                
    ##  [59] "huevofrito"                                                 
    ##  [60] "brotesdealfalfaychipsdecamote"                              
    ##  [61] "carnemechada"                                               
    ##  [62] "tomatefresco"                                               
    ##  [63] "iverde"                                                     
    ##  [64] "agregadochanchoenpiedraymerk"                               
    ##  [65] "enmarraquetay1"                                             
    ##  [66] "lengua"                                                     
    ##  [67] "porotosverdes"                                              
    ##  [68] "cebollacaramelizada"                                        
    ##  [69] "quesomantecosoderretido"                                    
    ##  [70] "kissmyhass"                                                 
    ##  [71] "paltafrita"                                                 
    ##  [72] "salsadequesoazul"                                           
    ##  [73] "pepinillos"                                                 
    ##  [74] "quesomantecoso"                                             
    ##  [75] "espinacaysalsadelacasa"                                     
    ##  [76] "pulledchickenahumado"                                       
    ##  [77] "quesodecabra"                                               
    ##  [78] "salsateriyaki"                                              
    ##  [79] "chipsdebetarragafrita"                                      
    ##  [80] "mayonesadecilantroyberrosenpanfranc"                        
    ##  [81] "escasero"                                                   
    ##  [82] "ajo"                                                        
    ##  [83] "mayonesa"                                                   
    ##  [84] "salsadetomate"                                              
    ##  [85] "chucrut"                                                    
    ##  [86] "hamburguesadecarnedevacunoycerdomolidacondimentadaconajoyaj"
    ##  [87] "chucrutymayonesacasera"                                     
    ##  [88] "rellenodepolloalaplancha"                                   
    ##  [89] "berros"                                                     
    ##  [90] "salsadecilantro"                                            
    ##  [91] "yunmixdechampi"                                             
    ##  [92] "oonesconquesocremaderretido"                                
    ##  [93] "hamburguesadry-aged"                                        
    ##  [94] "quesoprovolone"                                             
    ##  [95] "oo"                                                         
    ##  [96] "relishdepi"                                                 
    ##  [97] "oa"                                                         
    ##  [98] "mayonesatrufada"                                            
    ##  [99] "queso"                                                      
    ## [100] "cebolla"                                                    
    ## [101] "mostaza"                                                    
    ## [102] "ketchup"                                                    
    ## [103] "tocino"                                                     
    ## [104] "mixpuntapaleta"                                             
    ## [105] "tocinoypuntaganso"                                          
    ## [106] "encurtidos"                                                 
    ## [107] "salsa"                                                      
    ## [108] "costillardecerdoalmerk"                                     
    ## [109] "braseadoydesmenuzadoconcebollascaramelizadas"               
    ## [110] "contomatesconfitadosalor"                                   
    ## [111] "eganoyfondutadequesos"                                      
    ## [112] "montadoenunexquisitopanamasadoespecial"                     
    ## [113] "quesocheddar"                                               
    ## [114] "quesohavarti"                                               
    ## [115] "costillitasdevacunodesmenuzada"                             
    ## [116] "cebollacrunchy"                                             
    ## [117] "quesocrema"                                                 
    ## [118] "aceitunasverdes"                                            
    ## [119] "quesolas"                                                   
    ## [120] "guilas"                                                     
    ## [121] "salalmerk"                                                  
    ## [122] "mantequilla"                                                
    ## [123] "churrascofilete"                                            
    ## [124] "iverde.agregadomayonesa"                                    
    ## [125] "pastramiymostaza"                                           
    ## [126] "enpandecenteno"                                             
    ## [127] "camaronesfritosenpanko"                                     
    ## [128] "quesomozzarella"                                            
    ## [129] "salsasweetchili"                                            
    ## [130] "salsalabirrer"                                              
    ## [131] "ia"                                                         
    ## [132] "oonessalteados"                                             
    ## [133] "tocinoahumadocrocante"                                      
    ## [134] "arosdecebollaapanados"                                      
    ## [135] "hamburguesade3cortesdecarne"                                
    ## [136] "basedelechugaytomate"                                       
    ## [137] "yaj"                                                        
    ## [138] "iesverdesrellenosconmozzarella"                             
    ## [139] "tocinocrocante"                                             
    ## [140] "mayonesaspicy"                                              
    ## [141] "iesverdesrellenosconmozzarellaytocino"                      
    ## [142] "costillardecerdo100"                                        
    ## [143] "org.nicodecocci"                                            
    ## [144] "unlentaconsalsabbqcasera"                                   
    ## [145] "ensaladillacriollaconpalta"                                 
    ## [146] "enpanciabatta"                                              
    ## [147] "plateadadevacuno"                                           
    ## [148] "pasteleradechoclo"                                          
    ## [149] "papashilo"                                                  
    ## [150] "huevofritoycebollarebosada"                                 
    ## [151] "enmarraqueta"                                               
    ## [152] "lomoderes"                                                  
    ## [153] "unypapashilo"                                               
    ## [154] "enpanciabattaperuano"                                       
    ## [155] "panfocacciademasamadre"                                     
    ## [156] "berenjenas"                                                 
    ## [157] "ricottafr"                                                  
    ## [158] "pestodelacasayaceitedeajo"                                  
    ## [159] "chorizoargentino"                                           
    ## [160] "chimichurri"                                                
    ## [161] "huevofritoycebollacaramelizada"                             
    ## [162] "entra"                                                      
    ## [163] "oas"                                                        
    ## [164] "chorizo"                                                    
    ## [165] "pebre"                                                      
    ## [166] "cebollaasada"                                               
    ## [167] "mayoneracasada"                                             
    ## [168] "milanesadeposta"                                            
    ## [169] "apanadasinharina"                                           
    ## [170] "conquesomozzarella"                                         
    ## [171] "cebollitafrita"                                             
    ## [172] "salsadetomatescaserayuntoquedeor"                           
    ## [173] "egano"                                                      
    ## [174] "340grsdecarne"                                              
    ## [175] "condoblequesocheddar"                                       
    ## [176] "mayonesadeeneldoycebollamorada"                             
    ## [177] "hamburguesadeporotosnegros"                                 
    ## [178] "concebollacaramelizada"                                     
    ## [179] "veganesadecilantro"                                         
    ## [180] "betarragayzanahoriaenjulianaysalsabarbecuehechaah"          
    ## [181] "imismo"                                                     
    ## [182] "doblehamburguesa"                                           
    ## [183] "icherrypepper"                                              
    ## [184] "tocinoysalsastreat"                                         
    ## [185] "pollodeshilachadoencurryylechedecoco"                       
    ## [186] "contirasdezanahoria"                                        
    ## [187] "zapalloitaliano"                                            
    ## [188] "cebollamoradaylechuga"                                      
    ## [189] "soya"                                                       
    ## [190] "esamotostado"                                               
    ## [191] "miel"                                                       
    ## [192] "jengibreyaceitedes"                                         
    ## [193] "esamo"                                                      
    ## [194] "conpalta"                                                   
    ## [195] "lechugayquesocrema"                                         
    ## [196] "hamburguesacubiertaconcremadequesoazul"                     
    ## [197] "mermeladadecebolla"                                         
    ## [198] "pancetaahumada"                                             
    ## [199] "polloespeciadoyfrito"                                       
    ## [200] "zestyslaw"                                                  
    ## [201] "panpotatoroll"                                              
    ## [202] "salsastreat"                                                
    ## [203] "hamburguesadechampi"                                        
    ## [204] "oonesportobelloylegumbres"                                  
    ## [205] "tomateyk"                                                   
    ## [206] "etchupcasero"                                               
    ## [207] "cebollitascaramelizadas"                                    
    ## [208] "mixverdeyaderezodezanahoria"                                
    ## [209] "enpancasero"                                                
    ## [210] "jam"                                                        
    ## [211] "unserrano"                                                  
    ## [212] "quesodehuentelauqu"                                         
    ## [213] "enypimientosasados"                                         
    ## [214] "hamburguesadeara"                                           
    ## [215] "oitadewagyu"                                                
    ## [216] "servidoenpanbrioche"

## se juntan los vectores en un data frame, ya que es el mismo orden no afecta y se seleccionan solo los que se repitieron mas de 4 veces

``` r
Total<-as.data.frame(Total)
Ingr<-as.data.frame(Ingr)
Ingr$cantidad<-Total
names (Ingr)= c("ingredientes", "total")

Mejores = Ingr [Ingr$total >=4,]
Mejores<-as.data.frame(Mejores)
Mejores
```

    ##     ingredientes Total
    ## 45         palta     5
    ## 49        tomate    11
    ## 52       lechuga     7
    ## 54            aj     7
    ## 73    pepinillos     4
    ## 83      mayonesa     4
    ## 103       tocino     4
    ## 113 quesocheddar     4

\#\#Ingredientes Finales

``` r
Ingredientes_finales<-Mejores$ingredientes
Ingredientes_finales<-as.data.frame(Ingredientes_finales)

Ingredientes_finales
```

    ##   Ingredientes_finales
    ## 1                palta
    ## 2               tomate
    ## 3              lechuga
    ## 4                   aj
    ## 5           pepinillos
    ## 6             mayonesa
    ## 7               tocino
    ## 8         quesocheddar
