#+ echo = F
# Si su intención es compilar un notebook, comente la línea 5.

#+ echo = F
opts_chunk$set(comment = NA, tidy = FALSE, message = F, warnings = F,
               fig.align = 'center')

#' # Guía rápida de programación con capm 0.5 en R versión 3.1.0
 
#' ### Oswaldo Santos, oswaldosant@gmail.com
#' ### [Mi repositorio Github](https://github.com/oswaldosantos)
#' ### [Página web del `capm`](http://oswaldosantos.github.io/capm/)
#' ### Última actualización: 25/05/2014
#' ***
 
####' ### Introducción ####

#' En esta guía rápida le mostraré como implementar los 8 pasos básicos del flujo de trabajo propuesto (vea la página web del `capm`).<br>  
 
#' No daré detalles técnicos ni discutiré el significado de los resultados (ver el [libro](https://github.com/oswaldosantos/capm/wiki/1.2-Libro) si esto es lo que está buscando). Apenas daré la receta.<br>  
 
#' Presupuestos para reproducir esta guía:

#' - Conocimiento mínimo de programación en R.
#' - Familiaridad con las páginas de ayuda de las funciones de R.
#' - El directorio de trabajo actual contiene los siguientes archivos disponibles [aquí](https://github.com/oswaldosantos/capm/tree/master/Documentation/Quia_rapida_de_programacion). 
#'  * pilot.csv
#'  * psu.ssu.csv
#'  * santos.dbf
#'  * santos.prj
#'  * santos.shp
#'  * santos.shx
#'  * survey.data.csv
#'  En el enlace anterior también encontrará un archivo llamado guia_rapida_de_programacion_capm_0.5.R con los códigos de esta guía.

#+ 
library(capm)

#' ***

####' ### 1. Selección de unidades muestrales para diseños simples y complejos (piloto/final). ####

#' Comencemos con la selección de unidades muestrales para una muestra por conglomerados en dos etapas. El archivo `psu.ssu.csv` contiene datos de la ciudad Santos, Brasil. Los datos fueron extraídos del [(IBGE)](www.ibge.gov.br). La primera columna contiene identificadores únicos de sectores censales, nuestras unidades primarias de muestreo (UPM). La segunda columna contiene el número de domicilios en cada UPM. Los domicilios son nuestras Unidades Secundarias de muestreo (USM) que también son la medida del tamaño de las UPM.<br>  

#' Después de importar el archivo

#+
psu.ssu <- read.csv(file = 'psu.ssu.csv')

#' podemos ver que hay 653 UPM.

#+
str(psu.ssu)

#' Las 6 primeras líneas dan una idea de los datos.

#+ 
head(psu.ssu)

#' Los identificadores de las UPM parecen todos iguales pero esto es solo un resultado en notación científica. Los identificadores deben ser únicos para cada UPM. Para confirmar este requisito podemos cambiar el patrón de impresión y verificar que el número de identificadores diferentes es igual al número de UPM.

#+
print(head(psu.ssu), digits = 15)
length(unique(psu.ssu[ , 1]))

#' El archivo contiene exactamente la información que necesitamos para muestrear UPM con probabilidad proporcional a sus tamaños y con reposición. Si el argumento `write` de `SamplePPS` es definido como TRUE, las UPM seleccionadas serán guardadas en un archivo csv, que puede ser visto en un software de planillas. El resultado tendrá tantas líneas como UPM seleccionadas. Recuerde que la misma UPM puede ser seleccionada más de una vez porque el muestreo es con reemplazo.<br>  

#' Si usamos `set.seed(algun_numero)`, la próxima muestra seudoaleatoria siempre será la misma. En esta guía rápida usaré `set.seed(4)` para que pueda reproducir exactamente todos los ejemplos. Sin embargo, en aplicaciones reales no debe usar `set.seed`.

#+ 
set.seed(4)
pilot.psu <- SamplePPS(psu.ssu = psu.ssu, psu = 10, write = FALSE)

#' Inspeccionando el objeto que acabamos de crear, podemos ver que la `class` de los identificadores de las UPM fue convertida a  `character`. Esto significa que los identificadores ahora son representados como texto, no como números.

#+
str(pilot.psu)
head(pilot.psu)

#' Seleccionar USM es tan simple como la selección previa. El resultado tendrá tantas líneas como USM seleccionadas en cada UPM y tantas columnas como UPM seleccionadas.

#+
set.seed(4)
pilot.ssu <- SampleSystematic(psu.ssu = pilot.psu, su = 5, write = F)

#' Veamos las cuatro primeras columnas para tener una idea.

#+
head(pilot.ssu[ , 1:4])

#' ***

####' ### 2. Mapeamiento de las unidades primarias de muestreo a ser visitadas (muestras complejas). ####

#' Después de seleccionar las unidades de muestreo, tenemos que conocer sus localizaciones geográficas. Afortunadamente el `capm` tiene una función para localizar las UPM. Si tenemos un shapefile de las UPM, estamos hechos como en este caso. En el directorio de trabajo hay cinco archivos llamados "santos", cada uno con una extensión diferente. Todos esos archivos son una representación shapefile de las UPM del área muestreada (ciudad Santos). Conseguí los archivos en IBGE (mencionado anteriormente).<br>  

#+
MapkmlPSU(shape = 'santos', psu = pilot.psu[, 1], id = 1)

#' `MapkmlPSU` crea archivos kml para cada UPM mas un kml con todas las UPM seleccionadas. Esos archivos kml pueden ser abiertos con Google Earth clicando sobre los mismos. [QGIS](www.qgis.org) es una herramienta de fuente abierta que también puede graficar diferentes camadas como fondo para los archivos kml.<br>  

#' Por supuesto, R nos permite graficar las localizaciones de las UPM seleccionadas. No se preocupe si no entiende el siguiente fragmento de código, es tan solo una alternativa para Google Earth o QGIS, que estoy usando aquí mostrar que puede mapear las UPM seleccionadas..<br>  

#+
# EL paquete rgeos deve estar instalado.
library(rgdal); library(ggmap); library(maptools); library(plyr)

#+ fig.width = 11, fig.height = 11
santos <- readOGR(dsn = '.', layer = 'santos')
santos.pilot <- santos[as.character(santos@data[ , 1]) %in% pilot.psu[ , 1], ]
santos.pilot <- spTransform(santos.pilot, CRS('+init=epsg:4326'))

santos.pilot@data$id <- rownames(santos.pilot@data)
santos.pilot.points <- fortify(santos.pilot, region = "id")
santos.pilot.df <- join(santos.pilot.points, santos.pilot@data, by = "id")

# Si aparece el error '503 Service Unavailable' intente más tarde para ver si 
# los servidores OSM vuelven a funcionar (ver página de ayuda de get_openstreetmap).
osm.all.psu <- get_openstreetmap(bbox = c(-46.384, -23.989, -46.299, -23.930),
                                 scale = 34000, color = 'bw')
ggmap(osm.all.psu) + 
  geom_polygon(data = santos.pilot.df, aes(x = long, y = lat, fill = PSU),
               color = 'yellow', size = 1.2) +
  coord_equal()

#' Cualquiera que sea el método usado para producir los mapas, debemos trazar una ruta en el mapa de cada UPM seleccionada, de tal forma que todas las calles sean recorridas. Podemos fijar un domicilio en un punto arbitrario (ex. localización inferior izquierda) como el primer domicilio y a partir del mismo, recorrer la ruta contando domicilios (incluyendo los dos lados de los fragmentos de calles contenidos totalmente en la UPM) y entrevistando los que fueron seleccionados.<br>  

#' El siguiente mapa muestra la cuarta UPM muestreada.

#+ 
osm.psu4 <- get_openstreetmap(bbox = c(-46.349, -23.962, -46.345, -23.957),
                              scale = 5000)
ggmap(osm.psu4) +
  geom_polygon(data = santos.pilot[4, ], aes(x = long, y = lat), fill = NA,
               color = 'yellow', size = 2) +
  coord_equal()

#' ***

####' ### 3. Cálculo del tamaño y composición muestral (diseños complejos). ####

#' El cálculo del tamaño y la composición de muestras conglomeradas en dos etapas es un proceso complejo. Afortunadamente, con el `capm` solo tenemos que definir el nivel de confianza que queremos, el error que estamos dispuestos a aceptar y una estimativa de costo. Esta última es la razón entre el costo asociado a la visita de una UPM y el costo asociado a la realización de una entrevista.<br>  

#' Dos fuentes de datos son necesarias. La primera es el archivo `psu.ssu` que importamos en la primera sección. La segunda es el archivo con los datos que hipotéticamente colectamos en la muestra piloto que diseñamos anteriormente. El archivo `pilot` contiene tantas filas como domicilios visitados en el piloto. La primera columna contiene identificadores de las UPM a las que pertenecen los respectivos domicilios. La segunda columna contiene el número de perros observados en cada domicilio.

#+ 
pilot <- read.csv('pilot.csv')
Calculate2StageSampleSize(psu.ssu = psu.ssu, psu.x = pilot,
                          conf.level = 0.95, error = 0.1, cost = 10)

#' ***

####' ### 4. Repetición de los pasos 1 y 2 para la muestra final, si se comenzó con una muestra piloto. ####

#' Una vez definido el tamaño y la composición de la muestra final, seleccionar las unidades muestrales se reduce a repetir los pasos 1 y 2, usando la salida de la sección 3 (20 UPM y 19 USM por UPM).

#' 

#+
final.psu <- SamplePPS(psu.ssu, 20, write = F)
final.ssu <- SampleSystematic(final.psu, 19, write = F)
# Quitar el comentario de la siguiente línea para os kml.
#MapkmlPSU(shape = 'santos', psu = final.psu[, 1], id = 1)

#' ***

####' ### Estimación de características demográficas (totales, proporciones, medias y razones). ####

#' Después de haber definido la muestra final, supongamos que fuimos a visitar todos los domicilios seleccionados y que los datos colectados fueron registrados en un archivo llamado `survey.data.csv`.<br>  

#' Para ver la descripción de cada variable, vea la página de ayuda del archivo con el comando `?survey.data`.

#+ 
survey.data <- read.csv('survey.data.csv')
str(survey.data)
head(survey.data)

#' Para estimar los parámetros poblacionales, el primer paso es definir el diseño muestral del cuál provienen los datos. Para esto, necesitamos un archivo que contenga todas las unidades muestrales de la población (`psu.ssu`) y un archivo con los datos muestrales (`survey.data`). En este último archivo, las columnas que contienen los identificadores de las UPM y de las USM deben ser especificadas, así como el número de UPM incluidas en la muestra (para las UPM incluidas más de una vez, cada ocurrencia debe ser contada).  
 
#+ 
design <- DesignSurvey(psu.ssu = psu.ssu, sample = survey.data, psu.col = 2,
                       ssu.col = 1, psu.2cd = 20)

#' Viendo las variables incluidas en el diseño podemos notar que las dos primeras y las tres últimas no representan variables a ser estimadas. Esas variables fueron creadas para definir el diseño muestral.

#+
names(design$variables)

#' Definir el tipo de estimativa para cada variable es fácil. Comillas vacías excluyen una variable del proceso de estimación.

#+ 
variables <- c("total", "prop", "mean", "prop", "prop",
               "total", rep("prop", 8))

#' Es conveniente confirmar que definimos el tipo de estimativas que queremos.

#+
cbind(names(design$variables), variables)

#' Ahora estamos listos para obtener nuestras primeras estimativas.

#+
(estimates <- SummarySurvey(design = design, variables = variables, rnd = 3))

#' La salida anterior es bastante útil pero puede no ser suficiente. Hagamos una copia (`sample1`) de un subconjunto transformado de `survey.data`, para estimar el número total de animales esterilizados (en lugar de la proporción) y para obtener estimativas condicionadas al sexo.

#+
sample1 <- survey.data[, c('interview_id', 'psu', 'dogs', 'sex', 'sterilized',
                           'sterilized.ly', 'fate')]
sample1[, 'sterilized'] <- as.character(sample1[, 'sterilized'])
sample1[which(sample1$sterilized ==  "yes"), 'sterilized'] <- 1
sample1[which(sample1[, 'sterilized'] ==  "no"), 'sterilized'] <- 0
sample1[, 'sterilized'] <- as.numeric(sample1[, 'sterilized'])

#' Después de definir el diseño muestral de la forma usual

#+
design.sex <- DesignSurvey(psu.ssu = psu.ssu, sample = sample1, 
                           psu.col = 2, ssu.col = 1, psu.2cd = 20)

#' podemos crear el diseño para cada sexo.

#+ 
design.f <- subset(design.sex, sex ==  'Female')
design.m <- subset(design.sex, sex ==  'Male')

#' A partir de aquí, no hay nada nuevo.

#+
names(design.sex$variables)
variables.sex <- c('total', '', 'total', 'prop', 'prop')
cbind(names(design.sex$variables), variables.sex)
(estimates.f <- SummarySurvey(design.f, variables.sex, rnd = 3))
(estimates.m <- SummarySurvey(design.m, variables.sex, rnd = 3))

#' ***

####' ### 6. Construcción de pirámides poblacionales, condicionadas por el sexo, la edad y otra variable categórica como el estado reproductivo. ####

#' Las pirámides poblacionales resumen la composición básica de una población y como mínimo, son construidas usando las variables edad y sexo pero pueden ser condicionadas a una tercera variable categórica. Los datos a ser usados deben tener cada variable en una columna separada y esas columnas deben ser especificadas en los respectivos argumentos de la función. Para que las etiquetas de la figura salgan en español, debemos transformar los `"labels"` de las varibles `"sex"` y `"sterilized"`, así como el nombre de esta última.

#+
library(plyr)
matrix(names(survey.data), ncol = 1)
names(survey.data)[6] <- 'Esterilizado'
survey.data$sex <- revalue(survey.data$sex,
                           c('Female' = 'Hembra', 'Male' = 'Macho'))
survey.data$Esterilizado <- revalue(survey.data$Esterilizado, c('yes' = 'sim'))
PlotPopPyramid(dat = survey.data, age.col = 'age', sex.col = 'sex',
               stage.label = 'Años')
PlotPopPyramid(dat = survey.data, age.col = 5, sex.col = 4, str.col = 6,
               stage.label = 'Años')

#' ***

####' ### 7. Evaluación de los efectos producidos por intervenciones de manejo poblacional, a través de modelaje matemático. ####

#' Ahora estamos listos para simular el efecto de la inmigración, el abandono, la esterilización y la adopción, en la dinámica de poblaciones domiciliadas y callejeras. La función `SolveIASA` requiere varios parámetros para ejecutar un modelo matemático de dinámica poblacional. Algunos parámetros son de la población domiciliadas y otros de la población callejera.<br>  

#' Tenemos estimativas para la mayoría de los parámetros de la población domiciliada pero no tenemos estimativas para la población callejera. Con base en la literatura y la opinión de expertos, podemos definir estimativas subjetivas para la población callejera (en la próxima sección evaluaremos que tanto las estimativas subjetivas comprometen los resultados del modelo)
 
#+
# Condiciones iniciales

# Perros domiciliados         # Perros callejeros
f1 <- 39565 - 12783;          f2 <- f1 * 0.1
fs1 <- 12783;                 fs2 <- fs1 * 0.05
m1 <- 50289 - 9346;           m2 <- m1 * 0.1
ms1 <- 9346;                  ms2 <- ms1 * 0.05


# Parámetros

# Perros domiciliados         # Perros callejeros
b1 <-  7724;                  b2 <- b1 * 0.15
df1 <- 0.046;                 df2 <- df1 * 1.15
dm1 <- 0.053;                 dm2 <- dm1 * 1.15
sf1 <- 0.13;                  sf2 <- sf1 * 0.05
sm1 <- 0.043;                 sm2 <- sm1 * 0.05
k1 <- (f1 + m1) * 1.1;        k2 <- (f2 + m2) * 1.1
h1 <- 1;                      h2 <- 0.5;
ab <- 0.05;                   ad <- 0.104;
v <- 0.147
z <- v * 0.11

#+
init.solve.iasa = c(
  f1 = f1, fs1 = fs1, m1 = m1, ms1 = ms1,
  f2 = f2, fs2 = fs2, m2 = m2, ms2 = ms2)

pars.solve.iasa = c(
  b1 = b1, b2 = b2, df1 = df1, dm1 = dm1, df2 = df2, dm2 = dm2,
  sf1 = sf1, sf2 = sf2, sm1 = sm1, sm2 = sm2, k1 = k1, k2 = k2,
  h1 = h1, h2 = h2, ab = ab, ad = ad, v = v, z = z)


#' La solución del modelo para las estimativas puntuales (las definidas anteriormente) es fácil.

#+
solve.iasa.pt <- SolveIASA(pars = pars.solve.iasa,
                           init = init.solve.iasa,
                           time = 0:20, method = 'rk4')

#' Podemos estar interesados en que tanto las diferentes poblaciones cambian a través del tiempo.<br>  

#' Por ejemplo, calculemos el cambio relativo del número total de perros domiciliados esterilizados desde el comienzo hasta el final del periodo simulado

#+
CalculatePopChange(model.out = solve.iasa.pt, variable = 'ns1', t1 = 0, t2 = 20)

#' y el cambio absoluto del número de hembras callejeras intactas, desde el quinto hasta el décimo año.

#+
CalculatePopChange(model.out = solve.iasa.pt, variable = 'fs2',
                   t1 = 5, t2 = 10, ratio = F)

#' La dinámica de las diferentes subpoblaciones también pude graficarse (vea la página de ayuda de `PlotModels`).

#+
PlotModels(model.out = solve.iasa.pt, variable = 'ns2', x.label = 'Años',
           y.label = 'Animales callejeros esterilizados')

#' También podemos simular escenarios para evaluar la interacción  entre diferentes combinaciones de tazas de esterilización, abandono, adopción e inmigración. En el próximo ejemplo crearemos 900 escenarios (50 tazas de esterilización, 3 tazas de abandono, 3 tazas de adopción y 2 tazas de inmigración).

#+
solve.iasa.rg <- SolveIASA(pars = pars.solve.iasa,
                           init = init.solve.iasa,
                           time = seq(0, 20, by = 0.5),
                           s.range = seq(from = 0, to = 0.4, length.out = 50),
                           ab.range = c(0, .2),
                           ad.range = c(0, .2),
                           im.range = c(0, .2),
                           method = 'rk4')

#+ fig.width = 11, fig.height = 11
PlotModels(model.out = solve.iasa.rg, variable = 'ns', x.label = 'Anos',
           y.label = 'Taxz de esterilización',
           scen.label = 'Im = __ * Capacidade de carga (domiciliados)',
           leg.label = c('Animales\ndomiciliados\nesterilizados',
                       'Animales \ncallejeros\nesterilizados'))


#' ***

####' ### 8. Priorización de las intervenciones de acuerdo con el efecto que producen, a través de análisis de sensibilidad. ####

#' Finalmente podemos realizar análisis de sensibilidad global y local para clasificar los parámetros de acuerdo con la influencia que tienen. Dado que las intervenciones de manejo poblacional son mecanismos para modificar (o mantener estable) parámetros poblacionales, la clasificación de los parámetros es también una clasificación de las intervenciones.

#' En los análisis de sensibilidad, perturbamos cada una de las estimativas previas para ver qué tan sensible es la dinámica poblacional a esas perturbaciones. Realicemos 100 (fijado por la función) simulaciones seleccionando aleatoriamente, en cada simulación, un valor posible para cada parámetro. Cada parámetro será muestreado de un conjunto de valores, siendo el mínimo y el máximo iguales a 90% y 110% de las respectivas estimativas puntuales.

#+
rg.solve.iasa <- SetRanges(pars = pars.solve.iasa, range = 0.1)
glob.all.solve.iasa <- CalculateGlobalSens(
  model.out = solve.iasa.pt,
  ranges = rg.solve.iasa,
  sensv = 'n2', all = T)
PlotGlobalSens(global.out = glob.all.solve.iasa,
               x.label = 'Tiempo', y.label = 'animales callejeros enteros',
               nam.leg = 'Rango de sensibilidad',
               sd.leg = 'media +- sd    ')
glob.all.solve.iasa

#' A diferencia de las simulaciones basadas únicamente en estimativas puntuales, aquí obtenemos un conjunto de resultados representados por una franja, en lugar de un único resultado representado por una línea.<br>  

#' Estimamos algunos parámetros usando técnicas muestrales y otros de forma subjetiva. Dado que hay incertidumbre con respecto al valor exacto de los parámetros, los resultados de las perturbaciones también representan nuestras incertidumbres.<br>    

#' Una pregunta natural es si la dinámica poblacional es igual de sensible a todos los parámetros. Si no, cuáles son los parámetros más influyentes? Para responder esas preguntas, podemos realizar análisis de sensibilidad global pero perturbando un parámetro por vez, fijando los otros parámetros en las estimativas puntuales.

#+ fig.width = 11, fig.height = 11
glob.solve.iasa <- CalculateGlobalSens(
  model.out = solve.iasa.pt,
  ranges = rg.solve.iasa,
  sensv = 'n2', all = F)
PlotGlobalSens(global.out = glob.solve.iasa,
               x.label = 'Tiempo', y.label = 'animales callejeros enteros',
               nam.leg = 'Rango de sensibilidad',
               sd.leg = 'media +- sd    ')
head(glob.solve.iasa)

#' Los análisis de sensibilidad local son otra posibilidad, en la cual se realizan perturbaciones mínimas y se determina la sensibilidad a cada parámetro, usando medidas de influencia.

#+ fig.width = 11, fig.height = 15
local.solve.iasa <- CalculateLocalSens(model.out = solve.iasa.pt, sensv = 'n2')
PlotLocalSens(local.out = local.solve.iasa,
              x.sens = 'Tiempo', y.sens = 'Sensibilidad',
              y.ind = c("L1", "L2", "Media", "Min", "Max"))
summary(local.solve.iasa)

#' Observando la sensibilidad global a cada parámetro o las sensibilidades locales (en las subfiguras L1 y L2), es claro que la capacidad de carga para la población callejera es de lejos el parámetro más influyente para el número total de perros callejeros (cuanto mayor la barra, mayor la influencia del respectivo parámetro).

####' ### Conclusión ####

#' El manejo poblacional de animales de compañía depende de procedimientos complejos de análisis de datos. Afortunadamente el `capm` ofrece varios algoritmos útiles y los usuarios tan solo tiene que colocar sus datos en las funciones apropiadas del `capm`. Actualmente el flujo de trabajo propuesto está más enfocado en poblaciones de perros. Versiones futuras del `capm` probablemente incluirán funciones adicionales para analizar particularidades de poblaciones de gatos.<br>  

#' El `capm` tiene funciones no mostradas aquí y para las funciones mostradas, argumentos adicionales que no usamos explícitamente ofrecen mayor flexibilidad. Vea las páginas de las funciones o la sección [Funciones](https://github.com/oswaldosantos/capm/wiki/1.3-Funciones) en la documentación.
