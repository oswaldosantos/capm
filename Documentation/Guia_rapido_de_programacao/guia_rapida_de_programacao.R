#+ echo=F
# Si sua intenção é compilar um notebook, scomente a linha 5.

#+ echo=F
opts_chunk$set(comment=NA, tidy=FALSE, message=F, warnings=F, fig.align='center')

#' # Guia rápido de programação com o capm 0.4 em R versão 3.0.3

#' ### Oswaldo Santos, oswaldosant@gmail.com
#' ### [Meu repositório Github](https://github.com/oswaldosantos)
#' ### [Página web do `capm`](http://oswaldosantos.github.io/capm/)
#' ### Última atualização: 25/03/2014
#' ***

####' ### Introdução ####

#' Neste guia rápido lhe mostrarei como implementar os 8 passos básicos do fluxo de trabalho proposto (veja a página web do `capm`).<br>  

#' Não darei detalhes técnicos nem discutirei o significado dos resultados (ver o [livro](https://github.com/oswaldosantos/capm/wiki/2.2-Livro) se isto é o que está procurando). Apenas fornecerei a receita.<br>  

#' Pressupostos para reproduzir este guia:

#' - Conhecimento mínimo de programação em R.
#' - Versões R 3.0.2 y RStudio 0.98.501. 
#' - Familiaridade com as páginas de ajuda das funções do R.
#' - O diretório de trabalho atual contem os seguintes arquivos disponíveis [aqui](https://github.com/oswaldosantos/capm/tree/master/Documentation/Guia_rapido_de_programacao). 
#'  * pilot.csv
#'  * psu.ssu.csv
#'  * santos.dbf
#'  * santos.prj
#'  * santos.shp
#'  * santos.shx
#'  * survey.data.csv
#'  No enlace anterior também encontrará um arquivo chamado guia_rapido_de_programacao_capm_0.3.2.R com os códigos deste guia.

#+ 
library(capm)

#' ***

####' ### 1. Seleção de unidades amostrais para desenhos simples y complexos (piloto/final). ####

#' Comecemos com a seleção de unidades amostrais para uma amostra por conglomerados em dois estágios. O arquivo `psu.ssu.csv` contem dados da cidade Santos, Brasil. Os dados foram extraídos do [(IBGE)](www.ibge.gov.br). A primeira coluna contem identificadores únicos dos setores censitários, as nossas unidades primárias de amostragem (UPM). A segunda coluna contém o número de domicílios em cada UPM. Os domicílios são as nossas unidades secundárias de amostragem (USM) que também são a medida do tamanho das UPM.<br>  

#' Despois de importar o arquivo

#+
psu.ssu <- read.csv(file='psu.ssu.csv')

#' podemos ver que há 649 UPM.

#+
str(psu.ssu)

#' As 6 primeiras linhas dão uma ideia dos dados.

#+ 
head(psu.ssu)

#' Os identificadores das UPM parecem todos iguais mas isto es só um resultado em notação científica. Os identificadores devem ser únicos para cada UPM. Para conferir este requisito podemos mudar o padrão de impressão e verificar que o número de identificadores diferentes é igual ao número de UPM.

#+
print(head(psu.ssu), digits=15)
length(unique(psu.ssu[ , 1]))

#' O arquivo contém exatamente a informação que precisamos para amostrar UPM com probabilidade proporcional a seus tamanhos e com reposição. Se o argumento `write` de `SamplePPS` é definido como TRUE, as UPM selecionadas serão salvas em um arquivo csv, que pode ser visto em um software de planilhas. O resultado terá tantas linhas como UPM selecionadas. Lembre que a mesma UPM pode ser selecionada mais de uma vez porque a amostragem é com reposição.<br>  

#' Se usamos `set.seed(algum_numero)`, a próxima amostra pseudoaleatória sempre será a mesma. Em este guia rápido usarei `set.seed(4)` para que você possa reproduzir exatamente todos os exemplos. No entanto, em aplicações reais no deve usar `set.seed`.

#+ 
set.seed(4)
pilot.psu <- SamplePPS(psu.ssu=psu.ssu, psu=10, write=FALSE)

#' Inspecionando o objeto que acabamos de criar, podemos ver que a `class` dos identificadores das UPM foi convertida a  `character`. Isto significa que os identificadores agora são representados como texto, não como números.

#+
str(pilot.psu)
head(pilot.psu)

#' Selecionar USM é tão simples como a seleção anterior. O resultado terá tantas linhas como USM selecionadas em cada UPM e tantas colunas como UPM selecionadas.

#+
set.seed(4)
pilot.ssu <- SampleSystematic(psu.ssu=pilot.psu, 5, write=F)

#' Vejamos as quatro primeiras colunas para ter uma ideia.

#+
head(pilot.ssu[ , 1:4])

#' ***

####' ### 2. Mapeamento de unidades primárias de amostragem a serem visitadas (desenhos complexos). ####

#' Depois de selecionar as unidades de amostragem, temos que conhecer suas localizações geográficas. Felizmente o `capm` tem uma função para localizar as UPM. Se tivermos um shapefile das UPM, estamos feitos como neste caso. No diretório de trabalho há cinco arquivos chamados "santos", cada um com uma extensão diferente. Todos esses arquivos são uma representação shapefile das UPM da área amostrada (cidade Santos). Consegui os arquivos no IBGE (mencionado anteriormente).<br>  

#+
MapkmlPSU(shape='santos', psu=pilot.psu[, 1], id=1)

#' `MapkmlPSU` cria arquivos kml para cada UPM mais um kml com todas as UPM selecionadas. Esses arquivos kml podem ser abertos com Google Hearth clicando sobre os mesmos. [QGIS](www.qgis.org) é uma ferramenta de fonte aberta que também pode graficar diferentes camadas como fundo para os arquivos kml.<br>  

#' É claro, R permite-nos graficar as localizações das UPM selecionadas. Não se preocupe se não entende o seguinte fragmento de código, é só uma alternativa para ao Google Hearth e ao QGIS, que estou usando aqui só para mostrar que pode mapear as UPM selecionadas.<br>  

#+
library(rgdal); library(ggmap); library(maptools); library(plyr)

#+ fig.width=11, fig.height=11
santos <- readOGR(dsn='.', layer='santos')
santos.pilot <- santos[as.character(santos@data[ , 1]) %in% pilot.psu[ , 1], ]
santos.pilot <- spTransform(santos.pilot, CRS('+init=epsg:4326'))

santos.pilot@data$id <- rownames(santos.pilot@data)
santos.pilot.points <- fortify(santos.pilot, region="id")
santos.pilot.df <- join(santos.pilot.points, santos.pilot@data, by="id")

osm.all.psu <- get_openstreetmap(bbox = c(-46.384, -23.989, -46.299, -23.930),
                                 scale=34000, color='bw')
ggmap(osm.all.psu) + 
  geom_polygon(data=santos.pilot.df, aes(x=long, y=lat, fill=PSU),
               color='yellow', size=1.2) +
  coord_equal()

#' Qualquer que seja o método usado para produzir os mapas, devemos traçar um percurso no mapa de cada UPM selecionada, de forma que todas as ruas sejam percorridas. Podemos fixar um domicílio em um ponto arbitrário (ex. localização inferior esquerda) como o primeiro domicílio e a partir do mesmo, percorrer o percurso contando domicílios (incluindo as duas calçdas dos fragmentos de ruas contidos totalmente na UPM) e entrevistando os que foram selecionados.<br>  

#' O seguinte mapa mostra a quarta UPM amostrada.

#+ 
osm.psu4 <- get_openstreetmap(bbox = c(-46.349, -23.962, -46.345, -23.957),
                              scale=5000)
ggmap(osm.psu4) +
  geom_polygon(data=santos.pilot[4, ], aes(x=long, y=lat), fill=NA,
               color='yellow', size=2) +
  coord_equal()

#' ***

####' ### 3. Cálculo do tamanho e composição amostral (desenhos complexos). ####

#' O cálculo do tamanho e a composição das amostras conglomeradas em dois estágios é um processo complexo. Afortunadamente, com o `capm` só temos que definir o nível de confiança que queremos, o erro que estamos dispostos a aceitar e uma estimativa de custo. Esta última é a razão entre o custo associado à visita de uma UPM y o custo associado à realização de uma entrevista.<br>  

#' Duass fontes de dados são necessárias. A primeira é o arquivo `psu.ssu` que importamos na primeira seção. A segunda é o arquivo com os dados que hipoteticamente coletamos na amostra piloto que desenhamos anteriormente. O arquivo `pilot` contém tantas linhas como domicílios visitados no piloto. A primeira coluna contem identificadores das UPM às que pertencem os respectivos domicílios. A segunda coluna contém o número de cães observados em cada domicílio.

#+ 
pilot <- read.csv('pilot.csv')
Calculate2StageSampleSize(psu.ssu=psu.ssu, psu.x=pilot,
                          level=0.95, error=0.1, cost=10)

#' ***

####' ### 4. Repetição dos passos 1 e 2 para a amostra final, se começou-se com uma amostra piloto. ####

#' Uma vez definido o tamanho e a composição da amostra final, selecionar as unidades amostrais se reduz a repetir os passos 1 e 2, usando a saída da seção 3 (20 UPM e 19 USM por UPM).

#' 

#+
final.psu <- SamplePPS(psu.ssu, 20, write=F)
final.ssu <- SampleSystematic(final.psu, 19, write=F)
# Tirar o comentário da seguiente linhaa para os kml.
#MapkmlPSU(shape='santos', psu=final.psu[, 1], id=1)

#' ***

####' ### 5. Estimação de características demográficas (totais, proporções, médias y razões). ####

#' Depois de ter definido a amostra final, imaginemos que fomos a visitar todos os domicílios selecionados e que os dados coletados foram registrados em um arquivo chamado `survey.data.csv`.<br>  

#' Para ver a descrição de cada variável, veja a página de ajuda do arquivo com o comando `?survey.data`.

#+ 
survey.data <- read.csv('survey.data.csv')
str(survey.data)
head(survey.data)

#' Para estimar os parâmetros populacionais, o primeiro passo é definir o desenho amostral do qual provêm os dados. Para isto, precisamos um arquivo que contenha todas as unidades amostrais da população (`psu.ssu`) e um arquivo com os dados amostrais (`survey.data`). Neste último arquivo, as colunas que contêm os identificadores das UPM e das USM devem ser especificadas, assim como o número de UPM incluídas na amostra (para as UPM incluídas mais de uma vez, cada ocorrência deve ser contada).  

#+ 
design <- DesignSurvey(psu.ssu=psu.ssu, sample=survey.data, psu.col=2,
                       ssu.col=1, psu.2cd=20)

#' Olhando as variáveis incluídas no desenho podemos notar que as duas primeiras e as três últimas não representam variáveis a serem estimadas. Essas variáveis foram criadas para definir o desenho amostral

#+
names(design$variables)

#' Definir o tipo de estimativa para cada variável é fácil. Aspas vazias excluem uma variável do processo de estimação.

#+ 
variables <- c("", "", "total", "prop", "mean", "prop", "prop",
               "total", rep("prop", 8), "", "", "")

#' É conveniente confirmar que definimos o tipo de estimativas que queremos.

#+
cbind(names(design$variables), variables)

#' Agora estamos prontos para obter as nossas primeiras estimativas.

#+
(estimates <- SummarySurvey(design=design, variables=variables, rnd=3))

#' A saída anterior é bastante útil mas puede não ser suficiente. Façamos una copia (`sample1`) de um subconjunto transformado de `survey.data`, para estimar o número total de animais esterilizados (no lugar da proporção) e para obter estimativas condicionadas ao sexo.

#+
sample1 <- survey.data[, c('interview_id', 'psu', 'dogs', 'sex', 'sterilized',
                           'sterilized.ly', 'fate')]
sample1[, 'sterilized'] <- as.character(sample1[, 'sterilized'])
sample1[which(sample1$sterilized == "yes"), 'sterilized'] <- 1
sample1[which(sample1[, 'sterilized'] == "no"), 'sterilized'] <- 0
sample1[, 'sterilized'] <- as.numeric(sample1[, 'sterilized'])

#' Depois de definir o desenho amostral da forma usual

#+
design.sex <- DesignSurvey(psu.ssu=psu.ssu, sample=sample1, 
                           psu.col=2, ssu.col=1, psu.2cd=20)

#' podemos criar o desenho para cada sexo.

#+ 
design.f <- subset(design.sex, sex == 'Female')
design.m <- subset(design.sex, sex == 'Male')

#' A partir daqui, não há nada novo.

#+
names(design.sex$variables)
variables.sex <- c("", "", "total", "", "total",
                   "prop", "prop", "", "", "")
cbind(names(design.sex$variables), variables.sex)
(estimates.f <- SummarySurvey(design.f, variables.sex, rnd=3))
(estimates.m <- SummarySurvey(design.m, variables.sex, rnd=3))

#' ***

####' ### 6. Construção de pirâmides populacionais, condicionadas pelo sexo, a idade e outra variável categórica como o estado reprodutivo. ####

#' As pirâmides populacionais resumem a composição básica de uma população e como mínimo são construídas usando as variáveis idade e sexo, mas podem ser condicionadas a uma terceira variável categórica. Os dados a serem usados devem ter cada variável em uma coluna separada e essas colunas devem ser especificadas nos respectivos argumentos da função. Para que as etiquetas da figura fiquem em português, devemos transformar os `"labels"` das variáveis `"sex"` e `"sterilized"`, bem como o nome desta última.

#+
library(plyr)
matrix(names(survey.data), ncol=1)
names(survey.data)[6] <- 'Esterilizado'
survey.data$sex <- revalue(survey.data$sex,
                           c('Female'='Fêmea', 'Male'='Macho'))
survey.data$Esterilizado <- revalue(survey.data$Esterilizado,
                                    c('yes'='sim', 'no'='não'))
PlotPopPyramid(dat=survey.data, age.col='age', sex.col='sex',
               stage.label='Anos')
PlotPopPyramid(dat=survey.data, age.col=5, sex.col=4, str.col=6,
               stage.label='Anos')

#' ***

####' ### 7. Avaliação dos efeitos produzidos por intervenções de manejo populacional, por meio de modelagem matemática. ####

#' Agora estamos prontos para simular o efeito da imigração, o abandono, a esterilização e a adoção, na dinâmica de populações domiciliadas e não domiciliadas. A função `SolveIASA` precisa vários parâmetros para executar um modelo matemático de dinâmica populacional. Alguns parâmetros são da população domiciliada e outros da população não domiciliada.<br>  

#' Temos estimativas para a maioria dos parâmetros da população domiciliada, mas não temos estimativas para a população não domiciliada. Com base na literatura e na opinião de expertos, podemos definir estimativas subjetivas para a população domiciliada (na próxima seção avaliaremos que tanto as estimativas subjetivas comprometem os resultados do modelo)

#+
# Condiciones iniciais

# Cães domiciliados           # Cães não domiciliados
f1 <- 39537.848 - 12773.921;  f2 <- f1 * 0.1
fs1 <- 12773.921;             fs2 <- fs1 * 0.05
m1 <- 50254.640 - 9339.458;   m2 <- m1 * 0.1
ms1 <- 9339.458;              ms2 <- ms1 * 0.05


# Parâmetros

# Cães domiciliados           # Cães não domiciliados
b1 <-  7719.074;              b2 <- b1 * 0.15
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
  f1=f1, fs1=fs1, m1=m1, ms1=ms1,
  f2=f2, fs2=fs2, m2=m2, ms2=ms2)

pars.solve.iasa = c(
  b1=b1, b2=b2, df1=df1, dm1=dm1, df2=df2, dm2=dm2,
  sf1=sf1, sf2=sf2, sm1=sm1, sm2=sm2, k1=k1, k2=k2,
  h1=h1, h2=h2, ab=ab, ad=ad, v=v, z=z)


#' A solução do modelo para as estimativas pontuais (as definidas anteriormente) é fácil.

#+
solve.iasa.pt <- SolveIASA(pars=pars.solve.iasa,
                           init=init.solve.iasa,
                           time=0:20, method='rk4')

#' Podemos estar interessados em que tanto as diferentes populações mudam ao longo do tempo.<br>  

#' Por exemplo, calculemos a mudança relativa do número total de cães domiciliados esterilizados desde o começo até o final d período simulado.

#+
CalculatePopChange(model.out=solve.iasa.pt, variable='ns1', t1=0, t2=20)

#' e a mudança absoluta do número de fêmeas não domiciliadas intactas, desde o quinto até o décimo ano.

#+
CalculatePopChange(model.out=solve.iasa.pt, variable='fs2',
                   t1=5, t2=10, ratio=F)

#' A dinâmica das diferentes subpopulações também pode graficar-se (veja a página de ajuda de `PlotModels`).

#+
PlotModels(model.out=solve.iasa.pt, variable='ns2', x.label='Anos',
           y.label='Animais não domiciliados esterilizados')

#' Também podemos simular cenários para avaliar a interação entre diferentes combinações de taxas de esterilização, abandono, adoção e imigração. No próximo exemplo criaremos 900 cenários (50 taxas de esterilização, 3 taxas de abandono, 3 taxas de adoção e 2 taxas de imigração).

#+
solve.iasa.rg <- SolveIASA(pars=pars.solve.iasa,
                           init=init.solve.iasa,
                           time=seq(0, 20, by=0.5),
                           s.range=seq(from=0, to=0.4, length.out=50),
                           ab.range=c(0, .2),
                           ad.range=c(0, .2),
                           im.range=c(0, .2),
                           method='rk4')

#+ fig.width=11, fig.height=11
PlotModels(model.out=solve.iasa.rg, variable='ns', x.label='Anos',
           y.label='Taaxa de esterilização',
           scen.label='Im = __ * Capacidade de suporte (domiciliados)',
           leg.label=c('Animais\ndomiciliados\nesterilizados',
                       'Animais não\ndomiciliados\nesterilizados'))

#' ***

####' ### 8. Priorização das intervenções de acordo com o efeito que produzem, por meio de análises de sensibilidade. ####

#' Finalmente podemos realizar análises de sensibilidade global e local para classificar os parâmetros de acordo com a influência que têm. Dado que as intervenções de manejo populacional são mecanismos para modificar (o manter estável) parâmetros populacionais, a classificação dos parâmetros é também uma classificação das intervenções.

#' Nas análises de sensibilidade, perturbamos cada uma das estimativas prévias para ver que tão sensível é a dinâmica populacional a essas perturbações. Realizemos 100 (fixado pela função) simulações selecionando aleatoriamente, em cada simulação, um valor possível para cada parâmetro. Cada parâmetro será amostrado de um conjunto de valores, sendo o mínimo e o máximo iguais a 90% e 110% das respectivas estimativas pontuais.

#+
rg.solve.iasa <- SetRanges(pars=pars.solve.iasa, range=0.1)
glob.all.solve.iasa <- CalculateGlobalSens(
  model.out=solve.iasa.pt,
  ranges=rg.solve.iasa,
  sensv='n2', all = T)
PlotGlobalSens(global.out=glob.all.solve.iasa,
               x.label='Tempo', y.label='animais não domiciliados inteiros',
               nam.leg='Intervalo de sensibilidade',
               sd.leg='média +- sd    ')
glob.all.solve.iasa

#' A diferença das simulações baseadas unicamente em estimativas pontuais, aqui obtemos um conjunto de resultados representados por um envelope, no lugar de um único resultado representado por uma linha.<br>  

#' Estimamos alguns parâmetros usando técnicas amostrais e outros de forma subjetiva. Dado que há incertezas ao respeito do valor exato dos parâmetros, os resultados das perturbações também representam nossas incertezas.<br>    

#' Uma pregunta natural é se a dinâmica populacional é igual de sensível a todos os parâmetros. Caso não seja, quais são os parâmetros mais influentes? Para responder essas preguntas, podemos realizar análises de sensibilidade global, mas perturbando um parâmetro por vez, fixando os outros parâmetros nas estimativas pontuais.

#+ fig.width=11, fig.height=11
glob.solve.iasa <- CalculateGlobalSens(
  model.out=solve.iasa.pt,
  ranges=rg.solve.iasa,
  sensv='n2', all=F)
PlotGlobalSens(global.out=glob.solve.iasa,
               x.label='Tempo', y.label='animais não domiciliados inteiros',
               nam.leg='Intervalo de sensibilidade',
               sd.leg='média +- sd    ')
head(glob.solve.iasa)

#' As análises de sensibilidade local são outra possibilidade, na qual se realizam perturbações mínimas e se determina a sensibilidade a cada parâmetro, usando medidas de influência.

#+ fig.width=11, fig.height=15
local.solve.iasa <- CalculateLocalSens(model.out=solve.iasa.pt, sensv='n2')
PlotLocalSens(local.out=local.solve.iasa,
              x.sens='Tempo', y.sens='Sensibilidade',
              y.ind=c("L1", "L2", "Média", "Min", "Max"))
summary(local.solve.iasa)

#' Observando a sensibilidade global a cada parâmetro ou as sensibilidades locais (nas subfiguras L1 e L2), é claro que a capacidade de suporte para a população não domiciliada é de longe o parâmetro mais influente para o número total de cães não domiciliados (quanto maior a barra, maior a influência do respectivo parâmetro).

####' ### Conclusão ####

#' O manejo populacional de animais de companhia depende de procedimentos complexos de análises de dados. Afortunadamente o `capm` oferece vários algoritmos úteis e os usuários só têm que colocar seus dados nas funções apropriadas do `capm`. Atualmente o fluxo de trabalho proposto está mais focado em populações de cães Versões futuras do `capm` provavelmente incluirão funções adicionais para analisar particularidades de populações de gatos.<br>  

#' O `capm` tem funções no mostradas aqui e para as funções mostradas, argumentos adicionais que no usamos explicitamente oferecem maior flexibilidade. Veja as páginas das funciones o a seção [Funções](https://github.com/oswaldosantos/capm/wiki/2.3-Funções) na documentação.
