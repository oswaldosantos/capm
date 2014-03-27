#+ echo=F
# Uncomment the fifth line if you do not intend to compile a notebook.

#+ echo=F
#opts_chunk$set(comment=NA, tidy=FALSE, message=F, warnings=F, fig.align='center')

#' # Quick programming guide with capm 0.4 in R version 3.0.3
 
#' ### Oswaldo Santos, oswaldosant@gmail.com
#' ### [My Github repository](https://github.com/oswaldosantos)
#' ### [Web page for the `capm`](http://oswaldosantos.github.io/capm/)
#' ### Last update: 25/03/2014
#' ***
 
####' ### Introduction ####

#' In this quick guide I will show you how to implement the 8 basic steps of the purposed workflow (see the web page for the `capm`).<br>  
 
#' I will not give you technical details neither I Will go into discussions of the meaning of results (see the [book](https://github.com/oswaldosantos/capm/wiki/3.2-Book) if this is what you are looking  for). I will give you just the recipe.<br>  
 
#' Assumptions to reproduce this guide:

#' - Minimal background in R programming.
#' - Versions R 3.0.2 and RStudio 0.98.501. 
#' - Familiarity with R functions' help pages.
#' - The current working directory contains the following files, available [here](https://github.com/oswaldosantos/capm/tree/master/Documentation/Quick_programming_guide). 
#'  * pilot.csv
#'  * psu.ssu.csv
#'  * santos.dbf
#'  * santos.prj
#'  * santos.shp
#'  * santos.shx
#'  * survey.data.csv
#'  In the previous link you also will find a file named quick_programming_guide_capm_0.3.2.R with the code for this guide.

#+ 
library(capm)

#' ***

####' ### 1. Selection of sampling units for simple and complex surveys (pilot/final). ####

#' Let's begin with the selection of sampling units for a two-stage cluster sample. The file `psu.ssu.csv` contains data of Santos city in Brazil.  Data were extracted from a bureau of Statistics [(IBGE)](www.ibge.gov.br). The first column has unique identifiers of census tracks, our primary sampling units (PSU's). The second column contains the number of households in each PSU. Households are our secondary sampling units (SSU's) and they are also the measure of size for PSU's.<br>  

#' After importing the file

#+
psu.ssu <- read.csv(file='psu.ssu.csv')

#' we can see that there are 649 PSU.

#+
str(psu.ssu)

#' The first six rows give us an idea of the data.

#+ 
head(psu.ssu)

#' PSU's identifiers seems all equal but this just a scientific notation output. The identifiers must be unique for each PSU. To verify this requirement we can change the printing default or verify that the number of different identifiers is equal to the number of PSU.

#+
print(head(psu.ssu), digits=15)
length(unique(psu.ssu[ , 1]))

#' The file contains exactly the information we need to sample PSU's with probability proportional to their sizes, with replacement. If the `write` argument of `SamplePPS` is set as TRUE, selected PSU will be saved in a csv file, which can be viewed in a spreadsheet software. The output will have as many rows as selected PSU's. Remember that the same PSU can be selected more than once because sampling is with replacement.<br>  

#' If we use `set.seed(some_number)`, the next pseudo random sample always will be the same. In this quick guide I use `set.seed(4)` so you can reproduce exactly all the examples. However, in real applications you must not use `set.seed`.

#+ 
set.seed(4)
pilot.psu <- SamplePPS(psu.ssu=psu.ssu, psu=10, write=FALSE)

#' Inspecting the object we just created, we can see that the PSU's identifiers `class` were converted to `character`. This means that identifiers are now represented as text, not as numbers.

#+
str(pilot.psu)
head(pilot.psu)

#' Selecting SSU's is as simple as the previous selection. The output will have as many rows as selected SSU's in each PSU and as many columns as selected PSU's.

#+
set.seed(4)
pilot.ssu <- SampleSystematic(psu.ssu=pilot.psu, 5, write=F)

#' Let's see the first four columns to have an idea.

#+
head(pilot.ssu[ , 1:4])

#' ***

####' ### 2. Mapping primary sampling units to be visited (complex surveys). ####

#' Having selected the sampling units, we need to know their geographic location. Fortunately, `capm` has a function to locate the PSU's. If we have a shapefile of the PSU, we are done as in this case. In the working directory, there are five files named as "santos", each one with a different extension. All those files are a shapefile representation of the PSU's from the sampled area (Santos city). I got the files in the bureau of statistics previously mentioned.<br>  

#+
MapkmlPSU(shape='santos', psu=pilot.psu[, 1], id=1)

#' `MapkmlPSU` creates a kml file for each selected PSU plus a kml with all selected PSU's. Those kml files can be opened with Google Hearth just clicking on them. [QGIS](www.qgis.org) is an open source tool, which can also render different layers as a background to the kml files.<br>  

#' Of course, R allow us to plot the locations of the selected PSU's. Do not worry if you do not understand the following code snippet, it is just an alternative to Google Hearth or QGIS, which I am using here just to show that you can map the selected PSU.<br>  

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

#' Whatever the method used to produce the maps, we must to scketch a route in the map of each selected PSU, in order to go over all streets. We can set a household in an arbitrary point (i.e. the lower left location) as the first household. From it, we can go through the route counting the households (including both sides of streets fragment totally contained in the PSU) and interviewing those that were selected.<br>  

#' The following map shows the fourth selected PSU.

#+ 
osm.psu4 <- get_openstreetmap(bbox = c(-46.349, -23.962, -46.345, -23.957),
                              scale=5000)
ggmap(osm.psu4) +
  geom_polygon(data=santos.pilot[4, ], aes(x=long, y=lat), fill=NA,
               color='yellow', size=2) +
  coord_equal()

#' ***

####' ### 3. Calculation of sample size and composition (complex designs). ####

#' Calculating the size and composition of a two-stage cluster sampling is a complex task. Fortunately, in `capm` we just need to define the level of confidence we want, the error we are ready to accept and an estimate of cost. The last is the ratio between the cost associated with visiting a PSU and the cost associated with making an interview.<br>  

#' Two sources of data are needed. The first is the `psu.ssu` file we imported in the first section. The second contains the data we hypothetically collected in the pilot sample we designed above. The `pilot` file contains as many rows as households visited in the pilot. The first column contains identifiers for the PSU to which the respective household belongs to. The second column contains the number of dogs observed in the households.

#+ 
pilot <- read.csv('pilot.csv')
Calculate2StageSampleSize(psu.ssu=psu.ssu, psu.x=pilot,
                          level=0.95, error=0.1, cost=10)

#' ***

####' ### 4. Repetition of steps 1 and 2, for the final survey, if the workflow  begun with a pilot survey. ####

#' Once defined the size and composition for the final sample, selecting sampling units is a matter of repeating what we did in sections 1 and 2, using the output from section 3 (20 PSU's and 19 SSU's per PSU).

#' 

#+
final.psu <- SamplePPS(psu.ssu, 20, write=F)
final.ssu <- SampleSystematic(final.psu, 19, write=F)
# Uncomment the next line to creat the kml files.
#MapkmlPSU(shape='santos', psu=final.psu[, 1], id=1)

#' ***

####' ### 5. Estimation of demographic characteristics. ####

#' Having defined the final sample, suppose we went to visit all the selected households and the collected data were recorded in a file called `survey.data.csv`.<br>  

#' To see a description of each variable, see the help page of the file typing `?survey.data`.

#+ 
survey.data <- read.csv('survey.data.csv')
str(survey.data)
head(survey.data)

#' To estimate the population parameters, the first step is to define the sampling design from which the data come from. To do this, we need a file containing all the sampling units in the population (`psu.ssu`) and a file with the sampling data (`survey.data`). In this last file, the columns containing PSU's and SSU's identifiers must be specified, as well as the number of PSU's included in the sample (for PSU's included more than once, each occurrence must be counted).

#+ 
design <- DesignSurvey(psu.ssu=psu.ssu, sample=survey.data, psu.col=2,
                       ssu.col=1, psu.2cd=20)

#' Looking at the variables included in the design, we can see that the first two and the last three do not represent variables to be estimated. Those variables were created to define the sample design.

#+
names(design$variables)

#' Setting the type of estimate for each variable is easy. Empty quotes exclude a variable from the estimation procedure.

#+ 
variables <- c("", "", "total", "prop", "mean", "prop", "prop",
               "total", rep("prop", 8), "", "", "")

#' It is convenient to confirm we defined the type of estimates we wanted.

#+
cbind(names(design$variables), variables)

#' Now we are ready to get our first estimates.

#+
(estimates <- SummarySurvey(design=design, variables=variables, rnd=3))

#' The previous output is very useful but might not be enough. Let's make a copy (`sample1`) of a transformed subset of `survey.data`, to estimate the total number of sterilized animals (instead of the proportion) and to get estimates conditioned on sex.

#+
sample1 <- survey.data[, c('interview_id', 'psu', 'dogs', 'sex', 'sterilized',
                           'sterilized.ly', 'fate')]
sample1[, 'sterilized'] <- as.character(sample1[, 'sterilized'])
sample1[which(sample1$sterilized == "yes"), 'sterilized'] <- 1
sample1[which(sample1[, 'sterilized'] == "no"), 'sterilized'] <- 0
sample1[, 'sterilized'] <- as.numeric(sample1[, 'sterilized'])

#' After defining a sampling design in the usual way

#+
design.sex <- DesignSurvey(psu.ssu=psu.ssu, sample=sample1, 
                           psu.col=2, ssu.col=1, psu.2cd=20)

#' we can create a design for each sex.

#+ 
design.f <- subset(design.sex, sex == 'Female')
design.m <- subset(design.sex, sex == 'Male')

#' From here, there is nothing new.

#+
names(design.sex$variables)
variables.sex <- c("", "", "total", "", "total",
                   "prop", "prop", "", "", "")
cbind(names(design.sex$variables), variables.sex)
(estimates.f <- SummarySurvey(design.f, variables.sex, rnd=3))
(estimates.m <- SummarySurvey(design.m, variables.sex, rnd=3))

#' ***

####' ### 6. Construction of population pyramids, conditioned on sex, age and another categorical variable such as reproductive status. ####

#' Population pyramids summarizes the basic composition of a population. At minimum, they are built from age and sex variables but can be conditioned on a third categorical variable. The data to be used must have each variable in a separate column and those columns must be specified in the respective arguments' function.

#+
matrix(names(survey.data), ncol=1)
PlotPopPyramid(dat=survey.data, age.col='age', sex.col='sex')
PlotPopPyramid(dat=survey.data, age.col=5, sex.col=4, str.col=6)

#' ***

####' ### 7. Assessment of the effect produced by population management interventions, through mathematical modelling. ####

#' Now we are ready to simulate the effect of immigration, abandonment, sterilization and adoption, on owned and stray population dynamics. `SolveIASA` function uses many parameters to run a mathematical model of population dynamics. Some parameters are from the owned population and others from the stray population.<br>  

#' We have estimates for almost all parameters of the owned population but we have no estimates for the stray population. Based on literature and expert opinions, we can define subjective estimates for the stray population (in the next section we will assess how much the subjective estimates compromises the model results).

#+
# Initial conditions

# Owned dogs                  # Stray dogs
f1 <- 39537.848 - 12773.921;  f2 <- f1 * 0.1
fs1 <- 12773.921;             fs2 <- fs1 * 0.05
m1 <- 50254.640 - 9339.458;   m2 <- m1 * 0.1
ms1 <- 9339.458;              ms2 <- ms1 * 0.05


# Parameters

# Owned dogs                  # Stray dogs
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


#' Solving the model for point estimates (those we defined above) is straightforward.

#+
solve.iasa.pt <- SolveIASA(pars=pars.solve.iasa,
                           init=init.solve.iasa,
                           time=0:20, method='rk4')

#' We might be interested in how much different subopulations change through time.<br>  

#' For example, let's calculate the relative change in the total number of owned sterilized dogs from the beginning to the end of the simulated period

#+
CalculatePopChange(model.out=solve.iasa.pt, variable='ns1', t1=0, t2=20)

#' and the absolute change in the number of stray intact females from the fifth to the tenth year.

#+
CalculatePopChange(model.out=solve.iasa.pt, variable='fs2',
                   t1=5, t2=10, ratio=F)

#' The dynamics of different subpopulations can be plotted too (see the help page for `PlotModels`).

#+
PlotModels(model.out=solve.iasa.pt, variable='ns2')

#' We can also simulate scenarios to assess the interaction between different combinations of sterilization, abandonment, adoptions and immigration rates. In the following example we will create 900 scenarios (50 sterilization rates, 3 abandonment rates, 3 adoption rates and 2 immigration rates).

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
PlotModels(model.out=solve.iasa.rg, variable='ns')

#' ***

####' ### 8. Prioritize interventions according to the effect they produce, through sensitivity analysis. ####

#' Finally, we will make global and local sensitivity analysis to classify the parameters according to the influence they have. Because population management interventions are mechanisms to modify (or to maintain stability) population parameters, classification of parameters is also a classification for interventions.

#' In global sensitivity analysis, we perturb each one of the previous estimates to see how sensible is the population dynamics to those perturbations. Let's make 100 (fixed by the function) simulations selecting at random, in each simulation, one possible value for each parameter. Each parameter will be sampled from a set of values, being the minimum and maxim equal to 90% and 110% of the respective point estimate.

#+
rg.solve.iasa <- SetRanges(pars=pars.solve.iasa, range=0.1)
glob.all.solve.iasa <- CalculateGlobalSens(
  model.out=solve.iasa.pt,
  ranges=rg.solve.iasa,
  sensv='n2', all = T)
PlotGlobalSens(global.out=glob.all.solve.iasa)
glob.all.solve.iasa

#' In contrast to simulations based just in point estimates, here we got a set of possible results represented by an envelope, instead of a unique result represented by a line.<br>  

#' We estimated some parameters using sampling techniques and others parameters were estimated subjectively. Because there are uncertainties around what are the exact values for the parameters, results from perturbations are also a representation of our uncertainties.<br>    

#' A natural question arises. Population dynamics are equally sensible to all parameters? If not, what are the most influent parameters? To answer these questions, one approach is to make global sensitivity analysis perturbing one parameter at a time and fixing the others in the point estimates.

#+ fig.width=11, fig.height=11
glob.solve.iasa <- CalculateGlobalSens(
  model.out=solve.iasa.pt,
  ranges=rg.solve.iasa,
  sensv='n2', all=F)
PlotGlobalSens(global.out=glob.solve.iasa)
head(glob.solve.iasa)

#' Another approach is given by local sensitivity analysis. Here the idea is to make very small perturbations and determine the sensitivity to each parameter using measurements of influence.

#+ fig.width=11, fig.height=15
local.solve.iasa <- CalculateLocalSens(model.out=solve.iasa.pt, sensv='n2')
PlotLocalSens(local.out=local.solve.iasa)
summary(local.solve.iasa)

#' Looking at the global sensitivities to each parameters or looking at the local sensitivities (in L1 or L2 subplots), it is clear that carrying capacity for the stray population is by far the more influent parameter for total number of stray dogs (the greater the bar the greater the influence of the respective parameter).

####' ### Conclusion ####

#' Companion animal population management relies on complex data analysis procedures. Fortunately, the `capm` has many useful algorithms and users only have to put their data on the appropriate `capm` functions. Currently, the purposed workflow is more focused on dog populations. Future `capm` versions probably will include additional functions to address particularities of cat populations.<br>  

#' The `capm` has functions not showed here and for the fucntions we used, additional arguments which we did not use explicitly give more flexibility. See the functions' help pages or the [Funcions](https://github.com/oswaldosantos/capm/wiki/3.3-Functions) section in the documentation.
