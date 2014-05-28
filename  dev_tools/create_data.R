library(capm); library(rgdal)

#### Files required ####

wd = getwd()
setwd('~/Downloads/sp')
shape = readOGR('.', '35SEE250GC_SIR')
setwd('~/Downloads')
santos.dat = read.csv('Basico_SP2.csv', sep = ';')

#### santos shapefile ####

setwd("~/Copy/Projectos/capm/inst/extdata")
santos.shp = shape[shape@data[, 'NM_MUNICIP'] == 'SANTOS', 2]
names(santos.shp@data) = 'PSU'
writeOGR(santos.shp, dsn = ".", layer = "santos",
         driver = "ESRI Shapefile", overwrite_layer = T)

#### psu.ssu (universe) ####

setwd("~/Copy/Projectos/capm/data")
psu.ssu = merge(santos.dat, santos.shp@data, by = 1)
psu.ssu = psu.ssu[complete.cases(psu.ssu), c(1, 21)]
names(psu.ssu) = c('psu', 'ssu')
save(psu.ssu, file = 'psu.ssu.rda')

#### pilot ####
set.seed(4)
pilot = SamplePPS(psu.ssu, 10)
set.seed(4)
pilot = data.frame(psu = rep(pilot[, 1], each = 10), dogs = rpois(50, .8))
save(pilot, file = 'pilot.rda')

#### survey.data ####
Calculate2StageSampleSize(psu.ssu, pilot, cost = 10)
set.seed(17)
sa1 = SamplePPS(psu.ssu, 20)
sa1 = data.frame(interview_id = 1:(20 * 19), 
                 psu = as.numeric(rep(sa1[, 1], each = 19)),
                 dogs = rpois((20 * 19), 0.8))
survey.data <- NULL

for (i in 1:nrow(sa1)) {
  j <- 1
  if (sa1[i, 3] != 0) {
    while(j <= sa1[i, 3]) {
      survey.data <- rbind(survey.data, sa1[i, ])
      j <- j + 1
    }
  } else {
    survey.data <- rbind(survey.data, sa1[i, ])
  }
}
survey.data[survey.data$dogs > 1, 'dogs'] <- 1

survey.data <- cbind(survey.data, sex = 0, age = NA, sterilized = 0,
                     sterilized.ly = 0, births = 0, present = 0, fate = NA,
                     acquired = 0, outside = 0, acquired.ly = 0, immigrant = 0,
                     immigrant.ly = 0, immigrant.sterilized.ly = 0)

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1) {
    survey.data[i, 'sex'] <- sample(c('Female', 'Male'), 1, p = c(.47, .53))
    survey.data[i, 'age'] <- sample(0:18, 1, prob = seq(.999, .005, l = 19))
    survey.data[i, 'present'] <- sample(c('yes', 'no'), 1, p = c(.85, .15))
    survey.data[i, 'acquired'] <- sample(
      c('bought', 'adopted', 'gift', 'born_in_home'), 1,
      prob = c(.2, .1, .58, .12))
    survey.data[i, 'outside'] <- sample(c('yes', 'no'), 1, p = c(.08, .92))
    survey.data[i, 'acquired.ly'] <- sample(c('yes', 'no'), 1, p = c(.4, .6))
  }
  if (survey.data[i, 'sex'] == 'Female') {    
    survey.data[i, 'sterilized'] <- sample(c('yes', 'no'), 1, p = c(.25, .75))
    survey.data[i, 'births'] <- sample(c(0, rpois(1, 4)), 1, p = c(.85, .15))
  }
  if (survey.data[i, 'sex'] == 'Male') {    
    survey.data[i, 'sterilized'] <- sample(c('yes', 'no'), 1, p = c(.2, .8))
  }
  if (survey.data[i, 'present'] == 'yes') {
    survey.data[i, 'fate'] <- 'in_home'
  }
  if (survey.data[i, 'present'] == 'no') {    
    survey.data[i, 'fate'] <- sample(c('died', 'lost', 'given', 'sold'), 1,
                                     p = c(.4, .2, .2, .2))
  }
  if (survey.data[i, 'sterilized'] == 'yes' & survey.data[i, 'sex'] == 'Female') {
    survey.data[i, 'sterilized.ly'] <- sample(c('yes', 'no'), 1, p = c(.3, .7))
  }
  if (survey.data[i, 'sterilized'] == 'yes' & survey.data[i, 'sex'] == 'Male') {    
    survey.data[i, 'sterilized.ly'] <- sample(c('yes', 'no'), 1, p = c(.2, .8))
  }
  if (survey.data[i, 'sterilized'] == 'no') {
    survey.data[i, 'sterilized.ly'] <- 'no'
  }
  if (survey.data[i, 'acquired'] == 'bought' |
        survey.data[i, 'outside'] == 'yes') {    
    survey.data[i, 'immigrant'] <- 'yes'
  }
  if (survey.data[i, 'acquired'] != 0 &
        survey.data[i, 'acquired'] != 'bought' &
        survey.data[i, 'outside'] != 0 & survey.data[i, 'outside'] != 'yes') {
    survey.data[i, 'immigrant'] <- 'no'
  }
  if (survey.data[i, 'immigrant'] == 'yes' &
        survey.data[i, 'acquired.ly'] == 'yes') {
    survey.data[i, 'immigrant.ly'] <- 'yes'
  }
  if ((survey.data[i, 'immigrant'] == 'yes' &
         survey.data[i, 'acquired.ly'] == 'no') |
        survey.data[i, 'immigrant'] == 'no') {
    survey.data[i, 'immigrant.ly'] <- 'no'
    survey.data[i, 'immigrant.sterilized.ly'] <- 'no'
  }
  if (survey.data[i, 'immigrant.ly'] == 'yes') {
    survey.data[i, 'immigrant.sterilized.ly'] <- sample(c('yes', 'no'), 1,
                                                        prob = c(.2, .8))
  }
}

survey.data[survey.data[, 'dogs'] == 0, 4:ncol(survey.data)] <- NA
survey.data$immigrant.ly[survey.data$immigrant.ly == '0'] <- NA
survey.data$immigrant.sterilized.ly[survey.data$immigrant.sterilized.ly == '0'] <- NA

for (i in 1:ncol(survey.data)) {
  if (is.character(survey.data[, i])) {
    survey.data[, i] = as.factor(survey.data[, i])
  }
}

# Check
summary(survey.data)
str(survey.data)
PlotPopPyramid(survey.data, age.col = 5, sex.col = 4, str.col = 6)
summary(survey.data[survey.data$sex == 'Female', 'sterilized.ly'])
summary(survey.data[survey.data$sex == 'Male', 'sterilized.ly'])
summary(survey.data[survey.data$sex == 'Female', 'fate'])
summary(survey.data[survey.data$sex == 'Male', 'fate'])

setwd("~/Copy/Projectos/capm/data")
save(survey.data, file = 'survey.data.rda')

#### Write csv's ####
setwd("~/Copy/Projectos/capm/Documentation/Quick_programming_guide")
data(psu.ssu)
write.csv(psu.ssu, 'psu.ssu.csv', row.names = F)
data(pilot)
write.csv(pilot, 'pilot.csv', row.names = F)
data(survey.data)
write.csv(survey.data, 'survey.data.csv', row.names = F)

