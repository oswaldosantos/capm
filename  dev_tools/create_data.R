library(capm); library(rgdal)

#### Files required ####

wd = getwd()
setwd('~/Downloads/sp')
shape = readOGR('.', '35SEE250GC_SIR')
setwd('~/Downloads/base/CSV')
santos.dat = read.table('Basico_SP2.csv', sep = ';', dec = '.',
                        head = T, quote = '\"')

#### santos shapefile ####

setwd("~/Copy/Projectos/capm/inst/extdata")
santos.shp = shape[shape@data[, 'NM_MUNICIP'] == 'SANTOS', ]
santos = santos.shp[santos.shp@data[, 'TIPO'] == 'URBANO', 2]
names(santos@data) = 'psu'
writeOGR(santos, dsn = ".", layer = "santos", driver = "ESRI Shapefile")

#### psu.ssu (universe) ####

setwd("~/Copy/Projectos/capm/data")
psu.ssu = merge(santos.dat, santos.shp@data, by.x = 1, by.y = 2)
psu.ssu = psu.ssu[psu.ssu[, 20] < 4, c(1, 21)]
psu.ssu = psu.ssu[complete.cases(psu.ssu), ]
names(psu.ssu) = c('psu', 'ssu')
save(psu.ssu, file = 'psu.ssu.rda')

#### pilot ####
set.seed(4)
pilot = SamplePPS(psu.ssu, 10)
pilot = data.frame(psu = rep(pilot[, 1], each = 5), dogs = rpois(50, .8))
save(pilot, file = 'pilot.rda')

#### survey.data ####
Calculate2StageSampleSize(psu.ssu, pilot, cost = 10)
set.seed(4)
sa1 = SamplePPS(psu.ssu, 38)
sa1 = data.frame(interview_id = 1:(38 * 12), 
                 psu = as.numeric(rep(sa1[, 1], each = 12)),
                 dogs = rpois((38 * 12), 0.8))

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
survey.data <- cbind(survey.data, sex = NA, age = NA,
                esterilized = 0, esterilized1 = 0,
                adopted = NA, births = 0, 
                present = 0, death = 0, lost = 0)

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1) {
    survey.data[i, 'sex'] <- sample(c('Female', 'Male'), 1, prob = c(.47, .53))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1) {
    survey.data[i, 'age'] <- sample(0:18, 1, prob = seq(.999, .001, length.out = 19))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1 & survey.data[i, 'sex'] == 'Female') {    
    survey.data[i, 'esterilized'] <- sample(c('yes', 'no'), 1, prob = c(.25, .75))
  }
  if (survey.data[i, 'dogs'] == 1 & survey.data[i, 'sex'] == 'Male') {    
    survey.data[i, 'esterilized'] <- sample(c('yes', 'no'), 1, prob = c(.15, .85))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'esterilized'] == 'yes' & survey.data[i, 'sex'] == 'Female') {
    survey.data[i, 'esterilized1'] <- sample(c('yes', 'no'), 1, prob = c(.3, .7))
  }
  if (survey.data[i, 'esterilized'] == 'yes' & survey.data[i, 'sex'] == 'Male') {    
    survey.data[i, 'esterilized1'] <- sample(c('yes', 'no'), 1, prob = c(.20, .70))
  }
}

survey.data[survey.data$esterilized == 0, 'esterilized'] <- 'no'
survey.data[survey.data$esterilized1 == 0, 'esterilized1'] <- 'no'

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1) {    
    survey.data[i, 'adopted'] <- sample(c('yes', 'no'), 1, prob = c(.1, .9))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1 & survey.data[i, 'sex'] == 'Female') {    
    survey.data[i, 'births'] <- sample(c(0, rpois(1, 4)), 1, prob = c(.85, .15))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'dogs'] == 1) {    
    survey.data[i, 'present'] <- sample(c('yes', 'no'), 1, prob = c(.85, .15))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'present'] == 'no') {    
    survey.data[i, 'death'] <- sample(c('yes', 'no'), 1, prob = c(.6, .4))
  }
}

for (i in 1:nrow(survey.data)) {
  if (survey.data[i, 'present'] == 'no' & survey.data[i, 'death'] == 'no') {
    survey.data[i, 'lost'] <- 'yes'
  }
}

survey.data[survey.data[, 'death'] == '0', 'death'] <- 'no'
survey.data[survey.data[, 'lost'] == '0', 'lost'] <- 'no'

survey.data[survey.data[, 'dogs'] == 0, c('esterilized', 'esterilized1', 'births', 'present', 'death', 'lost')] = NA

for (i in 1:12) {
  if (is.character(survey.data[, i])) {
    survey.data[, i] = as.factor(survey.data[, i])
  }
}


#### check ####
str(survey.data)
PlotPopPyramid(survey.data, age.col = 5, sex.col = 4, str.col = 6)
summary(survey.data[survey.data$sex == 'Female', 'esterilized1'])
summary(survey.data[survey.data$sex == 'Male', 'esterilized1'])

setwd("~/Copy/Projectos/capm/data")
save(survey.data, file = 'survey.data.rda')
