#### santos shp ####
wd = getwd()
setwd('/home/oswaldo/Documents/ITEC/Cursos Dinâmica/Curso 2013/Palestras/sp')
mp = readOGR('.', '35SEE250GC_SIR')
st = mp[mp@data[, 'NM_MUNICIP'] == 'SANTOS', ]
sts = read.table('Basico_SP2.csv', sep = ',', dec = '.', head = T, quote = '\"')
St = merge(sts, st@data, by.x = 1, by.y = 2)

psu.ssu = St[St[, 20] < 4, c(1, 21)]
psu.ssu = psu.ssu[complete.cases(psu.ssu), ]
names(psu.ssu) = c('psu', 'ssu')
save(psu.ssu, file = 'psu.ssu.rda')

pilot = ppssr(psu.ssu, 20)
pilot = data.frame(psu = rep(pilot[, 1], each = 5), dogs = rpois(100, .8))
save(pilot, file = 'pilot.rda')

santos = st[st@data[, 'TIPO'] == 'URBANO', 2]
names(santos@data) = 'psu'
writeOGR(santos, dsn = ".", layer = "santos", driver = "ESRI Shapefile")


#### Sample dataset ####
clus2(psu.ssu, pilot)
sa1 = ppssr(psu.ssu, 324)
sa1 = data.frame(interview_id = 1:1296, 
                 psu = as.numeric(rep(sa1[, 1], each = 4)),
                 dogs = rpois(1296, 0.8))
Sample <- NULL
for (i in 1:nrow(sa1)) {
  j <- 1
  if (sa1[i, 3] != 0) {
    while(j <= sa1[i, 3]) {
      Sample <- rbind(Sample, sa1[i, ])
      j <- j + 1
    }
  } else {
    Sample <- rbind(Sample, sa1[i, ])
  }
}

Sample[Sample$dogs > 1, 'dogs'] <- 1
Sample <- cbind(Sample, sex = NA, age = NA,
                castrated = 0, castrated1 = 0,
                adopted = NA, births = 0, 
                present = 0, death = 0, lost = 0)

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1) {
    Sample[i, 'sex'] <- sample(c('Female', 'Male'), 1, prob = c(.47, .53))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1) {
    Sample[i, 'age'] <- sample(0:18, 1, prob = seq(.99, .01, length.out = 19))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1 & Sample[i, 'sex'] == 'Female') {
    Sample[i, 'castrated'] <- sample(c('yes', 'no'), 1, prob = c(.25, .75))
  }
  if (Sample[i, 'dogs'] == 1 & Sample[i, 'sex'] == 'Male') {
    Sample[i, 'castrated'] <- sample(c('yes', 'no'), 1, prob = c(.15, .85))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'castrated'] == 'yes' & Sample[i, 'sex'] == 'Female') {
    Sample[i, 'castrated1'] <- sample(c('yes', 'no'), 1, prob = c(.3, .7))
  }
  if (Sample[i, 'castrated'] == 'yes' & Sample[i, 'sex'] == 'Male') {
    Sample[i, 'castrated1'] <- sample(c('yes', 'no'), 1, prob = c(.25, .75))
  }
}

Sample[Sample$castrated == 0, 'castrated'] <- 'no'
Sample[Sample$castrated1 == 0, 'castrated1'] <- 'no'

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1) {
    Sample[i, 'adopted'] <- sample(c('yes', 'no'), 1, prob = c(.1, .9))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1 & Sample[i, 'sex'] == 'Female') {
    Sample[i, 'births'] <- sample(c(0, rpois(1, 4)), 1, prob = c(.85, .15))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'dogs'] == 1) {
    Sample[i, 'present'] <- sample(c('yes', 'no'), 1, prob = c(.85, .15))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'present'] == 'no') {
    Sample[i, 'death'] <- sample(c('yes', 'no'), 1, prob = c(.6, .4))
  }
}

for (i in 1:nrow(Sample)) {
  if (Sample[i, 'present'] == 'no' & Sample[i, 'death'] == 'no') {
    Sample[i, 'lost'] <- 'yes'
  }
}

Sample[Sample[, 'death'] == '0', 'death'] <- 'no'
Sample[Sample[, 'lost'] == '0', 'lost'] <- 'no'

Sample[Sample[, 'dogs'] == 0, c('castrated', 'castrated1', 'births', 'present', 'death', 'lost')] = NA

for (i in 1:12) {
  if (is.character(Sample[, i])) {
    Sample[, i] = as.factor(Sample[, i])
  }
}
pyramid(Sample, col.age = 5, col.sex = 4, col.cas = 6)

#### test ####
str(Sample)
summary(Sample[Sample$sex == 'Female', 'castrated1'])
summary(Sample[Sample$sex == 'Male', 'castrated1'])

save(Sample, file = '/home/oswaldo/Documents/Projects/capms/capm/capm/data/Sample.rda')
