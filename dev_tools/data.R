library(rgdal); library(rvest); library(capm)
cat("\014") 
rm(list = ls())

### Real data for census 2010 in Santos city -----------------------------------

# File: Basico_SP2.csv
# Source: IBGE -> Estatisticas -> Censos -> Censo_demografico_2010 ->
#         Resultados_do_universo -> Agregados_por_setores_censitarios ->
#         SP_Exceto_a_Capital_20150527.zip -> SP Exceto a Capital ->
#         Base informacoes setores2010 universo SP_Exceto_Capital ->
#         CSV
# Date of access: 13/04/2017
# Variables:
#  Nome_do_municipio: Nome do municipio
#  Cod_setor: Codigo do setor
#  Situacao_setor:
#   Situação urbana – códigos: 1, 2 e 3
#   - 1 - Área urbanizada de cidade ou vila
#   - 2 - Área não-urbanizada de cidade ou vila
#   - 3 - Área urbana isolada
#   Situação rural – códigos: 4, 5, 6, 7 e 8.
#   - 4 - Aglomerado rural de extensao urbana
#   - 5 - Aglomerado rural isolado – povoado
#   - 6 - Aglomerado rural isolado – nucleo
#   - 7 - Aglomerado rural isolado - outros aglomerados
#  V003: Média do número de moradores em domicílios particulares permanentes
basic <- read.csv2("./dev_tools/datasets/Basico_SP2.csv", encoding = "latin1")
basic <- basic[basic$Nome_do_municipio == "SANTOS",
               c("Cod_setor", "Situacao_setor", "V003")]

# File: Domicilio01_SP2.csv
# Source: IBGE -> Estatisticas -> Censos -> Censo_demografico_2010 ->
#         Resultados_do_universo -> Agregados_por_setores_censitarios ->
#         SP_Exceto_a_Capital_20150527.zip -> SP Exceto a Capital ->
#         Base informacoes setores2010 universo SP_Exceto_Capital ->
#         CSV
# Date of access: 13/04/2017
# Variables:
#  Cod_setor: Codigo do setor
#  V001: Domicilios particulares e domicilios coletivos
hh01 <- read.csv2("./dev_tools/datasets/Domicilio01_SP2.csv",
                  encoding = "latin1")
hh01 <- hh01[, c("Cod_setor", "V001")]

# File: Domicilio02_SP2.csv
# Source: IBGE -> Estatisticas -> Censos -> Censo_demografico_2010 ->
#         Resultados_do_universo -> Agregados_por_setores_censitarios ->
#         SP_Exceto_a_Capital_20150527.zip -> SP Exceto a Capital ->
#         Base informacoes setores2010 universo SP_Exceto_Capital ->
#         XLS -> then saved as csv
# Date of access: 13/04/2017
# Variables:
#  Cod_setor: Codigo do setor
#  V001: Moradores em domicilios particulares e domicilios coletivos
hh02 <- read.csv2("./dev_tools/datasets/Domicilio02_SP2.csv",
                  encoding = "latin1")
hh02 <- hh02[, c("Cod_setor", "V001")]

city <- merge(basic, hh01, by = "Cod_setor")
city <- merge(city, hh02, by = "Cod_setor")
names(city) <- c("track_id", "track_status",
                 "persons_mean", "hh", "persons")
city$persons_mean[is.na(city$persons_mean)] <- mean(city$persons_mean, na.rm = T)

## Hypothetical composition (persons, dogs and cats) of households -------------
hh <- data.frame(track_id = rep(city$track_id, city$hh))

hh_id <- c()
for (i in seq_along(city$hh)) {
  hh_id <- c(hh_id, 1:city$hh[i])
}
hh$hh_id <- hh_id

set.seed(2)
hh$persons <- unlist(mapply(rpois, city$hh, city$persons_mean))
hh$persons <- ifelse(hh$persons == 0, 1, hh$persons)
set.seed(2)
hh$dogs <- rpois(nrow(hh), .75)
set.seed(2)
hh$cats <- rpois(nrow(hh), .3)

# % of hh with dogs and cats.
sum(hh$dogs != 0) / nrow(hh)
sum(hh$cats != 0) / nrow(hh)

## Hypothetical opinions -------------------------------------------------------

# Reasons to not sterilize
not_ster <- c("cost", "wants_offspring", "lack_of_time",
              "its_cruelty", "animal_too_young",
              "dont_know", "others")
set.seed(2)
not_ster <- sample(rep(not_ster,
                       round(nrow(hh) * c(.4, .1, .08, .07, .05, .07, .23))))
hh$reasons_to_not_sterilize <- not_ster

hh$reasons_to_not_sterilize_others <- "no"
others_idx <- which(hh$reasons_for_not_sterilize == "others")
set.seed(2)
hh$reasons_for_to_sterilize_others[others_idx] <-
  sample(c("can_die", "can_get_ill", "dont_need"),
         length(others_idx), replace = TRUE)

# Most frequent fates
fates <- c("dissapear", "abandonment", "death_at_home",
           "adopted_out", "dont_know", "others")
set.seed(2)
fates <- sample(rep(fates,
                    round(nrow(hh) * c(.06, .17, .67, .07, .05, .03))))[1:nrow(hh)]
hh$common_fates <- fates
hh$common_fates_others <- ifelse(hh$common_fates == "others",
                                 "sold", "no")
set.seed(2)

# Tolerance
tolerance <- c("punish", "educate_without_punish", "adopted_out",
               "abandom", "dont_know")
set.seed(2)
tolerance <- sample(rep(tolerance,
                        nrow(hh) * c(.09, .7, .08, .12, .01)))[1:nrow(hh)]
hh$tolerance <- tolerance
hh$tolerance_others <- "no"

# Reasons to abandom
set.seed(2)
hh$reasons_to_abandom <- sample(rep(c("no", "aggression", "sickness",
                                      "behaviorual_problems", "moving",
                                      "lack_of_space", "pregnancy", "will_die",
                                      "old_age"),
                                    nrow(hh) * c(.9, .05, rep(.2, 7))))[1:nrow(hh)]


#### Hypothetical demography of animals ----------------------------------------

## Dogs ------------------------------------------------------------------------
dogs <- data.frame(track_id = rep(hh$track_id, hh$dogs),
                   hh_id = rep(hh$hh_id, hh$dogs))

# Names
# names_males <-
#   read_html("http://www.blogdocachorro.com.br/nomes-para-cachorros-machos/")
# names_males <- names_males %>%
#   html_nodes("#post-16403 li") %>%
#   html_text()
# writeLines(names_males, file("names_males.txt"))
# names_females <-
#   read_html("http://www.blogdocachorro.com.br/nomes-para-cachorros-femeas/")
# names_females <- names_females %>%
#   html_nodes("#post-16387 li") %>%
#   html_text()
# writeLines(names_females, file("names_females.txt"))
names_males <- readLines("./dev_tools/datasets/names_males.txt")
names_males[names_males == "Oswaldo"] <- "bono"
names_females <- readLines("./dev_tools/datasets/names_females.txt")
set.seed(2)
dogs$name <- sample(names_females, nrow(dogs), replace = TRUE)
set.seed(2)
male_idx <- sample(nrow(dogs), round((nrow(dogs) + nrow(dogs) * .03) / 2))
set.seed(2)
dogs$name[male_idx] <- sample(names_males, length(male_idx), replace = TRUE)
dogs$name <- tolower(iconv(dogs$name, to='ASCII//TRANSLIT'))

# Sex
dogs$sex <- "female"
dogs$sex[male_idx] <- "male"

# Age
age_strata <- seq(.2, 0, length.out = 16)
set.seed(4)
age_strata_f <- age_strata * c(1, runif(15, .8, .98))
age_strata_f <- sum(dogs$sex == "female") * age_strata_f / sum(age_strata_f)
set.seed(5)
age_strata_m <- age_strata * c(1, runif(15, .8, .98))
age_strata_m <- sum(dogs$sex == "male") * age_strata_m / sum(age_strata_m)
dogs$age <- 0
set.seed(2)
dogs$age[dogs$sex == "female"] <-
  sample(0:15, sum(dogs$sex == "female"), prob = age_strata_f, replace = TRUE)
set.seed(3)
dogs$age[dogs$sex == "male"] <-
  sample(0:15, sum(dogs$sex == "male"), prob = age_strata_m, replace = TRUE)

# Sterilized
dogs$sterilized <- "no"
set.seed(2)
dogs$sterilized[
  sample(which(dogs$sex == "female"), sum(dogs$sex == "female") * .23)] <- "yes"
set.seed(2)
dogs$sterilized[
  sample(which(dogs$sex == "male"), sum(dogs$sex == "male") * .17)] <- "yes"

# Sterilized in the previous 12 months
dogs$sterilized_last_year <- "no"
dsf_idx <- which(dogs$sterilized == "yes" & dogs$sex == "female")
dsm_idx <- which(dogs$sterilized == "yes" & dogs$sex == "male")
set.seed(2)
dogs$sterilized_last_year[sample(dsf_idx, length(dsf_idx) * .28)] <- "yes"
set.seed(2)
dogs$sterilized_last_year[sample(dsm_idx, length(dsm_idx) * .22)] <- "yes"

# Free-roaming
set.seed(2)
dogs$free_roaming <- sample(c("no", "yes"), nrow(dogs),
                            prob = c(.83, .17), replace = TRUE)

# Acquisition
dogs$acquisition <- sample(c("adopted", "bought", "found",
                             "gift", "born_in_home"), nrow(dogs),
                           prob = c(.20, .16, .07, .46, .10), replace = TRUE)

# Acquisition occured in the previous 12 months
set.seed(2)
dogs$acquired_last_year <- sample(c("no", "yes"), nrow(dogs),
                                     prob = c(.9, .1), replace = TRUE)

dogs$age[dogs$acquired_last_year == "yes" &
           dogs$acquisition == "born_in_home" &
           dogs$age > 1] <- 0

# Acquired sterilized
dogs$acquired_sterilized <- "no"
as_idx <- which(dogs$sterilized == "yes" & dogs$acquisition != "born_in_home")
set.seed(2)
dogs$acquired_sterilized[sample(as_idx, length(as_idx) * .6)] <- "yes"
as_idx <- which(dogs$sterilized == "yes" & dogs$acquisition == "adopted")
set.seed(2)
dogs$acquired_sterilized[sample(as_idx, length(as_idx) * .9)] <- "yes"

# City and state of acquisition
# http://wiki.openstreetmap.org/wiki/WikiProject_Brazil/Lista_Completa_de_Cidades
cities <- read.csv("./dev_tools/datasets/municipios.csv", header = FALSE,
                   as.is = TRUE)
cities <- cities[which(!grepl("[[:digit:]]+", cities$V1)), ]
cities$V1 <- tolower(iconv(cities$V1, to='ASCII//TRANSLIT'))
cities$V2 <- tolower(iconv(cities$V2, to='ASCII//TRANSLIT'))
cities$V1 <- gsub(" ", "_", cities$V1)
cities$V2 <- gsub(" ", "_", cities$V2)
cities$prob <- ifelse(cities$V1 == "sp", .04 / 650, .02 / 5000)
cities$prob[cities$V2 == "santos"] <- .9
cities$prob[cities$V2 == "sao_paulo"] <- .04
set.seed(2)
cs_idx <- sample(nrow(cities), nrow(dogs), prob = cities$prob, replace = TRUE)
dogs$acquisition_city <- cities[cs_idx, "V2"]
dogs$acquisition_state <- cities[cs_idx, "V1"]

# Turnover in the previous 12 months
set.seed(2)
dogs$turnover_last_year <- sample(c("no", "yes"), nrow(dogs), replace = TRUE,
                                  prob = c(.98, .02))

# Litter size if delivered in the previous 12 months
dogs$litter_size_last_year <- 0
lit_idx <- dogs$sex == "female" & dogs$age < 6
set.seed(2)
dogs$litter_size_last_year[lit_idx] <-rpois(sum(lit_idx), 4)
set.seed(2)
dogs$litter_size_last_year[lit_idx[sample(sum(lit_idx), sum(lit_idx) * .97)]] <- 0
dogs$litter_size_last_year[sample(nrow(dogs), nrow(dogs) * .92)] <- 0

## Check 
#PlotPopPyramid(dogs, age.col = "age", sex.col = "sex", str.col = "sterilized")
summary(dogs)

# All should be 0
sum(dogs$sex == "male" & dogs$litter_size_last_year > 0)
sum(dogs$acquired_last_year == TRUE & dogs$acquisition == "born_in_home" &
      dogs$age > 1)
sum(dogs$acquired_sterilized == "yes" & dogs$sterilized == "nao")
sum(dogs$acquisition_city == "santos" & dogs$acquisition_state != "sp")

# Frequencies
table(dogs$sex) / nrow(dogs)
table(dogs$sterilized) / nrow(dogs)
table(dogs$free_roaming) / nrow(dogs)
table(dogs$acquisition) / nrow(dogs)
table(dogs$acquired_last_year) / nrow(dogs)
table(dogs$acquired_sterilized[as_idx]) / length(as_idx)
table(dogs$acquired_sterilized[dogs$acquisition == "adopted"]) /
  sum(dogs$acquisition == "adopted")
table(dogs$lost) / nrow(dogs)
hist(dogs$litter_size_last_year[dogs$sex == "female" & dogs$age < 6])

## Cats ------------------------------------------------------------------------
cats <- data.frame(track_id = rep(hh$track_id, hh$cats),
                   hh_id = rep(hh$hh_id, hh$cats))

# Names
set.seed(4)
cats$name <- sample(names_females, nrow(cats), replace = TRUE)
set.seed(4)
male_idx <- sample(nrow(cats), round((nrow(cats) + nrow(cats) * .01) / 2))
set.seed(4)
cats$name[male_idx] <- sample(names_males, length(male_idx), replace = TRUE)
cats$name <- tolower(iconv(cats$name, to='ASCII//TRANSLIT'))

# Sex
cats$sex <- "female"
cats$sex[male_idx] <- "male"

# Age
age_strata <- seq(.2, 0, length.out = 13)
set.seed(4)
age_strata_f <- age_strata * c(1, runif(12, .8, .98))
age_strata_f <- sum(cats$sex == "female") * age_strata_f / sum(age_strata_f)
set.seed(5)
age_strata_m <- age_strata * c(1, runif(12, .8, .98))
age_strata_m <- sum(cats$sex == "male") * age_strata_m / sum(age_strata_m)
cats$age <- 0
set.seed(2)
cats$age[cats$sex == "female"] <-
  sample(0:12, sum(cats$sex == "female"), prob = age_strata_f, replace = TRUE)
set.seed(2)
cats$age[cats$sex == "male"] <-
  sample(0:12, sum(cats$sex == "male"), prob = age_strata_m, replace = TRUE)

# Sterilized
cats$sterilized <- "no"
set.seed(2)
cats$sterilized[
  sample(which(cats$sex == "female"), sum(cats$sex == "female") * .63)] <- "yes"
set.seed(2)
cats$sterilized[
  sample(which(cats$sex == "male"), sum(cats$sex == "male") * .56)] <- "yes"

# Sterilized in the previous 12 months
cats$sterilized_last_year <- "no"
csf_idx <- which(cats$sterilized == "yes" & cats$sex == "female")
csm_idx <- which(cats$sterilized == "yes" & cats$sex == "male")
set.seed(2)
cats$sterilized_last_year[sample(csf_idx, length(csf_idx) * .22)] <- "yes"
set.seed(2)
cats$sterilized_last_year[sample(csm_idx, length(csm_idx) * .18)] <- "yes"


# Free-roaming
set.seed(2)
cats$free_roaming <- sample(c("no", "yes"), nrow(cats),
                            prob = c(.34, .66), replace = TRUE)

# Acquisition
cats$acquisition <- sample(c("adopted", "bought", "found",
                             "gift", "born_in_home"), nrow(cats),
                           prob = c(.36, .01, .12, .29, .23), replace = TRUE)

# Acquisition occured in the previous 12 months
set.seed(2)
cats$acquired_last_year <- sample(c("no", "yes"), nrow(cats),
                                     prob = c(.89, .11), replace = TRUE)

cats$age[cats$acquired_last_year == "yes" &
           cats$acquisition == "born_in_home" &
           cats$age > 1] <- 0

# Acquired sterilized
cats$acquired_sterilized <- "no"
as_idx <- which(cats$sterilized == "yes" & cats$acquisition != "born_in_home")
set.seed(2)
cats$acquired_sterilized[sample(as_idx, length(as_idx) * .6)] <- "yes"
as_idx <- which(cats$sterilized == "yes" & cats$acquisition == "adopted")
set.seed(2)
cats$acquired_sterilized[sample(as_idx, length(as_idx) * .9)] <- "yes"

# City and state of acquisition
cities <- read.csv("./dev_tools/datasets/municipios.csv", header = FALSE,
                   as.is = TRUE)
cities <- cities[which(!grepl("[[:digit:]]+", cities$V1)), ]
cities$V1 <- tolower(iconv(cities$V1, to='ASCII//TRANSLIT'))
cities$V2 <- tolower(iconv(cities$V2, to='ASCII//TRANSLIT'))
cities$V1 <- gsub(" ", "_", cities$V1)
cities$V2 <- gsub(" ", "_", cities$V2)
cities$prob <- ifelse(cities$V1 == "sp", .02 / 650, .01 / 5000)
cities$prob[cities$V2 == "santos"] <- .93
cities$prob[cities$V2 == "sao_paulo"] <- .03
set.seed(2)
cs_idx <- sample(nrow(cities), nrow(cats), prob = cities$prob, replace = TRUE)
cats$acquisition_city <- cities[cs_idx, "V2"]
cats$acquisition_state <- cities[cs_idx, "V1"]

# Turnover in the previous 12 months
set.seed(2)
cats$turnover_last_year <- sample(c("no", "yes"), nrow(cats), replace = TRUE,
                                  prob = c(.98, .02))

# Litter size if delivered in the previous 12 months
cats$litter_size_last_year <- 0
lit_idx <- cats$sex == "female" & cats$age < 5
set.seed(2)
cats$litter_size_last_year[lit_idx] <-rpois(sum(lit_idx), 4)
set.seed(2)
cats$litter_size_last_year[lit_idx[sample(sum(lit_idx), sum(lit_idx) * .97)]] <- 0
cats$litter_size_last_year[sample(nrow(cats), nrow(cats) * .92)] <- 0

## Check 
#PlotPopPyramid(cats, age.col = "age", sex.col = "sex", str.col = "sterilized")
summary(cats)

# All should be 0
sum(cats$sex == "male" & cats$litter_size_last_year > 0)
sum(cats$acquired_last_year == TRUE & cats$acquisition == "born_in_home" &
      cats$age > 1)
sum(cats$acquired_sterilized == "yes" & cats$sterilized == "nao")
sum(cats$acquisition_city == "santos" & cats$acquisition_state != "sp")

# Frequencies
table(cats$sex) / nrow(cats)
table(cats$sterilized) / nrow(cats)
table(cats$free_roaming) / nrow(cats)
table(cats$acquisition) / nrow(cats)
table(cats$acquired_last_year) / nrow(cats)
table(cats$acquired_sterilized[as_idx]) / length(as_idx)
table(cats$acquired_sterilized[cats$acquisition == "adopted"]) /
  sum(cats$acquisition == "adopted")
table(cats$lost) / nrow(cats)
hist(cats$litter_size_last_year[cats$sex == "female" & cats$age < 5])


#### Survey data ---------------------------------------------------------------

## Two-stage cluster sampling --------------------------------------------------

# Sample
set.seed(2)
psu <- SamplePPS(city[, c("track_id", "hh")], 65)
ssu <- SampleSystematic(psu, 18)
cluster_sample0 <- matrix(ncol = ncol(hh))
colnames(cluster_sample0) <- names(hh)
for (i in seq_along(colnames(ssu))) {
  track <- hh[hh$track_id == colnames(ssu)[i], ]
  track <- track[ssu[, i], ]
  cluster_sample0 <- rbind(cluster_sample0, track)
}
cluster_sample0 <- cluster_sample0[-1, ]

# ID
# If any(w) is TRUE, repeat the sample until FALSE. This is a convenience
# equivalent to a sample in which hh are not selected more than once.
w <- c()
for (i in seq_along(colnames(ssu)[duplicated(colnames(ssu))])) {
  w0 <- ssu[1, colnames(ssu) == colnames(ssu)[duplicated(colnames(ssu))][i]]
  w[i] <- w0[1] == w0[2]
}
any(w)
cluster_sample <- cluster_sample0[, c("track_id", "hh_id")]

# Interviewer
names_ <- read.csv("./dev_tools/datasets/hombres.csv", as.is = TRUE)[, 1]
names_ <- c(names_, read.csv("./dev_tools/datasets/mujeres.csv", as.is = TRUE)[, 1])
names_ <- tolower(iconv(names_, to='ASCII//TRANSLIT'))
names_ <- unlist(strsplit(names_, " "))
set.seed(2)
cluster_sample$interviewer <-
  rep(rep(sample(names_, 10), each = 18), 10)[1:nrow(cluster_sample)]

# Date
cluster_sample$date <- rep(seq(as.Date('2011/03/01'),
                               as.Date('2011/05/05'), by="day"),
                           each = 30)[1:nrow(cluster_sample)]

# Address
cluster_sample$address <- paste0("address", 1:nrow(cluster_sample))

# Track ID
cluster_sample$track_id <- cluster_sample0$track_id

# Interview
set.seed(2)
cluster_sample$interview <- sample(c("answered", "refused", "closed"),
                                   nrow(cluster_sample),
                                   replace = TRUE,
                                   c(.9, .05, .07))

# Interviewee
set.seed(2)
cluster_sample$interviewee <- sample(names_, nrow(cluster_sample))

# Persons, dogs and cats
cluster_sample[, c("persons", "dogs", "cats")] <-
  cluster_sample0[, c("persons", "dogs", "cats")]

# Phone
cluster_sample$phone <- paste0("phone", 1:nrow(cluster_sample))

# Email
cluster_sample$email <- paste0("email", 1:nrow(cluster_sample))

head(cluster_sample)
head(as.data.frame(append(list(id = 1:nrow(cluster_sample)), cluster_sample)))

# Opinions
cluster_sample <- cbind.data.frame(cluster_sample, cluster_sample0[, -c(1:5)])

# NA's on hh not interviewed
hh_lost <- cluster_sample$interview != "answered"
cluster_sample[hh_lost, -c(1:6)] <- NA

# Dogs
cluster_sample_dogs <- merge(cluster_sample[!hh_lost, c("track_id", "hh_id")],
                             dogs, by = c("track_id", "hh_id"))
set.seed(2)
lost_dogs_idx <- sample(nrow(cluster_sample_dogs),
                        nrow(cluster_sample_dogs) * .1)
lost_dogs_idx[1:sum(cluster_sample_dogs$turnover_last_year == "yes")] <- 
  which(cluster_sample_dogs$turnover_last_year == "yes")
cluster_sample_dogs_lost <- cluster_sample_dogs[lost_dogs_idx,
                                                c("track_id", "hh_id", "name",
                                                  "sex", "age", "sterilized")]

set.seed(3)
cluster_sample_dogs_lost[, c("name", "sex", "age", "sterilized")] <- 
  dogs[sample(nrow(dogs), nrow(cluster_sample_dogs_lost)),
       c("name", "sex", "age", "sterilized")]
set.seed(8)
cluster_sample_dogs_lost$sex <-
  sample(c("female", "male"), nrow(cluster_sample_dogs_lost),
         replace = TRUE, prob = c(.45, .55))

set.seed(2)
cluster_sample_dogs_lost$fate <- sample(c("lost", "died", "adopted_out",
                                          "sold", "abandoned"),
                                        nrow(cluster_sample_dogs_lost),
                                        replace = TRUE,
                                        prob = c(.17, .7, .06, .04, .03))

# Must be TRUE
sum(cluster_sample$dogs, na.rm = TRUE) == nrow(cluster_sample_dogs)

# Cats
cluster_sample_cats <- merge(cluster_sample[!hh_lost, c("track_id", "hh_id")],
                             cats, by = c("track_id", "hh_id"))
set.seed(2)
lost_cats_idx <- sample(nrow(cluster_sample_cats),
                        nrow(cluster_sample_cats) * .12)
lost_cats_idx[1:sum(cluster_sample_cats$turnover_last_year == "yes")] <- 
  which(cluster_sample_cats$turnover_last_year == "yes")
cluster_sample_cats_lost <- cluster_sample_cats[lost_cats_idx,
                                                c("track_id", "hh_id", "name",
                                                  "sex", "age", "sterilized")]

set.seed(3)
cluster_sample_cats_lost[, c("name", "sex", "age", "sterilized")] <- 
  cats[sample(nrow(cats), nrow(cluster_sample_cats_lost)),
       c("name", "sex", "age", "sterilized")]
set.seed(2)
cluster_sample_cats_lost$sex <-
  sample(c("female", "male"), nrow(cluster_sample_cats_lost),
         replace = TRUE, prob = c(.49, .51))

set.seed(2)
cluster_sample_cats_lost$fate <- sample(c("lost", "died", "adopted_out",
                                          "sold", "abandoned"),
                                        nrow(cluster_sample_cats_lost),
                                        replace = TRUE,
                                        prob = c(.2, .75, .01, .01, .03))

# Must be TRUE
sum(cluster_sample$cats, na.rm = TRUE) == nrow(cluster_sample_cats)

## Join dogs and cats data
cluster_sample_animals <- rbind.data.frame(cluster_sample_dogs,
                                           cluster_sample_cats)
cluster_sample_animals$species <- rep(c("dog", "cat"),
                                      c(nrow(cluster_sample_dogs),
                                        nrow(cluster_sample_cats)))
cluster_sample_animals <- cluster_sample_animals[, c(1:3, 16, 4:15)]
cluster_sample_animals <-
  cluster_sample_animals[order(cluster_sample_animals$track_id,
                               cluster_sample_animals$hh_id), ]

cluster_sample_animals_lost <- rbind.data.frame(cluster_sample_dogs_lost,
                                                cluster_sample_cats_lost)
cluster_sample_animals_lost$species <- rep(c("dog", "cat"),
                                           c(nrow(cluster_sample_dogs_lost),
                                             nrow(cluster_sample_cats_lost)))
cluster_sample_animals_lost <- cluster_sample_animals_lost[, c(1:3, 8, 4:7)]
cluster_sample_animals_lost <-
  cluster_sample_animals_lost[order(cluster_sample_animals_lost$track_id,
                                    cluster_sample_animals_lost$hh_id), ]

## Systematic sampling ---------------------------------------------------------
set.seed(2)
sys_su_pilot <- SampleSystematic(su = 50, N = sum(city$hh))
sys_pilot <- hh[sys_su_pilot, "dogs"]
sys_sample_size <- CalculateSimpleSampleSize(sys_pilot, sum(city$hh), error = .07)
set.seed(2)
sys_su <- SampleSystematic(su = sys_sample_size, N = sum(city$hh))
sys_sample0 <- hh[sys_su, ]

# ID
sys_sample <- sys_sample0[, c("track_id", "hh_id")]

# Interviewer
set.seed(2)
sys_sample$interviewer <-
  rep(rep(sample(names_, 10), each = 18), 10)[1:nrow(sys_sample)]

# Date
sys_sample$date <- rep(seq(as.Date('2011/03/01'),
                           as.Date('2011/05/05'), by="day"),
                       each = 30)[1:nrow(sys_sample)]

# Address
sys_sample$address <- paste0("address", 1:nrow(sys_sample))

# Track ID
sys_sample$track_id <- sys_sample0$track_id

# Interview
set.seed(2)
sys_sample$interview <- sample(c("answered", "refused", "closed"),
                               nrow(sys_sample),
                               replace = TRUE,
                               c(.9, .05, .07))

# Interviewee
set.seed(2)
sys_sample$interviewee <- sample(names_, nrow(sys_sample))

# Persons, dogs and cats
sys_sample[, c("persons", "dogs", "cats")] <-
  sys_sample0[, c("persons", "dogs", "cats")]

# Phone
sys_sample$phone <- paste0("phone", 1:nrow(sys_sample))

# Email
sys_sample$email <- paste0("email", 1:nrow(sys_sample))

head(sys_sample)
head(as.data.frame(append(list(id = 1:nrow(sys_sample)), sys_sample)))

# Opinions
sys_sample <- cbind.data.frame(sys_sample, sys_sample0[, -c(1:5)])

# NA's on hh not interviewed
sys_hh_lost <- sys_sample$interview != "answered"
sys_sample[sys_hh_lost, -c(1:6)] <- NA

# Dogs
sys_sample_dogs <- merge(sys_sample[!sys_hh_lost, c("track_id", "hh_id")],
                         dogs, by = c("track_id", "hh_id"))
set.seed(2)
lost_dogs_idx <- sample(nrow(sys_sample_dogs),
                        nrow(sys_sample_dogs) * .1)
lost_dogs_idx[1:sum(sys_sample_dogs$turnover_last_year == "yes")] <- 
  which(sys_sample_dogs$turnover_last_year == "yes")
sys_sample_dogs_lost <- sys_sample_dogs[lost_dogs_idx,
                                        c("track_id", "hh_id", "name",
                                          "sex", "age", "sterilized")]

set.seed(3)
sys_sample_dogs_lost[, c("name", "sex", "age", "sterilized")] <- 
  dogs[sample(nrow(dogs), nrow(sys_sample_dogs_lost)),
       c("name", "sex", "age", "sterilized")]

set.seed(2)
sys_sample_dogs_lost$fate <- sample(c("lost", "died", "adopted_out",
                                      "sold", "abandoned"),
                                    nrow(sys_sample_dogs_lost),
                                    replace = TRUE,
                                    prob = c(.17, .7, .06, .04, .03))

# Must be TRUE
sum(sys_sample$dogs, na.rm = TRUE) == nrow(sys_sample_dogs)

# Cats
sys_sample_cats <- merge(sys_sample[!sys_hh_lost, c("track_id", "hh_id")],
                         cats, by = c("track_id", "hh_id"))
set.seed(2)
lost_cats_idx <- sample(nrow(sys_sample_cats),
                        nrow(sys_sample_cats) * .12)
lost_cats_idx[1:sum(sys_sample_cats$turnover_last_year == "yes")] <- 
  which(sys_sample_cats$turnover_last_year == "yes")
sys_sample_cats_lost <- sys_sample_cats[lost_cats_idx,
                                        c("track_id", "hh_id", "name",
                                          "sex", "age", "sterilized")]

set.seed(3)
sys_sample_cats_lost[, c("name", "sex", "age", "sterilized")] <- 
  cats[sample(nrow(cats), nrow(sys_sample_cats_lost)),
       c("name", "sex", "age", "sterilized")]

set.seed(2)
sys_sample_cats_lost$fate <- sample(c("lost", "died", "adopted_out",
                                      "sold", "abandoned"),
                                    nrow(sys_sample_cats_lost),
                                    replace = TRUE,
                                    prob = c(.2, .75, .01, .01, .03))

# Must be TRUE
sum(sys_sample$cats, na.rm = TRUE) == nrow(sys_sample_cats)

## Join dogs and cats data
sys_sample_animals <- rbind.data.frame(sys_sample_dogs,
                                       sys_sample_cats)
sys_sample_animals$species <- rep(c("dog", "cat"),
                                  c(nrow(sys_sample_dogs),
                                    nrow(sys_sample_cats)))
sys_sample_animals <- sys_sample_animals[, c(1:3, 16, 4:15)]
sys_sample_animals <-
  sys_sample_animals[order(sys_sample_animals$track_id,
                           sys_sample_animals$hh_id), -1]

sys_sample_animals_lost <- rbind.data.frame(sys_sample_dogs_lost,
                                            sys_sample_cats_lost)
sys_sample_animals_lost$species <- rep(c("dog", "cat"),
                                       c(nrow(sys_sample_dogs_lost),
                                         nrow(sys_sample_cats_lost)))
sys_sample_animals_lost <- sys_sample_animals_lost[, c(1:3, 8, 4:7)]
sys_sample_animals_lost <-
  sys_sample_animals_lost[order(sys_sample_animals_lost$track_id,
                                sys_sample_animals_lost$hh_id), -1]
sys_sample <- sys_sample[, -1]


## Stratified sampling ---------------------------------------------------------
# It does not make sense as only 3 (0.4%) tracks are rural and the remaining
# are urban.
sum(city$track_status < 4)
sum(city$track_status >= 4)

### Save files -----------------------------------------------------------------
# save(hh, file = "./data/hh.rda")
# save(dogs, file = "./data/dogs.rda")
# save(cats, file = "./data/cats.rda")
# save(city, file = "./data/city.rda")
# cluster_pilot <- cluster_sample[(18*5+1):(18*10), c("track_id", "dogs")]
# cluster_pilot <- cluster_pilot[complete.cases(cluster_pilot), ]
# save(cluster_pilot, file = "./data/cluster_pilot.rda")
# save(cluster_sample, file = "./data/cluster_sample.rda")
# save(sys_sample, file = "./data/sys_sample.rda")
# save(cluster_sample_animals, file = "./data/cluster_sample_animals.rda")
# save(cluster_sample_animals_lost,
#     file = "./data/cluster_sample_animals_lost.rda")
# save(sys_sample_animals, file = "./data/sys_sample_animals.rda")
# save(sys_sample_animals_lost,
#     file = "./data/sys_sample_animals_lost.rda")