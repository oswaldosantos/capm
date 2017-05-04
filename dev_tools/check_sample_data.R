cat("\014") 
rm(list = ls())
data("city")
data("cluster_sample")
data("cluster_sample_animals")
data("cluster_sample_animals_lost")

#### Household data ------------------------------------------------------------
names(cluster_sample)

## All sample track's ID's must be in the set of population track's ID's.
all(cluster_sample$track_id %in% city$track_id)

## All hh's ID's must be equal or less than the number oh hh per track.
hh_id_validity <- merge(cluster_sample[, c("track_id", "hh_id")],
      city[, c("track_id", "hh")], by = "track_id")
all(hh_id_validity$hh_id <= hh_id_validity$hh)

## Must include only the interviewers' names.
unique(cluster_sample$interviewer)

## Interview status.
table(cluster_sample$interview) / nrow(cluster_sample)
barplot(table(cluster_sample$interview))

## hh which didn't answered thw interview mustn't have answered questions.
all(is.na(cluster_sample[cluster_sample$interview != "answered", 7:19]))

## Subset of answered interviews for further analysis.
cluster_sample2 <- cluster_sample[complete.cases(cluster_sample), ]

## For persons, the minimum should be 1.
summary(cluster_sample2[, c("date", "persons", "dogs", "cats")])

## Reasons to not sterilize.
table(cluster_sample2$reasons_to_not_sterilize) / nrow(cluster_sample2)
barplot(table(cluster_sample2$reasons_to_not_sterilize))

table(cluster_sample2$reasons_to_not_sterilize_others) / nrow(cluster_sample2)

# If interviewees gave reasons other than those offered, the interviewer must
# have marked "others" in "reasons_to_not_sterilize" and specified the reason
# in "reasons_to_not_sterilize_others".
# The folowing must be TRUE.
all(which(!is.na(cluster_sample[cluster_sample$reasons_to_not_sterilize == "others",
               "reasons_to_not_sterilize_others"] == "no")))

## Common fates.
# Same rationale as in Reason to not sterilize.
table(cluster_sample2$common_fates) / nrow(cluster_sample2)
barplot(table(cluster_sample2$common_fates))

table(cluster_sample2$common_fates_others) / nrow(cluster_sample2)
barplot(table(cluster_sample2$common_fates_others))

all(which(!is.na(cluster_sample[cluster_sample$common_fates == "others",
                                "common_fates_others"] == "no")))

## Reasons to abandom.
table(cluster_sample2$reasons_to_abandom) / nrow(cluster_sample2)
barplot(table(cluster_sample2$reasons_to_abandom))

#### Animal data ---------------------------------------------------------------
names(cluster_sample_animals)

## Check the NA's
sapply(cluster_sample_animals, function(x) sum(is.na(x)))

## Each hh must be repaeated in "cluster_sample_animals" as many times as
## there are dogs and cats in it, acoording to "cluster_sample2".
presence_validity <- merge(cluster_sample_animals[, c("track_id", "hh_id")],
                           cluster_sample2[, c("track_id", "hh_id",
                                               "dogs", "cats")],
                           by = c("track_id", "hh_id"))
presence_validity <- presence_validity %>%
  group_by(track_id, hh_id) %>%
  summarise(n = n(), dogs = unique(dogs), cats = unique(cats)) %>%
  mutate(valid = n == dogs + cats)

# The following must be TRUE
all(presence_validity$valid)

## Species
table(cluster_sample_animals$species) / nrow(cluster_sample_animals)

## Dogs ------------------------------------------------------------------------
sample_dogs <- filter(cluster_sample_animals, species == "dog")

## Sex
table(sample_dogs$sex) / nrow(sample_dogs)

## Age
summary(sample_dogs$age)

## Sterilized
table(sample_dogs$sterilized) / nrow(sample_dogs)
sample_dogs %>%
  group_by(sex, sterilized) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / rep(c(sum(sample_dogs$sex == "female"),
                          sum(sample_dogs$sex == "male")),
                        each = 2))

PlotPopPyramid(sample_dogs, age.col = "age", sex.col = "sex", str.col = "sterilized")

## Sterilized in the previous 12 months (sterilization rate)

# Must be 0
sum(sample_dogs$sterilized == "no" & sample_dogs$sterilized_last_year == "yes")

table(sample_dogs$sterilized_last_year) / nrow(sample_dogs)

sample_dogs %>%
  group_by(sex, sterilized_last_year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / rep(c(sum(sample_dogs$sex == "female"),
                          sum(sample_dogs$sex == "male")),
                        each = 2))

## Free roaming
table(sample_dogs$free_roaming) / nrow(sample_dogs)

## Acquisition
table(sample_dogs$acquisition) / nrow(sample_dogs)
barplot(table(sample_dogs$acquisition))

## Acquired in the previous 12 months
table(sample_dogs$acquired_last_year) / nrow(sample_dogs)

# Adoption rate
sum(sample_dogs$acquired_last_year == "yes" &
      sample_dogs$acquisition == "adopted") / nrow(sample_dogs)

# Must be 0
sum(sample_dogs$age > 1 & sample_dogs$acquisition == "born_in_home" &
      sample_dogs$acquired_last_year == "yes")

## Acuired sterilized
table(sample_dogs$acquired_sterilized) / nrow(sample_dogs)

# Must be 0
sum(sample_dogs$sterilized == "no" & sample_dogs$acquired_sterilized == "yes")

## Imigration

# Proportion of immigrants
sum(sample_dogs$acquisition_city != "santos") / nrow(sample_dogs)

# Immigration rate (considering commerce as a source o immigration)
sum((sample_dogs$acquisition_city != "santos" |
      sample_dogs$acquisition == "bought") &
      sample_dogs$acquired_last_year == "yes") / nrow(sample_dogs)

# Immigration rate (only spatial)
sum(sample_dogs$acquisition_city != "santos" &
      sample_dogs$acquired_last_year == "yes") / nrow(sample_dogs)


## Aimals acquired in the 12 months following the lost of another animal
## (turnover rate)
table(sample_dogs$turnover_last_year) / nrow(sample_dogs)

## Litter size
barplot(table(sample_dogs[sample_dogs$litter_size_last_year > 0,
                          "litter_size_last_year"]))

# Propportion of females that delivered in the previous 12 months
table(sample_dogs$litter_size_last_year > 0) / sum(sample_dogs$sex == "female")

# Must be 0
sum(sample_dogs$litter_size_last_year > 0 & sample_dogs$sex == "male")

## Lost animals ----------------------------------------------------------------
names(cluster_sample_animals_lost)

## Check the NA's
sapply(cluster_sample_animals_lost, function(x) sum(is.na(x)))

## ID's validity
id_validity <- merge(cluster_sample_animals_lost[, c("track_id", "hh_id")],
                           cluster_sample2[, c("track_id", "hh_id")],
                           by = c("track_id", "hh_id"), all.x = TRUE)
# Mustn't has NA's
summary(id_validity)

## Species
table(cluster_sample_animals_lost$species) / nrow(cluster_sample_animals_lost)

## Dogs
sample_dogs2 <- filter(cluster_sample_animals_lost, species == "dog")

## Sex
table(sample_dogs2$sex) / nrow(sample_dogs2)

## Age
summary(sample_dogs2$age)

## Sterilized
table(sample_dogs2$sterilized) / nrow(sample_dogs2)
sample_dogs2 %>%
  group_by(sex, sterilized) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / rep(c(sum(sample_dogs2$sex == "female"),
                          sum(sample_dogs2$sex == "male")),
                        each = 2))

PlotPopPyramid(sample_dogs2, age.col = "age", sex.col = "sex", str.col = "sterilized")

## Fate
table(sample_dogs2$fate) / nrow(sample_dogs2)

# Abandonment rate
sum(sample_dogs2$fate == "abandoned" | sample_dogs2$fate == "lost") /
  nrow(sample_dogs)

# Death rate per sex
sum(sample_dogs2$fate == "died" & sample_dogs2$sex == "female") /
  sum(sample_dogs$sex == "female")
sum(sample_dogs2$fate == "died" & sample_dogs2$sex == "male") /
  sum(sample_dogs$sex == "male")
