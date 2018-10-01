library(tidyverse)

banco1 <- read_csv("~/Documents/projectos/wap-dog-and-cat-demography-pinhais/manuscript/Appendix4/banco1-checked.csv")
names(banco1)
banco1 <- banco1[, 1:14]
names(banco1) <- c("interview_id", "census_tract_id", "interviewer", "date",
                   "address", "interview", "interviewee", "number_of_dogs",
                   "number_of_cats", "number_of_persons", "cell_phone",
                   "e_mail", "reason_for_not_sterilize",
                   "reason_for_not_sterilize_others")

length(unique(banco1$interviewer))
set.seed(24)
banco1$interviewer <- paste0("fake_", sample(10, 1230, replace = TRUE))

length(unique(banco1$address))
set.seed(24)
banco1$address <- paste0("fake_", 1:1230)

table(banco1$interview)
banco1$interview[banco1$interview == "atendida"] <- "attended"
banco1$interview[banco1$interview == "casa_fechada"] <- "closed_hh"
banco1$interview[banco1$interview == "recusa"] <- "refused"

length(unique(banco1$interviewee))
set.seed(24)
banco1$interviewee <- paste0("fake_", sample(500, 1230, replace = TRUE))

banco1$cell_phone <- ifelse(is.na(banco1$cell_phone),
                            NA,
                            paste0("fake_", 1:sum(!is.na(banco1$cell_phone))))

banco1$e_mail <- ifelse(is.na(banco1$e_mail),
                        NA,
                        paste0("fake_", 1:sum(!is.na(banco1$e_mail))))

table(banco1$reason_for_not_sterilize)
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "custo_da_castracao"] <- "cost"
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "eh_perigoso_para_o_animal"] <-
  "dangerous_for_the_animal"
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "falta_de_tempo"] <- "lack_of_time"
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "nao_sabe"] <- "do_not_know"
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "outras"] <- "others"
banco1$reason_for_not_sterilize[
  banco1$reason_for_not_sterilize == "quer_crias_do_animal"] <- "wants_offspring"

banco1$reason_for_not_sterilize_others[
  !is.na(banco1$reason_for_not_sterilize_others)] <-
  c("recently_adopted", "do_not_want", "will_do_it", "is_male",
    "vet_recommendation", "will_do_it", "will_do_it", "will_do_it",
    "use_contraceptives", "do_not_want", "do_not_want", "too_young",
    "too_young", "do_not_want", "do_not_want", "do_not_need",
    "makes_the_animal_lazy", "use_contraceptives", "age", "age", "age",
    "recently_adopted", "do_not_want", "age", "age", "age", "is_male",
    "do_not_want", "sterilized_bitches_died_of_cancer",
    "restrained_in_the_house", "restrained", "use_contraceptives",
    "use_contraceptives", "lack_of_information", "lack_of_information",
    "use_contraceptives", "lack_of_information", "do_not_want", "do_not_want",
    "do_not_want", "recently_adopted", "will_do_it",
    "do_not_interact_with_other_dogs", "will_do_it", "Its_sin",
    "too_aggressive", "too_young", "restrained", "feel_sorry", "age",
    "restrained", "do_not_need", "owner_is_sick", "will_do_it",
    "will_do_it", "makes_the_animal_lazy", "too_older")

cluster_sample <- banco1
attr(cluster_sample, "spec") <- NULL
#save(banco1, file = "./data/cs_hh_pinhais2017.rda")

psu_ssu <- read_csv("~/Documents/projectos/wap-dog-and-cat-demography-pinhais/manuscript/Appendix4/psu_ssu.csv")
names(psu_ssu) <- c("census_tract_id", "hh")
attr(psu_ssu, "spec") <- NULL
#save(psu_ssu, file = "./data/psu_ssu_pinhais2017.rda")

banco2 <- read_csv("~/Documents/projectos/wap-dog-and-cat-demography-pinhais/manuscript/Appendix4/banco2-checked.csv")
names(banco2) <-
  c("interview_id", "census_tract_id", "name", "species", "sex", "age", "sterilized",
    "sterilized_ly", "go_out_on_the_street_alone", "acquisition",
    "acquired_ly", "acquired_sterilized", "acquisition_city",
    "acquisition_state", "lost_animals", "births_ly", "name3", "species3",
    "sex3", "age3", "sterilized3", "fate")

banco2[banco2 == "sim"] <- "yes"
banco2[banco2 == "nao"] <- "no"
banco2[banco2 == "macho"] <- "male"
banco2[banco2 == "femea"] <- "female"
banco2[banco2 == "cao"] <- "dog"
banco2[banco2 == "gato"] <- "cat"

table(banco2$acquisition)
banco2$acquisition[banco2$acquisition == "adotou"] <- "adopted"
banco2$acquisition[banco2$acquisition == "comprou"] <- "bought"
banco2$acquisition[banco2$acquisition == "ganhou"] <- "gift"

table(banco2$fate)
banco2$fate[banco2$fate == "doado"] <- "adopted_out"
banco2$fate[banco2$fate == "morreu"] <- "died"
banco2$fate[banco2$fate == "perdido"] <- "lost"
banco2$fate[banco2$fate == "vendido"] <- "sold"

banco2$acquisition_city <- str_to_title(banco2$acquisition_city)
banco2$acquisition_state <- str_to_title(banco2$acquisition_state)


dogs <- filter(banco2, species == "dog" | species3 == "dog")
attr(dogs, "spec") <- NULL
#save(banco1, file = "./data/cs_dogs_pinhais2017.rda")

cats <- filter(banco2, species == "cat" | species3 == "cat")
attr(cats, "spec") <- NULL
#save(banco1, file = "./data/cs_cats_pinhais2017.rda")