library(dplyr)
cat("\014") 
rm(list = ls())
data("hh")
data("city")
data("cluster_sample")
data("cluster_sample_animals")
data("cluster_sample_animals_lost")

## Owned dogs ------------------------------------------------------------------

sam <- cluster_sample[complete.cases(cluster_sample), c(1, 2, 8:10)]
design <- DesignSurvey(sample = sam,
                       psu.ssu = city[, c("track_id", "hh")],
                       psu.col = 1, ssu.col = 2, psu.2cd = 65,
                       cal.col = 3, cal.N = sum(hh$persons))
(est <- SummarySurvey(design, rep("total", 3)))
csd <- cluster_sample_animals[complete.cases(cluster_sample_animals), ]
csd <- csd[csd$species == "dog", ]
csdl <- cluster_sample_animals_lost[complete.cases(cluster_sample_animals_lost), ]
csdl <- csdl[csdl$species == "dog", ]

Nhat <- est["Total.dogs", "Estimate"]

# Totals by sex (f1 and m1)
tot_sex <- csd %>%
  group_by(sex) %>%
  summarise(Xhat = round(Nhat * n() / nrow(csd)))
f1 <- tot_sex[1, 2][[1]]
m1 <- tot_sex[2, 2][[1]]

# Totals of sterilized by sex (fs1 and ms1)
tot_sex_ster <- csd %>%
  group_by(sex, sterilized) %>%
  summarise(Xhat = round(n())) %>%
  ungroup() %>%
  mutate(Xhat = Xhat / rep(c(sum(Xhat[1:2]), sum(Xhat[3:4])), each = 2))
fs1 <- tot_sex_ster[2, 3][[1]] * f1
ms1 <- tot_sex_ster[4, 3][[1]] * m1

# Borns
b1 <- round(f1 * sum(csd$litter_size) / sum(csd$sex == "female"))

# Deadth reates (df1 and dm1)
df1 <- sum(csdl$fate == "died" & csdl$sex == "female") / sum(csd$sex == "female")
dm1 <- sum(csdl$fate == "died" & csdl$sex == "male") / sum(csd$sex == "male")

# Sterilization rates (sf1 and sm1)
ster <- csd %>%
  group_by(sex, sterilized_last_year) %>%
  summarise(Xhat = n() / nrow(csd))
sf1 <- ster[2, 3][[1]]
sm1 <- ster[4, 3][[1]]

# Carrying capacity (k1)
k1 <- round(est["Total.dogs", "Estimate"] * 1.1)

# (h1)
h1 <- 1

# Abandonment rate (a)
a <- sum(csdl$fate == "abandoned" | csdl$fate == "lost") / nrow(csd)

# Adoption rate (alpha)
alpha <- sum(csd$acquired_last_year == "yes" &
               (csd$acquisition == "adopted" |
                  csd$acquisition == "found")) / nrow(csd)

# Immigration rate (v)
v <- sum(csd$acquired_last_year == "yes" &
           csd$acquisition_state != "santos") / nrow(csd)

# Proportion of sterilized immigrants (z)
z <- as.numeric(table(csd$acquired_sterilized) / nrow(csd))[2]

## Stray dogs ------------------------------------------------------------------

N2 <- Nhat * 0.05
f2 <- round((N2 / 2))
fs2 <- round((N2 / 2) * 0.1)
m2 <- round((N2 / 2))
ms2 <- round((N2 / 2) * 0.05)

b2 <- round(f2 * b1 / f1 * 1.2)
df2 <- df1 * 1.2
dm2 <- dm1 * 1.2
sf2 <- sf1 * .3
sm2 <- sm1 * .3
k2 <- round(N2 * 1.1)
h2 <- 0.5

#### Model ---------------------------------------------------------------------

# Initial conditions and parameters.
pars <- c(b1 = b1, b2 = b2, df1 = df1, dm1 = dm1,
          df2 = df2, dm2 = dm2, sf1 = sf1, sf2 = sf2,
          sm1 = sm1, sm2 = sm2, k1 = k1, k2 = k2, h1 = h1,
          h2 = h2,  a = a, alpha = alpha, v = v, z = z)

init <- c(f1 = f1, fs1 = fs1, m1 = m1, ms1 = ms1,
          f2 = f2, fs2 = fs2, m2 = m2, ms2 = ms2)

## Point estimates
iasa <- SolveIASA(pars = pars, init = init, time = 0:30, method = 'rk4')

## Scenarios
iasa_scenarios <- SolveIASA(pars = pars,
                            init = init,
                            time = 0:30,
                            s.range = seq(0, .1, l = 10),
                            a.range = c(0, .1),
                            alpha.range = c(0, .1),
                            v.range = c(0, .6),
                            method = 'rk4')

PlotModels(iasa, variable = 'N1', x.label = 'Years',
           y.label = 'Total of owned dogs')
PlotModels(iasa_scenarios, 'N', x.label = 'Years')
