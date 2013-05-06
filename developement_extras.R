library(devtools)
library(bibtex)
library(tools)

# References for function documentation.
read.bib(file = "capm.bib")


# See *.Rd files of other packages.
# Example to see the ppsq.Rd file from the pps package.
db <- Rd_db("pps")
db[grep("/pps1.Rd", names(db))]

# Developement mode.
dev_mode()
install('capm')
library(capm)
dev_mode()