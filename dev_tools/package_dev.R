library(devtools)

# Development workflow
## Datasets
# Run pinhais_data.R and then the line below.
# use_data(cluster_sample, psu_ssu, dogs, cats, overwrite = TRUE)

install()
load_all()
document()

# News preview
show_news()

# Checking
check()
check_man()
run_examples()
#build_win()

# Update version in:
#   DESCRIPTION
#   capm-package
#   NEWS
#   CITATION
#   Home web page