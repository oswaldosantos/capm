library(devtools)

# Development workflow
# use_data(cats, city, cluster_sample, cluster_sample_animals,
#         cluster_sample_animals_lost, dogs, hh, sys_sample,
#         sys_sample_animals, sys_sample_animals_lost,
#         overwrite = T)
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