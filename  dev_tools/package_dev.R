library(devtools)

# Development workflow
setwd('~/Documents/Projectos')
load_all('capm')
document('capm')
install('capm')
setwd('~/Documents/Projectos/capm')

# News preview
show_news('capm')

# Checking
check()
check_doc()
run_examples()
build_win()

# Update version in:
#   DESCRIPTION
#   capm-package
#   README.md
#   NEWS
#   CITATION
#   Home web page
#   Web documentation in all languages

#####################
## release('capm') ##
#####################