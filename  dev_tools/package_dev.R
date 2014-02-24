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

#####################
## release('capm') ##
#####################