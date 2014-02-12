library(devtools)

# Development workflow
setwd('~/Documents/Projects')
load_all('capm')
document('capm')
install('capm')
setwd('~/Documents/Projects/capm')

# News preview
show_news('capm')

# Checking
check()
check_doc()
run_examples()
build_win()


###############
## Relsease  ##
## release() ##
##############