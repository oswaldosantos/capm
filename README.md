capm
====
  
  `capm` is a package for [R](http://www.r-project.org/), a free software environment for statistical computing and graphics. 


`capm` stands for Companion Animal Population Management and is an initiative to guide and automate quantitative analysis to support companion animal population management.

Current version offers functions that allow users to implement the following workflow:
  
  1. Selection of sampling units for simple and complex surveys (pilot/final).
  2. Mapping primary sampling units to be visited (complex surveys).
  3. Calculation of sample size and composition (complex designs).
  4. Repetition of steps 1 and 2, for the final survey, if the workflow begun with a pilot survey.
  5. Estimation of demographic characteristics (totals, proportions, means and ratios).
  6. Construction of population pyramids, conditioned on sex, age and another categorical variable such as reproductive status.
  7. Assessment of the effect produced by population management interventions, through mathematical modelling.
  8. Prioritization interventions according to the effect they produce, through sensitivity analysis.  

The current workflow is mainly focused on dog populations but also applies to cat populations. Future versions probably will include functions to address particularities of cat populations.

To install the development version, run `install_github('capm', 'oswaldosantos')` in (ideally) RStudio. `devtools` package must be previously loaded.

To install the production version, run `install.packages('capm')` in (ideally) RStudio.

If you find any error or have suggestions to improve or include functionality to `capm`, I will glad to know it. Please get in touch so I can better understand your needs.
