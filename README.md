# Modeling Longitudinal Gambling Data: Challenges and Opportunities

This README consists of two parts: 1) the tutorial on how to fit the marginal two 
part model, and 2) information about the simulation study.

OSF link: [https://osf.io/6pbgv/](https://osf.io/6pbgv/)

# Tutorial
The tutorial can be found in `tutorial.html` ([HTML preview link](https://htmlpreview.github.io/?https://github.com/rpsychologist/marginal-two-part/tutorial.html)), 
or the source document `tutorial.Rmd`. The tutorial shows hot to fit a custom marginal 
two-part gamma or lognormal model using `brms`.

# Simulation
## Prerequisites
The following folders must exist:
  data/
  save/
  figures/
  
Configure the number of CPU cores and simulation replications 
in `code/compute_setup.R`

## Required packages
```{r}
install.packages(c("brms", 
                   "powerlmm", 
                   "parallel",
                   "tidyverse", 
                   "ggstance", 
                   "cowplot"))
```

## Run the simulation
- `0_run_sims.R` sources all the simulation files. 

## Results
    - `1_results.R`
      Summarise all the results for the simulation with complete data,
      and saves the figures to `figures\`
    - `1_results_MAR.R`
      Summarizes MAR simulation and creates a table with the results
      
## Additional code
The code used to simulate data from the hurdle model can be found 
in `powerlmm:::sim_hurdle`
