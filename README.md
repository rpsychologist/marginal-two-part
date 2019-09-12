# Modeling Longitudinal Gambling Data: Challenges and Opportunities

This README consists of two parts: 1) the tutorial on how to fit the marginalized 
two part model, and 2) information about the simulation study.

Preprint: [https://psyarxiv.com/uvxk2/](https://psyarxiv.com/uvxk2/)
OSF link: [https://osf.io/6pbgv/](https://osf.io/6pbgv/)

# Tutorial
The tutorial can be found in `tutorial.html` ([HTML preview link](https://htmlpreview.github.io/?https://github.com/rpsychologist/marginal-two-part/blob/master/tutorial.html)), 
or the source document `tutorial.Rmd`. The tutorial shows how to fit a custom marginalized 
two-part gamma or lognormal model using `brms`.

# Simulation
## Requirements
The following folders must exist:
- data/
- save/
- figures/
  
Configure the number of CPU cores and simulation replications 
in `code/compute_setup.R`

## Required packages
```{r}
install.packages(c("brms", 
                   "powerlmm", # version >= 0.5
                   "parallel",
                   "tidyverse", 
                   "ggstance", 
                   "cowplot"))

# Powerlmm dev version used for the simulations                   
devtools::install_local(path = "powerlmm_0.4.0.9000.tar.gz")                  
```

At the time of writing, powerlmm version 0.5.0 is not yet published on CRAN. 
A unfinished development version is therefore included with this repo.

## Run the simulation
- `0_run_sims.R` sources all the simulation files. 

## Results
These two files are used to summarize the results:
- `1_results.R`
  Summarize all the results for the simulation with complete data,
  and saves the figures to `figures\`
- `1_results_MAR.R`
  Summarizes MAR simulation and creates a table with the results
      
## Additional code
The code used to simulate data from the hurdle model can be found 
in `powerlmm:::sim_hurdle`
