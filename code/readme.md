# Tutorial on how to reproduce and modify the Dynamic Document for the Deworming Open Policy Analysis

## Background of Deworming Open Policy Analysis

- Learn about Deworming Interventions around the world.
  - Evidence Action, a NGO leading deworming initiatives around the world, has put a excellent informational page [here](https://www.evidenceaction.org/dewormtheworld-2/)
  - Read the summary of this page from a previous URAP [here](https://github.com/BITSS-OPA/Tutorials/blob/master/Summaries/Deworming/Deworm%20the%20World%20-%20summary.md)

- Learn about Deworming Cost-Effectiveness.
  - Report from Evidence Action: [here](https://www.evidenceaction.org/2017-deworming-cost-effectiveness/)
  - Summary from previous URAP: [here](https://github.com/BITSS-OPA/Tutorials/blob/master/Summaries/Deworming/Deworming%20Cost-Effectiveness%20-%20summary.md)

## Your setup to reproduce and modify the dynamic document of the deworming OPA
- To run the dynamic document (DD) you will need to install [R](https://cran.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/). The relevant file is `05_final_opa.rmd`.

- To knit the file and see the report output, click the `Knit` button on the banner below the file name. When you knit a file, the `rmarkdown` package will call the `knitr` package. In the setup code, we called `knitr` to modify how we want RStudio to present our R code. You can learn more about the `knitr` options [here](https://yihui.org/knitr/options/#package-options)

- The first element of the DD is the header, a YAML script that specifies the output characteristics. The YAML script is enclosed by `---` (line 1 - 21).

- The rest of the DD consists of 1) narrative elements written in Markdown, and 2) analysis elements written in R code. Code chunks are enclosed by `` ```{r} `` at the beginning and `` ``` `` at the end.

- Each chunk of code has a name assigned in the curly brackets right after `r`, and we will refer to the code chunks with their respective names in this readme file. Code chunks can either contain analytic code or summary tables.

- Analytic code chunks are those that contain key analytic steps needed to reproduce the final policy estimate. Each of these chunks is wrapped into a function named `chunk_[name_of_the_chunk]` (e.g. the chunk final-pe wraps all steps into a function called `chunk_final_pe`). This function is called at the end of the same chunk and can be called later on to reproduce the final result without running all the non-analytic chunks.

- Each analytic code chunk calls 'invisible(list2env())' at the bottom. "list2env()" takes a function and takes all outputs and drops them into the global environment. The "invisible" function is used in place of "return" when you want the output assigned to a variable but don't want the output to be returned.

- Summary table code chunks are chunks that put all the equations and inputs into two summary tables after every section. It does so cumulatively.


##### Code chunk: setup

This code chunk checks whether you have installed the packages required by this DD, and it will install the packages that you haven't installed already. It adjusts some overarching settings of how `rmarkdown` should present the code throughout the DD. It also defines a function to set latex and html output to a certain color.

##### Code chunk: notes

This code chunk is a brief description of the object naming format that this DD takes on, as well as the function structure.

##### Code chunk: sources

This code chunk defines the different sources to be used in the analysis. Following [Hoces de la Guardia et al. 2020](https://osf.io/preprints/metaarxiv/jnyqh/), these sources are separated into three categories: data, research and guesswork. Each source is assigned to an R object (a variable) so that it can be systematically used throughout the DD. Inline comments describe the definitions and sources of the data.

*Insert naming conventions in appendix*


## Body of Analysis

### Overall Structure:

#### Part 1: Introduction
The introduction describes how important deworming is and lists out where to find materials used to reproduce our policy analysis on deworming.

#### Part 2: 3 Approaches to Compute the Cost Benefit Analysis

This part consists of 3 different approaches to compute cost benefit analysis.

The first two come from two papers, and the third one combines the first two. But when the DD introduces the three approaches, it follows the same protocol:

1. Text and formulas to define new components
2. Code block to calculate the introduced components by coding the equations.
  + Each code chunk is wrapped in the `chunk_[name]` function so that it can be reproduced and called by the interactive shiny app.
  + Each code chunk generates intermediate outputs so that variables can be printed in markdown text.

Approach 3 also includes two other sections: one summarizes all the approaches into a table, and the other informs readers of which approach the DD takes to reproduce the main policy estimate.

#### Part 3: Main Results

This part mainly renders the results of policy estimates derived using different approaches. The summary table only renders the Net Present Value (NPV) indicator, whereas the bullet point text results also presented the other two key indicator results: Cost Effective Ratio (CEA) format and Relative Cost Effective Ratio (RCEA) format.

##### Code chunk: all-steps

We first define a `unit_test_f` function that tests whether our computed result is the same as a hardcoded value , allowing for a very small computing error of 0.0001. The goal is to inform the programmers which variables change values and which ones don't. After making sure the change is what we want, we can then update the hardcoded values and use the unit test function to monitor changes in variable values. We use this unit test function for all variables defined in Part 2 so that we know the computed result is the expected result.

Then we import the source data (all the variables with the suffix `_so`) to reproduce key components such as wages and gains in earnings using the formulas provided above. In the end we return a list of labelled objects so that we can call them later.

##### Code chunk: main-results

This code chunk is divided into two parts. The first part calculates the final policy estimate using the functions we defined previously. It includes all approaches that we used to get the policy estimates and all the policy estimate formats: Net Present Value (NPV), Cost Effective Ratio (CEA), and Relative Cost Effective Ratio (RCEA). Then it tracks the variable value changes using `unit_test_f` function. The second part renders the NPV results into a summary table.

#### Code chunk: generate-plot-function

This code chunk defines a function that both the DD and the shiny app calls to generate plots for distribution of policy estimates with simulated data.

#### Part 4: Accounting for Uncertainty

In this part we use Monte Carlo simulation to account for the uncertainty from our source variables in the process of making our policy estimates. A Monte Carlo simulation builds models of possible results by sampling from the distribution of variables with uncertainty.


##### Code chunk: mc-setup

In our case we define the function `sim_data1_f` to complete the Monte Carlo simulation process for us:
* For most of the source variables (with the suffix `_so`), draw *n* samples from a normal distribution with the mean set to the original source data and standard deviation to be 10% of the mean.
* Use these *n* generated sample source variables to calculate *n* estimates for each of the 11 approaches.
* Store the final result in a list of 12 vectors. The first 11 are the *n* policy estimates we got from the Monte Carlo simulation, and the last one is the total time it took to run the simulation.

##### Code chunk: run-mc

* Here we run `sim_data1_f` and get the list of vectors with *n* policy estimates for each of the 11 approaches.  

* We use the unit test function to test whether we get the expected simulation results. As you might notice, `unit_test_f` checks for the standard deviation of the simulation result of each policy estimate because the simulation result is a vector of numbers. Standard deviation reflects homogeneity of data better than mean.

* Then we plot the *n* policy estimates for the approach that estimates **NPV without externality, with benefit from Hamory et al and costs from Evidence Action in 2019** to get the distribution of life income effects of deworming for each treated child.

#### Part 5: Sensitivity Analysis



##### Link with Shiny App

Code chunks with `purl = TRUE` in the curly brackets at the top of the chunk are exported into a R file called `all_analysis.R`. Then our shiny app can call the variables and functions defined in this R file.

## Appendix A: Abbreviations & Function Definition

#### Abbreviations
- DD: Dynamic document  
- W@W: Worms at work  
- KLPS2: Kenya Life Panel Survey Round 2
- KLPS3: Kenya Life Panel Survey Round 3
- KLPS4: Kenya Life Panel Survey Round 4  
- EA: Evidence Action


#### Appendix B: Function Definition

- `NPV_pe_f`: calculate the NPV formula for policy estimates
- `CEA_pe_f`: calculate the CEA formula for policy estimates
- `RCEA_pe_f`: calculate the RCEA formula for policy estimates
- `pv_benef_f`: calculate the present value of benefits
- `interest_f`: calculate the real interest rate given
- `earnings1_f`: calculates gains in earnings using the first approach (Baird et al.)
- `earnings2_f`: calculates gains in earnings using the second approach(Hamory et al.)
- `wage_0_mo_f`: calculates the starting wage that is the weighted average of the three sectors: ag, ww, se
- `wage_t_mo_f`: calculates the wage at period t that is the weighted average of the three sectors: ag, ww, se
- `lambda1_in_f`: calculates the direct impact of deworming for Approach 1 and 2
- `lambda_eff_f`: calculates impact of deworming on children who were treated and had a worm infection in the original evaluation, as well as the prevalence rate for Approach 3
- `lambda_t_f`: calculates the direct impact of deworming after adjusting for different length of treatment for Approach 3
- `lambda2_in_f`: calculates the indirect/externality impact of deworming for Approach 1 and 2
- `saturation_in_f`: calculates p, the saturation
- `pv_costs_f`: calculates both the direct costs of mass deworming and indirect costs on the education system due to the additional time treated individuals spend in school
- `s2_f`: calculates the total direct costs of deworming in USD with complete subsidy using Approach 1
- `s2_f_new`: calculates the total direct costs of deworming in USD with complete subsidy using Approach 2
- `cost_per_student_f`: calculates the cost per student to get education, K
- `delta_ed_final_f`: calculates both the direct increase in secondary schooling and the externality effects on secondary schooling
- `costs1_p1_f`: cleans and aggregates data at country level
- `costs1_p2_f`: computes weights and per capita costs
