# Tutorial on how to reproduce and modify the Dynamic Document for the Deworming Open Policy Analysis

## Background of Deworming Open Policy Analysis

- Learn about Deworming Interventions around the world.
  - Evidence Action, a NGO leading deworming intitiatives around the world, has put a excelente informational page [here](https://www.evidenceaction.org/dewormtheworld-2/)
  - Read the summary of this page from a previous URAP [here](https://github.com/BITSS-OPA/Tutorials/blob/master/Summaries/Deworming/Deworm%20the%20World%20-%20summary.md)  

- Learn about Deworming Cost-Effectiveness.
  - Report from Evidence Aciton: [here](https://www.evidenceaction.org/2017-deworming-cost-effectiveness/)
  - Summary from previous URAP: [here](https://github.com/BITSS-OPA/Tutorials/blob/master/Summaries/Deworming/Deworming%20Cost-Effectiveness%20-%20summary.md)

## Your setup to reproduce and modify the dynamic document of the deworming OPA
- To run the dynamic document (DD) you will need to install [R](https://cran.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/). The relevant file is `05_final_opa.rmd`.

- To knit the file and see the report output, click the `Knit` button on the banner below the file name. When you knit a file, the `rmarkdown` package will call the `knitr` package. In the setup code, we called `knitr` to modify how we want R studio to present our R code. You can learn more about the `knitr` options [here](https://yihui.org/knitr/options/#package-options)

- The first element of the DD is the header, a YAML script that specifies the output characteristics. The YAML script is enclosed by `---` (line 1 - 21).  

- The rest of the DD consists of 1) narrative elements written in Markdown, and 2) analysis elements written in R code. Code chunks are enclosed by `` ```{r} `` at the beginning and `` ``` `` at the end.

- Each chunk of code has a name assigned in the curly brackets right after `r`, and we will refer the code chunks to their respective names in this readme file. Code chunks can be either analytic or summary tables.

- Analytic code chunks are those that contain key analytic steps needed to reproduce the final policy estimate. Each of these chunks is wraped into a function named `chunk_[name_of_the_chunk]` (eg. the chunk final-pe wraps all steps into a function called `chunk_final_pe`). This function is called at the end of the same chunk and can be called later on to reproduce the final result without running all the non-analytics chunks.

### Description of specific code chunks

In this section we provide brief explanations for key code chunks.

##### Code chunk: setup

This code chunk checks whether you have installed the packages required by this DD, and it will install the packages that you haven't installed already. It adjusts some overarching settings of `rmarkdown` should present the code throughout the DD:

<!-- No need to explain each line of code, if you want to add more detail, you can do so as comment to the corresponding line in the original code -->

- Source code is displayed
- Warnings and messages are displayed only in console and **not** in the output document.
- **(?)** Root directory is set to be in the same directory where DD is.

This code chunk also defines a function to set latex and html output to a certain color.


##### Code chunk: notes

This code chunk is a brief description of the object naming format that this DD takes on, as well as the function structure.

##### Code chunk: sources

This code chunk defines the different sources to be used in the analysis. Following [Hoces de la Guardia et al. 2020](https://osf.io/preprints/metaarxiv/jnyqh/), theses sources are separated into three categories: data, research and guesswork. Each source is assinged to an R object (a variable) so that it can be systematically used throughout the DD. Inline comments describe the definitions and sources of the data.

*Insert naming conventions in appendix*


## Body of Analysis

After each analytic step is described in narrative form, there will be 1) a set of equations, written in latex, describing the step  and 2) a code chunk that defines a function to implement said step in R

At the end of each section, there will be a cumulative summary table of all the equations and sources used so far.

### 1. Introduction
The introduction describes how important deworming is and lists out where to find materials used to reproduce our policy analysis on deworming. This policy analysis describes three different approaches to compute the Cost Benefit Analysis (CBA) of deworming interventions.

### 2. Methodology

#### Common structure - common elements across all three approaches

In this section, we introduce several key parameters:

- Final policy estimate - **Net Present Value** (NPV) of deworming  
- Another index - **Cost effective ratio** in absolute terms (CEA) and relative terms(RCEA)
- **Benefits**: Additional earnings expected to generate due to a deworming treatment, computed as a discounted sum over their working lifetime.
- **Discounting rate**: Real interest rate

For each parameter, we will introduce them in plain text first, and then add additional info in a collapsible tab. The tab is coded in HTML and the code starts with `<details>` tag. Users can open and close this tab on demand. Here it includes two parts:

1. Latex formulas that define our new parameter.
  - Latex render formulas into a nice mathematical format
  - `r equationIndex <- equationIndex + 1` is for updating the numbering label next to the latex formula

2. ***Code Tracker***
<!--Discuss with Aleksandra -->

  - This code chunk defines a function that returns function objects that calculate our key parameters based on the latex formulas.
  - Code chunk `final-pe`:
    - `NPV_pe_f()`: function that calculates NPV
    - `CEA_pe_f()`: function that calculates CEA
    - `RCEA_pe_f()`: function that calculates Relative CEA
  - Code chunk `benefits`: Benefits (B)
    - `pv_benef_f()`: function that calculates net value of cumulated Benefits(B)
  - Code chunk `interest-rate`: The discounting rate
    - `interest_f()`: function that calculates the real interest rate
    - `interest_16`: interest rate for 2016
    - `interest_19`: interest rate for 2019



*(might delete)* Three CBA approaches also share their differences. In summary, approaches 1 and 2 reflect different types of estimating earning profiles for one specific context (Kenya). And approach 3 incorporates both earning profile methodologies, focuses only on direct costs, and generalizes costs and prevalence for different settings. They also used different discounting rates when conducting their analysis.

After the horizontal rule created by `-------`, we have our first summary table for the DD. The summary table is also in a collapsible tab encoded by the HTML tag `<details>`. The chunk of code `sum-tables2` creates two tables: one for equations used up until this point, the other for inputs specified up until this point. Right now we only specified the interest rates on the government bond for 2016(`i_16`) and 2019(`i_19`), as well as the inflation rate for 2016 (`\pi_16`) and 2019(`\pi_19`). *(Needs to fix latex syntax)*

`knitr::kable()` in the code chunk customizes the look of the table by adding column names, captions, and styling options.

**NOTE:** Every parameter in this section is also introduced like the common elements above: 1) Introduce in plain text 2) in a "Show all the details" collapsible tab, show the latex formula for said parameter and the function for generating such parameter.

#### 2.1 Approach 1: Baird et al.

- Effect on earnings: predicted by hours worked in the treatment group 10 years after the intervention

- Results:
  1. Total effect on earnings projected over a lifetime
  2. Estimated effect due to the government collecting additional taxes on higher earnings  
  We look at the effects under 2 scenarios: with and without externalities over those who did not receive deworming interventions.

##### 2.1.1 Gains in Earnings

Key parameter: gains in earings ($\Delta w_{t}$)

Code tracker: Code chunk `earnings1`
- `earnings1_f()`: function that creates gains in earnings in Approach 1.

##### 2.1.1.1 Earnings over time  

Key parameter: starting monthly wages in US dollars($\Delta w_{0}$), monthly wages after t years in US dollars ($\Delta w_{t}$)

Code tracker: Code chunk `wage_t`
- `wage_0_mo_f()`: *function* that calculates the initial weekly wage in dollars, by taking the weighted average of wages for the control group in agriculture, working wage, and self-employed sectors.
- `wage_0_mo`: *value* of initial weekly wage calculated with inputs from research (Suri paper, Worms at Work paper)
- `wage_t_mo_f()`: *function* that  calculates the weekly wage in dollars after t years
- `wage_t_mo`: *value* of the weekly wage in dollars after t years with inputs from research




## Appendix A: Abbreviations

DD: Dynamic document    
W@W: Worms at work  


NPV: Net Present Value  
CEA:  
RCEA:  
B: benefits  

## Appendix B: Input Meanings  

*\pi_16*: inflation rates for 2016

**2.1.1.1 Earnings over time**  
*w*<sub>t</sub>: wages in year t  
*w*<sub>0</sub>: initial wages in dollars  
*Xp*: number of years of work  
*h*: average worked hours dedicated to each sector  
*ag*: agriculture  
*ww*: working wage  
*se*: self-employed sectors  
