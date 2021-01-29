---
title: "<center><div class= 'mytitle'>Open Policy Analysis for Deworming</div></center>"
date: "<center><div class='mysubtitle'>29 January, 2021<br><img height = '80px' src = './shiny_app/www/bitss_logo_horizontal.png'><img height='80px' src='./shiny_app/www/CEGA_logo.png'></div></center>"
author: "<center><div class = 'contributors'>BITSS Team. Full list of contributors [here](https://github.com/BITSS-OPA/opa-deworming#list-of-contributors)</div></center>"
editor_options:
  chunk_output_type: console
output:
  
  bookdown::html_document2:
    code_download: yes
    code_folding: hide
    css: style.css
    highlight: tango
    includes:
      after_body: footer.html
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    theme: cerulean
    toc: yes
    toc_collapsed: no
    toc_depth: 3
    toc_float: yes
  html_document:
    df_print: paged
    toc: yes
    toc_depth: '3'
  word_document: null
link-citations: yes
pdf_document:
  extra_dependencies: xcolor
  fig_caption: no
bibliography: bibliography.bib
knit:
  # render to index.html for GitHub pages
  # render to 01_final_opa.html to knit locally
  # YAML does not support commenting inside the function
  (function(input_file, encoding) {
  rmarkdown::render(input_file, encoding=encoding, output_file=file.path("..", 'index.html'));
  rmarkdown::render(input_file, encoding=encoding, output_file='01_final_opa.html');
  })
  
---
\def\blue{\color{blue}}
\def\red{\color{red}}








```r
################
#####  Notes:
################
# Types of objects:
### Source ------->  Input & Model ------->  Policy Estimates (output)
###  (_so)           (_in)                           (_pe)
### values           functions                       values
###                  & values          
# Examples:                   
# - call_so_f       - tax_elas_in_f         - ten_year_revenue_pe
# - policy_so       - est_bill_in_f         - ten_year_top_tax_pe
#                                           - total_rev_pe
### arguments in functions should used "_var" and functions should used "_f"

# Each analytic code chunk will begin by listing all the inputs it needs, and
# the outputs it produces.
# - inputs: list
# - outputs: list
#### The key essential analytic steps are wrapted in a function   
#chunk_name_of_chunk <- function(){
##########################################
##########################################  
#
# here goes the essential analytic content
#
##########################################
##########################################  
#    return( )                         # A list with all the objects
#}                                     # generated inside the function
# The following line executes the code chunk and deposits its results
# into the current R enviornment:
#invisible( list2env(chunk_name_of_chunk(),.GlobalEnv) )
#
##### Execute values of the functions above when needed for the text:
# Anything under this comment is to create objects that are used in the body of
# text. Not to be used in the final results (could be deleted). Each of these
# object should end with the suffix _temp
#
# Use diagrams to represent complex nesting of functions. Example:
# # pv_costs_f
#  ├──── delta_ed_final_f
#  ├──── interest_f
#  └──── cost_per_student_f
#  |      └──── x
#  ├──── s2_f
#  └──── lambda2_in_f
```



```r
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_sources <- function(){
###############################################################################
###############################################################################  

    #############
    ##### Setup
    #############  
    nsims_so <- 1e4
    rescale_so <- FALSE
    policy_estimate_so <- "a3_inc_a2_all_sim"
    run_sim_so <- FALSE
    main_run_so <- TRUE
    periods_so <- 50               #Total number of periods to forecast wages
    costs_temp_so <- 1
    main_pe_so <- 289.8

    #############
    ##### Data  
    #############
    # ATTENTION!
    # costs2_ea_in
    ex_rate_so <- 74               #Exchange Rate - Central Bank of Kenya 74 , 85
    ex_rate_2018_so        <- 101.30  # Exchange rate (KES per international $)
                                    # - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)
    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    gov_bonds_new_so <- 0.09
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    inflation_new_so <- 0.04
    tax_so <- 0.16575              #ADD INFO!

    # costs data
    df_costs_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "DtW Costs")
    # crosswalk data on region and country
    df_costs_cw_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "state_country")
    # data on number of treated children
    df_counts_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "DtW Treatment #s")
    # Prevalence data:
    # original study
    # Any infection on original study
    #prevalence_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" = 0.55,
    # "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    df_prevalence_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
    # prevalence_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    new_costs_so <- NULL
    country_sel_so <- list("india", "kenya", "nigeria", "vietnam")
    country_sel_pop_so <- c(
      "india" = 1.366417750 * 1e9,
      "kenya" = 5.257397 * 1e7,
      "nigeria" = 2.0096360 * 1e8,
      "vietnam" = 9.646211 * 1e7
    )
    #https://data.worldbank.org/indicator/SP.POP.TOTL
    # options: "a1_tax_sim","a1_x_tax_sim","a1_all_sim", "a1_x_all_sim", "a2_tax_sim",
    # "a2_all_sim", "a3_inc_a1_all_sim", "a3_inc_a1_all_x_sim", "a3_inc_a2_all_sim"

    #############
    ##### Research
    #############
    df_research_so <- read_csv("rawdata/research/research_params.csv")   
    lambda1_so <- c(3.49, 0)            #Hrs per week increase for men and women, table 3, row 1, cols 2 & 3
    lambda1_sd_so <- c(1.42, 1.36)      #table 3, row 2, cols 2 & 3
    lambda1_new_so <- c(79.51465)       # avg treatment effect from klps2-4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(76)          # Hamory et al 2021
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, row 1 col 4
    lambda2_sd_so <- 7.8                # Table 3, row 2 col 4
    #This is are the parameters labeled eta in the doc
    prevalence_0_so <- 0.92 # 0.92 doi: https://doi.org/10.1111/j.1468-0262.2004.00481.x  location: table 2, row 6, column 1
    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B (Source data took the log, here the log is recovered)
    profits_se_so <- 1766          #Control group monthly self-employed profits -
                                   #Table 4, Panel C, Column 5, Row 1
                                   #FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 3, Panel B
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 3, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 3, Panel B
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)
                                                    #- see notes(0.1019575, -0.0010413), (0,0)
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct secondary schooling increase (from Joan)
                        0.0239963665555466, 0.027301406306074, 0.0234125454594173,
                       0.0279278879439199, 0.00647044449446303, 0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)
    delta_ed_par_so <- 1
    delta_ed_ext_par_so <- 1
    delta_ed_ext_so <- c(-0.0110126908021048,	0.0140448546741008,	-0.0034636291545585,  #Additional externality secondary schooling increase (from Joan)
                           0.0112940214439477,	0.0571608179771775,	-0.0560546793186931,
                           0.0558284756343451,	0.1546264843901160,	0.0055961489945619)
    delta_ed_ext_so <- cbind(delta_ed_ext_so, 1999:2007)
    include_ext_so <- TRUE
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    teach_sal_new_so <- (50000 * 12 / 49.773)
    teach_ben_new_so <- 0
                                  #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits -
                                  #news sources * 12 / ex_rate_2017_ppp_so
                                  # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                  # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    teach_sal_2017usdppp_so <- teach_sal_new_so * cpi_2017_so / cpi_2017_so # redundant, but for the sake of consistency

    n_students_so <- 45            #Average pupils per teacher	45
    #ATTENTION!
    years_of_treat_0_so <- 2.41      #Additional Years of Treatment - Table 1, Panel A
    unit_cost_local_so <- 43.66    #Deworm the World
    unit_cost_so <- 0.42           # Unit cost of deworming (in 2018 USD) - from Evidence Action
    #CALCULATIONS TO CONVERT ALL CURRENCY TO 2017 USD PPP
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018_so/ex_rate_2018_ppp_so
    unit_cost_2017usdppp_so <- unit_cost_ppp_so * cpi_2017_so / cpi_2018_so  # 0.8296927
    # Adjust for inflation: convert all costs to 2017 USD
    # Move this calculations into the body of the document (and outside of the sources chunk)
    costs_par_so <- 1
    costs_par_sd_so <- 0.1
    counts_par_so <- 1
    counts_par_sd_so <- 0.1

    #############
    ##### Guess work   
    #############
    # ATTENTION!
    # prevalence_r_in
    prevalence_r_so <- c("india" = 0.5665, "kenya" = 0.345, "nigeria" = 0.27, "vietnam" = 0.145)  #0.5665   0.5013121
    # based on https://docs.google.com/spreadsheets/d/1drKdU-kRjlRtwXq6nCqFC6gcoQ-eOaLfT9MWHSMZ0MA/edit?usp=sharing
    new_prevalence_r_so <- NULL
    years_of_treat_t_so <- 2.41      #Years of Treatment in new setting
    staff_time_so <- 0.3           #Added Deworming costs due to government staff time
    time_to_jm_so <- 10            #Time from initial period until individual join the labor force

    # Fix teach_sal_so       
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
###############################################################################
###############################################################################    
}
invisible( list2env(chunk_sources(),.GlobalEnv) )
#############
##### Notes:
#############
# on growth_rate_so: (http://data.worldbank.org/indicator/NY.GDP.PCAP.KD/), see calculation
# on "Kenya GDP per capita" tab. In W@W this equals 1.52%. ISSUE: This growth number should
# be updated to be 2002-2014, I think.
#
# on coef_exp_so: 1998/1999 Kenyan labor force survey; regression of earnings on age, age^2,
# female dummy, indicators for attained primary/secondary/beyond, and province dummies.
# Estimate used in W@W: (0.1019575, -0.0010413). ISSUE: For now assume no further life cycle
# adjustment beyond KLPS-3 (likely a conservative assumption).
#
# coverage_so: Overall Saturation (0.511) / 0.75 - not reported in table, average of T & C
```



<img src="/Users/fhoces/Desktop/sandbox/opa-deworming/code/images/main_pe.png" width="100%" style="display: block; margin: auto;" />

<div class = "divider"><span></span><span>
Executive Summary
</span><span></span></div>

This report is part of an Open Policy Analysis (OPA) on deworming interventions. An OPA is a policy analysis that emphasizes high levels of transparency and reproducibility. It contains one [open output](https://fhoces.shinyapps.io/shiny_app_test/) that best represents the facts to inform policy makers, one report (this document) that clearly explains all the analysis, and [one repository](https://github.org/bitss/opa-deworming) that contains all the materials to reproduce the report and final output.

This report describes three approaches to compute the net present value of mass deworming interventions. The first two approaches are exact reproductions from previous research [@baird2016worms; @klps4], and the third approach is a combination of the previous two with some modification suggested by Evidence Action, a key policy partner in this area. This third approach uses the same benefits as the previous approaches and adjusts for different costs, prevalence rates and length of treatment across settings. The report suggests that this final approach should be used as the best available policy estimate to compare costs and benefits of deworming in different settings.

Our main policy estimate predicts that a mass deworming intervention will have a net present value (comparison of stream of benefits and costs from today's perspective) of 289.8 for a setting with average prevalence and average unit costs (among the countries for which Evidence Action has data). Readers interested in learning about the predicted value for a specific setting are encouraged to use the [interactive app](https://fhoces.shinyapps.io/shiny_app_test/) components of this OPA.

<div class = "divider"><span></span><span>
*
</span><span></span></div>


# Open Policy Analysis {-}

This report is part of an Open Policy Analysis (OPA) project on deworming interventions. Using a framework for making policy analyses transparent and reproducible [@hoces2020framework], OPA’s goal is to clearly show how an analysis was conducted and how to best represent key figures or results for policy makers to use as a factual basis for deliberation. In addition, OPA facilitates the re-use of analyses across similar settings, and sheds light on how evidence generated by research is used in specific policy analyses[^13].

[^13]: In addition to making policy analyses more transparent and reproducible (the goal of the OPA initiative) CEGA leads and a [Costing Transparency Initiative](https://cega.berkeley.edu/initiative/cost-transparency-initiative/) to increase the availability of data and knowledge on how the costs of different development interventions are estimated. 

 This OPA project contains three components, following the OPA principles laid out in the aforementioned paper:

  1. One single output that best represents the factual information required by policy makers to inform their position regarding a policy of mass deworming. This output is presented in Figure 1, and described in the [results section](#policy-estimate) of this report. The connection between each component of the analysis and the final output can be explored interactively in [this web app](https://fhoces.shinyapps.io/shiny_app_test/).

  2. This detailed report that describes how to obtain the policy estimate and describes each component of the analysis.

  3. [A repository](https://github.org/bitss/opa-deworming) that contains all the materials needed to reproduce the analysis with minimal effort (report and interactive app).  

This report provides a complete description of the analysis behind the results presented to inform a policy discussion on deworming interventions. It describes how to reproduce the analysis in its entirety, and includes all the methodological choices involved. In order to document all the steps without overwhelming the reader, the report is displayed in a layered fashion. The first layer consists of a narrative description of the analysis. The second layer, which appears after clicking in the ![screenshot](images/show_details.png?display = inline-block) contains equations that show how each piece of the analysis was carried out. The third and final layer displays the code used to operationalize each equation. All of this information is contained in this document using dynamic documentation [@xie2015dynamic], so interested readers can access the source file of the report and reproduce the entire document in their own computing environments.


# Introduction  

Parasitic worm infections, also known as soil-transmitted helminths (STH) and schistosomiasis, are endemic in many countries across the globe, disproportionately affecting the poor. These parasitic worms interfere with regular bodily processes by decreasing nutrient uptake and can thus lead to serious consequences on human health, education outcomes, and long-term economic well being. In particular, evidence indicates that these worms contribute to malnourishment, impairment of mental and physical development, lower school attendance, and decreased wages [@croke2014long; @miguel2004worms; @baird2016worms].

Evidence from previous mass deworming interventions has demonstrated to be a highly effective public health policy. Here the report provides a policy analysis that compares benefits and costs of deworming across different settings, allowing for the translation of research findings into different policy-relevant scenarios.

The goals of this OPA are three. First, to increase the transparency and reproducibility behind existing policy analyses on the costs and benefits of mass deworming programs. Second, to update this policy analyses with input from stakeholders closely involved in policy making around deworming. And third, to illustrate how an open policy analysis framework can be implemented in practice.

The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:     

  1. Reproducing the original CBA produced by @baird2016worms, which estimates the net present value of a Kenya school-based deworming program after a 10 year follow-up for four different policy estimates.     
  2. Reproducing an updated version of such analysis on the same intervention but with additional follow-up data [@klps4].
  3. Producing a new analysis that, building from the previous two approaches, focuses on one specific policy estimate, and allows for results to vary depending on key characteristics of current settings where deworming policies are being implemented. This new approach was developed in consultation with a key stakeholder in this area, the non-governmental organization (NGO) Evidence Action (EA)[^1].




# Methodology  

The report first describes the common elements across all three approaches, and then describe each approach in detail.

## Common structure {-}

The starting point is a comparison of a stream of benefits and costs over the lifetime of the recipients of deworming. The final policy estimate is the discounted sum of all costs and benefits, known as the Net Present Value (NPV)[^12].

[^12]: Approaches 1 and 2 also present results in the format of internal rates of return (IRR). Following the principle of open output, the report restricts the presentation of results to just one format. NPV was chosen over IRR in consultation with Evidence Action to clearly communicate the scale of the welfare effects. 




<details><summary>Show all the details</summary>

\begin{equation}
NPV = B - C \\
\end{equation}

Where:  

- $NPV$: net present value of the deworming treatment   
- $B$: benefits of the deworming treatment  
- $C$: costs of the deworming treatment  



```r
# - inputs: total per capita benefits, total per capita costs
# - outputs: Net Present Value (NPV)
chunk_final_pe <- function(){
###############################################################################
###############################################################################  

    NPV_pe_f <- function(benefits_var = 1, costs_var = 1){
        benefits_var - costs_var
    }

###############################################################################
###############################################################################  
    return(list("NPV_pe_f" = NPV_pe_f))
}
# Excecute the previos function and load the listed objects in to the current R
# session (global environment)
invisible( list2env(chunk_final_pe(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
```


</details>
<br>

Benefits are equal to the additional lifetime earnings that individuals are expected to generate due to deworming treatment. These additional earnings are computed as a discounted sum over their working lifetime.  


<details><summary>Show all the details</summary>


\begin{equation}
B =   \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} E_{t}

\label{eq:1}
\tag{1}
\end{equation}

Where:

- $E_{t}$: earnings individuals are expected to generate at period t  
- $r$: real interest rate as the discounting rate  
- $t$: period t. Period 0 represents time of intervention. Individuals are assumed to enter the labor market 9 years after treatment.  


```r
# - inputs: stream earnings, discounting rate, number of periods
# - outputs: function that computes the present value of benefits
chunk_benefits <- function(){
###############################################################################
###############################################################################  

  pv_benef_f <- function(
    earnings_var = earnings_in,
    interest_r_var = interest_in,
    periods_var = periods_so
  ) {
      index_t <- 0:periods_var
      res1 <- sum( ( 1 / (1 + interest_r_var) )^index_t * earnings_var )
      return(res1)   
    }

###############################################################################
###############################################################################  
    return(list("pv_benef_f" = pv_benef_f))
}
invisible( list2env(chunk_benefits(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
```
</details>
<br>

At a high level all three approaches focus on the same type of benefits: the increase in incomes over the lifetime of beneficiaries of deworming. This is likely an under-estimate of the benefits as it does not quantify the non-pecuniary effects of improved health.  The costs can be separated into direct costs of implementing and evaluating deworming programs, and indirect costs, such as additional costs to the education system as a result of increased child attendance, associated with the benefits of deworming.

The main differences in benefits across the three approaches have to do with how to predict the earnings profiles over a lifecycle, and how to account for differences in worm prevalence rates and length of treatment across settings. Approaches 1 and 2 use different earning profiles, and approach 3 combines both earning profiles and adjusts for possible differences in prevalence rates of worm infections and length of treatment.

The main differences in costs between scenarios have to do with a) whether indirect costs are included, and b) how to compute the relevant unit cost for the analysis. The first two approaches include indirect costs and use the unit costs of a specific country (Kenya) where the study was originally conducted, while the third approach does not include indirect costs and use unit costs of various countries from data provided by Evidence Action.


### The discounting rate  {-}

All three approaches use the real interest rate ($r$) as the discounting rate. This is obtained from the interest rate on government bonds ($i$) minus the inflation rate ($\pi$).

<details><summary>Show all the details</summary>

\begin{equation}
r = \frac{1 + i}{1 + \pi} - 1 \\
r \approx i - \pi

\label{eq:2}
\tag{2}
\end{equation}

Where:   

- $r$: real interest rate as the discounting rate  
- $i$: interest rate on government bonds  
- $\pi$: inflation rate  


```r
# - inputs: nominal interest rate, inflation rate
# - outputs: real interest rate. exact and approximate formula
chunk_interest <- function(){
###############################################################################
###############################################################################  

    interest_f <- function(gov_bonds_var = gov_bonds_so ,
                           inflation_var = inflation_so) {  
        interest_exct_in <- (1 + gov_bonds_var) / (1 + inflation_var) - 1
        interest_in = gov_bonds_var - inflation_var
        return(list("interest_in" = interest_in,
                    "interest_exct_in" = interest_exct_in))
    }

###############################################################################
###############################################################################  
    return(list("interest_f" = interest_f))
}

invisible( list2env(chunk_interest(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:

interest_in <- as.numeric(
  interest_f(gov_bonds_var = gov_bonds_so,
             inflation_var = inflation_so)$interest_in
  )

interest_new_in <- as.numeric(
  interest_f(gov_bonds_var = gov_bonds_new_so,
             inflation_var = inflation_new_so)$interest_in  
  )
```

</details>
<br>

The actual value varies across approaches depending on the time and country chosen. For example approach 1 uses the return from government bonds and the inflation rate in Kenya for the year 2016, while approaches 2 and 3 uses the values for the same country for the year 2019. This results in discount rates of 9.85% and 5% for approach 1 and, 2 and 3 respectively.






## Approach 1: @baird2016worms

In this first approach, the effect on earnings over the entire lifecycle is predicted by extrapolating the effects on hours worked by individuals in the original treatment group, ten years after the intervention.

Two types of results are presented: the total effect on earnings projected over a lifetime and the estimated fiscal effect due to the government collecting additional taxes on higher earnings. The effects are calculated in two scenarios: with and without externalities over the population of children who did not receive deworming interventions. In the original deworming study conducted in Kenya, there is evidence of epidemiological externalities for children who remained untreated but who attended treatment schools, as well as for children living near treatment schools. Externality effects may not be as relevant for current-day deworming programs since most programs are national programs that target all school-aged children (and in some cases, preschool-aged children) in at-risk areas.


###  Gains in earnings

Gains in earnings ($\Delta W_{t}$) are the result of multiplying expected earnings in a certain period ($w_t$) with the effects of deworming on worked hours. This effect can have two components: a direct effect of deworming on the individual ($\lambda_1$) and the indirect effect on earnings due to externalities ($\lambda_2$). The indirect effects are considered within the context of the treatment coverage and saturation.

<details><summary>Show all the details</summary>

\begin{equation}
\Delta W_{t} = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)

\label{eq:3}
\tag{3}
\end{equation}

Where[^6]:   

 - $w_t$: the earnings in period $t$   
 - $\lambda_{1}$: the direct effects of deworming on earnings  
 - $\lambda_{2}$: the indirect effects of deworming on earnings   
 - $p$: saturation, measures the fraction of the population that is effectively using the treatment  
 - $R$: coverage, defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group  

[^6]: The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender.



```r
# - inputs: earnings wihtout treatment (wage_in), direct treatment eff
# (lambda1_so), indirect treatment eff (lambda2_so), saturation and coverage (coverage_so)
# - outputs: earnings (no name specified)
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings_app1_f <- function(wage_var = wage_t_in,
                          lambda1_var = lambda1_so,
                          lambda2_var = lambda2_so,
                          saturation_var = saturation,
                          coverage_var = coverage_so) {  
        res1 <- wage_var * ( lambda1_var + saturation_var *
                               lambda2_var / coverage_var )
        return(res1)
    }

###############################################################################
###############################################################################  
    return(list("earnings_app1_f" = earnings_app1_f))
}

invisible( list2env(chunk_earnings1(),.GlobalEnv) )
```

</details>
<br>

#### Earnings over time

Wages in year $t$ correspond to the initial weekly wage ($w_0$) adjusted by an economy-wide increase in salaries and by an increase in salaries due to additional experience at the individual level. The economy-wide wage adjustment is assumed to be equal to the per capita GDP growth ($g$) applied to the total number of years of work ($Xp$). The life cycle path for wages increases at decreasing rates (wages typically increase with more years of work, then decline later in a life cycle). It is assumed that individuals enter the labor force 10 years after the treatment period. Weekly wages are multiplied by 52 weeks to obtain the annual rate.

The initial wage in dollars ($w_{0}$) is a weighted average of wages for the control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the fraction of all the average worked hours dedicated to each sector ($h$).  

<!--To do: find the specific reference from in Suri (page, and table #,  location), and add using the @notation (you will need to edit the bibliography.bib file). If you cannot find the Suri reference, send me an email and I will look for it-->

The wage in agriculture comes from @suri2011selection, whereas the working wage comes from the study data and is defined as an hourly wage for those who reported more than 10 hrs of work per week in the control group. The self-employed wage ($w_{se}$) was constructed as the reported monthly earnings from self-employed profits, divided by the reported weekly number of hours worked in self-employment for those who worked a positive number of hours (multiplied by 4.5 to obtain the monthly total).

The monthly self-employed profits and self-employed hours for the control group, for those with positive hours, also comes from study data (Page 1168, Table 4, Panel C, Column 5, Row 1). The measure of hours in self employment used to compute wages is different from the one used to compute the weights above. The first one captures hours of work among those actively employed in the self-employed sector, and the second captures the average hours of work as self-employed among all the population of working age in the sample (hence capturing the relative importance of the self employed sector in the economy).


<details><summary>Show all the details</summary>

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50

\label{eq:4}
\tag{4}
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
\\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}

\label{eq:5}
\tag{5}
\end{equation}

\begin{equation}
w_{se} =  \frac{ \text{Monthly self-employed profits} }{4.5 \times E[h_{se}|h_{se}>0] }

\label{eq:6}
\tag{6}
\end{equation}

Where:  

- $w_t$: the weekly earnings in period $t$  
- $w_0$: the initial weekly earnings  
- $g$: per capita GDP growth  
- $Xp$: years of work  
- $\hat{\beta_1}$: coefficient estimate for $Xp$  
- $\hat{\beta_2}$: coefficient estimate for $Xp^2$  
- $ex$: exchange rate
- $h$: average worked hours dedicated to each sector  
- $ag$: agriculture  
- $ww$: working wage  
- $se$: self-employed sectors  



```r
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_in) and function (wage_0_f),
# Wage trajectory: value (wage_t_in) and function (wage_t_f).
chunk_wages <- function(){
################################################################################
################################################################################  
    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision

  wage_0_f <- function(wage_ag_var,
                          wage_ww_var,
                          profits_se_var,
                          hours_se_cond_var,
                          hours_ag_var,
                          hours_ww_var,
                          hours_se_var,
                          ex_rate_var){
        experience_aux <- 0:periods_so - time_to_jm_so
        wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
        wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
        alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) /
          sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
        res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
        return(res1)
    }

  wage_t_f <- function(wage_0_var,
                          growth_rate_var,
                          coef_exp1_var,
                          coef_exp2_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        res1 <- 52 * wage_0_var * ( ( 1 + growth_rate_var )^experience_aux ) *
          ( 1 + coef_exp1_var * experience_aux + coef_exp2_var *
              (experience_aux^2) ) * ifelse(0:periods_so >= time_to_jm_so, 1, 0)
        return(res1)
    }



################################################################################
################################################################################
    return(list("wage_0_f" = wage_0_f,
                "wage_t_f" = wage_t_f))
}

invisible( list2env(chunk_wages(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
wage_0_in <- wage_0_f(wage_ag_var = wage_ag_so,  
                      wage_ww_var = wage_ww_so,
                      profits_se_var = profits_se_so,
                      hours_se_cond_var = hours_se_cond_so,  
                      hours_ag_var = hours_ag_so,
                      hours_ww_var = hours_ww_so,
                      hours_se_var = hours_se_so,
                      ex_rate_var = ex_rate_so)  

#close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
#but I suspect diff due to computational precision
wage_t_in <- wage_t_f(wage_0_var = wage_0_in,
                      growth_rate_var = growth_rate_so,
                      coef_exp1_var = coef_exp_so[1],
                      coef_exp2_var = coef_exp_so[2])
```

</details>
<br>

#### Deworming effects: direct and externalities

The estimated impact of deworming on hours worked comes from @baird2016worms and are estimated separately for men ($\lambda_{1,male}$) and women ($\lambda_{1,female}$). These two parameters are combined with a simple mean in the analysis.

The estimated externality effect ($\lambda_{2}$) reflects the additional hours worked due to individuals who did not receive the treatment but still saw reductions in the likelihood of infection due to lower worm prevalence in their community.  Note that this parameter is not estimated by gender, so the report repeats its value two times. All the components to the equation \\ref{eq:7} come from @baird2016worms. The externalities effects are adjusted by the coverage and saturation of the original study.

<details><summary>Show all the details</summary>

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}\\

\label{eq:7}
\tag{7}
\end{equation}

Where:

- $\lambda_1$: average impact of deworming on hours worked for both men and women  
- $\lambda_{1,male}$: average impact of deworming on hours worked for men   
- $\lambda_{1, female}$: average impact of deworming on hours worked for women  



```r
# - inputs: direct (lambda1_so), and indirect (lambda2_so) treatment effects by gender
# - outputs: simple average of direct and indirect treatment eff.
chunk_lambdas<- function(){
###############################################################################
###############################################################################    

    lambda1_in_f <- function(lambda1_var = lambda1_so) {
        rep(0.5 * lambda1_var[1] + 0.5 *lambda1_var[2], 2)
    }
    lambda2_in_f <- function(lambda2_var = lambda2_so){
        rep(lambda2_var, 2)
    }

##############################################################################
###############################################################################  
    return(list("lambda1_in_f" = lambda1_in_f,
                "lambda2_in_f" = lambda2_in_f ) )
}
invisible( list2env(chunk_lambdas(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_in <- lambda1_in_f()
lambda2_in <- lambda2_in_f()
```

</details>
<br>

#### Coverage and saturation of the original study

The coverage ($R$) is defined as the fraction, among all neighboring schools (within 6 km), that were treated within the study. Since the treatment was applied to approximately two thirds of the population, $R$ is set to: $R  = 0.68$[^6].  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively using the treatment. It is defined as a weighted average of the treatment take-up under a full subsidy for deworming and the take-up under zero subsidy.   


[^6]: Last paragraph of page 9(1645) of @baird2016worms


For this setting @kremer2007illusion (Page 48, Table 1, Panel C, Col 1, Row 3) estimated that take-up with full subsidy ($Q(full)$) was 0.75. Miguel and Kremer (2004) (Table 3 and footnote 18) observed minimal to no take-up without subsidy ($Q(0)$), hence it is assigned the value of 0.  

<details><summary>Show all the details</summary>

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)

\label{eq:8}
\tag{8}
\end{equation}

Where:

- $p$: saturation, measures the fraction of the population that is effectively using the treatment  
- $R$: coverage, measures the fraction of the population that is effectively using the treatment  
- $Q(full)$: take-up with full subsidy  
- $Q(0)$: take-up without subsidy  



```r
# - inputs: coverage (coverage_so), take-up with full subsidy (q_full_so), and
# take-up with no subsidy (q_zero_so)
# - outputs: saturation (saturation_in)
chunk_coverage <- function(){
###############################################################################
###############################################################################  

  saturation_in_f <- function(coverage_var = coverage_so,
                              q_full_var = q_full_so,
                              q_zero_var = q_zero_so){
      saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
      return(list("saturation_in" = saturation_in))
    }

###############################################################################
###############################################################################  
    return(list("saturation_in_f" = saturation_in_f))   
}
invisible( list2env(chunk_coverage(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
saturation_in <- saturation_in_f()$saturation_in

# Computing values for inline text:

# pv_benef
# ├──── earnings_app1_f
# |      ├──── delta_ed_final_f
# |      ├──── saturation_in_f
# |      └──── wage_t_f()
# |      |      └──── wage_0_f()
# |      |            
# |      ├──── lambda1_in_f()
# |      └──── lambda2_in_f()
# └──── interest_f()

earnings_no_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_in[1],
  saturation_var = saturation_in,
  lambda2_var = 0,
  coverage_var = coverage_so
)

earnings_yes_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_in[1],
  saturation_var = saturation_in,
  lambda2_var = lambda2_in[1],
  coverage_var = coverage_so
)

pv_benef_no_ext_in <- pv_benef_f(
  earnings_var = earnings_no_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)

pv_benef_yes_ext_in <- pv_benef_f(
  earnings_var = earnings_yes_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)
```

</details>
<br>

#### Assessing computational reproducibility of original results  

Without externalities, the original analysis (@baird2016worms) obtains a present value of benefits of 142.43 (table 5, column 3, and row 9). Including externalities, they obtain a present value of benefits of 766.81 (table 5, column 3, and row 12). Following the steps described in this section, this analysis obtains the same result (142.4258784 and 766.8143995 respectively without rounding).  





### Costs

The costs are a combination of direct costs of mass deworming (relative to the status quo, which is no subsidy for deworming) and indirect costs on the education system due to the additional time treated individuals spend in school.

<details><summary>Show all the details</summary>
\begin{equation}
C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)

\label{eq:9}
\tag{9}
\end{equation}

Where:

- $S_2$: per-capita costs of deworming under the deworming intervention  
- $S_1$: per-capita costs of deworming if the government does not provide any additional resources for deworming  
- $Q(S_2)$: take-up under a mass deworming intervention  
- $Q(S_1)$: take-up without additional resources from the government  
- $K$: cost per student to get education   
- $\Delta \overline{E}_{t}(S1, S2)$: estimated increase in school attendance  


```r
# - inputs: periods (periods_so), additional education (delta_ed_final_in),
#  discount rate (interest) (varies by approach), cost per student
#  (cost_per_student_in), cost per treatment (s2_in), take-up with treatment
#  (q2_in)
# - outputs: present value of all costs (pv_costs_f)
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

  pv_costs_f <- function(
    periods_var = periods_so,
    delta_ed_var = delta_ed_final_in,
    interest_r_var = NULL,
    cost_of_schooling_var = cost_per_student_in,
    s1_var = 0,
    q1_var = 0,
    s2_var = s2_in,
    q2_var = q2_in) {
        index_t <- 0:periods_var
        # Effects over 9 years of education (post treatment)
        delta_ed_s <- c(0, delta_ed_var, rep(0,41))
        (s2_var * q2_var  - s1_var * q1_var) +
          sum( ( 1 / (1 + interest_r_var) )^index_t *
                 delta_ed_s * cost_of_schooling_var)
    }

###############################################################################
###############################################################################  
    return(list("pv_costs_f" = pv_costs_f))    # Try to return only functions
}
invisible( list2env(chunk_cost2(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:  
```

</details>
<br>

#### Direct costs: increase in deworming costs

Direct deworming costs ($DC$) are defined as the take-up under a mass deworming intervention ($Q_{2}$), times the per-capita costs of deworming under the intervention ($S_{2}$). These costs are compared to a status quo scenario where the government does not provide any additional resources for deworming. This analysis assumes that there is no subsidy for deworming under the status quo.    

##### Complete subsidy to per capita costs of deworming

With complete subsidy, the relevant costs represent the total direct costs of deworming in USD. The take-up with full subsidy ($Q_2$) comes from a previous study [@kremer2007illusion] and takes the value of 0.75.

<details><summary>Show all the details</summary>

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\

\label{eq:10}
\tag{10}
\end{equation}


```r
# - inputs: unit costs in local currency (unit_cost_local_so), exchange rate
#  (ex_rate_so), years of treatment (years_of_treat_0_so)
# - outputs: unit costs of treatment (s2_f)
chunk_unit_costs2 <- function(){
###############################################################################
###############################################################################  

    s2_f <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     years_of_treat_var = years_of_treat_0_so) {
      ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    }

###############################################################################
###############################################################################  
    return(list("s2_f" = s2_f) )
}
invisible( list2env(chunk_unit_costs2(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
s2_in <- s2_f()
```

</details>
<br>


#### Indirect costs: additional years of education and its costs for government

As a result of deworming treatment, there is an estimated increase in school attendance, which is multiplied by the cost of education per student to calculate the additional indirect cost on the education system imposed by a treated individual. The additional costs on education are computed as follows: first compute a cost per student ($K$). This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher. Second, the cost per student is multiplied by the estimated increase in school attendance ($\Delta \overline{E}_{t}(S1,S2)$). For this the report uses a series of estimated effects, including the additional direct increase in secondary schooling from 1999 to 2007 obtained from an additional analysis related to @baird2016worms. This series does not take into account the externality effects. To incorporate externality effects, the report would need another series (from the same source) that estimates the additional secondary schooling increase due to the externality in order to add it to the original series.

<details><summary>Show all the details</summary>

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}

\label{eq:11}
\tag{11}
\end{equation}



```r
# - inputs: teacher salary (teach_sal_so) and benefits (teach_ben_so), number
# of students (n_students_so), include externalities (include_ext_so), extra ed
# without ext (delta_ed_so), and extra ed due to ext (delta_ed_ext_so)
# - outputs: cost per student (cost_per_student_f), and total additional
# education (delta_ed_final_f)
chunk_edcosts <- function(){
###############################################################################
###############################################################################    

    cost_per_student_f <- function(teach_sal_var = teach_sal_so,
                                    teach_ben_var = teach_ben_so,
                                    n_students_var = n_students_so) {
        (teach_sal_var + teach_ben_var) / n_students_var
    }

    delta_ed_final_f <- function(include_ext_var = include_ext_so,
                                 delta_ed_var = delta_ed_so,
                                 delta_ed_ext_var = delta_ed_ext_so){
        if (include_ext_var == TRUE){
            delta_ed_final_in <-  delta_ed_ext_var[,1] + delta_ed_var[,1]
        }else{
            delta_ed_final_in <- delta_ed_var[,1]
        }
        return(delta_ed_final_in)
    }

###############################################################################
###############################################################################  
    return(list("cost_per_student_f" = cost_per_student_f,
                "delta_ed_final_f" = delta_ed_final_f))
}
invisible( list2env(chunk_edcosts(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
cost_per_student_in <- cost_per_student_f()
delta_ed_final_no_ext_in <- delta_ed_final_f(include_ext_var = FALSE)
delta_ed_final_yes_ext_in <- delta_ed_final_f(include_ext_var = TRUE)

# Computing values for inline text:

# pv_costs_f
#  ├──── delta_ed_final_f
#  ├──── interest_f
#  └──── cost_per_student_f
#  |      └──── x
#  ├──── s2_f
#  └──── lambda2_in_f

pv_cost_no_ext_in <- pv_costs_f(
      periods_var = periods_so,
      delta_ed_var = delta_ed_final_no_ext_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_so
    )

pv_cost_yes_ext_in <- pv_costs_f(
      periods_var = periods_so,
      delta_ed_var = delta_ed_final_yes_ext_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_so
    )
```

</details>
<br>

#### Assessing computational reproducibility of original results  

Without externalities, the original analysis (@baird2016worms) obtains a present value of costs of 11.78 (table 5, column 3, and adding rows 6 and 3). Including externalities, they obtain a present value of benefits of  25.2 (table 5, column 3, and adding rows 6 and 3 and 7). Following the steps described in this section, this analysis obtains the same result (11.7761881 and 25.1962131 respectively without rounding).  





## Approach 2: @klps4

In this second approach, benefits follow the same principle as in approach 1 (increase in lifetime earnings), but it uses updated data on the effects on the labor market outcomes. Instead of projecting a trend of earnings into the future (after the estimated impact of the 10 year follow-up), this analysis uses additional data from 15 and 20 year follow-ups after the original intervention.  Costs are fairly similar to approach 1, with the addition that in this second approach, the costs also account for discounting of the several rounds of treatment required for effective deworming.  Additionally, the interest rate is updated to current values of return on (Kenyan) government bonds and inflation.


### Gains in earnings

Gains in earnings ($\Delta W_{t}$) from 10, 15, and 20 years after the intervention are used to measure the effect of multiple rounds of deworming on welfare over time. This is an important difference from approach 1, which only measures gains in earnings at year 10 and extrapolates them into the future. To extrapolate earnings after the 20-year measurement, the authors assume that the welfare gains disapear 25 years after the intervention. Hence the treatment effect over an individual's working life is the sum of the treatment effects over their working lifetime[^8]. This approach also disregards externality effects and measures the estimated effects directly on earnings (as opposed to approach 1 that measures effects on earnings indirectly through hours worked). The estimated treatment effects that pools years 10, 15, and 20, is $80 dollars per person per year.

Gains in yearly earnings represent the treatment effect on welfare ($\alpha^{pooled}$), which implicitly takes into consideration the life cycle profile of wages, economywide growth, etc. This estimation approach is an important strength, and it is preferred because it uses additional data and it requires fewer parametric assumptions than approach 2.1. 

[^8]: In another specification the authors assume that effects persist through the rest of an individual's working life. Here the report selects the specification that is most highlighted in the paper (most conservative specification). The authors also analyse the welfare effects over consumption, but given that they do not aggregate both outcomes in the welfare effect the report only chooses one and focus on earning for comparability with the approach 1).


<details><summary>Show all the details</summary>

\begin{equation}
\Delta W_{t} = \mathbf{1}(10 < t \leq 25)\alpha^{pooled}

\label{eq:12}
\tag{12}
\end{equation}

Where:

- $\Delta W_t$: gains in earnings from 10, 15, and 20 years after the intervention  
- $\alpha^{pooled}$: pooled estimated treatment effects of 10, 15, 20 years after the intervention



```r
# - inputs: index for time (t_var), pooled treatment effect (lambda1_new_so[1])
# - outputs: effect on lifetime earnings (earnings_app2_f)
chunk_new_earnings <- function(){
###############################################################################
###############################################################################  

    earnings_app2_f <- function(t_var = 1,
                            lambda1k1_var = lambda1_new_so[1]) {
        1*(10 <= t_var & t_var < 25) * lambda1k1_var
    }

###############################################################################
###############################################################################             
    return(list("earnings_app2_f" = earnings_app2_f))
}

invisible( list2env(chunk_new_earnings(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
earnings_no_ext_new_in <- earnings_app2_f(t_var = 0:50,
                                      lambda1k1_var = lambda1_new_so[1])
```

</details>
<br>




### Costs

#### Direct costs: increase in deworming costs

Similar to approach 1, the direct deworming costs under approach 2 are calculated by comparing the costs under a complete subsidy to the costs under the status quo of no subsidy. The two main differences with the previous cost estimates are 1) now the direct costs are summed and discounted over the treatment period, and 2) cost data has been updated after gathering more recent figures from Evidence Action.


<details><summary>Show all the details</summary>

\begin{equation}
DC = \sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:13}
\tag{13}
\end{equation}


Since the analysis is discrete and cannot sum over a non-integer, the following is found:

\begin{equation}
DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \\
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:14}
\tag{14}
\end{equation}

Where:

- $DC$: direct deworming costs   
- $r$: discounting rate, defined as the real interest rate  
- $S_2$: per-capita costs of deworming under the deworming intervention  
- $S_1$: per-capita costs of deworming if the government does not provide any additional resources for deworming  
- $Q(S_2)$: take-up under a mass deworming intervention  
- $Q(S_1)$: take-up without additional resources from the government  



```r
# - inputs: unit costs (unit_cost_local_so), exchange rate (ex_rate_so),
#  new interest rate (interest_new_in)
# - outputs: total unit costs (s2_new_f)
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

  s2_new_f <- function(
    unit_cost_local_var = unit_cost_local_so,
    ex_rate_var = ex_rate_so,
    interest_var = interest_new_in,
    year_of_treat_var = years_of_treat_t_so) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      periods_temp <- floor(year_of_treat_var)
      part_of_last_year_temp <- round(year_of_treat_var - periods_temp, 1)
      sum(
        ( unit_cost * (1 + interest_var)^(-(0:periods_temp)) ) *
            c(rep(1,periods_temp), part_of_last_year_temp)
        )
    }

###############################################################################
###############################################################################  
    return(list("s2_new_f" = s2_new_f) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
# New costs are all in dollars so, will compute them using ex rate of 1.
s2_new_in <- s2_new_f(
  interest_var = interest_new_in,
  unit_cost_local_var = unit_cost_2017usdppp_so,
  ex_rate_var = 1,
  year_of_treat_var = years_of_treat_t_so
)
q2_in <- q_full_so
```

</details>
<br>

With complete subsidy, the costs of the intervention become the total direct costs of deworming each child (in USD). The original study (@baird2016worms) identify the unit cost to be \$0.42 per year. Adjusting for purchasing power and inflation, the report gets a per capita cost of \$0.83. Adding all indirect cost over an average 2.4 years of treatment, the average cost of deworming each child over the entire treatment period is $1.44.


#### Indirect costs: additional years of education and its costs for government  

The indirect cost on the education system is calculated similarly to approach 1: the cost per student is multiplied by the increase in school attendance due to deworming. The cost of additional schooling is given by the product of the annual cost of schooling each child and the number of additional years children attend school as a result of deworming. This analysis assumes that pressure is added to educational institutions for a maximum of nine years, starting at year zero. The cost per student ($K$) is updated with new information on annual teacher salary (including benefits)[^9], $12,055 (also adjusted for PPP), and the same average number of students per teacher (45).

Hence, the cost of schooling each child for an additional year is now $267.9 (USD).

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: @nyanchama2018 and @oduor2017. Since compensation for teachers in rural villages where the treatment was administered is below the national average, the report is overestimating the costs for a conservative analysis. The average number of students per teacher is 45.


<details><summary>Show all the details</summary>

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)

\label{eq:15}
\tag{15}
\end{equation}

Where:

- $K$: cost per student to get education  
- $\Delta \overline{E}_{t}(S1, S2)$: estimated increase in school attendance  



</details>
<br>

Over this nine year period, treated students attended school for an additional 0.15 years on average. Then the report gets an average cost of additional schooling per child over the nine-year period, $32.40.


### Assessing computational reproducibility of original results  

The second approach does not report benefits and costs separatedly. With all these elements the main result from the original analysis that is comparable with the results discussed here is a NPV of 499.72 (table A12, column 3, and row 6) This result corresponds to a social internal rate of return of 40.7% located as an inline result in the paper - also in Figure 1 - and in the appendix at table A12, column 3, and row 9). Following the steps described in this section, this analysis obtains the same result (499.7204653 and 40.7492806546435% respectively without rounding).




## Approach 3: Combination of Previous Approaches and Input From Key Policy Partners

In this third and final approach, the report borrowed some methodological elements from @baird2016worms and @klps4 and sought feedback from a key policy partner to best identify one clear output to inform policy makers. BITSS worked in collaboration with the NGO Evidence Action, a key technical assistance partner in this area. Evidence Action provided insights on what are the most relevant costs and benefits from the perspectives of policy makers, and on certain aspects of the analysis that could be updated with present-day data.

Under this approach, the benefits from deworming described in Approaches 1 and 2 are scaled to reflect differences in prevalence rates, and length of treatment. Additionally, the relevant costs are constrained to direct costs alone (excluding additional costs on education). Finally this approach uses  cost and prevalence inputs that reflect the current settings where Evidence Action is currently supporting deworming interventions. As of 2020, Evidence Action supports deworming interventions in four countries.

### Benefits   


#### Adjusting for different prevalence rates  

To account for different prevalence rates ($\eta$), the estimated treatment effect is decomposed in the impact of deworming on children who were treated and had a worm infection, or the effective treatment effect of deworming ($\lambda_{1}^{eff}$), and children who were treated and did not have a worm infection. By construction, the effect on this last group should be zero. Hence the effective treatment of deworming on infected populations will be equal to the estimated treatment (on the ovearll population), divided by the proportion of the prevalence of infections.

In the original evaluation, the prevalence rates were very high (0.92), hence the effect on the infected population was similar to that of the overall population. Currently deworming interventions are often implemented in geographies with much lower prevalence rates (though in populations with sufficient infection to justify treatment in accordance with World Health Organization guidelines), hence to obtain the expected effect over the new region, the report needs to multiply the effect on the infected population by the prevalence rate in the new region ($\eta_{new}$).


<details><summary>Show all the details</summary>

For approach 3, the report will modify treatment effects of approaches 1 and 2 (equation 4 and 13 respectively) by the following:   

\begin{equation}
\lambda_{1} = \eta \lambda^{eff}_{1} + (1 -  \eta) \times 0 \\
\lambda^{r}_{1} = \eta_{new}\lambda^{eff}_{1}

\label{eq:16}
\tag{16}
\end{equation}

Where:

- $\lambda_1$: direct effects of deworming on individuals' earnings. Here the report uses the symbol for treatment effect of approach 1, but the same logic applies to the treatment effect of approach 2 ($\alpha^{pooled}$)
- $\lambda^{eff}_1$: impact of deworming on children who were treated and had a worm infection in the original evaluation  
- $\lambda^r_1$: impact of deworming on children who were treated and had a worm infection in the new region  
- $\eta$: prevalence rates in the original evaluation   
- $\eta_{new}$: prevalence rates in the new region   


```r
# - inputs: previously estimated treatment effect (lambda1_in_f), prevalence
# rates in the original setting (prevalence_0_so), prevalence in the new setting
# (prevalence_r_so), countries included in the analysis (country_sel_so)
# and their population (country_sel_pop_so), or single input of new prevalence
# (new_prevalence_r_so)
# - outputs: effective treatment effect (lambda_eff_f)
chunk_lambdas_eff<- function(){
###############################################################################
###############################################################################    

    lambda_eff_f <- function(lambda1_var = lambda1_in_f(),
                           prevalence_0_var = prevalence_0_so,
                           prevalence_r_var = prevalence_r_so,
                           country_sel_var = country_sel_so,
                           country_sel_pop_var = country_sel_pop_so,
                           other_prevl_r_var = new_prevalence_r_so){
      temp_sel <- as.character(country_sel_var)  
      # if a positive number of countries is selected
      if (is.null(other_prevl_r_var)) {
        temp_weights <- country_sel_pop_var[temp_sel] /
          sum(country_sel_pop_var[temp_sel])
        prevalence_r_final <- sum( prevalence_r_var[temp_sel] * temp_weights )
      } else {
        prevalence_r_final <- other_prevl_r_var  
      }
      lambda1_eff_temp <- lambda1_var / prevalence_0_var
      lambda1_eff_in <- lambda1_eff_temp * prevalence_r_final
      return(  
        list("lambda1_eff_in" = lambda1_eff_in,
             "prevalence_r_final_in" = prevalence_r_final)
              )
    }  

###############################################################################
###############################################################################  
    return( list("lambda_eff_f" = lambda_eff_f) )
}
invisible( list2env(chunk_lambdas_eff(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_r_in <- lambda_eff_f()$lambda1_eff_in
prevalence_r_in <- lambda_eff_f()$prevalence_r_final_in
```

</details>
<br>

Evidence Action provided prevalence survey data for the geographies where they are involved. In order to be most analogous with the baseline prevalence estimate used in the original study, the prevalence estimates used are 1) the earliest point estimates available from before, or close to the time of, Evidence Action's involvement in that geography, and 2) are representative of any STH infection.      

#### Adjusting for different length of treatment  

The number of consecutive years over which a population is exposed to deworming treatment determines the intensity of the effects over this population over time. The two approaches reproduced so far hold the length of treatment constant at the levels estimated by the original study (2.4 years). In this third approach the report allows for the years of treatment to vary affecting both benefits and costs. The report assumes that the effects are linear in the number of years of treatment, with no additional effects after 6 years of treatment. The report assumed a maximum of 6 years of impact in this case based on the 20 year KLPS follow-up research, which shows a levelling-off of treatment effect after approximately 6 years of deworming (@klps4; Figure A.5 in Appendix, page A-6).

Adding the element of treatment duration allows us to take into account differences in the number of years of deworming treatment across different country contexts depending on program dynamics. Although the counterfactual of worm prevalence in the absence of treatment is largely unknown, it is known that consistent deworming continues to decrease worm prevalence over time, contributing to controlled worm environments and sustained benefits. In many deworming programs today, children receive regular treatment throughout a portion (and in some cases for the full term) of their primary schooling. It is worth noting that the assumption of linearity is an imperfect measure for various epidemiological reasons, though the report includes this variable of time into the equation as an estimate of the best guess at the differences in achieved impact over time, and in part because it helps capture that a new cohort enters primary school--and is therefore eligible for treatment--with each successive year of a deworming program.

<details><summary>Show all the details</summary>

For approach 3, treatment effects of approaches 1 and 2 (equations 4 and 13 respectively) will be modified by the following:   

\begin{equation}
\lambda_{1,t = 1} = \frac{\lambda_{1}}{L_{0}} \\
\lambda_{1,t} =
\begin{cases}
t \lambda_{1,t = 1} \quad \text{for } t=1, \dots, 6\\
\\
6  \lambda_{1,t = 1} \quad \text{for } t > 6\\
\end{cases}

\label{eq:17}
\tag{17}
\end{equation}



```r
# - inputs: treatment effect (lambda1_in_f), length of treatment in original
# study (years_of_treat_0_so), length of treatment in new setting (years_of_treat_t_so)
# - outputs: per year treatment effect (lambda1_t1) and total treatment effect
# (lambda1_t)

chunk_lambdas_t<- function(){
###############################################################################
###############################################################################    

    lambda_t_f <- function(lambda1_var = lambda1_in_f(),
                           years_of_treat_0_var = years_of_treat_0_so,
                           years_of_treat_t_var = years_of_treat_0_so){
          lambda1_t1 <- lambda1_var / years_of_treat_0_var
          if (years_of_treat_t_var<=6){
            lambda1_t <- years_of_treat_t_var * lambda1_t1
          } else if  (years_of_treat_t_var>6) {
            lambda1_t <- 6 * lambda1_t1
          }
          return(
            list(
              "lambda1_t1" = lambda1_t1,
              "lambda1_t" = lambda1_t)
            )
    }  

###############################################################################
###############################################################################  
    return( list("lambda_t_f" = lambda_t_f) )
}
invisible( list2env(chunk_lambdas_t(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:

# # earnings_app1_f
#  ├──── delta_ed_final_f
#  ├──── interest_f
#  └──── lambda_eff_f
#  |      └──── lambda_t_f
#  |            └──── lambda_in_f
#  ├──── saturation_f
#  └──── wage_t_f
#         └──── wage_0_f

lambda1_t_in <- lambda_eff_f(
  lambda1_var = lambda_t_f(
    lambda1_var = lambda1_in_f(),
    years_of_treat_0_var =  years_of_treat_0_so,
    years_of_treat_t_var =  years_of_treat_t_so
  )$lambda1_t,
  prevalence_0_var = prevalence_0_so,
  country_sel_var = list("india", "kenya", "nigeria", "vietnam"),
  other_prevl_r_var = NULL
)$lambda1_eff_in


app3_earnings_no_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_t_in[1],
  saturation_var = saturation_in,
  lambda2_var = 0,
  coverage_var = coverage_so
)

app3_earnings_yes_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_t_in[1],
  saturation_var = saturation_in,
  lambda2_var = lambda2_in[1],
  coverage_var = coverage_so
)

app3_pv_benef_no_ext_in <- pv_benef_f(
  earnings_var = app3_earnings_no_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)

app3_pv_benef_yes_ext_in <- pv_benef_f(
  earnings_var = app3_earnings_yes_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)


lambda1_t_new_in <- lambda_eff_f(
  lambda1_var = lambda_t_f(
    lambda1_var = lambda1_new_so[1],
    years_of_treat_0_var =  years_of_treat_0_so,
    years_of_treat_t_var =  years_of_treat_t_so
  )$lambda1_t,
  prevalence_0_var = prevalence_0_so,
  country_sel_var = list("india", "kenya", "nigeria", "vietnam"),
  other_prevl_r_var = NULL
)$lambda1_eff_in


earnings_no_ext_new_in<- earnings_app2_f(t_var = 0:50,
                                      lambda1k1_var = lambda1_t_new_in)

app3_pv_benef_all_new_in <- pv_benef_f(earnings_var = earnings_no_ext_new_in,
                                interest_r_var = interest_new_in,
                                periods_var = periods_so)
```


</details>

<!--
Now the benefits are flexible to worm prevalence and lenght of treatment. To facilitate comparison with the other two approaches, the report presents here the results using the same prevalence and length of treatment assumptions parameters as in approach 1 and 2. Both approaches implicitly assume prevalence rates of 100% and do not distinguish between original population and target populuation. Both approaches also set the length of treatment at 2.41 years.
-->

To compute the benefits for this approach, we use data on prevalence and length of treatment from the four countries for which Evidence Action has records. Readers interested in assessing the effects of deworming for a specific value of prevalence and length of treatment are referred to the [interactive app](https://fhoces.shinyapps.io/shiny_app_test/) (tab on key assumptions) where they can input the values that best reflect their setting. To facilitate comparison with the other two approaches, we present here the results using the same length of treatment assumptions parameters as in approach 1 and 2.   

Under approach 3, and using the same assumptions as above, the benefits will be: 77.61 and 702 when using benefits of approach 1 without and with externalities, and 289.9 when using the benefit structure of approach 2.  

### Costs

Evidence Action's Deworm the World Initiative provides technical assistance to governments to implement school-based deworming programs. Deworm the World works closely with policymakers and government staff who are responsible for ensuring the implementation of deworming programs within their geographies to plan, scale, and sustain school-based deworming programs targeting at-risk children. Deworm the World works to gain and maintain critical support amongst these key stakeholders, thus having important influence over how policymakers take-in and use evidence for decision making. Through Evidence Action's technical assistance, which typically includes financial support for program implementation, they have access to country-level government cost data on what it takes to implement and evaluate school-based deworming programs across different contexts. To estimate the costs in this analysis, the report uses costs of deworming provided by Evidence Action (detailed below) and follow a similar approach to @givewell, which takes those costs and includes an additional estimate around the amount of government staff time required to run deworming programs. The default cost is the per unit cost per treatment round per child across all countries. This is obtained as the weighted average of per unit costs ($c_{i}$) in all countries where Evidence Action currently has data on implementation of deworming interventions [^10].

[^10]: In some settings Evidence Action provides two rounds of treatment per year. In those cases, the unit costs discussed here represent the sum of both rounds

Costs per country include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such as the cost of drugs donated by WHO. These items include: drug procurement and management, monitoring and evaluation, policy and advocacy, prevalence surveys, program management, public mobilization/community sensitization, and training and distribution. Costs can vary by geography due to factors of population size, treatment strategies, age of the program, and costs of "doing business."

The country weights are computed as the fraction of all treated individuals that correspond to a given country. The per capita cost of each country is obtained by dividing the country's total costs by the total number of treated individuals in a given period. Total costs for a country represent the total cost across country regions faced by three different payers: Evidence Action, country governments, and other partners.  


<details><summary>Show all the details</summary>

\begin{equation}
C = \sum_{i \in Countries } \omega_{i} c_{i}

\label{eq:18}
\tag{18}
\end{equation}

\begin{equation}
\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \\
c_{i} = \frac{C_{i}}{N_{i}} \\

\label{eq:19}
\tag{19}
\end{equation}

\begin{equation}
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \\
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}
\end{equation}

Where:  

- $C$: weighted average of per unit costs in all countries  
- $c_i$: per unit costs in different countries  
- $\omega_i$: country weights for computing the costs   
- $N$: the number of all treated individuals   
- $C_{i,k}$: costs of a country at a specific payer level  
- $\delta_g$: additional government staff time required to implement a typical deworming intervention


```r
# - inputs: cost data by payer type at the contry/province level by year (df_costs_so)
#  crosswalk between country/state and region (df_costs_cw_so), treatment counts
#  by country/province and year (df_counts_so); staff time adjusment factor
#  (staff_time_so),
# - outputs: country level cost and population data (costs1_p1_f) and country
#  weights and per capita costs (costs1_p2_f)
#
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
  # clean and aggreagate data at country level
  costs1_p1_f <- function(
    df_costs_var = df_costs_so,
    df_costs_cw_var = df_costs_cw_so,
    df_counts_var = df_counts_so) {
    ## Counts
    # Data cleanning:
    # Add country variable
    # anoying message: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override

    df_counts_temp <- df_costs_cw_var %>%
      right_join(df_counts_var, by = "Country/State") %>%
      mutate(Country = tolower(Country))
    # keep only last year on record
    suppressMessages(            
        df_counts_last <- df_counts_temp %>%
          group_by(Country) %>%
          summarise("last_year" = max(Year)) %>%
          right_join(df_counts_temp, by = "Country") %>%
          filter(Year == last_year)          
    )
    # compute counts as the sum with-in country-year of treatments
    suppressMessages(            
        c_counts <- df_counts_last %>%
          group_by(Country, Year) %>%
          summarise("total" = sum(`# dewormed`))
    )

    ## Costs
    # Data cleaning:
    # Add country variable
    df_costs_temp <- df_costs_cw_var %>%
      right_join(df_costs_var, by = "Country/State") %>%
      select(-Country.y) %>% rename(Country = Country.x) %>%
      mutate(Country = tolower(Country))
    # values for last year with cost information
    suppressMessages(            
    df_costs_last <- df_costs_temp %>%
      group_by(Country) %>%
      summarise("last_year" = max(Year)) %>%
      right_join(df_costs_temp, by = "Country") %>%
      filter(Year == last_year)    
      )      
     # summing across payers and regions (last equation)
    suppressMessages(            
    costs_by_payer <- df_costs_last %>%
      filter(Payer != "Total") %>%
      group_by(Country, Payer) %>%
      summarise("costs_by_payer" =
                  sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE))
    )    
    #sum across payers
    suppressMessages(            
    country_cost <- costs_by_payer %>%
      group_by(Country) %>%
      summarise("costs_by_country" =
                  sum(costs_by_payer) )  
    # Compute the per capita cost for each country (c_i and w_i)
    )
    costs_data_in <- country_cost %>%
       left_join(c_counts, by = "Country")

    return( costs_data_in )
  }

  # Compute weights and per capta costs
  costs1_p2_f <- function(country_total_var = costs_data_in$total,
                         country_cost_var = costs_data_in$costs_by_country,
                         staff_time_var = staff_time_so,
                         country_name_var = costs_data_in$Country,
                         select_var = list("india", "kenya", "nigeria",
                                           "vietnam"),
                         other_costs_var = NULL) {
      # select countries
      country_total_var_temp <- country_total_var[country_name_var %in% select_var]
      country_cost_var_temp <- country_cost_var[country_name_var %in% select_var]
      # create country weight
      c_weights <- country_total_var_temp / sum(country_total_var_temp)
      # create country per capita costs, adjusted by staff time
      per_cap <- country_cost_var_temp * (1 + staff_time_var) /
        country_total_var_temp
      # replace contry costs with new one if there is a new country
      # (only count that new country)
      # (the weighthed sum of this scalar will just be the same number)
      if (!is.null(other_costs_var)) {
      # if (FALSE) {  
        per_cap <- other_costs_var * (1 + staff_time_var)
      }
      return( sum(c_weights * per_cap) )
    }
###############################################################################
###############################################################################  
    return( list("costs1_p1_f" = costs1_p1_f,
                 "costs1_p2_f" = costs1_p2_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
costs_data_in <- costs1_p1_f()
costs1_p2_in <- costs1_p2_f(select_var = list("india", "kenya", "nigeria",
                                              "vietnam"))
```

</details>

The unit costs of treatments, although small, vary substantially across regions. When including cost information for all the countries where Evidence action has data (India, Kenya, Nigeria, Vietnam) the unit costs is $0.08 per round of treatment. This final cost is primarily driven by the cost, and large population, of India, with a unit cost of $0.06, the other 3 remaining countries have relatively larger unit costs: $0.54, $0.86, $0.52 for Kenya, Nigeria and Vietnam respectively.





## Accounting for Uncertainty

This open policy analysis has aimed to make all the analysis presented so far highly reproducible. One direct result of this novel approach is that now it is possible to thoroughly assess how the final policy estimates change when any of the underlying sources of the analysis changes. This report has identified each source used in the analysis behind benefits and costs of Deworming interventions. Each of these sources in turn is measured with some uncertainty (either in prediction of future values or estimation of past ones). Traditional policy analysis assumes that each of these sources has no uncertainty, and in some cases incorporates uncertainty or performed sensitivity analysis for a few parameters of interest. By following the open policy analysis principles the report now can allow for each source to vary and explore the overall uncertainty of the final policy estimate.

Our approach consists in assuming that each source used in the analysis can be represented as a random draw from a normal distribution. The mean corresponds to the measured value. The standard deviation corresponds to the estimated standard error when available, and to a fraction of the mean when not available. As a default analysis, these standard deviations are suggested to be set to 10% of the mean. This choice is arbitrary, but unlike the default arbitrary choice of setting the standard deviations to zero, it makes explicit the uncertainty and it can be modified in the app.


<details><summary>Show all the details</summary>

Let $x$ denote each source used in this analysis.  

\begin{equation}
x \sim N(\hat{x}, \sigma_{x})

\label{eq:20}
\tag{20}
\\
\sigma_{x} =
\begin{cases}
\hat{\sigma_{x}} \quad \text{If $\hat{\sigma_{x}}$ is available}\\
\\
\delta_{u}\hat{x} \quad \text{otherwise}
\end{cases}
\end{equation}

As a default $\delta_{u} = 0.1$


```r
# This function takes as inputs means and standard deviations of source
# parameters and simualte draws of each source. When the source is a scalar,
# it generates a draw from a noromal dist (mean, sd). When it is a "small"
# (less than 4 elements) vector, generates independent multivariate normals.


#begin by cleaning up the cost data once
costs_data_in <- costs1_p1_f(df_costs_var = df_costs_so,
                          df_costs_cw_var = df_costs_cw_so,
                          df_counts_var = df_counts_so)


# Data: source comes from a standard data source. Government statistic or other
# publicly available statistic
# Research: any sources that requieres some type of investigation to obtain
# Guesswork: no clear source available

sim_data1_f <- function(nsims_var2 = 1e2,                   # "Setup" vars
                      main_run_var2,
                      periods_var2,
                      costs_data_var2 = costs_data_in,
                      run_sim_var2,
                      countries_var2,

                      ex_rate_var2,                  # "Data" vars
                      ex_rate_sd_var2,
                      growth_rate_var2,
                      growth_rate_sd_var2,
                      gov_bonds_var2,
                      gov_bonds_sd_var2,
                      gov_bonds_new_var2,                                                              
                      gov_bonds_new_sd_var2,                                                          
                      inflation_var2,
                      inflation_sd_var2,
                      inflation_new_var2,                          
                      inflation_new_sd_var2,                      
                      tax_var2,
                      tax_sd_var2,

                      lambda1_var2,                  # "Research" vars
                      lambda1_sd_var2,
                      lambda1_new_var2,
                      lambda1_new_sd_var2,
                      lambda2_var2,
                      lambda2_sd_var2,
                      wage_ag_var2,                 
                      wage_ag_sd_var2,
                      wage_ww_var2,
                      wage_ww_sd_var2,
                      profits_se_var2,
                      profits_se_sd_var2,
                      hours_se_cond_var2,
                      hours_se_cond_sd_var2,
                      hours_ag_var2,
                      hours_ag_sd_var2,
                      hours_ww_var2,
                      hours_ww_sd_var2,
                      hours_se_var2,
                      hours_se_sd_var2,
                      coef_exp_var2,         # sd for coef_exp is hard coded
                      prevalence_0_var2,
                      prevalence_0_sd_var2,
                      prevalence_r_var2,
                      prevalence_r_sd_var2,
                      new_prevl_r_var2,       # substitudes the prev_r above??
                      new_prevl_r_sd_var2,
                      coverage_var2,
                      coverage_sd_var2,
                      q_full_var2,
                      q_full_sd_var2,
                      q_zero_var2,
                      q_zero_sd_var2,
                      delta_ed_var2,
                      delta_ed_sd_var2,
                      delta_ed_ext_var2,
                      delta_ed_ext_sd_var2,
                      teach_sal_var2,
                      teach_sal_sd_var2,
                      teach_ben_var2,
                      teach_ben_sd_var2,
                      teach_sal_new_var2,
                      teach_sal_new_sd_var2,
                      teach_ben_new_var2,
                      teach_ben_new_sd_var2,
                      n_students_var2,
                      n_students_sd_var2,
                      years_of_treat_0_var2,
                      years_of_treat_0_sd_var2,
                      years_of_treat_t_var2,
                      years_of_treat_t_sd_var2,
                      unit_cost_local_var2,
                      unit_cost_local_sd_var2,
                      unit_cost_local_new_var2,
                      unit_cost_local_new_sd_var2,
                      costs_par_var2,
                      costs_par_sd_var2,
                      counts_par_var2,
                      counts_par_sd_var2,
                      staff_time_var2,      # Guesswork
                      staff_time_sd_var2,
                      new_costs_var2,
                      new_costs_sd_var2
                      ) {
    start_time <- Sys.time()
    ################
    ###### Draws
    ################
    set.seed(142857)
    #Default dist: normal, default sd: 0.1* mean
    #
    # Sources are separated into: data, research and guess work
    ## Data
    gov_bonds_sim <-        rnorm(n = nsims_var2, mean = gov_bonds_var2,
                                  sd = gov_bonds_sd_var2)
    inflation_sim <-        rnorm(nsims_var2, inflation_var2,
                                  inflation_sd_var2)
    gov_bonds_new_sim <-    rnorm(n = nsims_var2, mean = gov_bonds_new_var2,
                                  sd = gov_bonds_new_sd_var2)
    inflation_new_sim <-    rnorm(nsims_var2, inflation_new_var2,
                                  inflation_new_sd_var2)                  
    growth_rate_sim <-      rnorm(nsims_var2, growth_rate_var2, growth_rate_sd_var2)
    ex_rate_sim <-          rnorm(nsims_var2, ex_rate_var2, ex_rate_sd_var2)
    tax_sim <-              rnorm(nsims_var2, tax_var2, tax_sd_var2)

    ## Research
    aux1 <-0.1 * c(lambda1_var2[1], 0.01)
    # Each list is a pair mean, sd.
    aux2 <-  lapply(1:2, function(x) c(lambda1_var2[x], c(1.42, 1.36)[x] ) )
    lambda1_sim <- sapply(aux2,
                          function(x)  rnorm(nsims_var2, mean = x[1], sd = x[2]) )
    lambda2_sim <-          rnorm(nsims_var2, lambda2_var2,  lambda2_sd_var2)
    # New lambda here
    lambda1_new_sim <- rnorm(nsims_var2, lambda1_new_var2,  lambda1_new_sd_var2)

    wage_ag_sim <-          rnorm(nsims_var2, wage_ag_var2, wage_ag_sd_var2)
    wage_ww_sim <-          rnorm(nsims_var2, wage_ww_var2, wage_ww_sd_var2)
    profits_se_sim <-       rnorm(nsims_var2, profits_se_var2, profits_se_sd_var2)
    hours_se_cond_sim <-    rnorm(nsims_var2, hours_se_cond_var2,
                                  hours_se_cond_sd_var2)
    hours_ag_sim <-         rnorm(nsims_var2, hours_ag_var2, hours_ag_sd_var2)
    hours_ww_sim <-         rnorm(nsims_var2, hours_ww_var2, hours_ww_sd_var2)
    hours_se_sim <-         rnorm(nsims_var2, hours_se_var2, hours_se_sd_var2)
    coverage_sim <-         rnorm(nsims_var2, coverage_var2, coverage_sd_var2)

    unit_cost_local_sim <-  rnorm(nsims_var2, unit_cost_local_var2,
                                  unit_cost_local_sd_var2)
    unit_cost_local_new_sim <-  rnorm(nsims_var2, unit_cost_local_new_var2,
                              unit_cost_local_new_sd_var2)

    years_of_treat_0_sim <-   rnorm(nsims_var2, years_of_treat_0_var2,
                                  years_of_treat_0_sd_var2)
    years_of_treat_t_sim <-   rnorm(nsims_var2, years_of_treat_t_var2,
                                  years_of_treat_t_sd_var2)

    q_full_sim <-           rnorm(nsims_var2, q_full_var2, q_full_sd_var2)
    q_zero_sim <-           rnorm(nsims_var2, q_zero_var2, q_zero_sd_var2)

    # Prevalence here TO DO: draw from a beta instead of "truncated" normal
    prevalence_0_sim <- rnorm(nsims_var2, prevalence_0_var2, prevalence_0_sd_var2)
    prevalence_0_sim <- ifelse(
      prevalence_0_sim > 1,
      yes = 1,
      no = ifelse(prevalence_0_sim < 0, yes = 0, no = prevalence_0_sim)
    )

    aux4 <- lapply(countries_var2, #will have trouble when selecting no countries
                   function(x) c(prevalence_r_so[x],
                                 prevalence_r_so[x]) )

    # first draw samples of prevalence for each country
    prevalence_r_sim <- sapply(aux4,
                                function(x)
                                  rnorm(
                                    nsims_var2,
                                    mean = x[1] * prevalence_r_var2,
                                    sd = x[2] * prevalence_r_sd_var2
                                  ))
    prevalence_r_sim <- ifelse(
      prevalence_r_sim > 1,
      yes = 1,
      no = ifelse(prevalence_r_sim < 0, yes = 0, no = prevalence_r_sim)
    )
    colnames(prevalence_r_sim) <- as.character(countries_var2)  

    # if there is a new entry of prevalence, draw from it. If there is not
    # then leave as null
    if (!is.null(new_prevl_r_var2)){
          new_prevl_r_sim <- rnorm(nsims_var2, new_prevl_r_var2, new_prevl_r_sd_var2)
          new_prevl_r_sim <- ifelse(
            new_prevl_r_sim > 1,
            yes = 1,
            no = ifelse(new_prevl_r_sim < 0, 0, new_prevl_r_sim)
          )
    } else if (is.null(new_prevl_r_var2)){
          new_prevl_r_sim <- NULL
    }
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims_var2, mean = x[1],
                                                    sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims_var2, teach_sal_var2, teach_sal_sd_var2)
    teach_ben_sim <-    rnorm(nsims_var2, teach_ben_var2, teach_ben_sd_var2)

    teach_sal_new_sim <-    rnorm(nsims_var2, teach_sal_new_var2,
                                  teach_sal_new_sd_var2)
    teach_ben_new_sim <-    rnorm(nsims_var2, teach_ben_new_var2,
                                  teach_ben_new_sd_var2)

    n_students_sim <-   rnorm(nsims_var2, n_students_var2, n_students_sd_var2)
    # TO DO: modify to have a scalar multlying the series, and have that
    # scalar being N(1,0.1)
    delta_ed_sim <- sapply(delta_ed_so[,1],
                           function(x) rnorm(
                             nsims_var2,
                             mean = x * delta_ed_var2,
                             sd = delta_ed_sd_var2 * sd(delta_ed_so[, 1])) )
    colnames(delta_ed_sim) <- 1999:2007
    # modify to have a scalar multlying the series, and have that scalar
    # being N(1,0.1)
    delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1],
                               function(x)  {
                                 rnorm(
                                   nsims_var2,
                                   mean = x * delta_ed_ext_var2,
                                   sd =  sd(delta_ed_ext_so[, 1]) *
                                     delta_ed_ext_sd_var2
                                 )
                                 }
                               )
    colnames(delta_ed_ext_sim) <- 1999:2007

    counts_in <- costs_data_var2$total
    costs_no_staff_in <- costs_data_var2$costs_by_country

    # drawing samples form counts
    costs1_counts_sim <- sapply(counts_in,
                                function(x)  rnorm(nsims_var2,
                                                   mean = x * counts_par_var2,  
                                                   sd = x * counts_par_sd_var2)
                                )
    # drawing samples from costs
    costs1_all_costs_sim <- sapply(costs_no_staff_in,
                                function(x)  rnorm(nsims_var2,
                                                   mean = x * costs_par_var2,  
                                                   sd = x * costs_par_sd_var2)
                                )

    #computing unit cost for each simulation draw
    costs1_df_sim <- NULL

    #building "nsims_var2" simulated data sets (corresponding to costs_data_in)
    for (aux1_i in 1:nsims_var2){
      costs1_df_sim[[aux1_i]] <- data.frame(
        "Country" = costs_data_var2$Country,
        "total" = costs1_counts_sim[aux1_i,],
        "costs_by_country" = costs1_all_costs_sim[aux1_i,]
        )
    }
    temp_cost_sim <- rnorm(nsims_var2,
                           mean = new_costs_var2,
                           sd = new_costs_sd_var2)
    ## Guess work
    # drawing samples from staff time
    staff_time_sim <- rnorm(nsims_var2, staff_time_var2, staff_time_sd_var2)      
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #periods until individual join the labor force

    ######    
    ######    

    ################
    ###### Runs    
    ################

    #Vectors to store the results of each simulation
    a1_tax_sim           <- rep(NA, nsims_var2) #a1_tax_pe
    a1_x_tax_sim         <- rep(NA, nsims_var2) #a1_x_tax_pe
    a1_all_sim           <- rep(NA, nsims_var2) #a1_all_pe
    a1_x_all_sim         <- rep(NA, nsims_var2) #a1_x_all_pe
    a2_tax_sim           <- rep(NA, nsims_var2) #a2_tax
    a2_all_sim           <- rep(NA, nsims_var2) #a2_all
    a3_inc_a1_all_sim    <- rep(NA, nsims_var2) #a3_inc_a1_all
    a3_inc_a1_all_x_sim  <- rep(NA, nsims_var2) #a3_inc_a1_all_x
    a3_inc_a2_all_sim    <- rep(NA, nsims_var2) #a3_inc_a2_all_mpe

    for (i in 1:nsims_var2) {
    # one_run_f, for the most part, does not include standard deviations   
      invisible( list2env(
        one_run_f(main_run_var1 = FALSE,              
                run_sim_var1 = TRUE,
                wage_ag_var1 = wage_ag_sim[i],
                wage_ww_var1 = wage_ww_sim[i],
                profits_se_var1 = profits_se_sim[i],
                hours_se_cond_var1 = hours_se_cond_sim[i],
                hours_ag_var1 = hours_ag_sim[i],
                hours_ww_var1 = hours_ww_sim[i],
                hours_se_var1 = hours_se_sim[i],
                ex_rate_var1 = ex_rate_sim[i],
                growth_rate_var1 = growth_rate_sim[i],
                coef_exp_var1 = coef_exp_sim[i, 1], coef_exp2_var1 = coef_exp_sim[i,2],
                lambda1_var1 = lambda1_in_f(lambda1_var = lambda1_sim[i,]),
                prevalence_0_var1 = prevalence_0_sim[i],
                prevalence_r_var1 = prevalence_r_sim[i, ],
                new_prevl_r_var1 = new_prevl_r_sim[i],
                lambda2_var1 = lambda2_sim[i],
                coverage_var1 = coverage_sim[i],
                q_full_var1 = q_full_sim[i],
                q_zero_var1 = q_zero_sim[i],
                lambda1_new_var1 = lambda1_new_sim[i],
                gov_bonds_var1 = gov_bonds_sim[i],
                inflation_var1 = inflation_sim[i],
                gov_bonds_new_var1 = gov_bonds_new_sim[i],
                inflation_new_var1 = inflation_new_sim[i],
                delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                teach_sal_var1 = teach_sal_sim[i],
                teach_ben_var1 = teach_ben_sim[i],
                teach_sal_new_var1 = teach_sal_new_sim[i],
                teach_ben_new_var1 = teach_ben_new_sim[i],
                n_students_var1 = n_students_sim[i],
                unit_cost_local_var1 = unit_cost_local_sim[i],
                unit_cost_local_new_var1 = unit_cost_local_new_sim[i],
                years_of_treat_0_var1 = years_of_treat_0_sim[i],
                years_of_treat_t_var1 = years_of_treat_t_sim[i],
                tax_var1 = tax_sim[i],
                periods_var1 = periods_so,
                df_costs_var1 = costs1_df_sim[[i]],
                new_costs_var1 = temp_cost_sim[i],    
                staff_time_var1 = staff_time_sim[i],
                countries_var1 = countries_var2
                ),.GlobalEnv) ) # add costs here
      #Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
      a1_tax_sim[i] <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
      #Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
      a1_x_tax_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_x_in)
      # Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
      a1_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
      # Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
      a1_x_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_x_in)
      #KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
      a2_tax_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_new_in, costs_var = costs_a2_in)
      #KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
      a2_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new_in, costs_var = costs_a2_in)
      # EA1: no externality NPV using EAs costs
      a3_inc_a1_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_prevl_in, costs_var = costs2_ea_in)
      # EA2: yes externality NPV using EAs costs
      a3_inc_a1_all_x_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_prevl_in, costs_var = costs2_ea_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      a3_inc_a2_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_prevl_new_in, costs_var = costs2_ea_in)
    }

    total_time_sim <- Sys.time() - start_time

    ######    
    ######     
    return( list(
      "a1_tax_sim"          = a1_tax_sim,         
      "a1_x_tax_sim"        = a1_x_tax_sim,         
      "a1_all_sim"          = a1_all_sim,         
      "a1_x_all_sim"        = a1_x_all_sim,         
      "a2_tax_sim"          = a2_tax_sim,        
      "a2_all_sim"          = a2_all_sim,        
      "a3_inc_a1_all_sim"   = a3_inc_a1_all_sim,            
      "a3_inc_a1_all_x_sim" = a3_inc_a1_all_x_sim,            
      "a3_inc_a2_all_sim"   = a3_inc_a2_all_sim,            
      "total_time_sim"      = total_time_sim
    ) )
}

policy_estimates_varnames <- c(
  "a1_tax_sim",
  "a1_x_tax_sim",
  "a1_all_sim",
  "a1_x_all_sim",
  "a2_tax_sim",
  "a2_all_sim",
  "a3_inc_a1_all_sim",
  "a3_inc_a1_all_x_sim",
  "a3_inc_a2_all_sim"            
)

policy_estimates_text <- c(
  "A1. Tax revenue",
  "A1. With externalities. Tax",
  "A1. All income",
  "A1. With ext. All income",
  "A2. Tax",
  "A2. All income",
  "A3. All income of A1",
  "A3. All income of A1, with ext.",
  "A3. All income of A2. Main Policy Estimate"
  )
```

</details>

# Main Results  

In this document the report has presented three different approaches to measuring the welfare effects of deworming  interventions. The first approach was based on the original paper that measured the welfare effects of deworming (@baird2016worms) and proposed four different ways to compute this effect (with and without externalities, and from a societal or fiscal perspective). The second approach, based on more recent data, focused only on direct effects, and relies less on predictive effects over the lifecycle. Results for the second approach are also separated between the societal and fiscal perspective.   

The third and final approach uses similar methodologies with three main differences. First, the report allows the benefits to be scaled to account for differences in the prevalence of worm infections in settings different from the original study. Second, the report allows the benefits to be scaled by the length of treatment provided to children within a particular setting. Finally, based on feedback from Evidence Action on the relevant costs from present-day deworming programs, this approach uses more up to date information on treatment costs and it does not take into account the knock-on effects of additional schooling costs as a result of increased school attendance, which are accounted for in approaches #1 and #2[^11].

[^11]: Evidence Action suggests that the added costs on education will not be considered as costs from a policy makers perspective. Those costs corresponds to another intervention on itself (education) and incorporating its costs would also require to incorporate its benefits. 




The table below summarises the three different approaches and the different alternatives within each approach. The main policy estimate is defined as that of Evidence Action (approach 3) using the latest research (@klps4): approach 3.3 in the table (in bold).

<details><summary>Show all the details</summary>


```r
# TODO: Wrap this code chunk in chunk_xxxfunction

#chunk_runvalues <- function(){
# Function dependency is depicted as follows:
# f(g()) =
# f
# └──── g
#
#       ##     ###    ####    #####
# 1     2       3     4       5
#       ##     ###    ####    #####
# NPV_pe_f
# ├──── pv_benef_f
# │      ├──── earnings_app1_f
# │      |      ├──── wage_t_f
# │      |      |      └──── wage_0_f
# |      |      ├──── lambda_eff_f
# │      |      |      └────lambda1_t_f
# │      |      |            └────lambda1_in_f
# |      |      ├──── lambda1_in_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings_app2_f
# │      |      └────lambda_eff_f
# │      |           └────lambda1_t_f
# │      └──── interest_f
# └──── pv_costs_f (pv_costs_f)
#        ├──── delta_ed_final_f
#        ├──── interest_f
#        └──── s2_new_f
#        |      └──── costs1_p2_f
#        |             └──── costs1_p1_f
#        ├──── s2_f
#        └──── cost_per_student_f
#       ##     ###    ####    #####


# Approach 1
# NPV_pe_f --> a1_tax_pe
#     └────pv_benef_f --> pv_benef_tax_nx_in
#     |     ├────earnings_app1_f --> earnings_no_ext_in * tax_var1
#     |     |     ├────wage_t_f --> wage_t_in
#     |     |     |     └────wage_0_f --> wage_0_in
#     |     |     ├────lambda1_in_f --> lambda1_in
#     |     |     └────saturation_in_f --> saturation_in
#     |     └────interest_f --> interest_in
#     |
#     |
#     └────pv_costs_f --> costs2_in
#           ├────delta_ed_final_f --> delta_ed_final_in
#           ├────cost_per_student_f --> cost_per_student_in
#           ├────s2_f --> s2_in
#           └────interest_f --> interest_in


# unit test function
unit_test_f <- function(to_test_var, original_var, main_run_var = TRUE){
    if (main_run_var == TRUE) {
        if (length(to_test_var) > 1) {
            fails_test <- ( abs(sd(to_test_var) - original_var) > 0.0001 )
            text_val <- sd(to_test_var)
        } else {
            fails_test <- ( abs(to_test_var - original_var) > 0.0001 )
            text_val <- to_test_var
        }
        if (fails_test) {
            print(paste("Output has changed at",
                        deparse(substitute(to_test_var) ),
                        " to ", text_val) )
        }
      }
}

# TODO: update values of unit test within one_run_f
# one run of all the steps to get one policy estimate
one_run_f <-
  function(main_run_var1 = main_run_so,
           run_sim_var1 = run_sim_so,
           wage_ag_var1 = wage_ag_so,
           wage_ww_var1 = wage_ww_so,
           profits_se_var1 = profits_se_so,
           hours_se_cond_var1 = hours_se_cond_so,
           hours_ag_var1 = hours_ag_so,
           hours_ww_var1 = hours_ww_so,
           hours_se_var1 = hours_se_so,
           ex_rate_var1 = ex_rate_so,
           growth_rate_var1 = growth_rate_so,
           coef_exp_var1 = coef_exp_so[1],
           coef_exp2_var1 = coef_exp_so[2],
           lambda1_var1 = lambda1_in_f(lambda1_var = lambda1_so),
           prevalence_0_var1 = prevalence_0_so,
           prevalence_r_var1 = prevalence_r_so,
           new_prevl_r_var1 = new_prevalence_r_so,
           lambda2_var1 = lambda2_so,                                        
           coverage_var1 = coverage_so,                                        
           q_full_var1 = q_full_so,                                        
           q_zero_var1 = q_zero_so,                                        
           lambda1_new_var1 = lambda1_new_so,                                        
           gov_bonds_var1 = gov_bonds_so,                                        
           inflation_var1 = inflation_so,                                        
           gov_bonds_new_var1 = gov_bonds_new_so,                                                     
           inflation_new_var1 = inflation_new_so,                                       
           df_costs_var1 = costs_data_in,                                        
           staff_time_var1 = staff_time_so,                                        
           delta_ed_var1 = delta_ed_so,                                        
           delta_ed_ext_var1 = delta_ed_ext_so,                                        
           teach_sal_var1 = teach_sal_so,                                        
           teach_ben_var1 = teach_ben_so,                                        
           n_students_var1 = n_students_so,                                        
           teach_sal_new_var1 = teach_sal_new_so,                                            
           teach_ben_new_var1 = teach_ben_new_so,                              
           unit_cost_local_var1 = unit_cost_local_so,     
           unit_cost_local_new_var1 = unit_cost_2017usdppp_so,
           new_costs_var1 = new_costs_so,    
           countries_var1 = country_sel_so,
           years_of_treat_0_var1 = years_of_treat_0_so,
           years_of_treat_t_var1 = years_of_treat_t_so,
           tax_var1 = tax_so,                                        
           periods_var1 = periods_so) {                                        
    ####------------ Inputs for wage_t -----------------------------------------
    wage_0_in <- wage_0_f(
      wage_ag_var = wage_ag_var1,
      wage_ww_var = wage_ww_var1,
      profits_se_var = profits_se_var1,
      hours_se_cond_var = hours_se_cond_var1,
      hours_ag_var = hours_ag_var1,
      hours_ww_var = hours_ww_var1,
      hours_se_var = hours_se_var1,
      ex_rate_var = ex_rate_var1
    )
    unit_test_f(wage_0_in, 0.170124466664436, main_run_var = main_run_var1)
    ###---------- Inputs for earnings_app1_f ---------------------------------------
    wage_t_in <- wage_t_f(
      wage_0_var = wage_0_in,
      growth_rate_var = growth_rate_var1,
      coef_exp1_var = coef_exp_var1,
      coef_exp2_var = coef_exp2_var1
    )
    unit_test_f(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)

    lambda1_in <- lambda1_in_f(lambda1_var = lambda1_var1)
    unit_test_f(lambda1_in[1], 1.745, main_run_var = main_run_var1)

    lambda1_t_temp = lambda_t_f(
        lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
        years_of_treat_0_var = years_of_treat_0_var1,
        years_of_treat_t_var = years_of_treat_t_var1  
        )$lambda1_t

    lambda1_prevl_in <- lambda_eff_f(
      lambda1_var = lambda1_t_temp,
      prevalence_0_var = prevalence_0_var1,
      prevalence_r_var = prevalence_r_var1,
      other_prevl_r_var = new_prevl_r_var1,
      country_sel_var = countries_var1
      )$lambda1_eff_in
    unit_test_f(lambda1_prevl_in[1], 0.9508583060968, main_run_var = main_run_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)
    unit_test_f(lambda2_in[1], 10.2 , main_run_var = main_run_var1)

    saturation_in <- saturation_in_f(coverage_var = coverage_var1,
                                     q_full_var = q_full_var1,
                                     q_zero_var = q_zero_var1)$saturation_in
    unit_test_f(saturation_in, 0.511, main_run_var = main_run_var1)

    ###------------ Inputs for earnings_app2_f--------------------------------------
    lambda1_new_in <- lambda1_new_var1
    unit_test_f(lambda1_new_in, 79.51465,
              main_run_var = main_run_var1)
    lambda1_t_temp = lambda_t_f(
      lambda1_var = lambda1_new_var1,
      years_of_treat_0_var = years_of_treat_0_var1,
      years_of_treat_t_var = years_of_treat_t_var1  
    )$lambda1_t
    lambda1_prevl_new_in <- lambda_eff_f(lambda1_var = lambda1_t_temp,
                             prevalence_0_var = prevalence_0_var1,
                             prevalence_r_var = prevalence_r_var1,
                             other_prevl_r_var = new_prevl_r_var1,
                            country_sel_var = countries_var1
                            )$lambda1_eff_in
    unit_test_f(lambda1_prevl_new_in[1], 43.3278884864681, main_run_var = main_run_var1)

    ##------------ Inputs for pv_benef_f ---------------------------------------
    # earnings1
    earnings_no_ext_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_yes_ext_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )

    # earnings1 with prevalence
    earnings_no_ext_prevl_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prevl_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_yes_ext_prevl_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prevl_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )

    # earnings2
    earnings_no_ext_new_in <- earnings_app2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1])
    # earnings2 with prevalence
    earnings_no_ext_prevl_new_in <- earnings_app2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_prevl_new_in[1])

    # interest rate NEED TO UPDATE TO EXACT RESULT
    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test_f(earnings_no_ext_in, 31.1421332040266,
              main_run_var = main_run_var1)
    unit_test_f(earnings_yes_ext_in, 167.667817450905,
              main_run_var = main_run_var1)
    unit_test_f(earnings_no_ext_prevl_in, 16.9694876943406,
              main_run_var = main_run_var1)
    unit_test_f(earnings_yes_ext_prevl_in, 153.495171941219,
              main_run_var = main_run_var1)    
    unit_test_f(interest_in, 0.0985, main_run_var = main_run_var1)

    ##-------------- Inputs for costs2_f----------------------------------------
    # Make explicit non-function inputs:
    delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE,
                                          delta_ed_var = delta_ed_var1,
                                          delta_ed_ext_var = delta_ed_ext_var1)
    unit_test_f(delta_ed_final_in, 0.01134819, main_run_var = main_run_var1)

    delta_ed_final_x_in <- delta_ed_final_f(
      include_ext_var = TRUE,
      delta_ed_var = delta_ed_var1,
      delta_ed_ext_var = delta_ed_ext_var1
    )
    unit_test_f(delta_ed_final_x_in,  0.05911765, main_run_var = main_run_var1)

    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test_f(interest_in, 0.0985, main_run_var = main_run_var1)

    interest_new_in <- interest_f(
      gov_bonds_var = gov_bonds_new_var1,
      inflation_var = inflation_new_var1)$interest_in

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)
    unit_test_f(cost_per_student_in,  116.8549, main_run_var = main_run_var1)

    cost_per_student_new_in <- cost_per_student_f(
      teach_sal_var = teach_sal_new_var1,
      teach_ben_var = teach_ben_new_var1,
      n_students_var = n_students_var1
    )

    s2_in <- s2_f(
      unit_cost_local_var = unit_cost_local_var1,
      ex_rate_var = ex_rate_var1,
      years_of_treat_var = years_of_treat_0_var1
    )
    unit_test_f(s2_in, 1.4219, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(
      earnings_var = earnings_no_ext_in * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_nx_in, 23.6070893378784,
              main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_in * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_yx_in, 127.0994867217, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(
      earnings_var = earnings_no_ext_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_nx_in, 142.42587835824, main_run_var = main_run_var1)
    #Baird all and no ext + prevalence
    pv_benef_all_nx_prevl_in <- pv_benef_f(
      earnings_var = earnings_no_ext_prevl_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_nx_prevl_in, 77.608498246463, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_yx_in, 766.814399527604,
              main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_prevl_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_prevl_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_yx_prevl_in, 701.997019415827,
              main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new_in <- pv_benef_f(
      earnings_var = earnings_no_ext_new_in * tax_var1,
      interest_r_var = interest_new_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_new_in, 88.1820199569814,
              main_run_var = main_run_var1)

    # KLPS4 all and no ext
    pv_benef_all_new_in <- pv_benef_f(earnings_var = earnings_no_ext_new_in,
                                   interest_r_var = interest_new_in,
                                   periods_var = periods_var1)
    unit_test_f(pv_benef_all_new_in, 532.018219951622, main_run_var = main_run_var1)
    # KLPS4 all and no ext + prevalence
    pv_benef_all_prevl_new_in <- pv_benef_f(earnings_var = earnings_no_ext_prevl_new_in,
                                   interest_r_var = interest_new_in,
                                   periods_var = periods_var1)
    unit_test_f(pv_benef_all_prevl_new_in, 289.899107986178, main_run_var = main_run_var1)
    #Costs asd
    # costs1: EA costs no externalities
    cost1_in <- costs1_p2_f(country_total_var = df_costs_var1$total,
                            country_cost_var = df_costs_var1$costs_by_country,
                            staff_time_var = staff_time_var1,
                            country_name_var = df_costs_var1$Country,
                            select_var = countries_var1,
                            other_costs_var = new_costs_var1)
    unit_test_f(cost1_in,  0.08480686,
              main_run_var = main_run_var1)
    # s2_ea_in <-- cost1_in (costs1_p2_f) <-- cost_data (costs1_p1_f())
    s2_ea_in <- s2_new_f(interest_var = interest_new_in,
                      unit_cost_local_var = cost1_in,
                      ex_rate_var = 1,
                      year_of_treat_var = years_of_treat_t_var1)
    unit_test_f(s2_ea_in,  0.19634422968991, main_run_var = main_run_var1)
    costs2_ea_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_new_in,
      cost_of_schooling_var = 0,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_ea_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_ea_in,  0.147258172267433, main_run_var = main_run_var1)
    # costs2: Baird no externalities
    costs2_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_in, 11.776188118988, main_run_var = main_run_var1)
earnings_no_ext_in
    # Baird yes externalities
    costs2_x_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_x_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_x_in,  25.1962130559894, main_run_var = main_run_var1)

    s2_new_in <- s2_new_f(interest_var = interest_new_in,
                          unit_cost_local_var = unit_cost_local_new_var1,
                          ex_rate_var = 1,
                          year_of_treat_var = years_of_treat_t_var1)
    # costs2: KLPS4
    costs_a2_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_new_in,
      cost_of_schooling_var = cost_per_student_new_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_new_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs_a2_in, 32.2977546110344, main_run_var = main_run_var1)
    return( list(
      "wage_0_in" = wage_0_in,
      "wage_t_in" = wage_t_in,
      "lambda1_in" = lambda1_in,
      "lambda1_prevl_in" = lambda1_prevl_in,
      "lambda2_in" = lambda2_in,
      "saturation_in" = saturation_in,
      "lambda1_new_in" = lambda1_new_in,
      "lambda1_prevl_new_in" = lambda1_prevl_new_in,
      "earnings_no_ext_in" = earnings_no_ext_in,
      "earnings_no_ext_prevl_in" = earnings_no_ext_prevl_in,
      "earnings_yes_ext_in" = earnings_yes_ext_in,
      "earnings_yes_ext_prevl_in" = earnings_yes_ext_prevl_in,
      "earnings_no_ext_new_in" = earnings_no_ext_new_in,
      "earnings_no_ext_prevl_new_in" = earnings_no_ext_prevl_new_in,
      "interest_in" = interest_in,
      "costs1_country_in" = costs_data_in,
      "delta_ed_final_in" = delta_ed_final_in,
      "delta_ed_final_x_in" = delta_ed_final_x_in,
      "cost_per_student_in" = cost_per_student_in,
      "s2_in" = s2_in,
      "pv_benef_tax_nx_in" = pv_benef_tax_nx_in,
      "pv_benef_tax_yx_in" = pv_benef_tax_yx_in,
      "pv_benef_all_nx_in" = pv_benef_all_nx_in,
      "pv_benef_all_nx_prevl_in" = pv_benef_all_nx_prevl_in,
      "pv_benef_all_yx_in" =  pv_benef_all_yx_in,
      "pv_benef_all_yx_prevl_in" = pv_benef_all_yx_prevl_in,
      "pv_benef_tax_new_in" = pv_benef_tax_new_in,
      "pv_benef_all_new_in" = pv_benef_all_new_in,
      "pv_benef_all_prevl_new_in" = pv_benef_all_prevl_new_in,
      "costs2_ea_in" = costs2_ea_in,
      "costs2_in" = costs2_in,
      "costs2_x_in" = costs2_x_in,
      "costs_a2_in" = costs_a2_in,
      "cost1_in" = cost1_in
    ) )
  }

invisible( list2env(one_run_f(),.GlobalEnv) )

#  return( sapply( ls(pattern= "_in\\b"), function(x) get(x)) )

#}
```


```r
#Baird 1: Costs = Baird w/tax and no externalities (no ext);
#Benef = Baird no ext
a1_tax_pe <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test_f(a1_tax_pe, 11.8309012188904)
#Baird 2: Costs = Baird w/tax and yes externalities (no ext);
#Benef = Baird yes ext
a1_x_tax_pe <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_x_in)
unit_test_f(a1_x_tax_pe, 101.903273665711)
# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
a1_all_pe <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test_f(a1_all_pe, 130.649690239252)
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
a1_x_all_pe <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_x_in)
unit_test_f(a1_x_all_pe, 741.618186471615)

#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1_pe <- NPV_pe_f(benefits_var = pv_benef_tax_new_in, costs_var = costs_a2_in)
unit_test_f(klps4_1_pe, 55.884265345947)
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2_pe <- NPV_pe_f(benefits_var = pv_benef_all_new_in, costs_var = costs_a2_in)
unit_test_f(klps4_2_pe, 499.720465340588)

# EA1: no externality NPV using EAs costs
ea1_pe <- NPV_pe_f(benefits_var = pv_benef_all_nx_prevl_in, costs_var = costs2_ea_in)
unit_test_f(ea1_pe, 77.4612400741955)
# EA2: yes externality NPV using EAs costs
ea2_pe <- NPV_pe_f(benefits_var = pv_benef_all_yx_prevl_in, costs_var = costs2_ea_in)
unit_test_f(ea2_pe, 701.849761243559)
# EA3: benef= KLPS all and no ext; Costs=EA
ea3_pe <- NPV_pe_f(benefits_var = pv_benef_all_prevl_new_in, costs_var = costs2_ea_in)
unit_test_f(ea3_pe, 289.751849813911)
```

</details>


| Approach | Benefits                               | Costs                        | Social NPV (all)          | Fiscal NPV (tax)          |
|----------|----------------------------------------|------------------------------|---------------------------|---------------------------|
| 1.1      | @baird2016worms with no externalities  | Treatment, Education         | 130.6   | 11.8   |
| 1.2      | @baird2016worms with externalities     | Treatment, Education with externalities  | 741.6 | 101.9 |
| 2.1      | @klps4 with no externalities           | Treatment, Education         | 499.7  | 55.9  |
| 3.1      | 1.1 + prevalence + length of treatment | Treatment (EA)               | 77.5      | -                         |
| 3.2      | 1.2 + prevalence + length              | Treatment (EA)               | 701.8      | -                         |
| **3.3**  | **2.1 + prevalence + length**          | **Treatment (EA)**           | **289.8**  | **-**                     |

<br>




```
## [1] 10000
```

```
## [1] 1
## [1] 2
## [1] 3
## [1] 4
## [1] 5
## [1] 6
## [1] "Output has changed at to_test  to  4.82110704697217"
## [1] 7
## [1] 8
## [1] 9
```

```
## [1] "Output has changed at npv_all_sim[[6]]  to  4.82110704697217"
```

```
##     [1] 497.0966 493.0955 501.5418 499.8250 498.8175 500.7150 494.8784 493.0853
##     [9] 492.5267 503.1560 499.1645 499.2326 500.1965 499.2631 503.3985 494.7602
##    [17] 494.5370 493.8001 495.6832 495.4300 504.3520 499.9044 498.4400 499.1088
##    [25] 504.0104 497.4828 496.6004 500.5458 504.6720 496.7031 495.4181 497.4375
##    [33] 509.5955 498.9424 497.5397 500.7043 496.9887 505.2902 493.4583 501.8081
##    [41] 495.4982 499.4499 492.9094 482.3069 499.4237 502.7504 504.3174 495.2024
##    [49] 503.4084 504.8554 497.2025 494.6526 498.9521 502.1384 505.7913 500.9145
##    [57] 502.5294 501.0733 498.4520 489.6335 500.4152 492.1849 506.3899 498.4441
##    [65] 491.5238 501.4547 496.5024 501.2913 495.5843 506.8746 509.1470 500.5526
##    [73] 504.6650 497.9312 488.2416 507.5765 503.3969 497.9554 493.0609 496.3485
##    [81] 502.4685 496.9787 497.6883 503.2492 500.5699 504.0798 498.2608 501.4552
##    [89] 493.3305 497.1394 496.8018 498.3321 502.4216 505.3809 499.8809 505.5724
##    [97] 501.9164 498.5729 499.6778 501.9477 502.3881 495.7437 497.0967 499.2868
##   [105] 499.0548 491.7231 503.1814 498.2918 503.5042 499.7021 501.0132 493.1588
##   [113] 498.2596 500.4522 486.9761 495.4888 494.0027 498.6250 498.8988 499.4300
##   [121] 503.5751 506.3632 500.9024 486.7029 491.0948 499.3764 503.7915 504.6938
##   [129] 492.1394 501.1489 502.0334 497.6371 500.5288 501.6844 507.6980 495.6701
##   [137] 494.7961 497.7390 493.1575 502.5584 499.8832 496.1137 501.9052 498.1503
##   [145] 498.3981 500.0641 500.4080 494.5150 503.4301 496.5245 489.9438 502.2397
##   [153] 492.3954 501.5887 500.4471 505.0581 505.1125 497.0661 492.9668 501.5670
##   [161] 505.0670 488.5946 502.0402 502.3362 501.6037 499.6019 492.8437 502.0668
##   [169] 501.4765 497.4082 497.6435 495.8076 498.7900 486.3821 506.5789 500.9785
##   [177] 501.1667 501.6811 502.3215 494.1624 496.6413 502.3768 490.8040 500.7652
##   [185] 499.5750 495.6023 500.8433 501.0005 498.0051 495.6274 500.3685 496.0124
##   [193] 498.1797 498.4669 489.7585 489.2430 501.7073 504.8360 494.1835 497.5585
##   [201] 502.8064 497.9259 499.9325 501.7626 493.9351 496.8710 503.8245 487.9819
##   [209] 487.8529 489.5174 499.9605 498.3056 488.6134 500.6218 507.3394 493.1657
##   [217] 499.4531 503.2061 503.3852 496.4288 500.8109 492.6521 495.6474 504.1186
##   [225] 501.7340 499.8006 496.7867 496.0777 494.8769 495.1727 508.0503 502.3887
##   [233] 502.7563 496.4105 496.1195 499.8304 495.3805 496.4565 504.8458 488.4351
##   [241] 504.0410 496.0070 499.5761 495.3256 492.5277 502.0399 504.9237 495.6414
##   [249] 507.8560 501.2658 491.8234 501.7369 503.1888 502.4681 501.2940 498.3263
##   [257] 501.5990 499.9436 499.0305 503.7531 499.2943 499.2212 498.2003 493.3206
##   [265] 502.0022 505.3753 500.8186 503.0597 501.9831 504.5763 496.5373 496.2842
##   [273] 505.5671 505.5015 504.0185 499.0326 508.3109 497.1531 495.4290 495.6760
##   [281] 493.8837 499.8636 499.3247 501.2759 502.1584 506.6797 506.5401 508.0322
##   [289] 496.5600 501.9266 495.7139 490.9016 500.3485 506.8749 499.0127 502.2304
##   [297] 505.9392 492.3944 494.6901 505.4869 501.4198 495.6094 497.2076 498.9352
##   [305] 504.6426 503.1016 494.8568 495.2293 495.9227 509.0187 497.7701 495.1356
##   [313] 500.3701 496.8993 501.2538 500.9548 503.3854 497.5418 496.7582 496.8735
##   [321] 500.1297 490.7677 488.5600 504.0082 506.9279 499.9466 498.6919 503.5462
##   [329] 497.7599 499.3566 496.4153 499.4924 500.2243 499.9156 502.0459 497.4238
##   [337] 495.7285 496.6454 507.9574 500.1183 498.0553 504.5875 503.3727 493.3191
##   [345] 503.8592 490.4577 495.7843 495.8146 501.1246 507.3812 508.1390 495.0414
##   [353] 504.8586 502.1057 499.6641 495.7884 496.9813 498.9514 501.3056 497.5675
##   [361] 500.8199 491.6455 496.1854 489.2912 499.1000 506.5949 501.8957 495.3875
##   [369] 501.9524 494.3858 493.5542 502.3077 507.1996 499.8594 490.5716 501.9473
##   [377] 497.2277 503.2031 486.6983 505.9472 490.2242 497.0222 499.6519 499.9632
##   [385] 489.8985 503.2587 501.1125 502.8961 503.9287 498.4016 503.5536 500.1445
##   [393] 500.1332 497.2024 501.2919 496.3309 510.2537 497.3276 488.0425 490.1815
##   [401] 498.9572 502.5752 494.8060 505.0162 499.6332 491.5806 499.1820 501.8274
##   [409] 494.7858 494.1746 496.7242 491.6621 483.7531 486.5488 487.7734 496.7365
##   [417] 502.2695 498.5890 500.7764 496.0912 504.6320 503.3710 505.7931 503.7998
##   [425] 494.8664 493.0680 491.8256 504.8956 502.6606 500.5824 478.3241 508.2592
##   [433] 507.3399 494.1824 495.4732 493.6762 500.4620 495.3118 491.8189 502.0205
##   [441] 503.6689 505.2353 502.1415 496.4537 492.8162 492.9224 502.1212 495.7225
##   [449] 493.5003 498.9069 504.0773 510.8232 492.7036 502.1332 491.6545 505.4531
##   [457] 499.4066 500.6586 500.5109 498.9988 505.4450 501.6161 505.4924 502.9028
##   [465] 494.0839 503.6137 506.4796 485.1522 502.5042 496.9215 496.5418 501.0792
##   [473] 498.7734 499.5569 492.4874 502.8714 503.7119 493.1554 506.6657 491.3306
##   [481] 498.1977 499.0003 492.3994 494.4595 497.3370 497.2374 489.7938 490.4005
##   [489] 504.7615 499.0611 491.5626 494.4687 498.6163 502.7173 503.1276 498.7781
##   [497] 501.5339 499.2871 503.4953 503.3061 505.2520 502.9202 501.1284 498.9360
##   [505] 506.0155 498.2917 504.4475 497.4716 492.7946 500.9387 500.1216 499.2153
##   [513] 495.7664 498.2629 494.9016 494.7965 501.7939 496.8426 499.4313 502.0631
##   [521] 501.0419 503.4089 499.1824 500.7873 500.9191 500.4433 496.7790 499.5150
##   [529] 500.3481 494.5039 497.9153 496.7526 497.0802 498.7448 498.1612 508.4417
##   [537] 499.2000 495.5671 502.7383 496.6387 502.0072 506.1673 502.7099 498.7689
##   [545] 497.4728 497.3223 501.8391 499.8898 498.6710 501.9528 497.4691 501.6994
##   [553] 499.1064 502.9040 502.8651 502.4540 495.3099 498.5163 499.0393 500.4368
##   [561] 497.8026 500.1496 503.5820 503.7412 499.6950 500.3810 500.3896 493.7632
##   [569] 498.8558 500.2154 497.4585 508.0529 507.1009 506.2878 493.6025 496.4207
##   [577] 503.0969 492.7498 493.9334 497.0862 500.4374 503.8836 499.0033 497.4636
##   [585] 496.8969 502.7695 497.0951 502.8518 507.4325 501.9596 500.1929 498.3050
##   [593] 489.5154 492.7156 498.5134 495.8102 503.5138 491.5194 494.7831 505.8048
##   [601] 503.5920 497.4669 499.8842 503.1232 506.4760 500.3081 495.0829 502.8972
##   [609] 499.2024 503.9166 498.8558 496.9367 496.3982 493.1339 494.8871 495.9770
##   [617] 502.9287 503.7429 494.2266 506.4019 492.0871 504.7541 502.8866 497.8899
##   [625] 503.3070 495.9500 502.6895 501.6537 487.7835 500.5857 496.0771 498.8192
##   [633] 502.9401 500.7328 499.7373 493.7782 497.1216 504.7953 498.3289 502.3282
##   [641] 495.2175 496.9548 501.7761 499.7018 498.3963 493.4073 505.0997 500.4537
##   [649] 494.2506 494.2106 497.6451 503.1964 498.4533 499.2846 496.7795 501.7422
##   [657] 499.9435 503.9045 498.8196 491.4956 507.7478 495.1480 495.3345 500.3492
##   [665] 499.2503 501.9525 497.4216 506.7683 492.7468 498.1085 498.6962 501.5898
##   [673] 500.3202 506.0540 499.9316 503.5557 497.1869 502.6291 497.4616 503.7622
##   [681] 503.7841 498.5498 497.3455 499.9103 500.6441 505.2883 492.5817 503.8835
##   [689] 501.3601 502.3490 498.4472 498.2708 501.2994 505.8284 494.8277 503.6687
##   [697] 501.5191 503.8877 500.9585 500.0305 495.3403 496.9105 501.2382 501.2681
##   [705] 497.4915 499.0651 503.7596 492.3380 494.2789 505.0892 500.3902 492.5758
##   [713] 500.1158 495.1234 497.5613 497.0789 493.8694 493.1505 503.7958 500.0021
##   [721] 498.0422 501.6456 506.7714 502.0546 495.1647 501.1367 499.0156 500.1305
##   [729] 500.0126 509.0452 494.1224 502.0318 488.2389 501.0674 498.7918 503.6719
##   [737] 506.3176 497.6633 502.3265 503.7943 500.9182 496.5930 504.9658 508.2308
##   [745] 499.1899 499.5284 500.5975 495.6874 505.1343 493.0198 499.0888 500.9457
##   [753] 504.2087 500.6995 501.8973 498.3356 495.6530 498.1966 500.7748 509.0421
##   [761] 502.6001 500.1268 505.7529 495.6282 497.6682 503.0599 497.6577 493.3809
##   [769] 499.5758 495.4973 492.7382 501.8729 499.6799 497.7437 500.6996 503.0559
##   [777] 493.4191 503.5806 502.7284 508.9644 502.2193 501.2180 492.0007 503.9258
##   [785] 500.9071 497.8378 504.1442 501.8744 504.0708 503.9734 498.7501 499.9582
##   [793] 493.1649 496.6281 500.5333 503.4250 495.4726 492.8247 504.1922 503.2650
##   [801] 495.6966 498.2523 501.4550 497.9591 490.8519 491.2456 507.5524 500.4236
##   [809] 505.6112 495.2842 499.5057 499.7316 502.5266 502.2145 504.1636 490.6456
##   [817] 494.6019 507.5476 502.5231 488.7902 507.1130 500.2393 496.8724 493.3948
##   [825] 497.3010 503.7457 496.2404 503.4686 494.7717 505.5814 498.3344 498.2116
##   [833] 501.4365 497.2796 505.3877 501.3406 497.3283 494.4434 491.8072 494.6168
##   [841] 503.6464 498.6044 503.5361 499.7941 498.9845 507.9383 495.2266 500.8884
##   [849] 498.1179 499.6935 507.4510 495.2967 510.1445 494.7929 501.4271 497.5970
##   [857] 505.0407 507.1477 498.8343 504.5543 501.8030 496.5859 490.2288 495.7655
##   [865] 491.4168 496.8362 488.9372 503.1351 499.4331 498.3493 488.2847 497.5018
##   [873] 498.0589 503.4982 495.8908 498.3367 495.3323 502.5371 500.4922 494.7639
##   [881] 501.3870 495.8946 507.3198 505.0493 500.3651 494.2165 495.2612 500.5210
##   [889] 501.0791 493.5979 502.6416 498.3171 503.9760 491.2255 494.5867 505.8362
##   [897] 495.1697 498.4745 501.0850 495.3881 501.8092 495.9791 492.3956 502.0475
##   [905] 502.8777 499.3470 502.6244 494.6167 497.3088 495.8443 496.1074 500.6820
##   [913] 501.0050 491.4787 506.2176 500.2643 498.3110 498.4616 503.2476 503.5415
##   [921] 499.3030 502.8689 492.4631 501.8918 503.7557 490.3638 505.7438 495.2608
##   [929] 492.8327 491.9087 499.8840 495.5513 505.4720 501.7150 499.4018 496.7770
##   [937] 503.2912 500.7128 505.1758 498.9962 497.3469 504.6932 489.1435 501.2304
##   [945] 489.0425 498.6344 504.5374 504.3506 498.1816 500.1839 504.4926 501.8757
##   [953] 505.7842 500.4264 499.5344 501.6089 505.5727 503.1044 498.8848 491.5388
##   [961] 498.3748 492.8529 501.2371 499.4683 506.3017 491.2671 490.3649 500.8494
##   [969] 502.5457 501.3362 502.6857 495.8327 501.1177 495.0203 501.0641 493.2674
##   [977] 500.1025 501.1196 501.3697 501.2848 491.8805 504.6939 494.3566 504.2227
##   [985] 494.4733 507.2658 500.2487 503.7800 502.9653 502.0501 504.9438 502.3198
##   [993] 504.4071 505.9427 499.7733 506.6988 500.4528 495.3579 497.6061 501.0112
##  [1001] 498.1605 498.9894 499.8348 497.0416 494.5568 505.3647 498.0188 498.8608
##  [1009] 495.9888 496.4889 490.8967 503.5031 499.6646 497.1873 507.8979 502.7143
##  [1017] 503.8630 499.2744 496.7565 502.3410 501.4594 494.8933 502.5930 503.2536
##  [1025] 503.1336 490.3185 500.0249 501.7835 499.8788 496.5196 505.8502 503.0072
##  [1033] 501.2972 498.2529 501.3916 501.5307 495.7037 495.0172 487.6676 498.3708
##  [1041] 500.1360 495.3696 502.5370 492.6037 500.3404 498.5943 505.0856 490.3300
##  [1049] 499.3092 502.4312 503.0834 496.4125 497.3898 500.5434 499.8934 493.6282
##  [1057] 488.9936 503.2287 500.1224 495.8616 497.4930 501.6710 500.1885 501.1973
##  [1065] 506.3285 497.0924 498.2074 503.5598 500.7030 506.5389 504.4199 491.7993
##  [1073] 497.4771 496.7055 504.6150 499.7932 500.3465 499.7678 500.5893 504.7826
##  [1081] 494.0638 496.3057 503.1778 500.3316 503.1751 498.1256 502.7520 491.0430
##  [1089] 502.7993 503.3961 493.4876 500.7733 493.5407 498.4104 498.2383 500.4403
##  [1097] 504.6422 496.3701 494.2608 497.3110 497.7329 496.2805 493.7897 500.4472
##  [1105] 498.4927 503.3829 502.7533 506.1458 498.6744 500.0605 494.1237 497.8602
##  [1113] 500.0796 496.3237 502.7038 504.0662 498.6723 498.9303 508.9749 501.6589
##  [1121] 503.6957 495.5145 491.7334 497.7353 500.4111 493.3464 498.4496 501.7837
##  [1129] 491.5910 504.1307 501.2309 499.6773 498.8568 502.6357 499.0147 490.3172
##  [1137] 502.9629 495.5240 492.1609 489.0578 495.2007 494.9159 495.9847 502.2729
##  [1145] 486.8293 492.9924 495.2850 502.0252 507.7814 502.0781 493.9129 500.8134
##  [1153] 500.8950 506.9463 500.7719 496.7651 499.2364 495.2099 496.5033 501.2777
##  [1161] 490.1302 498.6106 495.6276 496.1367 493.5688 501.1138 504.8853 504.8836
##  [1169] 505.4914 505.1511 501.1873 504.0068 502.9296 496.2582 500.5254 494.2662
##  [1177] 503.6422 499.3159 501.2871 496.8410 496.8407 494.1837 488.0893 498.8970
##  [1185] 504.9028 489.4624 497.2569 503.2271 504.6300 498.0217 493.1961 497.3812
##  [1193] 500.4953 498.2931 501.0265 499.5607 497.8277 499.1733 499.6128 503.2106
##  [1201] 501.5322 495.1051 496.5766 507.1105 503.4875 505.4271 502.1954 503.3056
##  [1209] 502.1003 509.2672 497.2125 500.5121 489.8345 504.0819 496.5971 496.0036
##  [1217] 502.0194 498.6489 493.1558 494.7291 498.3987 492.5323 503.6387 500.5502
##  [1225] 493.9577 495.4892 497.4262 499.2020 508.4887 487.5159 499.3286 497.2662
##  [1233] 502.0228 500.1341 497.1883 507.8983 496.2195 500.9740 502.7063 497.4270
##  [1241] 502.6298 500.1895 502.8908 503.0004 497.8988 507.9837 490.1081 508.5019
##  [1249] 506.2586 495.2601 500.8034 498.3868 491.8383 502.9593 489.1646 501.6087
##  [1257] 504.9960 494.4250 506.4215 494.8298 493.9988 508.5806 498.5973 494.3528
##  [1265] 501.1376 500.9835 496.5007 503.7372 500.9231 498.5933 502.5751 504.9484
##  [1273] 504.4062 495.1480 501.5001 501.3706 494.2016 506.7609 503.8375 509.3777
##  [1281] 503.7333 502.3945 506.6660 506.5658 497.8614 502.5786 502.4907 503.9686
##  [1289] 501.6691 500.9777 492.8810 499.0019 500.1734 494.4884 499.2914 500.0177
##  [1297] 489.4009 499.7182 506.1942 505.0439 496.5919 499.5151 499.4165 506.1460
##  [1305] 495.6517 501.7583 504.2572 492.4895 499.8282 496.7879 498.6226 503.4458
##  [1313] 499.3806 503.9486 501.0203 497.1395 502.0258 494.2084 497.8175 502.1656
##  [1321] 494.4035 498.0578 500.7703 497.2160 498.7987 499.5227 499.6941 486.7209
##  [1329] 507.3841 510.2675 501.2634 503.1002 496.8874 507.0474 501.0894 498.3825
##  [1337] 496.1801 495.2950 494.2439 499.9433 500.1646 504.4872 499.4014 494.6934
##  [1345] 495.5139 500.8032 496.9060 491.1251 500.1789 502.0028 498.4573 501.2688
##  [1353] 499.8614 503.6288 502.7528 496.7739 507.9617 492.6201 498.5018 499.5213
##  [1361] 503.6638 504.2983 506.1940 503.7897 508.2770 500.5429 493.1982 493.5310
##  [1369] 494.5443 499.3861 502.2666 499.3845 499.5018 495.1962 505.1106 499.4348
##  [1377] 497.3477 501.6454 502.8019 503.3682 505.0831 491.5351 497.7092 497.0000
##  [1385] 500.0580 507.8504 495.3176 490.9503 496.4669 504.7588 502.4613 496.9200
##  [1393] 504.2659 495.4194 492.7526 498.1725 498.6733 500.3343 492.9178 497.1288
##  [1401] 499.4630 496.4179 494.3140 504.8976 500.5177 498.1097 499.6258 496.9370
##  [1409] 499.6896 498.2855 500.5218 506.7237 496.3244 496.4611 493.5018 503.8367
##  [1417] 495.4981 504.8124 495.5636 503.0756 501.7505 501.2935 497.7082 494.2688
##  [1425] 495.2734 496.2378 494.0582 494.2184 498.7107 504.7794 490.3465 491.9322
##  [1433] 494.9672 503.9901 500.0184 498.6828 490.9253 497.0628 501.4839 502.0686
##  [1441] 494.7627 498.2628 496.2528 487.3511 501.0462 493.6176 491.1742 501.4620
##  [1449] 498.6780 500.5907 499.1644 499.9711 504.3958 506.5303 490.2316 494.0463
##  [1457] 504.4170 505.1625 500.5410 499.9986 500.4033 512.6335 496.9102 499.4289
##  [1465] 494.0084 489.9827 497.7154 504.9960 502.5793 501.7863 495.8141 498.3497
##  [1473] 501.1926 509.4484 496.4144 495.9104 495.3657 502.3868 496.9951 506.3290
##  [1481] 499.6795 492.7517 490.8834 501.6153 494.7506 493.1474 498.4711 500.2343
##  [1489] 500.9540 495.3299 507.0953 505.6120 498.1825 496.0471 498.2266 498.8304
##  [1497] 502.7692 504.9053 497.4221 499.7068 501.0247 502.3544 498.9253 500.2535
##  [1505] 492.6259 504.7747 509.0891 500.2323 494.7467 507.8982 494.8420 501.4309
##  [1513] 505.2841 500.6158 501.3925 499.3080 492.6597 492.2177 501.7286 492.5366
##  [1521] 499.0166 488.3424 508.4660 497.9963 495.2150 494.0846 500.1321 501.1740
##  [1529] 506.0217 503.6801 504.5580 500.0405 491.9815 506.6758 499.5953 497.5421
##  [1537] 507.6456 499.5614 501.3598 497.8303 495.4113 495.3696 498.1732 503.5711
##  [1545] 495.8217 503.3112 503.6400 496.7455 487.0971 501.7616 502.6115 497.1966
##  [1553] 498.8527 493.1173 501.7312 498.7661 500.2497 504.3870 499.1611 497.8809
##  [1561] 504.2235 496.1919 501.1398 494.3312 498.2390 489.1413 500.3246 500.7284
##  [1569] 498.6174 498.6986 495.0758 495.1969 503.9313 500.5572 497.0665 497.2981
##  [1577] 495.1492 505.2341 497.9140 500.9991 499.2592 504.1380 495.3104 503.2353
##  [1585] 499.8665 504.2319 499.3995 498.2852 500.2178 494.6218 498.1976 495.6889
##  [1593] 497.7125 492.3511 497.0452 493.1679 503.4767 497.3134 499.4277 497.3030
##  [1601] 500.5607 500.6912 494.7031 499.9180 497.1516 502.6830 502.8040 495.1150
##  [1609] 500.5190 504.9405 499.8287 495.8863 502.3237 496.4326 499.5515 483.0334
##  [1617] 500.3085 501.3656 500.6846 493.5629 499.3087 488.9105 505.9481 505.4853
##  [1625] 499.7472 494.1327 503.2434 501.0087 500.4643 487.5610 505.8199 505.9062
##  [1633] 496.6627 496.0252 498.7319 504.8954 501.9995 494.7503 500.5438 497.8570
##  [1641] 503.3969 501.0306 508.8697 500.0724 493.2301 504.2706 499.0810 490.8674
##  [1649] 492.6872 500.4492 497.6930 503.1253 491.1592 496.2483 499.5933 506.0322
##  [1657] 504.8910 502.9644 506.8732 502.8326 500.6002 501.4245 495.3035 499.0644
##  [1665] 500.2139 494.1762 500.3738 496.9685 503.3157 505.3825 508.4758 494.8009
##  [1673] 504.1692 503.1481 500.8008 501.5517 496.2325 492.5410 500.9346 500.4277
##  [1681] 497.4439 496.3303 503.8146 506.9228 497.0251 492.5839 505.1729 488.6611
##  [1689] 501.3750 503.6642 496.4193 505.5361 509.0712 496.8769 495.0690 501.4586
##  [1697] 500.0986 497.8985 505.8032 494.0696 500.9570 497.0186 507.4037 506.0341
##  [1705] 501.2362 496.5762 506.1582 491.8377 506.2000 498.6262 501.7753 503.9206
##  [1713] 503.0214 503.3606 495.2769 502.4388 502.5007 497.8702 503.9524 489.0257
##  [1721] 495.2016 499.5086 497.7201 502.8154 495.4809 500.5430 494.5850 498.6769
##  [1729] 502.1582 493.0991 500.8824 505.3894 494.7269 501.7841 498.3421 501.2249
##  [1737] 496.8233 500.4808 507.5663 492.7934 500.7261 506.4275 496.8054 499.0135
##  [1745] 499.0736 494.2289 505.2470 503.2587 496.1531 500.9687 501.0824 499.0262
##  [1753] 499.5789 498.4145 505.3041 497.6289 496.3736 502.8280 505.7572 498.9690
##  [1761] 500.0316 511.3655 502.8516 489.8504 499.1166 506.1152 493.5523 499.7172
##  [1769] 494.8088 494.5750 500.1892 503.1227 498.8678 491.9972 499.9267 495.2808
##  [1777] 501.6864 501.9738 504.0514 498.1754 499.9688 510.3455 499.2490 497.8278
##  [1785] 500.3782 504.8014 498.1014 499.9790 498.1322 505.4884 501.5449 499.8922
##  [1793] 498.6447 499.7722 500.3150 503.3684 497.5242 497.7097 503.1290 502.3545
##  [1801] 504.1283 504.7509 499.1358 503.7970 495.6934 503.2967 500.7359 496.1569
##  [1809] 500.1996 491.5686 499.9988 495.7419 505.9474 501.2917 501.4077 506.1691
##  [1817] 499.8520 500.2289 496.9068 503.9463 507.4114 503.1824 494.2679 500.1832
##  [1825] 503.2655 502.2627 503.9269 498.1614 497.1398 496.1910 495.9212 499.5839
##  [1833] 498.4053 493.2703 503.2961 490.0336 507.5086 504.9983 503.3295 504.5207
##  [1841] 501.4365 499.0908 495.1679 500.1332 500.7149 498.4469 505.9094 503.7953
##  [1849] 501.4999 505.9885 502.8738 498.7508 497.9537 506.4110 499.7477 498.9352
##  [1857] 489.5452 489.1492 504.2347 495.5016 502.5067 499.3183 504.3365 497.6442
##  [1865] 499.1375 495.7530 501.5041 499.2599 497.4047 489.3072 501.9253 503.1890
##  [1873] 499.1687 496.6198 502.6831 499.5939 496.4142 491.2389 499.3816 495.7445
##  [1881] 502.1772 494.6414 503.0446 494.9225 498.6813 503.8947 494.9892 499.7778
##  [1889] 501.1206 501.7923 484.4922 501.7327 495.5891 502.2826 502.4460 495.7284
##  [1897] 499.8703 506.8309 497.8800 494.4286 491.8785 497.0054 502.4459 499.4429
##  [1905] 499.9927 508.1650 495.3328 495.3564 502.7231 499.0803 498.3066 497.5466
##  [1913] 504.0568 504.8932 495.9674 507.4713 489.3961 493.4362 500.5518 498.9811
##  [1921] 502.7151 496.4400 503.2076 503.5550 495.0595 496.4048 489.7357 498.2337
##  [1929] 495.5515 508.0055 501.4050 501.5624 489.7583 506.1745 497.2693 501.6714
##  [1937] 503.5649 505.6169 505.5706 504.1523 499.6811 495.1479 494.0508 501.8769
##  [1945] 490.0291 500.3272 488.4927 507.6610 499.4739 496.2624 503.3567 497.9973
##  [1953] 493.2689 502.7182 501.0096 502.3796 481.8022 505.6712 500.4657 503.2583
##  [1961] 500.3318 503.9756 497.9776 504.6220 495.1761 501.7273 507.4899 499.2698
##  [1969] 498.3965 491.3427 493.0090 501.2191 504.6893 501.8633 499.6480 502.4386
##  [1977] 497.2735 499.1747 494.9626 496.7152 508.3035 506.7966 500.2197 492.0661
##  [1985] 500.9372 499.4715 500.6880 504.3379 501.8956 505.4568 502.0290 500.0533
##  [1993] 497.7579 499.5108 502.1914 495.1488 505.5613 499.4782 501.5123 505.0347
##  [2001] 502.2921 501.8200 498.1116 498.5653 502.0455 498.5415 492.8303 498.0021
##  [2009] 502.9328 505.3342 497.3619 492.2110 499.2299 502.2715 494.2813 500.9942
##  [2017] 499.4784 503.6644 505.1780 500.1670 494.9664 495.5836 493.3298 499.6443
##  [2025] 503.0043 508.0712 506.9567 496.9200 497.6387 497.1924 498.5998 484.5229
##  [2033] 500.2052 494.2552 491.1159 495.5218 493.2253 499.7767 502.7154 493.2120
##  [2041] 501.4056 503.3733 496.6405 497.6449 502.5903 507.0590 500.8702 494.5302
##  [2049] 493.6866 491.4097 506.9609 499.8480 505.6131 498.0945 505.3209 503.2456
##  [2057] 493.3540 500.7532 500.1895 507.6874 498.0899 498.8665 494.3348 504.1739
##  [2065] 502.5211 491.3841 499.3506 501.7147 499.0268 505.0387 498.6630 501.5284
##  [2073] 490.9911 502.2848 494.0122 496.6672 507.0879 495.5193 499.4149 493.7997
##  [2081] 493.8013 497.8734 501.6065 495.5124 502.1922 493.1111 503.7600 500.0285
##  [2089] 506.0114 503.2067 503.9417 501.1378 500.5679 502.2230 496.6275 498.6473
##  [2097] 495.8201 495.5457 502.1145 492.1265 500.5799 499.5422 497.8446 502.0067
##  [2105] 501.1920 501.3111 499.2171 499.9141 498.1689 501.2077 506.1217 497.5134
##  [2113] 501.8076 501.2536 501.4356 494.5234 505.1282 498.2779 503.6974 499.7524
##  [2121] 503.7452 497.3309 497.5934 500.1639 499.9794 499.7196 490.4059 503.5696
##  [2129] 502.6400 499.6297 507.7112 504.0892 507.2871 500.2595 499.3970 496.6016
##  [2137] 497.0322 495.2109 495.8545 504.9230 497.5444 506.9490 504.0390 496.7153
##  [2145] 502.4311 508.4764 500.4232 501.8151 499.2798 495.7696 496.9334 503.1001
##  [2153] 500.7529 500.3795 504.4676 502.3372 509.0361 503.4691 494.6124 504.8008
##  [2161] 501.0838 503.0098 502.3548 489.5313 507.7374 507.4395 501.0119 494.5717
##  [2169] 498.5750 495.0639 498.3787 495.1810 497.3110 497.3800 504.0769 495.1612
##  [2177] 502.5973 492.7297 504.7155 507.4093 496.7201 500.1596 499.1716 503.3212
##  [2185] 503.9333 503.1441 501.1917 500.5526 504.7366 491.4346 498.3327 496.9896
##  [2193] 498.5538 510.8572 503.1796 501.5652 499.8958 495.6370 501.5699 495.4916
##  [2201] 504.2555 502.3655 502.4696 502.6143 503.1021 507.6116 501.5238 491.1886
##  [2209] 498.1673 495.8522 494.9058 495.2314 496.8508 495.5367 499.6911 498.6671
##  [2217] 494.2800 486.5893 496.7686 501.3557 502.7091 499.6069 494.3692 499.2784
##  [2225] 498.0850 505.0583 502.5898 502.0600 494.4039 506.4067 502.0316 501.3415
##  [2233] 499.6532 493.7103 495.9619 502.1371 499.8192 508.0250 491.1641 501.2872
##  [2241] 496.9019 502.9630 497.0692 503.7603 501.5463 498.4894 505.3677 494.7318
##  [2249] 495.9277 495.5217 505.7474 497.6686 490.4260 500.7305 497.7324 501.5798
##  [2257] 504.8068 502.4704 499.2956 500.4901 499.4840 506.6240 497.0451 504.0761
##  [2265] 505.4178 502.1101 498.2977 498.1022 503.5396 485.9098 504.2971 501.0639
##  [2273] 500.7038 496.3456 497.4505 503.2526 496.7464 489.3616 495.8647 495.5113
##  [2281] 502.8393 501.1370 504.4452 503.8553 504.6177 503.2590 496.1493 498.5168
##  [2289] 494.3704 501.9928 504.6613 498.7733 499.9412 496.8876 498.9463 493.6942
##  [2297] 502.7559 497.6709 501.0186 490.0108 501.6482 503.8026 506.0669 499.5406
##  [2305] 504.0276 498.1055 502.5215 499.7515 494.8393 504.0831 500.3986 496.1330
##  [2313] 497.2603 498.6518 492.2730 506.6040 501.8156 506.6456 497.9396 500.1965
##  [2321] 507.4565 497.3158 496.5632 496.8552 494.7424 486.1524 504.6424 505.2022
##  [2329] 495.7912 506.6114 502.6127 505.5119 501.8197 500.5605 495.6998 488.3848
##  [2337] 499.6463 500.2329 495.6292 486.3041 498.0225 506.1772 504.4889 497.7317
##  [2345] 498.4681 509.3961 503.1125 504.8202 501.6220 496.0867 502.8782 494.4749
##  [2353] 498.6624 498.6106 497.5719 498.0046 507.6648 500.2166 501.8237 501.0821
##  [2361] 499.6084 507.3208 497.8151 508.8998 496.6865 503.9947 504.2751 500.7832
##  [2369] 494.2157 504.5179 494.3070 488.2049 495.0196 504.7409 501.2435 504.5786
##  [2377] 499.6962 504.8496 496.8209 503.6673 504.8215 504.3755 491.5986 501.0302
##  [2385] 500.3357 506.7632 501.8714 507.9321 503.0930 492.1925 494.7643 493.2616
##  [2393] 502.0711 489.1011 502.0819 503.5020 497.2123 494.0572 499.8737 505.1492
##  [2401] 503.6557 503.3150 502.1085 499.2516 503.2327 497.6944 501.1184 502.1733
##  [2409] 502.0603 502.6371 500.1458 499.8580 498.1065 495.7986 500.9718 500.2391
##  [2417] 500.7972 501.8030 497.9510 505.1184 501.1353 499.7165 498.1407 496.4348
##  [2425] 505.2687 502.0581 486.8198 494.1570 497.7843 490.1974 501.9748 499.4998
##  [2433] 501.5339 494.8927 499.0737 496.9810 496.7186 501.8127 492.8321 500.6916
##  [2441] 497.0655 501.8819 499.9333 498.3387 488.7171 506.6195 491.0963 493.3640
##  [2449] 502.1198 498.0752 491.9969 501.9849 505.0500 495.3893 497.5009 485.1873
##  [2457] 498.7338 495.2771 505.8605 494.2462 496.5185 499.2815 500.6985 499.8808
##  [2465] 504.0718 497.7666 500.3380 504.3449 500.4165 496.4639 506.0870 502.1463
##  [2473] 499.5455 504.1623 503.7210 489.1281 502.3896 495.0314 501.0478 495.5601
##  [2481] 491.2741 500.4256 497.5651 509.8817 496.8612 505.6242 492.0832 499.1505
##  [2489] 501.6782 494.0595 489.7456 498.7843 497.1371 502.1974 504.0109 498.8804
##  [2497] 499.6485 499.4156 503.2701 504.1891 500.3034 500.7853 502.9992 496.7753
##  [2505] 500.3094 505.7367 494.5502 503.1331 493.3826 490.1286 500.4091 503.1684
##  [2513] 498.9009 500.6266 501.4201 501.6580 501.7687 495.4850 500.1923 501.0257
##  [2521] 500.1668 503.1313 500.8700 500.3170 507.7152 508.6671 500.6749 496.1599
##  [2529] 504.7330 495.2879 502.2341 498.2223 497.4005 505.5879 502.8120 497.6054
##  [2537] 493.1348 495.7684 493.3693 499.0832 497.7833 498.5132 511.3044 495.9906
##  [2545] 500.3276 500.0387 502.0639 498.0910 499.7205 505.8066 504.6350 505.4611
##  [2553] 502.6920 492.4287 502.7423 502.7576 494.4831 492.9766 495.8763 505.4337
##  [2561] 500.1555 509.8653 507.2170 503.5996 504.7261 493.2502 486.2206 498.6797
##  [2569] 503.0384 504.3832 500.6682 506.5340 507.5874 490.6852 503.3275 499.8520
##  [2577] 491.8429 493.0023 498.6889 497.0013 492.3081 498.1803 498.3652 508.9300
##  [2585] 495.5741 492.0420 498.5273 503.6554 502.6092 497.4776 495.3738 501.3391
##  [2593] 497.2935 505.5907 503.9850 497.1707 501.0368 501.3561 496.4568 501.4274
##  [2601] 489.8576 495.6603 497.2734 505.1829 502.1827 492.9045 491.1921 497.8987
##  [2609] 504.0764 495.6815 499.1221 503.1870 498.9650 498.7907 504.7626 504.0736
##  [2617] 496.6900 497.4283 494.9081 502.8477 487.2389 504.5536 499.9079 500.4293
##  [2625] 495.7057 493.2113 508.2159 499.8045 501.7836 499.9168 494.7101 496.6687
##  [2633] 503.4210 503.5402 506.5422 500.6819 498.7548 505.0584 504.0656 498.0215
##  [2641] 501.9922 507.1421 489.3744 498.2114 504.5297 502.2931 506.8111 504.4906
##  [2649] 499.8607 483.5838 497.2184 497.3538 504.7815 492.6390 498.0582 501.7215
##  [2657] 498.1388 489.6488 500.8111 499.1996 507.4374 504.2933 499.9805 494.6539
##  [2665] 497.9468 500.7481 493.9477 498.4435 501.0280 501.5893 505.8776 501.7664
##  [2673] 502.9436 497.0552 493.5412 494.9415 500.1098 504.3323 502.0074 501.3956
##  [2681] 501.6024 499.9541 493.2495 499.7427 501.7480 504.7109 486.0917 497.5341
##  [2689] 505.4501 495.5623 495.5139 501.7445 491.9190 504.6176 502.2709 495.5426
##  [2697] 491.5789 506.2471 496.2967 500.4960 498.4194 500.3422 496.3366 500.5535
##  [2705] 498.3240 486.2096 486.4802 494.8412 501.6500 502.9452 500.0355 497.8915
##  [2713] 499.4063 496.5245 504.5984 505.5734 499.7582 496.6151 501.6044 502.3074
##  [2721] 499.0336 495.8637 511.0642 505.0615 506.8990 502.9733 503.7645 504.6580
##  [2729] 503.3284 499.2489 502.4114 499.6058 500.2068 498.4703 499.4457 498.9230
##  [2737] 510.5809 501.1993 496.0136 501.2342 507.5056 486.8587 497.0774 507.8438
##  [2745] 496.7581 508.4703 499.9320 491.0252 492.4279 500.5587 497.4749 499.5170
##  [2753] 501.6403 490.8047 503.3655 499.3090 501.5217 493.3704 502.9271 500.9921
##  [2761] 499.1815 496.1393 498.8405 506.6622 500.0988 502.8213 500.5036 486.0412
##  [2769] 505.2632 504.0403 499.8394 504.6042 500.1545 495.9079 509.6111 490.7387
##  [2777] 497.3058 498.1231 502.5398 491.2227 500.1045 495.4404 495.3302 504.4176
##  [2785] 505.9141 507.7628 501.8406 501.6623 492.4296 498.3516 501.3202 501.9997
##  [2793] 507.0918 499.0478 504.8221 502.3808 499.0309 499.0230 503.1388 499.8063
##  [2801] 502.1234 503.8603 500.8251 504.0415 496.4212 502.4738 496.1674 494.8954
##  [2809] 499.4271 482.6085 504.8914 502.9230 493.2681 490.1297 505.9569 498.2270
##  [2817] 497.1738 497.7958 496.6716 497.6623 497.2938 501.9951 491.9332 509.0226
##  [2825] 495.4338 501.0993 504.9185 497.9355 498.4493 494.9723 503.1807 493.9476
##  [2833] 496.0097 494.4820 503.1957 496.8912 499.6584 502.0463 504.9029 493.5814
##  [2841] 494.7656 498.9777 501.6079 497.5998 503.6034 503.3012 501.3209 490.4181
##  [2849] 503.0205 501.6622 501.7741 497.5895 503.3889 508.2877 490.9202 500.4873
##  [2857] 497.5042 490.3874 498.3081 501.5483 503.4791 501.7931 497.7606 501.1135
##  [2865] 498.5761 502.7196 499.4449 498.4873 500.4928 498.6868 500.5265 509.7404
##  [2873] 499.4512 494.6273 501.4717 492.3733 499.3953 497.3586 504.7216 502.0944
##  [2881] 497.7043 499.8316 501.8181 498.2466 508.4299 484.4061 491.6376 502.6175
##  [2889] 502.9410 506.4391 492.6463 490.9412 498.9136 508.6134 504.2835 497.0309
##  [2897] 493.9697 494.4802 502.1024 506.1766 494.7316 509.2970 497.2913 495.7175
##  [2905] 506.2179 507.4834 492.4661 504.7982 508.3470 498.5848 490.9955 504.6999
##  [2913] 500.5984 494.0150 499.6361 501.9095 502.7378 489.9027 497.2287 499.1647
##  [2921] 496.4153 501.6887 504.0646 496.6635 504.1804 500.4110 505.9484 500.0713
##  [2929] 507.1054 501.5139 496.3042 503.6800 496.8484 503.2460 498.2870 499.0648
##  [2937] 491.8790 492.8687 497.8395 498.9420 490.6167 495.8668 497.8592 493.6761
##  [2945] 496.8124 502.5369 500.7257 493.0281 503.8609 500.0189 503.0205 507.9424
##  [2953] 500.1353 505.5749 503.6022 492.4991 496.6810 497.0643 500.0214 500.6887
##  [2961] 498.1448 503.3931 505.9399 495.7292 489.9643 496.4719 494.7353 484.7803
##  [2969] 498.6962 496.1679 499.3104 492.6032 495.5965 496.3875 500.7436 505.1761
##  [2977] 500.7567 498.4027 500.7821 501.1916 493.7502 503.5465 500.0760 506.1813
##  [2985] 505.2874 496.7875 495.9799 503.0048 497.0277 489.3236 498.1348 500.2765
##  [2993] 499.1622 502.0287 501.0014 502.5341 496.1198 490.3694 499.6889 507.3356
##  [3001] 508.1902 488.0016 499.1335 490.1861 496.8122 496.7790 505.4271 498.9923
##  [3009] 502.4412 501.9055 498.7614 501.6520 500.8349 488.5460 504.0836 499.8913
##  [3017] 501.7856 506.9424 486.6464 481.3071 504.4045 503.3766 500.8975 503.1583
##  [3025] 509.0936 502.8352 495.5722 494.1664 497.7576 500.2709 506.2470 501.6267
##  [3033] 501.3597 506.3808 492.0657 498.4090 500.1779 489.0146 505.1168 497.4098
##  [3041] 504.6068 495.4597 498.6411 506.7318 498.9651 493.4043 505.8784 495.4455
##  [3049] 503.7515 498.8581 492.1333 497.6073 501.8029 499.6184 500.8206 496.5978
##  [3057] 497.2080 498.0514 497.5563 498.4588 503.0927 500.2120 495.3890 496.3041
##  [3065] 497.7165 502.5728 492.6369 498.3319 503.2300 501.5987 500.9197 499.2433
##  [3073] 497.7938 500.6744 503.2108 497.5212 506.3120 499.3128 492.0914 494.5485
##  [3081] 497.8248 494.5218 494.6986 502.5909 496.9061 500.1724 477.9788 498.1809
##  [3089] 494.2942 508.5981 499.4285 507.7196 498.2480 494.0880 495.2231 498.0805
##  [3097] 504.1832 493.8134 492.4745 499.2308 496.4035 496.7398 504.2463 501.4660
##  [3105] 498.5076 499.2257 498.3665 500.7605 495.3967 511.6823 497.0788 502.0742
##  [3113] 504.2455 489.8762 507.0736 500.3602 492.5640 495.7980 494.8080 490.6667
##  [3121] 497.3439 504.5075 506.9966 500.9624 503.4542 496.7285 494.4799 495.1966
##  [3129] 500.6357 492.2303 492.6035 506.0383 503.3060 502.4095 506.0188 497.8034
##  [3137] 506.5657 500.2283 502.5115 503.1512 503.0948 501.7591 505.1422 497.8818
##  [3145] 504.0401 503.4384 498.1118 496.8572 494.6649 498.5352 499.5697 499.8096
##  [3153] 490.6870 499.4998 504.0012 493.8669 506.1183 505.7934 492.3838 498.3899
##  [3161] 498.4379 510.1017 491.1674 505.0845 500.9057 499.9744 497.4262 498.6558
##  [3169] 495.7352 501.4829 491.5403 504.7386 497.8924 502.4952 500.1308 496.3356
##  [3177] 507.3012 499.6760 499.4317 498.1741 498.1378 501.7173 495.1480 490.0683
##  [3185] 499.5625 505.0484 505.5777 502.6570 501.7842 500.2932 501.3840 507.8478
##  [3193] 496.1613 503.8095 506.4817 505.5734 495.7349 503.3011 501.6044 500.8850
##  [3201] 502.7046 505.6616 499.9468 498.1574 505.4733 501.4347 500.2332 498.0119
##  [3209] 499.9660 501.4538 501.2650 497.0972 496.7971 499.8632 507.0760 498.0062
##  [3217] 499.8382 500.0184 499.2882 503.8674 497.9931 496.0242 502.7624 510.5433
##  [3225] 498.7763 493.8340 495.8460 493.1009 504.1719 493.8052 506.4923 498.3282
##  [3233] 500.6193 503.6693 487.2325 489.4234 504.0576 501.3035 498.1794 498.1636
##  [3241] 501.8060 499.6079 495.2911 502.0231 496.2832 500.9940 499.3826 498.1736
##  [3249] 494.0982 498.3669 499.6717 502.9348 512.9774 491.8452 504.1533 494.8907
##  [3257] 492.3345 500.5812 493.9140 496.4515 498.1922 506.7917 508.3534 498.1798
##  [3265] 505.4466 500.6996 498.8838 501.8367 499.4739 502.4390 494.7243 503.4951
##  [3273] 500.8189 506.2086 505.0074 506.3318 496.7788 507.1839 501.4436 499.3588
##  [3281] 490.6175 488.8457 503.2484 493.2767 495.7032 495.6555 502.1357 497.6754
##  [3289] 502.1817 492.1037 500.8044 503.9067 509.1239 494.4490 491.8132 498.6958
##  [3297] 496.6256 495.5447 498.9201 498.1144 499.7614 503.2529 501.8520 501.3193
##  [3305] 506.1502 491.7316 496.8867 502.8683 502.6937 498.9087 507.2103 501.8396
##  [3313] 500.0213 508.9997 501.0407 503.9153 497.4993 495.3469 493.3636 500.7233
##  [3321] 496.0464 495.8283 494.9453 500.0815 485.1112 498.4505 503.1034 493.2573
##  [3329] 497.5679 494.5798 499.8967 496.1690 491.9359 494.2792 503.6284 502.6619
##  [3337] 501.8591 508.8975 500.2859 502.0856 497.2440 503.5848 501.2733 504.0129
##  [3345] 494.1545 504.9626 496.2391 501.6653 486.6931 492.4134 500.0803 501.9100
##  [3353] 499.3527 500.7196 505.4572 496.0956 501.8875 502.8872 507.1018 497.8722
##  [3361] 500.1548 497.6260 506.4395 503.4245 488.2734 495.9235 504.0071 502.5168
##  [3369] 496.9920 500.5301 502.8598 505.7131 502.0820 505.4442 495.2495 492.9573
##  [3377] 498.8119 492.4380 500.0822 499.4439 493.8851 508.4008 500.2143 500.4925
##  [3385] 501.8923 487.6356 496.1729 499.9430 492.3593 496.4542 503.4549 494.5643
##  [3393] 494.8644 493.9907 505.5804 503.0406 505.5565 500.5850 499.9708 499.1021
##  [3401] 507.4701 498.3888 492.1781 495.8622 499.3662 490.9009 504.5683 504.1273
##  [3409] 495.4175 506.2948 501.0800 497.7077 503.7773 497.0858 494.4264 500.8585
##  [3417] 501.6233 504.8672 499.6316 503.2305 502.1502 504.2853 494.2738 508.9609
##  [3425] 495.4685 499.5251 499.0955 503.8664 497.2877 495.0804 502.7574 503.7654
##  [3433] 506.2884 494.1936 504.8670 506.4741 498.6427 504.0948 501.1149 501.6391
##  [3441] 494.9821 490.9661 502.2959 505.7448 502.3654 497.4069 497.4521 500.2660
##  [3449] 500.4482 493.8174 499.0316 492.3206 502.4426 494.2820 501.8240 495.2567
##  [3457] 497.2928 505.9929 497.3970 496.1266 493.3394 497.6980 501.8188 496.4460
##  [3465] 495.4531 499.2094 498.1454 498.3303 500.4026 506.1031 503.3665 505.2091
##  [3473] 510.6922 505.4638 503.3436 506.6800 497.5296 505.4513 493.4301 500.9120
##  [3481] 500.2571 499.2519 493.9866 498.9962 488.4217 499.6354 496.9053 495.0062
##  [3489] 498.0521 494.6686 499.4188 495.7343 504.0415 495.6341 494.9319 499.3417
##  [3497] 497.1547 503.3613 502.7653 501.2255 498.4676 500.2398 490.8250 507.2248
##  [3505] 504.6809 498.4104 508.1917 499.6229 502.5172 495.0711 505.5344 505.4547
##  [3513] 499.1727 489.0468 502.6272 507.6495 501.3414 502.8338 502.2420 497.9097
##  [3521] 497.9343 501.3051 501.0749 504.0468 495.0028 494.0169 493.2014 507.6403
##  [3529] 496.5509 501.5469 499.7399 497.2517 499.6296 494.3119 500.2588 495.7144
##  [3537] 505.5258 504.7588 499.0667 497.8739 498.0083 498.1515 486.7395 497.9901
##  [3545] 496.5294 500.7741 494.8959 502.1551 499.4977 504.1306 486.6585 492.8127
##  [3553] 498.4161 501.6745 496.6253 509.3091 502.7335 498.7769 495.4603 497.9018
##  [3561] 501.2498 503.5577 508.4827 487.3251 507.3358 503.2012 494.8667 500.5219
##  [3569] 499.8752 505.4370 492.5130 498.9467 494.7219 503.5974 501.1576 506.5372
##  [3577] 497.6355 497.4128 493.0246 494.4504 502.2748 502.3533 495.1441 492.3573
##  [3585] 499.7110 502.8282 493.8747 503.4529 491.7427 494.0266 501.8413 501.3551
##  [3593] 495.7193 502.2484 505.1186 498.6392 493.8615 502.3666 497.9986 492.3769
##  [3601] 504.2812 506.2947 496.1212 507.1363 504.3386 501.0428 489.5230 496.7347
##  [3609] 512.3067 501.3359 486.5964 498.8773 498.7945 498.5116 493.3879 499.0951
##  [3617] 493.8363 503.0457 498.8581 506.9474 490.3180 500.1859 490.5401 495.1918
##  [3625] 501.7172 491.0087 503.9837 497.8777 502.2436 506.8438 498.4715 501.0425
##  [3633] 494.5495 494.3649 500.5072 497.3701 502.3686 503.3413 500.3412 502.3353
##  [3641] 493.3386 498.9168 503.9823 496.7508 502.7395 501.2914 490.8020 500.4064
##  [3649] 491.7207 502.8863 503.3993 498.9976 499.3189 495.9953 497.5516 499.3969
##  [3657] 497.3958 496.4638 499.4349 503.1814 496.5767 489.3478 505.7867 500.0838
##  [3665] 493.4065 499.7510 494.3772 495.0926 485.3996 505.0202 500.0430 493.7843
##  [3673] 501.4295 495.5233 492.2923 495.1322 500.2493 492.6050 499.1830 506.6936
##  [3681] 498.7448 507.5311 494.0875 498.1105 494.4079 507.5840 504.8536 499.8130
##  [3689] 502.4887 500.4608 489.0830 497.5085 506.4520 501.6196 503.8438 500.4242
##  [3697] 497.6653 498.9264 499.9166 505.9343 496.7309 498.6998 505.7032 499.7484
##  [3705] 501.3083 497.0930 500.2501 505.9504 500.1541 508.4415 500.7172 506.7197
##  [3713] 503.4147 496.0605 497.3714 500.4203 505.0516 484.1296 501.7182 503.0630
##  [3721] 496.1897 497.6379 494.5518 500.3909 503.3949 506.3288 496.5503 492.6883
##  [3729] 494.6209 498.0027 498.5032 503.6905 506.3699 498.4054 507.5555 493.3222
##  [3737] 496.8561 496.4330 497.0624 503.6252 503.0238 494.8283 493.4501 494.2914
##  [3745] 502.4323 498.5878 500.0435 487.3295 495.8032 494.8149 491.7727 492.5485
##  [3753] 499.9735 503.8618 507.4294 502.9232 497.4447 497.5181 502.0857 503.6877
##  [3761] 483.0751 503.7707 500.6909 501.0780 497.5966 505.4165 510.7904 495.9978
##  [3769] 497.7039 508.4352 496.3193 499.8334 508.0152 499.2442 490.5718 504.2026
##  [3777] 504.0926 495.1281 500.2172 496.6911 501.2066 499.5020 496.8439 493.9225
##  [3785] 503.2210 501.2802 506.9705 494.2751 498.1965 501.6424 491.6473 491.8072
##  [3793] 502.2990 501.5274 502.2240 498.0641 502.5405 497.7064 500.1638 498.3348
##  [3801] 503.6792 502.5962 498.4759 506.0871 504.2959 493.4918 500.5691 506.0682
##  [3809] 499.5317 504.1987 505.1272 499.7720 499.5854 509.0270 488.4042 503.2618
##  [3817] 493.6224 497.8612 494.2655 500.4882 506.3875 505.3345 501.5186 501.7677
##  [3825] 502.7596 505.6743 492.1982 499.6312 497.6037 504.3302 500.1978 503.1825
##  [3833] 490.2799 497.9270 501.4283 496.9365 492.0441 502.0789 500.2710 501.4951
##  [3841] 505.9420 492.3153 503.0043 501.3384 498.8313 498.1891 496.4570 506.0919
##  [3849] 503.8608 496.3124 503.5761 504.9587 501.0258 504.5084 505.6146 496.9620
##  [3857] 501.3894 505.2580 506.1393 494.4883 505.0894 489.1291 494.7094 494.7460
##  [3865] 495.7474 498.8785 492.3590 494.6513 512.0439 506.2991 506.3799 505.0133
##  [3873] 499.5743 499.8385 503.3772 505.8323 496.4841 505.8338 509.6610 503.9375
##  [3881] 501.3202 507.5703 505.1035 498.4085 500.9707 490.9799 497.7245 503.6981
##  [3889] 503.6982 495.2490 501.4911 504.1978 508.6746 502.9159 498.6439 501.2907
##  [3897] 497.8720 499.2564 499.3276 503.8347 500.3408 507.2097 499.2706 498.6084
##  [3905] 502.2898 496.4129 497.1875 498.9679 497.5053 500.9959 504.1738 496.7027
##  [3913] 499.9603 498.9529 500.6806 506.2802 503.6842 493.9312 484.7911 496.8186
##  [3921] 503.0717 496.5384 505.7790 495.2426 498.9953 496.2509 490.2980 504.3768
##  [3929] 494.5485 492.9774 498.4791 497.6629 497.1441 496.5364 501.8789 493.4854
##  [3937] 500.3148 499.7845 507.5714 503.6803 500.4225 492.4206 500.4127 503.3725
##  [3945] 503.8573 501.1074 504.1000 498.9914 500.5214 494.2870 501.0684 503.3800
##  [3953] 498.5795 503.0049 497.6454 504.8936 500.4541 512.0242 491.7134 495.6191
##  [3961] 495.1846 488.6028 504.5371 492.1195 498.8914 497.1352 499.1100 495.2829
##  [3969] 498.7374 498.2349 500.0732 507.7394 501.7831 497.5754 499.6836 499.5469
##  [3977] 496.4210 499.0844 495.2755 498.5845 498.1075 502.5368 488.9908 485.0388
##  [3985] 497.2546 500.8459 494.0978 495.9646 500.4594 498.5327 500.0817 501.2868
##  [3993] 498.9158 503.2621 496.1801 498.5624 499.7769 498.9221 503.0153 501.3750
##  [4001] 506.0063 501.3036 503.3156 499.6800 498.1162 497.4583 505.9272 499.0403
##  [4009] 497.1970 496.7791 503.1498 498.7233 500.8513 498.2924 499.3780 502.8972
##  [4017] 499.7311 504.5864 502.8586 500.3547 502.1404 497.7912 508.3170 499.2160
##  [4025] 494.9068 488.8037 498.4813 499.3128 502.8336 502.5787 495.0683 497.0024
##  [4033] 505.8382 498.4598 498.1768 494.2671 501.2198 505.7204 499.8527 495.6337
##  [4041] 494.4101 499.7329 496.5284 498.0755 495.7180 502.0839 505.5220 499.2798
##  [4049] 501.7016 494.0439 491.9053 496.4484 499.4598 495.8617 502.6079 493.2873
##  [4057] 498.9846 501.7565 499.1482 497.6590 502.5038 502.4406 490.8765 499.7112
##  [4065] 496.5981 496.2208 499.7228 500.1606 491.9638 499.7028 501.6244 504.1186
##  [4073] 492.8365 493.8069 498.1784 504.5561 498.0326 494.9485 494.4644 501.8023
##  [4081] 504.7108 504.3398 500.7935 501.7129 491.8499 497.3652 494.3698 505.3999
##  [4089] 504.8888 504.2668 504.5259 499.2033 500.6824 491.4323 498.0027 502.8932
##  [4097] 497.1806 496.3935 500.8752 497.9487 493.5914 501.7577 502.1466 498.3708
##  [4105] 497.7615 494.3019 496.3638 498.1681 499.2749 488.8525 499.0674 498.0754
##  [4113] 504.3904 504.8668 502.3028 498.6737 502.3438 503.5382 501.0370 504.2928
##  [4121] 502.4357 503.6998 501.5964 496.5620 504.6620 506.2693 499.7795 499.9069
##  [4129] 506.9415 495.2986 487.7932 501.5095 500.6917 506.1803 499.6764 501.4633
##  [4137] 496.9434 497.0703 497.8791 503.8335 502.6666 492.2852 497.6510 497.6404
##  [4145] 498.5303 505.3977 500.8122 499.5312 497.0058 500.6472 498.5511 493.3438
##  [4153] 506.1265 497.4016 498.9478 501.7032 500.2247 497.6932 502.5712 490.5912
##  [4161] 499.1512 496.5195 497.6280 509.4862 495.0727 501.0697 501.3740 496.6507
##  [4169] 490.0811 506.8683 497.9045 504.6549 502.5449 506.5526 503.3521 491.0937
##  [4177] 499.1567 506.5776 494.7556 498.3843 501.8752 499.5719 499.9048 492.9872
##  [4185] 496.8955 505.7687 490.8379 506.3297 500.0758 494.0208 499.3036 496.3092
##  [4193] 496.2360 503.2284 497.6422 500.2138 493.6231 504.6023 503.5763 501.0324
##  [4201] 488.1533 504.0208 498.6039 499.7425 501.1928 489.1290 491.6989 500.8882
##  [4209] 499.4172 487.8227 501.9022 494.5115 488.1095 500.2718 502.8103 506.7378
##  [4217] 499.9979 499.0849 501.3034 503.8840 494.0567 502.9648 501.3721 492.0955
##  [4225] 498.2538 493.4872 502.4373 495.8383 495.6712 494.3505 505.7767 497.3187
##  [4233] 497.5478 495.5883 499.2308 489.1413 506.4683 497.1599 499.2899 500.5235
##  [4241] 499.6864 498.0029 501.6894 496.6937 500.1414 490.5475 505.2118 492.9798
##  [4249] 501.8073 501.2756 495.8355 505.5834 492.0828 498.5061 499.3361 504.2796
##  [4257] 505.2567 490.0586 498.3218 498.3011 494.6433 504.6700 487.9301 496.3731
##  [4265] 502.3012 499.7408 492.5129 494.9675 505.2841 502.8020 502.2415 491.4337
##  [4273] 507.8105 496.3486 497.9867 503.3438 492.6165 496.1650 499.4152 496.0429
##  [4281] 495.4330 499.7210 487.9831 496.7246 509.3512 501.7468 505.7711 494.4436
##  [4289] 489.5975 508.8303 499.3996 499.1215 499.1000 499.0371 497.0163 493.4742
##  [4297] 493.3130 504.9540 504.6166 507.5122 507.5957 498.7028 492.8727 505.0899
##  [4305] 500.4317 505.2562 504.8161 491.5492 490.4766 492.0110 502.6459 500.7418
##  [4313] 503.2515 503.1065 495.5237 506.0938 504.6511 498.0163 503.0004 499.7003
##  [4321] 497.2234 492.1414 494.5022 498.2212 501.8652 501.2424 501.4417 504.5704
##  [4329] 500.4132 492.9968 500.4315 504.5327 499.3992 499.9023 502.5401 505.0248
##  [4337] 493.4154 500.2375 497.4684 496.5479 499.7944 508.4338 497.1766 512.0902
##  [4345] 480.1452 494.7410 501.6009 504.6141 502.8176 498.1574 503.8471 501.5996
##  [4353] 488.3558 502.0291 497.2192 505.6166 501.6392 504.6677 502.5567 502.4955
##  [4361] 493.6039 503.5505 500.6155 502.3695 494.1840 500.1897 501.5510 497.7982
##  [4369] 504.0285 492.2530 499.2034 496.2642 501.2710 502.9043 498.8340 501.0175
##  [4377] 508.7883 504.8331 493.2059 502.5877 497.4879 500.8850 498.0512 500.9222
##  [4385] 498.6874 499.4183 502.0925 494.8867 491.9943 487.0053 500.8994 496.4953
##  [4393] 490.5470 492.7855 499.4813 497.6725 496.3824 492.4384 503.8563 498.0787
##  [4401] 505.1687 500.4759 502.9068 497.2632 495.5266 506.4194 499.0068 502.2097
##  [4409] 506.6499 495.6645 501.9436 496.3966 484.5588 498.6956 500.2857 489.5056
##  [4417] 501.0315 498.3930 499.5755 499.5171 500.4422 487.6100 502.7591 501.4895
##  [4425] 503.0617 500.7938 498.9411 500.3606 505.1157 496.5878 497.6934 506.4953
##  [4433] 500.4657 503.7301 505.4267 501.3211 497.3451 494.7449 497.0571 502.0209
##  [4441] 498.6342 497.7810 495.3706 496.0616 495.3197 499.2722 502.7073 499.3728
##  [4449] 501.5201 503.4103 499.8429 504.8088 498.8941 496.6850 500.6255 499.8063
##  [4457] 508.1694 495.6564 503.2607 502.0218 499.1071 499.9485 498.5852 489.9893
##  [4465] 498.0717 493.5309 494.4265 496.8830 500.7318 502.0666 501.7901 503.0485
##  [4473] 499.0004 506.7578 494.0866 498.6180 501.6113 497.0282 501.8738 499.9333
##  [4481] 498.7595 505.3125 503.6733 500.3959 499.2673 497.9111 509.6746 497.3150
##  [4489] 493.3498 500.5504 507.2554 500.6016 504.8270 488.6946 496.4214 494.7210
##  [4497] 498.1603 502.8379 495.5132 493.5410 491.2667 501.2528 505.7317 497.7530
##  [4505] 499.8833 499.6182 500.3485 496.4266 487.2452 503.6053 494.3061 504.6185
##  [4513] 503.3744 500.2103 499.4960 500.1115 502.2016 496.1656 502.1327 499.0508
##  [4521] 502.3330 499.0947 501.8320 504.3527 505.3258 501.5598 505.4553 492.3255
##  [4529] 501.8616 495.8888 496.2197 493.4055 496.4997 503.3727 503.2963 502.2788
##  [4537] 499.3787 504.8410 506.6267 497.5906 504.3678 493.0483 501.7128 506.5274
##  [4545] 496.7324 504.8471 500.2499 501.9111 496.1007 504.6076 506.7645 500.3022
##  [4553] 498.8520 495.4631 494.0787 497.4681 501.6758 499.9164 498.7936 501.6666
##  [4561] 490.7527 505.7546 496.9300 503.8593 492.0443 501.4562 499.4792 495.7212
##  [4569] 506.1349 496.4397 500.7573 492.3855 495.8783 496.6500 501.8860 504.3227
##  [4577] 506.1531 493.9680 497.2334 495.2359 503.3261 501.5896 502.4042 498.6353
##  [4585] 490.7878 494.6422 496.1548 499.1655 498.2188 502.4588 493.9054 498.5846
##  [4593] 496.1831 493.6384 499.3272 499.4121 506.9311 494.9268 498.7573 504.8788
##  [4601] 500.8017 507.4312 497.0432 500.5341 496.2147 490.4622 498.0822 492.4462
##  [4609] 501.9961 501.3264 501.1807 499.5105 500.5972 496.5755 499.4121 504.3075
##  [4617] 500.2837 503.9958 499.3238 499.4859 500.2109 500.9396 495.8348 495.9594
##  [4625] 504.8559 487.3309 495.7915 503.2789 496.2229 491.9169 502.8087 503.4110
##  [4633] 496.1938 499.4603 498.5493 496.3015 502.5271 494.9538 492.9349 503.9207
##  [4641] 502.9190 493.0186 501.7139 499.6867 496.9370 501.5912 504.9571 493.2231
##  [4649] 494.1452 499.2674 500.6453 500.4824 491.9031 502.5594 508.0597 509.4680
##  [4657] 492.8453 502.1333 500.5710 497.6314 498.2534 502.7930 491.9174 501.1089
##  [4665] 494.7104 505.3754 501.0053 501.9097 498.3287 501.8253 499.8537 496.9643
##  [4673] 497.4023 500.7761 495.0019 500.6507 505.0648 499.9390 494.4038 504.2328
##  [4681] 498.7902 499.7618 500.5237 501.0777 499.7458 496.3246 502.5490 496.6041
##  [4689] 503.6325 494.6324 492.8610 500.1835 497.4502 507.3843 485.8224 497.2879
##  [4697] 496.6408 503.0098 497.1866 492.6315 499.0489 503.2826 500.5368 505.2485
##  [4705] 498.9458 501.7241 494.7040 500.9474 507.3238 505.3183 496.8783 498.4234
##  [4713] 499.5880 485.1228 499.6917 495.9084 504.1440 504.4276 503.3505 501.5671
##  [4721] 498.8145 500.6863 496.6032 502.7729 498.2045 493.8657 505.8318 496.3727
##  [4729] 498.3509 506.7159 493.5320 509.2482 504.8907 500.9565 500.5453 499.9702
##  [4737] 495.5532 499.4563 500.4213 500.6116 493.7387 504.4079 492.7249 497.1293
##  [4745] 500.0840 501.1064 500.6352 500.1454 495.5119 505.3046 494.0139 493.3791
##  [4753] 496.5654 507.8910 504.4490 500.2491 501.9987 500.4943 504.8338 491.8469
##  [4761] 499.9445 498.9157 503.5737 499.7751 499.7430 506.7114 501.4144 496.7868
##  [4769] 501.7720 506.1401 507.4787 505.0702 502.4046 502.7925 500.3324 502.6931
##  [4777] 498.5696 501.5707 496.9201 501.3974 495.7824 497.3591 498.8041 507.8790
##  [4785] 494.5851 493.4571 501.7434 497.3597 499.6358 490.5124 500.7791 501.2080
##  [4793] 497.0337 494.0346 484.0495 494.9332 502.1631 504.8311 506.3854 498.0210
##  [4801] 505.7430 500.7530 510.4425 505.4655 510.3721 504.4303 495.5770 497.2343
##  [4809] 502.8467 504.7560 497.9902 494.0606 493.7530 498.9421 492.2211 498.3438
##  [4817] 497.8587 503.1529 505.6525 497.8846 501.5342 491.9012 512.3422 494.8162
##  [4825] 503.6378 490.7022 499.1169 498.0502 495.5593 496.4127 504.4228 498.4655
##  [4833] 505.9057 503.2856 504.7411 496.2821 496.7531 494.4421 500.2316 491.8063
##  [4841] 494.2077 497.1342 500.1778 495.7194 507.1898 501.0220 498.6952 502.0194
##  [4849] 500.9217 495.0531 499.9714 497.6091 506.1872 493.0487 498.8596 502.8407
##  [4857] 501.1150 504.6268 496.4475 496.9174 498.1919 494.5762 503.0005 505.1955
##  [4865] 495.2322 495.3020 501.8023 498.0611 500.7296 505.7408 490.0463 498.9769
##  [4873] 499.2787 505.6200 501.0096 497.5190 479.7432 493.8775 499.1356 504.1784
##  [4881] 505.1238 503.4200 500.4775 494.8551 498.4183 497.6192 500.3988 499.9080
##  [4889] 496.7180 501.1664 497.3586 496.0651 502.2700 497.7023 500.0606 501.9152
##  [4897] 496.3504 503.9684 500.0883 508.7276 504.3953 501.2457 498.0678 494.0154
##  [4905] 505.4539 502.2058 496.7738 502.7320 495.5982 508.4467 499.5146 498.8440
##  [4913] 495.2198 499.3360 503.1700 498.9686 494.1156 490.3195 500.0657 492.5961
##  [4921] 502.9157 494.3241 505.4995 503.0239 487.0115 495.0326 506.4725 492.5624
##  [4929] 502.5165 504.9293 498.0171 494.0175 502.3443 491.2879 497.1635 498.1586
##  [4937] 503.6384 489.2627 499.8791 494.8389 489.6702 496.8491 497.0889 489.8503
##  [4945] 507.2588 491.7853 494.0069 505.8598 501.7153 498.1603 495.7248 496.9007
##  [4953] 503.1790 493.9139 504.3376 502.2890 500.6181 501.6618 503.0344 500.3907
##  [4961] 503.7210 501.5300 498.8726 504.7626 502.7524 496.9008 499.6614 503.0372
##  [4969] 495.0186 500.4678 503.3006 499.2966 497.6432 497.1857 501.4480 500.1815
##  [4977] 506.0220 507.4014 498.4981 504.0553 495.2500 499.0308 499.4527 502.3468
##  [4985] 495.7706 500.4374 494.4243 493.3691 490.9471 495.4769 499.6638 502.4810
##  [4993] 503.7175 504.9262 502.3486 499.5321 502.7934 505.8954 496.9393 494.1208
##  [5001] 503.8345 502.3281 500.7678 502.3517 503.6228 494.8211 501.6765 503.0427
##  [5009] 496.5093 492.5782 499.4404 497.6632 496.5025 499.5171 499.8014 506.7848
##  [5017] 505.8178 499.4665 498.1404 499.6572 491.6620 494.4262 502.1792 500.8268
##  [5025] 499.5719 501.4632 496.2456 501.8639 478.8723 499.6730 498.5299 496.9267
##  [5033] 495.9229 493.8711 499.2098 497.0194 505.2247 506.2317 499.4224 496.5352
##  [5041] 500.2591 505.2641 497.1879 503.0170 503.4586 486.4837 496.6424 499.4999
##  [5049] 496.6002 494.6490 493.1573 499.0903 501.0459 498.2244 490.5975 490.4597
##  [5057] 503.4486 499.7220 495.8849 498.3922 496.2037 492.9956 500.2348 507.8431
##  [5065] 498.4918 497.0396 497.4056 497.4719 501.1840 499.4445 499.8136 503.9448
##  [5073] 503.6750 494.0473 502.3938 497.0983 498.6265 500.6193 504.2220 499.7686
##  [5081] 495.5401 498.7532 495.0413 505.9553 499.6832 497.0633 499.0881 497.2173
##  [5089] 501.9926 496.7343 500.2030 503.1991 492.0409 502.8989 492.4410 497.2350
##  [5097] 504.6754 478.7747 495.7145 502.9417 494.1614 498.5157 501.6139 494.0236
##  [5105] 498.8143 497.1776 496.6646 503.1323 498.7076 494.7850 496.6960 491.0098
##  [5113] 505.9004 505.7552 501.5296 495.5199 498.4832 504.1150 491.9400 497.2232
##  [5121] 493.1471 492.4817 500.9208 502.0762 492.8012 499.5684 498.9747 504.5484
##  [5129] 498.3502 504.5429 489.0997 504.4494 505.1206 508.4022 501.5854 502.4304
##  [5137] 494.4164 496.8285 497.9591 507.8020 505.8072 496.9792 502.3855 504.8034
##  [5145] 502.0953 505.6339 493.9797 495.2584 503.5253 500.1784 493.9662 498.0657
##  [5153] 499.4050 494.9555 502.6069 496.9457 501.9728 488.4467 499.1318 502.5875
##  [5161] 503.0366 506.7423 505.8013 499.0098 497.6938 491.7482 493.4022 496.7964
##  [5169] 499.8810 503.8725 500.5580 492.4161 497.3106 502.3073 493.7656 501.1662
##  [5177] 502.8463 488.4442 493.3534 498.3088 505.2650 501.5523 497.0864 498.8530
##  [5185] 498.3020 503.9840 497.1245 507.6023 501.3107 501.0824 498.0572 500.9343
##  [5193] 503.7438 494.5475 494.2760 494.8393 503.3201 498.5110 501.3566 500.7019
##  [5201] 504.1548 500.9547 500.6983 502.2276 500.6458 500.4318 502.9651 499.0587
##  [5209] 502.4176 488.5332 503.8803 500.2825 501.1016 497.7041 501.4142 498.3701
##  [5217] 494.5473 489.5388 491.9192 493.4794 501.4442 503.7074 496.8413 500.0240
##  [5225] 506.4358 491.5237 501.1997 492.7473 502.5833 504.7922 478.5598 503.9351
##  [5233] 502.5396 499.6468 505.2023 492.4707 503.4464 506.8116 502.4674 505.2324
##  [5241] 498.6392 498.3459 505.2415 502.7586 488.0563 505.3582 493.5064 500.0283
##  [5249] 502.4580 505.2218 499.4630 498.2861 498.6516 501.1262 484.2706 506.2437
##  [5257] 504.1029 492.2681 500.4239 501.7118 504.0673 506.5241 501.9451 497.4477
##  [5265] 497.9054 495.0470 497.0506 504.0440 499.5453 502.2706 496.8134 502.6349
##  [5273] 503.0189 503.6369 505.6418 494.9100 494.6284 498.0484 494.8000 500.9246
##  [5281] 500.2448 504.6075 506.8644 496.2452 500.6268 492.2180 499.0616 494.9846
##  [5289] 503.8872 500.2717 493.8787 503.8794 501.3945 495.6309 491.8833 486.9763
##  [5297] 503.4556 501.9413 497.7850 493.0837 493.9709 505.4893 501.2084 499.1057
##  [5305] 492.1337 503.7526 493.3503 501.1934 500.8349 501.2390 505.2357 491.2409
##  [5313] 497.2348 505.1861 498.8362 498.8443 501.0406 499.9793 494.6764 509.1472
##  [5321] 499.7829 499.7662 501.7631 503.1373 498.1250 490.2109 497.9100 500.4309
##  [5329] 506.7534 499.3316 503.2675 503.9101 498.9867 505.1567 502.6834 497.3367
##  [5337] 490.0462 502.7276 505.9489 489.8601 501.3006 494.6543 499.0905 506.6158
##  [5345] 495.2989 504.0121 500.2594 496.1220 500.5119 493.8029 505.6428 498.8515
##  [5353] 505.3325 497.5202 498.7893 494.2979 493.7729 503.5051 500.2239 494.7991
##  [5361] 505.4529 497.3581 507.6781 497.9984 498.5879 508.2391 507.2966 495.0033
##  [5369] 498.9321 504.6331 507.3982 501.7427 493.2801 503.6302 500.3214 501.7543
##  [5377] 496.7386 504.2765 505.0569 497.0660 502.8942 499.1399 509.6265 498.7162
##  [5385] 504.4457 502.7460 503.7035 508.1562 501.4161 497.8100 490.7420 499.3797
##  [5393] 509.1258 505.2230 507.3467 496.6126 488.7495 498.0897 501.3649 501.3565
##  [5401] 503.5755 499.4593 504.0142 502.6696 495.1395 505.1042 510.3681 497.5140
##  [5409] 499.8213 505.8937 487.2403 502.3424 491.5657 500.6924 503.7974 494.5615
##  [5417] 488.0446 501.7201 495.1500 502.1111 484.7444 500.9599 492.2772 506.2432
##  [5425] 504.9152 504.4740 505.9031 495.8311 502.0792 497.8086 497.0512 499.0971
##  [5433] 502.4587 497.9029 499.2693 499.1846 504.6185 495.7531 506.5540 495.9925
##  [5441] 503.3482 490.3906 506.7935 496.7695 501.0360 503.8127 507.2502 493.7876
##  [5449] 502.6341 498.6413 498.9230 498.4593 491.2022 503.0508 503.7074 499.7588
##  [5457] 497.9130 502.9465 503.5127 492.4745 495.9192 500.9207 502.2903 498.6510
##  [5465] 500.3765 503.0320 503.5889 500.6771 497.6977 497.3705 502.1771 502.3781
##  [5473] 493.9686 497.0438 496.6695 502.0179 492.3691 498.4804 501.1560 496.2860
##  [5481] 505.9542 502.7100 494.1565 504.3083 498.5516 503.0046 494.6438 500.5176
##  [5489] 491.0347 507.0899 500.5930 498.3713 499.6444 501.0027 496.3042 492.5114
##  [5497] 502.5012 502.9436 508.3130 493.3160 500.3380 497.5522 506.3661 491.6755
##  [5505] 500.2101 501.0242 490.8583 497.3243 490.0832 502.2277 502.8557 500.8302
##  [5513] 500.7382 495.7341 501.9753 503.2058 507.5676 499.3466 497.4403 488.2164
##  [5521] 487.0568 496.6127 510.6415 494.5660 493.4945 500.8621 497.0415 505.5939
##  [5529] 501.9557 508.3449 490.7760 499.3079 501.4337 503.9896 497.6423 495.3732
##  [5537] 489.0677 502.1493 501.8106 505.8077 498.3889 500.1610 499.0502 501.2549
##  [5545] 503.6418 499.0512 498.4020 490.3525 507.4486 505.8045 493.8243 499.8247
##  [5553] 497.4889 499.2437 498.3290 502.1049 502.4478 502.0943 500.6816 498.8576
##  [5561] 499.3384 492.0896 498.0225 502.0926 490.4770 487.6609 494.2210 487.3915
##  [5569] 497.4305 508.8021 500.8207 496.6465 496.8431 496.9447 496.1179 501.4881
##  [5577] 495.6782 497.7540 498.4426 503.0112 506.7709 497.9807 501.0335 484.9307
##  [5585] 498.6269 499.9963 503.3512 507.9835 497.6600 502.9834 501.7787 499.4020
##  [5593] 492.5158 510.5723 499.0426 496.6590 501.1593 499.4508 500.5989 496.2528
##  [5601] 499.3655 500.9889 507.2887 500.2384 494.7425 491.8235 496.7717 508.9378
##  [5609] 501.4271 499.2821 497.6752 492.7152 507.4537 501.7134 497.7043 497.6698
##  [5617] 493.9723 505.9804 502.1395 499.5034 496.7595 494.1730 504.2834 498.1997
##  [5625] 494.2375 502.2847 496.6755 502.1752 500.7747 502.6991 501.4564 503.5119
##  [5633] 496.2134 495.6095 498.5278 503.8519 501.0339 501.4728 496.3262 505.4305
##  [5641] 503.1361 501.3311 498.0098 500.9668 503.2473 499.3336 492.4309 503.3200
##  [5649] 502.7606 494.5939 497.3409 501.7198 490.8683 507.2542 505.1467 501.1426
##  [5657] 499.1552 500.8630 501.3223 494.1976 500.5431 501.7878 512.9763 498.5638
##  [5665] 499.7940 493.0973 490.0509 505.9083 507.6661 495.5960 504.3694 502.4322
##  [5673] 497.4502 498.3980 498.2372 498.4163 504.0709 499.8130 499.6959 496.1667
##  [5681] 499.2021 505.3253 493.1583 505.9006 505.9673 501.0423 503.6451 498.8286
##  [5689] 502.3099 493.3306 496.6307 495.5066 499.7508 496.3549 498.8301 503.0506
##  [5697] 502.6267 502.3499 505.1553 496.1645 492.2033 505.6251 496.4081 499.4052
##  [5705] 502.3719 502.8617 496.5157 502.0621 501.6085 502.2265 495.5133 501.4417
##  [5713] 505.2515 505.4698 491.1186 495.8677 503.3953 490.5037 503.7614 503.4091
##  [5721] 496.7806 495.4371 493.8245 496.7115 500.2061 502.3868 496.3692 499.1740
##  [5729] 497.6806 497.1623 495.4532 501.2460 503.6789 496.2211 499.3257 500.7563
##  [5737] 508.8554 494.4961 492.8117 497.5466 494.7065 499.3809 495.1399 492.9022
##  [5745] 499.2046 499.9976 495.7742 496.8427 505.6469 505.2854 504.7939 486.4210
##  [5753] 502.3951 493.3676 493.7780 500.3642 499.5683 490.8881 495.0123 494.5850
##  [5761] 506.7174 506.6220 491.1707 509.0605 488.1464 502.0203 497.6345 499.5636
##  [5769] 494.0419 513.4365 487.0012 498.9523 497.9724 493.6452 500.1079 499.1590
##  [5777] 491.3569 492.8319 505.7376 501.7648 499.3444 500.3967 502.3423 494.2657
##  [5785] 495.1738 494.5511 503.9740 504.8108 498.4276 490.9968 496.7154 503.0446
##  [5793] 507.3191 493.6180 500.0234 495.2755 501.6343 498.1366 497.0207 496.7168
##  [5801] 498.4577 500.7923 497.1999 491.4197 506.7007 499.0720 503.3434 497.8112
##  [5809] 495.2485 499.8283 502.3975 504.3790 496.7355 506.2875 495.1014 490.8572
##  [5817] 498.3642 490.1062 498.2055 502.9851 496.3015 507.8737 504.0917 503.8258
##  [5825] 503.7345 500.1430 501.1841 503.3690 498.1155 503.3579 500.4160 495.8038
##  [5833] 500.3449 505.3320 492.0451 503.9081 508.8023 500.4298 492.8752 499.9944
##  [5841] 498.0625 500.2907 503.8777 507.0156 502.4718 501.1925 492.9228 504.1796
##  [5849] 503.7900 503.0022 495.8025 500.8565 490.0470 494.9776 501.0377 504.7007
##  [5857] 505.5338 503.3817 494.0413 494.8849 503.7158 499.2017 490.8516 501.9012
##  [5865] 494.2958 494.7562 499.7848 491.9369 487.9814 488.0637 497.7788 504.0509
##  [5873] 500.7631 505.4959 487.5684 502.2813 497.4304 493.4245 496.2488 503.3778
##  [5881] 503.2853 507.2683 499.9944 502.1585 496.1282 499.7694 499.0563 499.2444
##  [5889] 499.3174 502.0868 498.3481 500.4675 496.3888 502.3644 503.4746 506.2963
##  [5897] 495.8974 500.2561 501.8424 501.9536 497.8280 492.2718 507.2569 499.1402
##  [5905] 494.1067 504.0887 487.1083 500.3231 501.5857 502.3702 493.7258 497.9549
##  [5913] 503.4685 492.4705 488.4758 500.8040 500.9723 502.5430 492.9935 502.7610
##  [5921] 493.9398 504.9102 499.9778 480.8916 506.8503 501.9677 489.4416 493.9938
##  [5929] 499.5521 501.4929 501.3569 502.3903 499.4640 494.5495 504.0543 498.8446
##  [5937] 486.5963 497.2762 501.8988 500.2039 502.6147 500.7393 500.9815 493.0866
##  [5945] 498.6723 499.5934 495.5448 499.0046 502.5761 491.9119 501.3754 500.2965
##  [5953] 502.4333 499.5387 499.9047 504.8513 502.1909 497.8507 497.5335 498.0996
##  [5961] 500.5229 495.7296 508.7787 497.3693 498.2526 497.3922 499.1063 503.0327
##  [5969] 500.9868 493.0105 506.2260 502.2736 493.4460 501.0772 502.1274 504.4512
##  [5977] 493.4607 490.2520 489.0212 494.7001 495.7237 501.1247 499.0851 498.3985
##  [5985] 499.7652 502.0611 493.4525 496.1390 505.2127 500.8239 503.5725 495.7668
##  [5993] 502.8897 506.4524 494.7811 500.0675 500.5360 500.0954 488.2603 504.6178
##  [6001] 495.4892 497.9991 496.7271 499.7989 499.9213 498.6339 498.8349 497.9416
##  [6009] 501.3014 502.7298 501.6093 493.6632 504.5774 502.1090 496.8815 495.8638
##  [6017] 497.6441 502.0313 501.1327 492.0261 503.0862 495.5035 508.5017 502.1205
##  [6025] 505.0749 501.6623 500.1853 507.1580 508.5559 504.7214 501.0222 493.8782
##  [6033] 498.5464 503.9728 501.1163 497.6154 497.9577 500.3839 496.7138 499.5417
##  [6041] 493.2440 494.6140 498.2884 501.1990 501.5666 508.4502 498.9548 494.0003
##  [6049] 498.1010 504.6830 504.5473 503.8276 501.9787 502.8022 495.5946 506.9536
##  [6057] 494.6466 498.6935 498.0294 497.3505 505.0486 495.3246 501.6090 502.5690
##  [6065] 501.3427 488.6827 498.3811 497.9709 495.3654 498.2876 500.9734 492.7561
##  [6073] 502.5956 499.8589 500.9144 498.9572 489.0523 500.6840 500.3479 485.8550
##  [6081] 501.1601 507.1073 492.5042 498.9403 498.7724 493.5214 501.3830 498.2337
##  [6089] 493.2579 503.1078 497.4284 500.7994 502.6071 492.6104 497.3304 494.9269
##  [6097] 495.6852 504.5234 500.9953 493.7009 505.9267 499.1628 491.3375 492.2288
##  [6105] 499.9048 494.2005 499.2272 489.9516 489.0948 494.3081 490.5361 502.0238
##  [6113] 497.8093 496.7053 498.4178 494.1168 497.8179 498.6748 502.8085 499.9321
##  [6121] 501.0552 496.5592 505.5396 502.7288 499.8242 497.8843 503.0230 505.8904
##  [6129] 500.9742 499.6980 505.8314 504.1701 498.9732 503.5884 501.6541 499.2154
##  [6137] 500.6022 495.9914 499.5224 498.1782 499.9073 501.9607 497.8556 508.1919
##  [6145] 489.3737 500.9723 501.5315 503.8683 494.8761 503.3954 492.0323 499.4959
##  [6153] 501.6777 496.5199 500.0382 490.6307 506.4790 493.5589 506.5968 497.6996
##  [6161] 496.3949 497.2943 500.8090 503.6938 502.9164 500.0758 500.4452 500.5499
##  [6169] 508.0065 502.5211 498.3479 492.0760 501.9519 500.2275 500.0474 494.4853
##  [6177] 501.5626 503.7898 499.9772 504.7724 499.0118 498.1510 504.3703 497.3847
##  [6185] 495.6335 498.4819 504.5811 498.7446 501.6810 489.0071 491.4544 494.5546
##  [6193] 494.3090 505.6274 503.5857 505.0535 501.3820 503.5814 502.2274 499.7893
##  [6201] 506.3290 490.9268 507.1162 500.2295 501.6515 497.5796 499.8947 496.0089
##  [6209] 498.7671 501.4293 498.6601 495.8298 499.0319 498.6469 500.1785 500.0568
##  [6217] 504.4195 501.8743 505.8713 504.1312 504.8040 499.8419 495.8223 494.2384
##  [6225] 504.0325 500.9159 507.5696 492.8974 500.9934 498.0179 503.0071 504.5081
##  [6233] 500.8576 504.7770 503.6937 502.5209 500.3372 499.7778 504.6671 497.2855
##  [6241] 500.0955 503.5010 501.7991 501.3753 507.1967 504.0498 501.2648 500.2631
##  [6249] 492.8949 501.4136 495.3182 502.8033 504.8179 500.0571 496.6268 508.7989
##  [6257] 494.4447 501.8984 498.8937 491.3930 505.5770 500.5765 490.6654 492.3899
##  [6265] 498.6284 499.8703 503.0424 502.6446 487.7874 494.7998 500.7134 497.1152
##  [6273] 493.1858 502.4952 499.9486 496.5361 491.4155 498.2573 509.4853 500.2701
##  [6281] 491.8866 500.5241 503.0304 504.9614 498.9975 499.3563 500.4356 488.4850
##  [6289] 494.4431 504.5876 507.3165 503.1896 503.0994 497.2329 499.9230 505.1530
##  [6297] 503.7768 500.7659 501.0737 495.0581 510.7357 498.0376 506.4874 492.8231
##  [6305] 503.3286 494.9404 494.9453 496.3861 497.6726 498.9019 493.7996 491.6832
##  [6313] 493.7275 499.0957 497.8346 506.5308 503.4344 499.9551 502.4716 496.3255
##  [6321] 500.8271 501.4335 490.1961 493.5423 503.0025 494.9882 498.2527 500.0999
##  [6329] 499.8589 490.0498 500.1976 496.8587 496.5908 493.9822 499.7040 504.3184
##  [6337] 506.1691 501.5331 498.2505 498.3320 499.2326 499.6591 501.1437 487.9710
##  [6345] 500.2218 498.4494 500.9734 493.5064 485.1784 490.9145 506.3911 497.6621
##  [6353] 491.2055 494.7624 502.5501 501.8420 501.6919 506.4056 504.6143 502.5292
##  [6361] 493.8934 499.1308 503.2429 496.5195 502.5084 496.9833 501.2626 501.8650
##  [6369] 496.3575 494.3995 502.1796 503.1434 501.1586 500.4917 506.1907 503.7041
##  [6377] 497.2135 499.5185 507.2160 502.4037 494.6527 501.9599 500.1396 501.6575
##  [6385] 491.3022 494.8149 497.0055 496.9580 497.5460 501.9666 492.5689 505.2868
##  [6393] 498.9938 503.6323 493.1389 500.0982 496.4132 496.1709 503.6537 496.9958
##  [6401] 498.7037 505.4573 501.4173 503.6590 497.9073 502.0384 505.8474 486.0115
##  [6409] 501.5611 507.1094 498.4838 496.0506 492.7340 495.3401 505.6134 490.6084
##  [6417] 496.5677 498.0257 499.0245 497.8318 503.0708 492.8200 499.5200 506.0643
##  [6425] 504.7546 487.2819 498.4816 502.7399 501.6242 497.7399 502.1664 497.9754
##  [6433] 503.1856 498.3600 500.8517 499.0701 494.6347 502.1474 491.0329 494.3065
##  [6441] 499.5196 495.4800 502.9076 490.4041 503.9611 504.8271 494.8410 492.7703
##  [6449] 501.6996 502.4705 484.9699 499.2311 503.8612 492.2637 502.2254 497.5455
##  [6457] 498.3514 495.8116 496.0779 493.6173 491.6252 494.7425 503.3170 493.9730
##  [6465] 494.7623 494.2689 495.0499 492.5973 507.0925 500.0520 505.2562 497.1452
##  [6473] 500.4469 500.2905 496.6012 502.6702 497.3180 502.2735 493.7628 492.1883
##  [6481] 498.0497 499.1530 508.6825 507.2649 497.2491 491.8322 500.7028 505.4126
##  [6489] 500.7989 506.0055 501.0815 499.8516 506.1487 500.8900 500.8023 499.5139
##  [6497] 498.3271 498.5885 491.0143 487.9783 497.4171 498.2314 503.8892 494.0430
##  [6505] 504.3608 502.9507 498.9890 493.7513 500.4052 500.5502 490.3082 483.8674
##  [6513] 499.4746 502.7023 501.8927 493.6072 501.4307 497.2121 507.2005 505.5962
##  [6521] 496.7597 499.7121 492.2092 498.5162 499.5635 497.7965 503.1246 501.3703
##  [6529] 495.5267 494.9492 502.6495 501.4490 503.5456 500.8464 506.7040 497.2738
##  [6537] 505.2417 500.6068 503.4974 496.0864 505.0564 507.6716 500.1032 506.1442
##  [6545] 511.4028 486.5754 506.7053 500.7802 499.7640 495.5619 494.2456 501.4353
##  [6553] 499.3753 494.9380 493.7438 500.0670 504.1963 503.1053 500.6008 495.8157
##  [6561] 496.8197 481.0764 492.5735 498.6254 504.9168 497.5888 497.3517 499.1355
##  [6569] 498.7838 497.1759 499.4870 502.9991 496.5518 492.5963 491.9728 499.8664
##  [6577] 499.7958 502.2448 493.2551 500.6633 503.3758 486.0144 509.6568 494.4119
##  [6585] 496.5333 499.0573 503.1167 497.5945 496.2556 500.1743 499.5908 496.3806
##  [6593] 506.8935 498.8500 494.7605 497.0421 507.3686 493.8558 497.6015 497.9205
##  [6601] 498.9874 501.1940 495.2646 504.8187 500.3147 507.8046 498.0882 503.2331
##  [6609] 492.5744 491.1662 497.2054 494.4035 499.8323 495.9452 500.7191 501.3253
##  [6617] 500.0486 502.4388 502.7700 500.7083 498.7911 502.8098 502.3664 502.6552
##  [6625] 502.6606 505.3228 507.1018 500.3760 494.9610 503.7524 494.5138 496.3552
##  [6633] 501.2916 490.9722 498.6101 493.9834 488.2556 501.2562 508.2073 501.0556
##  [6641] 503.7324 481.1160 492.9246 499.2458 503.6983 503.0104 500.6864 497.4577
##  [6649] 497.4292 505.1426 499.1417 501.7354 490.9544 501.8517 498.2596 492.9765
##  [6657] 492.6865 495.7375 498.0410 504.4850 504.4388 500.3387 504.7693 499.4462
##  [6665] 497.3826 499.6289 501.4566 502.5645 494.4545 500.9057 504.2840 499.7520
##  [6673] 498.7482 508.2072 494.0461 499.6912 498.8144 496.3996 494.9277 498.3859
##  [6681] 507.3349 501.2502 494.5797 486.2446 501.1950 495.5895 500.8203 503.9786
##  [6689] 494.1567 505.2076 500.5662 499.2319 494.5941 495.6143 491.1465 492.9440
##  [6697] 498.5753 494.5569 499.4490 495.9308 495.3531 496.1138 503.2814 502.6458
##  [6705] 498.7043 500.3106 499.0089 497.9008 498.7267 497.2682 505.2328 503.6121
##  [6713] 500.4971 494.9302 498.8380 501.2145 497.0924 493.8109 506.2772 500.6286
##  [6721] 501.9036 502.1828 504.6207 508.0097 495.8335 489.6181 506.6139 505.2311
##  [6729] 496.5697 502.4580 500.2222 497.3937 501.0377 501.9076 505.5261 502.8861
##  [6737] 502.3378 499.0860 483.8789 497.9933 498.1470 499.0860 494.9391 499.8993
##  [6745] 501.8122 497.9225 501.7111 498.0092 508.4008 490.7789 498.0908 497.5752
##  [6753] 505.6805 495.9906 502.6801 500.0623 490.4391 496.7040 505.5488 493.8794
##  [6761] 495.1583 503.0357 494.1242 498.7824 507.4474 502.1337 499.3943 496.2822
##  [6769] 497.5213 506.7381 505.1814 504.8056 503.1721 502.9107 505.2667 504.7069
##  [6777] 504.7574 502.0398 492.5417 498.0891 502.8852 499.7722 495.1828 496.9789
##  [6785] 504.8064 490.9866 499.2606 502.7612 504.2095 498.0248 495.7950 488.3725
##  [6793] 498.8620 495.1000 497.4933 504.5174 502.9493 488.9808 490.7447 494.3984
##  [6801] 492.7759 499.2885 502.5168 502.8045 499.6495 495.8550 499.5125 501.3933
##  [6809] 497.6487 497.2577 499.9796 500.0512 503.6191 501.2835 495.2349 497.9688
##  [6817] 501.9003 502.2481 504.1458 507.4587 496.7927 505.0112 496.5385 504.0841
##  [6825] 503.2609 504.1934 487.6999 500.8764 500.6154 498.4429 487.6318 495.5606
##  [6833] 505.6623 498.7227 497.5115 502.1187 506.7293 502.5758 504.0427 498.0480
##  [6841] 501.6903 488.7077 495.0534 498.5586 505.4003 502.8246 501.0066 498.2886
##  [6849] 495.8339 502.8957 491.2706 507.1064 504.9575 500.3904 504.8755 500.7409
##  [6857] 500.3209 500.4371 496.0396 500.8572 501.6356 502.2280 501.7897 503.0745
##  [6865] 507.9943 499.3725 499.7201 502.3701 498.5209 502.3538 496.5534 503.0808
##  [6873] 498.9373 500.7482 497.3277 501.4748 498.0422 498.8368 503.8318 493.3350
##  [6881] 501.5285 500.4041 497.2639 496.8834 500.2418 497.6638 493.0579 495.8861
##  [6889] 498.3135 509.5978 499.2067 505.2448 506.2409 491.5007 499.6774 497.0524
##  [6897] 479.6567 501.0169 500.2325 490.9945 497.8493 496.5240 495.7196 497.8131
##  [6905] 500.5353 503.6410 499.4705 507.0802 498.7690 502.6499 505.4976 505.1473
##  [6913] 503.2458 509.2690 499.4393 496.7207 501.8143 502.4387 501.9815 501.8858
##  [6921] 499.3700 506.3767 491.0836 502.9562 492.3127 493.0698 499.0084 496.0667
##  [6929] 498.7755 499.0320 499.9167 497.5398 500.9175 499.2543 491.3608 497.1116
##  [6937] 501.3465 500.0932 492.6801 493.0373 502.0588 497.9201 491.7572 492.1352
##  [6945] 496.9762 502.3048 499.8743 490.6513 502.0126 500.8719 502.1653 493.9428
##  [6953] 503.3008 498.6810 501.8720 500.6598 503.8024 497.2035 504.3697 494.9367
##  [6961] 496.9521 502.2336 500.2892 494.7386 488.6494 500.2276 494.8458 502.3747
##  [6969] 504.0518 492.8988 493.7576 495.9301 505.8849 496.1861 504.3067 502.5569
##  [6977] 500.5149 499.5447 483.5456 504.4329 492.1322 507.2690 495.3598 494.7945
##  [6985] 503.2932 497.9904 505.7411 499.1406 498.4659 497.8213 510.9825 495.3663
##  [6993] 495.9481 498.9526 502.7172 492.0412 496.2707 499.1116 494.9637 483.7356
##  [7001] 492.9031 494.5920 500.8200 491.8884 493.0430 495.2035 504.4918 505.0785
##  [7009] 498.2691 500.0449 505.7469 498.4417 506.1114 494.5009 485.4096 493.2215
##  [7017] 490.8690 504.0128 506.0347 503.0448 502.6491 500.5465 491.3759 498.3536
##  [7025] 502.3410 496.1837 494.2940 499.8434 502.8372 504.0600 499.3522 502.0335
##  [7033] 501.5147 507.1059 504.3919 497.8755 501.8545 495.3388 502.3254 498.1278
##  [7041] 496.3410 502.1217 500.1989 499.4027 502.1377 493.5469 500.8493 498.9532
##  [7049] 504.6015 489.5577 490.1869 501.1678 499.8202 499.8849 504.7777 493.2038
##  [7057] 492.1400 502.3716 501.9581 503.5892 488.7373 500.9415 506.1619 487.7929
##  [7065] 499.1548 498.5004 499.8820 502.3917 498.3740 499.6172 500.4301 502.0491
##  [7073] 503.2496 499.7439 498.4934 492.0902 495.8223 499.7981 507.0245 504.4517
##  [7081] 507.1684 503.1656 500.2519 502.2790 501.7978 497.3587 505.1024 493.1211
##  [7089] 504.2552 493.8657 497.9439 495.2238 503.3786 493.6564 489.8513 495.4126
##  [7097] 494.9310 484.3893 505.7493 501.7917 509.9233 495.7235 492.1239 501.8811
##  [7105] 501.2696 502.5545 510.9916 504.2365 501.6363 502.5632 498.0300 497.8089
##  [7113] 500.2914 498.5953 496.4073 503.9878 492.9049 495.1400 501.8302 505.8809
##  [7121] 497.2260 503.8860 503.2630 498.4350 500.1159 500.4536 495.3616 503.8863
##  [7129] 498.9156 504.9129 491.0777 498.5496 506.2433 501.3032 499.8346 499.8807
##  [7137] 503.5518 504.7693 492.0762 494.5135 504.0214 507.0982 485.8957 505.6073
##  [7145] 503.2796 505.4427 494.1117 500.7323 501.3909 494.4395 500.6431 500.5570
##  [7153] 495.6659 503.0258 493.9740 501.1961 492.2826 499.1305 506.3395 503.5974
##  [7161] 499.9637 498.4369 494.6351 497.7229 497.6599 501.0805 502.9222 497.5994
##  [7169] 503.4138 504.6862 501.1706 495.4592 499.4292 492.5247 499.4682 499.2706
##  [7177] 503.5877 495.6926 502.3138 502.5801 497.1737 503.3971 500.9982 493.8615
##  [7185] 498.6025 496.7723 500.8564 501.8055 502.4754 503.8024 490.1350 498.7950
##  [7193] 500.6555 510.5052 499.1487 493.1638 495.2537 501.7275 493.8496 493.3699
##  [7201] 493.6229 496.1005 503.5001 502.9790 491.7629 493.2410 501.6174 497.1283
##  [7209] 503.7649 499.3744 499.4909 496.4167 492.7089 490.9429 502.7279 502.8707
##  [7217] 495.3860 501.9338 488.6223 500.0832 492.7714 502.5136 496.5037 500.2203
##  [7225] 495.4787 500.6711 493.5794 495.8354 491.9648 495.7904 489.5966 496.3919
##  [7233] 500.0944 499.3393 499.7832 497.3980 498.8630 495.7089 497.3480 499.7744
##  [7241] 502.8885 496.2252 502.0620 505.0577 495.1134 507.5549 504.3464 504.8283
##  [7249] 497.0642 496.4368 500.0201 492.9461 497.0920 499.7942 504.4001 500.4153
##  [7257] 492.3962 497.2780 497.4697 505.6064 503.8884 503.9026 501.1492 499.3250
##  [7265] 502.1086 495.7864 506.2657 498.5494 504.3933 497.4389 493.6370 490.1020
##  [7273] 505.7957 503.4141 498.2389 503.2342 497.5332 496.2951 507.2195 500.6661
##  [7281] 503.3138 503.0903 502.7180 499.6111 500.6901 505.3090 505.0731 494.2216
##  [7289] 495.5431 503.2097 499.4138 493.1667 510.5132 506.5974 502.7086 500.3068
##  [7297] 500.0481 497.9911 498.5699 501.9784 499.1946 500.1260 496.2855 497.9858
##  [7305] 503.0976 500.8974 496.2873 501.0132 503.1320 503.6099 490.9355 505.1014
##  [7313] 505.6239 488.4086 502.2683 494.1481 502.3317 503.5971 494.7448 498.8051
##  [7321] 505.1008 498.0065 505.0894 495.5106 505.2391 506.6919 499.5046 502.3754
##  [7329] 499.2415 501.2744 502.9546 498.2353 506.6640 503.2855 498.5264 495.9336
##  [7337] 501.1161 500.8683 503.6747 492.1289 499.3126 496.8530 501.6076 501.5224
##  [7345] 500.0302 508.1426 492.9969 497.6764 495.4654 495.1024 493.3665 491.8303
##  [7353] 496.8021 502.1674 498.8143 499.4667 495.5094 491.6706 491.0414 501.5622
##  [7361] 502.5663 504.6116 507.6571 500.9106 496.6542 503.4218 495.8339 497.4473
##  [7369] 496.1823 491.8135 499.1627 505.6715 497.1943 490.4709 499.6865 505.3467
##  [7377] 499.8123 503.5281 503.1657 503.3794 495.1089 496.0570 503.4765 502.4043
##  [7385] 502.0275 501.7861 496.4294 498.8854 499.0073 497.2881 501.8566 493.5425
##  [7393] 509.2906 496.3132 504.7661 506.8788 502.2537 503.6446 501.0682 497.9152
##  [7401] 494.0873 499.5115 493.4701 496.4153 508.5544 504.8810 507.5447 507.5683
##  [7409] 505.2693 495.2130 493.9971 501.9329 488.8608 498.5643 503.6756 493.1233
##  [7417] 500.6737 506.0880 497.7150 501.4839 494.6039 493.6388 496.5863 500.6481
##  [7425] 498.8741 504.2342 501.7853 500.5581 501.2500 501.0790 494.9013 499.6339
##  [7433] 502.8270 493.8284 504.5824 501.1425 495.9168 508.0704 499.1200 493.5996
##  [7441] 499.6785 498.6693 499.3974 501.6610 502.8221 502.3937 503.2809 501.7953
##  [7449] 494.1807 496.6295 494.8590 504.4032 500.8927 495.8194 501.6263 506.1509
##  [7457] 506.8523 501.6642 503.1341 498.5938 497.9830 501.6462 509.6618 499.7690
##  [7465] 493.4969 507.4869 501.6113 493.6118 505.2139 506.4660 494.3209 503.6353
##  [7473] 505.2234 500.2605 507.6656 508.9023 502.4912 497.8804 488.0033 502.2126
##  [7481] 501.3316 494.8186 500.4630 498.8453 485.4474 500.1673 500.0632 503.1300
##  [7489] 497.6947 502.9353 497.0254 500.8081 493.7190 504.4268 500.0241 504.8228
##  [7497] 505.1691 493.0197 497.2396 490.4115 492.7671 497.1448 499.5167 491.5589
##  [7505] 507.9370 501.4026 488.9034 488.5278 499.2722 497.6154 494.2238 503.7437
##  [7513] 493.4801 497.4996 496.1814 503.1690 492.4582 490.6377 492.5450 507.3032
##  [7521] 500.7763 502.0107 497.1238 496.4069 502.7310 509.8761 487.2704 504.4308
##  [7529] 506.2908 487.1591 497.5846 499.1826 498.3218 502.9170 503.4556 491.4960
##  [7537] 502.5792 502.2591 502.9517 499.0954 498.3704 497.7133 501.3665 504.0930
##  [7545] 506.7848 504.8419 500.3128 499.4034 499.6727 502.0792 502.9363 498.6138
##  [7553] 504.9156 497.1483 500.8626 492.5007 497.6178 502.4239 495.5030 501.2787
##  [7561] 497.4205 497.5460 493.9185 502.8054 501.9458 497.5125 504.0539 508.0958
##  [7569] 501.5891 503.2562 490.9025 504.4028 495.8101 503.8671 502.8255 484.7159
##  [7577] 503.8640 508.0361 500.0999 500.1920 502.0365 503.7792 508.4187 500.4082
##  [7585] 506.4992 500.8849 508.0124 491.8713 500.8705 504.1404 508.6787 500.9470
##  [7593] 497.2585 502.3719 503.1416 498.6976 499.2881 506.3487 505.5934 507.6230
##  [7601] 505.9357 503.9059 501.7304 499.4648 487.7881 495.9566 498.0530 506.2894
##  [7609] 503.0550 496.6939 507.7356 503.0451 500.6496 494.9948 499.7681 487.5469
##  [7617] 498.2165 479.9783 499.2681 496.7099 501.2047 501.4115 502.9527 496.4402
##  [7625] 499.0294 500.5531 490.6081 501.6739 497.9387 507.2391 495.9531 503.7546
##  [7633] 498.6313 488.4151 506.3620 499.8893 505.4064 498.3506 493.6790 495.5906
##  [7641] 489.2978 490.7717 503.5943 496.4286 497.2354 495.9170 501.3714 505.7231
##  [7649] 504.1282 504.6277 497.5903 502.7816 499.4183 500.6730 504.5702 501.3609
##  [7657] 501.3878 498.1033 501.7478 499.3519 494.9955 493.6823 493.9848 493.2659
##  [7665] 500.3690 502.3863 492.4824 499.9083 502.0356 502.3636 503.0423 497.1669
##  [7673] 501.1173 502.6307 498.1771 498.7741 502.6137 496.9114 496.3027 505.4262
##  [7681] 499.8480 499.0203 497.4965 501.1955 489.6437 504.8571 499.3751 499.7730
##  [7689] 495.4560 501.2411 498.7831 500.7992 501.5945 501.2953 513.6413 498.3662
##  [7697] 497.3700 504.8069 497.0300 505.7293 503.2400 500.7737 502.8660 497.1495
##  [7705] 500.1648 495.6000 498.9718 501.6539 506.9539 504.7995 496.1635 501.2439
##  [7713] 505.2189 500.3695 487.4070 502.3679 496.2827 499.0946 495.9229 502.8659
##  [7721] 487.0956 506.0917 494.9097 501.3382 504.1457 503.2376 494.4514 490.1178
##  [7729] 492.1016 506.4828 495.6013 504.1844 493.0939 502.9114 489.7008 500.0288
##  [7737] 498.9908 501.7646 501.6894 498.9386 501.7739 501.8471 500.7421 501.4678
##  [7745] 497.1743 501.5569 495.2251 490.6869 491.5827 502.6506 495.4156 499.4191
##  [7753] 489.4725 506.2729 503.4472 492.8878 503.9075 492.2529 498.2240 495.2336
##  [7761] 495.1847 494.9917 491.2586 489.2933 504.6952 497.0283 507.9388 497.5969
##  [7769] 495.1265 501.0288 491.6867 483.6080 494.6153 496.3781 499.5863 501.2003
##  [7777] 496.7816 499.3107 502.5471 504.9865 503.0233 495.1048 505.4049 490.0032
##  [7785] 504.0457 499.6805 498.4355 499.8510 502.2496 498.8440 490.8641 497.5227
##  [7793] 501.1271 496.6259 499.1536 503.5448 496.2066 512.6211 494.7159 497.0146
##  [7801] 498.2089 498.6312 502.0516 497.1490 504.6329 498.8782 500.7377 498.1638
##  [7809] 504.0381 498.0423 501.2552 496.4520 502.7028 509.9098 499.1369 494.0359
##  [7817] 497.3517 503.7032 497.9918 497.3134 496.8906 495.7520 504.2689 493.1920
##  [7825] 501.7691 494.5310 492.1407 506.7355 488.7786 496.5919 504.5881 493.6934
##  [7833] 504.8941 506.0954 500.4658 507.2499 505.8776 498.0603 493.3713 500.2026
##  [7841] 497.6481 491.9297 496.1698 502.3573 502.3293 501.3338 502.6631 505.4268
##  [7849] 503.0107 490.6498 501.7508 499.2734 496.1162 493.7450 477.5017 503.1896
##  [7857] 501.3906 486.5122 494.0192 505.3661 497.5191 498.3449 496.5872 499.9563
##  [7865] 491.2626 499.7793 493.6739 499.9886 502.6603 485.1259 495.1685 499.8640
##  [7873] 497.4503 501.8019 504.9705 502.7493 499.6960 500.0823 497.1074 500.4869
##  [7881] 503.4138 496.4787 504.5060 501.3497 504.6841 499.9961 490.7875 495.6343
##  [7889] 501.2672 493.5243 493.7519 506.2399 503.1557 503.8167 496.4293 495.3065
##  [7897] 501.9252 501.4743 498.4203 500.7160 497.8571 484.4134 506.0874 495.3012
##  [7905] 498.1788 498.6700 500.5741 491.7257 500.0357 494.4597 492.2378 504.3863
##  [7913] 496.3516 505.4901 504.8569 498.7766 497.1967 509.0684 495.8870 500.0798
##  [7921] 506.8371 492.0445 503.5198 501.3711 496.4409 504.5153 501.3149 487.9031
##  [7929] 503.0296 500.9760 502.2215 495.0355 503.7741 504.3231 492.7375 494.6652
##  [7937] 494.1019 493.1126 492.5751 499.5116 493.8594 506.2210 498.0854 499.0803
##  [7945] 499.9584 502.1657 497.8689 493.9978 503.6589 490.4625 490.8207 500.7273
##  [7953] 498.0842 496.1714 493.3672 501.2833 502.8103 495.8421 500.8384 496.6058
##  [7961] 499.2460 501.5487 494.9012 498.4090 496.8709 498.4465 485.7772 498.2891
##  [7969] 496.7124 491.9854 496.1914 501.2394 501.7665 505.9680 505.7333 503.1026
##  [7977] 506.5202 504.2129 501.4062 495.3584 488.3713 505.4764 503.2122 500.3216
##  [7985] 502.2073 501.2403 501.5913 496.3863 504.5950 499.1272 508.6559 488.9177
##  [7993] 497.1787 493.7244 490.5648 494.6106 502.4713 503.6742 500.6149 498.1236
##  [8001] 500.8525 496.1838 496.3216 494.7199 497.2543 500.3283 503.6284 505.6378
##  [8009] 504.2627 497.7950 506.0710 498.0292 496.1765 497.1049 504.5689 505.6711
##  [8017] 487.6699 498.6465 506.4319 498.6575 499.3187 495.9891 502.3934 490.2372
##  [8025] 497.4271 501.6655 496.1592 500.8885 502.0681 491.2075 495.7217 496.8524
##  [8033] 500.1794 502.4464 500.3355 497.2914 489.7327 500.0680 502.5981 497.2587
##  [8041] 510.0200 491.3681 498.8534 503.6368 502.1568 493.8185 502.1386 500.3648
##  [8049] 502.3427 501.7401 507.0749 492.6675 502.3969 498.3606 492.3962 506.2921
##  [8057] 502.2354 499.6077 505.3986 499.0615 502.0936 502.1332 502.5185 500.5277
##  [8065] 493.0756 501.2422 500.5844 487.9544 498.1199 500.8749 490.2953 498.6148
##  [8073] 496.4472 497.3611 494.1119 494.0376 490.6449 506.1438 499.6724 494.6033
##  [8081] 496.1569 498.6589 494.0584 492.8463 500.1703 493.4902 493.4329 503.5981
##  [8089] 488.7658 505.5827 506.9024 496.4356 502.0879 493.3941 500.2061 502.6517
##  [8097] 484.1589 504.1454 497.4951 502.6306 506.6683 491.0414 501.8364 505.3542
##  [8105] 503.8201 503.0868 503.2066 493.4616 501.8252 492.6588 506.6787 502.8861
##  [8113] 498.0751 493.3693 498.4691 508.7429 495.2774 496.0115 495.5920 505.2425
##  [8121] 507.8569 497.7920 496.4593 495.0787 499.3302 504.6652 503.3000 491.9852
##  [8129] 503.4515 495.6698 501.1689 501.7844 500.3676 493.2968 503.4623 508.3581
##  [8137] 506.9736 501.8382 507.2430 495.8188 502.8471 501.5222 500.5555 492.1283
##  [8145] 501.0968 502.7708 493.0274 505.7736 504.0965 498.9407 494.9276 504.8066
##  [8153] 493.0435 488.7205 500.0697 505.3521 495.3714 499.1347 497.9491 497.4578
##  [8161] 503.0104 500.3611 504.4558 479.3420 493.4304 500.3603 493.4538 499.2062
##  [8169] 495.7628 492.0876 503.6316 491.9908 501.9117 484.4630 499.9455 495.3279
##  [8177] 491.9236 497.8856 503.0232 505.6540 499.5713 501.9879 494.0697 496.0610
##  [8185] 502.8919 504.2679 494.9864 501.6423 501.0640 501.4044 510.1101 500.3474
##  [8193] 502.4853 502.2761 501.2258 497.1908 496.9312 495.8035 501.2288 500.9797
##  [8201] 500.6686 503.7258 502.9391 496.7148 499.4842 495.8788 498.8682 494.6142
##  [8209] 497.6448 499.0779 499.7666 503.4080 502.3946 506.8215 504.0706 508.3586
##  [8217] 493.7652 509.3449 504.4200 500.1785 490.8612 502.6166 505.0869 496.3200
##  [8225] 500.8349 500.6556 497.7645 510.4441 501.5927 500.7694 495.0686 509.6257
##  [8233] 504.8969 505.5902 497.3309 499.9692 507.2263 485.6460 496.5527 500.7806
##  [8241] 504.0006 498.1624 504.4213 499.5828 503.9696 491.1142 493.2788 496.5059
##  [8249] 501.5112 503.9980 506.7219 494.6818 498.8591 499.3300 496.7128 503.8178
##  [8257] 502.2078 503.7700 495.2881 500.4445 502.3455 497.7046 493.9051 498.8993
##  [8265] 506.6691 504.1509 506.3938 492.6648 500.4155 492.6846 489.9624 497.2799
##  [8273] 502.1114 501.8880 497.0774 502.4993 503.2876 502.9202 499.5365 501.9051
##  [8281] 501.6671 490.3251 504.0959 503.2238 495.9010 503.1232 500.7053 492.1042
##  [8289] 481.2410 496.4525 504.2282 496.1698 493.0513 498.4771 501.1705 500.6296
##  [8297] 504.3848 490.4690 507.4386 502.1603 499.5219 505.6993 502.0006 503.3427
##  [8305] 497.4790 498.7170 499.3891 506.6324 497.2423 498.3672 497.0142 490.0608
##  [8313] 502.8002 494.9392 495.3009 501.4395 498.9536 501.6518 502.3439 495.9936
##  [8321] 495.0079 493.5536 497.0077 499.8435 492.5425 503.5712 495.1380 498.5464
##  [8329] 506.1250 498.6050 497.1611 499.1908 495.1549 505.2528 494.5873 498.8687
##  [8337] 487.0679 507.0576 503.6071 503.5086 486.9290 492.0577 495.8840 502.3244
##  [8345] 503.8996 488.2003 497.4214 501.1223 502.6486 488.4150 504.4693 500.1329
##  [8353] 497.2300 497.1162 501.5227 497.3566 496.6895 485.3874 500.0913 500.7506
##  [8361] 499.2308 502.2317 493.7642 493.8455 502.9628 497.2603 501.7975 507.9002
##  [8369] 509.6000 501.5869 500.2452 497.5112 501.9204 508.2139 500.8103 498.0980
##  [8377] 506.6648 503.5446 507.7168 499.5249 504.3043 491.7091 488.9440 498.6444
##  [8385] 496.1514 501.0996 497.2686 502.0004 493.0141 497.1203 503.9846 501.4084
##  [8393] 497.5934 496.6900 500.8737 493.6611 498.4559 504.2237 504.7112 503.0431
##  [8401] 492.3245 502.8495 494.6128 494.0117 487.2492 500.3410 501.3228 501.7134
##  [8409] 504.6336 502.1483 512.0535 503.5653 495.6660 503.8876 494.8101 504.5943
##  [8417] 499.6516 498.1617 498.8437 499.4261 505.4278 498.5191 502.5843 501.1376
##  [8425] 499.3635 496.6738 501.3476 496.2915 499.5867 499.4186 496.1871 497.3443
##  [8433] 492.7129 498.0889 503.5734 496.5353 496.8067 502.3775 495.6991 502.2916
##  [8441] 505.8947 502.7657 495.6009 501.1529 507.6010 508.6836 496.7691 504.2591
##  [8449] 505.0468 491.7709 498.1832 500.2017 500.8391 498.9296 497.2171 499.9132
##  [8457] 497.1058 502.2955 502.2528 496.1366 500.5288 504.8448 495.1550 492.7805
##  [8465] 497.4057 498.9694 497.2667 501.6028 504.5505 493.4847 507.2445 502.0499
##  [8473] 503.3603 505.1036 501.7115 499.5975 498.5922 499.8341 495.2414 493.2769
##  [8481] 496.6092 498.9621 499.5531 505.0074 504.2787 504.8610 492.7623 495.1011
##  [8489] 501.4009 494.0678 498.6501 504.7325 495.5575 493.0866 492.4896 488.0886
##  [8497] 494.2482 493.4207 497.6869 499.9739 503.5915 502.7178 498.3991 500.9420
##  [8505] 489.6075 494.2494 500.2726 505.1092 494.5387 493.2745 500.0076 495.7579
##  [8513] 502.2227 503.6543 500.6947 498.6920 504.9782 491.9309 498.4488 505.2817
##  [8521] 503.5732 503.9408 498.3568 504.9112 508.3082 497.8531 502.9853 504.8121
##  [8529] 502.1249 501.2399 502.6491 504.6737 501.1830 493.8340 499.6554 504.1504
##  [8537] 505.3685 495.0916 491.2614 494.7351 495.8815 501.0584 494.0661 500.4972
##  [8545] 489.8697 495.7559 498.9037 496.7649 504.0744 507.7682 500.1483 503.9241
##  [8553] 495.9004 491.8115 499.6925 496.8750 497.0391 498.7504 497.1587 501.6841
##  [8561] 503.3734 501.0204 486.4844 495.4932 507.7523 497.3255 497.6368 495.2353
##  [8569] 502.1240 501.6626 496.4793 503.0819 498.5686 502.1244 495.2137 504.8309
##  [8577] 505.5929 504.8966 508.0427 499.3184 503.1057 502.0443 501.3139 494.1541
##  [8585] 493.0225 503.0500 505.8061 503.7503 500.6341 498.7374 499.4245 504.1445
##  [8593] 504.0171 486.7540 502.4102 504.9491 497.9767 497.4620 501.2720 501.7886
##  [8601] 496.1121 507.2135 492.6190 503.4284 503.3704 497.6179 489.6195 497.1947
##  [8609] 501.8524 497.8265 501.5200 496.5810 495.3951 500.4150 498.3412 490.4282
##  [8617] 492.0902 506.5726 492.2418 501.9870 501.8104 495.2781 498.1703 499.7037
##  [8625] 498.6953 496.8854 505.0673 503.3181 501.9580 507.2787 498.2019 502.0108
##  [8633] 506.0489 495.8545 498.8495 500.3630 497.9769 493.1028 503.1519 496.9638
##  [8641] 501.6429 499.6886 500.0424 497.1062 502.5181 500.9457 500.4753 503.0713
##  [8649] 508.0376 499.1394 504.9120 503.8287 499.0724 486.0177 500.6987 501.2166
##  [8657] 494.4471 509.7125 494.0575 497.9502 506.2134 503.3544 504.2123 489.2733
##  [8665] 488.8611 482.0601 505.4208 498.8890 496.0577 502.4699 492.9155 499.8157
##  [8673] 501.1011 499.0664 499.6612 498.9090 496.8254 504.7663 491.6507 490.5431
##  [8681] 499.1252 497.8937 499.7099 502.6301 488.6386 505.8514 493.9310 497.8323
##  [8689] 500.0276 503.1593 499.1504 502.7429 500.8883 498.7394 499.6110 494.1242
##  [8697] 494.8893 493.6136 491.3050 502.8052 497.4789 498.9393 498.5037 497.9029
##  [8705] 497.2076 504.6999 503.5719 495.3230 502.4700 504.4477 492.3039 503.9697
##  [8713] 493.5375 500.2345 499.1762 496.8388 502.3734 502.6640 505.4853 501.5562
##  [8721] 497.7486 496.4175 494.4735 484.3317 505.3967 504.3884 497.2497 500.8482
##  [8729] 494.2957 496.3175 505.0316 503.0166 502.4375 504.1086 501.4267 502.3388
##  [8737] 495.3166 497.2878 496.1972 498.4643 509.7354 503.4773 507.8906 503.0582
##  [8745] 501.0974 497.5099 501.4739 501.6440 496.6972 500.4309 502.0939 497.5444
##  [8753] 496.9133 494.2772 497.7428 502.4581 504.1514 507.7700 502.4345 493.5860
##  [8761] 493.6193 493.1219 497.3029 509.8901 497.1009 501.7013 502.7103 494.5316
##  [8769] 501.7835 505.3822 498.4314 494.0320 500.8332 499.4046 496.7086 496.1550
##  [8777] 498.2261 502.6437 505.4306 497.4253 501.7337 497.2699 509.0230 498.6112
##  [8785] 500.2434 496.0031 505.2123 499.3168 496.3192 492.9460 492.4684 503.8294
##  [8793] 501.9412 499.3811 499.8579 497.4960 505.5585 499.7460 494.0990 496.9973
##  [8801] 496.1519 496.3643 497.7782 496.0298 493.8077 500.9886 499.9572 502.5764
##  [8809] 498.0148 507.6553 500.9317 502.2333 502.4843 505.5297 505.8836 502.8486
##  [8817] 498.9848 502.6607 498.0341 505.9901 504.5115 506.0994 501.1895 498.5680
##  [8825] 512.4518 499.5856 502.0630 499.1220 491.7355 499.0612 501.7748 494.0055
##  [8833] 495.0747 498.8559 491.3967 500.0606 504.1337 505.9548 502.1930 503.9117
##  [8841] 491.4398 501.8172 510.0894 501.0532 504.3795 507.4286 495.8932 502.0823
##  [8849] 499.5258 499.2872 492.7861 501.4053 503.9372 510.5180 496.5454 491.0985
##  [8857] 498.5831 498.8127 492.4649 503.9285 506.3424 496.3249 499.8162 500.7072
##  [8865] 490.8818 499.2012 503.0404 501.0867 496.2784 501.6860 499.2428 501.3692
##  [8873] 504.6931 501.6727 512.3174 503.3567 495.1857 504.0753 491.7504 500.0469
##  [8881] 504.0856 498.5698 499.1291 500.8021 496.2037 502.6942 498.5650 500.4332
##  [8889] 497.6846 493.6036 502.3362 495.8749 495.9331 504.5218 500.4425 504.1043
##  [8897] 497.7577 484.7175 501.4142 500.8824 498.4415 490.6459 491.0360 503.1861
##  [8905] 493.7685 504.4534 486.6516 504.1248 494.0554 504.6676 501.5328 497.9046
##  [8913] 504.0229 499.7389 485.7865 504.3579 505.6829 495.6161 495.2161 508.9489
##  [8921] 501.5177 495.5971 503.3470 503.1476 505.2935 498.2118 503.1887 497.8676
##  [8929] 494.5442 501.1046 496.5535 503.3799 494.2835 508.2219 503.1438 495.0751
##  [8937] 499.7068 490.0696 500.3825 502.5316 501.1621 505.9281 508.1374 496.1463
##  [8945] 497.9438 501.2056 496.4885 507.4022 505.3645 502.9361 501.9122 508.0340
##  [8953] 496.2287 497.2306 503.2984 495.7738 501.2101 505.6145 500.6765 496.8277
##  [8961] 497.4325 501.1051 493.0071 501.3405 504.6407 499.5005 502.5658 500.2646
##  [8969] 499.1171 504.5874 505.2890 494.4265 501.3271 503.7282 491.7487 501.8069
##  [8977] 489.6788 500.2494 491.6730 503.0589 500.0070 508.3451 499.3971 502.6699
##  [8985] 493.3479 497.6691 497.2363 496.7049 499.2670 491.5418 491.1493 499.9493
##  [8993] 505.0237 491.7614 505.0922 494.0795 502.5707 495.2866 505.0244 496.6361
##  [9001] 505.2791 499.9408 506.3108 500.6001 497.4752 493.0168 499.7235 495.6773
##  [9009] 488.9170 498.1065 490.4525 498.6487 504.6935 508.8367 507.7616 504.7173
##  [9017] 506.5860 501.5410 491.6186 500.9170 492.8504 495.1506 484.9972 489.7271
##  [9025] 501.5400 498.4746 497.5747 503.1417 492.6207 507.0126 499.6460 504.7611
##  [9033] 508.8068 505.1636 503.8662 509.3046 495.7222 498.0804 501.9531 494.9928
##  [9041] 501.1047 501.7309 503.6587 497.5939 502.7152 507.9447 497.8282 503.8969
##  [9049] 497.1882 496.0170 499.3023 502.9767 493.4924 508.6818 508.0879 502.1226
##  [9057] 505.9830 497.6493 501.7116 485.7730 492.2196 504.7383 499.9006 504.7411
##  [9065] 485.0090 501.5423 501.7831 502.6228 501.4608 500.5962 494.9869 489.5037
##  [9073] 503.1814 502.3872 497.9212 494.2244 497.3706 496.2483 499.0747 487.8300
##  [9081] 502.7627 501.8336 502.6356 501.2215 506.9549 505.1385 498.5095 500.6763
##  [9089] 495.1770 496.1118 484.5987 496.6600 489.5785 507.2827 495.1431 495.7209
##  [9097] 499.9854 498.0759 497.9235 500.7098 498.5274 500.1891 503.3497 495.9365
##  [9105] 498.0005 498.8286 500.8996 501.4621 493.2139 504.6163 503.7516 503.3501
##  [9113] 497.5042 502.0534 499.3711 509.3649 499.6712 500.9119 498.8630 499.8630
##  [9121] 500.7895 511.3262 493.1535 492.6713 506.7069 499.7576 496.0904 485.6371
##  [9129] 494.5630 507.0394 500.7240 505.1969 500.5316 495.4777 503.2840 495.3559
##  [9137] 488.5938 506.0554 492.2052 489.8216 495.5132 495.2198 492.2726 502.6292
##  [9145] 505.3181 501.7666 498.4755 496.3721 505.6674 490.2645 494.0007 504.7286
##  [9153] 501.9823 502.0638 492.2666 492.5908 505.8138 495.9888 502.8584 504.5145
##  [9161] 503.4024 500.6181 495.7655 494.0387 506.7859 483.0999 502.2022 499.6710
##  [9169] 502.1540 505.3090 492.5179 501.2139 502.5972 497.7425 501.2189 505.3165
##  [9177] 504.4926 499.9474 501.5576 502.0961 499.4092 487.6397 497.1771 498.0418
##  [9185] 498.5156 502.1505 501.2529 500.9542 492.3603 495.7014 488.4364 499.2729
##  [9193] 501.0784 495.2658 502.1640 496.4652 498.7625 505.3987 504.6672 500.8964
##  [9201] 495.3544 495.2877 494.3641 501.7310 506.7040 495.0586 499.4307 507.7731
##  [9209] 499.5092 506.3241 502.9775 488.0347 500.5046 501.3075 499.2014 496.6960
##  [9217] 493.0963 494.0456 494.4451 501.2896 494.3944 503.2957 492.3244 504.4859
##  [9225] 495.8277 499.2157 500.6642 491.9710 496.4054 496.3425 504.3686 506.0185
##  [9233] 504.4796 497.9854 504.4319 503.5031 507.9960 496.5884 501.6773 496.6571
##  [9241] 494.6955 494.7858 500.5596 489.9746 490.0267 503.1781 494.3026 498.6260
##  [9249] 504.1533 489.8893 491.1501 498.7178 498.4738 499.5790 504.2774 498.2223
##  [9257] 490.9041 505.9784 503.6496 500.3532 492.6981 492.9125 505.2992 501.2648
##  [9265] 504.8180 495.2765 492.9654 495.0396 499.9317 497.5452 498.9473 496.2165
##  [9273] 502.7539 499.8541 503.8627 499.0712 501.4103 501.4037 508.2182 495.8381
##  [9281] 506.8742 495.8924 502.9286 499.0822 502.3697 497.9399 497.8683 501.1759
##  [9289] 503.3048 502.7543 500.1913 504.8905 500.0589 499.4097 498.1896 503.3200
##  [9297] 496.9023 508.8022 491.9427 493.5275 503.4353 495.4647 499.5502 502.3454
##  [9305] 497.5470 499.6455 502.9915 496.1117 494.1031 502.9414 497.0276 500.4126
##  [9313] 487.2377 509.8396 498.6136 495.0306 498.2619 494.2659 499.6312 497.2077
##  [9321] 509.4227 504.7607 497.7392 489.0477 491.2957 497.6726 500.3532 492.6681
##  [9329] 499.1310 498.6292 492.7314 496.6552 499.6023 499.2819 501.2333 498.7656
##  [9337] 502.7073 494.6591 502.5504 489.5997 497.5025 492.7766 493.9517 490.2933
##  [9345] 497.4465 481.3574 499.7378 503.8133 496.0941 493.9839 495.1880 507.7642
##  [9353] 504.4381 506.7083 491.9578 504.2392 504.7453 502.6573 500.5386 491.9501
##  [9361] 502.7191 498.4491 505.2576 496.7052 494.1516 507.2707 497.1065 497.6349
##  [9369] 501.4461 502.2660 497.3670 504.9898 505.3939 492.4780 496.1475 499.3996
##  [9377] 494.0895 498.0615 498.4663 502.9624 505.1241 502.2746 498.7186 496.4624
##  [9385] 501.2415 498.1036 490.7677 509.4316 507.0239 500.4450 502.2502 501.2450
##  [9393] 496.8381 503.9337 497.9883 501.4060 491.6438 501.3170 508.6832 498.8409
##  [9401] 504.3497 499.7801 501.7166 505.6448 501.9568 494.8434 501.9277 498.6156
##  [9409] 504.8013 499.9479 498.6073 500.0977 498.7698 502.9634 505.2742 493.3828
##  [9417] 501.4618 504.4148 503.6234 500.5172 505.6652 492.3062 498.7280 495.5355
##  [9425] 493.5390 501.6436 502.6153 510.6575 505.4963 495.5041 497.8775 499.0597
##  [9433] 495.5754 497.9051 499.2908 508.0567 501.6569 494.0979 495.9332 499.6558
##  [9441] 501.3088 505.6563 502.5208 499.5118 505.3632 509.1523 507.8592 500.7999
##  [9449] 497.1192 500.2713 496.1416 499.2961 499.9019 496.5028 498.9142 499.5241
##  [9457] 505.0022 499.0717 506.8330 501.3023 503.8394 502.0905 497.2775 497.7064
##  [9465] 504.8504 498.9844 501.2914 510.8499 495.3771 494.6422 500.4306 498.7850
##  [9473] 501.5275 508.8570 484.9900 501.8256 503.4879 508.1414 502.0326 507.2594
##  [9481] 498.4928 500.0264 501.1761 490.9129 492.2086 507.0591 495.3550 503.2892
##  [9489] 501.8828 496.1929 493.0653 506.7327 507.0476 499.2956 490.8907 484.8811
##  [9497] 491.8000 502.9890 502.3523 489.6537 506.5124 503.6820 502.4344 502.6675
##  [9505] 500.8576 502.6175 504.7413 505.4648 496.4229 499.9171 509.3753 503.7226
##  [9513] 495.8527 501.6915 495.0070 500.4791 500.0245 509.9580 505.6079 494.1532
##  [9521] 501.1746 496.8707 506.7061 496.4062 492.6338 500.9978 497.4908 498.0053
##  [9529] 505.9693 497.3805 495.3475 500.4042 491.4593 501.2027 495.9579 501.3171
##  [9537] 501.5346 499.0993 497.3999 496.2536 490.8677 496.6397 492.5014 505.2608
##  [9545] 501.7350 506.0482 498.6027 487.3695 507.8361 501.0034 500.6135 506.3597
##  [9553] 501.4246 506.1455 492.4160 503.6127 508.5549 500.5439 503.9363 502.8914
##  [9561] 499.9390 500.7862 499.0646 508.5328 506.0917 500.8016 497.7044 505.8324
##  [9569] 500.6971 493.6095 500.2002 496.9998 494.6005 490.4012 505.7972 494.5855
##  [9577] 496.7447 500.1044 502.1805 502.5189 498.0395 498.7220 501.0894 501.3197
##  [9585] 499.9240 499.2412 506.3028 507.0337 488.2942 502.4425 504.9432 504.5441
##  [9593] 499.5668 498.6031 504.0474 498.6246 500.8515 498.8795 504.6651 500.6026
##  [9601] 488.8055 498.7517 505.9705 504.4667 487.4269 489.8110 496.1123 501.0219
##  [9609] 505.2194 501.3351 504.9514 493.4284 502.1201 500.3442 496.2683 495.7441
##  [9617] 499.7627 501.4909 507.9406 500.3853 492.3730 507.4200 495.7637 500.5937
##  [9625] 504.7815 495.2895 503.4577 504.9252 497.5326 502.4015 500.2649 496.1332
##  [9633] 497.3098 498.3860 504.6233 500.5197 498.3922 506.6284 497.1378 502.1100
##  [9641] 495.5978 501.2781 501.5652 505.5281 502.5413 492.3947 503.5975 496.2403
##  [9649] 496.5126 496.3435 503.2902 503.2020 496.6250 497.3042 485.6169 488.1843
##  [9657] 495.9759 506.3111 497.7498 497.5926 504.2903 496.9980 500.1119 505.9453
##  [9665] 503.2856 498.1793 500.8218 506.5076 504.1012 497.9119 493.0146 500.4386
##  [9673] 494.6544 500.6006 499.1397 501.8666 502.0311 498.4274 507.5021 498.7886
##  [9681] 503.4889 503.0600 494.4266 505.4299 497.2083 497.1702 500.5819 496.7170
##  [9689] 503.4766 501.4996 503.4793 502.2250 508.1965 501.4424 491.8013 501.0730
##  [9697] 504.7297 506.7500 502.4795 501.4488 498.6867 508.6175 509.0324 498.3082
##  [9705] 499.9010 497.5370 495.4203 507.4014 498.0486 496.7466 495.7577 501.0041
##  [9713] 493.8526 500.5151 500.2703 507.4626 505.0373 503.9766 501.5913 498.8675
##  [9721] 501.2202 497.6394 501.0607 504.1816 499.5822 501.4663 492.6933 502.0511
##  [9729] 493.4581 500.9418 496.6862 498.5066 501.4515 501.7595 511.0581 495.1021
##  [9737] 499.8489 507.0956 508.7201 501.8146 502.6979 496.6273 489.3393 503.6401
##  [9745] 499.7237 498.1213 486.4743 493.5245 496.5441 501.4490 492.1819 510.1302
##  [9753] 500.4603 504.7627 498.7879 498.0326 498.9702 500.9439 483.0569 502.2084
##  [9761] 491.2749 488.5350 504.4450 506.5601 497.4289 508.3052 498.7951 506.4876
##  [9769] 506.0195 502.0140 495.7322 502.1033 502.0483 493.1206 490.5731 503.2220
##  [9777] 495.1970 493.3892 495.4880 501.5322 501.3735 496.4271 496.5126 502.8457
##  [9785] 501.9095 494.5697 492.2460 499.3397 500.8869 487.2875 488.8659 502.9012
##  [9793] 501.4730 504.3105 507.5651 500.4093 498.5764 500.1599 499.9621 501.2927
##  [9801] 499.0204 494.7676 502.1998 488.6596 495.8475 502.7703 496.3264 493.8155
##  [9809] 496.3955 500.0543 501.9289 502.5936 505.8627 501.0760 498.7561 507.9927
##  [9817] 503.5431 496.3930 500.0215 497.7181 496.3636 496.9144 502.0504 495.4252
##  [9825] 499.6205 502.8086 497.8540 505.5562 508.3073 502.8931 493.9529 506.4152
##  [9833] 498.3722 493.8717 505.1648 506.4056 492.3799 498.4057 502.4336 499.9135
##  [9841] 498.8603 503.5980 494.6186 493.4923 501.4008 503.4763 494.8910 503.5781
##  [9849] 504.1770 500.0037 500.0770 490.4247 499.3287 486.5121 501.1515 502.3249
##  [9857] 492.5504 504.3703 501.5714 492.4285 503.2440 501.1122 491.1324 504.5766
##  [9865] 493.7736 503.8812 503.5215 500.4755 498.7332 499.9908 497.6920 498.8145
##  [9873] 498.7633 496.4006 495.6607 497.3854 507.1641 503.2062 503.1086 503.5647
##  [9881] 498.3119 481.9459 503.3961 501.8561 503.8405 499.8002 497.2702 510.5257
##  [9889] 504.0118 486.1654 497.8192 504.3012 502.3387 498.4256 501.2336 492.9178
##  [9897] 506.5070 502.9221 507.9240 495.6045 501.8477 503.8155 499.7445 490.0094
##  [9905] 496.6499 502.5507 505.9142 500.0119 495.9378 492.8598 507.1005 491.8414
##  [9913] 496.4399 498.0476 501.9834 506.1544 497.3419 499.5283 509.1327 495.8664
##  [9921] 500.3570 496.2916 506.4079 501.8028 503.0788 498.3146 495.7623 500.2316
##  [9929] 485.8372 495.4389 502.7191 497.9038 509.4969 494.7460 502.8253 493.4920
##  [9937] 500.5025 498.9056 501.1583 497.8938 491.1660 503.2947 499.9590 503.9078
##  [9945] 501.7218 507.7223 501.6661 504.7038 495.8546 499.7946 503.9626 502.2969
##  [9953] 504.6049 496.0559 497.3302 503.6438 495.4755 503.2291 500.3171 500.2200
##  [9961] 502.3176 496.8541 484.0482 506.4698 489.2531 502.8077 497.4375 499.2936
##  [9969] 502.1699 496.2551 506.7585 499.4681 492.3600 496.8438 501.9054 499.2751
##  [9977] 495.7839 499.6223 499.0563 512.2457 497.0739 495.4718 504.4983 498.7607
##  [9985] 500.0669 497.4349 505.0310 496.5445 488.7305 500.0794 499.3746 496.7222
##  [9993] 502.6430 506.5557 489.9429 500.9887 493.1115 504.8768 498.2333 502.1795
```

![](../index_files/figure-html/run-mc-1.png)<!-- -->

# References


[^1]: EAs version of the analysis follows a similar structure than the cost effectiveness analysis performed by the charity evaluator GiveWell [@givewell].


[^5]:`F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]` Original [here](https://docs.google.com/spreadsheets/d/1jzS693Y-ZAIloQejlzSc3e3t7iPHyor1qt7HBjSVXhQ/edit#gid=509033857), editable version [here](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=509033857)
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter.


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit


[^7]: series avalable in file `~/opa-deworming/docs/materials/original_materials/Baird-etal-QJE-2016_fiscal-impact-calculations.xlsx` worksheet`Assumps&Panel A Calcs!A93`
