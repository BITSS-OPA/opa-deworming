---
title: "<center><div class= 'mytitle'>Open Policy Analysis for Deworming</div></center>"
date: "<center><div class='mysubtitle'>21 December, 2020</div></center>"
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
    lambda1_new_so <- c(79.51)   # avg treatment effect from klps2-4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(76)  # ADD SOURCE
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, row 1 col 4
    lambda2_sd_so <- 7.8                # Table 3, row 2 col 4
    #This is are the parameters labeled eta in the doc
    prevalence_0_so <- 0.92 # 0.92 doi: https://doi.org/10.1111/j.1468-0262.2004.00481.x  location: table 2, row 6, column 1
    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B (Source data took the log, here we recover it)
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



<img src="C:/Users/Aleksandra Ma/OneDrive/Documents/BITSS/opa-deworming/code/main_pe.png" width="100%" style="display: block; margin: auto;" />

<div class = "divider"><span></span><span>
Executive Summary
</span><span></span></div>

This report is part of an Open Policy Analysis (OPA) on deworming interventions. An OPA is a policy analysis that emphasizes high levels of transparency and reproducibility. It contains one [open output](https://fhoces.shinyapps.io/shiny_app_test/) that best represents the facts to inform policy makers, one report (this document) that clearly explains all the analysis, and [one repository](https://github.org/bitss/opa-deworming) that contains all the materials to reproduce the report and final output.

This report describes three approaches to compute the net present value of mass deworming interventions. The first two approaches are exact reproductions from previous research [@baird2016worms; @klps4], and the third approach is a combination of the previous two with some modification suggested by Evidence Action, a key policy partner in this area. This third approach uses the same benefits as the previous approaches and adjusts for different costs, prevalence rates and length of treatment across settings. We suggest that this final approach should be used as the best available policy estimate to compare costs and benefits of deworming in different settings.

The  main policy estimate predicts that a mass deworming intervention will have a net present value (comparison of stream of benefits and costs from today's perspective) of 289.8 for a setting with average prevalence and average unit costs (among the countries where Evidence Action has data for). Readers interested in learning about the predicted value for a specific setting are encourage to use the [interactive app](https://fhoces.shinyapps.io/shiny_app_test/) components of this OPA.

<div class = "divider"><span></span><span>
*
</span><span></span></div>


# Open Policy Analysis {-}

This report is part of an Open Policy Analysis (OPA) project on deworming interventions. A framework for making policy analyses transparent and reproducible [@hoces2020framework], OPA’s goal is to clearly show how an analysis was conducted and how to best represent key figures or results for policy makers to use as a factual basis for deliberation. In addition, OPA facilitates the re-use of analyses across similar settings, and sheds light on how evidence generated by research is used in specific policy analyses.

 This OPA project contains three components, following the OPA principles laid out in the aforementioned paper:

  1. One single output that best represents the factual information required by policy makers to inform their position regarding a policy of mass deworming. This output is presented in Figure  \@ref(fig:main-pe-print), and described in the [results section](#policy-estimate) of this report. The connection between each component of the analysis and the final output can be explored interactively in [this web app](https://fhoces.shinyapps.io/shiny_app_test/).

  2. This detailed report that describes how to obtain the policy estimate and describes each component of the analysis.

  3. [A repository](https://github.org/bitss/opa-deworming) that contains all the materials needed to reproduce the analysis with minimal effort (report and interactive app).  

This report provides a complete description of the analysis behind the results presented to inform a policy discussion on deworming interventions. It describes how to reproduce the analysis in its entirety, and includes all the methodological choices involved. In order to document all the steps without overwhelming the reader, the report is displayed in a layered fashion. The first layer consists of a narrative description of the analysis. The second layer, that appear after clicking in the ![screenshot](show_details.png?display = inline-block) contains equations that show how each piece of the analysis was carried out. And the third and final layer displays the code used to operationalize each equation. All this information is contained in this document using dynamic documentation [@xie2015dynamic], so interested readers can access the source file of the report and reproduce the entire document in their own computing environments.


# Introduction  

Parasitic worm infections, also known as soil-transmitted helminths (STH) and schistosomiasis, are endemic in many countries across the globe, disproportionately affecting the poor. These parasitic worms interfere with regular bodily processes by decreasing nutrient uptake and can thus lead to serious consequences on human health, education outcomes, and long-term economic well being. In particular, evidence indicates that these worms contribute to malnourishment, impairment of mental and physical development, lower school attendance, and decreased wages [@croke2014long; @miguel2004worms; @baird2016worms].

Evidence from previous mass deworming interventions has demonstrated to be a highly effective public health policy. Here we provide a policy analysis that compares benefits and costs of deworming across different settings, allowing for the translation of research findings into different policy-relevant scenarios.

The goals of this OPA are three. First, to increase the transparency and reproducibility behind existing policy analyses on the costs and benefits of mass deworming programs. Second, to update this policy analyses with input from stakeholders closely involved in policy making around deworming. And third, to illustrate how an open policy analysis framework can be implemented in practice.

The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:     

  1. Reproducing the original CBA produced by @baird2016worms, which estimates the net present value of a Kenya school-based deworming program after a 10 year follow-up for four different policy estimates.     
  2. Reproducing an updated version of such analysis on the same intervention but with additional follow-up data [@klps4].
  3. Producing a new analysis that, building from the previous two approaches, focuses on one specific policy estimate, and allows for results to vary depending on key characteristics of current settings where deworming policies are being implemented. This new approach was developed in consultation with a key stakeholder in this area, the non-governmental organization (NGO) Evidence Action (EA)[^1].




# Methodology  

We first describe the common elements across all three approaches, and then describe each approach in detail.

## Common structure {-}

The starting point is a comparison of a stream of benefits and costs over the lifetime of the recipients of deworming. The final policy estimate is the discounted sum of all costs and benefits, known as the Net Present Value (NPV)[^12].

[^12]: Approaches 1 and 2 also present results in the format of internal rates of return (IRR). Following the principle of open output, we restrict the presentation of results to just one format. NPV was chosen over IRR in consultation with Evidence Action to clearly communicate the scale of the welfare effects. [CONFIRM WITH GRACE]  


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

The estimated externality effect ($\lambda_{2}$) reflects the additional hours worked due to individuals who did not receive the treatment but still saw reductions in the likelihood of infection due to lower worm prevalence in their community.  Note that this parameter is not estimated by gender, so we repeat its value two times. All the components to the equation \\ref{eq:7} come from @baird2016worms. The externalities effects are adjusted by the coverage and saturation of the original study.

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

As a result of deworming treatment, there is an estimated increase in school attendance, which is multiplied by the cost of education per student to calculate the additional indirect cost on the education system imposed by a treated individual. The additional costs on education are computed as follows: first compute a cost per student ($K$). This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher. Second, the cost per student is multiplied by the estimated increase in school attendance ($\Delta \overline{E}_{t}(S1,S2)$). For this we use a series of estimated effects, including the additional direct increase in secondary schooling from 1999 to 2007 obtained from an additional analysis related to @baird2016worms. This series does not take into account the externality effects. To incorporate externality effects, we would need another series (from the same source) that estimates the additional secondary schooling increase due to the externality in order to add it to the original series.

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

Gains in yearly earnings represent the treatment effect on welfare ($\alpha^{pooled}$), which implicitly takes into consideration the life cycle profile of wages, economywide growth, etc.

[^8]: In another specification the authors assume that effects persist through the rest of an individual's working life. Here we select the specification that is most highlighted in the paper (most conservative specification). The authors also analyse the welfare effects over consumption, but given that they do not aggregate both outcomes in the welfare effect we only choose one and focus on earning for comparability with the approach 1).


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


Since the analysis is discrete, and we cannot sum over a non-integer, we find

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

With complete subsidy, the costs of the intervention become the total direct costs of deworming each child (in USD). Most recent (2018) data from Evidence Action reveals this cost to be \$0.42 per year. Adjusting for purchasing power and inflation, we get a per capita cost of \$0.83. Adding all indirect cost over an average 2.4 years of treatment, the average cost of deworming each child over the entire treatment period is $1.44.


#### Indirect costs: additional years of education and its costs for government  

The indirect cost on the education system is calculated similarly to approach 1: the cost per student is multiplied by the increase in school attendance due to deworming. The cost of additional schooling is given by the product of the annual cost of schooling each child and the number of additional years children attend school as a result of deworming. This analysis assumes that pressure is added to educational institutions for a maximum of nine years, starting at year zero. The cost per student ($K$) is updated with new information on annual teacher salary (including benefits)[^9], $12,055 (also adjusted for PPP), and the same average number of students per teacher (45).

Hence, the cost of schooling each child for an additional year is now $267.9 (USD).

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: @nyanchama2018 and @oduor2017. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45.


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

Over this nine year period, treated students attended school for an additional 0.15 years on average. Then we get an average cost of additional schooling per child over the nine-year period, $32.40.


### Assessing computational reproducibility of original results  

The second approach does not report benefits and costs separatedly. With all these elements the main result from the original analysis that is comparable with the results discussed here is a NPV of 499.72 (table A12, column 3, and row 6) This result corresponds to a social internal rate of return of 40.7% located as an inline result in the paper - also in Figure 1 - and in the appendix at table A12, column 3, and row 9). Following the steps described in this section, this analysis obtains the same result (499.689353 and 40.7483155643791% respectively without rounding).




## Approach 3: Combination of Previous Approaches and Input From Key Policy Partners

In this third and final approach, we borrowed some methodological elements from @baird2016worms and @klps4 and sought feedback from a key policy partner to best identify one clear output to inform policy makers. We worked in collaboration with the NGO Evidence Action, a key technical assistance partner in this area. Evidence Action provided insights on what are the most relevant costs and benefits from the perspectives of policy makers, and on certain aspects of the analysis that could be updated with present-day data.

Under this approach, the benefits from deworming described in Approaches 1 and 2 are scaled to reflect differences in prevalence rates, and length of treatment. Additionally, the relevant costs are constrained to direct costs alone (excluding additional costs on education). Finally this approach uses inputs costs and prevalence that reflect the current settings where Evidence Action is currently supporting deworming interventions. As of 2020, Evidence Action supports deworming interventions in four countries.

### Benefits   


#### Adjusting for different prevalence rates  

To account for different prevalence rates ($\eta$), the estimated treatment effect is decomposed in the impact of deworming on children who were treated and had a worm infection, or the effective treatment effect of deworming ($\lambda_{1}^{eff}$), and children who were treated and did not have a worm infection. By construction, the effect on this last group should be zero. Hence the effective treatment of deworming on infected populations will be equal to the estimated treatment (on the ovearll population), divided by the proportion of the prevalence of infections.

In the original evaluation, the prevalence rates were very high (0.92), hence the effect on the infected population was similar to that of the overall population. Currently deworming interventions are often implemented in geographies with much lower prevalence rates (though in populations with sufficient infection to justify treatment in accordance with World Health Organization guidelines), hence to obtain the expected effect over the new region, we need to multiply the effect on the infected population by the prevalence rate in the new region ($\eta_{new}$).


<details><summary>Show all the details</summary>

For approach 3, we will modify treatment effects of approaches 1 and 2 (equation 4 and 13 respectively) by the following:   

\begin{equation}
\lambda_{1} = \eta \lambda^{eff}_{1} + (1 -  \eta) \times 0 \\
\lambda^{r}_{1} = \eta_{new}\lambda^{eff}_{1}

\label{eq:16}
\tag{16}
\end{equation}

Where:

- $\lambda_1$: direct effects of deworming on individuals' earnings. Here we use the symbol for treatment effect of approach 1, but the same logic applies to the treatment effect of approach 2 ($\alpha^{pooled}$)
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

The number of consecutive years over which a population is exposed to deworming treatment determines the intensity of the effects over this population over time. The two approaches reproduced so far hold the length of treatment constant at the levels estimated by the original study (2.4 years). In this third approach we allow for the years of treatment to vary affecting both benefits and costs. We assume that the effects are linear in the number of years of treatment, with no additional effects after 6 years of treatment. We assumed a maximum of 6 years of impact in this case based on the 20 year KLPS follow-up research, which shows a levelling-off of treatment effect after approximately 6 years of deworming (@klps4; Figure A.5 in Appendix, page A-6).

Adding the element of treatment duration allows us to take into account differences in the number of years of deworming treatment across different country contexts depending on program dynamics. Although the counterfactual of worm prevalence in the absence of treatment is largely unknown, we know that consistent deworming continues to decrease worm prevalence over time, contributing to controlled worm environments and sustained benefits. In many deworming programs today, children receive regular treatment throughout a portion (and in some cases for the full term) of their primary schooling. It is worth noting that the assumption of linearity is an imperfect measure for various epidemiological reasons, though we include this variable of time into the equation as an estimate of the best guess at the differences in achieved impact over time, and in part because it helps capture that a new cohort enters primary school--and is therefore eligible for treatment--with each successive year of a deworming program.

<details><summary>Show all the details</summary>

For approach 3, we will modify treatment effects of approaches 1 and 2 (equations 4 and 13 respectively) by the following:   

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
Now the benefits are flexible to worm prevalence and lenght of treatment. To facilitate comparison with the other two approaches, we present here the results using the same prevalence and length of treatment assumptions parameters as in approach 1 and 2. Both approaches implicitly assume prevalence rates of 100% and do not distinguish between original population and target populuation. Both approaches also set the length of treatment at 2.41 years.
-->

To compute the benefits for this approach, this paper uses data on prevalence and length of treatment for the four countries that Evidence Action has records for. Readers interested in assessing the effects of deworming for a specific value of prevalence and length of treatment are referred to the [interactive app](https://fhoces.shinyapps.io/shiny_app_test/) (tab on key assumptions) where they can input the values that best reflect their setting. To facilitate comparison with the other two approaches, this paper presents here the results using the same length of treatment assumptions parameters as in approach 1 and 2.  

Under approach 3, and using the same assumptions as above, the benefits will be: 77.61 and 702 when using benefits of approach 1 without and with externalities, and 289.88 when using the benefit structure of approach 2.  

### Costs

Evidence Action's Deworm the World Initiative provides technical assistance to governments to implement school-based deworming programs. Deworm the World works closely with policymakers and government staff who are responsible for ensuring the implementation of deworming programs within their geographies to plan, scale, and sustain school-based deworming programs targeting at-risk children. Deworm the World works to gain and maintain critical support amongst these key stakeholders, thus having important influence over how policymakers take-in and use evidence for decision making. Through Evidence Action's technical assistance, which typically includes financial support for program implementation, they have access to country-level government cost data on what it takes to implement and evaluate school-based deworming programs across different contexts. To estimate the costs in this analysis, we use costs of deworming provided by Evidence Action (detailed below) and follow a similar approach to @givewell, which takes those costs and includes an additional estimate around the amount of government staff time required to run deworming programs. The default cost is the per unit cost per treatment round per child across all countries. This is obtained as the weighted average of per unit costs ($c_{i}$) in all countries where Evidence Action currently has data on implementation of deworming interventions [^10].

[^10]: In some settings Evidence Action provides two rounds of treatment per year. In those cases, the unit costs discussed here represent the sum of both rounds [CONFIRM WITH GRACE]

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

The unit costs of treatments, although small, vary substantially across regions. When including cost information for all the countries where Evidence action has data (India, Kenya, Nigeria, Vietnam) the unit costs is $0.08 per round of treatment [CONFIRM WITH GRACE, HARMONIZE WITH FOOTNOTE]. This final cost is primarily driven by the cost, and large population, of India, with a unit cost of $0.06, the other 3 remaining countries have relatively larger unit costs: $0.54, $0.86, $0.52 for Kenya, Nigeria and Vietnam respectively.





## Accounting for Uncertainty

This open policy analysis has aimed to make all the analysis presented so far highly reproducible. One direct result of this novel approach is that now it is possible to thoroughly assess how the final policy estimates change when any of the underlying sources of the analysis changes. In this OPA we have identified each source used in the analysis behind benefits and costs of Deworming interventions. Each of these sources in turn is measured with some uncertainty (either in prediction of future values or estimation of past ones). Traditional policy analysis assumes that each of these sources has no uncertainty, and in some cases incorporates uncertainty or performed sensitivity analysis for a few parameters of interest. By following the open policy analysis principles we now can allow for each source to vary and explore the overall uncertainty of the final policy estimate.

Our approach consists in assuming that each source used in the analysis can be represented as a random draw from a normal distribution. The mean corresponds to the measured value. The standard deviation corresponds to the estimated standard error when available, and to a fraction of the mean when not available. As a default analysis we suggest to set it these standard deviations to 10% of the mean. This choice is arbitrary, but unlike the default arbitrary choice of setting the standard deviations to zero, it makes explicit the uncertainty and it can be modified in the app.


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
        one_run_f(main_run_var1 = FALSE,              # HERE I NEED TO PLUG costs1_costs_sim
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

In this document we have presented three different approaches to measuring the welfare effects of deworming  interventions. The first approach was based on the original paper that measured the welfare effects of deworming (@baird2016worms) and proposed four different ways to compute this effect (with and without externalities, and from a societal or fiscal perspective). The second approach, based on more recent data, focused only on direct effects, and relies less on predictive effects over the lifecycle. Results for the second approach are also separated between the societal and fiscal perspective.   

The third and final approach uses similar methodologies with three main differences. First, we allow the benefits to be scaled to account for differences in the prevalence of worm infections in settings different from the original study. Second, we allow the benefits to be scaled by the length of treatment provided to children within a particular setting. Finally, based on feedback from Evidence Action on the relevant costs from present-day deworming programs, this approach uses more up to date information on treatment costs and it does not take into account the knock-on effects of additional schooling costs as a result of increased school attendance, which are accounted for in approaches #1 and #2[^11].

[^11]: Evidence Action suggests that the added costs on education will not be considered as costs from a policy makers perspective. Those costs corresponds to another intervention on itself (education) and incorporating its costs would also require to incorporate its benefits. [CONFIRM WITH GRACE]

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
```

```
## [1] "Output has changed at lambda1_new_in  to  79.51"
## [1] "Output has changed at lambda1_prevl_new_in[1]  to  43.3253546806668"
## [1] "Output has changed at pv_benef_tax_new_in  to  88.1768630910102"
## [1] "Output has changed at pv_benef_all_new_in  to  531.98710763807"
## [1] "Output has changed at pv_benef_all_prevl_new_in  to  289.882154747346"
```

```r
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
```

```
## [1] "Output has changed at klps4_1_pe  to  55.8791084799758"
```

```r
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2_pe <- NPV_pe_f(benefits_var = pv_benef_all_new_in, costs_var = costs_a2_in)
unit_test_f(klps4_2_pe, 499.720465340588)
```

```
## [1] "Output has changed at klps4_2_pe  to  499.689353027036"
```

```r
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

```
## [1] "Output has changed at ea3_pe  to  289.734896575078"
```

</details>

| Approach    | Benefits                                     | Costs                          |   NPV
|---------|-------------------------------------------------|--------------------------------|----------|
| 1.1 | @baird2016worms w/tax and no externalities (no ext) |  Treatment, Education          | 11.8   |
| 1.2 | @baird2016worms w/t and ext                         |    Treatment, Education (w/ext)| 101.9 |
| 1.3 | @baird2016worms  all and no ext                     |  Treatment, Education          | 130.6   |
| 1.4 | @baird2016worms all and ext                         |  Treatment, Education (w/ext)  | 741.6 |
| 2.1 | @klps4  w/t and no ext                              |  Treatment, Education          | 55.9  |
| 2.2 | @klps4  all and no ext                              |  Treatment, Education          | 499.7  |
| 3.1 | 1.3 + prevalence + length of treatment              | Treatment (EA)                 | 77.5      |
| 3.2 | 1.4 + prevalence + length                           | Treatment (EA)                 | 701.8      |
| **3.3**    | **2.2 + prevalence + length**                | **Treatment (EA)  **           | **289.7**  |  





<br>



![](05_final_opa_files/figure-html/run-mc-1.png)<!-- -->

# References


[^1]: EAs version of the analysis follows a similar structure than the cost effectiveness analysis performed by the charity evaluator GiveWell [@givewell].


[^5]:`F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]` Original [here](https://docs.google.com/spreadsheets/d/1jzS693Y-ZAIloQejlzSc3e3t7iPHyor1qt7HBjSVXhQ/edit#gid=509033857), editable version [here](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=509033857)
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter.


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit


[^7]: series avalable in file `~/opa-deworming/docs/materials/original_materials/Baird-etal-QJE-2016_fiscal-impact-calculations.xlsx` worksheet`Assumps&Panel A Calcs!A93`
