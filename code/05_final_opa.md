---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "23 July, 2020"
output: 
  bookdown::html_document2:
    code_folding: hide
    code_download: true
    collapsed: yes
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    toc: yes
    toc_depth: 3
    toc_float: yes
editor_options:
  chunk_output_type: console
title: "A Unifying Open Policy Analysis for Deworming"
bibliography: bibliography.bib  
link-citations: true
---
\def\blue{\color{blue}}
\def\red{\color{red}}









```r
################
#####  Notes:
################
# Types of objects:
### Source ------->  Input ------->  Model ------->  Policy Estimates (output)
###  (_so)           (_in)           (_mo)           (_pe)
### values           functions       functions       values
###                  & values        & values             
# Examples:                   
# - call_sou_f     - tax_elas_in_f   - tax_rev_mo_f  - ten_year_revenue_pe
# - policy_f       - est_bill_in_f   - tot_rev_mo_f  - ten_year_top_tax_pe
#                                    - ten_yrs_mo_f  - total_rev_pe
### arguments in functions should used "_var" and functions should "_f"

# DESCRIBE CHUNK STRUCTURE
# - inputs: list
# - outputs: list
#### function:  
#sample_function_f <- function(){
##########################################
##########################################  
#
# here goes the content
#
##########################################
##########################################  
#    return( )                         # A list with all (most?) the elements
#}                                     # generated inside the function
#invisible( list2env(sample_function_f(),.GlobalEnv) )
#
```



```r
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_params <- function(){
###############################################################################
###############################################################################  
    #############
    ##### Data  
    #############
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    gov_bonds_new_so <- 0.09
    inflation_new_so <- 0.04

    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B
    profits_se_so <- 1766          #Control group monthly self-employed profits -
                                   #Table 4, Panel A FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 4, Panel D
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 4, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 4, Panel A
    ex_rate_so <- 74               #Exchange Rate - Central Bank of Kenya 74 , 85

    ex_rate_2018        <- 101.30  # Exchange rate (KES per international $)
                                    # - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)

    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    tax_so <- 0.16575              #ADD INFO!
    unit_cost_local_so <- 43.66    #Deworm the World

    unit_cost_so <- 0.42           # Unit cost of deworming (in 2018 USD) - from Evidence Action
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so
    unit_cost_2017usdppp_so <- unit_cost_ppp_so*cpi_2017_so/cpi_2018_so

    years_of_treat_so <- 2.41      #Additional Years of Treatment - Table 1, Panel A
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
    #alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55,
    # "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    df_alpha_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
    # alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE

    #############
    ##### Research
    #############
    df_research_so <- read_csv("rawdata/research/research_params.csv")   
    lambda1_so <- c(3.49, 0)            #Hrs per week increase for men and women 
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, Panel B
    lambda1_new_so <- c(86.54642,   # avg treatment effect from klps2 (already adjusted for ppp and inflation) - w@w
                               82.99311,   # avg treatment effect from klps3 (already adjusted for ppp and inflation) - w@w
                               85.44088)   # avg treatment effect from klps4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(43, 83, 172)  # ADD SOURCE
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct secondary schooling increase (from Joan)
                        0.0239963665555466, 0.027301406306074, 0.0234125454594173,
                       0.0279278879439199, 0.00647044449446303, 0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)
    delta_ed_ext_so <- c(-0.0110126908021048,	0.0140448546741008,	-0.0034636291545585,  #Additional externality secondary schooling increase (from Joan)
                           0.0112940214439477,	0.0571608179771775,	-0.0560546793186931,
                           0.0558284756343451,	0.1546264843901160,	0.0055961489945619)
    delta_ed_ext_so <- cbind(delta_ed_ext_so, 1999:2007)    
    delta_ed_par_so <- 1
    delta_ed_ext_par_so <- 1
    include_ext_so <- TRUE
    alpha_0_so <- 1#0.77 #0.77
    alpha_r_so <- 1#0.15 #0.15
    #############
    ##### Guess work   
    #############
    periods_so <- 50               #Total number of periods to forecast wages
    time_to_jm_so <- 10            #Time from initial period until individual join the labor force
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)
                                                    #- see notes(0.1019575, -0.0010413), (0,0)
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    teach_sal_new_so <- (50000*12/49.77)
    teach_ben_new_so <- 0
                                  #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits -
                                  #news sources * 12 / ex_rate_2017_ppp_so
                                  # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                  # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    teach_sal_2017usdppp_so <- teach_sal_new_so * cpi_2017_so / cpi_2017_so # redundant, but for the sake of consistency

    n_students_so <- 45            #Average pupils per teacher	45
    staff_time_so <- 0.3           #Added Deworming costs due to goverment staff time
    run_sim_so <- FALSE
    main_run_so <- TRUE
    rescale_so <- TRUE
    costs_par_so <- 1
    costs_par_sd_so <- 0.1
    counts_par_so <- 1
    counts_par_sd_so <- 0.1
    nsims_so <- 1e1
    # options: "baird1_sim","baird2_sim","baird3_sim", "baird4_sim", "klps4_1_sim",
    # "klps4_2_sim", "ea1_sim", "ea2_sim", "ea3_sim", "cea_no_ext_ea_sim", "rcea_no_ext_ea_sim"
    policy_estimate_so <- "ea3_sim"

    # Fix teach_sal_so       
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
###############################################################################
###############################################################################    
}
invisible( list2env(chunk_params(),.GlobalEnv) )

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





# ADD A STANTARD DESCPRIPTION OF AN OPA REPORT {-}

- Complete narrative description. 
- Equations and code added to implement the narrated description. 
- Displayed in a layered fashion. Text should provide a good narrative explanation to policy makers, equations and code are meant to provide a full picture to analysts and researchers. 

# Introduction  

Mass deworming has demonstrated to be a highly effective public health intervention in the past. Here we provide a policy analysis that compares benefits and costs of deworming for different potential new settings. The goal of this analysis is to provide the best empirical information for policy makers debating the implemention of a deworming policy. This document describes all the analytical steps required to reproduce the analysis, displaying the actual computer code use in each step. In addition to this report, the reader can find all the materials to reproduce the findings presented here in [github.org/bitss/opa-deworming](https://github.org/bitss/opa-deworming). The main output presented in Figure \@ref(fig:main-pe-print), and described in the [results section](#policy-estimate) of this report, can also be explored interactively for different assumptions in [this web app](add link).


The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:     

  1. the original CBA produced by @baird2016worms,   
  2. an updated version of such analysis by a symilar research team [@klps4], and   
  3. a third approach that borrows some components of the previous two and some specific components added after consulting with a key stakeholder in this area, the non-governmental organization (NGO) Evidence Action (EA)[^1].



<!--
OLD TEXT (DELETE SOON):
The key policy estimate consists of a cost effectiveness analysis that compares the present
value of benefits and costs. The benefits quantified here are the effects on wages an the
costs are those of delivering the deworming treatment.  

The benefits will account for the direct effects of deworming and plus the indirect effects of deworming due to smaller pool of sick people in the community (herd inmunity). Effects are computed as a change in the earning profile of the population.

This analaysis contains elements from GiveWell's cost effectiveness analysis (see [here](https://docs.google.com/spreadsheets/d/1McptF0GVGv-QBlhWx_IoNVstWvt1z-RwVSu16ciypgs/edit#gid=1537947274), an editable version can be found [here](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1537947274))  and the cost benefit analysis described in [Baird et al., 2016](https://academic.oup.com/qje/article/131/4/1637/2468871).  
-->

# Methodology  

We first describe the common elements across all three approaches, and then describe each approach in detail.

## Common structure

The starting point is a comparison of a stream of benefits and costs over the lifetime of the recipients of deworming. The final policy estimate is the discounted sum of all costs and benefits, known as the Net Present Value (NPV). Another format to present this analysis is as a cost effectiveness ratio, in absolute terms or relative to the benchmark of cash transfers.

<details><summary>Show all the details</summary>

\begin{equation}
NPV = B - C \\
CEA = \frac{B \times F_0}{C} \\
RCEA = \frac{CEA(B,C)}{CEA_{cash}}

\label{eq:1}
\tag{1}
\end{equation}


```r
# - inputs: total per capita benefits, total per capita costs, fudging factor
# - outputs: Cost-effectiveness ratio & ratio to cash CEA
chunk_policy_est <- function(){
###############################################################################
###############################################################################  

    NPV_pe_f <- function(benefits_var = 1, costs_var = 1){
        benefits_var - costs_var
    }
    CEA_pe_f <- function(benefits_var = 1, fudging_var = 0, costs_var = 1) {
        ( benefits_var * ( 1 + fudging_var ) ) / costs_var
    }
    RCEA_pe_f <- function(CEA_var = 1, CEA_cash_var = 1){
        CEA_var / CEA_cash_var
    }

###############################################################################
###############################################################################  
    return(list("CEA_pe_f" = CEA_pe_f,
                "RCEA_pe_f" = RCEA_pe_f,
                "NPV_pe_f" = NPV_pe_f))
}
invisible( list2env(chunk_policy_est(),.GlobalEnv) )
```


</details>
<br>

Benefits are equal to the additional earnings that indivudual are expected to generate due to a deworming treatment. These additional earrnings are computed as a discounted sum over their working lifetime.  


<details><summary>Show all the details</summary>


\begin{equation}
B =   \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} E_{t}

\label{eq:2}
\tag{2}
\end{equation}


```r
# - inputs: nothing
# - outputs: function that computes the country weights used in the final costs
chunk_benefits <- function(){
###############################################################################
###############################################################################  

    pv_benef_f <- function(earnings_var = earnings_in, interest_r_var = interest_in,
                    periods_var = periods_so) {
      index_t <- 0:periods_var
      res1 <- sum( ( 1 / (1 + interest_r_var) )^index_t * earnings_var )
      return(res1)   
    }

###############################################################################
###############################################################################  
    return(list("pv_benef_f" = pv_benef_f))
}
invisible( list2env(chunk_benefits(),.GlobalEnv) )
```
</details>
<br>

<!-- Emma: figure out how to add half a line break, as oppose to one full "<br>"-->



At a high level all three approaches focus on the same type of benefits: the increase in incomes over the lifetime of beneficiaries of deworming. This is probably an under-estimate of the benefits as it does not quantify the non-pecuniary effects of improved health.  The costs can be separated into direct costs of implementing deworming policies, and indirect costs associated with the benefits of deworming.

The main differences across the three approaches regarding benefits have to do with how to predict the earnings profiles over a lifecycle, and whether or not to account for different prevalence rates. Approaches 1 and 2 use different earning profiles, and approach 3 combines both earning profiles and adjusts for possible differences in prevalence rates of worm infections.

The main differences in costs have to do with whether indirect costs are included, and what is the relevant unit cost for the analysis. The first two approaches include indirect costs and use the unit costs of a specific country (Kenya) while the third approach does not include indirect costs and use unit costs of multiple countries. 

In summary, approaches 1 and 2 reflect different types of estimating earning profiles for one specific context (Kenya). And approach 3 incorporates both earning profile methodologies, focuses only on direct costs, and generalizes costs and prevalence for different settings. 

### The discounting rate  

All approaches use the real interest rate ($r$) as the discounting rate. This is obtained from the interest rate on goverment bonds ($i$) minus the inflation rate ($\pi$).

<details><summary>Show all the details</summary>

\begin{equation}
r = \frac{1 + i}{1 + \pi} - \pi \\
r \approx i - \pi

\label{eq:3}
\tag{3}
\end{equation}

TO DO: after confirming that reproduction works, change all `interest_in` to `interest_exct_in`


```r
# - inputs: gov_bonds_so, inflation_so 
# - outputs: interest_in 
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
interest_16 <- as.numeric( 
  interest_f(gov_bonds_var = gov_bonds_so,
             inflation_var = inflation_so)$interest_in 
  )
interest_19 <- as.numeric( 
  interest_f(gov_bonds_var = gov_bonds_new_so,
             inflation_var = inflation_new_so)$interest_in  
  )
```

</details>
<br>

The actual value will vary across approaches depending on the time and country chosen. For example approach 1 used the return from government bonds and inflation in Kenya for the year 2016, while approach 3 used the values for the same country but for the year 2019. This resulted in discount rates of 9.85% and 5% for approach 1 and 3 respectively. 


-------

<details><summary>View Summary Table</summary>

<!-- Emma: figure out a way to remove "(#tab:sum_tables2)" from the caption of the summary tables. -->

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables2)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables2)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

</details>


## Approach 1: @baird2016worms

In this first approach, the effect on earnings over the entire lifecycle are predicted by extrapolating the effects on hours worked by individuals in the original treatment group, ten years after the intervention. 

Two types of results are presented: the total effect on earnings projected over a lifetime and the estimated fiscal effect due to the government collecting additional taxes on higher earnings. The effects are calculated in two scenarios: with and without externalities over the population of children who did not receive deworming interventions.


###  Gains in earnings

Gains in earnings ($E_t$) are the result of multiplying expected earnings in a certain period ($w_t$) with the effects of deworming on worked hours. This effect can have two components: a direct effect of deworming on the individual ($\lambda_1$) and the indirect effect on earnings due to externalities ($\lambda_2$). The indirect effects are considered within the context of the treatment's coverage and saturation.

<details><summary>Show all the details</summary>

\begin{equation}
E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)

\label{eq:4}
\tag{4}
\end{equation}

Where[^6]:   

 - $w_t$: are the earnings in period $t$.   
 - $\lambda_{1}$: is the direct effects of deworming on earnings.  
 - $\lambda_{2}$: is the indirect effects of deworming on earnings.   
 - $p$: saturation, measures the fraction of the population that is effectively using the treatment.  
 - $R$: coverage, defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group.  

[^6]: The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender.

<!-- Emma:  please make sure each code chunk with a "chunk_*" function has its inputs and outputs listed in its first two lines-->


```r
# - inputs: wage_in, lambda1_so, lambda2_so,
# - outputs: earnings (no name speceified)
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings1_f <- function(wage_var = wage_in,
                          lambda1_var = lambda1_so,
                          lambda2_var = lambda2_so,
                          saturation_var = saturation, coverage_var) {  
        res1 <- wage_var * ( lambda1_var + saturation_var * 
                               lambda2_var / coverage_var )
        return(res1)
    }

###############################################################################
###############################################################################  
    return(list("earnings1_f" = earnings1_f))
}

invisible( list2env(chunk_earnings1(),.GlobalEnv) ) 
```

</details>
<br>

#### Earnings over time

Wages in year $t$ correspond to the initial weekly wage ($w_0$) adjusted by an economy-wide increase in salaries and by an increase in salaries due to additional experience at the individual level. The economy-wide wage adjustment is assumed to be equal to the per capita GDP growth ($g$) applied to however many years of work ($Xp$). The life cycle path for wages increases at decreasing rates (wages typically increase with more years of work, then decline later in a life cycle). It is assumed that individuals enter the labor force 10 years after the treatment period. Weekly wages are multiplied by 52 weeks to obtain the annual rate. 

The initial wage in dollars ($w_{0}$) is a weighted average of wages for the control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the fraction of all the average worked hours dedicated to each sector ($h$).  

<!--Emma: pleas find the specific reference from in Suri (page, and table #,  location), and add using the @notation (you will need to edit the bibliography.bib file). If you cannot find the Suri reference, send me an email and I will look for it-->

The wage in agriculture comes from research (Suri, 2011), the working wage comes from the data and is defined as an hourly wage for the control group for those who reported more than 10 hrs of work per week. The self-employed wage ($w_{se}$) was constructed as the reported monthly earnings from self-employed profits, divided by the reported weekly number of hours work in self-employment for those who worked a positive number of hours (multiplied by 4.5 to obtain the monthly total). 

<!-- Emma, please record where are exactyl the values from below-->

The monthly self-employed profits and self-employed hours for the control group, for those with possitive hours, come from data [@baird2016worms]. The measure of hours in self employment used to compute wagesis different from the one used to compute the weights above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of working age in the sample (hence capturing the relative importance of the self employed sector in the economy).

<details><summary>Show all the details</summary>

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50

\label{eq:5}
\tag{5}
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
\\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}

\label{eq:6}
\tag{6}
\end{equation}

\begin{equation}
w_{se} =  \frac{ \text{Monthly self-employed profits} }{4.5 \times E[h_{se}|h_{se}>0] }

\label{eq:7}
\tag{7}
\end{equation}



```r
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_mo) and function (wage_0_mo_f), 
# Wage trajectory: value (wage_t_mo) and function (wage_t_mo_f).
chunk_wages <- function(){
################################################################################
################################################################################  
    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision

    wage_0_mo_f <- function(wage_ag_var, wage_ww_var, profits_se_var, 
                            hours_se_cond_var, hours_ag_var, hours_ww_var, 
                            hours_se_var, ex_rate_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
        wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
        alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) / 
          sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
        res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
        return(res1)
    }

    wage_t_mo_f <- function(wage_0_var,
                       growth_rate_var,
                       coef_exp1_var,
                       coef_exp2_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        res1 <- 52 * wage_0_var * ( ( 1 + growth_rate_var )^experience_aux ) *
          ( 1 + coef_exp1_var * experience_aux + coef_exp2_var * 
              (experience_aux^2) ) * ifelse(0:periods_so >= time_to_jm_so, 1, 0)
        return(res1)
    }

    wage_0_mo <- wage_0_mo_f(wage_ag_var = wage_ag_so,  
                         wage_ww_var = wage_ww_so,
                         profits_se_var = profits_se_so,
                         hours_se_cond_var = hours_se_cond_so,  
                         hours_ag_var = hours_ag_so,
                         hours_ww_var = hours_ww_so,
                         hours_se_var = hours_se_so,
                         ex_rate_var = ex_rate_so)  

    #close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
    #but I suspect diff due to computational precision
    wage_t_mo <- wage_t_mo_f(wage_0_var = wage_0_mo,
                       growth_rate_var = growth_rate_so,
                       coef_exp1_var = coef_exp_so[1],
                       coef_exp2_var = coef_exp_so[2])

################################################################################
################################################################################
    return(list("wage_0_mo_f" = wage_0_mo_f, "wage_0_mo" = wage_0_mo,
                "wage_t_mo_f" = wage_t_mo_f, "wage_t_mo" = wage_t_mo))
}

invisible( list2env(chunk_wages(),.GlobalEnv) )
```

</details>
<br>

#### Deworming effects: direct and externalities 

The estimated impact of deworming on hours worked come from @baird2016worms and are estimate separately for men ($\lambda_{1,male}$) and women ($\lambda_{1,female}$). This two parameters are combined with a simple mean in the analysis. 

The estimated externality effect ($\lambda_{2}$) reflects the additional hours worked due to individuals who did not recieve the treatment but still saw reductions in the likelihood of infection due to increase inmuinity in their community.  Note that this parameter is not estimated by gender, so we repeat its value two times. All the components to the equation \\ref{eq:8} come from @baird2016worms. The externalities effects are adjusted by the coverage and saturation of the original study. 

<details><summary>Show all the details</summary>

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}\\

\label{eq:8}
\tag{8}
\end{equation}



```r
# - inputs:
# - outputs:
chunk_lambdas<- function(){
###############################################################################
###############################################################################    

    lambda1_in_f <- function(lambda1_var = lambda1_so) {
        rep(0.5 * lambda1_var[1] + 0.5 *lambda1_var[2], 2)
    }
    # lambda_r_f is describe in the third approach REVIEW
    lambda_r_f <- function(lambda1_var = lambda1_in_f(), 
                           alpha_0_var = alpha_0_so, alpha_r_var=alpha_r_so){
        lambda1_eff_temp <- lambda1_var / alpha_0_var
        return( lambda1_eff_temp * alpha_r_var )
    }  

    lambda2_in_f <- function(lambda2_var = lambda2_so){
        rep(lambda2_var, 2)
    }

##############################################################################
###############################################################################  
    return(list("lambda_r_f" = lambda_r_f,     
                "lambda1_in_f" = lambda1_in_f,
                "lambda2_in_f" = lambda2_in_f ) )
}
invisible( list2env(chunk_lambdas(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_in <- lambda1_in_f()
lambda1_r_in <- lambda_r_f()
lambda2_in <- lambda2_in_f()
```

</details>
<br>

#### Coverage and saturation of the original study [@baird2016worms]

The coverage ($R$) is defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group. As the treatment was applied to approximately two thirds of the population, $R$ is set to: $R  = 0.68$.  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively using the treatment and is defined as a weighted average of the take-up under a full subsidy for deworming and the take-up under zero subsidy.   

<!-- Emma: find the reference below, let me know if you cannot find it--> 

For this (or similar?) setting Miguel and Kremer 2007 [add page, table, col, row] estimate that there is almost no take-up without subsidy ($Q(0)$), hence  is assigned the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy ($Q(full)$) was of 0.75.

<details><summary>Show all the details</summary>

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)

\label{eq:9}
\tag{9}
\end{equation}


```r
# - inputs: coverage_so, q_full_so, q_zero_so
# - outputs: saturation_in
chunk_coverage <- function(){
###############################################################################
###############################################################################  

    saturation_in_f <- function(coverage_var = coverage_so, 
                                q_full_var = q_full_so, q_zero_var = q_zero_so){
      saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
      return(list("saturation_in" = saturation_in))
    }

###############################################################################
###############################################################################  
    return(list("saturation_in_f" = saturation_in_f))   
}
invisible( list2env(chunk_coverage(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
```

</details>
<br>

<!-- Emma: please complete the values of the summary table (from the sources code chunk)-->

-------

<details><summary>View Summary Table</summary>


<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables3)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables3)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>


</details>

### Costs

The costs are a combination of direct costs of mass deworming (relative to the status quo, which is no subsidy for deworming) and indirect costs on the education system due to the additional time treated individuals spend in school.

<details><summary>Show all the details</summary>
\begin{equation}
C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)

\label{eq:10}
\tag{10}
\end{equation}


```r
# - inputs:
# - outputs:
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

    cost2_f <- function(periods_var = periods_so, 
                        delta_ed_var = delta_ed_final_in,
                        interest_r_var = interest, 
                        cost_of_schooling_var = cost_per_student_in,
               s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in) {
        index_t <- 0:periods_var
        delta_ed_s <- c(0, delta_ed_var, rep(0,41))
        (s2_var * q2_var  - s1_var * q1_var) + 
          sum( ( 1 / (1 + interest_r_var) )^index_t *
                 delta_ed_s * cost_of_schooling_var) 

    }
    
###############################################################################
###############################################################################  
    return(list("cost2_f" = cost2_f))    # Try to return only functions
}
invisible( list2env(chunk_cost2(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:  
```

</details>
<br>

#### Direct costs: increase in deworming costs

Direct deworming costs ($DC$) are defined as the take-up under a mass deworming ($Q_{2}$) intervention, times the per-capita costs of deworming under the intervention ($S_{2}$). This costs are compared to a status quo scenario where the government does not provide any additional resource for deworming. This analysis assumes that there is no subsidy for deworming under the status quo.    

##### Complete subsidy to per capita costs of deworming

With complete subsidy, the relevant costs represent the total direct costs of deworming in USD. The take-up with full subsidy ($Q_2$) comes from a previous study [@kremer2007illusion] and takes the value of 0.75.

<details><summary>Show all the details</summary>

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\

\label{eq:11}
\tag{11}
\end{equation}


```r
# - inputs:
# - outputs:
chunk_unit_costs2 <- function(){
###############################################################################
###############################################################################  

    s2_f <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so, 
                     years_of_treat_var = years_of_treat_so) {
      ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    }

###############################################################################
###############################################################################  
    return(list("s2_f" = s2_f) )
}
invisible( list2env(chunk_unit_costs2(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
```

</details>
<br>


#### Indirect costs: additional years of education and its costs for government

With the intervention there is an estimated increase in school attendance, which is multiplied by the cost of education per student to calculate the additional indirect cost on the education system imposed by a treated individual, who tends to spend more time in school than an untreated counterpart.

The additional costs on education are computed as following: first compute a cost per student ($K$). This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

Second, the cost per student is multiplied by the estimated increase in school attendance ($\Delta \overline{E}_{t}(S1,S2)$).
For this we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from an additional analysis related to @baird2016worms.

This series does not take into account the externality effects. To incorporate it we need another series (from the same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


<details><summary>Show all the details</summary>

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}

\label{eq:12}
\tag{12}
\end{equation}



```r
# - inputs:
# - outputs:
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
delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE)
```

</details>
<br>

Without externalities, the original analysis obtains a total NPV of benefits of 142.43, with 12.9 in tax revenue for government (table 5, column 3, and rows 9, 10 respectively).

Including externalities, they obtain a total NPV of benefits of 766.81, with 102.97 in tax revenue for government (table 5, column 3, and rows 12, 13 respectively).





-----------------
<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables16)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables16)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>


## Approach 2: @klps4

In this second approach, benefits follow the same principle as in approach 1 (increase in lifetime earnings), but now there is more data on the actual effects on the labor market outcomes. Instead of projecting a trend of earnings into the future (after the estimated impact of the 10 year follow-up), this analysis uses additional data from 15 and 20 year follow-ups to the original intervention.  Costs are fairly similar to approach 1, with the addition that in the second approach, the costs also account for several rounds of treatment required for effective deworming.  Additionally. the interest rate here is updated to current values of return on (Kenyan) goverment bonds and inflation.


### Gains in earnings 

Gains in earnings ($E_t$) from 10, 15, and 20 years after intervention are used to measure the effect of multiple rounds of deworming on welfare over time. This is an important difference with approach 1, which only measures gains in earnings by year 10 and extrapolates into the future. To extrapolate after the 20-year measurement, the authors assume that the welfare gains 20 years after the intervention persist through the rest of an individual's working life. Hence the treatment effect over an individual's working life is the sum of the treatment effects on welfare at each follow-up.

Gains in earnings represents the treatment effect on welfare, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc. @klps4 assumes that the effect on welfare identified 20 years after the intervention persists through one's working life[^8]. This approach also disregards externality effects. Their estimated treatment effects ($\alpha_{t}$) for years 10, 15, and 20, are $87, $83, $85 dollars per person per year.

[^8]: In another specification the authors assume that effects disappear after 25 years. Here we select the more persistent specification.

<details><summary>Show all the details</summary>

\begin{equation}
E_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t > 20)\alpha^{KLPS4}
\text{ for } t \leq 50

\label{eq:13}
\tag{13}
\end{equation}



```r
# - inputs:
# - outputs:
chunk_new_earnings <- function(){
###############################################################################
###############################################################################  

    earnings2_f <- function(t_var = 1,
                               lambda1k1_var = lambda1_new_so[1],
                               lambda1k2_var = lambda1_new_so[2],
                               lambda1k3_var = lambda1_new_so[3]) {
        1*(10 <= t_var & t_var < 15) * lambda1k1_var +
        1*(15 <= t_var & t_var < 20) * lambda1k2_var +
        1*(20 <= t_var) * lambda1k3_var
    }

###############################################################################
###############################################################################             
    return(list("earnings2_f" = earnings2_f))
}

invisible( list2env(chunk_new_earnings(),.GlobalEnv) )
```

</details>
<br>

----------

<details><summary>View summary table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $E_t=NA$ </td>
   <td style="text-align:left;"> $t=NA$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha^{KLPS3}=NA$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>

### Costs

#### Direct costs: increase in deworming costs

Similiar to approach 1, the direct deworming costs under approach 2 are calculated by comparing the costs under a complete subsidy to the costs under the status quo of no subsidiy. The two main differences with the previous costs estimates are that now the direct costs now are summed, and discounted, over the treatment period, and prices have been updated.  


<details><summary>Show all the details</summary>

\begin{equation}
DC = \sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:14}
\tag{14}
\end{equation}

Since the analysis is discrete, and we can not sum over a non-integer, we find
\begin{equation}
DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \\
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:15}
\tag{15}
\end{equation}




```r
# - inputs:
# - outputs:
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

    s2_f_new <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     interest_var = interest_19) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      sum(( unit_cost * (1 + interest_var)^(-(0:2)) ) * c(1,1,0.4))
    }

###############################################################################
###############################################################################  
    return(list("s2_f_new" = s2_f_new) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:

s2_in <- s2_f_new(interest_var = interest_19, unit_cost_local_var = 0.8296927, ex_rate_var = 1)
s2_new <- s2_in
q2_in <- q_full_so
```

</details>
<br>

With complete subsidy, the costs of the intervention become the total direct costs of deworming each child (in USD). Most recent (2018) data from Evidence Action reveals this cost to be \$0.42 per year. Adjusting for purchasing power and inflation, we get a per capita cost of \$0.83. Adding all indirect cost over an average 2.4 years of treatment, the average cost of deworming each child over the entire treatment period is $1.44.


#### Indirect costs: additional years of education and its costs for government  

The indirect cost on the education system is calculated similarly to approach 1: the cost per student is multiplied by the increase in school attendance due to deworming. The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school as a result of deworming. This analysis assumes that pressure is added to educational institutions for a maximum of nine years, starting at year zero. The cost per student ($K$) is updated with new information on annual teacher salary (including benefits)[^9], $12055 (also adjusted for PPP), and same average number of students per teacher (45).

Hence, the cost of schooling each child for an additional year is now $267.9 (USD).

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor [FIND SOURCES]. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FIND SOURCE]**.

<!-- Emma (w11): find sources for the above footnote-->


<details><summary>Show all the details</summary>

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)

\label{eq:16}
\tag{16}
\end{equation}



```r
delta_ed_in <- delta_ed_so[,1]
cost_per_student_in_new <- cost_per_student_f(teach_sal_var = (50000*12/49.77),
                                          teach_ben_var = 0,
                                          n_students_var = 45)
```
</details>
<br>

Over this nine year period, students attended school for an additional 0.15 years on average. Then we get an average cost of additional schooling per child over the nine-year period, $32.40.

TO DO:  add a sentence with final NPV (analogous to last sentence in app1) 

-------------------


<details><summary>View summary table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables20)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables20)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
 

## Approach 3: Combinantion of Previous Approaches and Input From Policy Makers

In this third and final approach, we borrowed some methodological elements from @baird2016worms and @klps4 and worked in collaboration with a key policy maker in this area: the NGO Evidence Action (EA). EA provided feedback on what are the relevant costs and benefits that they, and other local policy makers like ministries of health, would consider when making decisions on deworming interventions. Additionally, EA provided insights and data on what are the key main costs of implementing a deworming intervention in different countries. 

In this final approach the benefits from deworming described in the previous approachces are scaled to reflect differences prevalence rates. Additionally, the relevant costs are constrained to be only the direct costs, with varying implementation costs for different countries. 


When the original deworming study was conducted in Kenya in 1999, the prevalence rates of the different types of worms infections where of up to XXX for the relevant population, and the costs of for this setting where of XXXX. Today Evidence Actions supports deworming interventions in XXX countries, with prevalence rates ranging from XXXX to XXXXX and costs ranging from XXXXX to XXXXX. 

<!-- Emma (11): please read the three paragraph above and improve writing -->

### Benefits   

In order to account for different prevalence rates, the estimated treatment effect are decomposed in the the impact of deworming on children who where treated ($\lambda_{1}^{eff}$) and had a worm infection and children who where treated and did not had a worm infection. By construction, the effect on this last group should be zero.

Refer to equations 13 (app2) 4, and 8 (app1)  

#### $\lambda_{1}^{eff}$  

\begin{equation}
\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0

\label{eq:17}
\tag{17}
\end{equation}


Where:      

 - $\alpha$: represents the incidence of the condition.  
 - $\lambda_{1}^{eff}$: represents the effect of deworming over those affected with the condition.  

**TO DO: add a section that discusses where are the $\alpha's$ comming from**   

In the original evaluation, $\alpha = 0.77$, hence $\lambda_{1}^{eff} = 1.75/0.77 = 2.72$. The value of $\lambda^{r}_{1}$ for each region $r$ will depend on that region's $\alpha^{r}$.  

### Costs

For each country where Evidence Action provides technical assistance, GiveWell estimates the deworming cost per child using a combination of technical assistance costs, indirect and direct government expenditure, and partner costs. GiveWell's estimated cost is a weighted average of the unit costs across countries: for each country, the annual per capita cost is weighted by the proportion of the country's deworming costs relative to the total deworming costs across countries.

<details><summary>Equation</summary>
\begin{equation}
C = \sum_{i \in Countries } \omega_{i} c_{i}

\label{eq:18}
\tag{18}
\end{equation}

GiveWell estimates the cost per child dewormed in geographies where Evidence Action provides technical assistance. These costs include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such as the cost of drugs donated by WHO.

Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The final cost is a weighted average of the unit cost across countries.

- $\omega_{i}$: Weight for the weighted average.  
- $c_{i}$: Total cost per child, per year in country $i$.  
</details>

```r
# - inputs: nothing
# - outputs: function that computs the weighted sum of country costs
chunk_cost1 <- function(){
###############################################################################
###############################################################################  

    costs1_f <- function(country_w_var = 1, country_cost_var = 1) {
        sum(country_w_var * country_cost_var)
    }

###############################################################################
###############################################################################  
    return(list("costs1_f" = costs1_f))
}


invisible( list2env(chunk_cost1(),.GlobalEnv) )
```



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

All costs are in USD.

GW original analysis weights each country to take into account the number of treatments provided as well as the proportion of costs incurred by DtWI in that geography. The analytical foundations for such weights are not clear. Also not clear why should only account for DtW costs.  


- $N_{i}$: Number of treated children in country $i$.  
- $Ex_{i}$: Exchange rate from country $i$ to USD.  
- $k$: Costs distribute across $k$ payers.   
- $l$: Each payers costs come from $l$ items.   



```r
# - inputs: nothing
# - outputs: function that computes the country weights used in the final costs
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
    costs1_costs_f <- function(df_costs_var = df_costs_so,
                               df_costs_cw_var = df_costs_cw_so,
                               staff_time_var = staff_time_so){
      # Add country
      df_costs_var <- df_costs_cw_var %>%
        right_join(df_costs_var, by = "Country/State") %>%
        select(-Country.y) %>% rename(Country = Country.x) %>%
        mutate(Country = tolower(Country))
      # values for last year with cost information
      df_costs_last <- df_costs_var %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_costs_var, by = "Country") %>%
        filter(Year == last_year)    

      # summing across payers and regions (last equation)
      costs_by_item_temp <- df_costs_last %>%
        filter(Payer != "Total") %>%
        group_by(Country, `Program Area`) %>%
        summarise("costs_by_region" = sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE))

      #sum across country/state and multiply by delta
      country_cost <- costs_by_item_temp %>%
        group_by(Country) %>%
        summarise("costs_by_country" = sum(costs_by_region) * (1 + staff_time_var))  

      return( list("cost_data" = country_cost) )
    }

    costs1_counts_f <- function(df_counts_var = df_counts_so,
                          df_costs_cw_var = df_costs_cw_so){
      # Add country
      df_counts_var <- df_costs_cw_var %>%
        right_join(df_counts_var, by = "Country/State") %>%
        mutate(Country = tolower(Country))

      # values for last year with cost information
      df_counts_last <- df_counts_var %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_counts_var, by = "Country") %>%
        filter(Year == last_year)    

      c_counts <- df_counts_last %>%
        group_by(Country, Year) %>%
        summarise("total" = sum(`# dewormed`))

      return( list("counts_data" = c_counts) )
    }

    costs1_ratios_in_f <- function(counts_var = costs1_counts_f()$counts_data,
                                   costs_var = costs1_costs_f()$cost_data){
      # create country weight
      c_weights <- counts_var %>% ungroup() %>%
        mutate(country_w = total / sum(total))

      # Compute the per capita cost for each country
      ratios_data <- costs_var %>%
         left_join(c_weights, by = "Country") %>%
         mutate("per_cap" = costs_by_country / total)

          return( list("ratios_data" = ratios_data) )
      }

###############################################################################
###############################################################################  
    return( list("costs1_costs_f" = costs1_costs_f,
                "costs1_counts_f" = costs1_counts_f,
                "costs1_ratios_in_f" = costs1_ratios_in_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )
```

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables21)Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $N_i=NA$ </td>
   <td style="text-align:left;"> $Ex_i=NA$ </td>
   <td style="text-align:left;"> $k=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table18)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table18)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

</details>

#### Data required to compute costs.

$N_{i}, C_{i,k,l}, \delta_{g}$



<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables22)Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\alpha=NA$ </td>
   <td style="text-align:left;"> $\lambda_{1}^{eff}=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table20)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table20)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>

### Different format of policy estimate {#policy-estimate}

The key result for policy makers is defined as the cost effectiveness ratio (cell [`Deworming!B32`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B32)).

\begin{equation}
CEA_{deworming} = \frac{B (1 + F_{0})}{C}

\label{eq:20}
\tag{20}
\end{equation}

 - $C$ is the costs per person dewormed (`F2, 4,B23` --> [`F1, 2, H16`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=H16)).     
 - $B$ is the benefits per person dewormed (`F2, 4,B22`).
 - $F_{0}$ is a factor to account for leverage/fudging [not reviewed in this excercise] ([`F2, 6, D259`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1611790402&range=D259))


Also this quantity could be expressed in relative terms to the benchmark of cash transfers (cell [`Results!B9`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1034883018&range=B9)):

\begin{equation}
RCEA = \frac{CEA_{deworming}}{CEA_{cash}}

\label{eq:21}
\tag{21}
\end{equation}

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables23)Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $C=NA$ </td>
   <td style="text-align:left;"> $B=NA$ </td>
   <td style="text-align:left;"> $F_0=NA$ </td>
  </tr>
</tbody>
</table>
</details>

<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table21)Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $CEA_{deworming} = rac{B (1 + F_{0})}{C}$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $RCEA = rac{CEA_{deworming}}{CEA_{cash}}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table21)Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

</details>
# Main results


| Name    | Benfits                                   | Costs        |
|---------|-------------------------------------------|--------------|
| Baird 1 | Baird w/tax and no externalities (no ext) | Baird no ext |
| Baird 2 | Baird w/t and ext                         | Baird ext    |
| Baird 3 | Baird all and no ext                      | Baird no ext |
| Baird 4 | Baird all and ext                         | Baird ext    |
| KLPS4_1 | KLPS4 w/t and no ext                      | Baird no ext |
| KLPS4_2 | KLPS4 all and no ext                      | Baird no ext |
| EA 1    | Baird all and no ext                      | EA           |
| EA 2    | Baird all and ext                         | EA           |
| EA 3    | KLPS all and no ext                       | EA           |  




```r
# Function dependency is depicted as follows:
# f(g()) =
# f
# └──── g
#
#       ##     ###    ####
# 1     2       3     4
#       ##     ###    ####
# NPV_pe_f, CEA_pe_f, RCEA_pe_f
# ├──── pv_benef_f
# │      ├──── earnings1_f
# │      |      ├──── wage_t_mo_f
# │      |      |      └──── wage_0_mo_f
# |      |      ├──── lambda1_in_f
# │      |      |      └────lambda_r_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings2_f
# │      |      └────lambda_r_f
# │      └──── interest_f
# └──── cost1_f
# │      └──── costs1_ratios_in_f
# |             ├──── costs1_costs_f
# │             └──── costs1_counts_f
# └──── cost2_f = 11.776188118988
#        ├──── delta_ed_final_f
#        ├──── interest_f
#        ├──── s2_f
#        └──── cost_per_student_f
#       ##     ###    ####

#unit test function
unit_test <- function(to_test_var, original_var, main_run_var = TRUE){
    if (main_run_var == TRUE) {
        if (length(to_test_var) > 1) {
            fails_test <- ( abs(sd(to_test_var) - original_var) > 0.0001 )
            text_val <- sd(to_test_var)
        } else {
            fails_test <- ( abs(to_test_var - original_var) > 0.0001 )
            text_val <- to_test_var
        }
        if (fails_test) {
            print(paste("Output has change at", deparse(substitute(to_test_var) ), " to ", text_val) )
        }
      }
}

#TODO: update values of unit test within one_run
# one run of all the steps to get one policy estimate
one_run <-
  function(main_run_var1 = main_run_so,
           run_sim_var = run_sim_so,
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
           alpha_0_var1 = alpha_0_so,
           alpha_r_var1 = alpha_r_so,
           lambda2_var1 = lambda2_so,                                        
           coverage_var1 = coverage_so,                                        
           q_full_var1 = q_full_so,                                        
           q_zero_var1 = q_zero_so,                                        
           lambda1_new_var1 = lambda1_new_so,                                        
           gov_bonds_var1 = gov_bonds_so,                                        
           inflation_var1 = inflation_so,                                        
           gov_bonds_new_var1 = gov_bonds_new_so,                                                     
           inflation_new_var1 = inflation_new_so,                                       
           df_costs_var1 = df_costs_so,                                        
           df_costs_cw_var1 = df_costs_cw_so,                                        
           staff_time_var1 = staff_time_so,                                        
           df_counts_var1 = df_counts_so,                                        
           counts_sim_var1 = NULL,                                        
           costs_sim_var1 = NULL,                                        
           delta_ed_var1 = delta_ed_so,                                        
           delta_ed_ext_var1 = delta_ed_ext_so,                                        
           teach_sal_var1 = teach_sal_so,                                        
           teach_ben_var1 = teach_ben_so,                                        
           n_students_var1 = n_students_so,                                        
           teach_sal_new_var1 = teach_sal_new_so,                                            
           teach_ben_new_var1 = teach_ben_new_so,                              
           unit_cost_local_var1 = unit_cost_local_so,     
           unit_cost_local_new_var1 = 0.8296927,             #  CALL SO          
           years_of_treat_var1 = years_of_treat_so,                                        
           tax_var1 = tax_so,                                        
           periods_var1 = periods_so) {                                        
    ####------------ Inputs for wage_t ---------------------------------------------
    wage_0_in <- wage_0_mo_f(wage_ag_var = wage_ag_var1, wage_ww_var = wage_ww_var1,
                             profits_se_var = profits_se_var1, hours_se_cond_var = hours_se_cond_var1,  
                             hours_ag_var = hours_ag_var1, hours_ww_var = hours_ww_var1,
                             hours_se_var = hours_se_var1, ex_rate_var = ex_rate_var1)
    unit_test(wage_0_in, 0.170124466664436, main_run_var = main_run_var1)
    ###---------- Inputs for earnings1_f -------------------------------------------
    wage_t_in <- wage_t_mo_f(wage_0_var = wage_0_in, growth_rate_var = growth_rate_var1,
                             coef_exp1_var = coef_exp_var1, coef_exp2_var = coef_exp2_var1)

    lambda1_in <- lambda_r_f(lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
                             alpha_0_var = alpha_0_var1, alpha_r_var = alpha_r_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)

    saturation_in <- as.numeric(saturation_in_f(coverage_var = coverage_var1,
                                                q_full_var = q_full_var1,
                                                q_zero_var = q_zero_var1) )
    unit_test(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)
    # ADD UNIT TEST FOR SATURATION AN LAMBDAS    

    ###------------ Inputs for earnings2_f------------------------------------------
    lambda1_new_in <- lambda_r_f(lambda1_var = lambda1_new_var1,
                                 alpha_0_var = alpha_0_var1,
                                 alpha_r_var = alpha_r_var1)
    #ADD UNIT TEST FOR LAMBDAS    

    ##------------ Inputs for pv_benef_f -------------------------------------------
    # earnings1
    earnings_in_no_ext <- earnings1_f(wage_var = wage_t_in, lambda1_var = lambda1_in[1],
                                      lambda2_var = 0, saturation_var = saturation_in,
                                      coverage_var = coverage_var1)
    earnings_in_yes_ext <- earnings1_f(wage_var = wage_t_in, lambda1_var = lambda1_in[1],
                                       lambda2_var = lambda2_in[1], saturation_var = saturation_in,
                                       coverage_var = coverage_var1)

    # earnings2
    earnings_in_no_ext_new <- earnings2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1],
                                          lambda1k2_var = lambda1_new_in[2],
                                          lambda1k3_var = lambda1_new_in[3])
    # ADD UNIT TEST
    interest_in <- as.numeric( interest_f(gov_bonds_var = gov_bonds_var1,
                                          inflation_var = inflation_var1) )
    unit_test(earnings_in_no_ext, 31.1421332040266, main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext, 167.667817450905, main_run_var = main_run_var1)
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    ###------------- Inputs for costs1_ratios_in_f----------------------------------
    costs1_counts_in <- costs1_counts_f(df_counts_var = df_counts_var1,
                                        df_costs_cw_var = df_costs_cw_var1)$counts_data
    costs1_costs_in <- costs1_costs_f(df_costs_var = df_costs_var1,
                                      df_costs_cw_var = df_costs_cw_var1,
                                      staff_time_var = staff_time_var1)$cost_data
    # ADD UNIT TEST
    # The following section only runs when running the simulations (later add: a skip to load the data only once)
    if (run_sim_var ==  TRUE) {
        costs1_counts_in$total <- counts_sim_var1
        costs1_costs_in$costs_by_country <- costs_sim_var1
    }
    ##-------------- Inputs for costs1_f--------------------------------------------
      costs1_country <-  costs1_ratios_in_f(counts_var = costs1_counts_in,
                                            costs_var =  costs1_costs_in)
      unit_test(unlist(costs1_country$ratios_data$costs_by_country),  
                6880801.84046596, main_run_var = main_run_var1)

    ##-------------- Inputs for costs2_f--------------------------------------------
    # Make explicit non-function inputs:
    delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE,
                                          delta_ed_var = delta_ed_var1,
                                          delta_ed_ext_var = delta_ed_ext_var1)
    unit_test(delta_ed_final_in, 0.01134819, main_run_var = main_run_var1)

    delta_ed_final_in_x <- delta_ed_final_f(include_ext_var = TRUE,
                                            delta_ed_var = delta_ed_var1,
                                            delta_ed_ext_var = delta_ed_ext_var1)
    unit_test(delta_ed_final_in_x,  0.05911765, main_run_var = main_run_var1)

    interest_in <- as.numeric( interest_f(gov_bonds_var = gov_bonds_var1,
                                          inflation_var = inflation_var1) )
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    interest_in_new <- as.numeric(interest_f(gov_bonds_var = gov_bonds_new_var1,
                                             inflation_var = inflation_new_var1))

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)
    unit_test(cost_per_student_in,  116.8549, main_run_var = main_run_var1)

    #TODO: remove hardcoded numbers
    cost_per_student_in_new <- cost_per_student_f(teach_sal_var = teach_sal_new_var1,
                                          teach_ben_var = teach_ben_new_var1,
                                          n_students_var = n_students_var1)

    s2_in <- s2_f(unit_cost_local_var = unit_cost_local_var1,
                  ex_rate_var = ex_rate_var1, years_of_treat_var = years_of_treat_var1)
    unit_test(s2_in, 1.4219, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f, CEA_pe_f and RCEA_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_nx_in, 23.6070893378784, main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_yx_in, 127.0994867217, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_nx_in, 142.42587835824, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_yx_in, 766.814399527604, main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new * tax_var1,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    # ADD UNIT TEST
    # KLPS4 all and no ext
    pv_benef_all_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    unit_test(pv_benef_all_new, 950.2367, main_run_var = main_run_var1)

    #Costs
    # costs1: EA costs no externalities
    cost1_in <- costs1_f(country_w_var = costs1_country$ratios_data$country_w,
                         country_cost_var = costs1_country$ratios_data$per_cap)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)

    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, ex_rate_var = 1)
    costs2_ea_in <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in_new, cost_of_schooling_var = 0,
                         s1_var = 0, q1_var = 0, s2_var = s2_ea_in, q2_var = q_full_var1)

    # costs2: Baird no externalities
    costs2_in <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                         s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in, 11.776188118988, main_run_var = main_run_var1)

    # Baird yes externalities
    costs2_in_x <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in_x,
                           interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                           s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in_x,  25.1962130559894, main_run_var = main_run_var1)

    s2_new_in <- s2_f_new(interest_var = interest_in_new, unit_cost_local_var = unit_cost_local_new_var1, ex_rate_var = 1)
    # costs2: KLPS4
    costs_k <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in_new,
                       cost_of_schooling_var = cost_per_student_in_new,
                         s1_var = 0, q1_var = 0, s2_var = s2_new_in, q2_var = q_full_var1)
    unit_test(costs_k, 32.2996145651321, main_run_var = main_run_var1)

    return( list( "wage_0_in" = wage_0_in, "wage_t_in" = wage_t_in, "lambda1_in" = lambda1_in,
                  "lambda2_in" = lambda2_in, "saturation_in" = saturation_in,
                  "lambda1_new_in" = lambda1_new_in, "earnings_in_no_ext" = earnings_in_no_ext,
                  "earnings_in_yes_ext" = earnings_in_yes_ext,
                  "earnings_in_no_ext_new" = earnings_in_no_ext_new,
                  "interest_in" = interest_in, "costs1_counts_in" = costs1_counts_in,
                  "costs1_costs_in" = costs1_costs_in, "costs1_country" = costs1_country,
                  "delta_ed_final_in" = delta_ed_final_in, "delta_ed_final_in_x" = delta_ed_final_in_x,
                  "cost_per_student_in" = cost_per_student_in, "s2_in" = s2_in,  
                  "pv_benef_tax_nx_in"= pv_benef_tax_nx_in, "pv_benef_tax_yx_in" = pv_benef_tax_yx_in,
                  "pv_benef_all_nx_in" = pv_benef_all_nx_in,
                  "pv_benef_all_yx_in" =  pv_benef_all_yx_in, "pv_benef_tax_new" = pv_benef_tax_new,
                  "pv_benef_all_new" = pv_benef_all_new, "costs2_ea_in" = costs2_ea_in,
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k) )
  }

invisible( list2env(one_run(),.GlobalEnv) )
```

```
## [1] "Output has change at interest_in  to  0.00136568662640923"
## [1] "Output has change at interest_in  to  0.00136568662640923"
## [1] "Output has change at pv_benef_tax_nx_in  to  24.1070969615811"
## [1] "Output has change at pv_benef_tax_yx_in  to  129.79150484473"
## [1] "Output has change at pv_benef_all_nx_in  to  145.442515605316"
## [1] "Output has change at pv_benef_all_yx_in  to  783.055836167301"
## [1] "Output has change at pv_benef_all_new  to  970.890973263529"
## [1] "Output has change at costs2_in  to  11.8331988887693"
## [1] "Output has change at costs2_in_x  to  25.3328219110922"
## [1] "Output has change at costs_k  to  32.4783248798294"
```




```r
#TODO: update unit test values
#Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, 11.8309012188904)
```

```
## [1] "Output has change at baird1  to  12.2738980728118"
```

```r
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
baird2 <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
unit_test(baird2, 101.903273665711)
```

```
## [1] "Output has change at baird2  to  104.458682933638"
```

```r
# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
baird3 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test(baird3, 130.649690239252)
```

```
## [1] "Output has change at baird3  to  133.609316716546"
```

```r
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
baird4 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
unit_test(baird4, 741.618186471615)
```

```
## [1] "Output has change at baird4  to  757.723014256209"
```

```r
#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1 <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs_k)
unit_test(klps4_1, 125.202113576337)
```

```
## [1] "Output has change at klps4_1  to  128.446853938601"
```

```r
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs_k)
unit_test(klps4_2, 917.937055971637)
```

```
## [1] "Output has change at klps4_2  to  938.4126483837"
```

```r
# res_npv_no_ext_klps_eacosts <- NPV_pe_f(benefits_var = pv_benef_in_new, costs_var = cost1_in)
# unit_test(res_npv_no_ext_klps_eacosts, 59.15516)

# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
unit_test(ea1, 142.278620185973)
```

```
## [1] "Output has change at ea1  to  145.29514628382"
```

```r
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
unit_test(ea2, 766.667141355337)
```

```
## [1] "Output has change at ea2  to  782.908466845805"
```

```r
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
unit_test(ea3, 950.089412364501)
```

```
## [1] "Output has change at ea3  to  970.743603942033"
```

```r
#CEA for EA
#TODO: update CEA values.
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0)
unit_test(cea_no_ext_ea, 6452.86204429499)
```

```
## [1] "Output has change at cea_no_ext_ea  to  6588.14849257152"
```

```r
rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0),
         CEA_cash_var = 744)
unit_test(rcea_no_ext_ea, 8.6732016724395)
```

```
## [1] "Output has change at rcea_no_ext_ea  to  8.8550382964671"
```

```r
npv_table <- data.frame("no_ext" =  round( c(baird1, NA,
                                             NA), 1) ,
                        "yes_ext" = round( c(NA, baird2, NA), 1) ,
                        "no_ext_" = round( c(baird3, NA,
                                             ea1), 1) ,
                        "yes_ext_" = round( c(NA, baird4,
                                             ea2), 1) ,
                        "no_ext " = round( c(klps4_1, NA,
                                             NA), 1) ,
                        ".no_ext " = round( c(klps4_2, NA,
                                             ea3), 1) ,
                        row.names = c("no_ext", "yes_ext", "no_ext_"))

kable(npv_table, caption = "Caption of the table") %>%
  add_header_above(c(" ", "tax" = 2, "all" = 2, "tax" = 1, "all" = 1)) %>%
  add_header_above(c(" ", "Baird = EA" = 4, "KLPS4" = 2)) %>%
  add_header_above(c(" ", "Benefits" = 6)) %>%
  kable_styling("striped", full_width = F) %>%
  group_rows("Costs: Baird = KLPS4", 1, 2) %>%
  group_rows("Costs: EA", 3, 3)   # same result with group 1=4
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:main-results)Caption of the table</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Benefits</div></th>
</tr>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Baird = EA</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">KLPS4</div></th>
</tr>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">tax</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">all</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">tax</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">all</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> no_ext </th>
   <th style="text-align:right;"> yes_ext </th>
   <th style="text-align:right;"> no_ext_ </th>
   <th style="text-align:right;"> yes_ext_ </th>
   <th style="text-align:right;"> no_ext. </th>
   <th style="text-align:right;"> .no_ext. </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: Baird = KLPS4</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext </td>
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 133.6 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 128.4 </td>
   <td style="text-align:right;"> 938.4 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> yes_ext </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 104.5 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 757.7 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr grouplength="1"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: EA</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext_ </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 145.3 </td>
   <td style="text-align:right;"> 782.9 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 970.7 </td>
  </tr>
</tbody>
</table>


## Results for overall welfare (not only taxes)


- **NPV without externalities in @baird2016worms ($\lambda_2 = 0$):** 133.6    

- **NPV with externalities in @baird2016worms ($\lambda_2 = 10.2$ ):** 757.7  

- **NPV without externalities in @klps4:** 938.4   

- **NPV without externalities in EA 2019 ($\lambda_2 = 0$):** 145.3    

- **NPV with externalities in EA 2019 ($\lambda_2 = 10.2$ ):** 782.9

- **NPV without ext and benef from @klps4 and EA costs 2019 :** 970.7

- **CEA format:** 6588.1    

- **RCEA format (relative to cash):** 8.9    


# Montecarlo simulations  


```r
sim.data1 <- function(nsims = 1e2,
                      main_run_var2,
                      run_sim_var2,
                      wage_ag_var2,
                      wage_ag_var2_sd,
                      wage_ww_var2,
                      wage_ww_var2_sd,
                      profits_se_var2,
                      profits_se_var2_sd,
                      hours_se_cond_var2,
                      hours_se_cond_var2_sd,
                      hours_ag_var2,
                      hours_ag_var2_sd,
                      hours_ww_var2,
                      hours_ww_var2_sd,
                      hours_se_var2,
                      hours_se_var2_sd,
                      ex_rate_var2,
                      ex_rate_var2_sd,
                      growth_rate_var2,
                      growth_rate_var2_sd,
                      coef_exp_var2,         # sd for coef_exp is hard coded
                      lambda1_var2,
                      lambda1_var2_sd,
                      alpha_0_var2,
                      alpha_0_var2_sd,
                      alpha_r_var2,
                      alpha_r_var2_sd,
                      lambda2_var2,
                      lambda2_var2_sd,
                      coverage_var2,
                      coverage_var2_sd,
                      q_full_var2,
                      q_full_var2_sd,
                      q_zero_var2,
                      q_zero_var2_sd,
                      lambda1_new_var2,
                      lambda1_new_var2_sd,
                      gov_bonds_var2,
                      gov_bonds_var2_sd,
                      inflation_var2,
                      inflation_var2_sd,
                      gov_bonds_new_var2,                                                              
                      gov_bonds_new_var2_sd,                                                          
                      inflation_new_var2,                          
                      inflation_new_var2_sd,                      
                      costs_par_var2,
                      costs_par_var2_sd,
                      staff_time_var2,
                      staff_time_var2_sd,
                      counts_par_var2,
                      counts_par_var2_sd,
                      delta_ed_var2,
                      delta_ed_var2_sd,
                      delta_ed_ext_var2,
                      delta_ed_ext_var2_sd,
                      teach_sal_var2,
                      teach_sal_var2_sd,
                      teach_ben_var2,
                      teach_ben_var2_sd,
                      teach_sal_new_var2,
                      teach_sal_new_var2_sd,
                      teach_ben_new_var2,
                      teach_ben_new_var2_sd,
                      n_students_var2,
                      n_students_var2_sd,
                      unit_cost_local_var2,
                      unit_cost_local_var2_sd,
                      years_of_treat_var2,
                      years_of_treat_var2_sd,
                      tax_var2,
                      tax_var2_sd,
                      periods_var2,
                      df_costs_var2 = df_costs_so,
                      df_counts_var2 = df_counts_so,
                      df_costs_cw_var2 = df_costs_cw_so) {
    start_time <- Sys.time()
    ################
    ###### Draws
    ################
    set.seed(142857)
    #draw_vals <- function(all_the_sos){}
    #Defaoult dist: normal, default sd: 0.1* mean
    ## Data
    gov_bonds_sim <-        rnorm(n = nsims, mean = gov_bonds_var2, sd = gov_bonds_var2_sd)
    inflation_sim <-        rnorm(nsims, inflation_var2, inflation_var2_sd)

    gov_bonds_new_sim <-    rnorm(n = nsims, mean = gov_bonds_new_var2, sd = gov_bonds_new_var2_sd)
    inflation_new_sim <-    rnorm(nsims, inflation_new_var2, inflation_new_var2_sd)                  

    wage_ag_sim <-          rnorm(nsims, wage_ag_var2, wage_ag_var2_sd)
    wage_ww_sim <-          rnorm(nsims, wage_ww_var2, wage_ww_var2_sd)
    profits_se_sim <-       rnorm(nsims, profits_se_var2, profits_se_var2_sd)
    hours_se_cond_sim <-    rnorm(nsims, hours_se_cond_var2, hours_se_cond_var2_sd)
    hours_ag_sim <-         rnorm(nsims, hours_ag_var2, hours_ag_var2_sd)
    hours_ww_sim <-         rnorm(nsims, hours_ww_var2, hours_ww_var2_sd)
    hours_se_sim <-         rnorm(nsims, hours_se_var2, hours_se_var2_sd)
    coverage_sim <-         rnorm(nsims, coverage_var2, coverage_var2_sd)
    growth_rate_sim <-      rnorm(nsims, growth_rate_var2, growth_rate_var2_sd)

    ex_rate_sim <-          rnorm(nsims, ex_rate_var2, ex_rate_var2_sd)
    # ex_rate_new_sim
    tax_sim <-              rnorm(nsims, tax_var2, tax_var2_sd)

    unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_var2, unit_cost_local_var2_sd)
    # unit_cost_local_new_sim
    years_of_treat_sim <-   rnorm(nsims, years_of_treat_var2, years_of_treat_var2_sd)

    ## Research
    aux1 <- 0.1 * c(lambda1_var2[1], 0.01)
    # Each list is a pair mean, sd.
    aux2 <- lapply(1:2,function(x) c(lambda1_var2[x], aux1[x] ) )
    lambda1_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )
    lambda2_sim <-          rnorm(nsims, lambda2_var2,  lambda2_var2_sd)
    # New lambdas here
    aux3 <- lapply(1:3,function(x) c(lambda1_new_var2[x], lambda1_new_var2_sd[x] ) )
    lambda1_new_sim <- sapply(aux3, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )

    q_full_sim <-           rnorm(nsims, q_full_var2, q_full_var2_sd)
    q_zero_sim <-           rnorm(nsims, q_zero_var2, q_zero_var2_sd)

    # Prevalence here TO DO: draw from a beta instead of "truncated" normal
    alpha_0_sim <- rnorm(nsims, alpha_0_var2, alpha_0_var2_sd)
    alpha_0_sim <- ifelse(alpha_0_sim > 1, yes = 1, ifelse(alpha_0_sim < 0, 0, alpha_0_sim) )

    alpha_r_sim <- rnorm(nsims, alpha_r_var2, alpha_r_var2_sd)
    alpha_r_sim <- ifelse(alpha_r_sim > 1, yes = 1, ifelse(alpha_r_sim < 0, 0, alpha_r_sim) )
    ## Guess work
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #Time from initial period until individual join the labor force
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims, teach_sal_var2, teach_sal_var2_sd)
    teach_ben_sim <-    rnorm(nsims, teach_ben_var2, teach_ben_var2_sd)

    teach_sal_new_sim <-    rnorm(nsims, teach_sal_new_var2, teach_sal_new_var2_sd)
    teach_ben_new_sim <-    rnorm(nsims, teach_ben_new_var2, teach_ben_new_var2_sd)

    n_students_sim <-   rnorm(nsims, n_students_var2, n_students_var2_sd)

    delta_ed_sim <- sapply(delta_ed_so[,1], function(x) rnorm(nsims, mean =
                                                                x * delta_ed_var2,
                                                              sd = delta_ed_var2_sd * sd(delta_ed_so[,1]) ) )
    colnames(delta_ed_sim) <- 1999:2007

    delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1],
                               function(x)  rnorm(nsims,
                                                  mean = x * delta_ed_ext_var2,
                                                  sd = delta_ed_ext_var2_sd * sd(delta_ed_ext_so[, 1])))
    # OLD (WORKING)
    #delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1], function(x) rnorm(nsims,
    #                                                                   mean = x * 1,
    #                                                                   sd = 1 * sd(delta_ed_ext_so[,1])))

    colnames(delta_ed_ext_sim) <- 1999:2007
    #######    
    #######    
    costs1_counts_in <- costs1_counts_f(df_counts_var = df_counts_var2,
                                        df_costs_cw_var = df_costs_cw_var2)$counts_data
    costs1_counts_sim <- sapply(costs1_counts_in$total,
                                function(x)  rnorm(nsims, mean = x * counts_par_var2,  
                                                   sd = counts_par_var2_sd * x) )

    staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      

    costs1_costs_in <- lapply(staff_time_sim,
                              function(x) costs1_costs_f(df_costs_var = df_costs_var2,
                                                         df_costs_cw_var = df_costs_cw_var2,
                                                         staff_time_var = x)$cost_data)

    costs1_costs_sim <- t( sapply(costs1_costs_in, function(x)  {
        aux1 <- x$costs_by_country
        rnorm(length(aux1), mean = costs_par_var2 * aux1,  sd = costs_par_var2_sd * aux1)
    } )
    )

    ################
    ###### Runs    
    ################

    baird1_sim           <- rep(NA, nsims)
    baird2_sim           <- rep(NA, nsims)
    baird3_sim           <- rep(NA, nsims)
    baird4_sim           <- rep(NA, nsims)
    klps4_1_sim          <- rep(NA, nsims)
    klps4_2_sim          <- rep(NA, nsims)
    ea1_sim              <- rep(NA, nsims)
    ea2_sim              <- rep(NA, nsims)
    ea3_sim              <- rep(NA, nsims)
    cea_no_ext_ea_sim    <- rep(NA, nsims)
    rcea_no_ext_ea_sim   <- rep(NA, nsims)

    for (i in 1:nsims) {
    # one_run, for the most part, does not include standard deviations   
     # change to j?       
      invisible( list2env(
        one_run(main_run_var1 = FALSE,              # HERE I NEED TO PLUG costs1_costs_sim
                run_sim_var = TRUE,
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
                alpha_0_var1 = alpha_0_sim[i],
                alpha_r_var1 = alpha_r_sim[i],
                lambda2_var1 = lambda2_sim[i],
                coverage_var1 = coverage_sim[i],
                q_full_var1 = q_full_sim[i],
                q_zero_var1 = q_zero_sim[i],
                lambda1_new_var1 = lambda1_new_sim[i,],
                gov_bonds_var1 = gov_bonds_sim[i],
                inflation_var1 = inflation_sim[i],
                gov_bonds_new_var1 = gov_bonds_new_sim[i],
                inflation_new_var1 = inflation_new_sim[i],
                staff_time_var1 = staff_time_sim[i],
                delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                teach_sal_var1 = teach_sal_sim[i],
                teach_ben_var1 = teach_ben_sim[i],
                teach_sal_new_var1 = teach_sal_new_sim[i],
                teach_ben_new_var1 = teach_ben_new_sim[i],
                n_students_var1 = n_students_sim[i],
                unit_cost_local_var1 = unit_cost_local_sim[i],
                years_of_treat_var1 = years_of_treat_sim[i],
                tax_var1 = tax_sim[i],
                periods_var1 = periods_so,
                df_costs_var1 = df_costs_var2,
                df_costs_cw_var1 = df_costs_cw_var2,
                df_counts_var1 = df_counts_var2,
                counts_sim_var1 = costs1_counts_sim[i,],
                costs_sim_var1 = costs1_costs_sim[i,]),.GlobalEnv) ) # add costs here
      #Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
      baird1_sim[i] <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
      #Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
      baird2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
      # Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
      baird3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
      # Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
      baird4_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
      #KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
      klps4_1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs2_in)
      #KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
      klps4_2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_in)
      # EA1: no externality NPV using EAs costs
      ea1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
      # EA2: yes externality NPV using EAs costs
      ea2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      ea3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
      #CEA for EA
      cea_no_ext_ea_sim[i]  <- CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                        costs_var = costs2_ea_in, fudging_var = 0)
      rcea_no_ext_ea_sim[i]  <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                                              costs_var = costs2_ea_in, fudging_var = 0),
                                           CEA_cash_var = 744 )
    }

    total_time <- Sys.time() - start_time

    return( list(
      "baird1_sim"        = baird1_sim,         
      "baird2_sim"        = baird2_sim,         
      "baird3_sim"        = baird3_sim,         
      "baird4_sim"        = baird4_sim,         
      "klps4_1_sim"        = klps4_1_sim,        
      "klps4_2_sim"        = klps4_2_sim,        
      "ea1_sim"            = ea1_sim,            
      "ea2_sim"            = ea2_sim,            
      "ea3_sim"            = ea3_sim,            
      "cea_no_ext_ea_sim"  = cea_no_ext_ea_sim,  
      "rcea_no_ext_ea_sim" = rcea_no_ext_ea_sim,
      "total_time"         = total_time
    ) )
}

policy_estimates <- c("baird1_sim",          
"baird2_sim"         ,
"baird3_sim"         ,
"baird4_sim"         ,
"klps4_1_sim"        ,
"klps4_2_sim"        ,
"ea1_sim"            ,
"ea2_sim"            ,
"ea3_sim"            ,
"cea_no_ext_ea_sim"  ,
"rcea_no_ext_ea_sim" )

policy_estimates_text <- c(
  "Fiscal effects, 2016(W@W) B & C, no ext",
  "Fiscal effects, 2016(W@W) B & C, yes ext",  
  "Total effects, 2016(W@W) B & C, no ext",  
  "Total effects, 2016(W@W) B & C, yes ext",  
  "Fiscal effects, 2019(KLPS4) B & 2016(W@W) C, no ext",   
  "Total effects, 2019(KLPS4) B & 2016(W@W) C, no ext",  
  "Total effects, 2016(W@W) B & EA C, no ext",  
  "Total effects, 2016(W@W) B & EA C, ext",  
  "Total effects, 2019(KLPS4) B & EA C, no ext",
  "CEA for total effects, 2019(KLPS4) B & EA C, no ext",
  "RCEA to cash for total effects, 2019(KLPS4) B & EA C, no ext")
```



```r
npv_sim_all <-   sim.data1(nsims = nsims_so,                        
            gov_bonds_var2          = gov_bonds_so             ,                                    
            gov_bonds_var2_sd       = gov_bonds_so * 0.1          ,                                 
            inflation_var2          = inflation_so             ,                                    
            inflation_var2_sd       = inflation_so * 0.1          ,                                 
            gov_bonds_new_var2      = gov_bonds_new_so      ,          #add to app
            gov_bonds_new_var2_sd   = gov_bonds_new_so * 0.1,          #add to app
            inflation_new_var2      = inflation_new_so      ,          #add to app
            inflation_new_var2_sd   = inflation_new_so * 0.1,          #add to app
            wage_ag_var2            = wage_ag_so               ,                                      
            wage_ag_var2_sd         = wage_ag_so * 0.1            ,                                   
            wage_ww_var2            = wage_ww_so               ,                                      
            wage_ww_var2_sd         = wage_ww_so * 0.1            ,                                   
            profits_se_var2         = profits_se_so            ,                                   
            profits_se_var2_sd      = profits_se_so * 0.1         ,                                
            hours_se_cond_var2      = hours_se_cond_so         ,                                
            hours_se_cond_var2_sd   = hours_se_cond_so * 0.1      ,                             
            hours_ag_var2           = hours_ag_so              ,                                     
            hours_ag_var2_sd        = hours_ag_so * 0.1           ,                                  
            hours_ww_var2           = hours_ww_so              ,                                     
            hours_ww_var2_sd        = hours_ww_so * 0.1           ,                                  
            hours_se_var2           = hours_se_so              ,                                     
            hours_se_var2_sd        = hours_se_so * 0.1           ,                                  
            ex_rate_var2            = ex_rate_so               ,                                      
            ex_rate_var2_sd         = ex_rate_so * 0.1            ,                                   
            growth_rate_var2        = growth_rate_so           ,                                  
            growth_rate_var2_sd     = growth_rate_so * 0.1        ,
            coverage_var2           = coverage_so              ,
            coverage_var2_sd        = coverage_so * 0.1           ,  
            tax_var2                = tax_so                   ,                                             
            tax_var2_sd             = tax_so * 0.1                ,                                        
            unit_cost_local_var2    = unit_cost_local_so       ,                                     
            unit_cost_local_var2_sd = unit_cost_local_so * 0.1    ,                                       
            years_of_treat_var2     = years_of_treat_so        ,                                    
            years_of_treat_var2_sd  = years_of_treat_so * 0.1     ,                                      
            lambda1_var2            = lambda1_so,                                          
            lambda1_var2_sd         = rep(lambda1_so[1], 2) * 0.1 ,                                          
            lambda2_var2            = lambda2_so        ,                         
            lambda2_var2_sd         = lambda2_so * 0.1  ,                      
            q_full_var2             = q_full_so         ,                          
            q_full_var2_sd          = q_full_so * 0.1   ,                         
            coef_exp_var2           = coef_exp_so,                      
            #                      coef_exp_var2_sd = c(as.numeric(input$param21_1_1), as.numeric(input$param21_2_1)),                       
            teach_sal_var2          = teach_sal_so         ,                                          
            teach_sal_var2_sd       = teach_sal_so * 0.1      ,                                       
            teach_ben_var2          = teach_ben_so         ,                                          
            teach_ben_var2_sd       = teach_ben_so * 0.1      ,                                       
            teach_sal_new_var2      = teach_sal_new_so         ,          #add to app                                
            teach_sal_new_var2_sd   = teach_sal_new_so * 0.1      ,       #add to app                                
            teach_ben_new_var2      = teach_ben_new_so         ,          #add to app                               
            teach_ben_new_var2_sd   = 0.000001      ,                     #add to app
            n_students_var2         = n_students_so        ,                                         
            n_students_var2_sd      = n_students_so * 0.1     ,                                      
            delta_ed_var2           = delta_ed_par_so          ,                                           
            delta_ed_var2_sd        = delta_ed_par_so * 0.1       ,                                            
            delta_ed_ext_var2       = delta_ed_ext_par_so      ,                                           
            delta_ed_ext_var2_sd    = delta_ed_ext_par_so * 0.1   ,                                              
            q_zero_var2             = q_zero_so            ,                                             
            q_zero_var2_sd          = q_zero_so * 0.1         ,
            lambda1_new_var2        = lambda1_new_so,                   
            lambda1_new_var2_sd     = lambda1_new_sd_so,             
            alpha_0_var2            = alpha_0_so       ,  
            alpha_0_var2_sd         = alpha_0_so * 0.1    ,
            alpha_r_var2            = alpha_r_so       ,  
            alpha_r_var2_sd         = alpha_r_so * 0.1    ,                                                                         
            staff_time_var2         = staff_time_so    ,
            staff_time_var2_sd      = staff_time_so * 0.1,
            counts_par_var2         = counts_par_so    ,
            counts_par_var2_sd      = counts_par_sd_so ,
            costs_par_var2          = costs_par_so     ,
            costs_par_var2_sd       = costs_par_sd_so)


total_time <- npv_sim_all$total_time
position <- which( policy_estimates == policy_estimate_so )
npv_sim <- npv_sim_all[[ policy_estimates[position] ]]    
npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))
#Unit test the simulations for nsims = 100, 1000, 10000
all_res_100_sims <- c(
  8.7094550833253,
  49.2632644048296,
  49.4694221681446,
  277.265576489817,
  175.678254409271,
  1022.37359604602,
  49.9034982533997,
  278.49341938017,
  1022.26807845879,
  413.199179925478,
  0.555375241835319
)

all_res_1000_sims <- c(
  8.94904200012673,
  47.7557079511762,
  52.2490551593106,
  277.810659510651,
  176.29084181961,
  1051.32849483112,
  52.6663916192209,
  279.015364618585,
  1051.32339750587,
  407.842773941034,
  0.548175771426122
)

all_res_10000_sims <- c(
  8.79105935822713,
  47.6994033603414,
  52.2778377879947,
  279.54416693675,
  181.921984084524,
  1085.22816114431,
  52.7479120047233,
  280.883186390767,
  1085.20816160347,
  406.388379235971,
  0.546220939833294
)

if (FALSE) {
k <- 0
for ( i in policy_estimates ) {
    k <- k + 1
    to_test <- npv_sim_all[[i]]
    if (nsims_so == 1e4){
        unit_test(to_test, all_res_10000_sims[k], main_run_var = TRUE)
    } else if (nsims_so == 1e3){
        unit_test(to_test, all_res_1000_sims[k], main_run_var = TRUE)
    } else if (nsims_so == 1e2){
        unit_test(to_test, all_res_100_sims[k], main_run_var = TRUE)
    }
}
}
################
###### Results/Viz
################
library(plotly)
nsims <- nsims_so

npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))

rescale <- rescale_so

    plot1 <- ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2, ..scaled..), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-10, 400)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = "Distribution of Economic of Effects From Deworming (NPV)",
           subtitle = paste0("N = ", nsims, " simulations. Takes ",
                             round(total_time, 1)," ",attributes(total_time)$unit )  )+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.25, label = npv_for_text, size = 4)+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.10, label = npv_for_text2, size = 4)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    if (rescale == TRUE) {
      plot1 <- suppressMessages(
        plot1 +
          coord_cartesian(xlim = 1.2 * c( min( c(-1, npv_sim) ),
                                          max(npv_sim))
                          )
        )
    }
ggplotly(plot1)
```

<!--html_preserve--><div id="htmlwidget-28bb2bbe25e68bf301fe" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-28bb2bbe25e68bf301fe">{"x":{"data":[{"x":[-1273.68334895426,-1267.4724083239,-1261.26146769353,-1255.05052706316,-1248.8395864328,-1242.62864580243,-1236.41770517206,-1230.2067645417,-1223.99582391133,-1217.78488328097,-1211.5739426506,-1205.36300202023,-1199.15206138987,-1192.9411207595,-1186.73018012914,-1180.51923949877,-1174.3082988684,-1168.09735823804,-1161.88641760767,-1155.6754769773,-1149.46453634694,-1143.25359571657,-1137.04265508621,-1130.83171445584,-1124.62077382547,-1118.40983319511,-1112.19889256474,-1105.98795193438,-1099.77701130401,-1093.56607067364,-1087.35513004328,-1081.14418941291,-1074.93324878254,-1068.72230815218,-1062.51136752181,-1056.30042689145,-1050.08948626108,-1043.87854563071,-1037.66760500035,-1031.45666436998,-1025.24572373961,-1019.03478310925,-1012.82384247888,-1006.61290184852,-1000.40196121815,-994.191020587784,-987.980079957418,-981.769139327052,-975.558198696686,-969.347258066319,-963.136317435953,-956.925376805587,-950.714436175221,-944.503495544855,-938.292554914489,-932.081614284122,-925.870673653756,-919.65973302339,-913.448792393024,-907.237851762658,-901.026911132292,-894.815970501925,-888.605029871559,-882.394089241193,-876.183148610827,-869.972207980461,-863.761267350095,-857.550326719728,-851.339386089362,-845.128445458996,-838.91750482863,-832.706564198264,-826.495623567898,-820.284682937531,-814.073742307165,-807.862801676799,-801.651861046433,-795.440920416067,-789.229979785701,-783.019039155334,-776.808098524968,-770.597157894602,-764.386217264236,-758.17527663387,-751.964336003503,-745.753395373137,-739.542454742771,-733.331514112405,-727.120573482039,-720.909632851673,-714.698692221306,-708.48775159094,-702.276810960574,-696.065870330208,-689.854929699842,-683.643989069476,-677.433048439109,-671.222107808743,-665.011167178377,-658.800226548011,-652.589285917645,-646.378345287279,-640.167404656912,-633.956464026546,-627.74552339618,-621.534582765814,-615.323642135448,-609.112701505082,-602.901760874715,-596.690820244349,-590.479879613983,-584.268938983617,-578.057998353251,-571.847057722885,-565.636117092518,-559.425176462152,-553.214235831786,-547.00329520142,-540.792354571054,-534.581413940688,-528.370473310321,-522.159532679955,-515.948592049589,-509.737651419223,-503.526710788857,-497.315770158491,-491.104829528124,-484.893888897758,-478.682948267392,-472.472007637026,-466.26106700666,-460.050126376294,-453.839185745927,-447.628245115561,-441.417304485195,-435.206363854829,-428.995423224463,-422.784482594097,-416.57354196373,-410.362601333364,-404.151660702998,-397.940720072632,-391.729779442266,-385.5188388119,-379.307898181533,-373.096957551167,-366.886016920801,-360.675076290435,-354.464135660069,-348.253195029702,-342.042254399336,-335.83131376897,-329.620373138604,-323.409432508238,-317.198491877872,-310.987551247505,-304.776610617139,-298.565669986773,-292.354729356407,-286.143788726041,-279.932848095675,-273.721907465308,-267.510966834942,-261.300026204576,-255.08908557421,-248.878144943844,-242.667204313477,-236.456263683111,-230.245323052745,-224.034382422379,-217.823441792013,-211.612501161647,-205.40156053128,-199.190619900914,-192.979679270548,-186.768738640182,-180.557798009816,-174.34685737945,-168.135916749083,-161.924976118717,-155.714035488351,-149.503094857985,-143.292154227619,-137.081213597253,-130.870272966886,-124.65933233652,-118.448391706154,-112.237451075788,-106.026510445422,-99.8155698150556,-93.6046291846894,-87.3936885543233,-81.1827479239571,-74.9718072935909,-68.7608666632248,-62.5499260328586,-56.3389854024924,-50.1280447721263,-43.9171041417601,-37.7061635113939,-31.4952228810278,-25.2842822506616,-19.0733416202954,-12.8624009899293,-6.65146035956309,-0.440519729196922,5.77042090116925,11.9813615315354,18.1923021619016,24.4032427922677,30.6141834226339,36.8251240530001,43.0360646833662,49.2470053137324,55.4579459440986,61.6688865744648,67.8798272048309,74.0907678351971,80.3017084655633,86.5126490959294,92.7235897262956,98.9345303566618,105.145470987028,111.356411617394,117.56735224776,123.778292878127,129.989233508493,136.200174138859,142.411114769225,148.622055399591,154.832996029957,161.043936660324,167.25487729069,173.465817921056,179.676758551422,185.887699181788,192.098639812154,198.309580442521,204.520521072887,210.731461703253,216.942402333619,223.153342963985,229.364283594351,235.575224224718,241.786164855084,247.99710548545,254.208046115816,260.418986746182,266.629927376548,272.840868006915,279.051808637281,285.262749267647,291.473689898013,297.684630528379,303.895571158745,310.106511789112,316.317452419478,322.528393049844,328.73933368021,334.950274310576,341.161214940943,347.372155571309,353.583096201675,359.794036832041,366.004977462407,372.215918092773,378.42685872314,384.637799353506,390.848739983872,397.059680614238,403.270621244604,409.48156187497,415.692502505337,421.903443135703,428.114383766069,434.325324396435,440.536265026801,446.747205657167,452.958146287534,459.1690869179,465.380027548266,471.590968178632,477.801908808998,484.012849439364,490.223790069731,496.434730700097,502.645671330463,508.856611960829,515.067552591195,521.278493221562,527.489433851928,533.700374482294,539.91131511266,546.122255743026,552.333196373392,558.544137003759,564.755077634125,570.966018264491,577.176958894857,583.387899525223,589.598840155589,595.809780785956,602.020721416322,608.231662046688,614.442602677054,620.65354330742,626.864483937786,633.075424568153,639.286365198519,645.497305828885,651.708246459251,657.919187089617,664.130127719983,670.34106835035,676.552008980716,682.762949611082,688.973890241448,695.184830871814,701.39577150218,707.606712132547,713.817652762913,720.028593393279,726.239534023645,732.450474654011,738.661415284377,744.872355914744,751.08329654511,757.294237175476,763.505177805842,769.716118436208,775.927059066575,782.137999696941,788.348940327307,794.559880957673,800.770821588039,806.981762218405,813.192702848772,819.403643479138,825.614584109504,831.82552473987,838.036465370236,844.247406000602,850.458346630969,856.669287261335,862.880227891701,869.091168522067,875.302109152433,881.513049782799,887.723990413166,893.934931043532,900.145871673898,906.356812304264,912.56775293463,918.778693564997,924.989634195363,931.200574825729,937.411515456095,943.622456086461,949.833396716827,956.044337347194,962.25527797756,968.466218607926,974.677159238292,980.888099868658,987.099040499024,993.309981129391,999.520921759757,1005.73186239012,1011.94280302049,1018.15374365086,1024.36468428122,1030.57562491159,1036.78656554195,1042.99750617232,1049.20844680269,1055.41938743305,1061.63032806342,1067.84126869378,1074.05220932415,1080.26314995452,1086.47409058488,1092.68503121525,1098.89597184562,1105.10691247598,1111.31785310635,1117.52879373671,1123.73973436708,1129.95067499745,1136.16161562781,1142.37255625818,1148.58349688854,1154.79443751891,1161.00537814928,1167.21631877964,1173.42725941001,1179.63820004038,1185.84914067074,1192.06008130111,1198.27102193147,1204.48196256184,1210.69290319221,1216.90384382257,1223.11478445294,1229.3257250833,1235.53666571367,1241.74760634404,1247.9585469744,1254.16948760477,1260.38042823514,1266.5913688655,1272.80230949587,1279.01325012623,1285.2241907566,1291.43513138697,1297.64607201733,1303.8570126477,1310.06795327807,1316.27889390843,1322.4898345388,1328.70077516916,1334.91171579953,1341.1226564299,1347.33359706026,1353.54453769063,1359.75547832099,1365.96641895136,1372.17735958173,1378.38830021209,1384.59924084246,1390.81018147283,1397.02112210319,1403.23206273356,1409.44300336392,1415.65394399429,1421.86488462466,1428.07582525502,1434.28676588539,1440.49770651575,1446.70864714612,1452.91958777649,1459.13052840685,1465.34146903722,1471.55240966759,1477.76335029795,1483.97429092832,1490.18523155868,1496.39617218905,1502.60711281942,1508.81805344978,1515.02899408015,1521.23993471052,1527.45087534088,1533.66181597125,1539.87275660161,1546.08369723198,1552.29463786235,1558.50557849271,1564.71651912308,1570.92745975344,1577.13840038381,1583.34934101418,1589.56028164454,1595.77122227491,1601.98216290528,1608.19310353564,1614.40404416601,1620.61498479637,1626.82592542674,1633.03686605711,1639.24780668747,1645.45874731784,1651.6696879482,1657.88062857857,1664.09156920894,1670.3025098393,1676.51345046967,1682.72439110004,1688.9353317304,1695.14627236077,1701.35721299113,1707.5681536215,1713.77909425187,1719.99003488223,1726.2009755126,1732.41191614296,1738.62285677333,1744.8337974037,1751.04473803406,1757.25567866443,1763.4666192948,1769.67755992516,1775.88850055553,1782.09944118589,1788.31038181626,1794.52132244663,1800.73226307699,1806.94320370736,1813.15414433773,1819.36508496809,1825.57602559846,1831.78696622882,1837.99790685919,1844.20884748956,1850.41978811992,1856.63072875029,1862.84166938065,1869.05261001102,1875.26355064139,1881.47449127175,1887.68543190212,1893.89637253249,1900.10731316285,1900.10731316285,1900.10731316285,1893.89637253249,1887.68543190212,1881.47449127175,1875.26355064139,1869.05261001102,1862.84166938065,1856.63072875029,1850.41978811992,1844.20884748956,1837.99790685919,1831.78696622882,1825.57602559846,1819.36508496809,1813.15414433773,1806.94320370736,1800.73226307699,1794.52132244663,1788.31038181626,1782.09944118589,1775.88850055553,1769.67755992516,1763.4666192948,1757.25567866443,1751.04473803406,1744.8337974037,1738.62285677333,1732.41191614296,1726.2009755126,1719.99003488223,1713.77909425187,1707.5681536215,1701.35721299113,1695.14627236077,1688.9353317304,1682.72439110004,1676.51345046967,1670.3025098393,1664.09156920894,1657.88062857857,1651.6696879482,1645.45874731784,1639.24780668747,1633.03686605711,1626.82592542674,1620.61498479637,1614.40404416601,1608.19310353564,1601.98216290528,1595.77122227491,1589.56028164454,1583.34934101418,1577.13840038381,1570.92745975344,1564.71651912308,1558.50557849271,1552.29463786235,1546.08369723198,1539.87275660161,1533.66181597125,1527.45087534088,1521.23993471052,1515.02899408015,1508.81805344978,1502.60711281942,1496.39617218905,1490.18523155868,1483.97429092832,1477.76335029795,1471.55240966759,1465.34146903722,1459.13052840685,1452.91958777649,1446.70864714612,1440.49770651575,1434.28676588539,1428.07582525502,1421.86488462466,1415.65394399429,1409.44300336392,1403.23206273356,1397.02112210319,1390.81018147283,1384.59924084246,1378.38830021209,1372.17735958173,1365.96641895136,1359.75547832099,1353.54453769063,1347.33359706026,1341.1226564299,1334.91171579953,1328.70077516916,1322.4898345388,1316.27889390843,1310.06795327807,1303.8570126477,1297.64607201733,1291.43513138697,1285.2241907566,1279.01325012623,1272.80230949587,1266.5913688655,1260.38042823514,1254.16948760477,1247.9585469744,1241.74760634404,1235.53666571367,1229.3257250833,1223.11478445294,1216.90384382257,1210.69290319221,1204.48196256184,1198.27102193147,1192.06008130111,1185.84914067074,1179.63820004038,1173.42725941001,1167.21631877964,1161.00537814928,1154.79443751891,1148.58349688854,1142.37255625818,1136.16161562781,1129.95067499745,1123.73973436708,1117.52879373671,1111.31785310635,1105.10691247598,1098.89597184562,1092.68503121525,1086.47409058488,1080.26314995452,1074.05220932415,1067.84126869378,1061.63032806342,1055.41938743305,1049.20844680269,1042.99750617232,1036.78656554195,1030.57562491159,1024.36468428122,1018.15374365086,1011.94280302049,1005.73186239012,999.520921759757,993.309981129391,987.099040499024,980.888099868658,974.677159238292,968.466218607926,962.25527797756,956.044337347194,949.833396716827,943.622456086461,937.411515456095,931.200574825729,924.989634195363,918.778693564997,912.56775293463,906.356812304264,900.145871673898,893.934931043532,887.723990413166,881.513049782799,875.302109152433,869.091168522067,862.880227891701,856.669287261335,850.458346630969,844.247406000602,838.036465370236,831.82552473987,825.614584109504,819.403643479138,813.192702848772,806.981762218405,800.770821588039,794.559880957673,788.348940327307,782.137999696941,775.927059066575,769.716118436208,763.505177805842,757.294237175476,751.08329654511,744.872355914744,738.661415284377,732.450474654011,726.239534023645,720.028593393279,713.817652762913,707.606712132547,701.39577150218,695.184830871814,688.973890241448,682.762949611082,676.552008980716,670.34106835035,664.130127719983,657.919187089617,651.708246459251,645.497305828885,639.286365198519,633.075424568153,626.864483937786,620.65354330742,614.442602677054,608.231662046688,602.020721416322,595.809780785956,589.598840155589,583.387899525223,577.176958894857,570.966018264491,564.755077634125,558.544137003759,552.333196373392,546.122255743026,539.91131511266,533.700374482294,527.489433851928,521.278493221562,515.067552591195,508.856611960829,502.645671330463,496.434730700097,490.223790069731,484.012849439364,477.801908808998,471.590968178632,465.380027548266,459.1690869179,452.958146287534,446.747205657167,440.536265026801,434.325324396435,428.114383766069,421.903443135703,415.692502505337,409.48156187497,403.270621244604,397.059680614238,390.848739983872,384.637799353506,378.42685872314,372.215918092773,366.004977462407,359.794036832041,353.583096201675,347.372155571309,341.161214940943,334.950274310576,328.73933368021,322.528393049844,316.317452419478,310.106511789112,303.895571158745,297.684630528379,291.473689898013,285.262749267647,279.051808637281,272.840868006915,266.629927376548,260.418986746182,254.208046115816,247.99710548545,241.786164855084,235.575224224718,229.364283594351,223.153342963985,216.942402333619,210.731461703253,204.520521072887,198.309580442521,192.098639812154,185.887699181788,179.676758551422,173.465817921056,167.25487729069,161.043936660324,154.832996029957,148.622055399591,142.411114769225,136.200174138859,129.989233508493,123.778292878127,117.56735224776,111.356411617394,105.145470987028,98.9345303566618,92.7235897262956,86.5126490959294,80.3017084655633,74.0907678351971,67.8798272048309,61.6688865744648,55.4579459440986,49.2470053137324,43.0360646833662,36.8251240530001,30.6141834226339,24.4032427922677,18.1923021619016,11.9813615315354,5.77042090116925,-0.440519729196922,-6.65146035956309,-12.8624009899293,-19.0733416202954,-25.2842822506616,-31.4952228810278,-37.7061635113939,-43.9171041417601,-50.1280447721263,-56.3389854024924,-62.5499260328586,-68.7608666632248,-74.9718072935909,-81.1827479239571,-87.3936885543233,-93.6046291846894,-99.8155698150556,-106.026510445422,-112.237451075788,-118.448391706154,-124.65933233652,-130.870272966886,-137.081213597253,-143.292154227619,-149.503094857985,-155.714035488351,-161.924976118717,-168.135916749083,-174.34685737945,-180.557798009816,-186.768738640182,-192.979679270548,-199.190619900914,-205.40156053128,-211.612501161647,-217.823441792013,-224.034382422379,-230.245323052745,-236.456263683111,-242.667204313477,-248.878144943844,-255.08908557421,-261.300026204576,-267.510966834942,-273.721907465308,-279.932848095675,-286.143788726041,-292.354729356407,-298.565669986773,-304.776610617139,-310.987551247505,-317.198491877872,-323.409432508238,-329.620373138604,-335.83131376897,-342.042254399336,-348.253195029702,-354.464135660069,-360.675076290435,-366.886016920801,-373.096957551167,-379.307898181533,-385.5188388119,-391.729779442266,-397.940720072632,-404.151660702998,-410.362601333364,-416.57354196373,-422.784482594097,-428.995423224463,-435.206363854829,-441.417304485195,-447.628245115561,-453.839185745927,-460.050126376294,-466.26106700666,-472.472007637026,-478.682948267392,-484.893888897758,-491.104829528124,-497.315770158491,-503.526710788857,-509.737651419223,-515.948592049589,-522.159532679955,-528.370473310321,-534.581413940688,-540.792354571054,-547.00329520142,-553.214235831786,-559.425176462152,-565.636117092518,-571.847057722885,-578.057998353251,-584.268938983617,-590.479879613983,-596.690820244349,-602.901760874715,-609.112701505082,-615.323642135448,-621.534582765814,-627.74552339618,-633.956464026546,-640.167404656912,-646.378345287279,-652.589285917645,-658.800226548011,-665.011167178377,-671.222107808743,-677.433048439109,-683.643989069476,-689.854929699842,-696.065870330208,-702.276810960574,-708.48775159094,-714.698692221306,-720.909632851673,-727.120573482039,-733.331514112405,-739.542454742771,-745.753395373137,-751.964336003503,-758.17527663387,-764.386217264236,-770.597157894602,-776.808098524968,-783.019039155334,-789.229979785701,-795.440920416067,-801.651861046433,-807.862801676799,-814.073742307165,-820.284682937531,-826.495623567898,-832.706564198264,-838.91750482863,-845.128445458996,-851.339386089362,-857.550326719728,-863.761267350095,-869.972207980461,-876.183148610827,-882.394089241193,-888.605029871559,-894.815970501925,-901.026911132292,-907.237851762658,-913.448792393024,-919.65973302339,-925.870673653756,-932.081614284122,-938.292554914489,-944.503495544855,-950.714436175221,-956.925376805587,-963.136317435953,-969.347258066319,-975.558198696686,-981.769139327052,-987.980079957418,-994.191020587784,-1000.40196121815,-1006.61290184852,-1012.82384247888,-1019.03478310925,-1025.24572373961,-1031.45666436998,-1037.66760500035,-1043.87854563071,-1050.08948626108,-1056.30042689145,-1062.51136752181,-1068.72230815218,-1074.93324878254,-1081.14418941291,-1087.35513004328,-1093.56607067364,-1099.77701130401,-1105.98795193438,-1112.19889256474,-1118.40983319511,-1124.62077382547,-1130.83171445584,-1137.04265508621,-1143.25359571657,-1149.46453634694,-1155.6754769773,-1161.88641760767,-1168.09735823804,-1174.3082988684,-1180.51923949877,-1186.73018012914,-1192.9411207595,-1199.15206138987,-1205.36300202023,-1211.5739426506,-1217.78488328097,-1223.99582391133,-1230.2067645417,-1236.41770517206,-1242.62864580243,-1248.8395864328,-1255.05052706316,-1261.26146769353,-1267.4724083239,-1273.68334895426,-1273.68334895426],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.62416765071897,0.627788722755852,0.631392837749635,0.634941307379216,0.638489777008797,0.641968767157401,0.645443111822346,0.648869093700956,0.652268000900666,0.65564051237788,0.658962880596768,0.662281483025419,0.665526421739568,0.668771360453718,0.671957528952501,0.675124357723499,0.678255339505091,0.68134358617545,0.684419221701595,0.687428619711742,0.690438017721889,0.693379257662602,0.696309742514794,0.699195521437371,0.702047226337082,0.704877650758438,0.707650901468735,0.710424152179031,0.713121408434915,0.715816717384002,0.718459590726482,0.721077650412973,0.723666489174719,0.726208164919199,0.72874333599139,0.731209658086478,0.733675980181565,0.73608370379942,0.73847585911376,0.740832045559142,0.743151368653541,0.74545658915781,0.747704552974078,0.749952516790345,0.75213760131023,0.754315807495133,0.756452839889624,0.758563008817728,0.760652723727472,0.762696684287515,0.764739828299522,0.766719507509333,0.768699186719145,0.770634253622488,0.772551666137307,0.774443790731432,0.776301028309842,0.778151071322278,0.779950292312824,0.78174951330337,0.783502542589387,0.785245961499309,0.786960920522654,0.788650797728072,0.790328616095412,0.791967247749751,0.793605879404089,0.795198567843828,0.796788276038282,0.798348036983673,0.799891160210154,0.801418949184158,0.802917833147409,0.804414606032586,0.805871594854085,0.807328583675584,0.808755737265387,0.810173165117276,0.811573537641028,0.81295372084369,0.814328252203792,0.815673481815848,0.817018711427904,0.818335644954865,0.819648179879352,0.820943367827396,0.822225428452748,0.823499763612223,0.824753526001458,0.826007288390693,0.827235486987361,0.828463077623637,0.8296742675787,0.83087775867016,0.83207276203726,0.833254167378927,0.83443379939392,0.835595070779337,0.836756342164754,0.8379031645824,0.839046188752815,0.840181070952127,0.841307667066321,0.842431336542799,0.84354325414619,0.844655171749581,0.845755348265247,0.846854265732742,0.847946267928725,0.84903379135193,0.850118251095098,0.851195913581011,0.852273454609893,0.853342715955093,0.854411977300294,0.855476200546883,0.856538447248545,0.857598288100534,0.858654833663351,0.859710818313661,0.860762903809981,0.861814989306301,0.86286434509739,0.863913139936557,0.864960765792967,0.866007368655056,0.86705369581187,0.868099135700689,0.869144575589509,0.870189824941311,0.871235062311718,0.872280744811969,0.873326672727255,0.87437312925373,0.875420574529988,0.876468138643578,0.877517862939637,0.878567587235696,0.879619561519301,0.880672262332927,0.881726622927469,0.88278293446111,0.883839929596828,0.884900423459166,0.885960917321505,0.887025480044869,0.888090665757821,0.889158786101797,0.890229111386349,0.891300954845349,0.892376805663658,0.893452656481967,0.89453423119825,0.895615931533813,0.896701801383352,0.897789612745599,0.898879859883318,0.899973980527775,0.901068679972097,0.902169243832189,0.90326980769228,0.904375539053441,0.905482614385013,0.906592928784247,0.907706516528963,0.90882140042719,0.909941432306011,0.911061464184831,0.912187201017846,0.913313537370797,0.914443581183643,0.915576008576987,0.916710248525116,0.917848477151747,0.918986793844785,0.920130454748194,0.921274115651604,0.922421362959446,0.923570005115873,0.924720530896581,0.925873618193883,0.927027192718404,0.928184100873803,0.929341009029202,0.930500498583873,0.931660512045905,0.932821753684554,0.933984062567333,0.935146702989473,0.936310400089666,0.937474097189859,0.938638153274995,0.939802231223697,0.940965837751648,0.942129186354633,0.943291854031215,0.944453257851741,0.945614485554365,0.946772621792886,0.947930758031408,0.949085333602356,0.950238770327088,0.951389326794953,0.952536521576212,0.95368241193808,0.954821710937786,0.955961009937492,0.957091913409074,0.958221550974622,0.959344579810084,0.960462678619088,0.961577037065827,0.962681608864992,0.963786180664156,0.96487543577361,0.965964382751165,0.967041133049493,0.968112249876376,0.96917555597532,0.970226532545515,0.971275443711826,0.972303868607096,0.973332293502366,0.974340786668232,0.975344151366912,0.976333709903356,0.977309413746968,0.978278960257679,0.979224316182926,0.980169672108174,0.981085004084908,0.981997245122173,0.982887538494067,0.983763825026919,0.984627902234896,0.985465330008369,0.986302006455682,0.987097615314296,0.98789322417291,0.988656480884457,0.989407264214138,0.990137679332113,0.990840594173438,0.991536924268795,0.992188902062373,0.992840879855951,0.993447856883051,0.994045814801428,0.994613118151517,0.99515397096581,0.995680345684727,0.996161018108346,0.996641690531965,0.997062649084847,0.997480088531132,0.997854590470165,0.998205780155711,0.998532613334394,0.998814585655223,0.999092544156349,0.999302394259993,0.999512244363638,0.999665175331097,0.999800074804139,0.999898989531576,0.999956200119257,1,0.999976887272222,0.999953774544445,0.999858530383906,0.999752577260317,0.999597661625115,0.999406481671013,0.99919096196534,0.99891231236989,0.99863366277444,0.998267065906956,0.997898860055643,0.99746792069849,0.997008240252922,0.99651224664794,0.995959353047959,0.99539761445938,0.994749959765019,0.994102305070659,0.993378041618588,0.992634278612647,0.991841807539655,0.991000799015563,0.990139701564807,0.989200528728985,0.988261355893162,0.987232379694927,0.986194349617967,0.98509551880209,0.983958170858757,0.982789372798451,0.981552484042696,0.980313632650372,0.97897722207819,0.977640811506008,0.976232588995792,0.974796920655823,0.973319061977289,0.971784646896461,0.970237392766705,0.968604989665906,0.966972586565107,0.965259223239053,0.963529838021764,0.961748895645219,0.959923779650104,0.958075826944145,0.956156473960809,0.954237120977472,0.952230248915192,0.950218390970505,0.948147693701913,0.946045295632505,0.943911653892265,0.941720906726877,0.939525225979793,0.937248539163069,0.934971852346346,0.93263174380643,0.930271736303455,0.927874299729052,0.925433790057315,0.922980205913194,0.920462201155728,0.917944196398263,0.91536135627849,0.912769040138601,0.910135830987189,0.907472550951366,0.904790379032552,0.902059632713902,0.899328886395253,0.896535341975037,0.893740762714724,0.890904875765456,0.88805021757424,0.885173559099395,0.882262681025366,0.879346832187108,0.876383682199065,0.873420532211022,0.870418835856149,0.867407434334796,0.864373828781607,0.861318251712786,0.858254406044059,0.855158768009622,0.852063129975185,0.848934794204204,0.845803231266828,0.842652154633577,0.83948880721186,0.836316673062175,0.833125668891727,0.829934158797711,0.826719596072657,0.823505033347603,0.820276323662676,0.817042254450101,0.81380152834885,0.81055194236619,0.807300815434377,0.804039624276666,0.800778433118955,0.797510727795725,0.794241749695977,0.790970568880353,0.787697514082207,0.784424340075579,0.781150796892736,0.777877253709893,0.774606520744353,0.771335942376273,0.768069450043902,0.764805142211828,0.761544181222982,0.758289290684603,0.75503513343161,0.751792637426628,0.748550141421647,0.745319236376581,0.74209193304919,0.73887293844777,0.735663438285578,0.732457384729106,0.729268103051475,0.726078821373844,0.722909150749311,0.719742301271367,0.716589577650536,0.713447167142437,0.710312148854766,0.707195972909086,0.704079796963407,0.700991034737245,0.697902674650722,0.694834429263055,0.691775250032376,0.688727992394575,0.685699141819539,0.682673318686788,0.679675727567933,0.676678136449078,0.67370614470006,0.670740528111899,0.667791292073097,0.664858151688354,0.661931832085849,0.659031459520686,0.656131086955524,0.653260674463606,0.650393155608684,0.647545793996814,0.644711014294264,0.641886594814149,0.6390842454361,0.636282637879366,0.633512222759662,0.630741807639958,0.627994117605844,0.625254961272891,0.622528905636612,0.619820161792077,0.617115373919808,0.614436034705042,0.611756695490277,0.609101034270201,0.606449940159837,0.60381345186693,0.601189302127076,0.598571424642809,0.595972788289689,0.593374151936569,0.59079827161747,0.58822359881113,0.585663500680083,0.583111134487025,0.580566109342473,0.578034297800649,0.575503626613101,0.572990535032677,0.570477443452252,0.567977210063746,0.565480933497399,0.562991614587389,0.560510190404576,0.558030978365783,0.555562398866435,0.553093819367087,0.550634704518254,0.548176929522698,0.545724224150179,0.543275193503705,0.540828055858258,0.538385701790664,0.53594334772307,0.53350554840356,0.531067807686148,0.5286318406326,0.526196666477507,0.523761708614579,0.521327082263906,0.518892314743401,0.516456256698417,0.514020198653433,0.511581443746732,0.509142024590373,0.506699958280359,0.504255309049356,0.501809184427656,0.499357506493864,0.496905828560072,0.494446158097255,0.491985732521084,0.489518899019775,0.487048095342064,0.484573465384519,0.482090749826678,0.479607700945707,0.477111643991488,0.474615587037268,0.472108846778222,0.469598130114061,0.467080553424818,0.464553976219266,0.462025090491375,0.459481574997716,0.456938059504056,0.454379517006639,0.451818113476336,0.449246537340907,0.446666428239078,0.444081518894813,0.441482022412354,0.438882525929896,0.436264068119456,0.433644640979894,0.431011892813546,0.428372132375995,0.425724980740761,0.423064626403634,0.420402974041005,0.417721907954244,0.415040841867484,0.412343920584106,0.409642167680767,0],"text":["npv_sim: -1273.6833490<br />scaled: 0.4096422<br />1/2: 0.5","npv_sim: -1267.4724083<br />scaled: 0.4123439<br />1/2: 0.5","npv_sim: -1261.2614677<br />scaled: 0.4150408<br />1/2: 0.5","npv_sim: -1255.0505271<br />scaled: 0.4177219<br />1/2: 0.5","npv_sim: -1248.8395864<br />scaled: 0.4204030<br />1/2: 0.5","npv_sim: -1242.6286458<br />scaled: 0.4230646<br />1/2: 0.5","npv_sim: -1236.4177052<br />scaled: 0.4257250<br />1/2: 0.5","npv_sim: -1230.2067645<br />scaled: 0.4283721<br />1/2: 0.5","npv_sim: -1223.9958239<br />scaled: 0.4310119<br />1/2: 0.5","npv_sim: -1217.7848833<br />scaled: 0.4336446<br />1/2: 0.5","npv_sim: -1211.5739427<br />scaled: 0.4362641<br />1/2: 0.5","npv_sim: -1205.3630020<br />scaled: 0.4388825<br />1/2: 0.5","npv_sim: -1199.1520614<br />scaled: 0.4414820<br />1/2: 0.5","npv_sim: -1192.9411208<br />scaled: 0.4440815<br />1/2: 0.5","npv_sim: -1186.7301801<br />scaled: 0.4466664<br />1/2: 0.5","npv_sim: -1180.5192395<br />scaled: 0.4492465<br />1/2: 0.5","npv_sim: -1174.3082989<br />scaled: 0.4518181<br />1/2: 0.5","npv_sim: -1168.0973582<br />scaled: 0.4543795<br />1/2: 0.5","npv_sim: -1161.8864176<br />scaled: 0.4569381<br />1/2: 0.5","npv_sim: -1155.6754770<br />scaled: 0.4594816<br />1/2: 0.5","npv_sim: -1149.4645363<br />scaled: 0.4620251<br />1/2: 0.5","npv_sim: -1143.2535957<br />scaled: 0.4645540<br />1/2: 0.5","npv_sim: -1137.0426551<br />scaled: 0.4670806<br />1/2: 0.5","npv_sim: -1130.8317145<br />scaled: 0.4695981<br />1/2: 0.5","npv_sim: -1124.6207738<br />scaled: 0.4721088<br />1/2: 0.5","npv_sim: -1118.4098332<br />scaled: 0.4746156<br />1/2: 0.5","npv_sim: -1112.1988926<br />scaled: 0.4771116<br />1/2: 0.5","npv_sim: -1105.9879519<br />scaled: 0.4796077<br />1/2: 0.5","npv_sim: -1099.7770113<br />scaled: 0.4820907<br />1/2: 0.5","npv_sim: -1093.5660707<br />scaled: 0.4845735<br />1/2: 0.5","npv_sim: -1087.3551300<br />scaled: 0.4870481<br />1/2: 0.5","npv_sim: -1081.1441894<br />scaled: 0.4895189<br />1/2: 0.5","npv_sim: -1074.9332488<br />scaled: 0.4919857<br />1/2: 0.5","npv_sim: -1068.7223082<br />scaled: 0.4944462<br />1/2: 0.5","npv_sim: -1062.5113675<br />scaled: 0.4969058<br />1/2: 0.5","npv_sim: -1056.3004269<br />scaled: 0.4993575<br />1/2: 0.5","npv_sim: -1050.0894863<br />scaled: 0.5018092<br />1/2: 0.5","npv_sim: -1043.8785456<br />scaled: 0.5042553<br />1/2: 0.5","npv_sim: -1037.6676050<br />scaled: 0.5067000<br />1/2: 0.5","npv_sim: -1031.4566644<br />scaled: 0.5091420<br />1/2: 0.5","npv_sim: -1025.2457237<br />scaled: 0.5115814<br />1/2: 0.5","npv_sim: -1019.0347831<br />scaled: 0.5140202<br />1/2: 0.5","npv_sim: -1012.8238425<br />scaled: 0.5164563<br />1/2: 0.5","npv_sim: -1006.6129018<br />scaled: 0.5188923<br />1/2: 0.5","npv_sim: -1000.4019612<br />scaled: 0.5213271<br />1/2: 0.5","npv_sim:  -994.1910206<br />scaled: 0.5237617<br />1/2: 0.5","npv_sim:  -987.9800800<br />scaled: 0.5261967<br />1/2: 0.5","npv_sim:  -981.7691393<br />scaled: 0.5286318<br />1/2: 0.5","npv_sim:  -975.5581987<br />scaled: 0.5310678<br />1/2: 0.5","npv_sim:  -969.3472581<br />scaled: 0.5335055<br />1/2: 0.5","npv_sim:  -963.1363174<br />scaled: 0.5359433<br />1/2: 0.5","npv_sim:  -956.9253768<br />scaled: 0.5383857<br />1/2: 0.5","npv_sim:  -950.7144362<br />scaled: 0.5408281<br />1/2: 0.5","npv_sim:  -944.5034955<br />scaled: 0.5432752<br />1/2: 0.5","npv_sim:  -938.2925549<br />scaled: 0.5457242<br />1/2: 0.5","npv_sim:  -932.0816143<br />scaled: 0.5481769<br />1/2: 0.5","npv_sim:  -925.8706737<br />scaled: 0.5506347<br />1/2: 0.5","npv_sim:  -919.6597330<br />scaled: 0.5530938<br />1/2: 0.5","npv_sim:  -913.4487924<br />scaled: 0.5555624<br />1/2: 0.5","npv_sim:  -907.2378518<br />scaled: 0.5580310<br />1/2: 0.5","npv_sim:  -901.0269111<br />scaled: 0.5605102<br />1/2: 0.5","npv_sim:  -894.8159705<br />scaled: 0.5629916<br />1/2: 0.5","npv_sim:  -888.6050299<br />scaled: 0.5654809<br />1/2: 0.5","npv_sim:  -882.3940892<br />scaled: 0.5679772<br />1/2: 0.5","npv_sim:  -876.1831486<br />scaled: 0.5704774<br />1/2: 0.5","npv_sim:  -869.9722080<br />scaled: 0.5729905<br />1/2: 0.5","npv_sim:  -863.7612674<br />scaled: 0.5755036<br />1/2: 0.5","npv_sim:  -857.5503267<br />scaled: 0.5780343<br />1/2: 0.5","npv_sim:  -851.3393861<br />scaled: 0.5805661<br />1/2: 0.5","npv_sim:  -845.1284455<br />scaled: 0.5831111<br />1/2: 0.5","npv_sim:  -838.9175048<br />scaled: 0.5856635<br />1/2: 0.5","npv_sim:  -832.7065642<br />scaled: 0.5882236<br />1/2: 0.5","npv_sim:  -826.4956236<br />scaled: 0.5907983<br />1/2: 0.5","npv_sim:  -820.2846829<br />scaled: 0.5933742<br />1/2: 0.5","npv_sim:  -814.0737423<br />scaled: 0.5959728<br />1/2: 0.5","npv_sim:  -807.8628017<br />scaled: 0.5985714<br />1/2: 0.5","npv_sim:  -801.6518610<br />scaled: 0.6011893<br />1/2: 0.5","npv_sim:  -795.4409204<br />scaled: 0.6038135<br />1/2: 0.5","npv_sim:  -789.2299798<br />scaled: 0.6064499<br />1/2: 0.5","npv_sim:  -783.0190392<br />scaled: 0.6091010<br />1/2: 0.5","npv_sim:  -776.8080985<br />scaled: 0.6117567<br />1/2: 0.5","npv_sim:  -770.5971579<br />scaled: 0.6144360<br />1/2: 0.5","npv_sim:  -764.3862173<br />scaled: 0.6171154<br />1/2: 0.5","npv_sim:  -758.1752766<br />scaled: 0.6198202<br />1/2: 0.5","npv_sim:  -751.9643360<br />scaled: 0.6225289<br />1/2: 0.5","npv_sim:  -745.7533954<br />scaled: 0.6252550<br />1/2: 0.5","npv_sim:  -739.5424547<br />scaled: 0.6279941<br />1/2: 0.5","npv_sim:  -733.3315141<br />scaled: 0.6307418<br />1/2: 0.5","npv_sim:  -727.1205735<br />scaled: 0.6335122<br />1/2: 0.5","npv_sim:  -720.9096329<br />scaled: 0.6362826<br />1/2: 0.5","npv_sim:  -714.6986922<br />scaled: 0.6390842<br />1/2: 0.5","npv_sim:  -708.4877516<br />scaled: 0.6418866<br />1/2: 0.5","npv_sim:  -702.2768110<br />scaled: 0.6447110<br />1/2: 0.5","npv_sim:  -696.0658703<br />scaled: 0.6475458<br />1/2: 0.5","npv_sim:  -689.8549297<br />scaled: 0.6503932<br />1/2: 0.5","npv_sim:  -683.6439891<br />scaled: 0.6532607<br />1/2: 0.5","npv_sim:  -677.4330484<br />scaled: 0.6561311<br />1/2: 0.5","npv_sim:  -671.2221078<br />scaled: 0.6590315<br />1/2: 0.5","npv_sim:  -665.0111672<br />scaled: 0.6619318<br />1/2: 0.5","npv_sim:  -658.8002265<br />scaled: 0.6648582<br />1/2: 0.5","npv_sim:  -652.5892859<br />scaled: 0.6677913<br />1/2: 0.5","npv_sim:  -646.3783453<br />scaled: 0.6707405<br />1/2: 0.5","npv_sim:  -640.1674047<br />scaled: 0.6737061<br />1/2: 0.5","npv_sim:  -633.9564640<br />scaled: 0.6766781<br />1/2: 0.5","npv_sim:  -627.7455234<br />scaled: 0.6796757<br />1/2: 0.5","npv_sim:  -621.5345828<br />scaled: 0.6826733<br />1/2: 0.5","npv_sim:  -615.3236421<br />scaled: 0.6856991<br />1/2: 0.5","npv_sim:  -609.1127015<br />scaled: 0.6887280<br />1/2: 0.5","npv_sim:  -602.9017609<br />scaled: 0.6917753<br />1/2: 0.5","npv_sim:  -596.6908202<br />scaled: 0.6948344<br />1/2: 0.5","npv_sim:  -590.4798796<br />scaled: 0.6979027<br />1/2: 0.5","npv_sim:  -584.2689390<br />scaled: 0.7009910<br />1/2: 0.5","npv_sim:  -578.0579984<br />scaled: 0.7040798<br />1/2: 0.5","npv_sim:  -571.8470577<br />scaled: 0.7071960<br />1/2: 0.5","npv_sim:  -565.6361171<br />scaled: 0.7103121<br />1/2: 0.5","npv_sim:  -559.4251765<br />scaled: 0.7134472<br />1/2: 0.5","npv_sim:  -553.2142358<br />scaled: 0.7165896<br />1/2: 0.5","npv_sim:  -547.0032952<br />scaled: 0.7197423<br />1/2: 0.5","npv_sim:  -540.7923546<br />scaled: 0.7229092<br />1/2: 0.5","npv_sim:  -534.5814139<br />scaled: 0.7260788<br />1/2: 0.5","npv_sim:  -528.3704733<br />scaled: 0.7292681<br />1/2: 0.5","npv_sim:  -522.1595327<br />scaled: 0.7324574<br />1/2: 0.5","npv_sim:  -515.9485920<br />scaled: 0.7356634<br />1/2: 0.5","npv_sim:  -509.7376514<br />scaled: 0.7388729<br />1/2: 0.5","npv_sim:  -503.5267108<br />scaled: 0.7420919<br />1/2: 0.5","npv_sim:  -497.3157702<br />scaled: 0.7453192<br />1/2: 0.5","npv_sim:  -491.1048295<br />scaled: 0.7485501<br />1/2: 0.5","npv_sim:  -484.8938889<br />scaled: 0.7517926<br />1/2: 0.5","npv_sim:  -478.6829483<br />scaled: 0.7550351<br />1/2: 0.5","npv_sim:  -472.4720076<br />scaled: 0.7582893<br />1/2: 0.5","npv_sim:  -466.2610670<br />scaled: 0.7615442<br />1/2: 0.5","npv_sim:  -460.0501264<br />scaled: 0.7648051<br />1/2: 0.5","npv_sim:  -453.8391857<br />scaled: 0.7680695<br />1/2: 0.5","npv_sim:  -447.6282451<br />scaled: 0.7713359<br />1/2: 0.5","npv_sim:  -441.4173045<br />scaled: 0.7746065<br />1/2: 0.5","npv_sim:  -435.2063639<br />scaled: 0.7778773<br />1/2: 0.5","npv_sim:  -428.9954232<br />scaled: 0.7811508<br />1/2: 0.5","npv_sim:  -422.7844826<br />scaled: 0.7844243<br />1/2: 0.5","npv_sim:  -416.5735420<br />scaled: 0.7876975<br />1/2: 0.5","npv_sim:  -410.3626013<br />scaled: 0.7909706<br />1/2: 0.5","npv_sim:  -404.1516607<br />scaled: 0.7942417<br />1/2: 0.5","npv_sim:  -397.9407201<br />scaled: 0.7975107<br />1/2: 0.5","npv_sim:  -391.7297794<br />scaled: 0.8007784<br />1/2: 0.5","npv_sim:  -385.5188388<br />scaled: 0.8040396<br />1/2: 0.5","npv_sim:  -379.3078982<br />scaled: 0.8073008<br />1/2: 0.5","npv_sim:  -373.0969576<br />scaled: 0.8105519<br />1/2: 0.5","npv_sim:  -366.8860169<br />scaled: 0.8138015<br />1/2: 0.5","npv_sim:  -360.6750763<br />scaled: 0.8170423<br />1/2: 0.5","npv_sim:  -354.4641357<br />scaled: 0.8202763<br />1/2: 0.5","npv_sim:  -348.2531950<br />scaled: 0.8235050<br />1/2: 0.5","npv_sim:  -342.0422544<br />scaled: 0.8267196<br />1/2: 0.5","npv_sim:  -335.8313138<br />scaled: 0.8299342<br />1/2: 0.5","npv_sim:  -329.6203731<br />scaled: 0.8331257<br />1/2: 0.5","npv_sim:  -323.4094325<br />scaled: 0.8363167<br />1/2: 0.5","npv_sim:  -317.1984919<br />scaled: 0.8394888<br />1/2: 0.5","npv_sim:  -310.9875512<br />scaled: 0.8426522<br />1/2: 0.5","npv_sim:  -304.7766106<br />scaled: 0.8458032<br />1/2: 0.5","npv_sim:  -298.5656700<br />scaled: 0.8489348<br />1/2: 0.5","npv_sim:  -292.3547294<br />scaled: 0.8520631<br />1/2: 0.5","npv_sim:  -286.1437887<br />scaled: 0.8551588<br />1/2: 0.5","npv_sim:  -279.9328481<br />scaled: 0.8582544<br />1/2: 0.5","npv_sim:  -273.7219075<br />scaled: 0.8613183<br />1/2: 0.5","npv_sim:  -267.5109668<br />scaled: 0.8643738<br />1/2: 0.5","npv_sim:  -261.3000262<br />scaled: 0.8674074<br />1/2: 0.5","npv_sim:  -255.0890856<br />scaled: 0.8704188<br />1/2: 0.5","npv_sim:  -248.8781449<br />scaled: 0.8734205<br />1/2: 0.5","npv_sim:  -242.6672043<br />scaled: 0.8763837<br />1/2: 0.5","npv_sim:  -236.4562637<br />scaled: 0.8793468<br />1/2: 0.5","npv_sim:  -230.2453231<br />scaled: 0.8822627<br />1/2: 0.5","npv_sim:  -224.0343824<br />scaled: 0.8851736<br />1/2: 0.5","npv_sim:  -217.8234418<br />scaled: 0.8880502<br />1/2: 0.5","npv_sim:  -211.6125012<br />scaled: 0.8909049<br />1/2: 0.5","npv_sim:  -205.4015605<br />scaled: 0.8937408<br />1/2: 0.5","npv_sim:  -199.1906199<br />scaled: 0.8965353<br />1/2: 0.5","npv_sim:  -192.9796793<br />scaled: 0.8993289<br />1/2: 0.5","npv_sim:  -186.7687386<br />scaled: 0.9020596<br />1/2: 0.5","npv_sim:  -180.5577980<br />scaled: 0.9047904<br />1/2: 0.5","npv_sim:  -174.3468574<br />scaled: 0.9074726<br />1/2: 0.5","npv_sim:  -168.1359167<br />scaled: 0.9101358<br />1/2: 0.5","npv_sim:  -161.9249761<br />scaled: 0.9127690<br />1/2: 0.5","npv_sim:  -155.7140355<br />scaled: 0.9153614<br />1/2: 0.5","npv_sim:  -149.5030949<br />scaled: 0.9179442<br />1/2: 0.5","npv_sim:  -143.2921542<br />scaled: 0.9204622<br />1/2: 0.5","npv_sim:  -137.0812136<br />scaled: 0.9229802<br />1/2: 0.5","npv_sim:  -130.8702730<br />scaled: 0.9254338<br />1/2: 0.5","npv_sim:  -124.6593323<br />scaled: 0.9278743<br />1/2: 0.5","npv_sim:  -118.4483917<br />scaled: 0.9302717<br />1/2: 0.5","npv_sim:  -112.2374511<br />scaled: 0.9326317<br />1/2: 0.5","npv_sim:  -106.0265104<br />scaled: 0.9349719<br />1/2: 0.5","npv_sim:   -99.8155698<br />scaled: 0.9372485<br />1/2: 0.5","npv_sim:   -93.6046292<br />scaled: 0.9395252<br />1/2: 0.5","npv_sim:   -87.3936886<br />scaled: 0.9417209<br />1/2: 0.5","npv_sim:   -81.1827479<br />scaled: 0.9439117<br />1/2: 0.5","npv_sim:   -74.9718073<br />scaled: 0.9460453<br />1/2: 0.5","npv_sim:   -68.7608667<br />scaled: 0.9481477<br />1/2: 0.5","npv_sim:   -62.5499260<br />scaled: 0.9502184<br />1/2: 0.5","npv_sim:   -56.3389854<br />scaled: 0.9522302<br />1/2: 0.5","npv_sim:   -50.1280448<br />scaled: 0.9542371<br />1/2: 0.5","npv_sim:   -43.9171041<br />scaled: 0.9561565<br />1/2: 0.5","npv_sim:   -37.7061635<br />scaled: 0.9580758<br />1/2: 0.5","npv_sim:   -31.4952229<br />scaled: 0.9599238<br />1/2: 0.5","npv_sim:   -25.2842823<br />scaled: 0.9617489<br />1/2: 0.5","npv_sim:   -19.0733416<br />scaled: 0.9635298<br />1/2: 0.5","npv_sim:   -12.8624010<br />scaled: 0.9652592<br />1/2: 0.5","npv_sim:    -6.6514604<br />scaled: 0.9669726<br />1/2: 0.5","npv_sim:    -0.4405197<br />scaled: 0.9686050<br />1/2: 0.5","npv_sim:     5.7704209<br />scaled: 0.9702374<br />1/2: 0.5","npv_sim:    11.9813615<br />scaled: 0.9717846<br />1/2: 0.5","npv_sim:    18.1923022<br />scaled: 0.9733191<br />1/2: 0.5","npv_sim:    24.4032428<br />scaled: 0.9747969<br />1/2: 0.5","npv_sim:    30.6141834<br />scaled: 0.9762326<br />1/2: 0.5","npv_sim:    36.8251241<br />scaled: 0.9776408<br />1/2: 0.5","npv_sim:    43.0360647<br />scaled: 0.9789772<br />1/2: 0.5","npv_sim:    49.2470053<br />scaled: 0.9803136<br />1/2: 0.5","npv_sim:    55.4579459<br />scaled: 0.9815525<br />1/2: 0.5","npv_sim:    61.6688866<br />scaled: 0.9827894<br />1/2: 0.5","npv_sim:    67.8798272<br />scaled: 0.9839582<br />1/2: 0.5","npv_sim:    74.0907678<br />scaled: 0.9850955<br />1/2: 0.5","npv_sim:    80.3017085<br />scaled: 0.9861943<br />1/2: 0.5","npv_sim:    86.5126491<br />scaled: 0.9872324<br />1/2: 0.5","npv_sim:    92.7235897<br />scaled: 0.9882614<br />1/2: 0.5","npv_sim:    98.9345304<br />scaled: 0.9892005<br />1/2: 0.5","npv_sim:   105.1454710<br />scaled: 0.9901397<br />1/2: 0.5","npv_sim:   111.3564116<br />scaled: 0.9910008<br />1/2: 0.5","npv_sim:   117.5673522<br />scaled: 0.9918418<br />1/2: 0.5","npv_sim:   123.7782929<br />scaled: 0.9926343<br />1/2: 0.5","npv_sim:   129.9892335<br />scaled: 0.9933780<br />1/2: 0.5","npv_sim:   136.2001741<br />scaled: 0.9941023<br />1/2: 0.5","npv_sim:   142.4111148<br />scaled: 0.9947500<br />1/2: 0.5","npv_sim:   148.6220554<br />scaled: 0.9953976<br />1/2: 0.5","npv_sim:   154.8329960<br />scaled: 0.9959594<br />1/2: 0.5","npv_sim:   161.0439367<br />scaled: 0.9965122<br />1/2: 0.5","npv_sim:   167.2548773<br />scaled: 0.9970082<br />1/2: 0.5","npv_sim:   173.4658179<br />scaled: 0.9974679<br />1/2: 0.5","npv_sim:   179.6767586<br />scaled: 0.9978989<br />1/2: 0.5","npv_sim:   185.8876992<br />scaled: 0.9982671<br />1/2: 0.5","npv_sim:   192.0986398<br />scaled: 0.9986337<br />1/2: 0.5","npv_sim:   198.3095804<br />scaled: 0.9989123<br />1/2: 0.5","npv_sim:   204.5205211<br />scaled: 0.9991910<br />1/2: 0.5","npv_sim:   210.7314617<br />scaled: 0.9994065<br />1/2: 0.5","npv_sim:   216.9424023<br />scaled: 0.9995977<br />1/2: 0.5","npv_sim:   223.1533430<br />scaled: 0.9997526<br />1/2: 0.5","npv_sim:   229.3642836<br />scaled: 0.9998585<br />1/2: 0.5","npv_sim:   235.5752242<br />scaled: 0.9999538<br />1/2: 0.5","npv_sim:   241.7861649<br />scaled: 0.9999769<br />1/2: 0.5","npv_sim:   247.9971055<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   254.2080461<br />scaled: 0.9999562<br />1/2: 0.5","npv_sim:   260.4189867<br />scaled: 0.9998990<br />1/2: 0.5","npv_sim:   266.6299274<br />scaled: 0.9998001<br />1/2: 0.5","npv_sim:   272.8408680<br />scaled: 0.9996652<br />1/2: 0.5","npv_sim:   279.0518086<br />scaled: 0.9995122<br />1/2: 0.5","npv_sim:   285.2627493<br />scaled: 0.9993024<br />1/2: 0.5","npv_sim:   291.4736899<br />scaled: 0.9990925<br />1/2: 0.5","npv_sim:   297.6846305<br />scaled: 0.9988146<br />1/2: 0.5","npv_sim:   303.8955712<br />scaled: 0.9985326<br />1/2: 0.5","npv_sim:   310.1065118<br />scaled: 0.9982058<br />1/2: 0.5","npv_sim:   316.3174524<br />scaled: 0.9978546<br />1/2: 0.5","npv_sim:   322.5283930<br />scaled: 0.9974801<br />1/2: 0.5","npv_sim:   328.7393337<br />scaled: 0.9970626<br />1/2: 0.5","npv_sim:   334.9502743<br />scaled: 0.9966417<br />1/2: 0.5","npv_sim:   341.1612149<br />scaled: 0.9961610<br />1/2: 0.5","npv_sim:   347.3721556<br />scaled: 0.9956803<br />1/2: 0.5","npv_sim:   353.5830962<br />scaled: 0.9951540<br />1/2: 0.5","npv_sim:   359.7940368<br />scaled: 0.9946131<br />1/2: 0.5","npv_sim:   366.0049775<br />scaled: 0.9940458<br />1/2: 0.5","npv_sim:   372.2159181<br />scaled: 0.9934479<br />1/2: 0.5","npv_sim:   378.4268587<br />scaled: 0.9928409<br />1/2: 0.5","npv_sim:   384.6377994<br />scaled: 0.9921889<br />1/2: 0.5","npv_sim:   390.8487400<br />scaled: 0.9915369<br />1/2: 0.5","npv_sim:   397.0596806<br />scaled: 0.9908406<br />1/2: 0.5","npv_sim:   403.2706212<br />scaled: 0.9901377<br />1/2: 0.5","npv_sim:   409.4815619<br />scaled: 0.9894073<br />1/2: 0.5","npv_sim:   415.6925025<br />scaled: 0.9886565<br />1/2: 0.5","npv_sim:   421.9034431<br />scaled: 0.9878932<br />1/2: 0.5","npv_sim:   428.1143838<br />scaled: 0.9870976<br />1/2: 0.5","npv_sim:   434.3253244<br />scaled: 0.9863020<br />1/2: 0.5","npv_sim:   440.5362650<br />scaled: 0.9854653<br />1/2: 0.5","npv_sim:   446.7472057<br />scaled: 0.9846279<br />1/2: 0.5","npv_sim:   452.9581463<br />scaled: 0.9837638<br />1/2: 0.5","npv_sim:   459.1690869<br />scaled: 0.9828875<br />1/2: 0.5","npv_sim:   465.3800275<br />scaled: 0.9819972<br />1/2: 0.5","npv_sim:   471.5909682<br />scaled: 0.9810850<br />1/2: 0.5","npv_sim:   477.8019088<br />scaled: 0.9801697<br />1/2: 0.5","npv_sim:   484.0128494<br />scaled: 0.9792243<br />1/2: 0.5","npv_sim:   490.2237901<br />scaled: 0.9782790<br />1/2: 0.5","npv_sim:   496.4347307<br />scaled: 0.9773094<br />1/2: 0.5","npv_sim:   502.6456713<br />scaled: 0.9763337<br />1/2: 0.5","npv_sim:   508.8566120<br />scaled: 0.9753442<br />1/2: 0.5","npv_sim:   515.0675526<br />scaled: 0.9743408<br />1/2: 0.5","npv_sim:   521.2784932<br />scaled: 0.9733323<br />1/2: 0.5","npv_sim:   527.4894339<br />scaled: 0.9723039<br />1/2: 0.5","npv_sim:   533.7003745<br />scaled: 0.9712754<br />1/2: 0.5","npv_sim:   539.9113151<br />scaled: 0.9702265<br />1/2: 0.5","npv_sim:   546.1222557<br />scaled: 0.9691756<br />1/2: 0.5","npv_sim:   552.3331964<br />scaled: 0.9681122<br />1/2: 0.5","npv_sim:   558.5441370<br />scaled: 0.9670411<br />1/2: 0.5","npv_sim:   564.7550776<br />scaled: 0.9659644<br />1/2: 0.5","npv_sim:   570.9660183<br />scaled: 0.9648754<br />1/2: 0.5","npv_sim:   577.1769589<br />scaled: 0.9637862<br />1/2: 0.5","npv_sim:   583.3878995<br />scaled: 0.9626816<br />1/2: 0.5","npv_sim:   589.5988402<br />scaled: 0.9615770<br />1/2: 0.5","npv_sim:   595.8097808<br />scaled: 0.9604627<br />1/2: 0.5","npv_sim:   602.0207214<br />scaled: 0.9593446<br />1/2: 0.5","npv_sim:   608.2316620<br />scaled: 0.9582216<br />1/2: 0.5","npv_sim:   614.4426027<br />scaled: 0.9570919<br />1/2: 0.5","npv_sim:   620.6535433<br />scaled: 0.9559610<br />1/2: 0.5","npv_sim:   626.8644839<br />scaled: 0.9548217<br />1/2: 0.5","npv_sim:   633.0754246<br />scaled: 0.9536824<br />1/2: 0.5","npv_sim:   639.2863652<br />scaled: 0.9525365<br />1/2: 0.5","npv_sim:   645.4973058<br />scaled: 0.9513893<br />1/2: 0.5","npv_sim:   651.7082465<br />scaled: 0.9502388<br />1/2: 0.5","npv_sim:   657.9191871<br />scaled: 0.9490853<br />1/2: 0.5","npv_sim:   664.1301277<br />scaled: 0.9479308<br />1/2: 0.5","npv_sim:   670.3410684<br />scaled: 0.9467726<br />1/2: 0.5","npv_sim:   676.5520090<br />scaled: 0.9456145<br />1/2: 0.5","npv_sim:   682.7629496<br />scaled: 0.9444533<br />1/2: 0.5","npv_sim:   688.9738902<br />scaled: 0.9432919<br />1/2: 0.5","npv_sim:   695.1848309<br />scaled: 0.9421292<br />1/2: 0.5","npv_sim:   701.3957715<br />scaled: 0.9409658<br />1/2: 0.5","npv_sim:   707.6067121<br />scaled: 0.9398022<br />1/2: 0.5","npv_sim:   713.8176528<br />scaled: 0.9386382<br />1/2: 0.5","npv_sim:   720.0285934<br />scaled: 0.9374741<br />1/2: 0.5","npv_sim:   726.2395340<br />scaled: 0.9363104<br />1/2: 0.5","npv_sim:   732.4504747<br />scaled: 0.9351467<br />1/2: 0.5","npv_sim:   738.6614153<br />scaled: 0.9339841<br />1/2: 0.5","npv_sim:   744.8723559<br />scaled: 0.9328218<br />1/2: 0.5","npv_sim:   751.0832965<br />scaled: 0.9316605<br />1/2: 0.5","npv_sim:   757.2942372<br />scaled: 0.9305005<br />1/2: 0.5","npv_sim:   763.5051778<br />scaled: 0.9293410<br />1/2: 0.5","npv_sim:   769.7161184<br />scaled: 0.9281841<br />1/2: 0.5","npv_sim:   775.9270591<br />scaled: 0.9270272<br />1/2: 0.5","npv_sim:   782.1379997<br />scaled: 0.9258736<br />1/2: 0.5","npv_sim:   788.3489403<br />scaled: 0.9247205<br />1/2: 0.5","npv_sim:   794.5598810<br />scaled: 0.9235700<br />1/2: 0.5","npv_sim:   800.7708216<br />scaled: 0.9224214<br />1/2: 0.5","npv_sim:   806.9817622<br />scaled: 0.9212741<br />1/2: 0.5","npv_sim:   813.1927028<br />scaled: 0.9201305<br />1/2: 0.5","npv_sim:   819.4036435<br />scaled: 0.9189868<br />1/2: 0.5","npv_sim:   825.6145841<br />scaled: 0.9178485<br />1/2: 0.5","npv_sim:   831.8255247<br />scaled: 0.9167102<br />1/2: 0.5","npv_sim:   838.0364654<br />scaled: 0.9155760<br />1/2: 0.5","npv_sim:   844.2474060<br />scaled: 0.9144436<br />1/2: 0.5","npv_sim:   850.4583466<br />scaled: 0.9133135<br />1/2: 0.5","npv_sim:   856.6692873<br />scaled: 0.9121872<br />1/2: 0.5","npv_sim:   862.8802279<br />scaled: 0.9110615<br />1/2: 0.5","npv_sim:   869.0911685<br />scaled: 0.9099414<br />1/2: 0.5","npv_sim:   875.3021092<br />scaled: 0.9088214<br />1/2: 0.5","npv_sim:   881.5130498<br />scaled: 0.9077065<br />1/2: 0.5","npv_sim:   887.7239904<br />scaled: 0.9065929<br />1/2: 0.5","npv_sim:   893.9349310<br />scaled: 0.9054826<br />1/2: 0.5","npv_sim:   900.1458717<br />scaled: 0.9043755<br />1/2: 0.5","npv_sim:   906.3568123<br />scaled: 0.9032698<br />1/2: 0.5","npv_sim:   912.5677529<br />scaled: 0.9021692<br />1/2: 0.5","npv_sim:   918.7786936<br />scaled: 0.9010687<br />1/2: 0.5","npv_sim:   924.9896342<br />scaled: 0.8999740<br />1/2: 0.5","npv_sim:   931.2005748<br />scaled: 0.8988799<br />1/2: 0.5","npv_sim:   937.4115155<br />scaled: 0.8977896<br />1/2: 0.5","npv_sim:   943.6224561<br />scaled: 0.8967018<br />1/2: 0.5","npv_sim:   949.8333967<br />scaled: 0.8956159<br />1/2: 0.5","npv_sim:   956.0443373<br />scaled: 0.8945342<br />1/2: 0.5","npv_sim:   962.2552780<br />scaled: 0.8934527<br />1/2: 0.5","npv_sim:   968.4662186<br />scaled: 0.8923768<br />1/2: 0.5","npv_sim:   974.6771592<br />scaled: 0.8913010<br />1/2: 0.5","npv_sim:   980.8880999<br />scaled: 0.8902291<br />1/2: 0.5","npv_sim:   987.0990405<br />scaled: 0.8891588<br />1/2: 0.5","npv_sim:   993.3099811<br />scaled: 0.8880907<br />1/2: 0.5","npv_sim:   999.5209218<br />scaled: 0.8870255<br />1/2: 0.5","npv_sim:  1005.7318624<br />scaled: 0.8859609<br />1/2: 0.5","npv_sim:  1011.9428030<br />scaled: 0.8849004<br />1/2: 0.5","npv_sim:  1018.1537437<br />scaled: 0.8838399<br />1/2: 0.5","npv_sim:  1024.3646843<br />scaled: 0.8827829<br />1/2: 0.5","npv_sim:  1030.5756249<br />scaled: 0.8817266<br />1/2: 0.5","npv_sim:  1036.7865655<br />scaled: 0.8806723<br />1/2: 0.5","npv_sim:  1042.9975062<br />scaled: 0.8796196<br />1/2: 0.5","npv_sim:  1049.2084468<br />scaled: 0.8785676<br />1/2: 0.5","npv_sim:  1055.4193874<br />scaled: 0.8775179<br />1/2: 0.5","npv_sim:  1061.6303281<br />scaled: 0.8764681<br />1/2: 0.5","npv_sim:  1067.8412687<br />scaled: 0.8754206<br />1/2: 0.5","npv_sim:  1074.0522093<br />scaled: 0.8743731<br />1/2: 0.5","npv_sim:  1080.2631500<br />scaled: 0.8733267<br />1/2: 0.5","npv_sim:  1086.4740906<br />scaled: 0.8722807<br />1/2: 0.5","npv_sim:  1092.6850312<br />scaled: 0.8712351<br />1/2: 0.5","npv_sim:  1098.8959718<br />scaled: 0.8701898<br />1/2: 0.5","npv_sim:  1105.1069125<br />scaled: 0.8691446<br />1/2: 0.5","npv_sim:  1111.3178531<br />scaled: 0.8680991<br />1/2: 0.5","npv_sim:  1117.5287937<br />scaled: 0.8670537<br />1/2: 0.5","npv_sim:  1123.7397344<br />scaled: 0.8660074<br />1/2: 0.5","npv_sim:  1129.9506750<br />scaled: 0.8649608<br />1/2: 0.5","npv_sim:  1136.1616156<br />scaled: 0.8639131<br />1/2: 0.5","npv_sim:  1142.3725563<br />scaled: 0.8628643<br />1/2: 0.5","npv_sim:  1148.5834969<br />scaled: 0.8618150<br />1/2: 0.5","npv_sim:  1154.7944375<br />scaled: 0.8607629<br />1/2: 0.5","npv_sim:  1161.0053781<br />scaled: 0.8597108<br />1/2: 0.5","npv_sim:  1167.2163188<br />scaled: 0.8586548<br />1/2: 0.5","npv_sim:  1173.4272594<br />scaled: 0.8575983<br />1/2: 0.5","npv_sim:  1179.6382000<br />scaled: 0.8565384<br />1/2: 0.5","npv_sim:  1185.8491407<br />scaled: 0.8554762<br />1/2: 0.5","npv_sim:  1192.0600813<br />scaled: 0.8544120<br />1/2: 0.5","npv_sim:  1198.2710219<br />scaled: 0.8533427<br />1/2: 0.5","npv_sim:  1204.4819626<br />scaled: 0.8522735<br />1/2: 0.5","npv_sim:  1210.6929032<br />scaled: 0.8511959<br />1/2: 0.5","npv_sim:  1216.9038438<br />scaled: 0.8501183<br />1/2: 0.5","npv_sim:  1223.1147845<br />scaled: 0.8490338<br />1/2: 0.5","npv_sim:  1229.3257251<br />scaled: 0.8479463<br />1/2: 0.5","npv_sim:  1235.5366657<br />scaled: 0.8468543<br />1/2: 0.5","npv_sim:  1241.7476063<br />scaled: 0.8457553<br />1/2: 0.5","npv_sim:  1247.9585470<br />scaled: 0.8446552<br />1/2: 0.5","npv_sim:  1254.1694876<br />scaled: 0.8435433<br />1/2: 0.5","npv_sim:  1260.3804282<br />scaled: 0.8424313<br />1/2: 0.5","npv_sim:  1266.5913689<br />scaled: 0.8413077<br />1/2: 0.5","npv_sim:  1272.8023095<br />scaled: 0.8401811<br />1/2: 0.5","npv_sim:  1279.0132501<br />scaled: 0.8390462<br />1/2: 0.5","npv_sim:  1285.2241908<br />scaled: 0.8379032<br />1/2: 0.5","npv_sim:  1291.4351314<br />scaled: 0.8367563<br />1/2: 0.5","npv_sim:  1297.6460720<br />scaled: 0.8355951<br />1/2: 0.5","npv_sim:  1303.8570126<br />scaled: 0.8344338<br />1/2: 0.5","npv_sim:  1310.0679533<br />scaled: 0.8332542<br />1/2: 0.5","npv_sim:  1316.2788939<br />scaled: 0.8320728<br />1/2: 0.5","npv_sim:  1322.4898345<br />scaled: 0.8308778<br />1/2: 0.5","npv_sim:  1328.7007752<br />scaled: 0.8296743<br />1/2: 0.5","npv_sim:  1334.9117158<br />scaled: 0.8284631<br />1/2: 0.5","npv_sim:  1341.1226564<br />scaled: 0.8272355<br />1/2: 0.5","npv_sim:  1347.3335971<br />scaled: 0.8260073<br />1/2: 0.5","npv_sim:  1353.5445377<br />scaled: 0.8247535<br />1/2: 0.5","npv_sim:  1359.7554783<br />scaled: 0.8234998<br />1/2: 0.5","npv_sim:  1365.9664190<br />scaled: 0.8222254<br />1/2: 0.5","npv_sim:  1372.1773596<br />scaled: 0.8209434<br />1/2: 0.5","npv_sim:  1378.3883002<br />scaled: 0.8196482<br />1/2: 0.5","npv_sim:  1384.5992408<br />scaled: 0.8183356<br />1/2: 0.5","npv_sim:  1390.8101815<br />scaled: 0.8170187<br />1/2: 0.5","npv_sim:  1397.0211221<br />scaled: 0.8156735<br />1/2: 0.5","npv_sim:  1403.2320627<br />scaled: 0.8143283<br />1/2: 0.5","npv_sim:  1409.4430034<br />scaled: 0.8129537<br />1/2: 0.5","npv_sim:  1415.6539440<br />scaled: 0.8115735<br />1/2: 0.5","npv_sim:  1421.8648846<br />scaled: 0.8101732<br />1/2: 0.5","npv_sim:  1428.0758253<br />scaled: 0.8087557<br />1/2: 0.5","npv_sim:  1434.2867659<br />scaled: 0.8073286<br />1/2: 0.5","npv_sim:  1440.4977065<br />scaled: 0.8058716<br />1/2: 0.5","npv_sim:  1446.7086471<br />scaled: 0.8044146<br />1/2: 0.5","npv_sim:  1452.9195878<br />scaled: 0.8029178<br />1/2: 0.5","npv_sim:  1459.1305284<br />scaled: 0.8014189<br />1/2: 0.5","npv_sim:  1465.3414690<br />scaled: 0.7998912<br />1/2: 0.5","npv_sim:  1471.5524097<br />scaled: 0.7983480<br />1/2: 0.5","npv_sim:  1477.7633503<br />scaled: 0.7967883<br />1/2: 0.5","npv_sim:  1483.9742909<br />scaled: 0.7951986<br />1/2: 0.5","npv_sim:  1490.1852316<br />scaled: 0.7936059<br />1/2: 0.5","npv_sim:  1496.3961722<br />scaled: 0.7919672<br />1/2: 0.5","npv_sim:  1502.6071128<br />scaled: 0.7903286<br />1/2: 0.5","npv_sim:  1508.8180534<br />scaled: 0.7886508<br />1/2: 0.5","npv_sim:  1515.0289941<br />scaled: 0.7869609<br />1/2: 0.5","npv_sim:  1521.2399347<br />scaled: 0.7852460<br />1/2: 0.5","npv_sim:  1527.4508753<br />scaled: 0.7835025<br />1/2: 0.5","npv_sim:  1533.6618160<br />scaled: 0.7817495<br />1/2: 0.5","npv_sim:  1539.8727566<br />scaled: 0.7799503<br />1/2: 0.5","npv_sim:  1546.0836972<br />scaled: 0.7781511<br />1/2: 0.5","npv_sim:  1552.2946379<br />scaled: 0.7763010<br />1/2: 0.5","npv_sim:  1558.5055785<br />scaled: 0.7744438<br />1/2: 0.5","npv_sim:  1564.7165191<br />scaled: 0.7725517<br />1/2: 0.5","npv_sim:  1570.9274598<br />scaled: 0.7706343<br />1/2: 0.5","npv_sim:  1577.1384004<br />scaled: 0.7686992<br />1/2: 0.5","npv_sim:  1583.3493410<br />scaled: 0.7667195<br />1/2: 0.5","npv_sim:  1589.5602816<br />scaled: 0.7647398<br />1/2: 0.5","npv_sim:  1595.7712223<br />scaled: 0.7626967<br />1/2: 0.5","npv_sim:  1601.9821629<br />scaled: 0.7606527<br />1/2: 0.5","npv_sim:  1608.1931035<br />scaled: 0.7585630<br />1/2: 0.5","npv_sim:  1614.4040442<br />scaled: 0.7564528<br />1/2: 0.5","npv_sim:  1620.6149848<br />scaled: 0.7543158<br />1/2: 0.5","npv_sim:  1626.8259254<br />scaled: 0.7521376<br />1/2: 0.5","npv_sim:  1633.0368661<br />scaled: 0.7499525<br />1/2: 0.5","npv_sim:  1639.2478067<br />scaled: 0.7477046<br />1/2: 0.5","npv_sim:  1645.4587473<br />scaled: 0.7454566<br />1/2: 0.5","npv_sim:  1651.6696879<br />scaled: 0.7431514<br />1/2: 0.5","npv_sim:  1657.8806286<br />scaled: 0.7408320<br />1/2: 0.5","npv_sim:  1664.0915692<br />scaled: 0.7384759<br />1/2: 0.5","npv_sim:  1670.3025098<br />scaled: 0.7360837<br />1/2: 0.5","npv_sim:  1676.5134505<br />scaled: 0.7336760<br />1/2: 0.5","npv_sim:  1682.7243911<br />scaled: 0.7312097<br />1/2: 0.5","npv_sim:  1688.9353317<br />scaled: 0.7287433<br />1/2: 0.5","npv_sim:  1695.1462724<br />scaled: 0.7262082<br />1/2: 0.5","npv_sim:  1701.3572130<br />scaled: 0.7236665<br />1/2: 0.5","npv_sim:  1707.5681536<br />scaled: 0.7210777<br />1/2: 0.5","npv_sim:  1713.7790943<br />scaled: 0.7184596<br />1/2: 0.5","npv_sim:  1719.9900349<br />scaled: 0.7158167<br />1/2: 0.5","npv_sim:  1726.2009755<br />scaled: 0.7131214<br />1/2: 0.5","npv_sim:  1732.4119161<br />scaled: 0.7104242<br />1/2: 0.5","npv_sim:  1738.6228568<br />scaled: 0.7076509<br />1/2: 0.5","npv_sim:  1744.8337974<br />scaled: 0.7048777<br />1/2: 0.5","npv_sim:  1751.0447380<br />scaled: 0.7020472<br />1/2: 0.5","npv_sim:  1757.2556787<br />scaled: 0.6991955<br />1/2: 0.5","npv_sim:  1763.4666193<br />scaled: 0.6963097<br />1/2: 0.5","npv_sim:  1769.6775599<br />scaled: 0.6933793<br />1/2: 0.5","npv_sim:  1775.8885006<br />scaled: 0.6904380<br />1/2: 0.5","npv_sim:  1782.0994412<br />scaled: 0.6874286<br />1/2: 0.5","npv_sim:  1788.3103818<br />scaled: 0.6844192<br />1/2: 0.5","npv_sim:  1794.5213224<br />scaled: 0.6813436<br />1/2: 0.5","npv_sim:  1800.7322631<br />scaled: 0.6782553<br />1/2: 0.5","npv_sim:  1806.9432037<br />scaled: 0.6751244<br />1/2: 0.5","npv_sim:  1813.1541443<br />scaled: 0.6719575<br />1/2: 0.5","npv_sim:  1819.3650850<br />scaled: 0.6687714<br />1/2: 0.5","npv_sim:  1825.5760256<br />scaled: 0.6655264<br />1/2: 0.5","npv_sim:  1831.7869662<br />scaled: 0.6622815<br />1/2: 0.5","npv_sim:  1837.9979069<br />scaled: 0.6589629<br />1/2: 0.5","npv_sim:  1844.2088475<br />scaled: 0.6556405<br />1/2: 0.5","npv_sim:  1850.4197881<br />scaled: 0.6522680<br />1/2: 0.5","npv_sim:  1856.6307288<br />scaled: 0.6488691<br />1/2: 0.5","npv_sim:  1862.8416694<br />scaled: 0.6454431<br />1/2: 0.5","npv_sim:  1869.0526100<br />scaled: 0.6419688<br />1/2: 0.5","npv_sim:  1875.2635506<br />scaled: 0.6384898<br />1/2: 0.5","npv_sim:  1881.4744913<br />scaled: 0.6349413<br />1/2: 0.5","npv_sim:  1887.6854319<br />scaled: 0.6313928<br />1/2: 0.5","npv_sim:  1893.8963725<br />scaled: 0.6277887<br />1/2: 0.5","npv_sim:  1900.1073132<br />scaled: 0.6241677<br />1/2: 0.5","npv_sim:  1900.1073132<br />scaled: 0.6241677<br />1/2: 0.5","npv_sim:  1900.1073132<br />scaled: 0.6241677<br />1/2: 0.5","npv_sim:  1893.8963725<br />scaled: 0.6277887<br />1/2: 0.5","npv_sim:  1887.6854319<br />scaled: 0.6313928<br />1/2: 0.5","npv_sim:  1881.4744913<br />scaled: 0.6349413<br />1/2: 0.5","npv_sim:  1875.2635506<br />scaled: 0.6384898<br />1/2: 0.5","npv_sim:  1869.0526100<br />scaled: 0.6419688<br />1/2: 0.5","npv_sim:  1862.8416694<br />scaled: 0.6454431<br />1/2: 0.5","npv_sim:  1856.6307288<br />scaled: 0.6488691<br />1/2: 0.5","npv_sim:  1850.4197881<br />scaled: 0.6522680<br />1/2: 0.5","npv_sim:  1844.2088475<br />scaled: 0.6556405<br />1/2: 0.5","npv_sim:  1837.9979069<br />scaled: 0.6589629<br />1/2: 0.5","npv_sim:  1831.7869662<br />scaled: 0.6622815<br />1/2: 0.5","npv_sim:  1825.5760256<br />scaled: 0.6655264<br />1/2: 0.5","npv_sim:  1819.3650850<br />scaled: 0.6687714<br />1/2: 0.5","npv_sim:  1813.1541443<br />scaled: 0.6719575<br />1/2: 0.5","npv_sim:  1806.9432037<br />scaled: 0.6751244<br />1/2: 0.5","npv_sim:  1800.7322631<br />scaled: 0.6782553<br />1/2: 0.5","npv_sim:  1794.5213224<br />scaled: 0.6813436<br />1/2: 0.5","npv_sim:  1788.3103818<br />scaled: 0.6844192<br />1/2: 0.5","npv_sim:  1782.0994412<br />scaled: 0.6874286<br />1/2: 0.5","npv_sim:  1775.8885006<br />scaled: 0.6904380<br />1/2: 0.5","npv_sim:  1769.6775599<br />scaled: 0.6933793<br />1/2: 0.5","npv_sim:  1763.4666193<br />scaled: 0.6963097<br />1/2: 0.5","npv_sim:  1757.2556787<br />scaled: 0.6991955<br />1/2: 0.5","npv_sim:  1751.0447380<br />scaled: 0.7020472<br />1/2: 0.5","npv_sim:  1744.8337974<br />scaled: 0.7048777<br />1/2: 0.5","npv_sim:  1738.6228568<br />scaled: 0.7076509<br />1/2: 0.5","npv_sim:  1732.4119161<br />scaled: 0.7104242<br />1/2: 0.5","npv_sim:  1726.2009755<br />scaled: 0.7131214<br />1/2: 0.5","npv_sim:  1719.9900349<br />scaled: 0.7158167<br />1/2: 0.5","npv_sim:  1713.7790943<br />scaled: 0.7184596<br />1/2: 0.5","npv_sim:  1707.5681536<br />scaled: 0.7210777<br />1/2: 0.5","npv_sim:  1701.3572130<br />scaled: 0.7236665<br />1/2: 0.5","npv_sim:  1695.1462724<br />scaled: 0.7262082<br />1/2: 0.5","npv_sim:  1688.9353317<br />scaled: 0.7287433<br />1/2: 0.5","npv_sim:  1682.7243911<br />scaled: 0.7312097<br />1/2: 0.5","npv_sim:  1676.5134505<br />scaled: 0.7336760<br />1/2: 0.5","npv_sim:  1670.3025098<br />scaled: 0.7360837<br />1/2: 0.5","npv_sim:  1664.0915692<br />scaled: 0.7384759<br />1/2: 0.5","npv_sim:  1657.8806286<br />scaled: 0.7408320<br />1/2: 0.5","npv_sim:  1651.6696879<br />scaled: 0.7431514<br />1/2: 0.5","npv_sim:  1645.4587473<br />scaled: 0.7454566<br />1/2: 0.5","npv_sim:  1639.2478067<br />scaled: 0.7477046<br />1/2: 0.5","npv_sim:  1633.0368661<br />scaled: 0.7499525<br />1/2: 0.5","npv_sim:  1626.8259254<br />scaled: 0.7521376<br />1/2: 0.5","npv_sim:  1620.6149848<br />scaled: 0.7543158<br />1/2: 0.5","npv_sim:  1614.4040442<br />scaled: 0.7564528<br />1/2: 0.5","npv_sim:  1608.1931035<br />scaled: 0.7585630<br />1/2: 0.5","npv_sim:  1601.9821629<br />scaled: 0.7606527<br />1/2: 0.5","npv_sim:  1595.7712223<br />scaled: 0.7626967<br />1/2: 0.5","npv_sim:  1589.5602816<br />scaled: 0.7647398<br />1/2: 0.5","npv_sim:  1583.3493410<br />scaled: 0.7667195<br />1/2: 0.5","npv_sim:  1577.1384004<br />scaled: 0.7686992<br />1/2: 0.5","npv_sim:  1570.9274598<br />scaled: 0.7706343<br />1/2: 0.5","npv_sim:  1564.7165191<br />scaled: 0.7725517<br />1/2: 0.5","npv_sim:  1558.5055785<br />scaled: 0.7744438<br />1/2: 0.5","npv_sim:  1552.2946379<br />scaled: 0.7763010<br />1/2: 0.5","npv_sim:  1546.0836972<br />scaled: 0.7781511<br />1/2: 0.5","npv_sim:  1539.8727566<br />scaled: 0.7799503<br />1/2: 0.5","npv_sim:  1533.6618160<br />scaled: 0.7817495<br />1/2: 0.5","npv_sim:  1527.4508753<br />scaled: 0.7835025<br />1/2: 0.5","npv_sim:  1521.2399347<br />scaled: 0.7852460<br />1/2: 0.5","npv_sim:  1515.0289941<br />scaled: 0.7869609<br />1/2: 0.5","npv_sim:  1508.8180534<br />scaled: 0.7886508<br />1/2: 0.5","npv_sim:  1502.6071128<br />scaled: 0.7903286<br />1/2: 0.5","npv_sim:  1496.3961722<br />scaled: 0.7919672<br />1/2: 0.5","npv_sim:  1490.1852316<br />scaled: 0.7936059<br />1/2: 0.5","npv_sim:  1483.9742909<br />scaled: 0.7951986<br />1/2: 0.5","npv_sim:  1477.7633503<br />scaled: 0.7967883<br />1/2: 0.5","npv_sim:  1471.5524097<br />scaled: 0.7983480<br />1/2: 0.5","npv_sim:  1465.3414690<br />scaled: 0.7998912<br />1/2: 0.5","npv_sim:  1459.1305284<br />scaled: 0.8014189<br />1/2: 0.5","npv_sim:  1452.9195878<br />scaled: 0.8029178<br />1/2: 0.5","npv_sim:  1446.7086471<br />scaled: 0.8044146<br />1/2: 0.5","npv_sim:  1440.4977065<br />scaled: 0.8058716<br />1/2: 0.5","npv_sim:  1434.2867659<br />scaled: 0.8073286<br />1/2: 0.5","npv_sim:  1428.0758253<br />scaled: 0.8087557<br />1/2: 0.5","npv_sim:  1421.8648846<br />scaled: 0.8101732<br />1/2: 0.5","npv_sim:  1415.6539440<br />scaled: 0.8115735<br />1/2: 0.5","npv_sim:  1409.4430034<br />scaled: 0.8129537<br />1/2: 0.5","npv_sim:  1403.2320627<br />scaled: 0.8143283<br />1/2: 0.5","npv_sim:  1397.0211221<br />scaled: 0.8156735<br />1/2: 0.5","npv_sim:  1390.8101815<br />scaled: 0.8170187<br />1/2: 0.5","npv_sim:  1384.5992408<br />scaled: 0.8183356<br />1/2: 0.5","npv_sim:  1378.3883002<br />scaled: 0.8196482<br />1/2: 0.5","npv_sim:  1372.1773596<br />scaled: 0.8209434<br />1/2: 0.5","npv_sim:  1365.9664190<br />scaled: 0.8222254<br />1/2: 0.5","npv_sim:  1359.7554783<br />scaled: 0.8234998<br />1/2: 0.5","npv_sim:  1353.5445377<br />scaled: 0.8247535<br />1/2: 0.5","npv_sim:  1347.3335971<br />scaled: 0.8260073<br />1/2: 0.5","npv_sim:  1341.1226564<br />scaled: 0.8272355<br />1/2: 0.5","npv_sim:  1334.9117158<br />scaled: 0.8284631<br />1/2: 0.5","npv_sim:  1328.7007752<br />scaled: 0.8296743<br />1/2: 0.5","npv_sim:  1322.4898345<br />scaled: 0.8308778<br />1/2: 0.5","npv_sim:  1316.2788939<br />scaled: 0.8320728<br />1/2: 0.5","npv_sim:  1310.0679533<br />scaled: 0.8332542<br />1/2: 0.5","npv_sim:  1303.8570126<br />scaled: 0.8344338<br />1/2: 0.5","npv_sim:  1297.6460720<br />scaled: 0.8355951<br />1/2: 0.5","npv_sim:  1291.4351314<br />scaled: 0.8367563<br />1/2: 0.5","npv_sim:  1285.2241908<br />scaled: 0.8379032<br />1/2: 0.5","npv_sim:  1279.0132501<br />scaled: 0.8390462<br />1/2: 0.5","npv_sim:  1272.8023095<br />scaled: 0.8401811<br />1/2: 0.5","npv_sim:  1266.5913689<br />scaled: 0.8413077<br />1/2: 0.5","npv_sim:  1260.3804282<br />scaled: 0.8424313<br />1/2: 0.5","npv_sim:  1254.1694876<br />scaled: 0.8435433<br />1/2: 0.5","npv_sim:  1247.9585470<br />scaled: 0.8446552<br />1/2: 0.5","npv_sim:  1241.7476063<br />scaled: 0.8457553<br />1/2: 0.5","npv_sim:  1235.5366657<br />scaled: 0.8468543<br />1/2: 0.5","npv_sim:  1229.3257251<br />scaled: 0.8479463<br />1/2: 0.5","npv_sim:  1223.1147845<br />scaled: 0.8490338<br />1/2: 0.5","npv_sim:  1216.9038438<br />scaled: 0.8501183<br />1/2: 0.5","npv_sim:  1210.6929032<br />scaled: 0.8511959<br />1/2: 0.5","npv_sim:  1204.4819626<br />scaled: 0.8522735<br />1/2: 0.5","npv_sim:  1198.2710219<br />scaled: 0.8533427<br />1/2: 0.5","npv_sim:  1192.0600813<br />scaled: 0.8544120<br />1/2: 0.5","npv_sim:  1185.8491407<br />scaled: 0.8554762<br />1/2: 0.5","npv_sim:  1179.6382000<br />scaled: 0.8565384<br />1/2: 0.5","npv_sim:  1173.4272594<br />scaled: 0.8575983<br />1/2: 0.5","npv_sim:  1167.2163188<br />scaled: 0.8586548<br />1/2: 0.5","npv_sim:  1161.0053781<br />scaled: 0.8597108<br />1/2: 0.5","npv_sim:  1154.7944375<br />scaled: 0.8607629<br />1/2: 0.5","npv_sim:  1148.5834969<br />scaled: 0.8618150<br />1/2: 0.5","npv_sim:  1142.3725563<br />scaled: 0.8628643<br />1/2: 0.5","npv_sim:  1136.1616156<br />scaled: 0.8639131<br />1/2: 0.5","npv_sim:  1129.9506750<br />scaled: 0.8649608<br />1/2: 0.5","npv_sim:  1123.7397344<br />scaled: 0.8660074<br />1/2: 0.5","npv_sim:  1117.5287937<br />scaled: 0.8670537<br />1/2: 0.5","npv_sim:  1111.3178531<br />scaled: 0.8680991<br />1/2: 0.5","npv_sim:  1105.1069125<br />scaled: 0.8691446<br />1/2: 0.5","npv_sim:  1098.8959718<br />scaled: 0.8701898<br />1/2: 0.5","npv_sim:  1092.6850312<br />scaled: 0.8712351<br />1/2: 0.5","npv_sim:  1086.4740906<br />scaled: 0.8722807<br />1/2: 0.5","npv_sim:  1080.2631500<br />scaled: 0.8733267<br />1/2: 0.5","npv_sim:  1074.0522093<br />scaled: 0.8743731<br />1/2: 0.5","npv_sim:  1067.8412687<br />scaled: 0.8754206<br />1/2: 0.5","npv_sim:  1061.6303281<br />scaled: 0.8764681<br />1/2: 0.5","npv_sim:  1055.4193874<br />scaled: 0.8775179<br />1/2: 0.5","npv_sim:  1049.2084468<br />scaled: 0.8785676<br />1/2: 0.5","npv_sim:  1042.9975062<br />scaled: 0.8796196<br />1/2: 0.5","npv_sim:  1036.7865655<br />scaled: 0.8806723<br />1/2: 0.5","npv_sim:  1030.5756249<br />scaled: 0.8817266<br />1/2: 0.5","npv_sim:  1024.3646843<br />scaled: 0.8827829<br />1/2: 0.5","npv_sim:  1018.1537437<br />scaled: 0.8838399<br />1/2: 0.5","npv_sim:  1011.9428030<br />scaled: 0.8849004<br />1/2: 0.5","npv_sim:  1005.7318624<br />scaled: 0.8859609<br />1/2: 0.5","npv_sim:   999.5209218<br />scaled: 0.8870255<br />1/2: 0.5","npv_sim:   993.3099811<br />scaled: 0.8880907<br />1/2: 0.5","npv_sim:   987.0990405<br />scaled: 0.8891588<br />1/2: 0.5","npv_sim:   980.8880999<br />scaled: 0.8902291<br />1/2: 0.5","npv_sim:   974.6771592<br />scaled: 0.8913010<br />1/2: 0.5","npv_sim:   968.4662186<br />scaled: 0.8923768<br />1/2: 0.5","npv_sim:   962.2552780<br />scaled: 0.8934527<br />1/2: 0.5","npv_sim:   956.0443373<br />scaled: 0.8945342<br />1/2: 0.5","npv_sim:   949.8333967<br />scaled: 0.8956159<br />1/2: 0.5","npv_sim:   943.6224561<br />scaled: 0.8967018<br />1/2: 0.5","npv_sim:   937.4115155<br />scaled: 0.8977896<br />1/2: 0.5","npv_sim:   931.2005748<br />scaled: 0.8988799<br />1/2: 0.5","npv_sim:   924.9896342<br />scaled: 0.8999740<br />1/2: 0.5","npv_sim:   918.7786936<br />scaled: 0.9010687<br />1/2: 0.5","npv_sim:   912.5677529<br />scaled: 0.9021692<br />1/2: 0.5","npv_sim:   906.3568123<br />scaled: 0.9032698<br />1/2: 0.5","npv_sim:   900.1458717<br />scaled: 0.9043755<br />1/2: 0.5","npv_sim:   893.9349310<br />scaled: 0.9054826<br />1/2: 0.5","npv_sim:   887.7239904<br />scaled: 0.9065929<br />1/2: 0.5","npv_sim:   881.5130498<br />scaled: 0.9077065<br />1/2: 0.5","npv_sim:   875.3021092<br />scaled: 0.9088214<br />1/2: 0.5","npv_sim:   869.0911685<br />scaled: 0.9099414<br />1/2: 0.5","npv_sim:   862.8802279<br />scaled: 0.9110615<br />1/2: 0.5","npv_sim:   856.6692873<br />scaled: 0.9121872<br />1/2: 0.5","npv_sim:   850.4583466<br />scaled: 0.9133135<br />1/2: 0.5","npv_sim:   844.2474060<br />scaled: 0.9144436<br />1/2: 0.5","npv_sim:   838.0364654<br />scaled: 0.9155760<br />1/2: 0.5","npv_sim:   831.8255247<br />scaled: 0.9167102<br />1/2: 0.5","npv_sim:   825.6145841<br />scaled: 0.9178485<br />1/2: 0.5","npv_sim:   819.4036435<br />scaled: 0.9189868<br />1/2: 0.5","npv_sim:   813.1927028<br />scaled: 0.9201305<br />1/2: 0.5","npv_sim:   806.9817622<br />scaled: 0.9212741<br />1/2: 0.5","npv_sim:   800.7708216<br />scaled: 0.9224214<br />1/2: 0.5","npv_sim:   794.5598810<br />scaled: 0.9235700<br />1/2: 0.5","npv_sim:   788.3489403<br />scaled: 0.9247205<br />1/2: 0.5","npv_sim:   782.1379997<br />scaled: 0.9258736<br />1/2: 0.5","npv_sim:   775.9270591<br />scaled: 0.9270272<br />1/2: 0.5","npv_sim:   769.7161184<br />scaled: 0.9281841<br />1/2: 0.5","npv_sim:   763.5051778<br />scaled: 0.9293410<br />1/2: 0.5","npv_sim:   757.2942372<br />scaled: 0.9305005<br />1/2: 0.5","npv_sim:   751.0832965<br />scaled: 0.9316605<br />1/2: 0.5","npv_sim:   744.8723559<br />scaled: 0.9328218<br />1/2: 0.5","npv_sim:   738.6614153<br />scaled: 0.9339841<br />1/2: 0.5","npv_sim:   732.4504747<br />scaled: 0.9351467<br />1/2: 0.5","npv_sim:   726.2395340<br />scaled: 0.9363104<br />1/2: 0.5","npv_sim:   720.0285934<br />scaled: 0.9374741<br />1/2: 0.5","npv_sim:   713.8176528<br />scaled: 0.9386382<br />1/2: 0.5","npv_sim:   707.6067121<br />scaled: 0.9398022<br />1/2: 0.5","npv_sim:   701.3957715<br />scaled: 0.9409658<br />1/2: 0.5","npv_sim:   695.1848309<br />scaled: 0.9421292<br />1/2: 0.5","npv_sim:   688.9738902<br />scaled: 0.9432919<br />1/2: 0.5","npv_sim:   682.7629496<br />scaled: 0.9444533<br />1/2: 0.5","npv_sim:   676.5520090<br />scaled: 0.9456145<br />1/2: 0.5","npv_sim:   670.3410684<br />scaled: 0.9467726<br />1/2: 0.5","npv_sim:   664.1301277<br />scaled: 0.9479308<br />1/2: 0.5","npv_sim:   657.9191871<br />scaled: 0.9490853<br />1/2: 0.5","npv_sim:   651.7082465<br />scaled: 0.9502388<br />1/2: 0.5","npv_sim:   645.4973058<br />scaled: 0.9513893<br />1/2: 0.5","npv_sim:   639.2863652<br />scaled: 0.9525365<br />1/2: 0.5","npv_sim:   633.0754246<br />scaled: 0.9536824<br />1/2: 0.5","npv_sim:   626.8644839<br />scaled: 0.9548217<br />1/2: 0.5","npv_sim:   620.6535433<br />scaled: 0.9559610<br />1/2: 0.5","npv_sim:   614.4426027<br />scaled: 0.9570919<br />1/2: 0.5","npv_sim:   608.2316620<br />scaled: 0.9582216<br />1/2: 0.5","npv_sim:   602.0207214<br />scaled: 0.9593446<br />1/2: 0.5","npv_sim:   595.8097808<br />scaled: 0.9604627<br />1/2: 0.5","npv_sim:   589.5988402<br />scaled: 0.9615770<br />1/2: 0.5","npv_sim:   583.3878995<br />scaled: 0.9626816<br />1/2: 0.5","npv_sim:   577.1769589<br />scaled: 0.9637862<br />1/2: 0.5","npv_sim:   570.9660183<br />scaled: 0.9648754<br />1/2: 0.5","npv_sim:   564.7550776<br />scaled: 0.9659644<br />1/2: 0.5","npv_sim:   558.5441370<br />scaled: 0.9670411<br />1/2: 0.5","npv_sim:   552.3331964<br />scaled: 0.9681122<br />1/2: 0.5","npv_sim:   546.1222557<br />scaled: 0.9691756<br />1/2: 0.5","npv_sim:   539.9113151<br />scaled: 0.9702265<br />1/2: 0.5","npv_sim:   533.7003745<br />scaled: 0.9712754<br />1/2: 0.5","npv_sim:   527.4894339<br />scaled: 0.9723039<br />1/2: 0.5","npv_sim:   521.2784932<br />scaled: 0.9733323<br />1/2: 0.5","npv_sim:   515.0675526<br />scaled: 0.9743408<br />1/2: 0.5","npv_sim:   508.8566120<br />scaled: 0.9753442<br />1/2: 0.5","npv_sim:   502.6456713<br />scaled: 0.9763337<br />1/2: 0.5","npv_sim:   496.4347307<br />scaled: 0.9773094<br />1/2: 0.5","npv_sim:   490.2237901<br />scaled: 0.9782790<br />1/2: 0.5","npv_sim:   484.0128494<br />scaled: 0.9792243<br />1/2: 0.5","npv_sim:   477.8019088<br />scaled: 0.9801697<br />1/2: 0.5","npv_sim:   471.5909682<br />scaled: 0.9810850<br />1/2: 0.5","npv_sim:   465.3800275<br />scaled: 0.9819972<br />1/2: 0.5","npv_sim:   459.1690869<br />scaled: 0.9828875<br />1/2: 0.5","npv_sim:   452.9581463<br />scaled: 0.9837638<br />1/2: 0.5","npv_sim:   446.7472057<br />scaled: 0.9846279<br />1/2: 0.5","npv_sim:   440.5362650<br />scaled: 0.9854653<br />1/2: 0.5","npv_sim:   434.3253244<br />scaled: 0.9863020<br />1/2: 0.5","npv_sim:   428.1143838<br />scaled: 0.9870976<br />1/2: 0.5","npv_sim:   421.9034431<br />scaled: 0.9878932<br />1/2: 0.5","npv_sim:   415.6925025<br />scaled: 0.9886565<br />1/2: 0.5","npv_sim:   409.4815619<br />scaled: 0.9894073<br />1/2: 0.5","npv_sim:   403.2706212<br />scaled: 0.9901377<br />1/2: 0.5","npv_sim:   397.0596806<br />scaled: 0.9908406<br />1/2: 0.5","npv_sim:   390.8487400<br />scaled: 0.9915369<br />1/2: 0.5","npv_sim:   384.6377994<br />scaled: 0.9921889<br />1/2: 0.5","npv_sim:   378.4268587<br />scaled: 0.9928409<br />1/2: 0.5","npv_sim:   372.2159181<br />scaled: 0.9934479<br />1/2: 0.5","npv_sim:   366.0049775<br />scaled: 0.9940458<br />1/2: 0.5","npv_sim:   359.7940368<br />scaled: 0.9946131<br />1/2: 0.5","npv_sim:   353.5830962<br />scaled: 0.9951540<br />1/2: 0.5","npv_sim:   347.3721556<br />scaled: 0.9956803<br />1/2: 0.5","npv_sim:   341.1612149<br />scaled: 0.9961610<br />1/2: 0.5","npv_sim:   334.9502743<br />scaled: 0.9966417<br />1/2: 0.5","npv_sim:   328.7393337<br />scaled: 0.9970626<br />1/2: 0.5","npv_sim:   322.5283930<br />scaled: 0.9974801<br />1/2: 0.5","npv_sim:   316.3174524<br />scaled: 0.9978546<br />1/2: 0.5","npv_sim:   310.1065118<br />scaled: 0.9982058<br />1/2: 0.5","npv_sim:   303.8955712<br />scaled: 0.9985326<br />1/2: 0.5","npv_sim:   297.6846305<br />scaled: 0.9988146<br />1/2: 0.5","npv_sim:   291.4736899<br />scaled: 0.9990925<br />1/2: 0.5","npv_sim:   285.2627493<br />scaled: 0.9993024<br />1/2: 0.5","npv_sim:   279.0518086<br />scaled: 0.9995122<br />1/2: 0.5","npv_sim:   272.8408680<br />scaled: 0.9996652<br />1/2: 0.5","npv_sim:   266.6299274<br />scaled: 0.9998001<br />1/2: 0.5","npv_sim:   260.4189867<br />scaled: 0.9998990<br />1/2: 0.5","npv_sim:   254.2080461<br />scaled: 0.9999562<br />1/2: 0.5","npv_sim:   247.9971055<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   241.7861649<br />scaled: 0.9999769<br />1/2: 0.5","npv_sim:   235.5752242<br />scaled: 0.9999538<br />1/2: 0.5","npv_sim:   229.3642836<br />scaled: 0.9998585<br />1/2: 0.5","npv_sim:   223.1533430<br />scaled: 0.9997526<br />1/2: 0.5","npv_sim:   216.9424023<br />scaled: 0.9995977<br />1/2: 0.5","npv_sim:   210.7314617<br />scaled: 0.9994065<br />1/2: 0.5","npv_sim:   204.5205211<br />scaled: 0.9991910<br />1/2: 0.5","npv_sim:   198.3095804<br />scaled: 0.9989123<br />1/2: 0.5","npv_sim:   192.0986398<br />scaled: 0.9986337<br />1/2: 0.5","npv_sim:   185.8876992<br />scaled: 0.9982671<br />1/2: 0.5","npv_sim:   179.6767586<br />scaled: 0.9978989<br />1/2: 0.5","npv_sim:   173.4658179<br />scaled: 0.9974679<br />1/2: 0.5","npv_sim:   167.2548773<br />scaled: 0.9970082<br />1/2: 0.5","npv_sim:   161.0439367<br />scaled: 0.9965122<br />1/2: 0.5","npv_sim:   154.8329960<br />scaled: 0.9959594<br />1/2: 0.5","npv_sim:   148.6220554<br />scaled: 0.9953976<br />1/2: 0.5","npv_sim:   142.4111148<br />scaled: 0.9947500<br />1/2: 0.5","npv_sim:   136.2001741<br />scaled: 0.9941023<br />1/2: 0.5","npv_sim:   129.9892335<br />scaled: 0.9933780<br />1/2: 0.5","npv_sim:   123.7782929<br />scaled: 0.9926343<br />1/2: 0.5","npv_sim:   117.5673522<br />scaled: 0.9918418<br />1/2: 0.5","npv_sim:   111.3564116<br />scaled: 0.9910008<br />1/2: 0.5","npv_sim:   105.1454710<br />scaled: 0.9901397<br />1/2: 0.5","npv_sim:    98.9345304<br />scaled: 0.9892005<br />1/2: 0.5","npv_sim:    92.7235897<br />scaled: 0.9882614<br />1/2: 0.5","npv_sim:    86.5126491<br />scaled: 0.9872324<br />1/2: 0.5","npv_sim:    80.3017085<br />scaled: 0.9861943<br />1/2: 0.5","npv_sim:    74.0907678<br />scaled: 0.9850955<br />1/2: 0.5","npv_sim:    67.8798272<br />scaled: 0.9839582<br />1/2: 0.5","npv_sim:    61.6688866<br />scaled: 0.9827894<br />1/2: 0.5","npv_sim:    55.4579459<br />scaled: 0.9815525<br />1/2: 0.5","npv_sim:    49.2470053<br />scaled: 0.9803136<br />1/2: 0.5","npv_sim:    43.0360647<br />scaled: 0.9789772<br />1/2: 0.5","npv_sim:    36.8251241<br />scaled: 0.9776408<br />1/2: 0.5","npv_sim:    30.6141834<br />scaled: 0.9762326<br />1/2: 0.5","npv_sim:    24.4032428<br />scaled: 0.9747969<br />1/2: 0.5","npv_sim:    18.1923022<br />scaled: 0.9733191<br />1/2: 0.5","npv_sim:    11.9813615<br />scaled: 0.9717846<br />1/2: 0.5","npv_sim:     5.7704209<br />scaled: 0.9702374<br />1/2: 0.5","npv_sim:    -0.4405197<br />scaled: 0.9686050<br />1/2: 0.5","npv_sim:    -6.6514604<br />scaled: 0.9669726<br />1/2: 0.5","npv_sim:   -12.8624010<br />scaled: 0.9652592<br />1/2: 0.5","npv_sim:   -19.0733416<br />scaled: 0.9635298<br />1/2: 0.5","npv_sim:   -25.2842823<br />scaled: 0.9617489<br />1/2: 0.5","npv_sim:   -31.4952229<br />scaled: 0.9599238<br />1/2: 0.5","npv_sim:   -37.7061635<br />scaled: 0.9580758<br />1/2: 0.5","npv_sim:   -43.9171041<br />scaled: 0.9561565<br />1/2: 0.5","npv_sim:   -50.1280448<br />scaled: 0.9542371<br />1/2: 0.5","npv_sim:   -56.3389854<br />scaled: 0.9522302<br />1/2: 0.5","npv_sim:   -62.5499260<br />scaled: 0.9502184<br />1/2: 0.5","npv_sim:   -68.7608667<br />scaled: 0.9481477<br />1/2: 0.5","npv_sim:   -74.9718073<br />scaled: 0.9460453<br />1/2: 0.5","npv_sim:   -81.1827479<br />scaled: 0.9439117<br />1/2: 0.5","npv_sim:   -87.3936886<br />scaled: 0.9417209<br />1/2: 0.5","npv_sim:   -93.6046292<br />scaled: 0.9395252<br />1/2: 0.5","npv_sim:   -99.8155698<br />scaled: 0.9372485<br />1/2: 0.5","npv_sim:  -106.0265104<br />scaled: 0.9349719<br />1/2: 0.5","npv_sim:  -112.2374511<br />scaled: 0.9326317<br />1/2: 0.5","npv_sim:  -118.4483917<br />scaled: 0.9302717<br />1/2: 0.5","npv_sim:  -124.6593323<br />scaled: 0.9278743<br />1/2: 0.5","npv_sim:  -130.8702730<br />scaled: 0.9254338<br />1/2: 0.5","npv_sim:  -137.0812136<br />scaled: 0.9229802<br />1/2: 0.5","npv_sim:  -143.2921542<br />scaled: 0.9204622<br />1/2: 0.5","npv_sim:  -149.5030949<br />scaled: 0.9179442<br />1/2: 0.5","npv_sim:  -155.7140355<br />scaled: 0.9153614<br />1/2: 0.5","npv_sim:  -161.9249761<br />scaled: 0.9127690<br />1/2: 0.5","npv_sim:  -168.1359167<br />scaled: 0.9101358<br />1/2: 0.5","npv_sim:  -174.3468574<br />scaled: 0.9074726<br />1/2: 0.5","npv_sim:  -180.5577980<br />scaled: 0.9047904<br />1/2: 0.5","npv_sim:  -186.7687386<br />scaled: 0.9020596<br />1/2: 0.5","npv_sim:  -192.9796793<br />scaled: 0.8993289<br />1/2: 0.5","npv_sim:  -199.1906199<br />scaled: 0.8965353<br />1/2: 0.5","npv_sim:  -205.4015605<br />scaled: 0.8937408<br />1/2: 0.5","npv_sim:  -211.6125012<br />scaled: 0.8909049<br />1/2: 0.5","npv_sim:  -217.8234418<br />scaled: 0.8880502<br />1/2: 0.5","npv_sim:  -224.0343824<br />scaled: 0.8851736<br />1/2: 0.5","npv_sim:  -230.2453231<br />scaled: 0.8822627<br />1/2: 0.5","npv_sim:  -236.4562637<br />scaled: 0.8793468<br />1/2: 0.5","npv_sim:  -242.6672043<br />scaled: 0.8763837<br />1/2: 0.5","npv_sim:  -248.8781449<br />scaled: 0.8734205<br />1/2: 0.5","npv_sim:  -255.0890856<br />scaled: 0.8704188<br />1/2: 0.5","npv_sim:  -261.3000262<br />scaled: 0.8674074<br />1/2: 0.5","npv_sim:  -267.5109668<br />scaled: 0.8643738<br />1/2: 0.5","npv_sim:  -273.7219075<br />scaled: 0.8613183<br />1/2: 0.5","npv_sim:  -279.9328481<br />scaled: 0.8582544<br />1/2: 0.5","npv_sim:  -286.1437887<br />scaled: 0.8551588<br />1/2: 0.5","npv_sim:  -292.3547294<br />scaled: 0.8520631<br />1/2: 0.5","npv_sim:  -298.5656700<br />scaled: 0.8489348<br />1/2: 0.5","npv_sim:  -304.7766106<br />scaled: 0.8458032<br />1/2: 0.5","npv_sim:  -310.9875512<br />scaled: 0.8426522<br />1/2: 0.5","npv_sim:  -317.1984919<br />scaled: 0.8394888<br />1/2: 0.5","npv_sim:  -323.4094325<br />scaled: 0.8363167<br />1/2: 0.5","npv_sim:  -329.6203731<br />scaled: 0.8331257<br />1/2: 0.5","npv_sim:  -335.8313138<br />scaled: 0.8299342<br />1/2: 0.5","npv_sim:  -342.0422544<br />scaled: 0.8267196<br />1/2: 0.5","npv_sim:  -348.2531950<br />scaled: 0.8235050<br />1/2: 0.5","npv_sim:  -354.4641357<br />scaled: 0.8202763<br />1/2: 0.5","npv_sim:  -360.6750763<br />scaled: 0.8170423<br />1/2: 0.5","npv_sim:  -366.8860169<br />scaled: 0.8138015<br />1/2: 0.5","npv_sim:  -373.0969576<br />scaled: 0.8105519<br />1/2: 0.5","npv_sim:  -379.3078982<br />scaled: 0.8073008<br />1/2: 0.5","npv_sim:  -385.5188388<br />scaled: 0.8040396<br />1/2: 0.5","npv_sim:  -391.7297794<br />scaled: 0.8007784<br />1/2: 0.5","npv_sim:  -397.9407201<br />scaled: 0.7975107<br />1/2: 0.5","npv_sim:  -404.1516607<br />scaled: 0.7942417<br />1/2: 0.5","npv_sim:  -410.3626013<br />scaled: 0.7909706<br />1/2: 0.5","npv_sim:  -416.5735420<br />scaled: 0.7876975<br />1/2: 0.5","npv_sim:  -422.7844826<br />scaled: 0.7844243<br />1/2: 0.5","npv_sim:  -428.9954232<br />scaled: 0.7811508<br />1/2: 0.5","npv_sim:  -435.2063639<br />scaled: 0.7778773<br />1/2: 0.5","npv_sim:  -441.4173045<br />scaled: 0.7746065<br />1/2: 0.5","npv_sim:  -447.6282451<br />scaled: 0.7713359<br />1/2: 0.5","npv_sim:  -453.8391857<br />scaled: 0.7680695<br />1/2: 0.5","npv_sim:  -460.0501264<br />scaled: 0.7648051<br />1/2: 0.5","npv_sim:  -466.2610670<br />scaled: 0.7615442<br />1/2: 0.5","npv_sim:  -472.4720076<br />scaled: 0.7582893<br />1/2: 0.5","npv_sim:  -478.6829483<br />scaled: 0.7550351<br />1/2: 0.5","npv_sim:  -484.8938889<br />scaled: 0.7517926<br />1/2: 0.5","npv_sim:  -491.1048295<br />scaled: 0.7485501<br />1/2: 0.5","npv_sim:  -497.3157702<br />scaled: 0.7453192<br />1/2: 0.5","npv_sim:  -503.5267108<br />scaled: 0.7420919<br />1/2: 0.5","npv_sim:  -509.7376514<br />scaled: 0.7388729<br />1/2: 0.5","npv_sim:  -515.9485920<br />scaled: 0.7356634<br />1/2: 0.5","npv_sim:  -522.1595327<br />scaled: 0.7324574<br />1/2: 0.5","npv_sim:  -528.3704733<br />scaled: 0.7292681<br />1/2: 0.5","npv_sim:  -534.5814139<br />scaled: 0.7260788<br />1/2: 0.5","npv_sim:  -540.7923546<br />scaled: 0.7229092<br />1/2: 0.5","npv_sim:  -547.0032952<br />scaled: 0.7197423<br />1/2: 0.5","npv_sim:  -553.2142358<br />scaled: 0.7165896<br />1/2: 0.5","npv_sim:  -559.4251765<br />scaled: 0.7134472<br />1/2: 0.5","npv_sim:  -565.6361171<br />scaled: 0.7103121<br />1/2: 0.5","npv_sim:  -571.8470577<br />scaled: 0.7071960<br />1/2: 0.5","npv_sim:  -578.0579984<br />scaled: 0.7040798<br />1/2: 0.5","npv_sim:  -584.2689390<br />scaled: 0.7009910<br />1/2: 0.5","npv_sim:  -590.4798796<br />scaled: 0.6979027<br />1/2: 0.5","npv_sim:  -596.6908202<br />scaled: 0.6948344<br />1/2: 0.5","npv_sim:  -602.9017609<br />scaled: 0.6917753<br />1/2: 0.5","npv_sim:  -609.1127015<br />scaled: 0.6887280<br />1/2: 0.5","npv_sim:  -615.3236421<br />scaled: 0.6856991<br />1/2: 0.5","npv_sim:  -621.5345828<br />scaled: 0.6826733<br />1/2: 0.5","npv_sim:  -627.7455234<br />scaled: 0.6796757<br />1/2: 0.5","npv_sim:  -633.9564640<br />scaled: 0.6766781<br />1/2: 0.5","npv_sim:  -640.1674047<br />scaled: 0.6737061<br />1/2: 0.5","npv_sim:  -646.3783453<br />scaled: 0.6707405<br />1/2: 0.5","npv_sim:  -652.5892859<br />scaled: 0.6677913<br />1/2: 0.5","npv_sim:  -658.8002265<br />scaled: 0.6648582<br />1/2: 0.5","npv_sim:  -665.0111672<br />scaled: 0.6619318<br />1/2: 0.5","npv_sim:  -671.2221078<br />scaled: 0.6590315<br />1/2: 0.5","npv_sim:  -677.4330484<br />scaled: 0.6561311<br />1/2: 0.5","npv_sim:  -683.6439891<br />scaled: 0.6532607<br />1/2: 0.5","npv_sim:  -689.8549297<br />scaled: 0.6503932<br />1/2: 0.5","npv_sim:  -696.0658703<br />scaled: 0.6475458<br />1/2: 0.5","npv_sim:  -702.2768110<br />scaled: 0.6447110<br />1/2: 0.5","npv_sim:  -708.4877516<br />scaled: 0.6418866<br />1/2: 0.5","npv_sim:  -714.6986922<br />scaled: 0.6390842<br />1/2: 0.5","npv_sim:  -720.9096329<br />scaled: 0.6362826<br />1/2: 0.5","npv_sim:  -727.1205735<br />scaled: 0.6335122<br />1/2: 0.5","npv_sim:  -733.3315141<br />scaled: 0.6307418<br />1/2: 0.5","npv_sim:  -739.5424547<br />scaled: 0.6279941<br />1/2: 0.5","npv_sim:  -745.7533954<br />scaled: 0.6252550<br />1/2: 0.5","npv_sim:  -751.9643360<br />scaled: 0.6225289<br />1/2: 0.5","npv_sim:  -758.1752766<br />scaled: 0.6198202<br />1/2: 0.5","npv_sim:  -764.3862173<br />scaled: 0.6171154<br />1/2: 0.5","npv_sim:  -770.5971579<br />scaled: 0.6144360<br />1/2: 0.5","npv_sim:  -776.8080985<br />scaled: 0.6117567<br />1/2: 0.5","npv_sim:  -783.0190392<br />scaled: 0.6091010<br />1/2: 0.5","npv_sim:  -789.2299798<br />scaled: 0.6064499<br />1/2: 0.5","npv_sim:  -795.4409204<br />scaled: 0.6038135<br />1/2: 0.5","npv_sim:  -801.6518610<br />scaled: 0.6011893<br />1/2: 0.5","npv_sim:  -807.8628017<br />scaled: 0.5985714<br />1/2: 0.5","npv_sim:  -814.0737423<br />scaled: 0.5959728<br />1/2: 0.5","npv_sim:  -820.2846829<br />scaled: 0.5933742<br />1/2: 0.5","npv_sim:  -826.4956236<br />scaled: 0.5907983<br />1/2: 0.5","npv_sim:  -832.7065642<br />scaled: 0.5882236<br />1/2: 0.5","npv_sim:  -838.9175048<br />scaled: 0.5856635<br />1/2: 0.5","npv_sim:  -845.1284455<br />scaled: 0.5831111<br />1/2: 0.5","npv_sim:  -851.3393861<br />scaled: 0.5805661<br />1/2: 0.5","npv_sim:  -857.5503267<br />scaled: 0.5780343<br />1/2: 0.5","npv_sim:  -863.7612674<br />scaled: 0.5755036<br />1/2: 0.5","npv_sim:  -869.9722080<br />scaled: 0.5729905<br />1/2: 0.5","npv_sim:  -876.1831486<br />scaled: 0.5704774<br />1/2: 0.5","npv_sim:  -882.3940892<br />scaled: 0.5679772<br />1/2: 0.5","npv_sim:  -888.6050299<br />scaled: 0.5654809<br />1/2: 0.5","npv_sim:  -894.8159705<br />scaled: 0.5629916<br />1/2: 0.5","npv_sim:  -901.0269111<br />scaled: 0.5605102<br />1/2: 0.5","npv_sim:  -907.2378518<br />scaled: 0.5580310<br />1/2: 0.5","npv_sim:  -913.4487924<br />scaled: 0.5555624<br />1/2: 0.5","npv_sim:  -919.6597330<br />scaled: 0.5530938<br />1/2: 0.5","npv_sim:  -925.8706737<br />scaled: 0.5506347<br />1/2: 0.5","npv_sim:  -932.0816143<br />scaled: 0.5481769<br />1/2: 0.5","npv_sim:  -938.2925549<br />scaled: 0.5457242<br />1/2: 0.5","npv_sim:  -944.5034955<br />scaled: 0.5432752<br />1/2: 0.5","npv_sim:  -950.7144362<br />scaled: 0.5408281<br />1/2: 0.5","npv_sim:  -956.9253768<br />scaled: 0.5383857<br />1/2: 0.5","npv_sim:  -963.1363174<br />scaled: 0.5359433<br />1/2: 0.5","npv_sim:  -969.3472581<br />scaled: 0.5335055<br />1/2: 0.5","npv_sim:  -975.5581987<br />scaled: 0.5310678<br />1/2: 0.5","npv_sim:  -981.7691393<br />scaled: 0.5286318<br />1/2: 0.5","npv_sim:  -987.9800800<br />scaled: 0.5261967<br />1/2: 0.5","npv_sim:  -994.1910206<br />scaled: 0.5237617<br />1/2: 0.5","npv_sim: -1000.4019612<br />scaled: 0.5213271<br />1/2: 0.5","npv_sim: -1006.6129018<br />scaled: 0.5188923<br />1/2: 0.5","npv_sim: -1012.8238425<br />scaled: 0.5164563<br />1/2: 0.5","npv_sim: -1019.0347831<br />scaled: 0.5140202<br />1/2: 0.5","npv_sim: -1025.2457237<br />scaled: 0.5115814<br />1/2: 0.5","npv_sim: -1031.4566644<br />scaled: 0.5091420<br />1/2: 0.5","npv_sim: -1037.6676050<br />scaled: 0.5067000<br />1/2: 0.5","npv_sim: -1043.8785456<br />scaled: 0.5042553<br />1/2: 0.5","npv_sim: -1050.0894863<br />scaled: 0.5018092<br />1/2: 0.5","npv_sim: -1056.3004269<br />scaled: 0.4993575<br />1/2: 0.5","npv_sim: -1062.5113675<br />scaled: 0.4969058<br />1/2: 0.5","npv_sim: -1068.7223082<br />scaled: 0.4944462<br />1/2: 0.5","npv_sim: -1074.9332488<br />scaled: 0.4919857<br />1/2: 0.5","npv_sim: -1081.1441894<br />scaled: 0.4895189<br />1/2: 0.5","npv_sim: -1087.3551300<br />scaled: 0.4870481<br />1/2: 0.5","npv_sim: -1093.5660707<br />scaled: 0.4845735<br />1/2: 0.5","npv_sim: -1099.7770113<br />scaled: 0.4820907<br />1/2: 0.5","npv_sim: -1105.9879519<br />scaled: 0.4796077<br />1/2: 0.5","npv_sim: -1112.1988926<br />scaled: 0.4771116<br />1/2: 0.5","npv_sim: -1118.4098332<br />scaled: 0.4746156<br />1/2: 0.5","npv_sim: -1124.6207738<br />scaled: 0.4721088<br />1/2: 0.5","npv_sim: -1130.8317145<br />scaled: 0.4695981<br />1/2: 0.5","npv_sim: -1137.0426551<br />scaled: 0.4670806<br />1/2: 0.5","npv_sim: -1143.2535957<br />scaled: 0.4645540<br />1/2: 0.5","npv_sim: -1149.4645363<br />scaled: 0.4620251<br />1/2: 0.5","npv_sim: -1155.6754770<br />scaled: 0.4594816<br />1/2: 0.5","npv_sim: -1161.8864176<br />scaled: 0.4569381<br />1/2: 0.5","npv_sim: -1168.0973582<br />scaled: 0.4543795<br />1/2: 0.5","npv_sim: -1174.3082989<br />scaled: 0.4518181<br />1/2: 0.5","npv_sim: -1180.5192395<br />scaled: 0.4492465<br />1/2: 0.5","npv_sim: -1186.7301801<br />scaled: 0.4466664<br />1/2: 0.5","npv_sim: -1192.9411208<br />scaled: 0.4440815<br />1/2: 0.5","npv_sim: -1199.1520614<br />scaled: 0.4414820<br />1/2: 0.5","npv_sim: -1205.3630020<br />scaled: 0.4388825<br />1/2: 0.5","npv_sim: -1211.5739427<br />scaled: 0.4362641<br />1/2: 0.5","npv_sim: -1217.7848833<br />scaled: 0.4336446<br />1/2: 0.5","npv_sim: -1223.9958239<br />scaled: 0.4310119<br />1/2: 0.5","npv_sim: -1230.2067645<br />scaled: 0.4283721<br />1/2: 0.5","npv_sim: -1236.4177052<br />scaled: 0.4257250<br />1/2: 0.5","npv_sim: -1242.6286458<br />scaled: 0.4230646<br />1/2: 0.5","npv_sim: -1248.8395864<br />scaled: 0.4204030<br />1/2: 0.5","npv_sim: -1255.0505271<br />scaled: 0.4177219<br />1/2: 0.5","npv_sim: -1261.2614677<br />scaled: 0.4150408<br />1/2: 0.5","npv_sim: -1267.4724083<br />scaled: 0.4123439<br />1/2: 0.5","npv_sim: -1273.6833490<br />scaled: 0.4096422<br />1/2: 0.5","npv_sim: -1273.6833490<br />scaled: 0.4096422<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,397.397752699523,397.397752699523],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 397.3978","xintercept: 397.3978"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[596.096629049285],"y":[0.25],"text":"Median NPV:<br />  397.4","hovertext":"x: 596.0966<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[596.096629049285],"y":[0.1],"text":"SD NPV:<br />  1036.85","hovertext":"x: 596.0966<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic of Effects From Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1718.84745847214,2470.55621552245],"tickmode":"array","ticktext":["-1000","0","1000","2000"],"tickvals":[-1000,0,1000,2000],"categoryorder":"array","categoryarray":["-1000","0","1000","2000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"50e53bdc35f":{"x":{},"y":{},"alpha":{},"type":"scatter"},"50e51f6593e4":{"xintercept":{}},"50e545d06dd6":{"x":{},"y":{}},"50e576db2068":{"x":{},"y":{}}},"cur_data":"50e53bdc35f","visdat":{"50e53bdc35f":["function (y) ","x"],"50e51f6593e4":["function (y) ","x"],"50e545d06dd6":["function (y) ","x"],"50e576db2068":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
# dens <- density(npv_sim)
# fig <- plot_ly(x=~dens$x, y = ~dens$y, type='scatter', mode='lines', fill='tozeroy')
# fig <- fig %>% 
#   layout(xaxis=list(title='NPV'), yaxis=list(title='Distribution Density'), title=paste0("Distribution of NPV of ", policy_estimates_text[position]))
# fig
#knitr::purl("code/05_final_opa.Rmd", "code/shiny_app/all_analysis.R")
```

# Sensitivity Analysis  



# References


[^1]: EAs version of the analysis follows a similar structure than the cost effectiveness analysis performed by the charity evaluator GiveWell [@givewell].


[^5]:`F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]` Original [here](https://docs.google.com/spreadsheets/d/1jzS693Y-ZAIloQejlzSc3e3t7iPHyor1qt7HBjSVXhQ/edit#gid=509033857), editable version [here](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=509033857)
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter.


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit


[^6]: last paragraph of page 9(1645) of @baird2016worms

[^7]: series avalable in file `~/opa-deworming/docs/materials/original_materials/Baird-etal-QJE-2016_fiscal-impact-calculations.xlsx` worksheet`Assumps&Panel A Calcs!A93`
