---
title: "Dynamic Document for Fiscal Impacts of Deworming"
date: "23 October, 2019"
output:
  html_document:
    code_folding: hide
    code_download: true
    collapsed: yes
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    toc: yes
    toc_depth: 2
    toc_float: yes
  word_document: default
  pdf_document: default
editor_options:
  chunk_output_type: console
---

\def\blue{\color{blue}}




```r
rm(list = ls())
options(tinytex.verbose = TRUE)
```







```r
# TO-DO:
# add source links to bonds and inflation
# update deworming cost - DONE
# get rid of gamma in the function
# explain K better (and others?) in documentation - DONE
# put all costs in PPP units - DONE
# edit methodology section

# TIME-VARIANT INPUTS (may need to be updated)

  # Data  
    gov_bonds_so <- 0.09                   # Interest rate on government bonds - trading economics (updated 10/14) (add links)
    inflation_so <- 0.04                   # Kenyan inflation rate - World Bank Development Indicators (updated 10/14) (add links)
    tax_so       <- 0.16575                # ADD INFO!
    unit_cost_so <- 0.42                   # Unit cost of deworming (in 2018 USD) - from Evidence Action
    
  # Research
    lambda1_2017usdppp_so <- c(87, 83, 81) # Average treatment effect on earnings in each KLPS round (5 year period), already adjusted for PPP (and inflation?) - W@W
    
  # Guess work   
    periods_so    <- 50                    #Total number of periods to forecast wages
    time_to_jm_so <- 10                    #Time from intial period until individual join the labor force
    teach_sal_so  <- 5041                  #Yearly secondary schooling compensation	(assuming in 2007 dollars) - from ROI materials
    teach_ben_so  <- 217.47                #Yearly secondary schooling teacher benefits	(assuming in 2007 dollars)
    n_students_so <- 45                    #Average pupils per teacher	45

# TIME-INVARIANT INPUTS
  
  # Data 
    years_of_treat_so <- 2.41       # Additional Years of Treatment - Table 1, Panel A
    
    ex_rate_2018        <-101.30    # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2007        <- 67.318   # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.245   # PPP exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=KE
    ex_rate_2007_ppp_so <- 23.514   # PPP exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=KE
    
    cpi_2007_so <- 207.342          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    
  # Research
    q_full_so   <- 0.75                     #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so   <- 0                        #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451,  # (Delta E) Additional direct seconday schooling increase (from Joan)
                      0.00696052250263997, 
                      0.0258570306763183,     
                      0.0239963665555466,  
                      0.027301406306074,   
                      0.0234125454594173,
                      0.0279278879439199,  
                      0.00647044449446303, 
                      0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)

# CALCULATIONS TO CONVERT ALL CURRENCY TO 2017 USD PPP (will need to be updated if monetary inputs are updated)
    
  # Adjust for currency: convert all costs to USD PPP **NOTE: 1 international dollar = 1 USD (https://data.worldbank.org/indicator/PA.NUS.PRVT.PP?locations=KE-US)***
      
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so
    teach_sal_ppp_so <- teach_sal_so*ex_rate_2007/ex_rate_2007_ppp_so
    teach_ben_ppp_so <- teach_ben_so*ex_rate_2007/ex_rate_2007_ppp_so
    
  # Adjust for inflation: convert all costs to 2017 USD
    
    unit_cost_2017usdppp_so <- unit_cost_ppp_so*cpi_2017_so/cpi_2018_so
    teach_sal_2017usdppp_so <- teach_sal_ppp_so*cpi_2017_so/cpi_2007_so
    teach_ben_2017usdppp_so <- teach_ben_ppp_so*cpi_2017_so/cpi_2007_so      

#############
##### Notes:
#############
### Source ---->  Input ----> Model ----> Policy Estimates (output)
###  (_so)        (_in)       (_mo)        (_pe)
### values      functions   functions      values
###             & values    & values 
### arguments in functions should used "_var" and functions should "_f"
```

# Methodology
 
The target parameter to reproduce corresponds to the NPV of deworming, including spillovers, and can be found in the file `Baird-etal-QJE-2016_fiscal-impact-calculations-UPDATED-KLPS-3_2018-01-04.xlsx`, sheet, `Calcs-Table 5`, cell `C51`. CHECK IF THIS IS CORRECT.

## Main Equation (the model)

\begin{equation}
NPV =  \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\label{eq:1}
\tag{1}
\end{equation}



```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
npv_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_earnings_var = delta_earnings_p_in,
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^index_t * delta_earnings_var
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )
          ) - (s2_var * q2_var  - s1_var * q1_var)*years_of_treat_var
############################################################################### 
  return(res1) 
}
```

## Sub components:

### "$\tau$"

\begin{equation}
NPV =  \left[
\blue{\tau} \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

The annual tax rate $\tau$ is estimated to be 16.6%. It is calcuated as the product of "government expenditures" and "percent non-donor financed" according to `Baird-etal-QJE-2016_fiscal-impact-calculations-UPDATED-KLPS-3_2018-01-04.xlsx`, sheet, `Assumps&Panel A Calcs`.
**NOTE** I don't understand how this is calculated.

### "$r$"

\begin{equation}
NPV =  \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + \blue{r}}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + \blue{r}}\right)^{t} \Delta \overline{E}_t(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

The real interest rate $r$ is obtained from the interest rate on goverment bonds (0.09) minus the inflation rate (0.04).


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
interest_in_f <- function(gov_bonds_var = gov_bonds_so , inflation_var = inflation_so) {  
############################################################################### 
  interest_in = gov_bonds_var - inflation_var 
############################################################################### 
  return(list("interest_in" = interest_in))
}
invisible( list2env(interest_in_f(),.GlobalEnv) )
```

The resulting value is a $r$ = 5%

### "$\Delta Y_t$"

\begin{equation}
NPV =  \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \blue{\Delta Y_t} -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

$\Delta Y_t$ represents the treatment effect on earnings, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc. 

We estimate treatment effects on total earnings by round. KLPS2 captures effects after 10 years; KLPS3 captures the effects after 15 years; and KLPS4 after 20 years. We will need to make assumptions about earnings gains from deworming after 20 years.

#### Assumption: persistent earnings

If we assume that the effect on earnings identified 20 years after the intervention persists through one's working life, we have 

\begin{equation}
\Delta Y_t = \mathbf{1}(10 < t \leq 15)\lambda_1^{KLPS2} + \mathbf{1}(15 < t \leq 20)\lambda_1^{KLPS3} + \mathbf{1}(t > 20)\lambda_1^{KLPS4}
\text{ for } t \leq 50
\end{equation}


```r
# - inputs: periods_so, lambda1_2017usdppp_so
# - outputs: 
delta_earnings_p_in_f <- function(t_var = 0:periods_so, 
                                  lambda1k1_var = lambda1_2017usdppp_so[1], 
                                  lambda1k2_var = lambda1_2017usdppp_so[2], 
                                  lambda1k3_var = lambda1_2017usdppp_so[3]) {
############################################################################### 
delta_earnings_p_in = 1*(10 <= t_var & t_var < 15) * lambda1k1_var + 
                      1*(15 <= t_var & t_var < 20) * lambda1k2_var + 
                      1*(20 <= t_var) * lambda1k3_var
############################################################################### 
  return(list("delta_earnings_p_in" = delta_earnings_p_in))
}
invisible(list2env(delta_earnings_p_in_f(),.GlobalEnv) )
```

#### Assumption: Non-persistent earnings.

However, if we assume earnings gains persist for 5 years after KLPS4 (matching the time period between previous rounds), then disappear, we have

\begin{equation}
\Delta Y_t = \mathbf{1}(10 < t \leq 15)\lambda_1^{KLPS2} + \mathbf{1}(15 < t \leq 20)\lambda_1^{KLPS3} + \mathbf{1}(20 < t \leq 25)\lambda_1^{KLPS4}
\text{ for } t \leq 50
\end{equation}


```r
# - inputs: periods_so, lambda1_2017usdppp_so
# - outputs: 
delta_earnings_in_f <- function(t_var = 0:periods_so, 
                                lambda1k1_var = lambda1_2017usdppp_so[1], 
                                lambda1k2_var = lambda1_2017usdppp_so[2], 
                                lambda1k3_var = lambda1_2017usdppp_so[3]) {
############################################################################### 
delta_earnings_in = 1*(10 <= t_var & t_var < 15) * lambda1k1_var + 
                    1*(15 <= t_var & t_var < 20) * lambda1k2_var + 
                    1*(20 <= t_var & t_var < 25) * lambda1k3_var
############################################################################### 
  return(list("delta_earnings_in" = delta_earnings_in))
}
invisible( list2env(delta_earnings_in_f(),.GlobalEnv) )
```

Note that both expressions assume that there are no additional earnings gains for the treatment group for the first 10 years post-intervention. This model also disregards externality effects.

### $K$ and $\Delta \overline{E}_t(S1,S2)$ 

\begin{equation}
NPV =  \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
\blue{K} \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \blue{\Delta \overline{E}_t(S1,S2)}
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

$K$ represents the cost per student. This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}
\end{equation}

Teacher salary is estimated by the average salary of teachers in Kenya ($1.7061\times 10^{4}). Likewise, teacher benefits are estimated by the average benefits of teachers in Kenya ($736). Each of these have been adjusted for inflation. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per school is (45).

**NOTE** I confirmed average teacher salary and average number of students based on a google search, but am unable to find info on teacher benefits.

For $\Delta \overline{E}(S1,S2)$ we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from [need to define the source "from Joan" in `Assumps&Panel A Calcs!A93`].

This series does not take into account the externality effects. To incorporate the we need another series (same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


```r
# - inputs: coverage_so, q_full_so, q_zero_so 
# - outputs: saturation_in 
ed_costs_in_f <- function(teach_sal_var = teach_sal_2017usdppp_so, teach_ben_var = teach_ben_2017usdppp_so, 
                          n_students_var = n_students_so,
                          delta_ed_var = delta_ed_so[,1]){
 ###############################################################################    
    cost_per_student_in <- (teach_sal_var + teach_ben_var)/ n_students_var
    delta_ed_in <- delta_ed_var
############################################################################### 
    return(list("cost_per_student_in" = cost_per_student_in, "delta_ed_in" = delta_ed_in)) 
} 
invisible( list2env(ed_costs_in_f(),.GlobalEnv) )
```

**NOTE** need to understand better the date of each component (of the model, not only this section).

### $\left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)$

\begin{equation}
NPV =  \left[
\tau \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\right] - \blue{ \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) }
\end{equation}

#### $S_{1}Q(S_{1}) = 0$
There is no subsidy for deworming under the status quo.   


#### $S_{2}$: complete subsidy to per capita costs of deworming.

With complete subsidy, $S_2$ represents the total direct costs of deworming in USD. Calculated as follows

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\
\end{equation}

#### $Q_{2}$
The take-up with full subsidy ($Q_2$) comes from a previous study (Miguel and Kremer 2007) and takes the value of 0.75.


```r
# - inputs: 
# - outputs: 
costs_f <- function(unit_cost_var = unit_cost_2017usdppp_so,
                    years_of_treat_var = years_of_treat_so, 
                    q_full_var = q_full_so){
############################################################################### 
    s2_in <- (unit_cost_var)*years_of_treat_var
    q2_in <- q_full_var
############################################################################### 
    return(list("s2_in" = s2_in, "q2_in" = q2_in)) 
} 
invisible( list2env(costs_f(),.GlobalEnv) )
```

# Results and robustness


```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
npv_clambda_p_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                lambda1_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^index_t * (1*(10 <= index_t & index_t < 15) * lambda1_var + 1*(15 <= index_t & index_t < 20) * lambda1_var +1*(20 <= index_t) * lambda1_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )
          ) - (s2_var * q2_var  - s1_var * q1_var)*years_of_treat_var
############################################################################### 
  return(res1) 
}
```


```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
npv_clambda_d_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                lambda1_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^index_t * (1*(10 <= index_t & index_t < 15) * lambda1_var + 1*(15 <= index_t & index_t < 20) * lambda1_var + 1*(20 <= index_t & index_t < 25) * lambda1_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )
          ) - (s2_var * q2_var  - s1_var * q1_var)*years_of_treat_var
############################################################################### 
  return(res1) 
}
```


```r
#########
# PANEL A
#########

l1_social_persist_int10 <- (multiroot(function(x) npv_clambda_p_mo_f(lambda1_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=100))$root
l1_social_persist_int05 <- (multiroot(function(x) npv_clambda_p_mo_f(lambda1_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=100))$root
l1_social_die_int10     <- (multiroot(function(x) npv_clambda_d_mo_f(lambda1_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=100))$root
l1_social_die_int05     <- (multiroot(function(x) npv_clambda_d_mo_f(lambda1_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=100))$root

l1_tax_persist_int10 <- (multiroot(function(x) npv_clambda_p_mo_f(lambda1_var = x, interest_r_var = 0.10), 4, maxiter=100))$root
l1_tax_persist_int05 <- (multiroot(function(x) npv_clambda_p_mo_f(lambda1_var = x, interest_r_var = 0.05), 4, maxiter=100))$root
l1_tax_die_int10     <- (multiroot(function(x) npv_clambda_d_mo_f(lambda1_var = x, interest_r_var = 0.10), 4, maxiter=100))$root
l1_tax_die_int05     <- (multiroot(function(x) npv_clambda_d_mo_f(lambda1_var = x, interest_r_var = 0.05), 4, maxiter=100))$root

#########
# PANEL B
#########

irr_social_persist <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1), .1, maxiter=100))$root
irr_social_die     <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_earnings_var = delta_earnings_in), .1, maxiter=100))$root
irr_tax_persist    <- (multiroot(function(x) npv_mo_f(interest_r_var = x), .1, maxiter=100))$root
irr_tax_die        <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_earnings_var = delta_earnings_in), .1, maxiter=100))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_int05_persist <- npv_mo_f(tax_var = 1)
npv_int05_die     <- npv_mo_f(delta_earnings_var = delta_earnings_in, tax_var = 1)
npv_int10_persist <- npv_mo_f(interest_r_var = 0.10, tax_var = 1)
npv_int10_die     <- npv_mo_f(interest_r_var = 0.10, delta_earnings_var = delta_earnings_in, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_int05_persist <- npv_mo_f()
tax_int05_die     <- npv_mo_f(delta_earnings_var = delta_earnings_in)
tax_int10_persist <- npv_mo_f(interest_r_var = 0.10)
tax_int10_die     <- npv_mo_f(interest_r_var = 0.10, delta_earnings_var = delta_earnings_in)
```

|Real annualized interest rate (r)|Treatment effect timeframe|Net Present Value (2017 USD PPP)    |Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|------------------:|------------------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
|                                                                                                                                                                                                  
|                                 |50 years           |0                                          |                                                |10%                                     |**9.53**  |
|                                 |50 years           |0                                          |                                                |5%                    |**4.41**  |
|                                 |25 years           |0                                          |                                                |10%                                     |**12.28**      |
|                                 |25 years           |0                                          |                                                |5%                    |**7.35**      |
|                                 |50 years           |                                           |0                                               |10%                                     |**57.49**     |
|                                 |50 years           |                                           |0                                               |5%                    |**26.6**     |
|                                 |25 years           |                                           |0                                               |10%                                     |**74.07**         |
|                                 |25 years           |                                           |0                                               |5%                    |**44.33**         |
|                                                                                                                                                                                                   
|                                 |50 years           |0                                          |                                                |**34.3%**|*                                         |
|                                 |50 years           |                                           | 0                                              |**13.1%**   |*                                         |
|                                 |25 years           |0                                          |                                                |**34.1%**    |*                                         |
|                                 |25 years           |                                           | 0                                              |**11.4%**       |*                                         |
|
| 10%                             |50 years           |**309**         |**18**              |                                        |*                                         |
|5%             |50 years           |**875**         |**104**              |                                        |*                                         |
| 10%                             |25 years           |**233**             |**6**                  |                                        |*                                         |
|5%             |25 years           |**514**             |**44**                  |                                        |*                                         |**Notes.** The Net Present Value takes three factors into account: (tax on) earnings gains (a result of additional schooling), the cost of additional schooling, and the cost of deworming medication.

The earnings gains observed 10, 15, and 20 years from the intervention are estimated to be 87, 83, 81 dollars per person per year respectively. We assume there are no earnings gains in the first 10 years after receiving deworming medication, and earnings gains persist through the end of one's working life (50 years after receiving treatment) or die out after the last observed five-year period (25 years after receiving treatment). The annual tax on earnings is assumed to be 16.6%.

The cost of additional schooling is given by the product of the cost of schooling each child and the additional years of schooling. The cost of schooling each child for an additional year ($395.5) is calculated by dividing the sum of annual teacher salary ($1.706133\times 10^{4}) and teacher benefits ($736) by the number of average number of students per teacher (45). And on average, treated individuals had 0.02 additional years of schooling.

Both earnings gains and the cost of additional schooling are discounted by the real interest rate, where we consider two: 5% and 10% to look at effects over a range of values. These values (calculated as goverment bonds minus inflation) correspond with the second quartile and median interest rates between 1998 and 2018. 

The cost of deworming one person for one year is $0.83 and the treatment was administered for an average of 2.41 years.

All three values are given in PPP units and are adjusted for inflation.

Panel A gives the minimum average earnings gains required to achieve a postive NPV under different assumptions of the treatment timeframe and the internal rate of return. Panel B gives the social and government internal rates of return for each assumption of the treatment effect timeframe given the observed earnings gains. Panel C gives the social and government Net Present Values for each interest rate and each treatment timeframe given the observed earnings gains.

\* The average observed earnings gains is 83.67.

## Additional notes for replication

### The `multiroot` function

The earnings gains in panel A and the internal rates of return in panel B are caculated using the `multiroot` function which solves for $n$ roots of $n$ (nonlinear) equations. An input to this function `start` is a scalar containing an initial guess for the unknown value.

The values in panel A are robust to all `start` values trued thus far (between 0 and 4). The values in panel B are robust to `start` values between 0 and .5; with a `start` value of .6, the internal rates of return for the tax NPV become hugely negative, and for a `start` value of .8 the internal rates of return for the social NPV also become hugely negative. **NOTE** update this.

