---
title: "Dynamic Document for Fiscal Impacts of Deworming"
date: "15 October, 2019"
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
# To do:
# 1 - Re-write w_t with w_0 outside.                DONE
# 2 - use app functions for sims in DD              DONE 
# 3 - run app based on DD                           DONE
# 4 - add starting values from script into app      DONE
# 5 - deploy                                        DONE
# - scale lambda 1 to infection rates
# - add small output after each section
# - ...

call_params_f <- function(){
    #############
    ##### Data  
    #############
   #gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    gov_bonds_so <- 0.09           # Updated on 10/14/19 - trading economics (add links)
   #inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    inflation_so <-  0.04          #Kenyan inflation rate - World Bank Development Indicators (updated 10/14) (add links)
    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B
    profits_se_so <- 1766          #Control group monthly self-employed profits - Table 4, Panel A  FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 4, Panel D
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 4, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 4, Panel A
    ex_rate_so <- 85               #Exchange Rate - Central Bank of Kenya
    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    tax_so <- 0.16575              #ADD INFO!
    unit_cost_local_so <- 43.66    #Deworm the World
    years_of_treat_so <- 2.41      #Additional Years of Treatment - Table 1, Panel A
    #############
    ##### Research
    #############    
    #lambda1_so <- c(3.49, 0)       #Hrs per week increase for men and women CONFIRM
    #lambda2_so <- 10.2             #Externality effect (proportional) - Table 3, Panel B
    lambda1_so <- c(87, 83, 81)
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct seconday schooling increase (from Joan)
                        0.0239963665555466, 0.027301406306074, 0.0234125454594173,
                       0.0279278879439199, 0.00647044449446303, 0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)
    delta_ed_ext_so <- c(-0.0110126908021048,	0.0140448546741008,	-0.0034636291545585,  #Additional externality secondary schooling increase (from Joan)
                           0.0112940214439477,	0.0571608179771775,	-0.0560546793186931,
                           0.0558284756343451,	0.1546264843901160,	0.0055961489945619)
    delta_ed_ext_so <- cbind(delta_ed_ext_so, 1999:2007)    
    include_ext_so <- TRUE
    
    #############
    ##### Guess work   
    #############
    periods_so <- 50               #Total number of periods to forecast wages
    time_to_jm_so <- 10            #Time from intial period until individual join the labor force
    coef_exp_so <- c(0, 0)         #Years of experience coefficients (1-linear, 2-cuadratic)	- see notes
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    n_students_so <- 45            #Average pupils per teacher	45
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) ) 
}
invisible( list2env(call_params_f(),.GlobalEnv) )


#############
##### Notes:
#############
### Source ---->  Input ----> Model ----> Policy Estimates (output)
###  (_so)        (_in)       (_mo)        (_pe)
### values      functions   functions      values
###             & values    & values 
### arguments in functions should used "_var" and functions should "_f"

#invisible( list2env(call_params_f(),.GlobalEnv) )

# on growth_rate_so: (http://data.worldbank.org/indicator/NY.GDP.PCAP.KD/), see calculation on "Kenya GDP per capita" tab. In W@W this equals 1.52%. ISSUE: This growth number should be updated to be 2002-2014, I think.
# on coef_exp_so: 1998/1999 Kenyan labor force survey; regression of earnings on age, age^2, female dummy, indicators for attained primary/secondary/beyond, and province dummies. Estimate used in W@W: (0.1019575, -0.0010413). ISSUE: For now assume no further life cycle adjustment beyond KLPS-3 (likely a conservative assumption).
# coverage_so: Overall Saturation (0.511) / 0.75 - not reported in table, average of T & C
```

# Methodology
 
The target parameter to reproduce corresponds to the NPV of deworming, including spillovers, and can be found in the file `Baird-etal-QJE-2016_fiscal-impact-calculations-UPDATED-KLPS-3_2018-01-04.xlsx`, sheet, `Calcs-Table 5`, cell `C51`. CHECK IF THIS IS CORRECT.

## Main Equation (the model)

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\label{eq:1}
\tag{1}
\end{equation}



```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
# Gamma is used to index gender.
npv_mo_f <- function(interest_r_var,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_earnings_var = delta_earnings_p_in,
                wage_var = wage_t_mo,
                lambda1_male_var = lambda1_so[1],
                lambda1_female_var = lambda1_so[2], 
                tax_var = tax_so,
                saturation_var = saturation_in,             
                coverage_var = coverage_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so) {
  ns <- c(n_male_var, n_female_var)
  index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )

  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^index_t * delta_earnings_var
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )
          ) - (s2_var * q2_var  - s1_var * q1_var)
  return(res1) 
}
```

## Sub components:

### "$\gamma$"

\begin{equation}
NPV =  \sum_{\blue{\gamma}} N_{\blue{\gamma}} \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

Gamma is simply an indicator for gender which takes on two values, corresponding for male and female.

### "$\tau$"

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\blue{\tau} \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

### "$r$"

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + \blue{r}}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + \blue{r}}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

The real interest rate $r$ is obtained from the interest rate on goverment bonds (0.09) minus the inflation rate (0.04).


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
interest_in_f <- function(gov_bonds_var = gov_bonds_so , inflation_var = inflation_so) {  
  interest_in = gov_bonds_var - inflation_var 
  return(list("interest_in" = interest_in))
}
invisible( list2env(interest_in_f(),.GlobalEnv) )
```

The resulting value is a $r$ = 5%

### "$\Delta Y_t$"

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \blue{\Delta Y_t} -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
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
# - inputs: periods_so, lambda1_so
# - outputs: 
delta_earnings_p_in_f <- function(t_var = 0:periods_so, 
                             lambda1k1_var = lambda1_so[1], 
                             lambda1k2_var = lambda1_so[2], 
                             lambda1k3_var = lambda1_so[3]) {
############################################################################### 
delta_earnings_p_in = 1*(10 <= t_var & t_var < 15) * lambda1k1_var + 
                      1*(15 <= t_var & t_var < 20) * lambda1k2_var + 
                      1*(20 <= t_var) * lambda1k3_var
############################################################################### 
  return(list("delta_earnings_p_in" = delta_earnings_p_in))
}
invisible(list2env(delta_earnings_p_in_f(),.GlobalEnv) )
#delta_earnings_p_in <- delta_earnings_p_in_f()
```

#### Assumption: Non-persistent earnings.

However, if we assume earnings gains persist for 5 years after KLPS4 (matching the time period between previous rounds), then disappear, we have

\begin{equation}
\Delta Y_t = \mathbf{1}(10 < t \leq 15)\lambda_1^{KLPS2} + \mathbf{1}(15 < t \leq 20)\lambda_1^{KLPS3} + \mathbf{1}(20 < t \leq 25)\lambda_1^{KLPS4}
\text{ for } t \leq 50
\end{equation}


```r
# - inputs: periods_so, lambda1_so
# - outputs: 
delta_earnings_in_f <- function(t_var = 0:periods_so, 
                                lambda1k1_var = lambda1_so[1], 
                                lambda1k2_var = lambda1_so[2], 
                                lambda1k3_var = lambda1_so[3]) {
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

### $K$ and $\Delta \overline{E}_{\gamma t}(S1,S2)$ 

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta Y_t -
\blue{K} \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \blue{\Delta \overline{E}_{\gamma t}(S1,S2)}
\right] - \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}

$K$ represents the cost per student. This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}
\end{equation}

For $\Delta \overline{E}_{\gamma t}(S1,S2)$ we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from [need to define the source "from Joan" in `Assumps&Panel A Calcs!A93`].

This series does not take into account the externality effects. To incorporate the we need another series (same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


```r
include_ext_mo <- TRUE
# - inputs: coverage_so, q_full_so, q_zero_so 
# - outputs: saturation_in 
ed_costs_in_f <- function(teach_sal_var = teach_sal_so, teach_ben_var = teach_ben_so, 
                          n_students_var = n_students_so, delta_ed_ext_var = delta_ed_ext_so,
                          delta_ed_var = delta_ed_so, include_ext_var = include_ext_mo){
    
    cost_per_student_in <- (teach_sal_var + teach_ben_var) / n_students_var
    
    # Nothing here yet with delta_ed_vals, but would like to incorporate model from Joan
    delta_ed_ext_total_in <- delta_ed_ext_var[,1] + delta_ed_var[,1]
    
    if (include_ext_var == TRUE){
      delta_ed_final_in <-  delta_ed_ext_total_in
    }else{
      delta_ed_final_in <- delta_ed_var[,1]
    }
    return(list("cost_per_student_in" = cost_per_student_in, "delta_ed_final_in" = 
                  delta_ed_final_in,  "delta_ed_ext_total_in" = delta_ed_ext_total_in)) 
} 
invisible( list2env(ed_costs_in_f(),.GlobalEnv) )
```

**Note:** need to understand better the date of each component (of the model, not only this section).

### $\left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)$

\begin{equation}
NPV =  \sum_{\gamma} N_{\gamma} \left[
\tau \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} \Delta Y_t -
K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2)
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
costs_f <- function(unit_cost_local_var = unit_cost_local_so, ex_rate_var = ex_rate_so,
                    years_of_treat_var = years_of_treat_so, q_full_var = q_full_so){
    s2_in <- ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    q2_in <- q_full_var
    return(list("s2_in" = s2_in, "q2_in" = q2_in)) 
} 
invisible( list2env(costs_f(),.GlobalEnv) )
```

# Results and robustness

## Disregarding externalities


```r
#########
# PANEL A
#########


#########
# PANEL B
#########

irr_social_persist <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1), 0.4, maxiter=100))$root
irr_social_die     <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_earnings_var = delta_earnings_in), 0.4, maxiter=100))$root
irr_tax_persist    <- (multiroot(function(x) npv_mo_f(interest_r_var = x), 0.4, maxiter=100))$root
irr_tax_die        <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_earnings_var = delta_earnings_in), 0.4, maxiter=100))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_realint_persist <- npv_mo_f(interest_r_var=interest_in, tax_var = 1)
npv_realint_die     <- npv_mo_f(interest_r_var=interest_in, delta_earnings_var = delta_earnings_in, tax_var = 1)
npv_int10_persist   <- npv_mo_f(interest_r_var = 0.10, tax_var = 1)
npv_int10_die       <- npv_mo_f(interest_r_var = 0.10, delta_earnings_var = delta_earnings_in, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_realint_persist <- npv_mo_f(interest_r_var = interest_in)
tax_realint_die     <- npv_mo_f(interest_r_var=interest_in, delta_earnings_var = delta_earnings_in)
tax_int10_persist   <- npv_mo_f(interest_r_var = 0.10)
tax_int10_die       <- npv_mo_f(interest_r_var = 0.10, delta_earnings_var = delta_earnings_in)

# ASK FERNANDO HOW TO CODE THIS PROPERLY: AND DON'T HARDCODE IN THE TABLE

#r_4 <- multiroot(npv_mo_f, 0.4, maxiter=100)
#r_4 <- r_4$root
#r_5 <- multiroot(npv_mo_f, 0.4, maxiter=100, delta_earnings_var = delta_earnings_in)
#r_5 <- r_5$root
```

|        |Real annualized interest rate (r)|Treatment timeframe | Net Present Value (2017 USD PPP)   | Net Present Value of tax revenue (2017 USD PPP)| IRR (annualized)| Avg earnings gains ($\lambda_1$) |
|--------|--------------------------------:|-------------------:|-----------------------------------:|-----------------------------------------------:|----------------:|---------------------------------:|
|PANEL A                                                                                                                                                                                                   
|        |                                 |40 years            |0                                   |                                                |10%              |**X**                             |
|        |                                 |40 years            |0                                   |                                                |5%               |**X**                             |
|        |                                 |25 years            |0                                   |                                                |10%              |**X**                             |
|        |                                 |25 years            |0                                   |                                                |5%               |**X**                             |
|        |                                 |40 years            |                                    |0                                               |10%              |**X**                             |
|        |                                 |40 years            |                                    |0                                               |5%               |**X**                             |
|        |                                 |25 years            |                                    |0                                               |10%              |**X**                             |
|        |                                 |25 years            |                                    |0                                               |5%               |**X**                             |
|PANEL B                                                                                                                                                                                                   
|        |                                 |40 years            |0                                   |                                                |**0.5**        |83.67    |
|        |                                 |40 years            |                                    | 0                                              |**26.7%**        |83.67    |
|        |                                 |25 years            |0                                   |                                                |**54.2%**        |83.67    |
|        |                                 |25 years            |                                    | 0                                              |**26.3%**        |83.67    |
|PANEL C                                                                                                                                                                                           |
|        | 10%                             |40 years            |**337**  |**46**              |                 |83.67    |
|        |5%             |40 years            |**910**|**139**            |                 |83.67    |
|        | 10%                             |25 years            |**261**      |**34**                  |                 |83.67    |
|        |5%             |25 years            |**549**    |**79**                |                 |83.67    |
Note:
