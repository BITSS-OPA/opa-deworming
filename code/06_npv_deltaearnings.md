---
title: "Dynamic Document for Fiscal Impacts of Deworming"
date: "13 January, 2021"
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
# get rid of gamma in the function
# edit methodology section
# get rid of .02. take sum of vector instead of mean and refer to Baird et. al.
# change code of equations to match new setup
# insert figures
# format table

# TIME-VARIANT INPUTS (may need to be updated)

  # Data  
    gov_bonds_so <- 0.09                   # Interest rate on government bonds - trading economics (updated 10/14) (add links)
    inflation_so <- 0.04                   # Kenyan inflation rate - World Bank Development Indicators (updated 10/14) (add links)
    tax_so       <- 0.16575                # ADD INFO!
    unit_cost_so <- 0.42                   # Unit cost of deworming (in 2018 USD) - from Evidence Action
  # unit_cost_local_so <- 43.66            #Deworm the World
    
  # Research
    lambda1_2017usdppp_so <- c(86.54642,   # avg treatment effect from klps2 (already adjusted for ppp and inflation) - w@w
                               82.99311,   # avg treatment effect from klps3 (already adjusted for ppp and inflation) - w@w 
                               85.44088)   # avg treatment effect from klps4 (already adjusted for ppp and inflation) - w@w
    consump_2017usdppp_so <- c(0,          # avg treatment effect from klps2 not collected (treat as if zero)
                               1010.9890,  # avg treatment effect from klps3 (already adjusted for ppp and inflation) - w@w 
                               201.0693)   # avg treatment effect from klps4 (already adjusted for ppp and inflation) - w@w
    wtp_so <- 50*12                        # willingness to pay for deworming medication (2018 KSH)- Health-WTP_2018-01-19.pdf
    
  # Guess work   
    periods_so    <- 50                    #Total number of periods to forecast wages
    time_to_jm_so <- 10                    #Time from intial period until individual join the labor force
    teach_sal_so  <- 50000                 #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits - news sources
                                              # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                              # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    teach_sal_so <- 12*teach_sal_so        #Yearly secondary schooling compensation
    n_students_so <- 45                    #Average pupils per teacher	45

# TIME-INVARIANT INPUTS
  
  # Data 
    years_of_treat_so <- 2.41       # Additional Years of Treatment - Table 1, Panel A
    
    ex_rate_2018        <-101.30    # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2009        <- 77.352   # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2007        <- 67.318   # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2009_ppp_so <- 31.317   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2007_ppp_so <- 25.024   # KLPS4_E+_globals.do (originally from the World Bank)
    
    cpi_2007_so <- 207.342          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2009_so <- 214.537          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
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
    teach_sal_ppp_so <- teach_sal_so/ex_rate_2017_ppp_so
    wtp_so           <- wtp_so/ex_rate_2018_ppp_so
    
  # Adjust for inflation: convert all costs to 2017 USD
    
    unit_cost_2017usdppp_so <- unit_cost_ppp_so*cpi_2017_so/cpi_2018_so
    teach_sal_2017usdppp_so <- teach_sal_ppp_so*cpi_2017_so/cpi_2017_so # redundant, but for the sake of consistency
    wtp_2017usdppp_so       <- wtp_so*cpi_2017_so/cpi_2018_so

#############
##### Notes:
#############
### Source ---->  Input ----> Model ----> Policy Estimates (output)
###  (_so)        (_in)       (_mo)        (_pe)
### values      functions   functions      values
###             & values    & values 
### arguments in functions should used "_var" and functions should "_f"
```
 
The target parameter to reproduce corresponds to the NPV of deworming, and can be found in the file `Baird-etal-QJE-2016_fiscal-impact-calculations-UPDATED-KLPS-3_2018-01-04.xlsx`, sheet, `Calcs-Table 5`, cell `C51`. CHECK IF THIS IS CORRECT.

# The model

The net present value takes into account the short-term cost of deworming, the medium-term cost of schooling since deworming increases education, putting pressure on schooling instutions, and the labor market gains, which are realized in the long-term. The compenents of each term of the model are discussed below. Note that this model disregards short term health gains, which are considered in the robustness section.

\begin{equation}
NPV =
-\underbrace{\left[\sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{t,2}Q(S_{t,2}) - S_{t,1}Q(S_{t,1}) \big]\right]}_{\text{cost of deworming medication}}
- \underbrace{\left[K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)\right]}_{\text{cost of schooling}}
+ \underbrace{\left[\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta W_t \right]}_{\text{labor market gains}}
\label{eq:1}
\tag{1}
\end{equation}


```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
npv_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * delta_welfare_var
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
          apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )) - 
          sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}
```


### Discount factor (r)

Each term in the model is discounted by $r$, which is set to the real interest rate [**rationale?**], obtained from the interest rate on goverment bonds (0.09) minus the inflation rate (0.04).


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

The resulting value is $r$ = 5%.

## Cost of deworming medication

The costs of deworming medication is obtained by the sum of discounted costs of deworming over the treatment period. The average treatment period in this study was 2.41 years, which we round to 2.4. So starting year zero, we have

\begin{equation}
\sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]
\end{equation}

Since the analysis is discrete, and we can not sum over a non-integer, we find
\begin{equation}
\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + .4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]
\end{equation}

### Current subsidy for deworming ($S_{1}Q(S_{1})$)
Since there is no subsidy for deworming under the status quo, we have $S_{1}Q(S_{1}) =0$.


### Complete subsidy for dewormng ($S_2Q(S_2)$)

With complete subsidy, $S_2$ represents the total direct costs of deworming each child in USD. Most recent (2018) data from Evidence Action reveals this cost to be $0.42. Adjusting for purchasing power and inflation, we get a per capita cost of $0.83.

The take-up with full subsidy ($Q_2$) comes from a previous study (Miguel and Kremer 2007) and takes the value of 0.75.


```r
# - inputs: 
# - outputs: 
costs_f <- function(unit_cost_var = unit_cost_2017usdppp_so,
                    years_of_treat_var = years_of_treat_so, 
                    q_full_var = q_full_so){
############################################################################### 
    s2_in <- c(rep(unit_cost_var,2), .4*unit_cost_var)
    q2_in <- q_full_var
############################################################################### 
    return(list("s2_in" = s2_in, "q2_in" = q2_in)) 
} 
invisible( list2env(costs_f(),.GlobalEnv) )
```

**So we get an average cost of deworming each child over the entire treatment period, $1.44.**

## Cost of schooling

We account for the cost of schooling since deworming medication increases school attendance and may put pressure on educational institutions. Schooling costs are given by the discounted sum of the additional cost of education per child as a result of deworming.

The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school as a result of deworming. Assuming pressure is added to educational institutions for a maximum of nine years, starting at year zero, we have

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\end{equation}

### Cost per student ($K$)

$K$ represents the cost of schooling each child for an additional year ($267.88). It is calculated by dividing an estimate of annual teacher salary by the number of average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary}}{\text{number students}}
\end{equation}

Annual teacher salary ($12055) is based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FILL IN]**.

### Additional years of education ($\Delta \overline{E}_t(S1,S2)$)

For $\Delta \overline{E}(S1,S2)$ we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from [need to define the source "from Joan" in `Assumps&Panel A Calcs!A93`].

This series does not take into account the externality effects. To incorporate the we need another series (same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


```r
# - inputs: coverage_so, q_full_so, q_zero_so 
# - outputs: saturation_in 
ed_costs_in_f <- function(teach_sal_var = teach_sal_2017usdppp_so, 
                          n_students_var = n_students_so,
                          delta_ed_var = delta_ed_so[,1]){
 ###############################################################################    
    cost_per_student_in <- (teach_sal_var)/ n_students_var
    delta_ed_in <- delta_ed_var
############################################################################### 
    return(list("cost_per_student_in" = cost_per_student_in, "delta_ed_in" = delta_ed_in)) 
} 
invisible( list2env(ed_costs_in_f(),.GlobalEnv) )
```

Over this nine year period, students attended school for an additional 0.15 years on average.

**Then we get an average cost of additional schooling per child over the nine-year period, $32.40.**

## Labor market gains

Labor market gains are given by the tax on the discounted sum of welfare gains from deworming.

\begin{equation}
\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta W_t
\end{equation}

### Tax ($\tau$)

The annual tax rate $\tau$ is estimated to be 16.6%. It is calcuated as the product of "government expenditures" and "percent non-donor financed" according to `Baird-etal-QJE-2016_fiscal-impact-calculations-UPDATED-KLPS-3_2018-01-04.xlsx`, sheet, `Assumps&Panel A Calcs`.
**NOTE** I don't understand how this is calculated.

To find the social gains, we set $\tau$ equal to 1.

### Welfare gains ($\Delta W_t$)

$\Delta W_t$ represents the treatment effect on welfare, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc.

We estimate treatment effects on total welfare by round. KLPS2 captures effects after 10 years; KLPS3 captures the effects after 15 years; and KLPS4 after 20 years. We will need to make assumptions about welfare gains from deworming after 20 years

#### Treatment effect timeframe

If we assume welfare gains persist for 5 years after KLPS4 (matching the time period between previous rounds), then disappear, we have

\begin{equation}
\Delta W_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(20 < t \leq 25)\alpha^{KLPS4}
\text{ for } t \leq 50
\end{equation}


```r
# - inputs: periods_so, lambda1_2017usdppp_so
# - outputs: 
delta_welfare_in_f <- function(t_var = 0:periods_so, 
                               welfarek1_var, 
                               welfarek2_var, 
                               welfarek3_var) {
############################################################################### 
delta_welfare_in <- 1*(10 <= t_var & t_var < 15) * welfarek1_var + 
                    1*(15 <= t_var & t_var < 20) * welfarek2_var + 
                    1*(20 <= t_var & t_var < 25) * welfarek3_var
############################################################################### 
  return(delta_welfare_in)
}
```

However, if we assume that the effect on welfare identified 20 years after the intervention persists through one's working life, we have 

\begin{equation}
\Delta W_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t > 20)\alpha^{KLPS4}
\text{ for } t \leq 50
\end{equation}


```r
# - inputs: periods_so, lambda1_2017usdppp_so
# - outputs: 
delta_welfare_p_in_f <- function(t_var = 0:periods_so, 
                                  welfarek1_var, 
                                  welfarek2_var, 
                                  welfarek3_var) {
############################################################################### 
delta_welfare_p_in <- 1*(10 <= t_var & t_var < 15) * welfarek1_var + 
                      1*(15 <= t_var & t_var < 20) * welfarek2_var + 
                      1*(20 <= t_var) * welfarek3_var
############################################################################### 
  return(delta_welfare_p_in)
}
```

Note that both expressions assume that there are no additional earnings gains for the treatment group for the first 10 years post-intervention. This model also disregards externality effects.

#### Measures of welfare

We consider two measures of welfare: earnings and consumption. Using earnings gains to measure welfare, we substitute each $\alpha$ term with the average treatment effect on earnings in each round of data collection: 87, 83, 85 dollars per person per year.


```r
delta_earnings_in = delta_welfare_in_f(welfarek1_var = lambda1_2017usdppp_so[1],
                                       welfarek2_var = lambda1_2017usdppp_so[2],
                                       welfarek3_var = lambda1_2017usdppp_so[3])

delta_earnings_p_in = delta_welfare_p_in_f(welfarek1_var = lambda1_2017usdppp_so[1],
                                           welfarek2_var = lambda1_2017usdppp_so[2],
                                           welfarek3_var = lambda1_2017usdppp_so[3])
```

Similarly, using consumption gains to measure welfare, we substitute each $\alpha$ term with the average treatment effect on consumption in each round of data collection: 0, 1011, 201 dollars per person per year. We set treatment effect on consumption to zero in the first period since we didn't collect this data. **Note: check to make sure this is correct**


```r
delta_consumption_in = delta_welfare_in_f(welfarek1_var = consump_2017usdppp_so[1],
                                          welfarek2_var = consump_2017usdppp_so[2],
                                          welfarek3_var = consump_2017usdppp_so[3])

delta_consumption_p_in = delta_welfare_p_in_f(welfarek1_var = consump_2017usdppp_so[1],
                                              welfarek2_var = consump_2017usdppp_so[2],
                                              welfarek3_var = consump_2017usdppp_so[3])
```

# Main results

Panel A (for both earnings and consumption) gives the minimum average gains required to achieve a postive Net Present Value under varying assumptions of the treatment timeframe and the internal rate of return. Panel B gives the social and government internal rates of return for each assumption of the treatment effect timeframe given the observed gains. Panel C gives the social and government Net Present Values for each interest rate and each treatment timeframe given the observed gains.

The Net Present Value accounts for three factors: the cost of deworming medication, the cost of additional schooling, and labor market / consumption gains.

The average cost of deworming per child ($2.00) is given by the product of the annual cost of deworming per child ($0.83) and number of years over which the treatment was administered on average (2.41). The annual cost of deworming per child in Kenya was obtained from a 2018 estimate by Evidence Action.

The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school a a result of deworming. The cost of schooling each child for an additional year ($267.88) is calculated by dividing an estimate of annual teacher salary ($12055) by the number of average number of students per teacher (45). The estimates of annual teacher salaries are based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor. The estimated average number of students per teacher is based on **[FILL IN]**. We use a series of estimated effects of the additional increase in secondary schooling from 1999 to 2007 obtained from Baird et. al. according to which, summed over this nine year period, students attended school for an additional 0.15 years on average. This series does not take externality effects into account.

All monetary values have been adjusted for inflation based on inflation rates reported by the World Bank and converted to 2017 USD PPP based on PPP exchange rates reported by the Bureau of Labor Statistics. They are also discounted by the real interest rate, where we consider two: 5% and 10% to look at effects over a range of values. These values correspond with the second quartile and median interest rates between 1998 and 2018. 

**Sources** 1) Sarah Baird & Joan Hamory Hicks & Michael Kremer & Edward Miguel, 2016. "Worms at Work: Long-run Impacts of a Child Health Investment," The Quarterly Journal of Economics, vol 131(4), pages 1637-1680. 2) Nyanchama, Venic. “Secondary School Teachers Salary in Kenya.” Tuko.co.ke - Kenya News., Tuko, 15 Oct. 2018, www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html. 3) Oduor, Augustine. “Windfall for Teachers as TSC Releases New Salaries.” The Standard, standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries. 4) World Bank. 5) Bureau of Labor Statistics


```r
npv_cwelfare_p_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * (1*(10 <= l_index_t & l_index_t < 15) * delta_welfare_var + 1*(15 <= l_index_t & l_index_t < 20) * delta_welfare_var +1*(20 <= l_index_t) * delta_welfare_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )) - 
    sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}

npv_cwelfare_d_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * (1*(10 <= l_index_t & l_index_t < 15) * delta_welfare_var + 1*(15 <= l_index_t & l_index_t < 20) * delta_welfare_var + 1*(20 <= l_index_t & l_index_t < 25) * delta_welfare_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )
          ) - sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}
```

## Earnings

The treatment effect on earnings observed 10, 15, and 20 years from the intervention (in each round of data collection) were, on average, 87, 83, 85 dollars per person per year respectively. We assume there are no earnings gains in the first 10 years after receiving deworming medication, and earnings gains persist through the end of one's working life (50 years after receiving treatment) or die out after the last observed five-year period (25 years after receiving treatment). The annual tax on earnings is assumed to be 16.6% based on **[FILL IN]**.


```r
#########
# PANEL A
#########

l1_social_persist_int10 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_persist_int05 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_social_die_int10     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_die_int05     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

l1_tax_persist_int10 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=10000000, positive = T))$root
l1_tax_persist_int05 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=10000000, positive = T))$root
l1_tax_die_int10     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=10000000, positive = T))$root
l1_tax_die_int05     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=10000000, positive = T))$root

#########
# PANEL B
#########

irr_social_persist <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_earnings_p_in), .1, maxiter=1000000, positive = T))$root
irr_social_die     <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_earnings_in), .1, maxiter=10000000, positive = T))$root
irr_tax_persist    <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_welfare_var = delta_earnings_p_in), .1, maxiter=1000000, positive = T))$root
irr_tax_die        <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_welfare_var = delta_earnings_in), .1, maxiter=1000000, positive = T))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_int05_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, tax_var = 1)
npv_int05_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, tax_var = 1)
npv_int10_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10, tax_var = 1)
npv_int10_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_int05_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = .05)
tax_int05_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = .05)
tax_int10_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10)
tax_int10_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10)
```

### Treatment effect timeframe: 25 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)|Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|-------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                               |                                                |10%                                     |**7.99**      |
|                                 |0                               |                                                |5%                    |**4.83**      |
|                                 |                                |0                                               |10%                                     |**48.21**         |
|                                 |                                |0                                               |5%                    |**29.12**         |
| Panel B                                                                                                                                                               
|                                 |0                               |                                                |**42.1%**    |*                                         |
|                                 |                                | 0                                              |**16.4%**       |*                                         |
| Panel C
| 10%                             |**249**  |**20**                  |                                        |*                                         |
|5%             |**537**  |**62**                  |                                        |*                                         |

### Treatment effect timeframe: 50 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)  |Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|---------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                                 |                                                |10%                                     |**6.2**  |
|                                 |0                                 |                                                |5%                    |**2.9**  |
|                                 |                                  |0                                               |10%                                     |**37.42**     |
|                                 |                                  |0                                               |5%                    |**17.48**     |
| Panel B                                                                                                                                                               
|                                 |0                                 |                                                |**42.1%**|*                                         |
|                                 |                                  | 0                                              |**17.5%**   |*                                         |
| Panel C
| 10%                             |**329**|**33**              |                                        |*                                         |
|5%             |**918**|**125**              |                                        |*                                         |

## Consumption

The treatment effect on consumption observed 15 and 20 years from the intervention (in each round of data collection) were, on average, 1011 and 201 dollars per person per year respectively. We assume there are no consumption gains in the first 10 years after receiving deworming medication, and consumption gains persist through the end of one's working life (50 years after receiving treatment) or die out after the last observed five-year period (25 years after receiving treatment). The annual tax on consumption is assumed to be 16.6% based on **[FILL IN]**.


```r
#########
# PANEL A
#########

l1_social_persist_int10 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_persist_int05 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_social_die_int10     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_die_int05     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

l1_tax_persist_int10 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_persist_int05 <- (multiroot(function(x) npv_cwelfare_p_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int10     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int05     <- (multiroot(function(x) npv_cwelfare_d_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

#########
# PANEL B
#########

irr_social_persist <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_consumption_p_in), .1, maxiter=1000000, positive = T))$root
irr_social_die     <- (multiroot(function(x) npv_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_consumption_in), .1, maxiter=1000000, positive = T))$root
irr_tax_persist    <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_welfare_var = delta_consumption_p_in), .1, maxiter=1000000, positive = T))$root
irr_tax_die        <- (multiroot(function(x) npv_mo_f(interest_r_var = x, delta_welfare_var = delta_consumption_in), .1, maxiter=1000000, positive = T))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_int05_persist <- npv_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.05, tax_var = 1)
npv_int05_die     <- npv_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.05, tax_var = 1)
npv_int10_persist <- npv_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.10, tax_var = 1)
npv_int10_die     <- npv_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.10, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_int05_persist <- npv_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.05)
tax_int05_die     <- npv_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.05)
tax_int10_persist <- npv_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.10)
tax_int10_die     <- npv_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.10)
```

### Treatment effect timeframe: 25 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)|Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|-------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                               |                                                |10%                                     |**7.99**      |
|                                 |0                               |                                                |5%                    |**4.83**      |
|                                 |                                |0                                               |10%                                     |**48.21**         |
|                                 |                                |0                                               |5%                    |**29.12**         |
| Panel B                                                                                                                                                               
|                                 |0                               |                                                |**48.4%**    |*                                         |
|                                 |                                | 0                                              |**28.8%**       |*                                         |
| Panel C
| 10%                             |**1108**  |**162**                  |                                        |*                                         |
|5%             |**2523**  |**391**                  |                                        |*                                         |

### Treatment effect timeframe: 50 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)  |Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|---------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                                 |                                                |10%                                     |**6.2**  |
|                                 |0                                 |                                                |5%                    |**2.9**  |
|                                 |                                  |0                                               |10%                                     |**37.42**     |
|                                 |                                  |0                                               |5%                    |**17.48**     |
| Panel B                                                                                                                                                               
|                                 |0                                 |                                                |**48.5%**|*                                         |
|                                 |                                  | 0                                              |**29%**   |*                                         |
| Panel C
| 10%                             |**1295**|**193**              |                                        |*                                         |
|5%             |**3419**|**540**              |                                        |*                                         |


## Additional notes for replication

### The `multiroot` function

The earnings gains in panel A and the internal rates of return in panel B are caculated using the `multiroot` function which solves for $n$ roots of $n$ (nonlinear) equations. An input to this function `start` is a scalar containing an initial guess for the unknown value.

The values in panel A are robust to all `start` values trued thus far (between 0 and 4). The values in panel B are robust to `start` values between 0 and .5; with a `start` value of .6, the internal rates of return for the tax NPV become UNSTABLE , and for a `start` value of .8 the internal rates of return for the social NPV also become hugely negative. **NOTE** update this.

# Robustness

We modify the NPV equation to account for willingness to pay for deworming medication. Given this less conservative measure of NPV, the total Net Present Value is can be interpreted as the sum of long-term net benefits of deworming, given by labor market gains, and short-term net benefits of deworming, given by direct health effects.

\begin{equation}
NPV =
-\underbrace{\left[\sum_{t=0}^{1} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]\right]}_{\text{cost of deworming medication}}
+ \underbrace{\left[\sum_{t=0}^{1} \left( \frac{1}{1 + r}\right)^{t}w_t \right]}_{\text{direct health effects}}
- \underbrace{\left[K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)\right]}_{\text{cost of schooling}}
+ \underbrace{\left[\tau \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta W_t \right]}_{\text{labor market gains}}
\label{eq:2}
\tag{2}
\end{equation}


```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
npv_wtp_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                wtp_var = wtp_2017usdppp_so,
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * delta_welfare_var
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
          apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )) + 
          sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}
```

## Willingness to pay ($w_t$)

The willingness to pay for deworming medication for one's self or one's child $w$ was estimated by Miguel, Lo, and Smith in 2018. A survey that randomized the order of conditions, severity, price, and first asking about the child or the adult revealed a downward sloping demand curve. The vast majority of respondents (>80\%) are willing to pay KSH 50 per month, or KSH 600 per year. Adjusting for PPP dollars (and inflation), we get a willingness to pay of $11.70 per year, which we use as a conservative estimate for $w$.


# Additional results


```r
npv_cwelfarewtp_p_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                wtp_var = wtp_2017usdppp_so,
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * (1*(10 <= l_index_t & l_index_t < 15) * delta_welfare_var + 1*(15 <= l_index_t & l_index_t < 20) * delta_welfare_var +1*(20 <= l_index_t) * delta_welfare_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) ) ) + 
          sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}

npv_cwelfarewtp_d_mo_f <- function(interest_r_var = interest_in,
                n_male_var = 1/2, n_female_var = 1/2, 
                delta_welfare_var,
                lambda1_male_var = lambda1_2017usdppp_so[1],
                lambda1_female_var = lambda1_2017usdppp_so[2], 
                tax_var = tax_so,
                cost_of_schooling_var = cost_per_student_in,
                delta_ed_male_var = delta_ed_so[,1],
                delta_ed_female_var = delta_ed_so[,1],
                wtp_var = wtp_2017usdppp_so,
                s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
  ns <- c(n_male_var, n_female_var)
  l_index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
############################################################################### 
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
  benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * (1*(10 <= l_index_t & l_index_t < 15) * delta_welfare_var + 1*(15 <= l_index_t & l_index_t < 20) * delta_welfare_var + 1*(20 <= l_index_t & l_index_t < 25) * delta_welfare_var)
  }

  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
            apply( ( 1 / (1 + interest_r_var) )^l_index_t *
                     delta_ed_s * cost_of_schooling_var, 2, sum) )) + 
          sum( ( 1 / (1 + interest_r_var) )^(0:2) * (s2_var * q2_var  - s1_var * q1_var) )
############################################################################### 
  return(res1) 
}
```

## Earnings


```r
#########
# PANEL A
#########

l1_social_persist_int10 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_persist_int05 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_social_die_int10     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_die_int05     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

l1_tax_persist_int10 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_persist_int05 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int10     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int05     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

#########
# PANEL B
#########

# irr_social_persist <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_earnings_p_in), -2, maxiter=1000000, positive = T))$root
# irr_social_die     <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_earnings_in), -2, maxiter=1000000, positive = T))$root
# irr_tax_persist    <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, delta_welfare_var = delta_earnings_p_in), -2, maxiter=1000000, positive = T))$root
# irr_tax_die        <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, delta_welfare_var = delta_earnings_in), -2, maxiter=1000000, positive = T))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_int05_persist <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_p_in, tax_var = 1)
npv_int05_die     <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_in, tax_var = 1)
npv_int10_persist <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10, tax_var = 1)
npv_int10_die     <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_int05_persist <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = .05)
tax_int05_die     <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = .05)
tax_int10_persist <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10)
tax_int10_die     <- npv_wtp_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10)
```

### Treatment effect timeframe: 25 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)|Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|-------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                               |                                                |10%                                     |**7.13**      |
|                                 |0                               |                                                |5%                    |**4.4**      |
|                                 |                                |0                                               |10%                                     |**43**         |
|                                 |                                |0                                               |5%                    |**26.53**         |
| Panel B                                                                                                                                                               
|                                 |0                               |                                                |**48.4**     |*                                         |
|                                 |                                | 0                                              |**28.8**        |*                                         |
| Panel C
| 10%                             |**252**  |**23**                  |                                        |*                                         |
|5%             |**540**  |**65**                  |                                        |*                                         |

### Treatment effect timeframe: 50 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)  |Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|---------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                                 |                                                |10%                                     |**5.53**  |
|                                 |0                                 |                                                |5%                    |**2.64**  |
|                                 |                                  |0                                               |10%                                     |**33.38**     |
|                                 |                                  |0                                               |5%                    |**15.92**     |
| Panel B                                                                                                                                                               
|                                 |0                                 |                                                |**48.5%**|*                                         |
|                                 |                                  | 0                                              |**29%**   |*                                         |
| Panel C
| 10%                             |**331**|**36**              |                                        |*                                         |
|5%             |**921**|**128**              |                                        |*                                         |

## Consumption


```r
#########
# PANEL A
#########

l1_social_persist_int10 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_persist_int05 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_social_die_int10     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_social_die_int05     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, tax_var = 1, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

l1_tax_persist_int10 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_persist_int05 <- (multiroot(function(x) npv_cwelfarewtp_p_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int10     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, interest_r_var = 0.10), 4, maxiter=1000000, positive = T))$root
l1_tax_die_int05     <- (multiroot(function(x) npv_cwelfarewtp_d_mo_f(delta_welfare_var = x, interest_r_var = 0.05), 4, maxiter=1000000, positive = T))$root

#########
# PANEL B
#########

# irr_social_persist <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_consumption_p_in), .001, maxiter=1000000, positive = T))$root
# irr_social_die     <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, tax_var = 1, delta_welfare_var = delta_consumption_in), .001, maxiter=1000000, positive = T))$root
# irr_tax_persist    <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, delta_welfare_var = delta_consumption_p_in), .001, maxiter=1000000, positive = T))$root
# irr_tax_die        <- (multiroot(function(x) npv_wtp_mo_f(interest_r_var = x, delta_welfare_var = delta_consumption_in), .001, maxiter=1000000, positive = T))$root

#########
# PANEL C
#########

# Net Present Value (2017 USD PPP)
npv_int05_persist <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.05, tax_var = 1)
npv_int05_die     <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.05, tax_var = 1)
npv_int10_persist <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.10, tax_var = 1)
npv_int10_die     <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.10, tax_var = 1)

# Net Present Value of tax revenue (2017 USD PPP)
tax_int05_persist <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.05)
tax_int05_die     <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.05)
tax_int10_persist <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_p_in, interest_r_var = 0.10)
tax_int10_die     <- npv_wtp_mo_f(delta_welfare_var = delta_consumption_in,   interest_r_var = 0.10)
```

### Treatment effect timeframe: 25 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)|Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|-------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                               |                                                |10%                                     |**7.13**      |
|                                 |0                               |                                                |5%                    |**4.4**      |
|                                 |                                |0                                               |10%                                     |**43**         |
|                                 |                                |0                                               |5%                    |**26.53**         |
| Panel B                                                                                                                                                               
|                                 |0                               |                                                |**48.4%**    |*                                         |
|                                 |                                | 0                                              |**28.8%**       |*                                         |
| Panel C
| 10%                             |**1111**  |**165**                  |                                        |*                                         |
|5%             |**2526**  |**394**                  |                                        |*                                         |

### Treatment effect timeframe: 50 years

|Real annualized interest rate (r)|Net Present Value (2017 USD PPP)  |Net Present Value of tax revenue (2017 USD PPP) |IRR (annualized)                        | Avg earnings gains (2017 USD PPP)        |
|--------------------------------:|---------------------------------:|-----------------------------------------------:|---------------------------------------:|-----------------------------------------:|
| Panel A                                                                                                                                                                                             
|                                 |0                                 |                                                |10%                                     |**5.53**  |
|                                 |0                                 |                                                |5%                    |**2.64**  |
|                                 |                                  |0                                               |10%                                     |**33.38**     |
|                                 |                                  |0                                               |5%                    |**15.92**     |
| Panel B                                                                                                                                                               
|                                 |0                                 |                                                |**48.5%**|*                                         |
|                                 |                                  | 0                                              |**29%**   |*                                         |
| Panel C
| 10%                             |**1298**|**196**              |                                        |*                                         |
|5%             |**3422**|**543**              |                                        |*                                         |

