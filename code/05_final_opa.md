---
title: "A Unifying Open Policy Analysis for Deworming"
date: "02 October, 2019"
output:
  html_document:
    code_folding: hide
    code_download: true
    collapsed: yes
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    toc: yes
    toc_depth: 3
    toc_float: yes
  pdf_document: default
  word_document: default
editor_options:
  chunk_output_type: console
pdf_document:
  extra_dependencies: ["xcolor"]
---
\def\blue{\color{blue}}
\def\red{\color{red}}









```r
################ 
#####  Notes:
################ 
### Source ---------->  Input ---------->  Model ---------->  Policy Estimates (output)
###  (_so)              (_in)              (_mo)                (_pe)
### values            functions          functions              values
###                   & values           & values             
# - call_sources_f- tax_elasticity_in_f  - tax_revenue_mo_f     - ten_year_revenue_pe
# - policy_f      - est_billionares_in_f - total_rev_mo_f       - ten_year_top_tax_pe
#                                        - ten_years_mo_f       - total_rev_pe
### arguments in functions should used "_var" and functions should "_f"


# DESCRIBE FUNCTIONS STRUCTURE
# - inputs: list
# - outputs: list
#### function:  
#sample_function_f <- function(){
########################################## 
##########################################  
#
#    ...
#
########################################## 
##########################################  
#    return( )                                  # A list with all (most?) the elements 
#}                                              # generated inside the function 
#invisible( list2env(sample_function_f(),.GlobalEnv) ) 
#
```



```r
call_params_f <- function(){
    #############
    ##### Data  
    #############
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
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
    lambda1_so <- c(3.49, 0)       #Hrs per week increase for men and women CONFIRM
    lambda2_so <- 10.2             #Externality effect (proportional) - Table 3, Panel B
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



# Key policy estimates for policy makers  

The key policy estimate consists of a cost effectiveness analysis that compares the present 
value of benefits and costs. The benefits quantified here are the effects on wages an the
costs are those of delivering the deworming treatment.  





The benefits will account for the direct effects of deworming and plus the indirect effects of deworming due to smaller pool of sick people in the community (herd inmunity). Effects are computed as a change in the earning profile of the population. 


# Methodology


This analaysis contains elements from GiveWell's cost effectiveness analaysis (see [here](https://docs.google.com/spreadsheets/d/1McptF0GVGv-QBlhWx_IoNVstWvt1z-RwVSu16ciypgs/edit#gid=1537947274), an editable version can be found [here](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1537947274))  and the cost benefit analysis described in [Baird et al., 2016](https://academic.oup.com/qje/article/131/4/1637/2468871).  

## Main Equation (the model)

The key result for policy makers is defined as the cost effectivness ratio (cell [`Deworming!B32`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B32)). 

\begin{equation}
CEA_{deworming} = \frac{B (1 + \blue{F_{0}})}{C}
\label{eq:1}
\tag{1}
\end{equation}

 - $C$ is the costs per person dewormed (`F2, 4,B23` --> [`F1, 2, H16`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=H16)).     
 - $B$ is the benefits per person dewormed (`F2, 4,B22`).
 - $\blue{F_{0}}$ is a factor to account for leverage/fudging [not reviewed in this excercise] ([`F2, 6, D259`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1611790402&range=D259))


Also this quantity could be expressed in relative terms to the benchmark of cash transfers (cell [`Results!B9`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1034883018&range=B9)): 

\begin{equation}
RCEA = \frac{CEA_{deworming}}{CEA_{cash}}
\end{equation}



```r
# - inputs: total per capita benefits, total per capita costs, fudging factor 
# - outputs: Cost-effectiveness ratio & ratio to cash CEA
policy_est_f <- function(){
############################################################################### 
###############################################################################  
  
    CEA_pe <- function(benefits_var = 1, fudging_var = 1,
                       costs_var = 1) {
     ( benefits_var * ( 1 + fudging_var ) ) / costs_var
    }
    RCEA_pe <- function(CEA_var = 1, CEA_cash_var = 1) CEA_var / CEA_cash_var
      
############################################################################### 
###############################################################################  
    return(list("CEA_pe" = CEA_pe, 
                "RCEA_pe" = RCEA_pe))
}
invisible( list2env(policy_est_f(),.GlobalEnv) )
```

## Sub-components:

We begin by describing the underlying analysis behind the costs. Through this excercise we use the following notation the letters $F, P, Q$ denote components
in percentages, monetary units (US dollars and local currency) and quantities respectively. Each new element will be tracked using a sub-index, and supra-indecis will be
used to track groups, like geographies, time, and other catergories. For example $Q^{i}_{2}$ represents the second quantity described in this analysis (total adjusted number childred dewormed per year) in location $i$. At the end of each description we will show in parenthesis the original location of the parameter in GiveWell's spreadsheets (using the notation `file, sheet number, cell`[^1]). When a parameter in an equation does not depend on any subsequent component, it is highlighted in bold.

### Costs ("$C$")

\begin{equation}
C = \sum_{i \in Countries } w_{i} c_{i}
\label{eq:2}
\tag{2}
\end{equation}

GiveWell estimates the cost per child dewormed in geographies where Evidence Action provides technical assistance.These costs include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such the cost of drugs donated by WHO. 

Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The final cost is a weighted average of the unit cost across countries. 

- $w_{i}$: Weight for the weighted average ([`F1, 2, C:G8`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=C8:G8)).  
- $c_{i}$: Total cost per child, per year in country $i$ (`F1, 2, C:G16`).  

Build $c_i$ as a function of three stakeholders: DtW, other donors, goverment.  
Each stakeholders spends on: line items.    
Incorporate currency. 


```r
# - inputs: nothing
# - outputs: function that computs the weighted sum of country costs
costs_f <- function(){
############################################################################### 
###############################################################################  
  
    final_cost <- function(country_w_var = 1, country_cost_var = 1) {
        sum(country_w_var * country_cost_var)
    }
    
############################################################################### 
###############################################################################  
    return(list("final_cost" = final_cost))
}

#To delete: 

pe <- 1
benefits <- 1
wages <- function(base_wage_var) {
    1 + base_wage_var
}

base_wage <- function(sectors_var){
  sectors_var
}

sectors <- function(hours_var){
  hours_var / sum(hours_var)
}

effect <- 1

cost <- function(unit_costs_var) {
    unit_costs_var * total_units
}

unit_costs <- function(local_costs_var){
  local_costs_var 
}


total_units <- function(total_units_var){
  total_units_var
}





invisible( list2env(costs_f(),.GlobalEnv) )
```
    


\begin{equation}
w_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \\
c_{i} = \frac{C_{i}}{N_{i}} Ex_{i} \\
C_{i} = \sum_{k \in payers}C_{i,k} \\
C_{i,k} = \sum_{l \in items}C_{i,k,l}
\label{eq:3}
\tag{3}
\end{equation}


**Ask Grace about the currency on the data**  

GW original analyaia weights each country to take into account the number of treatments provided as well as the proportion of costs incurred by DtWI in that geography. The analytical foundations for such weights are not clear. Also not clear why should only account for DtW costs.  

   
- $N_{i}$: Number of treatet children in country $i$.  
- $Ex_{i}$: Exchange rate from country $i$ to USD.  
- $k$: Costs distribute across $k$ payers.   
- $l$: Each payers costs come from $l$ items.   



```r
# - inputs: nothing
# - outputs: function that computes the country weights used in the final costs
costs_inp_f <- function(){
############################################################################### 
###############################################################################  
  
    costs_inp <- function(n_country_var, ex_rate_var, df_costs_var){
      #country weights
      country_w <- n_country_var / sum(n_country_var)
      
      num_countries_temp <- length(n_country_var)
      cost_payer_temp <- numeric(num_countries_temp)
      
      costs_by_payer_temp <- df_costs_var %>% 
                          group_by(country, item) %>% 
                          summarise("costs" = sum(costs))
      
      country_cost <- costs_by_payer_temp %>% 
                          group_by(country) %>% 
                          summarise("costs" = sum(costs))  
      
      return( list(country_w, country_cost) )
    }
      
############################################################################### 
###############################################################################  
    return(list("costs_inp" = costs_inp))
}
invisible( list2env(costs_inp_f(),.GlobalEnv) )
```

#### Data requiered to compute costs.

$N_{i}, Ex_{i}, C_{i,k,l}$

### Benefits ("$B$")  


GW computes the benefits like this:   

[(`F2, 4, B22`)](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B22)

\begin{equation}
B = \left( \frac{1}{1 + r} \right)^{t_{L}} \sum_{t = 0}^{T - 1}
\frac{ln( 1 + \red{ F_{8} })}{(1 + r)^{t}}F_{9}F_{10}F_{11}F_{12}F_{13}\frac{ \red{ F_{15} } }{F_{14}}W
\label{eq:16}
\tag{16}
\end{equation}


- $F_{7}$: Discount rate[^3] (`F2, 2, B3`).  
- $\red{F_{8}}$: Treatment effect of deworming on earnings (15.4%)[^4] (`F2, 2, B3`).  
- $F_{9}$: Multiplier for resource sharing within households[^5] (`F2, 2, B9`).   
- $F_{10}$: Adjustment for El Niño[^6] (`F2, 4, B10`).  
- $F_{11}$: Adjustment for years of treatment in Miguel and Kremer vs. years of treatment in charities' programs[^7] (`F2, 4, B11`).  
- $F_{12}$: Replicability adjustment[^8] (`F2, 4, B12`).  
- $F_{13}$: Additional years of treatment assigned to the treatment group from Miguel and Kremer[^9] (ITT to TOT?) (`F2, 4, B13`).  
- $F_{14}$: Proportion of deworming going to children — DtW (`F2, 4, B18`).  
- $\red{ F_{15} }$: Worm intensity adjustment ([`F3, 1, I52`](https://docs.google.com/spreadsheets/d/1joUnjoxdlVVkXAc2kcoi-_MrIROjddSXF3qScPBoKTE/edit#gid=2095417847&range=I52)).  
- $Q_{7}$: Time to labor market[^10]  (`F2, 4, B5`).  
- $Q_{8}$: Time in the labor market (`F2, 4, B7`).  
- $W$: Moral weights (conversion from increase in earnings to utils) [^11] (`F2, 3, B21`).  


Bair et al. (2016) compute benefits like this:

\begin{equation}
B =   \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} \blue{ w_{t} }
\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right) 
\end{equation}

**Note:** The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender. 
 
Where:   
 - $w_t$: earnings in period $t$.   
 - $\lambda_{1}$: direct effects of deworming on earnings.  
 - $\lambda_{2}$: indirect effects of deworming on earnings.   
 - $p$: saturation, measures the fraction of the population that is effectively usign the treatment.  
 - $R$: coverage, defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group.  



### 1 - "$r$"  

The real interest rate $r$ is obtained from the interest rate on betterment bonds (0.118) minus the inflation rate (0.02).


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
interest_in_f <- function(gov_bonds_var = gov_bonds_so , inflation_var = inflation_so) {  
  interest_in = gov_bonds_var - inflation_var 
  return(list("interest_in" = interest_in))
}
invisible( list2env(interest_in_f(),.GlobalEnv) )
```

The resulting value is a $r$ = 9.85%

### 2 - "$w_{t}$"

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50
\end{equation}

individuals in the data are assumed to enter the labor force 10 years after the (data) present day ($w_t = 0$ for $t<10$). Wage at time $t$ is the weekly starting wage in USD ($w_0$) that has a base growth rate equal to the per capita GDP growth ($g$) applied to however many years of work ($Xp$). In addition to this growth, the salaries are adjusted to represent a (concave) wage life cycle profile ($1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2$).

#### 2.1 - "$w_0$"

\begin{equation}
w_t =  \text{#weeks} \times \blue{w_0} (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l} \\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}  
\end{equation}

The initial wage in dollars ($w_{0}$) is a weighted average of wages for control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the average number of hours in each sector ($h_l$) relative to the sum of the average number of hours in each sector.

The wage in agriculture comes from research (Suri, 2011), the working wage comes from the data and its defined as  hourly wage for the control group for those who reported more than 10 hrs of work per week. The self-employed wage ($w_{se}$) was constructed as follows:

\begin{equation}
w_{se} =  \frac{ \text{Monthly self-employed profits} }{4.5 \times E[h_{se}|h_{se}>0] }
\end{equation}

Where both parameters (Monthly self-employed profits and self-employed hours for the control group, conditional on hrs >0 - $E[h_{se}|h_{se}>0]$ -) come from the data (ww paper).  The measure of hours in self employment used to compute wages is ($E[h_{se}|h_{se}>0]$) is different from the one is to compute the weights $\alpha_l$ above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of workin age in the sample (hence capturing the relative inportance of the self employed sector in the economy)



```r
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so, 
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so), 
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so), 
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_mo) and function (wage_0_mo_f), Wage trayectory:
#  value (wage_t_mo) and function (wage_t_mo_f).
wages_f <- function(wage_ag_var_h1 = wage_ag_so,  
                     wage_ww_var_h1 = wage_ww_so,
                     profits_se_var_h1 = profits_se_so,
                     hours_se_cond_var_h1 = hours_se_cond_so,  
                     hours_ag_var_h1 = hours_ag_so,
                     hours_ww_var_h1 = hours_ww_so,
                     hours_se_var_h1 = hours_se_so,
                     ex_rate_var_h1 = ex_rate_so, 
                     periods_var_h1 = periods_so, 
                     time_to_jm_var_h1 = time_to_jm_so, 
                     growth_rate_var_h1 = growth_rate_so,
                     coef_exp1_var_h1 = coef_exp_so[1],
                     coef_exp2_var_h1 = coef_exp_so[2]){
################################################################################
################################################################################  

    experience_aux <- 0:periods_var_h1 - time_to_jm_var_h1

    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision
    
    wage_0_mo_f <- function(wage_ag_var, wage_ww_var, profits_se_var, hours_se_cond_var, 
                            hours_ag_var, hours_ww_var, hours_se_var, ex_rate_var) {
      wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
      wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
      alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) / sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
      res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
      return(res1)
    }
    
    wage_t_mo_f <- function(wage_0_var,
                       growth_rate_var,
                       experience_var,
                       coef_exp1_var,
                       coef_exp2_var) {
      res1 <- 52 * wage_0_var *( ( 1 + growth_rate_var )^experience_var ) *
        ( 1 + coef_exp1_var * experience_var + coef_exp2_var * experience_var^2 ) *
        ifelse(0:periods_var_h1 >= time_to_jm_var_h1, 1, 0)
      return(res1)
    }
    
    wage_0_mo <- wage_0_mo_f(wage_ag_var = wage_ag_var_h1,  
                         wage_ww_var = wage_ww_var_h1,
                         profits_se_var = profits_se_var_h1,
                         hours_se_cond_var = hours_se_cond_var_h1,  
                         hours_ag_var = hours_ag_var_h1,
                         hours_ww_var = hours_ww_var_h1,
                         hours_se_var = hours_se_var_h1,
                         ex_rate_var = ex_rate_var_h1)  

    #close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
    #but I suspect diff due to computational precision
    wage_t_mo <- wage_t_mo_f(wage_0_var = wage_0_mo,
                       growth_rate_var = growth_rate_var_h1,
                       experience_var = experience_aux,
                       coef_exp1_var = coef_exp1_var_h1,
                       coef_exp2_var = coef_exp2_var_h1)

################################################################################
################################################################################
    return(list("wage_0_mo_f" = wage_0_mo_f, "wage_0_mo" = wage_0_mo, 
                "wage_t_mo_f" = wage_t_mo_f, "wage_t_mo" = wage_t_mo))
}

invisible( list2env(wages_f(),.GlobalEnv) )
```

### 3 - "$\lambda_{1}$"  and  "$\lambda_{2}$"

$\lambda_{1,\gamma}$ represents the estimated impact of deworming on hours of work for men a women. This two parameter are combined with a underweighted mean:

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female} \\
\lambda_{1,\gamma} = \alpha \lambda^{eff}_{1,\gamma} + (1 -  \alpha) \times 0
\end{equation}

Where: 
 - $\alpha$: represents the incidence of the condition.  
 - $\lambda_{1}^{eff}$: represents the effect of deworming over those affected with the condition.  
 - $\lambda_{2}^{eff}$: ?. **[discuss with Ted/Michael]**
 
**WARNING: the next paragraph has a bunch of hard coded numbers that need to be reviewed and coded up**   

In the original evaluation, $\alpha = 0.9$ [**NEED TO INSERT THE TRUE NUMBER HERE**], hence $\lambda_{1}^{eff} = 1.75/0.9 = 1.94$. The value of $\lambda^{r}_{1}$ for each region $r$ will depend on that region's $\alpha^{r}$.  

Its components come from the W\@W paper. 

$\lambda_{2,\gamma}$ the estimated externality effect (EXPLAIN) and comes from research (W\@W). Note that this parameter in not estimated by gender, so we repeat its value two times.


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
lambdas_in_f <- function(lambda1_var = lambda1_so, lambda2_var = lambda2_so, alpha_var = 1){
    lambda1_temp <- alpha_var * lambda1_var 
    lambda1_in <- rep(0.5 * lambda1_var[1] + 0.5 *lambda1_var[2], 2)
    lambda2_in <- rep(lambda2_var, 2)
    return(list("lambda1_in" = lambda1_in, "lambda2_in" = lambda2_in))  
}

invisible( list2env(lambdas_in_f(),.GlobalEnv) )
```





### 4 - $R$ and $p$


The coverage, $R$, is defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group (last paragraph of page 9(1645) of paper). As the treatment was appplied to approximatedly two thirds of the population, $R$ is set to: $R  = 0.68$.  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively usign the treatment and is defined as:  

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)
\end{equation}

For this (or similar?) setting Miguel and Kremer 2007 [add page, table, col, row] estimate that there is almost no take-up without subsidy, hence $Q(0)$ is assinged the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy is $Q(full) = 0.75$.


```r
# - inputs: coverage_so, q_full_so, q_zero_so 
# - outputs: saturation_in 
saturation_in_f <- function(coverage_var = coverage_so, q_full_var = q_full_so, q_zero_var = q_zero_so){
    saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
    return(list("saturation_in" = saturation_in)) 
} 
invisible( list2env(saturation_in_f(),.GlobalEnv) )
```




---------------------

\begin{equation}
F_{15} = \frac{\sum_{r \in G_{10}} F^{r}_{16} F^{r}_{17}}{\sum_{r}  F^{r}_{17}}
\label{eq:17}
\tag{17}
\end{equation}

- $F^{r}_{16}$: DtW weight for worm intensity by location, adjusted (windorinzation) (`F3, 1, X14`)  
- $F^{r}_{17}$: DtW weight for worm intensity by location, unadjusted (different from above)   
(`F3, 1, C14` --> [`F3, 10, K26`](https://docs.google.com/spreadsheets/d/1joUnjoxdlVVkXAc2kcoi-_MrIROjddSXF3qScPBoKTE/edit#gid=1162875528&range=K26))  

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
\begin{align}
  F^{r}_{16} &= \left\{
  \begin{array}{lr}
        \widetilde{F}^{r}_{16}, & \text{if } q(\widetilde{F}^{r}_{16}) \in (10\%, 90\%)\\
        q(\widetilde{F}^{r}_{16})_{10}& \text{if } q(\widetilde{F}^{r}_{16})\leq q(\widetilde{F}^{r}_{16})_{10}\\
        q(\widetilde{F}^{r}_{16})_{90} & \text{o/w } 
  \end{array}
        \right.\\
  \widetilde{F}^{r}_{16} &= 0.8F_{18} + 0.2\frac{F_{19}}{F_{20}}F^{r}_{21}
        \label{eq:18}
\tag{18}
\end{align}

- $\widetilde{F}^{r}_{16}$: Weight before winzorizing (`F3, 1, U14`)  
- $F_{18}$: Intensity adjustment to account for S. haem (`F3, 1, K14`)  
- $F_{19}$: Average intermediate intensity adjustment across regions and charities (`F3, 1, G50`)   
- $F_{20}$: Average Goblal Burden of Disease adjustment across regions and charities (`F3, 1, L50`)   
- $F^{r}_{21}$: Goblal Burden of Decease (GBD) adjustment for DtW in specific region (`F3, 1, L14`)  

\begin{equation}
F^{r}_{18} = F^{r}_{22} +  F^{r}_{23}
\label{eq:19}
\tag{19}
\end{equation}

- $F^{r}_{22}$:  DtW intermediate intensity adjustment for specific region (`F3, 1, G14`)  
- $F^{r}_{23}$:  Proportion of S. Haem in region (`F3, 1, J14`)  


\begin{equation}
F^{r}_{22} = F^{r}_{24} F^{r}_{25} F^{r}_{26}\\
F^{r}_{23} = 0.2 F^{r}_{27}\frac{\sum_{all} F^{r}_{22}}{0.8}
\label{eq:20}
\tag{20}
\end{equation}

- $F^{r}_{24}$: Calculated [location] intensity adjustment ([`F3, 4, H27`](https://docs.google.com/spreadsheets/d/1joUnjoxdlVVkXAc2kcoi-_MrIROjddSXF3qScPBoKTE/edit#gid=1839934728&range=H27)).  
- $F^{r}_{25}$: Adjustment for representativeness (=100%)  (`F3, 1, E14`).   
- $F^{r}_{26}$: Adjustment for co-infection (=100%)    (`F3, 1, EF4`).  
- $F^{r}_{27}$: Prevalence rate of schisto haem  (`F3, 1, I14`)   
  
\begin{equation}
F^{r}_{21} = \sum_{d\in G_{11}}\blue{F^{d}_{28}}F^{d}_{32}\\
F^{r}_{24} = \sum_{d\in G_{11}}\blue{F^{d}_{28}}F^{d}_{29}\\
F^{r}_{27} = \blue{ \frac{\widetilde{F}^{r}_{27}}{ \sum_{all} \widetilde{F}^{r}_{27} } }\\
F^{r}_{29} = \frac{F_{30}}{F_{31}}
\label{eq:21}
\tag{21}
\end{equation}


- $\blue{ \widetilde{F}^{r}_{27} }$: GiveWell S. haem. prevalence estimate ([`F1,1, H14`](https://docs.google.com/spreadsheets/d/1joUnjoxdlVVkXAc2kcoi-_MrIROjddSXF3qScPBoKTE/edit#gid=2095417847&range=H14) --> [`F3, 3, O18`](https://docs.google.com/spreadsheets/d/1joUnjoxdlVVkXAc2kcoi-_MrIROjddSXF3qScPBoKTE/edit#gid=2041655207&range=O18))
- $\blue{F^{r}_{28}}$: Infection weight (`F3, 4, B:E2`).  
- $F^{r}_{29}$: Prevalence rates of each infection by location (`F3, 4, B:E27`).  
- $F^{r}_{30}$: Eggs per gram in [location] and [condition] (`F3, 3, E18`).  
- $F^{r}_{31}$: Eggs per gram in original study by [condition] (`F3, 3, E4`).  
- $F^{r}_{32}$: Intensity adjustment from GBD (`F3, 7, F18`).  


\begin{equation}
F^{r}_{32} = \frac{Q^{dr}_{9}}{Q^{d}_{10}}\\
Q^{dr}_{9} = Q_{11,1}(F^{dr}_{33})^3 + Q_{11,2}(F^{dr}_{33})^2 + Q_{11,3}F^{dr}_{33}\\
F^{r}_{33} = F_{34,1} + F_{34,2}
\label{eq:22}
\tag{22}
\end{equation}


- $F^{r}_{33}$: Average prevalence for two age groups (`F3, 7, J18`).  
- $Q^{dr}_{9}$: Intensity (eggs per gram) (`F3, 6, V17`).    
- $F^{r}_{34, age}$: Prevalence for each age group (`F3, 6, T:S17`).  
- $Q^{d}_{10}$: Eggs per gram by type of desease (`F3, 7, B:E5`).    

\begin{equation}
F^{r}_{17} = \frac{F^{r}_{35}F^{r}_{36}}{\sum_{r}F^{r}_{35}F^{r}_{36}}
\label{eq:23}
\tag{23}
\end{equation}

- $F^{r}_{35}$: Proportion of projected treatments in [location] from 2017-2020 (`F3, 10, J26`).  
- $F^{r}_{36}$: Proportion of all costs paid by DtW in 2014-2015 (`F3, 10, H26->D42`).  

\begin{equation}
F^{r}_{35} = \frac{Q^{r}_{11}}{\sum_{r}Q^{r}_{11}}\\
F^{r}_{36} = \frac{P^{r}_{13}}{P^{r}_{14}}\\
Q^{r}_{11} = \frac{Q_{12} Q^{r}_{13}}{\sum_{r}Q^{r}_{13}}
\label{eq:24}
\tag{24}
\end{equation}

- $Q^{r}_{11}$: 2017-2020 projected treatments (`F3, 10, E26`).  
- $Q_{12}$: 168,996,327 (in `F3, 10, E26`).  
- $Q^{r}_{13}$: Treatments in [location]  in 2015-16 (`F3, 10, D26`).  
- $P^{r}_{13}$: DtWI costs in [location] (`F3, 10, B42`).  
- $P^{r}_{14}$: Total costs in [location] (`F3, 10, C42`).  



```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outpu ts: total_rev_pe
```


# Main results

```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outputs: total_rev_pe
```


# Montecarlo simulations  

```r
# Draws
# Compute inputs
# Compute model
# Run sims
```

# Sensitivity Analysis  


[^1]: `F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]` Original [here](https://docs.google.com/spreadsheets/d/1jzS693Y-ZAIloQejlzSc3e3t7iPHyor1qt7HBjSVXhQ/edit#gid=509033857), editable version [here](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=509033857)
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter.


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit

[^4]: "Roughly 10 years after the initial experiment described in Miguel and Kremer 2004, The Kenya Life Panel Survey 2 (KLPS-2) measured earnings in the Miguel and Kremer 2004 study population. This was followed by a 15 year follow-up survey, the KLPS-3.   
 We estimate that the average treatment household experienced a 15.4% increase in earnings. This corresponds to an increase in ln(earnings) of 0.143. This estimate is based on a preliminary, confidential analysis by Ted Miguel and others that pools earnings data from the KLPS-2 and KLPS-3. The estimate is formed using the total earnings findings across the whole sample (including individuals with zero earnings) and trimming the top 1% of earners in both the treatment and control groups. We take the log effect of earnings at the sample mean. The p-value for this result is .019. The authors have graciously shared their preliminary estimates with us, but we cannot yet publish the full details.   
  Note that this is an intention-to-treat effect. Not all individuals in the study population received treatment. We adjust this estimate for treatment compliance later using a parameter that captures the additional years of deworming received by the treatment group. A private, internal-only document related to this parameter is available to GiveWell staff at https://docs.google.com/document/d/1AK1H5kZi7-60zcxZs1vpZORVXk6M2y3DkkKymU50FkY/edit#heading=h.fm1rpvmo3lo. "

[^5]: "If a person treated for worms earns additional income and supports a family, then multiple people may benefit—not just the person who was dewormed.   
 In a multi-person household with one wage earner, a 10% increase in wages could enable every member of the household to consume 10% more. However, many households will have multiple wage earners, and household size may change overtime.  
 A rough model for estimating a value for this parameter is available at https://docs.google.com/spreadsheets/d/112uuyYt6QLRZuJojwz6fHv4JQ-GHNeIpiT-SauY3kmM/edit#gid=0. The appropriate value for this parameter will depend on many uncertain factors (e.g. household composition and how household composition changes overtime). We currently use a default value of 2.0. Our rough model suggests values close to 2 under a range of reasonable assumptions."


[^6]: "Updated cell note for the parameter:
Baird et al. 2016 compares the first two groups of schools to receive deworming (as treatment group) to group 3 (control).  
 Our worm intensity adjustment is calculated by comparing baseline intensity of infection in group 1 schools with baseline intensity of infection in the areas where our charities work.  
 This adjustment accounts for group 2 having a significantly higher baseline intensity of infection than group 1, likely due to flooding from an unusually strong El Niño event.   
 The average of baseline worm intensity in groups 1 and 2 was ~45% higher than in group 1 only (based on data we do not have permission to share). Taking the odds ratio implies an adjustment of ~65%.   
 (Last updated September 2018 | Link to Baird et al. 2016: https://doi.org/10.1093/qje/qjw022 | Link to Miguel and Kremer 2004: http://www.jstor.org/stable/3598853) Suggested value:65%.  More info here: https://docs.google.com/document/d/1pyGpVYcerlUSObdaIz7_6Tmv1D8O0L4IGQx1HtzTVbk/edit"

[^7]:"This is a really complicated input. I'm going with a value of 75% for now since it seems like a lot of deworming programs are in their infancy and hence will be providing early years of treatments to targeted children, but I think that my best guess would vary between 60-100% depending on the charity if I were just thinking on the margin. (In particular, SCI closer to 60%, DtWI closer to 100%.) Right now there isn't a great way to change this input for each charity, so I'm taking something toward the lower end of the range for all charities. 
 I think in the long-run we should do a better job answering the question, "How many years of deworming treatment have children already received on average in places where our charities would spend marginal funds?" This would involve figuring out whether LF programs had been operating, whether pre-school-aged kids receive deworming, etc.   
 A major reason one might want to use ~70-100%: for a few charities, the bulk of their marginal funding seems to be going toward starting deworming programs in regions that may not have received any deworming before. In those cases, if I look at just the next few years of deworming impact, I probably wouldn't discount for years of treatment vs. Baird at all because the average year of deworming treatment being provided will be a kid's ~second year, similar to the deworming treatment effect that we're observing in Baird. However, thinking marginally like this complicates our CEA: we'll need to change this discount over time as charities' programs mature (assuming they continued to operate). I'm not sure about the best philosophical approach to CEAs here, but am currently thinking I'll change this discount in the future as charities' programs mature.  
If you just look at the long run and assume that on average charities will be providing kids' ~4th year of deworming treatment, then I think a discount like 65% is probably more appropriate.  
A few factors I considered that pushed in opposite directions:
- I think that on average our charities are treating kids who are slightly younger than were treated in Miguel and Kremer, pushing things slightly in favor of our charities since I expect deworming effects to be larger at younger ages.
- OTOH, it may be that in some cases our charities are operating in places where children received deworming as pre-schoolers, which would decrease marginal value of additional years of treatment. We don't have good information yet on how common this is.
- The deworming coverage rates in Miguel and Kremer were low, so it might be appropriate to think of the treatments provided in that study as roughly a kid's ~2nd and 3rd year of treatment rather than ~3rd and 4th. That pushes in favor of discounting charities' impact more.
- In the long run, I'm not sure how many years of treatment an average child will receive who is targeted by our charities' programs. It depends on the composition of ages in schools where our charities treat, how often kids drop out, how often kids miss deworming treatment over the course of their time in school, etc. I think just looking at the average age of child treated and knowing whether there are pre-school treatments would be useful here. In general I guessed that the average kid will receive ~8 years of treatment in the long-run.  
 As a sensitivity check to account for the above, I edited James's model for this input here: https://docs.google.com/spreadsheets/d/1lLF28COf90edQFipk4ZC7BPZd-kKoj9sNDDt1oH8_vc/edit?usp=sharing . That sheet is very rough and may be difficult to follow. It includes some information on Natalie's guesses about the average number of years of deworming that kids have received in each charity's program."

[^8]: https://docs.google.com/document/d/13Tb3AnIT7zPgmKcGuwH365gQj6chWd0-blBlbzXjKpA/edit?usp=sharing


[^9]: "Josh: I broadly agree with Chris's argument here, so moving to 1.69. Note that this makes a ~30% difference to the bottom line.  
 In summary: on the cost side of our CEA we use a TOT framework, but on the benefits side we use an ITT effect. That's conservative; Miguel and Kremer only achieved ~50-60% coverage in an average deworming round. If they'd achieved 100% coverage we should expect that the impact of the program would've been much larger. One way to convert ITT to TOT is to just divide by the coverage rate achieved in the experiment. This method is imperfect because there were likely spillover effects of deworming, which substantially complicates the analysis. I think a key question for how much to update is what you expect the relative magnitude of spillover benefits per person dewormed to be in Miguel and Kremer compared to our charities' programs. This is a complex question, but I'd guess that spillover benefits were slightly larger in Miguel and Kremer. As with Chris, rough estimates of offsetting effects lead me to want to make a ~70% adjustment here. (I'd decrease this adjustment by ~10-20% since I'd guess that spillover benefits were ~10-20% larger in M&K, but that's offset by a ~10-20% adjustment for lower coverage in the average round of the experiment.)   
More details below.  
More detailed background:
- Our CEA uses a treatment-on-the-treated (TOT) framework on the cost side: i.e., we estimate the cost per child actually dewormed by our recommended charities. However, we've been using an intention-to-treat (ITT) framework on the benefits side: We implicitly assumed that Miguel and Kremer 2004 achieved 100% coverage (i.e., the treatment group received the full additional 2.41 years of deworming that were assigned to it), when the actual difference in coverage between treatment and control was much smaller, more like 50-60%. [It seems the average coverage rate for each round was roughly in the 50-60% range in the treatment group and ~1% in the control group. See Table III, Pg. 170, Miguel and Kremer 2004. Note that in the text it says, "Take-up rates were approximately 75% in the treatment group and 5% in the control group." (p. 8), implying a relative difference of ~70% coverage. It seems like that was meant to estimate what portion of the treatment and control groups ever received any treatment, which is less directly comparable to the coverage rates we estimate for our charities' programs.]
- A simple way to inflate the ITT effect to a TOT effect is to divide the ITT effect by the difference in coverage rates between treatment and control. 
- However, there are potential issues with doing this: a major one is that other members of the treatment group may have substantially benefitted from spillovers of deworming treatment (i.e., less transmission of worms since nearby children had been treated). When we estimate the treatment effect under ITT, we're partially counting the direct benefits of treatment but also counting spillover benefits. So it exaggerates the benefits to just divide by the coverage rate; you're attributing a bigger effect for the full sample than we'd actually expect to exist if everyone were treated.  
I think the key question for deciding whether to simply inflate the ITT effect is: How do we expect the magnitude of spillover benefits per person dewormed in Miguel and Kremer to compare to the magnitude of spillover benefits in charities' programs? If the spillover benefits per person dewormed in Miguel and Kremer were equal to or smaller than in our charities' programs, then I think we would not be overestimating the benefits of deworming by using this simple inflator. Otherwise, we are.  
 My current best guess is that the spillover benefits were probably somewhat larger in the Miguel and Kremer study than in our charities' programs. But, I have huge uncertainty about the relative magnitudes; I'd likely need to do more research and formal modeling to answer it well.  
 Several complications with thinking about the magnitude of spillover benefits are:
a) We also expect that the control group may have benefitted from spillovers from the treatment group. This would lead the impact of deworming as measured in M&K relative to the true counterfactual of deworming treatment to be underestimated. Because this was a cluster RCT and worm transmission is geographically determined, it seems likely that the spillover benefits in control clusters were smaller than in untreated children in treatment clusters, but still may have been meaningful.
b) Children who are treated may also benefit from spillovers to some extent: if a higher portion of one's community is treated, then worm transmission may be more effectively reduced, leading to lower reinfection rates. This kind of benefit would also occur in charities' programs.
c) The relative impact of spillovers seems like it would depend substantially on: 1) coverage rates of deworming programs, 2) baseline intensity of worm infections. 
- Re coverage, our charities aim to treat everyone in their target regions and according to our reviews of SCI and Deworm the World Initiative may achieve coverage rates of ~80-95%, a higher coverage rate than was achieved in Miguel and Kremer. This means that there is a smaller untreated portion of the population that could receive spillover benefits. On the other hand, perhaps the relationship between coverage and spillovers is nonlinear: maybe if nearly all people in the community are treated then reinfection rates drop by a nonlinear amount.
- Re intensity, as Chris mentioned, perhaps spillover effects are larger when the worm burden is especially bad, in a nonlinear way. (We're currently adjusting linearly for worm intensity in our CEA.) OTOH, I could see an argument that spillovers are especially good at lower intensity levels, since maybe you could get closer to eliminating any infections.
d) So far I've mainly been thinking about spillovers on school-aged children. However, our model doesn't count any spillover benefits to pre-school-aged children even though they may exist (though could also depend on whether pre-school-aged children are receiving treatment in areas where our charities work). This is another reason you might expect that spillovers in charities' programs are actually larger than the amount of spillovers that we're including in our estimate when inflating the M&K effect.  
  I made a super rough model that one could play around with here: https://docs.google.com/spreadsheets/d/1oNyirkpFB-HYbXVMorloEZYpNm8k4XTFKV9egjXIPA4/edit?usp=sharing. I think there are probably major issues with it, so not putting weight on it, but it didn't make it seem like obviously we should expect much smaller spillover benefits in charities' programs than M&K; my current best guess was maybe ~10% less."

[^10]: https://docs.google.com/document/d/1NxN6SO8GNv1AhpHMy1-IGu-tStT7py29W8AFU0u_bLw/edit?ts=599b4840


[^11]: "Our model accounts for changes in the natural log of consumption. The logarithmic model captures the idea that money has diminishing value as you get more and more of it.   
 For example, our model considers a 50% increase in income as a little better than 50% as good as an 100% increase in income." https://www.givewell.org/how-we-work/our-criteria/cost-effectiveness/comparing-moral-weights
