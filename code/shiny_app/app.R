#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

call_params_f <- function(){
  # TO DELETE:
  saturation_so <- 99
  full_saturation_so <- 99
  
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



npv_mo_f <- function(n_male_var = 1/2, n_female_var = 1/2, 
                     interest_r_var = interest_in,
                     wage_var = wage_t_mo,
                     lambda1_male_var = lambda1_so[1],
                     lambda1_female_var = lambda1_so[2], 
                     tax_var = tax_so,
                     saturation_var = saturation_in,             
                     coverage_var = coverage_so,
                     cost_of_schooling_var = cost_per_student_in,
                     delta_ed_male_var = delta_ed_so[,1],
                     delta_ed_female_var = delta_ed_so[,1], 
                     lambda2_male_var = lambda2_in[1],
                     lambda2_female_var = lambda2_in[2],
                     s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                     periods_var = periods_so) {
  ns <- c(n_male_var, n_female_var)
  lambda1s <- c(lambda1_male_var, lambda1_female_var)
  lambda2s <- c(lambda2_male_var, lambda2_female_var)
  index_t <- 0:periods_var
  delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
  delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
  
  benef <- matrix(NA, 51,2)
  for (i in 1:2){
    benef[,i] <- ( 1 / (1 + interest_r_var) )^index_t * wage_var *
      ( lambda1s[i] + saturation_var * lambda2s[i] / coverage_var )
  }
  
  res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
                        apply( ( 1 / (1 + interest_r_var) )^index_t *
                                 delta_ed_s * cost_of_schooling_var, 2, sum) )
  ) - (s2_var * q2_var  - s1_var * q1_var)
  #  wser()
  return(res1)   
}

wage_0_mo_f <- function(wage_ag_var, wage_ww_var, profits_se_var, hours_se_cond_var, 
                        hours_ag_var, hours_ww_var, hours_se_var, ex_rate_var) {
  wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
  wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
  alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) / sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
  res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
  return(res1)
}

wage_t_mo_f <- function(wage_0_var, growth_rate_var, experience_var, 
                        coef_exp1_var, coef_exp2_var) {
  res1 <- 52 * wage_0_var *( ( 1 + growth_rate_var )^experience_var ) *
    ( 1 + coef_exp1_var * experience_var + coef_exp2_var * experience_var^2 ) *
    ifelse(0:periods_so >= time_to_jm_so, 1, 0)
  return(res1)
}


### UP TO HERE: run all the relevan functions on sources. 


ui <- fluidPage(
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-width: 400px; max-height: 400px; position:relative;",
               numericInput("param1", label = h3("N Sims = "), value = 1e4),
               br("Data"), 
               withMathJax(),
               sliderInput("param2", label = "Gov Bonds (\\( i \\))"  ,
                           min = 0.001, max = 0.2, value = gov_bonds_so), 
               sliderInput("param2_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * gov_bonds_so, value = 0.1 * gov_bonds_so), 
               sliderInput("param3", label = "Inflation (\\( \\pi \\) ) = ",
                           min = 0.001, max = 0.2, value = inflation_so), 
               sliderInput("param3_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * inflation_so, value = 0.1 * inflation_so), 
               sliderInput("param4", label = "Agri Wages (\\( w_{ag} \\))",
                           min = wage_ag_so / 2, max = 2 * wage_ag_so, value = wage_ag_so),
               sliderInput("param4_1", label = "SD = ",
                           min = 0.0000001* wage_ag_so, max = 1 * wage_ag_so, value = 0.1 * wage_ag_so), 
               sliderInput("param5", label = "Work-non ag-Wages  (\\( w_{ww} \\))",
                           min = wage_ww_so / 2, max = 2 * wage_ww_so, value = wage_ww_so),
               sliderInput("param5_1", label = "SD = ",
                           min = 0.0000001* wage_ww_so, max = 1 * wage_ww_so, value = 0.1 * wage_ww_so), 
               sliderInput("param6", label = "Profits se = ",
                           min = profits_se_so / 2, max = 2 * profits_se_so, value = profits_se_so),
               sliderInput("param6_1", label = "SD = ",
                           min = 0.000001* profits_se_so, max = 1 * profits_se_so, value = 0.1 * profits_se_so), 
               sliderInput("param7", label = "Hours se (>0) = ",
                           min = hours_se_cond_so / 2, max = 2 * hours_se_cond_so, value = hours_se_cond_so),
               sliderInput("param7_1", label = "SD = ",
                           min = 0.000001* hours_se_cond_so, max = 1 * hours_se_cond_so, value = 0.1 * hours_se_cond_so), 
               sliderInput("param8", label = "H_ag = ",
                           min = hours_ag_so / 2, max = 2 * hours_ag_so, value = hours_ag_so),
               sliderInput("param8_1", label = "SD = ",
                           min = 0.000001* hours_ag_so, max = 1 * hours_ag_so, value = 0.1 * hours_ag_so), 
               sliderInput("param9", label = "H_ww = ", 
                           min = hours_ww_so / 2, max = 2 * hours_ww_so, value = hours_ww_so),
               sliderInput("param9_1", label = "SD = ", 
                           min = 0.000001* hours_ww_so, max = 1 * hours_ww_so, value = 0.1 * hours_ww_so), 
               sliderInput("param10", label = "H_se = ",
                           min = hours_se_so / 2, max = 2 * hours_se_so, value = hours_se_so),
               sliderInput("param10_1", label = "SD = ",
                           min = 0.000001* hours_se_so, max = 1 * hours_se_so, value = 0.1 * hours_se_so), 
               sliderInput("param11", label = "Exchange rate = ",
                           min = ex_rate_so / 2, max = 2 * ex_rate_so, value = ex_rate_so),
               sliderInput("param11_1", label = "SD = ",
                           min = 0.000001* ex_rate_so, max = 1 * ex_rate_so, value = 0.1 * ex_rate_so), 
               sliderInput("param12", label = "growth = ",
                           min = growth_rate_so / 2, max = 2 * growth_rate_so, value = growth_rate_so),
               sliderInput("param12_1", label = "SD = ",
                           min = 0.000001* growth_rate_so, max = 1 * growth_rate_so, value = 0.1 * growth_rate_so), 
               sliderInput("param13", label = "Coverage (R) = ", 
                           min = coverage_so / 2, max = 2 * coverage_so, value = coverage_so),
               sliderInput("param13_1", label = "SD = ", 
                           min = 0.000001* coverage_so, max = 1 * coverage_so, value = 0.1 * coverage_so), 
               sliderInput("param14", label = "Saturation (p) = ",
                           min = saturation_so / 2, max = 2 * saturation_so, value = saturation_so),
               sliderInput("param14_1", label = "SD = ",
                           min = 0.0000001* saturation_so, max = 1 * saturation_so, value = 0.1 * saturation_so), 
               sliderInput("param15", label = "Tax rate = ",
                           min = tax_so / 2, max = 2 * tax_so, value = tax_so, step = 0.001),
               sliderInput("param15_1", label = "SD = ",
                           min = 0.00001* tax_so, max = 1 * tax_so, value = 0.1 * tax_so), 
               sliderInput("param16", label = "Costs ot T (local $) = ",
                           min = unit_cost_local_so / 2, max = 2 * unit_cost_local_so, value = unit_cost_local_so),
               sliderInput("param16_1", label = "SD = ",                
                           min = 0.000001* unit_cost_local_so, max = 1 * unit_cost_local_so, value = 0.1 * unit_cost_local_so), 
               sliderInput("param17", label = "Years of T = ",
                           min = years_of_treat_so / 2, max = 2 * years_of_treat_so, value = years_of_treat_so),
               sliderInput("param17_1", label = "SD = ",
                           min = 0.000001* years_of_treat_so, max = 1 * years_of_treat_so, value = 0.1 * years_of_treat_so), 
               
               br("Research"),
               numericInput("param18_1", label = h3("Lambda 1_m = "), value = lambda1_so[1]),
               numericInput("param18_1_1", label = h3("sd = "), value = 0.17),
               numericInput("param18_2", label = h3("Lambda 1_f = "), value = lambda1_so[2]),
               numericInput("param18_2_1", label = h3("sd = "), value = 0.17),
               sliderInput("param19", label = "Lambda 2 = ",
                           min = 0, max = 2 * lambda2_so, value = lambda2_so * 1),
               sliderInput("param19_1", label = "SD = ",
                           min = 0.0000001* lambda2_so, max = 1 * lambda2_so, value = 0.1 * lambda2_so), 
               sliderInput("param20", label = "Take-up = ",
                           min = q_full_so / 2, max = 2 * q_full_so, value = q_full_so), 
               sliderInput("param20_1", label = "SD = ",
                           min = 0.00000001* q_full_so, max = 1 * q_full_so, value = 0.1 * q_full_so), 
               sliderInput("param28", label = "Take-up with no subsidy = ",
                           min = q_zero_so / 2, max = 2 * q_zero_so, value = q_zero_so), 
               sliderInput("param28_1", label = "SD = ",
                           min = 0.00000001* q_zero_so, max = 1 * q_zero_so, value = 0.1 * q_zero_so), 
               checkboxInput("checkbox1", label = "Use additional education with externalities", value = TRUE),
               sliderInput("param26", label = "x * Delta E = ",
                           min = 0.0000001, max = 4, value = 1), 
               sliderInput("param26_1", label = "SD = ",
                           min = 0.0000001, max = 4, value = 1), 
               sliderInput("param27", label = "x * Delta E (ext)  = ",
                           min = 0.0000001, max = 4, value = 1), 
               sliderInput("param27_1", label = "SD = ",
                           min = 0.0000001, max = 4, value = 1), 
               
               br("Guesswork"),
               numericInput("param21_1", label = h3("Coef Xp = "), value = coef_exp_so[1]),
               numericInput("param21_1_1", label = h3("SD = "), value = 0.001),
               numericInput("param21_2", label = h3("Coef Xp^2 = "), value = coef_exp_so[2]),
               numericInput("param21_2_1", label = h3("SD = "), value = 0.001),
               sliderInput("param22", label = "Teacher salary = ",
                           min = teach_sal_so / 2, max = 2 * teach_sal_so, value = teach_sal_so),
               sliderInput("param22_1", label = "SD = ",
                           min = 0.00000001* teach_sal_so, max = 1 * teach_sal_so, value = 0.1 * teach_sal_so), 
               sliderInput("param23", label = "Teacher benefits = ",
                           min = teach_ben_so / 2, max = 2 * teach_ben_so, value = teach_ben_so),
               sliderInput("param23_1", label = "SD = ",
                           min = 0.0000001* teach_ben_so, max = 1 * teach_ben_so, value = 0.1 * teach_ben_so), 
               sliderInput("param24", label = "Student per teach = ",
                           min = n_students_so / 2, max = 2 * n_students_so, value = n_students_so),
               sliderInput("param24_1", label = "SD = ",
                           min = 0.0000001* n_students_so, max = 1 * n_students_so, value = 0.1 * n_students_so), 
               sliderInput("param25", label = "Full Saturation (Q(F)) = ",
                           min = full_saturation_so / 2, max = 2 * full_saturation_so, value = full_saturation_so), 
               sliderInput("param25_1", label = "SD = ",
                           min = 0.0000001* full_saturation_so, max = 1 * full_saturation_so, value = 0.1 * full_saturation_so) 
               ),
  mainPanel(
    plotOutput("plot1")
  )
)




server <- function(input, output) {
    sim.data1 <- function(nsims = 1e4, 
                          gov_bonds_vari,                #Data
                          gov_bonds_sd,
                          inflation_vari,
                          inflation_sd,
                          wage_ag_vari,
                          wage_ag_sd,
                          wage_ww_vari,
                          wage_ww_sd,
                          profits_se_vari,
                          profits_se_sd,
                          hours_se_cond_vari,
                          hours_se_cond_sd,
                          hours_ag_vari, 
                          hours_ag_sd,
                          hours_ww_vari,
                          hours_ww_sd,
                          hours_se_vari,
                          hours_se_sd,
                          ex_rate_vari,
                          ex_rate_sd,
                          growth_rate_vari, 
                          growth_rate_sd, 
                          coverage_vari,
                          coverage_sd,
                          full_saturation_vari, 
                          full_saturation_sd, 
                          saturation_vari,
                          saturation_sd,
                          tax_vari, 
                          tax_sd, 
                          unit_cost_local_vari, 
                          unit_cost_local_sd, 
                          years_of_treat_vari,
                          years_of_treat_sd,
                          lambda1_vari,                   #Research
                          lambda1_sd, 
                          lambda2_vari,
                          lambda2_sd,
                          q_full_vari, 
                          q_full_sd,
                          q_zero_vari,
                          q_zero_sd,
                          delta_ed_par1,
                          delta_ed_sd1,
                          delta_ed_par2,
                          delta_ed_sd2,
                          coef_exp_vari,                  #Guesswork
                          coef_exp_sd,
                          teach_sal_vari,
                          teach_sal_sd,
                          teach_ben_vari,
                          teach_ben_sd,
                          n_students_vari,
                          n_students_sd, 
                          include_ext_vari=TRUE
    ) {
      
      set.seed(1234)
      nsims <- 1e4
      include_ext_mo <- TRUE
      
      #Defaoult dist: normal, default sd: 0.1* mean
      ## Data 
      gov_bonds_sim <-        rnorm(n = nsims, mean = gov_bonds_vari, sd = gov_bonds_sd)	
      inflation_sim <-        rnorm(nsims, inflation_vari, inflation_sd)
      wage_ag_sim <-          rnorm(nsims, wage_ag_vari, wage_ag_sd)
      wage_ww_sim <-          rnorm(nsims, wage_ww_vari, wage_ww_sd)
      profits_se_sim <-       rnorm(nsims, profits_se_vari, profits_se_sd)
      hours_se_cond_sim <-    rnorm(nsims, hours_se_cond_vari,hours_se_cond_sd)
      hours_ag_sim <-         rnorm(nsims, hours_ag_vari, hours_ag_sd)
      hours_ww_sim <-         rnorm(nsims, hours_ww_vari, hours_ww_sd)
      hours_se_sim <-         rnorm(nsims, hours_se_vari, hours_se_sd)
      ex_rate_sim <-          rnorm(nsims, ex_rate_vari, ex_rate_sd)
      growth_rate_sim <-      rnorm(nsims, growth_rate_vari, growth_rate_sd)
      coverage_sim <-         rnorm(nsims, coverage_vari, coverage_sd)
      tax_sim <-              rnorm(nsims, tax_vari, tax_sd)
      unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_vari, unit_cost_local_sd)
      years_of_treat_sim <-   rnorm(nsims, years_of_treat_vari, years_of_treat_sd)
      
      ## Research
      # Each list is a pair mean, sd. 
      aux2 <- lapply(1:2,function(x) c(lambda1_vari[x],lambda1_sd[x]) )
      lambda1_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) ) 
      lambda2_sim <-          rnorm(nsims, lambda2_vari, lambda2_sd)
      q_full_sim <-           rnorm(nsims, q_full_vari, q_full_sd)
      q_zero_sim <-           rnorm(nsims, q_zero_vari, q_zero_sd)
      
      ## Guess work
      periods_val <- 50           #Total number of periods to forecast wages
      time_to_jm_val <- 10        #Time from intial period until individual join the labor force
      aux2 <- lapply(1:2,function(x) c(coef_exp_vari[x],coef_exp_sd[x]) )
      coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
      teach_sal_sim <- rnorm(nsims, teach_sal_vari, teach_sal_sd)
      teach_ben_sim <- rnorm(nsims, teach_ben_vari, teach_ben_sd)
      n_students_sim <- rnorm(nsims, n_students_vari, n_students_sd)
      
      delta_ed_sim <- sapply(delta_ed_so[,1], function(x) rnorm(nsims, mean = 
                                                                  x * delta_ed_par1, 
                                                                sd = delta_ed_sd1 * sd(delta_ed_so[,1]) ) )
      colnames(delta_ed_sim) <- 1999:2007
      
      delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1], function(x)  rnorm(nsims, mean = 
                                                                           x * delta_ed_par2, 
                                                                         sd = delta_ed_sd2 * sd(delta_ed_ext_so[,1])))
      colnames(delta_ed_ext_sim) <- 1999:2007
          
      ########
      npv_sim <- rep(NA, nsims)
      
      #to loop over:
      # HERE IS WHERE I WAS DOING THE RUNS 
      for (i in 1:nsims) {
        
        #1 - r
        interest_in <- gov_bonds_sim[i] - inflation_sim[i]
        #2 - w
        experience_aux <- 0:periods_so - time_to_jm_so
        
        wage_0_mo <- wage_0_mo_f(wage_ag_var = wage_ag_sim[i],  
                                 wage_ww_var = wage_ww_sim[i],
                                 profits_se_var = profits_se_sim[i],
                                 hours_se_cond_var = hours_se_cond_sim[i],  
                                 hours_ag_var = hours_ag_sim[i],
                                 hours_ww_var = hours_ww_sim[i],
                                 hours_se_var = hours_se_sim[i],
                                 ex_rate_var = ex_rate_sim[i])  
        
        #close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
        #but I suspect diff due to computational precision
        wage_t_mo <- wage_t_mo_f(wage_0_var = wage_0_mo,
                                 growth_rate_var = growth_rate_sim[i],
                                 experience_var = experience_aux,
                                 coef_exp1_var = coef_exp_sim[i,1],
                                 coef_exp2_var = coef_exp_sim[i,2])
        
        #3 - lambda
        lambda1_in <- rep(0.5 * lambda1_sim[i,1] + 0.5 *lambda1_sim[i,2], 2)
        lambda2_in <- rep(lambda2_sim[i], 2)
        
        #4 - R and p
        saturation_in <- coverage_sim[i] * q_full_sim[i] + ( 1 - coverage_sim[i] ) * q_zero_sim[i]
        
        #5 - K and ΔE⎯⎯⎯⎯γt(S1,S2)
        cost_per_student_in <- (teach_sal_sim[i] + teach_ben_sim[i]) / n_students_sim[i]
        
        # Nothing here yet with delta_ed_vals, but would like to incorporate model from Joan
        delta_ed_ext_total_in <- delta_ed_ext_sim[i,] + delta_ed_sim[i,]
        
        if (include_ext_mo == TRUE){
          delta_ed_final_in <-  delta_ed_ext_total_in
        }else{
          delta_ed_final_in <- delta_ed_var[i,]
        }
        
        #6 - (S2Q(S2)−S1Q(S1))
        s2_in <- ( unit_cost_local_sim[i] / ex_rate_sim[i] ) * years_of_treat_sim[i]
        q2_in <- q_full_sim[i]
        
        npv_sim[i] <- npv_mo_f(n_male_var = 1/2, n_female_var = 1/2, 
                               interest_r_var = interest_in,
                               wage_var = wage_t_mo,
                               lambda1_male_var = lambda1_in[1],
                               lambda1_female_var = lambda1_in[2], 
                               tax_var = tax_sim[i],
                               saturation_var = saturation_in,             
                               coverage_var = coverage_sim[i],
                               cost_of_schooling_var = cost_per_student_in,
                               delta_ed_male_var = delta_ed_final_in,
                               delta_ed_female_var = delta_ed_final_in, 
                               lambda2_male_var = lambda2_in[1],
                               lambda2_female_var = lambda2_in[2],
                               s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
                               periods_var = periods_so)
        ##################
        
      }
      
      return(npv_sim)
    }

      
   
  reactive.data1 <- reactive( {
    sim.data1(nsims = as.numeric(input$param1), 
              gov_bonds_vari = as.numeric(input$param2), 
              gov_bonds_sd = as.numeric(input$param2_1),
              inflation_vari = as.numeric(input$param3),
              inflation_sd = as.numeric(input$param3_1),
              wage_ag_vari = as.numeric(input$param4),
              wage_ag_sd = as.numeric(input$param4_1),
              wage_ww_vari = as.numeric(input$param5),
              wage_ww_sd = as.numeric(input$param5_1),
              profits_se_vari = as.numeric(input$param6), 
              profits_se_sd = as.numeric(input$param6_1), 
              hours_se_cond_vari = as.numeric(input$param7), 
              hours_se_cond_sd = as.numeric(input$param7_1), 
              hours_ag_vari = as.numeric(input$param8), 
              hours_ag_sd = as.numeric(input$param8_1), 
              hours_ww_vari = as.numeric(input$param9),
              hours_ww_sd = as.numeric(input$param9_1),
              hours_se_vari = as.numeric(input$param10),
              hours_se_sd = as.numeric(input$param10_1),
              ex_rate_vari = as.numeric(input$param11),
              ex_rate_sd = as.numeric(input$param11_1),
              growth_rate_vari = as.numeric(input$param12),
              growth_rate_sd = as.numeric(input$param12_1),
              coverage_vari = as.numeric(input$param13),
              coverage_sd = as.numeric(input$param13_1),
              saturation_vari = as.numeric(input$param14),
              saturation_sd = as.numeric(input$param14_1),
              tax_vari = as.numeric(input$param15), 
              tax_sd = as.numeric(input$param15_1), 
              unit_cost_local_vari = as.numeric(input$param16), 
              unit_cost_local_sd = as.numeric(input$param16_1), 
              years_of_treat_vari = as.numeric(input$param17),
              years_of_treat_sd = as.numeric(input$param17_1),
              lambda1_vari = c(as.numeric(input$param18_1), as.numeric(input$param18_2)),
              lambda1_sd = c(as.numeric(input$param18_1_1), as.numeric(input$param18_2_1)),
              lambda2_vari = as.numeric(input$param19), 
              lambda2_sd = as.numeric(input$param19_1), 
              q_full_vari = as.numeric(input$param20), 
              q_full_sd = as.numeric(input$param20_1), 
              q_zero_vari = as.numeric(input$param28), 
              q_zero_sd = as.numeric(input$param28_1), 
              coef_exp_vari = c(as.numeric(input$param21_1), as.numeric(input$param21_2)), 
              coef_exp_sd = c(as.numeric(input$param21_1_1), as.numeric(input$param21_2_1)), 
              teach_sal_vari = as.numeric(input$param22),
              teach_sal_sd = as.numeric(input$param22_1),
              teach_ben_vari = as.numeric(input$param23),
              teach_ben_sd = as.numeric(input$param23_1),
              n_students_vari = as.numeric(input$param24), 
              n_students_sd = as.numeric(input$param24_1), 
              include_ext_vari = input$checkbox1, 
              full_saturation_vari = as.numeric(input$param25),
              full_saturation_sd = as.numeric(input$param25_1),
              delta_ed_par1 = as.numeric(input$param26),
              delta_ed_sd1 = as.numeric(input$param26_1),
              delta_ed_par2 = as.numeric(input$param27),
              delta_ed_sd2 = as.numeric(input$param27_1)
    ) 
  } 
)
  
  
  output$plot1 <- renderPlot({
    npv_sim <- reactive.data1()
    
    #unit test
    if (TRUE) {
      plot_title <- "Distribution NPV of Fiscal Impacts of Deworming"
    } else {
      plot_title <- "OUTPUT CHANGE" 
    }
    
    npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
    #npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))
    ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-30,100)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = plot_title, 
           subtitle = "With Externalities")+
      annotate("text", x = 70, y = 0.012, label = npv_for_text, size = 6)+
      #annotate("text", x = 80, y = 0.004, label = npv_for_text2, size = 6)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  })
  
}

shinyApp(ui = ui, server = server)



