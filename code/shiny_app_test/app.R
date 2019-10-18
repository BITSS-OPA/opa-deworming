#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#here I need to run a script that calls parameters and functions

# Define UI for application that draws a histogram

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



# Define server logic required to draw a histogram

server <- function(input, output) {
    sim.data1 <- function(nsims = 1e4,
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
                          lambda1_va2,
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
                          df_costs_var2,
                          df_costs_cw_var2,
                          staff_time_var2,
                          staff_time_var2_sd,
                          df_counts_var2,
                          delta_ed_var2,
                          delta_ed_ext_var2,
                          teach_sal_var2,
                          teach_sal_var2_sd,
                          teach_ben_var2,
                          teach_ben_var2_sd,
                          n_students_var2,
                          n_students_var2_sd,
                          unit_cost_local_var2,
                          unit_cost_local_var2_sd,
                          years_of_treat_var2,
                          years_of_treat_var2_sd,
                          tax_var2,
                          tax_var2_sd,
                          periods_var2) {
            set.seed(142857)
            nsims <- 1e2
            include_ext_mo <- TRUE
            start_time <- Sys.time()
            ################
            ###### Draws
            ################
            #Defaoult dist: normal, default sd: 0.1* mean
            ## Data
            gov_bonds_sim <-        rnorm(n = nsims, mean = gov_bonds_var2, sd = gov_bonds_var2_sd)
            inflation_sim <-        rnorm(nsims, inflation_var2, inflation_var2_sd)
            
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
            tax_sim <-              rnorm(nsims, tax_var2, tax_var2_sd)
            
            unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_var2, unit_cost_local_var2_sd)
            years_of_treat_sim <-   rnorm(nsims, years_of_treat_var2, years_of_treat_var2_sd)
            
            ## Research
            aux1 <- 0.1 * c(lambda1_var2[1], 0.01)
            # Each list is a pair mean, sd.
            aux2 <- lapply(1:2,function(x) c(lambda1_var2[x], aux1[x] ) )
            lambda1_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )
            lambda2_sim <-          rnorm(nsims, lambda2_var2,  lambda2_var2_sd)
            # New lambdas here
            aux3 <- lapply(1:3,function(x) c(lambda1_new_var2[x], lambda1_new_sd_var2[x] ) )
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
            time_to_jm_val <- 10        #Time from intial period until individual join the labor force
            aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
            coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
            teach_sal_sim <-    rnorm(nsims, teach_sal_var2, teach_sal_var2_sd)
            teach_ben_sim <-    rnorm(nsims, teach_ben_var2, teach_ben_var2_sd)
            n_students_sim <-   rnorm(nsims, n_students_var2, n_students_var2_sd)
            
            delta_ed_sim <- sapply(delta_ed_var2[,1], function(x) rnorm(nsims, mean =
                                                                          x * 1,
                                                                      sd = 1 * sd(delta_ed_var2[,1]) ) )
            colnames(delta_ed_sim) <- 1999:2007
            
            delta_ed_ext_sim <- sapply(delta_ed_ext_var2[,1], function(x)  rnorm(nsims, mean =
                                                                                   x * 1,
                                                                               sd = 1 * sd(delta_ed_ext_var2[,1])))
            colnames(delta_ed_ext_sim) <- 1999:2007
            
            #######
            costs1_counts_in <- costs1_counts_f(df_counts_var = df_counts_var2,
                                                df_costs_cw_var = df_costs_cw_var2)$counts_data
            costs1_counts_sim <- sapply(costs1_counts_in$total, function(x)  rnorm(nsims, mean = x,  sd = 0.1 * x) )
            
            staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      
            
            costs1_costs_in <- lapply(staff_time_sim, function(x) costs1_costs_f(df_costs_var = df_costs_var2,
                                                                                 df_costs_cw_var = df_costs_cw_var2,
                                                                                 staff_time_var = x)$cost_data)
            
            costs1_costs_sim <- t( sapply(costs1_costs_in, function(x)  {
                aux1 <- x$costs_by_country
                rnorm(length(aux1), mean = aux1,  sd = 0.1 * aux1)
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
                
                invisible( list2env(
                    one_run(main_run_var1 = FALSE,
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
                            df_costs_var1 = df_costs_so,
                            df_costs_cw_var1 = df_costs_cw_so,
                            staff_time_var1 = staff_time_so,
                            df_counts_var1 = df_counts_so,
                            delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                            delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                            teach_sal_var1 = teach_sal_sim[i],
                            teach_ben_var1 = teach_ben_sim[i],
                            n_students_var1 = n_students_sim[i],
                            unit_cost_local_var1 = unit_cost_local_sim[i],
                            years_of_treat_var1 = years_of_treat_sim[i],
                            tax_var1 = tax_sim[i],
                            periods_var1 = periods_so),.GlobalEnv) )
                
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
                ea1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = cost1_in)
                # EA2: yes externality NPV using EAs costs
                ea2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = cost1_in)
                # EA3: benef= KLPS all and no ext; Costs=EA
                ea3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = cost1_in)
                #CEA for EA
                cea_no_ext_ea_sim[i]  <- CEA_pe_f(benefits_var = pv_benef_all_nx_in, 
                                                  costs_var = cost1_in, fudging_var = 0)
                rcea_no_ext_ea_sim[i]  <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                                                        costs_var = cost1_in, fudging_var = 0),
                                                     CEA_cash_var = 744 )
            }
            
            total_time <- Sys.time() - start_time
            
            return( list(
                "baird1_sim "        = baird1_sim,         
                "baird2_sim "        = baird2_sim,         
                "baird3_sim "        = baird3_sim,         
                "baird4_sim "        = baird4_sim,         
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
        
    
        #UPDATE VALUES BELOW
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
        
        npv_sim <- reactive.data1() # FORMAT THE OUTPUT AS A DF

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
        

        ################
        ###### Results/Viz
        ################
        
        npv_sim <- npv_sim[[ policy_estimates[3] ]]    
        
        npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
        npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))
        
        ggplot() +
            geom_density(aes(x = npv_sim,
                             alpha = 1/2), kernel = "gau") +
            geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
            coord_cartesian(xlim = 1.2 * c(min(c(-1, npv_sim) ),max(npv_sim))) +
            guides(alpha = "none", colour="none") +
            labs(y = NULL,
                 x = "NPV" ,
                 title = paste0("Distribution of NPV of ", policy_estimates_text[3]
                 ),
                 subtitle = paste0("N = ", nsims, " simulations. Takes ",
                                   round(total_time, 1)," ",attributes(total_time)$unit )  )+
            annotate("text", x = 1.5 * median(npv_sim), y = 0.012, label = npv_for_text, size = 6)+
            annotate("text", x = 1.5 * median(npv_sim), y = 0.004, label = npv_for_text2, size = 6)+
            theme(axis.ticks = element_blank(), axis.text.y = element_blank())
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
