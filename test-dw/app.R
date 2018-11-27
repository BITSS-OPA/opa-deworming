#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 400px; position:relative;",
               numericInput("param1", label = h3("N Sims = "), value = 1e4),
               br("Data"), 
               sliderInput("param2", label = "Govt bonds = ",
                           min = 0.001, max = 0.2, value = gov_bonds), 
               sliderInput("param2_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * gov_bonds, value = 0.1 * gov_bonds), 
               sliderInput("param3", label = "Inflation = ",
                           min = 0.001, max = 0.2, value = inflation), 
               sliderInput("param3_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * inflation, value = 0.1 * inflation), 
               sliderInput("param4", label = "W_ag = ",
                           min = wage_ag_val / 2, max = 2 * wage_ag_val, value = wage_ag_val),
               sliderInput("param4_1", label = "SD = ",
                           min = 0.0000001* wage_ag_val, max = 1 * wage_ag_val, value = 0.1 * wage_ag_val), 
               sliderInput("param5", label = "W_ww = ",
                           min = wage_ww_val / 2, max = 2 * wage_ww_val, value = wage_ww_val),
               sliderInput("param5_1", label = "SD = ",
                           min = 0.0000001* wage_ww_val, max = 1 * wage_ww_val, value = 0.1 * wage_ww_val), 
               sliderInput("param6", label = "Profits se = ",
                           min = profits_se_val / 2, max = 2 * profits_se_val, value = profits_se_val),
               sliderInput("param6_1", label = "SD = ",
                           min = 0.000001* profits_se_val, max = 1 * profits_se_val, value = 0.1 * profits_se_val), 
               sliderInput("param7", label = "Hours se (>0) = ",
                           min = hours_se_cond_val / 2, max = 2 * hours_se_cond_val, value = hours_se_cond_val),
               sliderInput("param7_1", label = "SD = ",
                           min = 0.000001* hours_se_cond_val, max = 1 * hours_se_cond_val, value = 0.1 * hours_se_cond_val), 
               sliderInput("param8", label = "H_ag = ",
                           min = hours_ag_val / 2, max = 2 * hours_ag_val, value = hours_ag_val),
               sliderInput("param8_1", label = "SD = ",
                           min = 0.000001* hours_ag_val, max = 1 * hours_ag_val, value = 0.1 * hours_ag_val), 
               sliderInput("param9", label = "H_ww = ", 
                           min = hours_ww_val / 2, max = 2 * hours_ww_val, value = hours_ww_val),
               sliderInput("param9_1", label = "SD = ", 
                           min = 0.000001* hours_ww_val, max = 1 * hours_ww_val, value = 0.1 * hours_ww_val), 
               sliderInput("param10", label = "H_se = ",
                           min = hours_se_val / 2, max = 2 * hours_se_val, value = hours_se_val),
               sliderInput("param10_1", label = "SD = ",
                           min = 0.000001* hours_se_val, max = 1 * hours_se_val, value = 0.1 * hours_se_val), 
               sliderInput("param11", label = "Exchange rate = ",
                           min = ex_rate_val / 2, max = 2 * ex_rate_val, value = ex_rate_val),
               sliderInput("param11_1", label = "SD = ",
                           min = 0.000001* ex_rate_val, max = 1 * ex_rate_val, value = 0.1 * ex_rate_val), 
               sliderInput("param12", label = "growth = ",
                           min = growth_rate_val / 2, max = 2 * growth_rate_val, value = growth_rate_val),
               sliderInput("param12_1", label = "SD = ",
                           min = 0.000001* growth_rate_val, max = 1 * growth_rate_val, value = 0.1 * growth_rate_val), 
               sliderInput("param13", label = "Coverage (R) = ", 
                           min = coverage_val / 2, max = 2 * coverage_val, value = coverage_val),
               sliderInput("param13_1", label = "SD = ", 
                           min = 0.000001* coverage_val, max = 1 * coverage_val, value = 0.1 * coverage_val), 
               sliderInput("param14", label = "Saturation (p) = ",
                           min = saturation_val / 2, max = 2 * saturation_val, value = saturation_val),
               sliderInput("param14_1", label = "SD = ",
                           min = 0.0000001* saturation_val, max = 1 * saturation_val, value = 0.1 * saturation_val), 
               sliderInput("param15", label = "Tax rate = ",
                           min = tax_val / 2, max = 2 * tax_val, value = tax_val, step = 0.001),
               sliderInput("param15_1", label = "SD = ",
                           min = 0.00001* tax_val, max = 1 * tax_val, value = 0.1 * tax_val), 
               sliderInput("param16", label = "Costs ot T (local $) = ",
                           min = unit_cost_local_val / 2, max = 2 * unit_cost_local_val, value = unit_cost_local_val),
               sliderInput("param16_1", label = "SD = ",                
                           min = 0.000001* unit_cost_local_val, max = 1 * unit_cost_local_val, value = 0.1 * unit_cost_local_val), 
               sliderInput("param17", label = "Years of T = ",
                           min = years_of_treat_val / 2, max = 2 * years_of_treat_val, value = years_of_treat_val),
               sliderInput("param17_1", label = "SD = ",
                           min = 0.000001* years_of_treat_val, max = 1 * years_of_treat_val, value = 0.1 * years_of_treat_val), 
               
               br("Research"),
               numericInput("param18_1", label = h3("Lambda 1_m = "), value = lambda1_vals[1]),
               numericInput("param18_1_1", label = h3("sd = "), value = 0.17),
               numericInput("param18_2", label = h3("Lambda 1_f = "), value = lambda1_vals[2]),
               numericInput("param18_2_1", label = h3("sd = "), value = 0.17),
               sliderInput("param19", label = "Lambda 2 = ",
                           min = 0, max = 2 * lambda2_val, value = lambda2_val * 1),
               sliderInput("param19_1", label = "SD = ",
                           min = 0.0000001* lambda2_val, max = 1 * lambda2_val, value = 0.1 * lambda2_val), 
               sliderInput("param20", label = "Take-up = ",
                           min = q_full_val / 2, max = 2 * q_full_val, value = q_full_val), 
               sliderInput("param20_1", label = "SD = ",
                           min = 0.00000001* q_full_val, max = 1 * q_full_val, value = 0.1 * q_full_val), 
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
               numericInput("param21_1", label = h3("Coef Xp = "), value = coef_exp_val[1]),
               numericInput("param21_1_1", label = h3("SD = "), value = 0.001),
               numericInput("param21_2", label = h3("Coef Xp^2 = "), value = coef_exp_val[2]),
               numericInput("param21_2_1", label = h3("SD = "), value = 0.001),
               sliderInput("param22", label = "Teacher salary = ",
                           min = teach_sal_val / 2, max = 2 * teach_sal_val, value = teach_sal_val),
               sliderInput("param22_1", label = "SD = ",
                           min = 0.00000001* teach_sal_val, max = 1 * teach_sal_val, value = 0.1 * teach_sal_val), 
               sliderInput("param23", label = "Teacher benefits = ",
                           min = teach_ben_val / 2, max = 2 * teach_ben_val, value = teach_ben_val),
               sliderInput("param23_1", label = "SD = ",
                           min = 0.0000001* teach_ben_val, max = 1 * teach_ben_val, value = 0.1 * teach_ben_val), 
               sliderInput("param24", label = "Student per teach = ",
                           min = n_students_val / 2, max = 2 * n_students_val, value = n_students_val),
               sliderInput("param24_1", label = "SD = ",
                           min = 0.0000001* n_students_val, max = 1 * n_students_val, value = 0.1 * n_students_val), 
               sliderInput("param25", label = "Full Saturation (Q(F)) = ",
                           min = full_saturation_val / 2, max = 2 * full_saturation_val, value = full_saturation_val), 
               sliderInput("param25_1", label = "SD = ",
                           min = 0.0000001* full_saturation_val, max = 1 * full_saturation_val, value = 0.1 * full_saturation_val) 
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
    #Defaoult dist: normal, default sd: 0.1* mean
    ## Data 
    gov_bonds_sim <- rnorm(n = nsims, mean = gov_bonds_vari, sd = gov_bonds_sd)	
    inflation_sim <- rnorm(nsims, inflation_vari, inflation_sd)
    wage_ag_val_sim <- rnorm(nsims, wage_ag_vari, wage_ag_sd)
    wage_ww_val_sim <- rnorm(nsims, wage_ww_vari, wage_ww_sd)
    profits_se_val_sim <- rnorm(nsims, profits_se_vari, profits_se_sd)
    hours_se_cond_val_sim <- rnorm(nsims, hours_se_cond_vari, hours_se_cond_sd)
    hours_ag_val_sim <- rnorm(nsims, hours_ag_vari, hours_ag_sd)
    hours_ww_val_sim <- rnorm(nsims, hours_ww_vari, hours_ww_sd)
    hours_se_val_sim <- rnorm(nsims, hours_se_vari, hours_se_sd)
    ex_rate_val_sim <- rnorm(nsims, ex_rate_vari, ex_rate_sd)
    growth_rate_val_sim <- rnorm(nsims, growth_rate_vari, growth_rate_sd)
    coverage_val_sim <- rnorm(nsims, coverage_vari, coverage_sd)
    saturation_val_sim <- rnorm(nsims, saturation_vari, saturation_sd)
    full_saturation_val_sim <- rnorm(nsims, full_saturation_vari, full_saturation_sd) ###Check here later
    tax_val_sim <- rnorm(nsims, tax_vari, tax_sd)
    unit_cost_local_val_sim <- rnorm(nsims, unit_cost_local_vari, unit_cost_local_sd)
    years_of_treat_val_sim <- rnorm(nsims, years_of_treat_vari, years_of_treat_sd)
    
    ## Research
    aux1 <- lapply(1:2,function(x) c(lambda1_vari[x],lambda1_sd[x]) )
    lambda1_vals_sim <- sapply(aux1, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) ) 
    lambda2_val_sim <- rnorm(nsims, lambda2_vari, lambda2_sd)
    q_full_val_sim <- rnorm(nsims, q_full_vari, q_full_sd)
    
    ## Guess work
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #Time from intial period until individual join the labor force
    aux2 <- lapply(1:2,function(x) c(coef_exp_vari[x],coef_exp_sd[x]) )
    coef_exp_val_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
    teach_sal_val_sim <- rnorm(nsims, teach_sal_vari, teach_sal_sd)
    teach_ben_val_sim <- rnorm(nsims, teach_ben_vari, teach_ben_sd)
    n_students_val_sim <- rnorm(nsims, n_students_vari, n_students_sd)
    
    delta_ed_vals_sim <- sapply(delta_ed_vals[,1], function(x)  rnorm(nsims, mean = 
                                                                        x * delta_ed_par1, 
                                                                      sd = delta_ed_sd1 * sd(delta_ed_vals[,1])))
    colnames(delta_ed_vals_sim) <- 1999:2007
    
    delta_ed_ext_vals_sim <- sapply(delta_ed_ext_vals[,1], function(x)  rnorm(nsims, mean = 
                                                                                x * delta_ed_par2, 
                                                                              sd = delta_ed_sd2 * sd(delta_ed_ext_vals[,1])))
    
    colnames(delta_ed_ext_vals_sim) <- 1999:2007
    
    npv_sim <- rep(NA, nsims)
    #yes externality NPV
    for (i in 1:nsims) {
      interst_r_val <- gov_bonds_sim[i] - inflation_sim[i]
      wage_0_val <- wage_0_f(wage_ag = wage_ag_val_sim[i], 
                             wage_ww = wage_ww_val_sim[i], 
                             profits_se = profits_se_val_sim[i], 
                             hours_se_cond = hours_se_cond_val_sim[i], 
                             hours_ag = hours_ag_val_sim[i], 
                             hours_ww = hours_ww_val_sim[i], 
                             hours_se = hours_se_val_sim[i], 
                             ex_rate = ex_rate_val_sim[i])  
      experience_val <- 0:periods_val - time_to_jm_val
      wage_t_val <- wage_t(wage_0 = wage_0_val, 
                           growth_rate = growth_rate_val_sim[i], 
                           experience = experience_val, 
                           coef_exp1 = coef_exp_val_sim[i,1], 
                           coef_exp2 = coef_exp_val_sim[i,2])
      lambda1_vals_aux <- rep(0.5 * lambda1_vals_sim[i,1] + 0.5 * lambda1_vals_sim[i,2], 2)
      lambda2_vals <- rep(lambda2_val_sim[i], 2)
      coverage_val_aux <-  saturation_val_sim[i] / full_saturation_val_sim[i]
      saturation_val_aux <- full_saturation_val_sim[i] * coverage_val_sim[i]
      cost_per_student <- (teach_sal_val_sim[i] + teach_ben_val_sim[i]) / n_students_val_sim[i]
      q2_val_aux <- q_full_val_sim[i]
      s2_val_aux <- ( unit_cost_local_val_sim[i] / ex_rate_val_sim[i] ) * years_of_treat_val_sim[i]
      delta_ed_ext_total_sim <- delta_ed_vals_sim[i,] + delta_ed_ext_vals_sim[i,]
      
      if (include_ext_vari==TRUE){
        delta_ed_final <-  delta_ed_ext_total_sim
      }else{
        delta_ed_final <- delta_ed_vals_sim[i,]
      }
      
      npv_sim[i] <- npv(interest_r = interst_r_val, 
                        wage = wage_t_val, 
                        lambda1_male = lambda1_vals_aux[1], 
                        lambda1_female = lambda1_vals_aux[2], 
                        lambda2_male =  lambda2_vals[1], 
                        lambda2_female =  lambda2_vals[2],
                        coverage = coverage_val_aux,
                        saturation = saturation_val_aux,
                        tax = tax_val_sim[i], 
                        cost_of_schooling=cost_per_student, 
                        delta_ed_male = delta_ed_final, 
                        delta_ed_female = delta_ed_final, 
                        q2 = q2_val_aux, 
                        s2 = s2_val_aux)
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
  } )
  
  
  output$plot1 <- renderPlot({
    npv_sim <- reactive.data1()
    
    #unit test
    if (abs(sd(npv_sim) - 30.72)<0.01 ) {
      plot_title <- "Distribution NPV of Fiscal Impacts of Deworming"
    } else {
      plot_title <- "OUTPUT CHANGE" 
    }
    
    npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
    npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))
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
      annotate("text", x = 80, y = 0.004, label = npv_for_text2, size = 6)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  })
  
}

shinyApp(ui = ui, server = server)



