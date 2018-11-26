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
                           min = 0.00001, max = 0.4 * gov_bonds, value = 0.1 * gov_bonds), 
               sliderInput("param3", label = "Inflation = ",
                           min = 0.001, max = 0.2, value = inflation), 
               sliderInput("param3_1", label = "SD = ",
                           min = 0.001, max = 0.4 * inflation, value = 0.1 * inflation), 
               sliderInput("param4", label = "W_ag = ",
                           min = wage_ag_val / 2, max = 2 * wage_ag_val, value = wage_ag_val),
               sliderInput("param4_1", label = "SD = ",
                           min = 0.01* wage_ag_val, max = 1 * wage_ag_val, value = 0.1 * wage_ag_val), 
               sliderInput("param5", label = "W_ww = ",
                           min = wage_ww_val / 2, max = 2 * wage_ww_val, value = wage_ww_val),
               sliderInput("param5_1", label = "SD = ",
                           min = 0.01* wage_ww_val, max = 1 * wage_ww_val, value = 0.1 * wage_ww_val), 
               sliderInput("param6", label = "Profits se = ",
                           min = profits_se_val / 2, max = 2 * profits_se_val, value = profits_se_val),
               sliderInput("param7", label = "Hours se (>0) = ",
                           min = hours_se_cond_val / 2, max = 2 * hours_se_cond_val, value = hours_se_cond_val),
               sliderInput("param8", label = "H_ag = ",
                           min = hours_ag_val / 2, max = 2 * hours_ag_val, value = hours_ag_val),
               sliderInput("param9", label = "H_ww = ", 
                           min = hours_ww_val / 2, max = 2 * hours_ww_val, value = hours_ww_val),
               sliderInput("param10", label = "H_se = ",
                           min = hours_se_val / 2, max = 2 * hours_se_val, value = hours_se_val),
               sliderInput("param11", label = "Exchange rate = ",
                           min = ex_rate_val / 2, max = 2 * ex_rate_val, value = ex_rate_val),
               sliderInput("param12", label = "growth = ",
                           min = growth_rate_val / 2, max = 2 * growth_rate_val, value = growth_rate_val),
               sliderInput("param13", label = "Coverage (R) = ", 
                           min = coverage_val / 2, max = 2 * coverage_val, value = coverage_val),
               sliderInput("param14", label = "Saturation (p) = ",
                           min = saturation_val / 2, max = 2 * saturation_val, value = saturation_val),
               sliderInput("param15", label = "Tax rate = ",
                           min = tax_val / 2, max = 2 * tax_val, value = tax_val, step = 0.001),
               sliderInput("param16", label = "Costs ot T (local $) = ",
                           min = unit_cost_local_val / 2, max = 2 * unit_cost_local_val, value = unit_cost_local_val),
               sliderInput("param17", label = "Years of T = ",
                           min = years_of_treat_val / 2, max = 2 * years_of_treat_val, value = years_of_treat_val),
               
               br("Research"),
               numericInput("param18_1", label = h3("Lambda 1_m = "), value = lambda1_vals[1]),
               numericInput("param18_2", label = h3("Lambda 1_f = "), value = lambda1_vals[2]),
               sliderInput("param19", label = "Lambda 2 = ",
                           min = 0, max = 2 * lambda2_val, value = lambda2_val),
               sliderInput("param20", label = "Take-up = ",
                           min = q_full_val / 2, max = 2 * q_full_val, value = q_full_val), 
               checkboxInput("checkbox1", label = "Use additional education with externalities", value = TRUE),
               
               br("Guesswork"),
               numericInput("param21_1", label = h3("Coef Xp = "), value = coef_exp_val[1]),
               numericInput("param21_2", label = h3("Coef Xp^2 = "), value = coef_exp_val[2]),
               sliderInput("param22", label = "Teacher salary = ",
                           min = teach_sal_val / 2, max = 2 * teach_sal_val, value = teach_sal_val),
               sliderInput("param23", label = "Teacher benefits = ",
                           min = teach_ben_val / 2, max = 2 * teach_ben_val, value = teach_ben_val),
               sliderInput("param24", label = "Student per teach = ",
                           min = n_students_val / 2, max = 2 * n_students_val, value = n_students_val),
               sliderInput("param25", label = "Full Saturation (Q(F)) = ",
                           min = full_saturation_val / 2, max = 2 * full_saturation_val, value = full_saturation_val)
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
                        hours_se_cond_vari, 
                        hours_ag_vari, 
                        hours_ww_vari,
                        hours_se_vari,
                        ex_rate_vari,
                        growth_rate_vari, 
                        coverage_vari,
                        full_saturation_vari, 
                        saturation_vari,
                        tax_vari, 
                        unit_cost_local_vari, 
                        years_of_treat_vari,
                        lambda1_vari,                   #Research
                        lambda2_vari, 
                        q_full_vari, 
                        coef_exp_vari,                  #Guesswork
                        teach_sal_vari,
                        teach_ben_vari,
                        n_students_vari, 
                        include_ext_vari=TRUE
                        ) {
    set.seed(1234)
    #Defaoult dist: normal, default sd: 0.1* mean
    ## Data 
    gov_bonds_sim <- rnorm(n = nsims, mean = gov_bonds_vari, sd = gov_bonds_sd)	
    inflation_sim <- rnorm(nsims, inflation_vari, inflation_sd)
    wage_ag_val_sim <- rnorm(nsims, wage_ag_vari, wage_ag_sd)
    wage_ww_val_sim <- rnorm(nsims, wage_ww_vari, wage_ww_sd)
    profits_se_val_sim <- rnorm(nsims, profits_se_vari, 0.1 * profits_se_vari)
    hours_se_cond_val_sim <- rnorm(nsims, hours_se_cond_vari, 0.1 * hours_se_cond_vari)
    hours_ag_val_sim <- rnorm(nsims, hours_ag_vari, 0.1 * hours_ag_vari)
    hours_ww_val_sim <- rnorm(nsims, hours_ww_vari, 0.1 * hours_ww_vari)
    hours_se_val_sim <- rnorm(nsims, hours_se_vari, 0.1 * hours_se_vari)
    ex_rate_val_sim <- rnorm(nsims, ex_rate_vari, 0.1 * ex_rate_vari)
    growth_rate_val_sim <- rnorm(nsims, growth_rate_vari, 0.1 * growth_rate_vari)
    coverage_val_sim <- rnorm(nsims, coverage_vari, 0.1 * coverage_vari)
    saturation_val_sim <- rnorm(nsims, saturation_vari, 0.1 * saturation_vari)
    full_saturation_val_sim <- rnorm(nsims, full_saturation_vari, 0.1 * full_saturation_vari)
    tax_val_sim <- rnorm(nsims, tax_vari, 0.1 * tax_vari)
    unit_cost_local_val_sim <- rnorm(nsims, unit_cost_local_vari, 0.1 * unit_cost_local_vari)
    years_of_treat_val_sim <- rnorm(nsims, years_of_treat_vari, 0.1 * years_of_treat_vari)
    
    ## Research
    lambda1_vals_sim <- sapply(lambda1_vari, function(x)  rnorm(nsims, mean = x, sd = 0.1 * x) )
    lambda2_val_sim <- rnorm(nsims, lambda2_vari, 0.1 * lambda2_vari)
    q_full_val_sim <- rnorm(nsims, q_full_vari, 0.1 * q_full_vari)
    
    ## Guess work
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #Time from intial period until individual join the labor force
    coef_exp_val_sim <-  sapply(coef_exp_vari, function(x)  rnorm(nsims, mean = x, sd = 0.001))
    teach_sal_val_sim <- rnorm(nsims, teach_sal_vari, 0.1 * teach_sal_vari)
    teach_ben_val_sim <- rnorm(nsims, teach_ben_vari, 0.1 * teach_ben_vari)
    n_students_val_sim <- rnorm(nsims, n_students_vari, 0.1 * n_students_vari)
    
    delta_ed_vals_sim <- sapply(delta_ed_vals[,1], function(x)  rnorm(nsims, mean = x, sd = sd(delta_ed_vals[,1])))
    colnames(delta_ed_vals_sim) <- 1999:2007
    
    delta_ed_ext_vals_sim <- sapply(delta_ed_ext_vals[,1], function(x)  rnorm(nsims, mean = x, sd = sd(delta_ed_ext_vals[,1])))
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
              hours_se_cond_vari = as.numeric(input$param7), 
              hours_ag_vari = as.numeric(input$param8), 
              hours_ww_vari = as.numeric(input$param9),
              hours_se_vari = as.numeric(input$param10),
              ex_rate_vari = as.numeric(input$param11),
              growth_rate_vari = as.numeric(input$param12),
              coverage_vari = as.numeric(input$param13),
              saturation_vari = as.numeric(input$param14),
              tax_vari = as.numeric(input$param15), 
              unit_cost_local_vari = as.numeric(input$param16), 
              years_of_treat_vari = as.numeric(input$param17),
              lambda1_vari = c(as.numeric(input$param18_1), as.numeric(input$param18_2)),
              lambda2_vari = as.numeric(input$param19), 
              q_full_vari = as.numeric(input$param20), 
              coef_exp_vari = c(as.numeric(input$param21_1), as.numeric(input$param21_2)), 
              teach_sal_vari = as.numeric(input$param22),
              teach_ben_vari = as.numeric(input$param23),
              n_students_vari = as.numeric(input$param24), 
              include_ext_vari = input$checkbox1, 
              full_saturation_vari = as.numeric(input$param25)
    ) 
  } )
  
  output$plot1 <- renderPlot({
    npv_sim <- reactive.data1()
    npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
    ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-30,100)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = "Distribution NPV of Fiscal Impacts of Deworming", 
           subtitle = "With Externalities")+
      annotate("text", x = 70, y = 0.012, label = npv_for_text, size = 6)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  })
  
}

shinyApp(ui = ui, server = server)



