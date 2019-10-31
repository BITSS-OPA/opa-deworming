
library(shiny)

shinyServer( function(input, output, session) {
  #Dynamic UI
  
  output$data_in <- renderUI({
    output <- tagList()
    output [[1]] <- sliderInput("param2", label = "Gov Bonds (\\( i \\))"  ,
                                min = 0.001, max = 0.2, value = gov_bonds_so)
  })
  
  
  
  reactive.data1 <- reactive( {
    sim.data1(
      nsims = as.numeric(input$param1),                                                    
      gov_bonds_var2 = as.numeric(input$param2),                                           
      gov_bonds_var2_sd = as.numeric(input$param2_1),                                      
      inflation_var2 = as.numeric(input$param3),                                           
      inflation_var2_sd = as.numeric(input$param3_1),                                      
      wage_ag_var2 = as.numeric(input$param4),                                             
      wage_ag_var2_sd = as.numeric(input$param4_1),                                        
      wage_ww_var2 = as.numeric(input$param5),                                             
      wage_ww_var2_sd = as.numeric(input$param5_1),                                        
      profits_se_var2 = as.numeric(input$param6),                                          
      profits_se_var2_sd = as.numeric(input$param6_1),                                     
      hours_se_cond_var2 = as.numeric(input$param7),                                       
      hours_se_cond_var2_sd = as.numeric(input$param7_1),                                  
      hours_ag_var2 = as.numeric(input$param8),                                            
      hours_ag_var2_sd = as.numeric(input$param8_1),                                       
      hours_ww_var2 = as.numeric(input$param9),                                            
      hours_ww_var2_sd = as.numeric(input$param9_1),                                       
      hours_se_var2 = as.numeric(input$param10),                                           
      hours_se_var2_sd = as.numeric(input$param10_1),                                      
      ex_rate_var2 = as.numeric(input$param11),                                            
      ex_rate_var2_sd = as.numeric(input$param11_1),                                       
      growth_rate_var2 = as.numeric(input$param12),                                        
      growth_rate_var2_sd = as.numeric(input$param12_1),  
      coverage_var2 = as.numeric(input$param13), 
      coverage_var2_sd = as.numeric(input$param13_1),
      tax_var2 = as.numeric(input$param15),                                                 
      tax_var2_sd = as.numeric(input$param15_1),                                            
      unit_cost_local_var2 = as.numeric(input$param16),                                           
      unit_cost_local_var2_sd = as.numeric(input$param16_1),                                           
      years_of_treat_var2 = as.numeric(input$param17),                                          
      years_of_treat_var2_sd = as.numeric(input$param17_1),                                          
      lambda1_var2 = c(as.numeric(input$param18_1), as.numeric(input$param18_2)),                                          
      lambda1_var2_sd = c(as.numeric(input$param18_1_1), as.numeric(input$param18_2_1)),                                     
      lambda2_var2 = as.numeric(input$param19),                                             
      lambda2_var2_sd = as.numeric(input$param19_1),                                        
      q_full_var2 = as.numeric(input$param20),                                              
      q_full_var2_sd = as.numeric(input$param20_1),                                           
      coef_exp_var2 = c(as.numeric(input$param21_1), as.numeric(input$param21_2)),                      
      teach_sal_var2 = as.numeric(input$param22),                                             
      teach_sal_var2_sd = as.numeric(input$param22_1),                                        
      teach_ben_var2 = as.numeric(input$param23),                                             
      teach_ben_var2_sd = as.numeric(input$param23_1),                                        
      n_students_var2 = as.numeric(input$param24),                                            
      n_students_var2_sd = as.numeric(input$param24_1),                                       
      delta_ed_var2 = as.numeric(input$param26),                                              
      delta_ed_var2_sd = as.numeric(input$param26_1),                                             
      delta_ed_ext_var2 = as.numeric(input$param27),                                              
      delta_ed_ext_var2_sd = as.numeric(input$param27_1),                                               
      q_zero_var2 = as.numeric(input$param28),                                                
      q_zero_var2_sd = as.numeric(input$param28_1), 
      lambda1_new_var2 = c(as.numeric(input$param29_1), as.numeric(input$param29_2), 
                           as.numeric(input$param29_3)),                   
      lambda1_new_var2_sd = c(as.numeric(input$param29_1_1), as.numeric(input$param29_2_1), 
                              as.numeric(input$param29_3_1)),             
      alpha_0_var2 = as.numeric(input$param30),    
      alpha_0_var2_sd = as.numeric(input$param30_1), 
      alpha_r_var2 = as.numeric(input$param31),    
      alpha_r_var2_sd = as.numeric(input$param31_1),                                                                         
      counts_par_var2 = as.numeric(input$param32), 
      counts_par_var2_sd = as.numeric(input$param32_1),
      staff_time_var2 = as.numeric(input$param33), 
      staff_time_var2_sd = as.numeric(input$param33_1), 
      costs_par_var2 = as.numeric(input$param34), 
      costs_par_var2_sd = as.numeric(input$param34_1) 
      )
    } 
  )
  
  ################
  ###### Results/Viz
  ################
  # THINK ABOUT WRAPPING THE OUTPUT IN ONE COMMOMN FUNCTION ACROSS DD AND APP
  output$plot1 <- renderPlot({      
    npv_sim_all <- reactive.data1()
    
    total_time <- npv_sim_all$total_time
    position <- which( policy_estimates_text == input$policy_est)
    npv_sim <- npv_sim_all[[ policy_estimates[position] ]]    
    npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
    npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))
    
    plot1 <- ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-10, 400)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = paste0("Distribution of NPV of ", policy_estimates_text[position]
           ),
           subtitle = paste0("N = ", input$param1, " simulations. Takes ",
                             round(total_time, 1)," ",attributes(total_time)$unit )  )+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.012, label = npv_for_text, size = 6)+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.004, label = npv_for_text2, size = 6)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    if (input$rescale == TRUE) {
      plot1 <- suppressMessages( plot1 + coord_cartesian(xlim = 1.2 * c( min( c(-1, npv_sim) ), max( c(100, npv_sim) ))) )
      }
    print(plot1)  
    }, height = 800, width = 800 )

})
