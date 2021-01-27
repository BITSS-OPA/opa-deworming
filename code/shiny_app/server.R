
library(shiny)

shinyServer( function(input, output, session) {
  #Dynamic UI


  #Show/hide SDs code.
  onclick("toggleDataSDs",
          lapply(
            c("SD1", "SD2", "SD3", "SD4","SD12", "SD13",
              "SD15","SD39"),
            toggle, anim=TRUE)
          )

  onclick("toggleResearchSDs",
          lapply(c("SD5", "SD6", "SD7","SD8", "SD9", "SD10","SD11","SD14",
                   "SD16","SD17","SD18", "SD20", "SD21","SD22",
                   "SD23", "SD24", "SD25", "SD27", "SD28",
                   "SD29", "SD32","SD33","SD34", "SD35", "SD36"), toggle, anim=TRUE))

  onclick("toggleGWSDs",
          lapply(c("SD36","SD19","SD37"), toggle, anim=TRUE))


# Generate reactive simulated data for plotting

  reactive.data1<- reactive( {
    sim_data1_f(
      nsims = as.numeric(input$param_num_of_sim),
      gov_bonds_var2     = as.numeric(input$param_gov_bonds16),
      gov_bonds_sd_var2  = as.numeric(input$param_gov_bonds16_sd),
      inflation_var2     = as.numeric(input$param_inflation16),
      inflation_sd_var2  = as.numeric(input$param_inflation16_sd),
      gov_bonds_new_var2    = as.numeric(input$param_gov_bonds19),
      gov_bonds_new_sd_var2 = as.numeric(input$param_gov_bonds19_sd),
      inflation_new_var2    = as.numeric(input$param_inflation19),
      inflation_new_sd_var2 = as.numeric(input$param_inflation19_sd),
      wage_ag_var2 = as.numeric(input$param_wage_ag),
      wage_ag_sd_var2 = as.numeric(input$param_wage_ag_sd),
      wage_ww_var2 = as.numeric(input$param_wage_non_ag),
      wage_ww_sd_var2 = as.numeric(input$param_wage_non_ag_sd),
      profits_se_var2 = as.numeric(input$param_profits_se),
      profits_se_sd_var2 = as.numeric(input$param_profits_se_sd),
      hours_se_cond_var2 = as.numeric(input$param_hours_se_cond),
      hours_se_cond_sd_var2 = as.numeric(input$param_hours_se_cond_sd),
      hours_ag_var2 = as.numeric(input$param_hours_ag),
      hours_ag_sd_var2 = as.numeric(input$param_hours_ag_sd),
      hours_ww_var2 = as.numeric(input$param_hours_ww),
      hours_ww_sd_var2 = as.numeric(input$param_hours_ww_sd),
      hours_se_var2 = as.numeric(input$param_hours_se),
      hours_se_sd_var2 = as.numeric(input$param_hours_se_sd),
      ex_rate_var2 = as.numeric(input$param_ex_rate),
      ex_rate_sd_var2 = as.numeric(input$param_ex_rate_sd),
      growth_rate_var2 = as.numeric(input$param_growth_rate),
      growth_rate_sd_var2 = as.numeric(input$param_growth_rate_sd),
      coverage_var2 = as.numeric(input$param_coverage),
      coverage_sd_var2 = as.numeric(input$param_coverage_sd),
      tax_var2 = as.numeric(input$param_tax),
      tax_sd_var2 = as.numeric(input$param_tax_sd),
      unit_cost_local_var2 = as.numeric(input$param_unit_cost_local),
      unit_cost_local_sd_var2 = as.numeric(input$param_unit_cost_local_sd),
      unit_cost_local_new_var2 = as.numeric(input$param_unit_cost_2017usdppp),
      unit_cost_local_new_sd_var2 = as.numeric(input$param_unit_cost_2017usdppp_sd),
      years_of_treat_0_var2   = as.numeric(input$param_years_of_treat_0)        ,
      years_of_treat_0_sd_var2= as.numeric(input$param_years_of_treat_0_sd)     ,
      years_of_treat_t_var2   = as.numeric(input$param_years_of_treat_t)   ,
      years_of_treat_t_sd_var2= as.numeric(input$param_years_of_treat_t_sd)     ,
      lambda1_var2 = c(as.numeric(input$param_lambda1_male), as.numeric(input$param_lambda1_female)),
      lambda1_sd_var2 = c(as.numeric(input$param_lambda1_male_sd), as.numeric(input$param_lambda1_female_sd)),
      lambda2_var2 = as.numeric(input$param_lambda2),
      lambda2_sd_var2 = as.numeric(input$param_lambda2_sd),
      q_full_var2 = as.numeric(input$param_q_full),
      q_full_sd_var2 = as.numeric(input$param_q_full_sd),
      coef_exp_var2 = c(as.numeric(input$param_coef_exp1), as.numeric(input$param_coef_exp2)),
      teach_sal_var2    = as.numeric(input$param_teach_sal),
      teach_sal_sd_var2 = as.numeric(input$param_teach_sal_sd),
      teach_ben_var2    = as.numeric(input$param_teach_ben),
      teach_ben_sd_var2 = as.numeric(input$param_teach_ben_sd),
      teach_sal_new_var2     = (50000 * 12 / 49.773),                   # change to match DD
      teach_sal_new_sd_var2  = (50000 * 12 / 49.773) * 0.1,             # change to match DD
      teach_ben_new_var2     = 0,                                       # change to 0 to match DD
      teach_ben_new_sd_var2  = 0.000001  ,                              # change to 0.000001 to match DD
      n_students_var2 = as.numeric(input$param_n_students),
      n_students_sd_var2 = as.numeric(input$param_n_students_sd),
      delta_ed_var2 = as.numeric(input$param_delta_ed_par),
      delta_ed_sd_var2 = as.numeric(input$param_delta_ed_par_sd),
      delta_ed_ext_var2 = as.numeric(input$param_delta_ed__ext_par),
      delta_ed_ext_sd_var2 = as.numeric(input$param_delta_ed__ext_par_sd),
      q_zero_var2 = as.numeric(input$param_q_zero),
      q_zero_sd_var2 = 0.001,                                            # change to 0.001 to match DD
      lambda1_new_var2 = as.numeric(input$param_lambda1_new),
      lambda1_new_sd_var2 = as.numeric(input$param_lambda1_new_sd),
      prevalence_0_var2 = as.numeric(input$param_prevl_0),
      prevalence_0_sd_var2 = as.numeric(input$param_prevl_0_sd),
#     prevalence_r_var2 = as.numeric(input$param_prevl_r),
      prevalence_r_var2 = 1,
      prevalence_r_sd_var2 = as.numeric(input$param_prevl_r_sd),
      counts_par_var2 = as.numeric(input$param_counts_par),
      counts_par_sd_var2 = as.numeric(input$param_counts_par_sd),
      staff_time_var2 = as.numeric(input$param_staff_time),
      staff_time_sd_var2 = as.numeric(input$param_staff_time_sd),
      costs_par_var2 = as.numeric(input$param_costs_par),
      costs_par_sd_var2 = as.numeric(input$param_costs_par_sd),
      new_costs_var2 = as.numeric(input$param_costs2_ea),
      new_costs_sd_var2 = as.numeric(input$param_costs2_ea_sd),
      new_prevl_r_var2 = as.numeric(input$param_prevl_r),
      new_prevl_r_sd_var2 = as.numeric(input$param_prevl_r_sd),
      countries_var2 = list("india", "kenya", "nigeria", "vietnam")  # = input$param36  to make it interactive


    )
  }
  )
  # Export Input Parameter Values
  output$downloadParams <- downloadHandler(
    filename = "export.txt",
    content = function(file) {
      inputList <- names(reactiveValuesToList(input))
      inputLabel <- inputMaster[inputList]
      exportVars <- paste0(inputLabel, " = ", sapply(inputList, function(inpt) input[[inpt]]))
      write(exportVars, file)
    })



  # Export All Assumption Plot
  output$downloadPlotAll <- downloadHandler(filename = function() {
    "plotAll.png"
  },
  content = function(file) {
    ggsave(file, plotInputAll(), height = 8, width = 12)
  })

  # Export Key Assumption Plot
  output$downloadPlotKA <- downloadHandler(filename = function() {
    "plotKA.png"
  },
  content = function(file) {
    ggsave(file, plotInputKA(), height = 8, width = 12)
  })

  # Sync cost variable for Key Assumptions and All Assumptions

  observeEvent(
    input$param_ka_costs2_ea,
    updateSliderInput(session, "param_costs2_ea", value = input$param_ka_costs2_ea)
  )

  observeEvent(
    input$param_costs2_ea,
    updateSliderInput(session, "param_ka_costs2_ea", value = input$param_costs2_ea)
  )



  # Sync prevalence variable for Key Assumptions and All Assumptions

  observeEvent(
    input$param_ka_prevl_r,
    updateSliderInput(session, "param_prevl_r", value = input$param_ka_prevl_r)
  )

  observeEvent(
    input$param_prevl_r,
    updateSliderInput(session, "param_ka_prevl_r", value = input$param_prevl_r)
  )

  # Sync length of treatment in new environment for Key Assumptions and All Assumptions

  observeEvent(
    input$param_ka_years_of_treat_t,
    updateNumericInput(session, "param_years_of_treat_t", value = input$param_ka_years_of_treat_t)

  )

  observeEvent(
    input$param_years_of_treat_t,
    updateSliderInput(session, "param_ka_years_of_treat_t", value = input$param_years_of_treat_t)
  )

  # Reset all inputs from Key Assumption tab
  observeEvent(input$resetKA, {reset("KA")})

  # Reset all inputs from All Assumption tab
  observeEvent(input$resetAll, {reset("All")})

  # Show/hide components of each model
  observeEvent(input$policy_est,{
    # all params
    list_master <- c(
      "param_gov_bonds16",                                             #Data
      "param_gov_bonds16_sd",
      "param_gov_bonds19",
      "param_gov_bonds19_sd",
      "param_inflation16",
      "param_inflation16_sd",
      "param_inflation19",
      "param_inflation19_sd",
      "param_wage_ag",
      "param_wage_ag_sd",
      "param_wage_non_ag",
      "param_wage_non_ag_sd",
      "param_profits_se",
      "param_profits_se_sd",
      "param_hours_se_cond",
      "param_hours_se_cond_sd",
      "param_hours_ag",
      "param_hours_ag_sd",
      "param_hours_ww",
      "param_hours_ww_sd",
      "param_hours_se",
      "param_hours_se_sd",
      "param_ex_rate",
      "param_ex_rate_sd",
      "param_growth_rate",
      "param_growth_rate_sd",
      "param_coverage",
      "param_coverage_sd",
      "param_tax",
      "param_tax_sd",
      "param_unit_cost_local",
      "param_unit_cost_local_sd",
      "param_unit_cost_2017usdppp",
      "param_unit_cost_2017usdppp_sd",
      "param_years_of_treat_0",
      "param_years_of_treat_0_sd",
      "param_years_of_treat_t",
      "param_years_of_treat_t_sd",
      "param_lambda1_male",                                          #Research
      "param_lambda1_male_sd",
      "param_lambda1_female",
      "param_lambda1_female_sd",
      "param_q_full",
      "param_q_full_sd",
      "param_q_zero",
      "param_delta_ed_par",
      "param_delta_ed_par_sd",
      "param_q_zero",
      "param_prevl_0",
      "param_prevl_0_sd",
      "param_coef_exp1",                                          #Guesswork
      "param_coef_exp2",
      "param_teach_sal",
      "param_teach_sal_sd",
      "param_teach_ben",
      "param_teach_ben_sd",
      "param_n_students",
      "param_n_students_sd",
      "param_prevl_r",
      "param_prevl_r_sd",
      "param_counts_par",
      "param_counts_par_sd",
      "param_costs_par",
      "param_costs_par_sd",
      "param_lambda2",
      "param_lambda2_sd",
      "param_delta_ed__ext_par",
      "param_delta_ed__ext_par_sd",
      "param_lambda1_new",
      "param_lambda1_new_sd",
      "param_staff_time",
      "param_staff_time_sd"
    )
    if (input$policy_est == "A1. Tax revenue") {
      # remove: counts adj, costs adj, lambda 2, delda ed w/ext, new lambdas,
      # costs due to staff,
      # new gov bonds, new inflation, new cost of teaching,
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda2",
                     "param_lambda2_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_staff_time",
                     "param_staff_time_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_unit_cost_2017usdppp",
                     "param_unit_cost_2017usdppp_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero")
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A1. With externalities. Tax") {
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_staff_time",
                     "param_staff_time_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_unit_cost_2017usdppp",
                     "param_unit_cost_2017usdppp_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero")
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A1. All income") {
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda2",
                     "param_lambda2_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_staff_time",
                     "param_staff_time_sd",
                     "param_tax",
                     "param_tax_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_unit_cost_2017usdppp",
                     "param_unit_cost_2017usdppp_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero")
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A1. With ext. All income") {
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_staff_time",
                     "param_staff_time_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_unit_cost_2017usdppp",
                     "param_unit_cost_2017usdppp_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero")
      list_show <- list_master[ - which(list_master %in% list_hide)]


    } else if (input$policy_est == "A2. Tax") {
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda2",
                     "param_lambda2_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_wage_ag",
                     "param_wage_ag_sd",
                     "param_wage_non_ag",
                     "param_wage_non_ag_sd",
                     "param_profits_se",
                     "param_profits_se_sd",
                     "param_hours_se_cond",
                     "param_hours_se_cond_sd",
                     "param_hours_ag",
                     "param_hours_ag_sd",
                     "param_hours_ww",
                     "param_hours_ww_sd",
                     "param_hours_se",
                     "param_hours_se_sd",
                     "param_growth_rate",
                     "param_growth_rate_sd",
                     "param_coverage",
                     "param_coverage_sd",
                     "param_lambda1_male",
                     "param_lambda1_male_sd",
                     "param_lambda1_female",
                     "param_lambda1_female_sd",
                     "param_staff_time",
                     "param_staff_time_sd" ,
                     "param_gov_bonds16",
                     "param_gov_bonds16_sd",
                     "param_inflation16",
                     "param_inflation16_sd",
                     "param_unit_cost_local",
                     "param_profits_se_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero"
      )
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A2. All income") {
      list_hide <- c("param_counts_par",
                     "param_counts_par_sd",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_lambda2",
                     "param_lambda2_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_wage_ag",
                     "param_wage_ag_sd",
                     "param_wage_non_ag",
                     "param_wage_non_ag_sd",
                     "param_profits_se",
                     "param_profits_se_sd",
                     "param_hours_se_cond",
                     "param_hours_se_cond_sd",
                     "param_hours_ag",
                     "param_hours_ag_sd",
                     "param_hours_ww",
                     "param_hours_ww_sd",
                     "param_hours_se",
                     "param_hours_se_sd",
                     "param_growth_rate",
                     "param_growth_rate_sd",
                     "param_coverage",
                     "param_coverage_sd",
                     "param_lambda1_male",
                     "param_lambda1_male_sd",
                     "param_lambda1_female",
                     "param_lambda1_female_sd",
                     "param_staff_time",
                     "param_staff_time_sd",
                     "param_tax",
                     "param_tax_sd",
                     "param_gov_bonds16",
                     "param_gov_bonds16_sd",
                     "param_inflation16",
                     "param_inflation16_sd",
                     "param_unit_cost_local",
                     "param_unit_cost_local_sd",
                     "param_prevl_0",
                     "param_prevl_0_sd",
                     "param_q_zero")
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A3. All income of A1") {
      list_hide <- c("param_lambda2",
                     "param_lambda2_new",
                     "param_teach_sal",
                     "param_teach_sal_sd",
                     "param_teach_ben",
                     "param_teach_ben_sd",
                     "param_n_students",
                     "param_n_students_sd",
                     "param_delta_ed_par",
                     "param_delta_ed_par_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_unit_cost_local",
                     "param_unit_cost_local_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_q_zero",
                     "param_costs_par",
                     "param_costs_par_sd")
      list_show <- list_master[ - which(list_master %in% list_hide)]
    } else if (input$policy_est == "A3. All income of A1, with ext.") {
      list_hide <- c("param_teach_sal",
                     "param_teach_sal_sd",
                     "param_teach_ben",
                     "param_teach_ben_sd",
                     "param_n_students",
                     "param_n_students_sd",
                     "param_delta_ed_par",
                     "param_delta_ed_par_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_lambda1_new",
                     "param_lambda1_new_sd",
                     "param_unit_cost_local",
                     "param_unit_cost_local_sd",
                     "param_gov_bonds19",
                     "param_gov_bonds19_sd",
                     "param_inflation19",
                     "param_inflation19_sd",
                     "param_q_zero",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_unit_cost_local",
                     "param_unit_cost_local_sd")
      list_show <- list_master[ - which(list_master %in% list_hide)]

    } else if (input$policy_est == "A3. All income of A2. Main Policy Estimate") {
      list_hide <- c("param_lambda2",
                     "param_lambda2_sd",
                     "param_coef_exp1",
                     "param_coef_exp2",
                     "param_teach_sal",
                     "param_teach_sal_sd",
                     "param_teach_ben",
                     "param_teach_ben_sd",
                     "param_n_students",
                     "param_n_students_sd",
                     "param_delta_ed_par",
                     "param_delta_ed_par_sd",
                     "param_delta_ed__ext_par",
                     "param_delta_ed__ext_par_sd",
                     "param_unit_cost_local",
                     "param_unit_cost_local_sd",
                     "param_unit_cost_2017usdppp",
                     "param_unit_cost_2017usdppp_sd",
                     "param_wage_ag",
                     "param_wage_ag_sd",
                     "param_wage_non_ag",
                     "param_wage_non_ag_sd",
                     "param_profits_se",
                     "param_profits_se_sd",
                     "param_hours_se_cond",
                     "param_hours_se_cond_sd",
                     "param_hours_ag",
                     "param_hours_ag_sd",
                     "param_hours_ww",
                     "param_hours_ww_sd",
                     "param_hours_se",
                     "param_hours_se_sd",
                     "param_growth_rate",
                     "param_growth_rate_sd",
                     "param_coverage",
                     "param_coverage_sd",
                     "param_lambda1_male",
                     "param_lambda1_male_sd",
                     "param_lambda1_female",
                     "param_lambda1_female_sd",
                     "param_tax",
                     "param_tax_sd",
                     "param_gov_bonds16",
                     "param_gov_bonds16_sd",
                     "param_inflation16",
                     "param_inflation16_sd",
                     "param_q_zero",
                     "param_costs_par",
                     "param_costs_par_sd",
                     "param_counts_par",
                     "param_counts_par_sd",
                     "param_ex_rate",
                     "param_ex_rate_sd",
                     "param_unit_cost_2017usdppp_sd")

      list_show <- list_master[ - which(list_master %in% list_hide)]

    }
    sapply(list_hide,
           function(x) hideElement(id = x) )
    sapply(list_show,
           function(x) showElement(id = x) )
  })


  hideElement("show_eq")
  #observeEvent(input$run, {
  ################
  ###### Results/Viz
  ################
  output$eqns <- renderUI({
    #if (input$run == TRUE) {showElement("show_eq")}
      if (input$policy_est == "A1. Tax revenue" ) {
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]

                      }_{\\text{cost of deworming medication}}

            \\tag{1}
            \\end{equation}
            $$ \n See', a("Approach 1", href='https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)', target = "_blank"), 'in the documentation component for more details'  )
        )

      } else if (input$policy_est ==  "A1. With externalities. Tax"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]

                      }_{\\text{cost of deworming medication}}

            \\tag{2}
            \\end{equation}
            $$ \n See' , a("Approach 1", href="05_final_opa.html#21_Approach_1:_Baird_et_al_(2016)", target = "_blank"),  'in the documentation component for more details'  )
        )
      } else if (input$policy_est == "A1. All income"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]

                      }_{\\text{cost of deworming medication}}

            \\tag{3}
            \\end{equation}
            $$ \n See' , a("Approach 1", href="https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)", target = "_blank"),  'in the documentation component for more details'
          )
        )
      } else if (input$policy_est ==  "A1. With ext. All income"){
        withMathJax(
          helpText('$$
              \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]

                      }_{\\text{cost of deworming medication}}

            \\tag{4}
            \\end{equation}
            $$ \n See', a("Approach 1", href="https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)", target = "_blank"),  'in the documentation component for more details'  )
        )
      } else if (input$policy_est == "A2. Tax"){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
                 \\left[ \\tau \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\left[\\sum_{t=0}^{1.4} \\left( \\frac{1}{1 + r}\\right)^{t} \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      \\right]
                      }_{\\text{cost of deworming medication}}

            \\tag{5}
            \\end{equation}
                                 $$ \n See' , a("Approach 2", href="https://bitss-opa.github.io/opa-deworming/#22_Approach_2:_Hamory_et_al_(2020)", target = "_blank"),  'in the documentation component for more details'))
      } else if (input$policy_est == "A2. All income"){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
                 \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}) -
                      K \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta \\overline{E}_t(S1,S2)
                      \\right]
                            }_{\\text{net labor market gains}} -
                      \\underbrace{
                      \\left[\\sum_{t=0}^{1.4} \\left( \\frac{1}{1 + r}\\right)^{t} \\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \\big]
                      \\right]
                      }_{\\text{cost of deworming medication}}

            \\tag{6}
            \\end{equation}
                                 $$ \n See' , a("Approach 2", href="https://bitss-opa.github.io/opa-deworming/#22_Approach_2:_Hamory_et_al_(2020)", target = "_blank"),  ' in the documentation component for more details' ))
      } else if (input$policy_est == "A3. All income of A1"){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1})
                     \\right]
                            }_{\\text{labor market gains}} -
                      \\underbrace{
                       \\left[\\sum_{t=0}^{t_{treat}} \\left( \\frac{1}{1 + r}\\right)^{t}  Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      \\right]
                      }_{\\text{cost of deworming medication}}

            \\tag{7}
            \\end{equation}
             $$ \n See' , a("Approach 3", href="https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners", target = "_blank"),  ' in the documentation component for more details'))
      } else if (input$policy_est == "A3. All income of A1, with ext."){
        withMathJax(helpText('$$
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\lambda_{1}, \\lambda_{2})
                     \\right]
                            }_{\\text{labor market gains}} -
                      \\underbrace{
                      Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      }_{\\text{cost of deworming medication}}

            \\tag{8}
            \\end{equation}
            $$ \n See' , a("Approach 3", href="https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners", target = "_blank"),  ' in the documentation component for more details'))
      } else if (input$policy_est == "A3. All income of A2. Main Policy Estimate"){
        withMathJax(helpText(
          '$$
            \\begin{equation}
              NPV =  \\underbrace{
              \\left[ \\sum_{t=0}^{50} \\left( \\frac{1}{1 + r}\\right)^{t} \\Delta W_t(\\alpha^{pooled}, \\eta_{new}, L_{new})
                     \\right]
                            }_{\\text{Benefits (B)}} -
                      \\underbrace{
                       Q(S_{2})\\sum_{i \\in Countries } \\omega_{i} c_{i}(\\delta_{g})\\
                      }_{\\text{Costs (C)}}

            \\tag{9}
            \\end{equation}
              $$ \n See' , a("Approach 3", href="https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners", target = "_blank"),  ' in the documentation component for more details'
        ))
      }
    })
  #})


  # Generate Plot with All Asumptions
  plotInputAll <- function(){
    npv_all_sim <- reactive.data1()
    plot1 <- generate_plot_f(npv_all_sim, input$policy_est, input$rescale, TRUE)[[1]]

    position <- generate_plot_f(npv_all_sim, input$policy_est, input$rescale, TRUE)[[2]]
    total_time_sim <- generate_plot_f(npv_all_sim, input$policy_est, input$rescale, TRUE)[[3]]
    plot1 <- plot1 + labs(y = NULL,
                          x = "Net Present Value (Benefits -  Costs)" ,
                          title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
                          subtitle = paste0(policy_estimates_text[position], ". ",
                                            "N = ", input$param_num_of_sim, " simulations. Takes ",
                                            round(total_time_sim, 1)," ",attributes(total_time_sim)$units )  )



  }
  output$plot1 <- renderPlot({
    input$updateAll
    
    isolate({print(plotInputAll())})
  }, height = 550
  )

  # Generate Plot with Key Assumptions
  plotInputKA <- function(){
    npv_all_sim <- reactive.data1()
    output_plot <- generate_plot_f(npv_all_sim, "A3. All income of A2. Main Policy Estimate", input$rescale, TRUE)
    plot1 <- output_plot[[1]]

    position <- output_plot[[2]]

    plot1 <- plot1 + labs(y = NULL,
                          x = "Net Present Value (Benefits -  Costs)" ,
                          title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
                          subtitle = paste0(policy_estimates_text[position], ". ")
    )
  }

  
  output$plot1_ka <- renderPlot({
    
    input$updateKA
    isolate(print(plotInputKA()))

  }, height = 550
  )
  


  # Generate Main Policy Estimate Plot
  output$plot1_main <- renderPlot({
    npv_all_sim <- reactive.data1()
    output_plot <- generate_plot_f(npv_all_sim, "A3. All income of A2. Main Policy Estimate", input$rescale)
    plot1 <- output_plot[[1]]

    position <- output_plot[[2]]

    plot1 <- plot1 + labs(y = NULL,
           x = "Net Present Value (Benefits -  Costs)" ,
           title = "Net Lifetime Income Effects of Deworming for Each Treated Children",
           subtitle = "Distribution of the Net Present Value of Deworming Interventions"
           )
    print(plot1)
  }, height = 550
  )

  # Master List of Input ID & Label

  inputMaster <- c(# ---- Key Assumptions
                   "param_ka_costs2_ea" = "Yearly unit costs in new country (in $US)",
                   "param_ka_prevl_r" = "Prevalence in new region",
                   "param_ka_years_of_treat_t" = "Length of treatment (years)",
                   "resetKA" = "Reset Button for Key Assumptions",
                   "updateKA" = "Update Button for Key Assumptions",
                   # ---- All Assumptions
                   "rescale" = "Rescale Checkbox Status",
                   "policy_est" = "Policy Estimate Assumption",
                   "param_num_of_sim" = "Number of simulations",
                   "resetAll" = "Reset Button for All Assumptions",
                   "updateAll" = "Update Button for All Assumptions",
                   # ---- research tab

                   "param_lambda1_male" = "Increase in number of hours worked due to treatment (Male)",
                   "param_lambda1_male_sd" = "SD of Increase in number of hours worked due to treatment (Male)",
                   "param_lambda1_female" = "Increase in number of hours worked due to treatment (Female)",
                   "param_lambda1_female_sd" = "SD of Increase in number of hours worked due to treatment (Female)",
                   "param_lambda1_new" = "Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                   "param_lambda1_new_sd" = "SD of Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                   "param_lambda2" = "Increase in number of hours worked due to treatment (Externalities included)",
                   "param_lambda2_sd" = "SD of Increase in number of hours worked due to treatment (Externalities included)",
                   "param_prevl_0" = "Prevalence in original study",
                   "param_prevl_0_sd" = "SD of Prevalence in original study",
                   "param_wage_ag" = "Agricultural Wages",
                   "param_wage_ag_sd" = "SD of Agricultural Wages",
                   "param_wage_non_ag" = "Wages of a Wage Worker",
                   "param_wage_non_ag_sd" = "SD of Wages of a Wage Worker",
                   "param_profits_se" = "Average monthly self-employed profits (Profits SE)",
                   "param_profits_se_sd" = "SD of Profits SE",
                   "param_hours_se_cond" = "Weekly hours worked by self-employed workers(>10)",
                   "param_hours_se_cond_sd" = "SD of Weekly hours worked by self-employed workers(>10)",
                   "param_hours_ag" = "Weekly hours worked by agricultural workers",
                   "param_hours_ag_sd" = "SD of Weekly hours worked by agricultural workers",
                   "param_hours_ww" = "Weekly hours worked by wage earners",
                   "param_hours_ww_sd" = "SD of Weekly hours worked by wage earners",
                   "param_hours_se" = "Weekly hours worked by self-employed workers (no condition)",
                   "param_hours_se_sd" = "SD of Weekly hours worked by self-employed workers (no condition)",
                   "param_coef_exp1" = "Coefficients of Teacher Experience, Xp (beta_1)",
                   "param_coef_exp2" = "Coefficients of Teacher Experience Squared, Xp ^2 (beta_2)",
                   "param_coverage" = "Coverage(R)",
                   "param_coverage_sd" = "SD of Coverage(R)",
                   "param_q_full" = "Take up (Q(S_2))",
                   "param_q_full_sd" = "SD of Take up (Q(S_2))",
                   "param_q_zero" = "Take-up with no subsidy ( Q(S_1) )",
                   "param_delta_ed_par" = "x * Delta{E}",
                   "param_delta_ed_par_sd" = "SD of x * Delta{E}",
                   "param_delta_ed__ext_par" = "x * Delta{E}  (ext)",
                   "param_delta_ed__ext_par_sd" = "SD of x * Delta{E}  (ext)",
                   "param_teach_sal" = "Teacher salary",
                   "param_teach_sal_sd" = "SD of Teacher salary",
                   "param_teach_ben" = "Teacher Benefits",
                   "param_teach_ben_sd" = "SD of Teacher Benefits",
                   "param_n_students" = "Students per teacher",
                   "param_n_students_sd" = "SD of Students per teacher",
                   "param_years_of_treat_0" = "Years of treatment in orginal study",
                   "param_years_of_treat_0_sd" = "SD of Years of treatment in orginal study",
                   "param_unit_cost_local" = "Costs of Treatment (local $)",
                   "param_unit_cost_local_sd" = "SD of Costs of Treatment (local $)",
                   "param_unit_cost_2017usdppp" = "Costs of Treatment (US $)",
                   "param_unit_cost_2017usdppp_sd" = "SD of Costs of Treatment (US $)",
                   "param_costs_par" = "Costs adjustment",
                   "param_costs_par_sd" = "SD of Costs adjustment",
                   "param_counts_par" = "Counts adjustment",
                   "param_counts_par_sd" = "SD of Counts adjustment",
                   #---- data tab
                   "param_costs2_ea" = "Yearly unit costs in new country (in $US)",
                   "param_costs2_ea_sd" = "SD of Yearly unit costs in new country (in $US)",
                   "param_ex_rate" = "Exchange rate",
                   "param_ex_rate_sd" = "SD of Exchange rate",
                   "param_growth_rate" = "GDP Growth Rate",
                   "param_growth_rate_sd" = "SD of GDP Growth Rate",
                   "param_gov_bonds16" = "Government Bonds",
                   "param_gov_bonds16_sd" = "SD of Government Bonds",
                   "param_gov_bonds19" = "Government Bonds",
                   "param_gov_bonds19_sd" = "SD of Government Bonds",
                   "param_inflation16" = "Inflation Rate",
                   "param_inflation16_sd" = "SD of Inflation Rate",
                   "param_inflation19" = "Inflation Rate",
                   "param_inflation19_sd" = "SD of Inflation Rate",
                   "param_tax" = "Tax rate",
                   "param_tax_sd" = "SD of Tax rate",
                   # ---- GW tab
                   "param_prevl_r" = "Prevalence in new region",
                   "param_prevl_r_sd" = "SD of Prevalence in new region",
                   "param_years_of_treat_t" = "Years of Treatment in New Setting",
                   "param_years_of_treat_t_sd" = "SD of Years of Treatment in New Setting",
                   "param_staff_time" = "Additional costs due to staff time",
                   "param_staff_time_sd" = "SD of Additional costs due to staff time",
                   # ---- Buttons for All Assumption tab
                   "resetAll" = "Reset Button for All Assumptions",
                   "show_eq" = "Show Equation Checkbox Status"



                   )


})
