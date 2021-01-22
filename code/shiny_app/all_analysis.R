## ----parameters, echo=print_code----------------------------------------------
# Do not run data set on git/github until privacy has been cleared
################
##### Data  
################
################
##### Research
################
################
##### Guess work   
################
################
#####  Notes:
################
### Source ---->  Input ----> Model ----> Policy Estimates (output)
###  (_so)        (_in)       (_mo)        (_pe)
### values      functions   functions      values
###             & values    & values
### arguments in functions should used "_var" and functions should "_f"
#invisible( list2env(call_params_f(),.GlobalEnv) )


# Each analytic code chunk will begin by listing all the inputs it needs, and
# the outputs it produces.
# - inputs: list
# - outputs: list
#### The key essential analytic steps are wrapted in a function   
#chunk_name_of_chunk <- function(){
##########################################
##########################################  
#
# here goes the essential analytic content
#
##########################################
##########################################  
#    return( )                         # A list with all the objects
#}                                     # generated inside the function
# The following line executes the code chunk and deposits its results
# into the current R enviornment:
#invisible( list2env(chunk_name_of_chunk(),.GlobalEnv) )
#
##### Execute values of the functions above when needed for the text:
# Anything under this comment is to create objects that are used in the body of
# text. Not to be used in the final results (could be deleted). Each of these
# object should end with the suffix _temp



## ----sources, eval = TRUE, echo=print_code, message=FALSE, warning=FALSE------
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_sources <- function(){
###############################################################################
###############################################################################
  
    #############
    ##### Setup
    #############  
    nsims_so <- 1e4
    policy_estimate_so <- "Main Equation"
    rescale_so <- TRUE
    #############
    ##### Data  
    #############
    
  # Create objects for data extracted from various sources
    
    r_input1_so <- 0.1
    r_input2_so <- 0.2
    #############
    ##### Research
    #############
  
  # Create objects for parameters extracted from research papers 
    q_input1_so <- 0.5
    q_input2_so <- 0.8
    #############
    ##### Guess work   
    #############
  
  # Create objects for variables from educated guesses or estimates  
  

    #############
    ##### Notes: 
    #############
  
  # Notes for the objects defined above, including sources, explanations, etc. 
    k_input1_so <- 3
    k_input2_so <- 4
    
    #return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
    return (
      list("nsims_so" = nsims_so,
           "policy_estimate_so" = policy_estimate_so,
           "rescale_so" = rescale_so,
           "r_input1_so" = r_input1_so,
           "r_input2_so" = r_input2_so,
           "q_input1_so" = q_input1_so,
           "q_input2_so" = q_input2_so,
           "k_input1_so" = k_input1_so,
           "k_input2_so" = k_input2_so
           )
    )
}
invisible(list2env(chunk_sources(),.GlobalEnv) )


## -----------------------------------------------------------------------------
#my thoughts: should we forefront the conclusions before the methodology? 

#Sandra: I think we should specify which approach we use to generate the graph, but keep the methodology before the conclusions. 


## ----final-output-------------------------------------------------------------


## ----test, eval=TRUE----------------------------------------------------------
# - inputs: 
# - outputs: 
chunk_test <- function(){
############################################################################### 
###############################################################################  
  
    # random equation to use as our main equation to get the final result
    mainequation_f <- function(r_final_var = 1,
                               q_final_var = 1,
                               k_final_var = 1) {
        return (r_final_var + q_final_var - k_final_var)
    }
    
    # random equation to use as our alternative equation to get the final result
    alternative_f <- function( r_final_var = 1,
                               q_final_var = 1,
                               k_final_var = 1){
      return (r_final_var + q_final_var + k_final_var)
      
    }
    
############################################################################### 
###############################################################################  
    return(list("mainequation_f" = mainequation_f, "alternative_f" = alternative_f))    # Try to return only functions
}
invisible( list2env(chunk_test(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
mainequation_in <- mainequation_f()
alternative_in <- alternative_f()


## ----comp1,  echo=print_code, eval=TRUE---------------------------------------
# - inputs: factors of r
# - outputs: r value
chunk_r <- function(){
###############################################################################
###############################################################################  

    r_function_f <- function(r_input1_var = r_input1_so , r_input2_var = r_input2_so) {  
        r_input1_var - r_input2_var
        
    }

###############################################################################
###############################################################################  
    return(list("r_function_f" = r_function_f))
}

invisible( list2env(chunk_r(),.GlobalEnv) )



## ----comp2,  echo=print_code, eval=TRUE---------------------------------------
# - inputs: factors of q
# - outputs: q value
chunk_q <- function(){
###############################################################################
###############################################################################  

    q_function_f <- function(q_input1_var = q_input1_so , q_input2_var = q_input2_so) {  
        (q_input1_var * q_input2_var)^2
        
    }

###############################################################################
###############################################################################  
    return(list("q_function_f" = q_function_f))
}

invisible( list2env(chunk_q(),.GlobalEnv) )



## ----comp3,  echo=print_code, eval=TRUE---------------------------------------
# - inputs: factors of q
# - outputs: q value
chunk_k <- function(){
###############################################################################
###############################################################################  

    k_function_f <- function(k_input1_var = k_input1_so , k_input2_var = k_input2_so) {  
        (k_input1_var * k_input2_var)^2
        
    }

###############################################################################
###############################################################################  
    return(list("k_function_f" = k_function_f))
}

invisible( list2env(chunk_k(),.GlobalEnv) )




## ----all-steps,  echo=print_code, eval = TRUE---------------------------------
#unit test function
unit_test_f <- function(to_test_var, original_var, main_run_var = TRUE){
    if (main_run_var == TRUE) {
        if (length(to_test_var) > 1) {
            fails_test <- ( abs(sd(to_test_var) - original_var) > 0.0001 )
            text_val <- sd(to_test_var)
        } else {
            fails_test <- ( abs(to_test_var - original_var) > 0.0001 )
            text_val <- to_test_var
        }
        if (fails_test) {
            print(paste("Output has change at",
                        deparse(substitute(to_test_var) ),
                        " to ", text_val) )
        }
      }
}

one_run <- 
  function(r_input1_var1 = r_input1_so, 
           r_input2_var1 = r_input2_so, 
           q_input1_var1 = q_input1_so,
           q_input2_var1 = q_input2_so,
           k_input1_var1 = k_input1_so,
           k_input2_var1 = k_input2_so){# Variables needed to generate the final policy estimates
    
    r_in <- r_function_f(r_input1_var = r_input1_var1,
                         r_input2_var = r_input2_var1)
    q_in <- q_function_f(q_input1_var = q_input1_var1,
                         q_input2_var = q_input2_var1)
    k_in <- k_function_f(k_input1_var = k_input1_var1,
                         k_input2_var = k_input2_var1)
    return (list("r_in" = r_in,
                 "q_in" = q_in,
                 "k_in" = k_in))
           }
    
invisible(list2env(one_run(), .GlobalEnv))

    


## ----main-results,  echo=print_code, eval = TRUE------------------------------
# - perform the calculations to achieve final results

result1 <- mainequation_f(r_final_var = r_in,
                          q_final_var = q_in,
                          k_final_var = k_in)
result2 <- alternative_f(r_final_var = r_in,
                          q_final_var = q_in,
                          k_final_var = k_in)
#...

results_table <- data.frame("results1" =   c("results", NA,
                                             NA) ,
                        "results2" =  c(NA, "results", NA),
                        "results3" = c("results", NA,
                                             "results"),
                        
                        row.names = c("situation1", "situation2", "situation3"))

kable(results_table, caption = "Table Caption") %>%
  kable_styling("striped", full_width = F)


## ----generate-plot-function, purl = TRUE, echo = FALSE------------------------
# generate_plot_f: function to generate plots for both Dynamic Document and
# shiny app. It takes in the simulated data, policy estimate text, and rescale
# variable. These are intermediary variables to exclude the interactivity of
# shiny app from the plot generation process.  
chunk_generate_plot <- function() {
  generate_plot_f <- function(result1_sim_all,
                              policy_estimates_text_selected,
                              rescale, SD = FALSE){
    total_time_sim <- result1_sim_all$total_time_sim
    position <- which( policy_estimates_text == policy_estimates_text_selected)
    result1_sim <- result1_sim_all[[ policy_estimates_varnames[position] ]]    
    result1_for_text <- paste("Median NPV: ", round(median(result1_sim), 2))
    result1_for_text2 <- NULL
    if (SD){
    result1_for_text2 <- paste("SD NPV: ", round(sd(result1_sim), 2))
    }
    plot1 <- ggplot() +
      geom_density(
        aes(x = result1_sim,
            alpha = 1 / 2, ..scaled..),
        kernel = "gau",
        lwd = 1,
        fill = "#007ba7",
        color = "darkblue",
        alpha = 0.3
      ) +
      geom_vline(
        xintercept = c(0, median(result1_sim)),
        col = c("black", "darkblue"),
        lwd = c(1, 1),
        linetype = c("solid", "dashed")
      ) +
      coord_cartesian(xlim = c(-300,1000),  ylim =  c( 0, 1.2 ))  +  # fixing the x axis so shifts in the density can be seen
      #xlim(range(density(result1_sim)$x)) +
      guides(alpha = "none", colour = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      annotate(
        "text",
        x = 1 * median(result1_sim),
        y = 0.2,
        label = result1_for_text,
        size = 6,
        color = "darkblue"
      ) +
      annotate(
        "text",
        x = 1 * median(result1_sim),
        y = 0.1,
        label = result1_for_text2,
        size = 6,
        color = "darkblue"
      ) +
      theme(
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5)
      )

    if (rescale == TRUE) {
      plot1 <-
        suppressMessages(plot1 + coord_cartesian(xlim = 1.2 * c(min(c(
          -1, result1_sim
        )), max(c(
          100, result1_sim
        )))))
    }
    return (list(plot1,position,total_time_sim))
}
################################################################################
################################################################################
return(list("generate_plot_f" = generate_plot_f))
}

invisible( list2env(chunk_generate_plot(),.GlobalEnv) )


## ----mc-setup,  echo=print_code, eval = TRUE----------------------------------

sim_data1_f <- function(nsims = 1e2,
                      r_input1_var2,
                      r_input1_var2_sd,
                      r_input2_var2,
                      r_input2_var2_sd,
                      q_input1_var2,
                      q_input1_var2_sd,
                      q_input2_var2,
                      q_input2_var2_sd,
                      k_input1_var2,
                      k_input1_var2_sd,
                      k_input2_var2,
                      k_input2_var2_sd){
    ################
    ###### Draws   
    ################  
  start_time <- Sys.time()
  set.seed(142857)
  r1_sim <- rnorm(n = nsims, mean = r_input1_var2, sd= r_input1_var2_sd)
  r2_sim <- rnorm(n = nsims, mean = r_input2_var2, sd= r_input2_var2_sd)
  q1_sim <- rnorm(n = nsims, mean = q_input1_var2, sd= q_input1_var2_sd)
  q2_sim <- rnorm(n = nsims, mean = q_input2_var2, sd= q_input2_var2_sd)
  k1_sim <- rnorm(n = nsims, mean = k_input1_var2, sd= k_input1_var2_sd)
  k2_sim <- rnorm(n = nsims, mean = k_input2_var2, sd= k_input2_var2_sd)
  
  
  
                      

    ################
    ###### Runs    
    ################

  result1_sim <- rep(NA, nsims) #result1
  result2_sim <- rep(NA, nsims) #result2
  
  for (i in 1:nsims){
    invisible(list2env(
      one_run(r_input1_var1 = r1_sim[i],
              r_input2_var1 = r2_sim[i],
              q_input1_var1 = q1_sim[i],
              q_input2_var1 = q2_sim[i],
              k_input1_var1 = k1_sim[i],
              k_input2_var1 = k2_sim[i]
              ), .GlobalEnv))
    
    result1_sim[i] <- mainequation_f(r_final_var = r_in,
                          q_final_var = q_in,
                          k_final_var = k_in)
    result2_sim[i] <- alternative_f(r_final_var = r_in,
                          q_final_var = q_in,
                          k_final_var = k_in)
  }
    total_time <- Sys.time() - start_time
    return(list("result1_sim" = result1_sim,
                "result2_sim" = result2_sim))
  
  
}

policy_estimates_varnames <- c(
  "result1_sim",
  "result2_sim"
)

policy_estimates_text <- c(
  "Main Equation",
  "Alternative Equation"
)



## ----mc-run, dpi = 400, echo = print_code, eval = TRUE------------------------
# Run Monte Carlo simulation for our main model
result1_sim_all <- sim_data1_f(nsims = nsims_so, 
                      r_input1_var2 = r_input1_so,
                      r_input1_var2_sd = r_input1_so * 0.1,
                      r_input2_var2 = r_input2_so,
                      r_input2_var2_sd = r_input2_so * 0.1,
                      q_input1_var2 = q_input1_so,
                      q_input1_var2_sd = q_input1_so * 0.1,
                      q_input2_var2 = q_input2_so,
                      q_input2_var2_sd = q_input2_so * 0.1,
                      k_input1_var2 = k_input1_so,
                      k_input1_var2_sd = k_input1_so * 0.1,
                      k_input2_var2 = k_input2_so,
                      k_input2_var2_sd = k_input2_so * 0.1
                             
                             )



################
###### Results/Viz
################


library(plotly)


plot1 <- generate_plot_f(result1_sim_all, policy_estimate_so, rescale_so)[[1]] +
      labs(y = NULL,
       x = "Main Estimate" ,
       title = "Project Title",
       subtitle = "Distribution of Key Indicator"
       ) 
print(plot1)

