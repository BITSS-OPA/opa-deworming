---
title: "Template"
date: "15 January, 2021"
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
```



```r
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_sources <- function(){
###############################################################################
###############################################################################  
    #############
    ##### Data  
    #############
  
  # Create objects for data extracted from various sources
  
    #############
    ##### Research
    #############
  
  # Create objects for parameters extracted from research papers 
  
    #############
    ##### Guess work   
    #############
  
  # Create objects for variables from educated guesses or estimates  
  

    #############
    ##### Notes: 
    #############
  
  # Notes for the objects defined above, including sources, explanations, etc. 
  
}
```

# Introduction
Summary of the issue and introduction to the policy analysis is conducted. 

The goal of this analysis is to provide the best empirical information for policy makers debating the implemention of "x" policy. This document describes all the analytical steps required to reproduce the analysis, and displaying the actual computer code use in each step. In addition to this report, the reader can find all the materials to reproduce the findings presented here in GitHub. The main output, presented in the results section of this report, can also be explored interactively for different assumptions on the corresponding shiny app. 

## Source Information for data + analytical methods

For this dynamic document, we are conducting this specific analysis, and it is computed using three different approaches:

1. Approach 1 (source link)
2. Approach 2 (source link)
3. Approach 3 (source link)



???  

## Key policy estimates for policy makers  

```r
#my thoughts: should we forefront the conclusions before the methodology? 

#Sandra: I think we should specify which approach we use to generate the graph, but keep the methodology before the conclusions. 
```


???

# Methodology

Explain what the final estimate indicator is, how the analysis is to be performed, what factors are looked at, etc.

## Common Structure

Introduce the starting point and the final policy estimate. Include alternative indicators of our final policy estimates as well.  

### Main Equation (the model)

\begin{equation}
y = f(x)
\label{eq:1}
\tag{1}
\end{equation}

### Alternative Equation

\begin{equation}
y = g(x)
\label{eq:2}
\tag{2}
\end{equation}



```r
# - inputs: 
# - outputs: 
chunk_test <- function(){
############################################################################### 
###############################################################################  
  
    mainequation_f <- function(something_var = something_default) {
        something_var
    }
    
    alternative_f <- function(something_var = something_default)
    
############################################################################### 
###############################################################################  
    return(list("mainequation_f" = mainequation_f, "alternative_f" = alternative_f))    # Try to return only functions
}
invisible( list2env(chunk_test(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
mainequation_in <- mainequation_f()
alternative_in <- alternative_f()
```

## Sub Common Components:

### Component 1 ("$r$")

This is the formula used to calculate component 1[^1]

\begin{equation}
r = X \times \lambda_1  + (1 - X) \times \lambda_2
\label{eq:3}
\tag{3}
\end{equation}


```r
# - inputs: factors of r
# - outputs: r value
chunk_r <- function(){
###############################################################################
###############################################################################  

    r_function_f <- function(r_input1_var = r_input1_so , r_input2_var = r_input2_so) {  
        r_value = r_input1_var - r_input2_var
        return(list("r_value" = r_value))
    }

###############################################################################
###############################################################################  
    return(list("r_function_f" = r_function_f))
}

invisible( list2env(chunk_r(),.GlobalEnv) )
r_parameter <- as.numeric( r_function_f() )
```

## Approach 1: Source Name (source link)
### Component 2 ("$q$")

This is the formula used to calculate component 2[^2]

\begin{equation}
q =  \text{input} \times \alpha_0 (1 + g)^{X}(1 + \hat{\beta_1} X + \hat{\beta_2} X^2)
\label{eq:}
\tag{4}
\end{equation}


```r
# - inputs: factors of q
# - outputs: q value
chunk_q <- function(){
###############################################################################
###############################################################################  

    q_function_f <- function(q_input1_var = q_input1_so , q_input2_var = q_input2_so) {  
        r_value = (q_input1_var * q_input2_var)^2
        return(list("q_value" = q_value))
    }

###############################################################################
###############################################################################  
    return(list("q_function_f" = q_function_f))
}

invisible( list2env(chunk_q(),.GlobalEnv) )
q_parameter <- as.numeric( q_function_f() )
```

## Approach 2: Source Name (source link)
### Component 3 ("$k$")

This is the formula used to calculate component 3[^3]

\begin{equation}
k = R \times X  + (1 - R) \times X
\label{eq:5}
\tag{5}
\end{equation}




```r
# - inputs: factors of q
# - outputs: q value
chunk_k <- function(){
###############################################################################
###############################################################################  

    k_function_f <- function(k_input1_var = k_input1_so , k_input2_var = k_input2_so) {  
        k_value = (k_input1_var * k_input2_var)^2
        return(list("k_value" = k_value))
    }

###############################################################################
###############################################################################  
    return(list("k_function_f" = k_function_f))
}

invisible( list2env(chunk_k(),.GlobalEnv) )
k_parameter <- as.numeric( k_function_f() )
```

## Summary of All Approaches 


| Approach    | Part 1                                   | Part 2        |
|---------|-------------------------------------------|--------------|
| 1.1 | Specification of Approach 1 with Part 1 Assumption 1 | Specification of Approach 1 with Part 2 Assumption 1  |
| 1.2 | Specification of Approach 1 with Part 1 Assumption 2 | Specification of Aprroach 1 with Part 2 Assumption 2  |
| 2.1 | Specification of Approach 2 with Part 1 Assumption 1 | Specification of Approach 2 with Part 2 Assumption 1 |
| **2.2** | **Specification of Approach 2 with Part 1 Assumption 2** | **Specification of Approach 2 with Part 2 Assumption 2**|

Bolded row is the assumptions and the approach we use to generate the main policy estimate plot. 


# Main results


```r
#unit test function
unit_test <- function(to_test_var, original_var, main_run_var = TRUE){
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
                         k_input2_var - k_input2_var1)
    return (list("r_in" = r_in,
                 "q_in" = q_in,
                 "k_in" = k_in))
           }
    
invisible(list2env(one_run(), .GlobalEnv))
```



```r
# - perform the calculations to achieve final results

result1 <- mainequation_f(something_var = something_default)
result2 <- alternative_f(something_var = something_default)
#...

results_table <- data.frame("results1" =   c("results", NA,
                                             NA) ,
                        "results2" =  c(NA, "results", NA),
                        "results3" = c("results", NA,
                                             "results"),
                        
                        row.names = c("situation1", "situation2", "situation3"))

kable(results_table, caption = "Table Caption") %>%
  kable_styling("striped", full_width = F)
```




# Monte Carlo Simulations  

```r
sim.data1 <- function(nsims = 1e2,
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
  
                      }

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
    
    result1_sim[i] <- mainequation_f(something_var = something_default)
    result2_sim[i] <- alternative_f(something_var = something_default)
    
    total_time <- Sys.time() - start_time
    return(list("result1_sim" = result1_sim,
                "result2_sim" = result2_sim))
  }

policy_estimates <- c(
  "result1_sim",
  "result2_sim"
)

policy_estimates_text <- c(
  "Main Equation",
  "Alternative Equation"
)
```


```r
# Run Monte Carlo simulation for our main model
result1_sim_all <- sim.data1(nsims = nsims_so, 
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
                      k_input2_var2_sd = k_input2_so * 0.1,
                             
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
```

# Sensitivity Analysis  

# Conclusions

# References


[^1]: Notes of referenced section


[^2]: Notes on referenced section

[^3]: Notes on referenced section
