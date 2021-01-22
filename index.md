---
title: "<center><div class= 'mytitle'>Template</div></center>"
date: "<center><div class='mysubtitle'>21 January, 2021</div></center>"
author: "<center><div class = 'contributors'>Contributors</div></center>"
output:
  bookdown::html_document2:
    code_download: yes
    code_folding: hide
    css: style.css
    highlight: tango
    includes:
      after_body: footer.html
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    theme: cerulean
    toc: yes
    toc_collapsed: no
    toc_depth: 3
    toc_float: yes
  html_document:
    df_print: paged
    toc: yes
    toc_depth: '3'
  word_document: null
link-citations: yes
pdf_document:
  extra_dependencies: xcolor
  fig_caption: no
bibliography: bibliography.bib

knit: 
  # render to index.html for GitHub pages
  # render to 05_final_opa.html to knit locally
  # YAML does not support commenting inside the function
  (function(input_file, encoding) {
  rmarkdown::render(input_file, encoding=encoding, output_file=file.path("..", 'index.html')); 
  rmarkdown::render(input_file, encoding=encoding, output_file='00_template.html'); 
  })
---
\def\blue{\color{blue}}
\def\red{\color{red}}






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

Explanation for the main equation 

<details><summary>Show all the details</summary>
\begin{equation}
y = r + q - k
\label{eq:1}
\tag{1}
\end{equation}

Where: 

- $y$: one-liner to define y
- $r$: one-liner to define r
- $k$: one-liner to define k

</details>



### Alternative Equation

Explanation for the alternative equation 

<details><summary>Show all the details</summary>
\begin{equation}
y = r + q + k
\label{eq:2}
\tag{2}
\end{equation}

Where:

- $y$: one-liner to define y
- $r$: one-liner to define r
- $k$: one-liner to define k


```r
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
```


</details>

## Sub Common Components:

### Component 1 ("$r$")

This is the formula used to calculate component 1[^1]

<details><summary>Show all the details</summary>
\begin{equation}
r = X \times \lambda_1  + (1 - X) \times \lambda_2
\label{eq:3}
\tag{3}
\end{equation}

Where: 

- $r$: one-liner for r
- $X$: one-liner for X
- $\lambda_1$: one-liner for $\lambda_1$
- $\lambda_2$: one-liner for $\lambda_2$


```r
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
```
</details>

## Approach 1: Source Name (source link)
### Component 2 ("$q$")

This is the formula used to calculate component 2[^2]

<details><summary>Show all the details</summary>
\begin{equation}
q =  \text{input} \times \alpha_0 (1 + g)^{X}(1 + \hat{\beta_1} X + \hat{\beta_2} X^2)
\label{eq:}
\tag{4}
\end{equation}

Where: 

- $q$: one-liner to define q
- $\alpha_0$: one-liner to define $\alpha_0$
- $g$: one-liner to define g
- $\hat{\beta_1}$: one-liner to define $\hat{\beta_1}$
- $\hat{\beta_2}$: one-liner to define $\hat{\beta_2}$



```r
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
```
</details>

## Approach 2: Source Name (source link)
### Component 3 ("$k$")

This is the formula used to calculate component 3[^3]

<details><summary>Show all the details</summary>
\begin{equation}
k = R \times X  + (1 - R) \times X
\label{eq:5}
\tag{5}
\end{equation}

Where:

- $k$: one-liner to define k
- $R$: one-liner to define R



```r
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
```

</details>
## Summary of All Approaches 


| Approach    | Part 1                                   | Part 2        |
|---------|-------------------------------------------|--------------|
| 1.1 | Specification of Approach 1 with Part 1 Assumption 1 | Specification of Approach 1 with Part 2 Assumption 1  |
| 1.2 | Specification of Approach 1 with Part 1 Assumption 2 | Specification of Aprroach 1 with Part 2 Assumption 2  |
| 2.1 | Specification of Approach 2 with Part 1 Assumption 1 | Specification of Approach 2 with Part 2 Assumption 1 |
| **2.2** | **Specification of Approach 2 with Part 1 Assumption 2** | **Specification of Approach 2 with Part 2 Assumption 2**|

Bolded row is the assumptions and the approach we use to generate the main policy estimate plot. 


# Main results
<details><summary>Show all the details</summary>

```r
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
```

</details>

```r
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
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:main-results)Table Caption</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> results1 </th>
   <th style="text-align:left;"> results2 </th>
   <th style="text-align:left;"> results3 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> situation1 </td>
   <td style="text-align:left;"> results </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> results </td>
  </tr>
  <tr>
   <td style="text-align:left;"> situation2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> results </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> situation3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> results </td>
  </tr>
</tbody>
</table>




# Monte Carlo Simulations  

```r
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
```


```r
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
```

![](../index_files/figure-html/mc-run-1.png)<!-- -->




# References


[^1]: Notes of referenced section


[^2]: Notes on referenced section

[^3]: Notes on referenced section
