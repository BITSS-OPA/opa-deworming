---
title: "Template"
date: "28 May, 2019"
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
```


# Key policy estimates for policy makers  



# Methodology

## Main Equation (the model)

\begin{equation}
CEA = \frac{B (1 + F_{0})}{C}
\label{eq:1}
\tag{1}
\end{equation}

 - $C$ is the costs per person dewormed.   
 - $B$ is the benefits per person dewormed. 
 - $F_{0}$ is a factor to account for leverage/fudging [not reviewed in this excercise]

We begin by describing the underlying analysis behind the costs. Through this excercise we use the following notation the letters $F, P, Q$ denote components
in percentages, monetary units (US dollars and local currency) and quantities respectively. Each new element will be tracked using a sub-index, and supra-indecis will be
used to track groups, like geographies, time, and other catergories. For example $Q^{i}_{2}$ represents the second quantity described in this analysis (total adjusted number childred dewormed per year) in location $i$. At the end of each description we will show in parenthesis the original location of the parameter in GiveWell's spreadsheets (using the notation `file, sheet number, cell`[^1].

\begin{equation}
C = \sum_{i \in G_{1} (countries) } F^{i}_{1} P^{i}_{1}
\label{eq:2}
\tag{2}
\end{equation}

- $F^{i}_{1}$: Weight for the weighted average (`F1, 2, H16`).  
- $P^{i}_{1}$: Total cost per child, per year in region $i$ (`F1, 2, C:G16`).  


\begin{equation}
F^{i}_{1} = \frac{F^{i}_{2} Q^{i}_{1}}{\sum_{j \in G_{1}} F^{j}_{2} Q^{j}_{1}}
\label{eq:3}
\tag{3}
\end{equation}

- $F^{i}_{2}$: is the proportion of the costs that are paid by the Deworm the World initiative (DtW from now on) (`F1, 2, C:G6`).  
- $Q^{i}_{1}$: estimated number of treatments delivered and commited (`F1, 2, C:G7`).  

\begin{equation}
P^{i}_{1} = \left(P^{i}_{3} + P^{i}_{4} + P^{i}_{5} + P^{i}_{6} + P^{i}_{7}  \right)\frac{1}{Q^{i}_{1}}
\label{eq:4}
\tag{4}
\end{equation}

- $F^{i}_{2}$: is the proportion of the costs that are paid by the Deworm the World initiative (DtW from now on) (`F1, 2, C:G6`).  
- $Q^{i}_{1}$: estimated number of treatments delivered and commited (`F1, 2, C:G7`).  



```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outputs: total_rev_pe
```


## Sub components:

### 1 - Component 1 ("$r$")


```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outputs: total_rev_pe
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


[^1]: `F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]`  
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter. 
