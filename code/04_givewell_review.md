---
title: "Notes on Givewell's Cost Effectiveness Analysis on Deworming "
date: "22 August, 2019"
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
  pdf_document: default
  word_document: default
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
 - $B$ is the benefits per person dewormed .
 - $F_{0}$ is a factor to account for leverage/fudging [not reviewed in this excercise][^12]


## Sub components:

We begin by describing the underlying analysis behind the costs. Through this excercise we use the following notation the letters $F, P, Q$ denote components
in percentages, monetary units (US dollars and local currency) and quantities respectively. Each new element will be tracked using a sub-index, and supra-indecis will be
used to track groups, like geographies, time, and other catergories. For example $Q^{i}_{2}$ represents the second quantity described in this analysis (total adjusted number childred dewormed per year) in location $i$. At the end of each description we will show in parenthesis the original location of the parameter in GiveWell's spreadsheets (using the notation `file, sheet number, cell`[^1]).

### Costs ("$C$")


```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outputs: total_rev_pe
```

\begin{equation}
C = \sum_{i \in G_{1} (countries) } F^{i}_{1} P^{i}_{1}
\label{eq:2}
\tag{2}
\end{equation}

GiveWell estimates the cost per child dewormed in geographies where Evidence Action provides technical assistance.These costs include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such the cost of drugs donated by WHO. 

Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The final cost is a weighted average of the unit cost across countries. 

- $F^{i}_{1}$: Weight for the weighted average (`F1, 2, H16`).  
- $P^{i}_{1}$: Total cost per child, per year in region $i$ (`F1, 2, C:G16`).  


\begin{equation}
F^{i}_{1} = \frac{F^{i}_{2} Q^{i}_{1}}{\sum_{j \in G_{1}} F^{j}_{2} Q^{j}_{1}}
\label{eq:3}
\tag{3}
\end{equation}

The weight for each country takes into account the number of treatments provided as well as the proportion of costs incurred by DtWI in that geography. 

- $F^{i}_{2}$: is the proportion of the costs that are paid by the Deworm the World initiative (DtW from now on) (`F1, 2, C:G6`).  
- $Q^{i}_{1}$: estimated number of treatments delivered and commited (`F1, 2, C:G7`).  

\begin{equation}
P^{i}_{1} = \left(P^{i}_{3} + P^{i}_{4} + P^{i}_{5} + P^{i}_{6} + P^{i}_{7}  \right)\frac{1}{Q^{i}_{2}}
\label{eq:4}
\tag{4}
\end{equation}

- $P^{i}_{3}$: Total cost of DtW across all years (`F1, 1, Z39`).  
- $P^{i}_{4}$: Cost of donated drugs per year (`F1, 1, Z13`).  
- $P^{i}_{5}$: Additional partner costs (other than drugs) (`F1, 1, Z50`).  
- $P^{i}_{6}$: Goverment financial costs per year (`F1, 1, X41`).  
- $P^{i}_{7}$: Goverment staff value time (`F1, 2, N42`).  

- $Q^{i}_{2}$: total adjusted children dewormed per year (`F1, 1, Z15`).  

$P^{i}_{2}$ and $F^{i}_{2}$ are defined in terms of some of the previous elements.

\begin{equation}
F^{i}_{2} = \frac{P^{i}_{2}}{P^{i}_{1}}\\
P^{i}_{2} = \frac{P^{i}_{3}}{Q^{i}_{2}}
\label{eq:5}
\tag{5}
\end{equation}

- $P^{i}_{2}$: DtW costs per children per year (`F1, 2, C:G14`).  

The total costs of DtW across all years ($P^{i}_{3}$) is aggregated across years, regions (within country), cost activity (policy & advocacy, prevalence surveys, drug procurement & management, training & distribution, public mobilization & community sensitization, monitoring & evaluation, program management, giveWell added costs[^2]) and deworming round (often February or August)

\begin{equation}
P^{i}_{3} = \sum_{t \in years (G_{2})}
\sum_{r \in region (G_{3})}
\sum_{a \in activ (G_{4})}
\sum_{m \in month (G_{5})} P^{itram}_{3}
\label{eq:6}
\tag{6}
\end{equation}


- $P^{itram}_{3}$: costs by year, location, region, activity, and month. (`F1, [r+m+y], E15:21`)

Total number of adjusted children ($Q^{i}_{2}$) is computed as follows:

\begin{equation}
Q^{i}_{2} = \sum_{t \in G_{2}}
\sum_{r \in G_{3}}
 Q^{itr}_{2} \\
 Q^{itr}_{2} = \frac{\widetilde{Q}^{itr, Feb}_{2} - \widetilde{Q}^{itr, Aug}_{2}}{Q^{itr}_{3}}\\
 \widetilde{Q}^{itr, m}_{2} = \sum_{s \in sch.age (G_{6})} \left(
\sum_{e \in enr.st(G_{7})} \widetilde{Q}^{itrmse}_{2}\right) Q_{4}^{itrm}
\label{eq:7}
\tag{7}
\end{equation}



- $Q^{itr}_{2}$, and $Q^{itrm}_{2}$ (`F1, 4, B19` and `F1, [country] num of children (C), AB:AC34`)
- $Q^{itr}_{3}$: Actual treatment rounds per year (`F1, 4, D[crt]`)
- $Q^{itrm}_{4}$: Adjustment factor (`F1, C, [rmt]25`)


\begin{equation}
Q^{itrm}_{4} = 1 - \left.\left( \frac{Q^{itrms,en}_{2}}{Q^{itrms,en}_{5}} - F^{itrm}_{3} F^{itrm}_{4} F^{itrm}_{5} \right)  \middle/ \left( \frac{Q^{itrms,en}_{2}}{Q^{itrms,en}_{5}} \right) \right.
\label{eq:8}
\tag{8}
\end{equation}


- $Q^{itrms, en}_{5}$: Total enrolled school-aged children targeted (`F1, C, [rtm]13`)
- $F^{itrm}_{3}$: Percentage of schools visited during coverage validation (and/or during process monitoring) that distributed deworming tablets on deworming day and/or mop-up day (`F1, C, [rtm]22 `).   
- $F^{itrm}_{4}$: Percentage of enrolled school-aged children attending school on deworming day or mop-up day, according to attendance registers viewed in schools visited during coverage validation (and/or during process monitoring) (`F1, C, [rtm]23 `).   
- $F^{itrm}_{5}$: Percentage Of children enrolled in a school that distributed deworming tablets on deworming day and/or mop-up day and who attended school on deworming day and/or mop-up day, percentage who reported consuming deworming tablets (according to student interviews during coverage validation and/or process monitoring) (`F1, C, [rtm]24 `).  


\begin{equation}
P^{i}_{4} = \sum_{t}
\sum_{r}
\sum_{m} P^{itrm}_{4}
\label{eq:9}
\tag{9}
\end{equation}


- $P^{itrm}_{4}$: Cost of donated drugs in country, location, year, month (`F1, [crtm], E18`).  


\begin{equation}
P^{i}_{5} = \sum_{t}
\sum_{r}
\sum_{m} \widetilde{P}^{itrm}_{5}  - P^{i}_{4} \\
\widetilde{P}^{itr}_{5} = \sum_{m}
\widetilde{P}^{itrm}_{5}
\label{eq:10}
\tag{10}
\end{equation}



- $P^{itrm}_{4}$: Cost of donated drugs in country, location, year, month (`F1, [crtm], E18`).  
- $\widetilde{P}^{itrm}_{5}$: Total partner costs (incl. drugs) (`F1, [crtm], E23`).


\begin{equation}
P^{i}_{6} = \sum_{t}
\sum_{r} \widetilde{P}^{itr}_{6}  \\
\widetilde{P}^{itr}_{6} = P^{itr}_{8} - ( \widetilde{P}^{itr}_{5} + P^{itr}_{3})
\label{eq:11}
\tag{11}
\end{equation}

- $P^{itr}_{8}$: Total costs across activities, including GiveWell (`F1, 1, [rtc]11`).  

\begin{equation}
P^{itr}_{8} = \widetilde{P}^{itr}_{8} + P^{itr}_{9}\\
\widetilde{P}^{itr}_{8} = \sum_{m}
\sum_{a} \widetilde{P}^{itrma}_{8}  \\
\widetilde{P}^{itr}_{9} = \widetilde{P}^{itr}_{8} \frac{F_{6}}{1 - F_{6}}
\label{eq:12}
\tag{12}
\end{equation}

- $P^{itr}_{9}$: Total costs across activities, including GiveWell (`F1, 1, [rtc]10`).  
- $F_{6}$: Percentage of costs that go into high-level activities (`F1, 1, E62`) 

\begin{equation}
P^{i}_{7} = \sum_{t} \sum_{r} \left(  \sum_{j \in G_8} \left( \sum_{k \in G_9}\widetilde{P}^{itrjk}_{7}/2  \right) \right)
\label{eq:13}
\tag{13}
\end{equation}

- $\widetilde{P}^{itrjk}_{7}$:  Cost per treatment imputed to training and deworming day (`F1, 3, K23`).  

\begin{equation}
\widetilde{P}^{i}_{7} = \frac{P_{10}/P_{11}}{Q_{6}}
\label{eq:14}
\tag{14}
\end{equation}

- $P^{itr}_{10}$: Cost of training and DD in local currency (`F1, 3, H23`).  
- $P^{itr}_{11}$: Exchange rate (1 USD =  X local currency) (`F1, 3, J16`).  
- $Q_{6}$:  Total number of children treated in location r (`F1, 3, J17`) 

\begin{equation}
Q^{i}_{1} = \left. \left( \sum_{t\in 16, 17} P_{3}^{it}  P_{12}^{i}\right) \middle/ P_{2}^{i}\right.
\label{eq:15}
\tag{15}
\end{equation}

- $P^{itr}_{12}$: DtW spending commitments for the future (2018-2020) (`F1, 2, C:G4`).  

### Benefits ("$B$")


\begin{equation}
B = \left( \frac{1}{1 + F_{7}} \right)^{Q_{7}} \sum_{t = 0}^{Q_{8} - 1}
\frac{ln( 1 + F_{8})}{(1 + F_{7})}F_{9}F_{10}F_{11}F_{12}F_{13}\frac{F_{15}}{F_{14}}W
\label{eq:16}
\tag{16}
\end{equation}


- $F_{7}$: Discount rate[^3] (`F2, 2, B3`).  
- $F_{8}$: Treatment effect of deworming on earnings (15.4%)[^4] (`F2, 2, B3`).  
- $F_{9}$: Multiplier for resource sharing within households[^5] (`F2, 2, B9`).   
- $F_{10}$: Adjustment for El Niño[^6] (`F2, 4, B10`).  
- $F_{11}$: Adjustment for years of treatment in Miguel and Kremer vs. years of treatment in charities' programs[^7] (`F2, 4, B11`).  
- $F_{12}$: Replicability adjustment[^8] (`F2, 4, B12`).  
- $F_{13}$: Additional years of treatment assigned to the treatment group from Miguel and Kremer[^9] (ITT to TOT?) (`F2, 4, B13`).  
- $F_{14}$: Proportion of deworming going to children — DtW (`F2, 4, B18`).  
- $F_{15}$: Worm intensity adjustment (`F3, 1, I52`).  
- $Q_{7}$: Time to labor market[^10]  (`F2, 4, B5`).  
- $Q_{8}$: Time in the labor market (`F2, 4, B7`).  
- $W$: Moral weights (conversion from increase in earnings to utils) [^11] (`F2, 3, B21`).  

\begin{equation}
F_{15} = \frac{\sum_{r \in G_{10}} F^{r}_{16} F^{r}_{17}}{\sum_{r}  F^{r}_{17}}
\label{eq:17}
\tag{17}
\end{equation}

- $F^{r}_{16}$: DtW weight for worm intensity by location, adjusted (windorinzation) (`F3, 1, X14`)  
- $F^{r}_{17}$: DtW weight for worm intensity by location, unadjusted (`F3, 1, C14` --> `F3, 10, K26`)  


\begin{align}
  F^{r}_{16} &= \left\{
  \begin{array}{lr}
        \widetilde{F}^{r}_{16}, & \text{if } q(\widetilde{F}^{r}_{16}) \in (10\%, 90\%)\\
        q(\widetilde{F}^{r}_{16})_{10}& \text{if } q(\widetilde{F}^{r}_{16})\leq q(\widetilde{F}^{r}_{16})_{10}\\
        q(\widetilde{F}^{r}_{16})_{90} & \text{o/w } 
  \end{array}
        \right.\\
  \widetilde{F}^{r}_{16} &= 0.8F_{18} + 0.2\frac{F_{19}}{F_{20}}F^{r}_{21}
        \label{eq:18}
\tag{18}
\end{align}

- $\widetilde{F}^{r}_{16}$: Weight before winzorizing (`F3, 1, U14`)  
- $F_{18}$: Intensity adjustment to account for S. haem (`F3, 1, K14`)  
- $F_{19}$: Average intermediate intensity adjustment across regions and charities (`F3, 1, G50`)   
- $F^{r}_{21}$: Average Goblal Burden of Disease adjustment across regions and charities (`F3, 1, L50`)   
- $F_{21}$: Goblal Burden of Decease (GBD) adjustment for DtW in specific region (`F3, 1, L14`)  

\begin{equation}
F^{r}_{18} = F^{r}_{22} +  F^{r}_{23}
\label{eq:19}
\tag{19}
\end{equation}

- $F^{r}_{22}$:  DtW intermediate intensity adjustment for specific region (`F3, 1, G14`)  
- $F^{r}_{23}$:  Proportion of S. Haem in region (`F3, 1, J14`)  


\begin{equation}
F^{r}_{22} = F^{r}_{24} F^{r}_{25} F^{r}_{26}\\
F^{r}_{23} = 0.2 F^{r}_{27}\frac{\sum_{all} F^{r}_{22}}{0.8}
\label{eq:20}
\tag{20}
\end{equation}

- $F^{r}_{24}$: Calculated [location] intensity adjustment (`F3, 4, H27`).  
- $F^{r}_{25}$: Adjustment for representativeness (=100%)  (`F3, 1, E14`).   
- $F^{r}_{26}$: Adjustment for co-infection (=100%)    (`F3, 1, EF4`).  
- $F^{r}_{27}$: Prevalence rate of schisto haem  (`F3, 1, I14` --> `F3, 3, O18`)   

\begin{equation}
F^{r}_{21} = \sum_{d\in G_{11}}F^{d}_{28}F^{d}_{32}\\
F^{r}_{24} = \sum_{d\in G_{11}}F^{d}_{28}F^{d}_{29}\\
F^{r}_{27} = \frac{\widetilde{F}^{r}_{27}}{ \sum_{all} \widetilde{F}^{r}_{27} }\\
F^{r}_{29} = \frac{F_{30}}{F_{31}}
\label{eq:21}
\tag{21}
\end{equation}


- $F^{r}_{28}$: Infection weight (`F3, 4, B:E2`).  
- $F^{r}_{29}$: Prevalence rates of each infection by location (`F3, 4, B:E27`).  
- $F^{r}_{30}$: Eggs per gram in [location] and [condition] (`F3, 3, E18`).  
- $F^{r}_{31}$: Eggs per gram in original study by [condition] (`F3, 3, E4`).  
- $F^{r}_{32}$: Intensity adjustment from GBD (`F3, 7, F18`).  


\begin{equation}
F^{r}_{32} = \frac{Q^{dr}_{9}}{Q^{d}_{10}}\\
Q^{dr}_{9} = Q_{11,1}(F^{dr}_{33})^3 + Q_{11,2}(F^{dr}_{33})^2 + Q_{11,3}F^{dr}_{33}\\
F^{r}_{33} = F_{34,1} + F_{34,2}
\label{eq:22}
\tag{22}
\end{equation}


- $F^{r}_{33}$: Average prevalence for two age groups (`F3, 7, J18`).  
- $Q^{dr}_{9}$: Intensity (eggs per gram) (`F3, 6, V17`).    
- $F^{r}_{34, age}$: Prevalence for each age group (`F3, 6, T:S17`).  
- $Q^{d}_{10}$: Eggs per gram by type of desease (`F3, 7, B:E5`).    

\begin{equation}
F^{r}_{17} = \frac{F^{r}_{35}F^{r}_{36}}{\sum_{r}F^{r}_{35}F^{r}_{36}}
\label{eq:23}
\tag{23}
\end{equation}

- $F^{r}_{35}$: Proportion of projected treatments in [location] from 2017-2020 (`F3, 10, J26`).  
- $F^{r}_{36}$: Proportion of all costs paid by DtW in 2014-2015 (`F3, 10, H26->D42`).  

\begin{equation}
F^{r}_{35} = \frac{Q^{r}_{11}}{\sum_{r}Q^{r}_{11}}\\
F^{r}_{36} = \frac{P^{r}_{13}}{P^{r}_{14}}\\
Q^{r}_{11} = \frac{Q_{12} Q^{r}_{13}}{\sum_{r}Q^{r}_{13}}
\label{eq:24}
\tag{24}
\end{equation}

- $Q^{r}_{11}$: 2017-2020 projected treatments (`F3, 10, E26`).  
- $Q_{12}$: 168,996,327 (in `F3, 10, E26`).  
- $Q^{r}_{13}$: Treatments in [location]  in 2015-16 (`F3, 10, D26`).  
- $P^{r}_{13}$: DtWI costs in [location] (`F3, 10, B42`).  
- $P^{r}_{14}$: Total costs in [location] (`F3, 10, C42`).  



```r
# - inputs: tax_rev_init_mo, top_tax_base_in
# - outpu ts: total_rev_pe
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


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit

[^4]: "Roughly 10 years after the initial experiment described in Miguel and Kremer 2004, The Kenya Life Panel Survey 2 (KLPS-2) measured earnings in the Miguel and Kremer 2004 study population. This was followed by a 15 year follow-up survey, the KLPS-3.   
 We estimate that the average treatment household experienced a 15.4% increase in earnings. This corresponds to an increase in ln(earnings) of 0.143. This estimate is based on a preliminary, confidential analysis by Ted Miguel and others that pools earnings data from the KLPS-2 and KLPS-3. The estimate is formed using the total earnings findings across the whole sample (including individuals with zero earnings) and trimming the top 1% of earners in both the treatment and control groups. We take the log effect of earnings at the sample mean. The p-value for this result is .019. The authors have graciously shared their preliminary estimates with us, but we cannot yet publish the full details.   
  Note that this is an intention-to-treat effect. Not all individuals in the study population received treatment. We adjust this estimate for treatment compliance later using a parameter that captures the additional years of deworming received by the treatment group. A private, internal-only document related to this parameter is available to GiveWell staff at https://docs.google.com/document/d/1AK1H5kZi7-60zcxZs1vpZORVXk6M2y3DkkKymU50FkY/edit#heading=h.fm1rpvmo3lo. "

[^5]: "If a person treated for worms earns additional income and supports a family, then multiple people may benefit—not just the person who was dewormed.   
 In a multi-person household with one wage earner, a 10% increase in wages could enable every member of the household to consume 10% more. However, many households will have multiple wage earners, and household size may change overtime.  
 A rough model for estimating a value for this parameter is available at https://docs.google.com/spreadsheets/d/112uuyYt6QLRZuJojwz6fHv4JQ-GHNeIpiT-SauY3kmM/edit#gid=0. The appropriate value for this parameter will depend on many uncertain factors (e.g. household composition and how household composition changes overtime). We currently use a default value of 2.0. Our rough model suggests values close to 2 under a range of reasonable assumptions."


[^6]: "Updated cell note for the parameter:
Baird et al. 2016 compares the first two groups of schools to receive deworming (as treatment group) to group 3 (control).  
 Our worm intensity adjustment is calculated by comparing baseline intensity of infection in group 1 schools with baseline intensity of infection in the areas where our charities work.  
 This adjustment accounts for group 2 having a significantly higher baseline intensity of infection than group 1, likely due to flooding from an unusually strong El Niño event.   
 The average of baseline worm intensity in groups 1 and 2 was ~45% higher than in group 1 only (based on data we do not have permission to share). Taking the odds ratio implies an adjustment of ~65%.   
 (Last updated September 2018 | Link to Baird et al. 2016: https://doi.org/10.1093/qje/qjw022 | Link to Miguel and Kremer 2004: http://www.jstor.org/stable/3598853) Suggested value:65%.  More info here: https://docs.google.com/document/d/1pyGpVYcerlUSObdaIz7_6Tmv1D8O0L4IGQx1HtzTVbk/edit"

[^7]:"This is a really complicated input. I'm going with a value of 75% for now since it seems like a lot of deworming programs are in their infancy and hence will be providing early years of treatments to targeted children, but I think that my best guess would vary between 60-100% depending on the charity if I were just thinking on the margin. (In particular, SCI closer to 60%, DtWI closer to 100%.) Right now there isn't a great way to change this input for each charity, so I'm taking something toward the lower end of the range for all charities. 
 I think in the long-run we should do a better job answering the question, "How many years of deworming treatment have children already received on average in places where our charities would spend marginal funds?" This would involve figuring out whether LF programs had been operating, whether pre-school-aged kids receive deworming, etc.   
 A major reason one might want to use ~70-100%: for a few charities, the bulk of their marginal funding seems to be going toward starting deworming programs in regions that may not have received any deworming before. In those cases, if I look at just the next few years of deworming impact, I probably wouldn't discount for years of treatment vs. Baird at all because the average year of deworming treatment being provided will be a kid's ~second year, similar to the deworming treatment effect that we're observing in Baird. However, thinking marginally like this complicates our CEA: we'll need to change this discount over time as charities' programs mature (assuming they continued to operate). I'm not sure about the best philosophical approach to CEAs here, but am currently thinking I'll change this discount in the future as charities' programs mature.  
If you just look at the long run and assume that on average charities will be providing kids' ~4th year of deworming treatment, then I think a discount like 65% is probably more appropriate.  
A few factors I considered that pushed in opposite directions:
- I think that on average our charities are treating kids who are slightly younger than were treated in Miguel and Kremer, pushing things slightly in favor of our charities since I expect deworming effects to be larger at younger ages.
- OTOH, it may be that in some cases our charities are operating in places where children received deworming as pre-schoolers, which would decrease marginal value of additional years of treatment. We don't have good information yet on how common this is.
- The deworming coverage rates in Miguel and Kremer were low, so it might be appropriate to think of the treatments provided in that study as roughly a kid's ~2nd and 3rd year of treatment rather than ~3rd and 4th. That pushes in favor of discounting charities' impact more.
- In the long run, I'm not sure how many years of treatment an average child will receive who is targeted by our charities' programs. It depends on the composition of ages in schools where our charities treat, how often kids drop out, how often kids miss deworming treatment over the course of their time in school, etc. I think just looking at the average age of child treated and knowing whether there are pre-school treatments would be useful here. In general I guessed that the average kid will receive ~8 years of treatment in the long-run.  
 As a sensitivity check to account for the above, I edited James's model for this input here: https://docs.google.com/spreadsheets/d/1lLF28COf90edQFipk4ZC7BPZd-kKoj9sNDDt1oH8_vc/edit?usp=sharing . That sheet is very rough and may be difficult to follow. It includes some information on Natalie's guesses about the average number of years of deworming that kids have received in each charity's program."

[^8]: https://docs.google.com/document/d/13Tb3AnIT7zPgmKcGuwH365gQj6chWd0-blBlbzXjKpA/edit?usp=sharing


[^9]: "Josh: I broadly agree with Chris's argument here, so moving to 1.69. Note that this makes a ~30% difference to the bottom line.  
 In summary: on the cost side of our CEA we use a TOT framework, but on the benefits side we use an ITT effect. That's conservative; Miguel and Kremer only achieved ~50-60% coverage in an average deworming round. If they'd achieved 100% coverage we should expect that the impact of the program would've been much larger. One way to convert ITT to TOT is to just divide by the coverage rate achieved in the experiment. This method is imperfect because there were likely spillover effects of deworming, which substantially complicates the analysis. I think a key question for how much to update is what you expect the relative magnitude of spillover benefits per person dewormed to be in Miguel and Kremer compared to our charities' programs. This is a complex question, but I'd guess that spillover benefits were slightly larger in Miguel and Kremer. As with Chris, rough estimates of offsetting effects lead me to want to make a ~70% adjustment here. (I'd decrease this adjustment by ~10-20% since I'd guess that spillover benefits were ~10-20% larger in M&K, but that's offset by a ~10-20% adjustment for lower coverage in the average round of the experiment.)   
More details below.  
More detailed background:
- Our CEA uses a treatment-on-the-treated (TOT) framework on the cost side: i.e., we estimate the cost per child actually dewormed by our recommended charities. However, we've been using an intention-to-treat (ITT) framework on the benefits side: We implicitly assumed that Miguel and Kremer 2004 achieved 100% coverage (i.e., the treatment group received the full additional 2.41 years of deworming that were assigned to it), when the actual difference in coverage between treatment and control was much smaller, more like 50-60%. [It seems the average coverage rate for each round was roughly in the 50-60% range in the treatment group and ~1% in the control group. See Table III, Pg. 170, Miguel and Kremer 2004. Note that in the text it says, "Take-up rates were approximately 75% in the treatment group and 5% in the control group." (p. 8), implying a relative difference of ~70% coverage. It seems like that was meant to estimate what portion of the treatment and control groups ever received any treatment, which is less directly comparable to the coverage rates we estimate for our charities' programs.]
- A simple way to inflate the ITT effect to a TOT effect is to divide the ITT effect by the difference in coverage rates between treatment and control. 
- However, there are potential issues with doing this: a major one is that other members of the treatment group may have substantially benefitted from spillovers of deworming treatment (i.e., less transmission of worms since nearby children had been treated). When we estimate the treatment effect under ITT, we're partially counting the direct benefits of treatment but also counting spillover benefits. So it exaggerates the benefits to just divide by the coverage rate; you're attributing a bigger effect for the full sample than we'd actually expect to exist if everyone were treated.  
I think the key question for deciding whether to simply inflate the ITT effect is: How do we expect the magnitude of spillover benefits per person dewormed in Miguel and Kremer to compare to the magnitude of spillover benefits in charities' programs? If the spillover benefits per person dewormed in Miguel and Kremer were equal to or smaller than in our charities' programs, then I think we would not be overestimating the benefits of deworming by using this simple inflator. Otherwise, we are.  
 My current best guess is that the spillover benefits were probably somewhat larger in the Miguel and Kremer study than in our charities' programs. But, I have huge uncertainty about the relative magnitudes; I'd likely need to do more research and formal modeling to answer it well.  
 Several complications with thinking about the magnitude of spillover benefits are:
a) We also expect that the control group may have benefitted from spillovers from the treatment group. This would lead the impact of deworming as measured in M&K relative to the true counterfactual of deworming treatment to be underestimated. Because this was a cluster RCT and worm transmission is geographically determined, it seems likely that the spillover benefits in control clusters were smaller than in untreated children in treatment clusters, but still may have been meaningful.
b) Children who are treated may also benefit from spillovers to some extent: if a higher portion of one's community is treated, then worm transmission may be more effectively reduced, leading to lower reinfection rates. This kind of benefit would also occur in charities' programs.
c) The relative impact of spillovers seems like it would depend substantially on: 1) coverage rates of deworming programs, 2) baseline intensity of worm infections. 
- Re coverage, our charities aim to treat everyone in their target regions and according to our reviews of SCI and Deworm the World Initiative may achieve coverage rates of ~80-95%, a higher coverage rate than was achieved in Miguel and Kremer. This means that there is a smaller untreated portion of the population that could receive spillover benefits. On the other hand, perhaps the relationship between coverage and spillovers is nonlinear: maybe if nearly all people in the community are treated then reinfection rates drop by a nonlinear amount.
- Re intensity, as Chris mentioned, perhaps spillover effects are larger when the worm burden is especially bad, in a nonlinear way. (We're currently adjusting linearly for worm intensity in our CEA.) OTOH, I could see an argument that spillovers are especially good at lower intensity levels, since maybe you could get closer to eliminating any infections.
d) So far I've mainly been thinking about spillovers on school-aged children. However, our model doesn't count any spillover benefits to pre-school-aged children even though they may exist (though could also depend on whether pre-school-aged children are receiving treatment in areas where our charities work). This is another reason you might expect that spillovers in charities' programs are actually larger than the amount of spillovers that we're including in our estimate when inflating the M&K effect.  
  I made a super rough model that one could play around with here: https://docs.google.com/spreadsheets/d/1oNyirkpFB-HYbXVMorloEZYpNm8k4XTFKV9egjXIPA4/edit?usp=sharing. I think there are probably major issues with it, so not putting weight on it, but it didn't make it seem like obviously we should expect much smaller spillover benefits in charities' programs than M&K; my current best guess was maybe ~10% less."

[^10]: https://docs.google.com/document/d/1NxN6SO8GNv1AhpHMy1-IGu-tStT7py29W8AFU0u_bLw/edit?ts=599b4840


[^11]: "Our model accounts for changes in the natural log of consumption. The logarithmic model captures the idea that money has diminishing value as you get more and more of it.   
 For example, our model considers a 50% increase in income as a little better than 50% as good as an 100% increase in income." https://www.givewell.org/how-we-work/our-criteria/cost-effectiveness/comparing-moral-weights
 
 
 [^12]: (`F2, 4, D259`)
