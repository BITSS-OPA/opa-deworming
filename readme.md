# Case Study on Open Policy Analysis For Cost Effectiveness Analysis of Deworming Interventions.

[See here for a complete list of all the collaborators behind this OPA](LINK TO DEWORMING CONTRIBUTORS).

(These are copied directly from the wealth tax OPA readme; do we want to do something similar for this repo?)  

Open in RStudio: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/BITSS/opa-wealthtax/master?urlpath=rstudio)

Go straight to the Shiny app: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/BITSS/opa-wealthtax/master?urlpath=shiny/code/interactive_visualization/)

This repository and its accompanying dynamic document and Shiny app are three key components of an Open Policy Analysis (OPA) on the costs and benefits of mass deworming interventions in various settings. Together, these materials create a transparent and reproducible analysis to facilitate collaboration and discussion about deworming policy. 

1 - [An interactive plot](http://wealthtaxsimulator.org/simulator_app/), which summarizes the interactions between all key parameters for a given set of assumptions. Materials can be found in: [`code\shiny_app`](https://github.com/BITSS-OPA/opa-deworming/tree/master/code/shiny_app).

2 - Open policy report in the form of a [dynamic document](http://wealthtaxsimulator.org/analysis/) [replace with link to deworming doc], which details all data, code, and assumptions included in the analysis. Materials can be found in: [`code\dynamic_doc`](https://github.com/BITSS/opa-wealthtax/tree/master/code/dynamic_doc).

3 - This Github repository, which stores all data, code, and materials necessary to replicate the analysis in full with minimal effort.  

This case study complies with the highest levels of the [Open Policy Analysis (OPA) Guidelines](https://www.bitss.org/opa/community-standards/). We also look to demonstrate tools, ideas, and practices through which OPA can be implemented in practice. This exercise, and future case studies, in turn will inform the Guidelines.

To learn more about BITSS and our OPA initiative [click here](https://www.bitss.org/opa/). If you would like to collaborate with BITSS to develop a demonstration of a OPA in your organization please email Aleks Bogdanoski (abogdanoski@berkeley.edu).

old Readme
-----
Looking for w_t:
- Calcs-Table 5
  - Row 21 ='Assumps&Panel A Calcs'!$B$135*'Assumps&Panel A Calcs'!$B$10*'Model Params&Exp Profiles'!R9*'Model Params&Exp Profiles'!R8
  - Mean starting wage, USD * Weeks per year * (1+Per-capita GDP growth, 2002-2011 (accessed 1/29/13))^years of xp * (1 + years of xp * Years of xp coef + years of xp^2 * Years of xp^2 coef)
    - Mean starting wage, USD = Mean starting wage (KSh) / exchange rate
      - Mean starting wage (KSh) = Ag wage (Suri, 2011) * share of hrs in ag + working wage * share of hrs working for wages + self-emp wage * share of hours in self-emp
        - share of hrs in ag = Control hrs per week, agriculture/(agric + working for wages+ self-employment)
          - hrs ag = Assumps&Panel A Calcs B25 - still not sure how it came from table 4 (no panel D and )
        - share of hrs in ag ...
        - share of hrs in self-emp ...
        - working wage = Control group hourly wage, working for wages (conditional on >=10 hrs per week)
        - self-emp wage = Monthly self-employed profits/(4.5*weekly self-employed hrs, cond hrs>0)
          - Monthly self-employed profits = Control group monthly self-employed profits
          - weekly self-employed hrs, cond hrs>0) = Control group weekly self-employed hours, conditional on hrs >0


## Questions about DW KLPS analysis  
 - lambda_1^{x} is in which units (hour wage, five year wage?)  
 - Display output as NPV  
 -
