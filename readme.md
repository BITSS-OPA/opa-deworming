# Case Study on Open Policy Analysis about Fiscal Impacts of Deworming Interventions.


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