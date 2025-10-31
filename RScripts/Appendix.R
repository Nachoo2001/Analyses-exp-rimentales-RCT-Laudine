#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
# --------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                -------------
#--------------------                             Appendices                               -----------------------------------# 
#--------------------                          Authors: XX & XX                            -----------------------------------#    
#--------------------                              Version 1                               -----------------------------------#  
#--------------------                              June 2024                               -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


# Instructions and comments for replication
# This file is built with chunk labels than are called from the appendix Rmarkdown document.
# The file should be run from the R project "Analysis_Notebooks".
# These chunks are to be launched after running the package loading and data wrangling scripts.

#------ DescriptiveAvgCoverageRate --------
tauxCouv <- MainDB %>% 
  select(c(Dep,Nom.commune, NationalAvgTauxCouv2021, 
           Taux.de.couv.global, Taux.de.couv.EAJE...ensemble, 
           Taux.de.couv.assistantes.maternelles)) %>% 
  distinct(Nom.commune, .keep_all = TRUE)

# Average coverage per department
table <- tauxCouv %>% 
  group_by(Dep) %>% 
  summarise(NationalAvgTauxCouv2021 = round(mean(NationalAvgTauxCouv2021, na.rm = TRUE), 1),
            AverageCoverage = round(mean(Taux.de.couv.global, na.rm = TRUE), 1),
            AverageEaje = round(mean(Taux.de.couv.EAJE...ensemble, na.rm = TRUE), 1),
            AverageAssmat = round(mean(Taux.de.couv.assistantes.maternelles, na.rm = TRUE), 1)
            #AverageDpt = mean(TAUXCOUV_DEP)
  ) %>% mutate(Dep = case_when( Dep == 75 ~ "Paris",
                                Dep == 93 ~ "Seine-Saint-Denis",
                                Dep == 94 ~ "Val-de-Marne")) %>% 
  arrange(desc( AverageCoverage))

# Create a flextable from the summarised table
avgCoverageRate <- flextable(table) %>%
  set_header_labels(
    Dep = "District",
    NationalAvgTauxCouv2021 = "National Average Coverage (2021)",
    AverageCoverage = "Average Coverage of the sample (2021)", 
    AverageEaje = "Average Coverage in daycare in the sample",
    AverageAssmat = "Average Coverage in childminders in the sample"
  )  %>%
  set_caption("Average Coverage Rates per District in our sample, based on 2021 data") %>%  # Add a title/caption
  width(j = c("Dep", "NationalAvgTauxCouv2021", "AverageCoverage", "AverageEaje", "AverageAssmat"), width = 3.8, unit = "cm") %>%  # Set wider column widths
  bold(i = 1, part = "header")  # Make the header bold

# Print flextable
avgCoverageRate


#------ DescriptiveCoverageRateDaycareChildmindersDept --------

# With 2021 Data
txcouv_EAJE_assmat_dept <- txcouv_pe_com_EAJE_assmat %>% 
  filter(Numéro.Département == 93 |Numéro.Département == 94 |Numéro.Département == 75) %>% 
  select(Numéro.Département, Taux.de.couv.EAJE...ensemble, Taux.de.couv.assistantes.maternelles, Taux.de.couv.global) %>% 
  group_by(Numéro.Département) %>%
  summarise(AverageCoverage = round(mean(Taux.de.couv.global, na.rm = TRUE), 1), 
            AverageEaje = round(mean(Taux.de.couv.EAJE...ensemble, na.rm = TRUE),1), 
            AverageAssmat = round(mean(Taux.de.couv.assistantes.maternelles, na.rm = TRUE),1)
  )%>% mutate(Numéro.Département = case_when( Numéro.Département == 75 ~ "Paris",
                                              Numéro.Département == 93 ~ "Seine-Saint-Denis",
                                              Numéro.Département == 94 ~ "Val-de-Marne")) %>% 
  arrange(desc( AverageCoverage))



# Create a flextable from the summarised table
EAJE_assmat_dept <- flextable(txcouv_EAJE_assmat_dept) %>%
  set_header_labels(
    Numéro.Département = "District",
    AverageCoverage = "Average Coverage of the district (2021)", 
    AverageEaje = "Average Coverage in daycare of the district",
    AverageAssmat = "Average Coverage in childminders of the district") %>%
  # Add a title
  set_caption( "Average Coverage in Districts (2021)") %>%
  # Adjust columns width
  width(j = 1:4, width = 4.5,  unit = "cm") %>%
  # Adjust columns width to fit content
  bold(i = 1, part = "header")  # Make the header bold

# Print flextable
EAJE_assmat_dept 


#------ DescriptiveCoverageRateDaycareChildmindersFrance --------


# Total France in 2021
total_france <- txcouv_pe_com_EAJE_assmat %>% summarise(
  AverageCoverage = round(mean(Taux.de.couv.global, na.rm = TRUE),1), 
  AverageEaje = round(mean(Taux.de.couv.EAJE...ensemble, na.rm = TRUE),1), 
  AverageAssmat = round( mean(Taux.de.couv.assistantes.maternelles, na.rm = TRUE),1)
)

# Create a flextable from the summarised table
flextable_france <- flextable(total_france) %>%
  set_header_labels(
    AverageCoverage = "Average coverage in France",
    AverageEaje = "Average coverage daycare in France",
    AverageAssmat = "Average coverage private childminders in France") %>%
  set_caption( "French national average Coverage rates (2021)") %>%
  width(j = 1:3, width = 6,  unit = "cm") %>%
  # Adjust columns width to fit content
  bold(i = 1, part = "header")  # Make the header bold

# Print flextable
flextable_france


#------------------ DESCRIPTIVE STATS -------------------

#----------------------- Descriptive -------------
# D'abord créer un dataframe avec les variables originales
tabDes_temp <- MainDB  %>% 
  mutate(
    # Variables catégorielles maintenues comme facteurs
    SingleMum = as.factor(ifelse(SingleMum == TRUE, "Yes", "No")),
    Active = as.factor(ifelse(Act3 == "Active", "Yes", "No")),
    Educ = as.factor(ifelse(Educ == "Sup", "Yes", "No")),
    BornFr = as.factor(ifelse(FrenchYNBaseline == "France", "No", "Yes")),
    EverUsedECS = as.factor(ifelse(UsedECEC == "Already used", "Yes", "No")),
    PlanToUseECS = as.factor(ifelse(ECSPlanToBaseline == TRUE, "Yes", "No")),
    HighECSCov = as.factor(ifelse(HighLowECECBaseline == "High ECEC covering", "Yes", "No")),
    DepParis = as.factor(ifelse(Dep == "75", "Yes", "No")),
    WorkPlanTo = as.factor(ifelse(WorkPlanTo == TRUE, "Yes", "No")),
    #BabyFemale = as.factor(ifelse(BabyFemale == TRUE, "Yes", "No")),
    Primipare = as.factor(ifelse(Primipare == TRUE, "Yes", "No")),
    ComputerAccess = as.factor(ifelse(ComputerYN == "Oui", "Yes", "No")),
    Age = Age,  # Garde numérique
    NumberOfChildren = as.numeric(NumberChildren),  # Garde numérique
    InfoBaseline = as.factor(ifelse(InfoBaseline == "Low knowledge", "Yes", "No")),
    FmilyEarnLessThan2500 = as.factor(ifelse(FmilyEarnLessThan2500 == "Yes", "Yes", "No")),
    DescriptiveNorms = as.factor(ifelse(DescriptiveNorms == "Yes", "Yes", "No")),
    NormsOpposedYes = as.factor(ifelse(NormsOpposedYes == "Yes", "Yes", "No")),
    LikertReturnHK = as.factor(ifelse(LikertReturnHK1or0 == "Yes", "Yes", "No")),
    TrustCreche = as.factor(ifelse(TrustCreche1or0 == "Yes", "Yes", "No")),
    Discount50 = as.factor(ifelse(Discount501or0 == 1, "Yes", "No")),
    ECSApp = as.factor(ifelse(ECSApp == 1, "Yes", "No")),
    AppCreche = as.factor(ifelse(AppCreche == 1, "Yes", "No")),
    ECSUseYes = as.factor(ifelse(ECSUseYes == 1, "Yes", "No")),
    UseCreche = as.factor(ifelse(UseCreche == 1, "Yes", "No"))
  ) %>%
  select(
    Educ, SingleMum, Age, Primipare, NumberOfChildren, BornFr,
    FmilyEarnLessThan2500, Discount50, InfoBaseline, Active,
    WorkPlanTo, EverUsedECS, PlanToUseECS, ComputerAccess,
    LikertReturnHK, TrustCreche, DescriptiveNorms, NormsOpposedYes,
    DepParis, HighECSCov, GenderChild, ECSApp, AppCreche,
    ECSUseYes, UseCreche
  )

# Création du tableau descriptif
tbl_summary <- tabDes_temp  %>%
  tbl_summary(
    type = list(
      Age ~ "continuous2",
      NumberOfChildren ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
      all_categorical() ~ c("{n} ({p}%)")
    ),
    digits = list(
      all_continuous() ~ c(1, 1),
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany",
    label = list(
      Educ ~ "The mother is high-SES (Post-secondary education)",
      SingleMum ~ "Single-parent family",
      Age ~ "Age of the mother",
      Primipare ~ "The household is primiparous",
      NumberOfChildren ~ "Number of children in the household",
      BornFr ~ "The mother has a migration background",
      FmilyEarnLessThan2500 ~ "The household earns less than €2,500 per month",
      Discount50 ~ "The mother is present biased",
      InfoBaseline ~ "Mother s knowledge about early childcare",
      Active ~ "The mother is active at the baseline",
      WorkPlanTo ~ "The mother wants to work after the maternity leave",
      EverUsedECS ~ "The household already accessed early childcare in the past",
      PlanToUseECS ~ "The mother wants to use early childcare",
      ComputerAccess ~ "The household has access to a computer",
      LikertReturnHK ~ "The mother believes in early childcare benefits",
      TrustCreche ~ "The mother trusts early childcare",
      DescriptiveNorms ~ "The majority of friends and relatives use early childcare",
      NormsOpposedYes ~ "The mother perceives social approval for using early childcare",
      DepParis ~ "The mother lives in Paris",
      HighECSCov ~ "Early childcare coverage in the area of residence",
      GenderChild ~ "Gender of the child",
      ECSApp ~ "Applied to any early childcare facility at endline",
      AppCreche ~ "Applied to any daycare center at endline",
      ECSUseYes ~ "Accessed any early childcare facility at endline",
      UseCreche ~ "Accessed any daycare center at endline"
    )
  ) %>%
  modify_header(label = "**Variable**",
                stat_0 = "**n = {N}**") %>%
  modify_caption("Descriptive Statistics") %>%
  bold_labels()

# Convert to flextable for Rmd output
ft <- tbl_summary %>%
  as_flex_table() %>%
  theme_booktabs() %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding = 3, part = "all") %>%
  align(align = "left", part = "all") %>%
  autofit()

ft
#---------------------- DiffEduc --------------------


tabVarEduc <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France", 0, 1),
    EverUsedECS1or0  = ifelse(UsedECEC == "Already used", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0), 
    NumberOfChildren = as.numeric(NumberChildren), 
    InfoBaseline1or0 = ifelse(InfoBaseline == "Low knowledge", 1, 0),
  ) %>% 
  select(
    "High-SES" = Educ1or0,
    "Single-parent family" = SingleMum1or0,
    "Age of the mother" = Age,
    "The household is primiparous" = Primipare1or0,
    "Number of children in the household" = NumberOfChildren,
    "The mother has a migration background" = BornFr1or0,
    # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present biased" = Discount501or0,
    "Mother s knowledge about early childcare" =InfoBaseline1or0,
    "The mother is active at the baseline" = Active1or0,
    "The mother wants to work after the maternity leave" = WorkPlanTo1or0,
    #"The mother did not smoke" = DidNotSmoke1or0,
    #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
    "The household has already used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "The mother wants to use early childcare" = PlanToUseECS1or0,  
    "The household has access to a computer" = ComputerYes1or0,
    # Intend to use, block variable
    #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
    #  "Knows only daycare" = KnowsCrecheOnly1or0,
    #    "Value socialization" = ValSocialisation,
    #"Believe in returns to early childcare" = LikertReturnHK1or0,
    #"The mother trusts early childcare" = TrustCreche1or0,
    "The mother lives in Paris" = DepParis1or0,
    "Early childcare coverage in the area of residence" = HighECSCov1or0,
    "Gender of the child" = GenderChild,
    #  StrataWave
    "Applied to any early childcare facility at endline" = ECSApp, 
    "Applied to any daycare center at endline" = AppCreche,
    "Accessed any early childcare facility at endline" = ECSUseYes, 
    "Accessed any daycare center at endline" = UseCreche
  )


summary_baseline_variables_SES <- tabVarEduc %>%
  tbl_summary(
    by = "High-SES",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference()%>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Low-SES",
                stat_2 ~ "High-SES") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Socio-Economic Status**") %>% 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_SES %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by SES across the variables used in this study") %>% 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.") 


#---------------------- DiffMig --------------------
summary_baseline_variables_Migration <- tabVarEduc %>%
  tbl_summary(
    by = "The mother has a migration background",  # 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Born abroad",  # 
                stat_2 ~ "Born in France") %>%  # 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Migration Background**") %>%  # 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_Migration %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by migration background across the variables used in this study") %>%  # 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#-------------------- DiffInfo --------------------

summary_baseline_variables_Info <- tabVarEduc %>%
  tbl_summary(
    by = "Mother s knowledge about early childcare",  # 
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "High knowledge",  # Changé ici
                stat_2 ~ "Low knowledge") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Initial Level of Knowledge**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_Info %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by level of baseline knowledge across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffPresentBias --------------------
summary_baseline_variables_TimeOrientation <- tabVarEduc %>%
  tbl_summary(
    by = "The mother is present biased",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Not present biased",  # Changé ici
                stat_2 ~ "Present biased") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Temporal Orientation**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_TimeOrientation %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by present bias across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#---------------------- DiffUse --------------------

summary_baseline_variables_PreviousUse <- tabVarEduc %>%
  tbl_summary(
    by = "The household has already used early childcare",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Never used",  # Changé ici
                stat_2 ~ "Already used") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Previous Childcare Usage**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_PreviousUse %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by previous childcare usage across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffActive --------------------

summary_baseline_variables_Activity <- tabVarEduc %>%
  tbl_summary(
    by = "The mother is active at the baseline",  # Changé ici
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Inactive",  # Changé ici
                stat_2 ~ "Active") %>%  # Changé ici
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Activity Status**") %>%  # Changé ici
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_Activity %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by baseline activity status across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")
#---------------------- DiffParis --------------------


tabVarDep <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France",0, 1),
    EverUsedECS1or0  = ifelse(UsedECEC == "Already used", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0), 
    NumberOfChildren = as.numeric(NumberChildren), 
    InfoBaseline1or0 = ifelse(InfoBaseline == "Low knowledge", 1, 0)
  ) %>% 
  select(Dep,
         "High-SES" = Educ1or0,
         "Single-parent family" = SingleMum1or0,
         "Age of the mother" = Age,
         "The household is primiparous" = Primipare1or0,
         "Number of children in the household" = NumberOfChildren,
         "The mother has a migration background" = BornFr1or0,
         # "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
         #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
         "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
         "The mother is present biased" = Discount501or0,
         "Mother s knowledge about early childcare" =InfoBaseline1or0,
         "The mother is active at the baseline" = Active1or0,
         "The mother wants to work after the maternity leave" = WorkPlanTo1or0,
         #"The mother did not smoke" = DidNotSmoke1or0,
         #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
         "The household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
         "The mother wants to use early childcare" = PlanToUseECS1or0,  
         "The household has access to a computer" = ComputerYes1or0,
         # Intend to use, block variable
         #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
         #  "Knows only daycare" = KnowsCrecheOnly1or0,
         #    "Value socialization" = ValSocialisation,
         #"Believe in returns to early childcare" = LikertReturnHK1or0,
         #"The mother trusts early childcare" = TrustCreche1or0,
         "The mother lives in Paris" = DepParis1or0,
         "Early childcare coverage in the area of residence" = HighECSCov1or0,
         "Gender of the child" = GenderChild,
         #  StrataWave
         "Applied to any early childcare facility at endline" = ECSApp, 
         "Applied to any daycare center at endline" = AppCreche,
         "Accessed any early childcare facility at endline" = ECSUseYes, 
         "Accessed any daycare center at endline" = UseCreche
  )

summary_baseline_variables_District_Paris <- tabVarDep %>%
  mutate(Paris_vs_Others = ifelse(Dep == "75", "Paris", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "Paris_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Paris",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Paris comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_District_Paris %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by district across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")

#---------------------- DiffSSD --------------------



# Second: Seine-Saint-Denis vs Others
summary_baseline_variables_District_SSD <- tabVarDep %>%
  mutate(SSD_vs_Others = ifelse(Dep == "93", "Seine-Saint-Denis", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "SSD_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Seine-Saint-Denis",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Seine-Saint-Denis comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))

summary_baseline_variables_District_SSD %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:6), unit = "cm", width=2) %>%  # Modifié pour inclure une colonne supplémentaire
  set_caption(caption = "Differences by district across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")


#---------------------- DiffVDM --------------------

# Third: Val-de-Marne vs Others
summary_baseline_variables_District_VDM <- tabVarDep %>%
  mutate(VDM_vs_Others = ifelse(Dep == "94", "Val-de-Marne", "Other districts")) %>%
  select(-c(Dep, "The mother lives in Paris")) %>% 
  tbl_summary(
    by = "VDM_vs_Others",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference() %>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Val-de-Marne",
                stat_2 ~ "Other districts") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**District: Val-de-Marne comparison**") %>%
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1))%>%
  modify_table_body(~ .x %>% select(-std.error))


summary_baseline_variables_District_VDM %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:6), unit = "cm", width=2) %>%  # Modifié pour inclure une colonne supplémentaire
  set_caption(caption = "Differences by district across the variables used in this study") %>%  # Changé ici
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.")



#------ TakeUPT2 ----------

## This chunk construct the balance table of covariates at baseline for the appendix
# We add more covariates for the second round, following the coments
tabVar <- MainDB  %>% filter(Assignment =="T2") %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(MigrationBackground == "Yes", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Already used", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    BabyFemale = ifelse(BabyFemale == TRUE, 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0)
  ) %>% 
  select(
    "Single-parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children in the household" = NumberOfChildren3,
    # "The mother has no child" = Primipare1or0,
    "The mother has a migration background" = BornFr1or0,
    "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present orientated" = Discount501or0,
    "The mother is active at baseline" = Active1or0,
    "The mother wants to work after maternity leaves" = WorkPlanTo1or0,
    #"The mother did not smoke" = DidNotSmoke1or0,
    #"The mother wants to breastfeed" = BreastFeedIntend1or0, 
    "The household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "The mother wants to use early childcare" = PlanToUseECS1or0,  
    "The household has access to a computer" = ComputerYes1or0,
    # Intend to use, block variable
    #  "Knows early childcare is subsidised" = AffordSubsidies1or0,
    #  "Knows only daycare" = KnowsCrecheOnly1or0,
    #    "Value socialization" = ValSocialisation,
    #"Believe in returns to early childcare" = LikertReturnHK1or0,
    #"The mother trusts early childcare" = TrustCreche1or0,
    "The mother lives in Paris" = DepParis1or0,
    "Early childcare coverage is high" = HighECSCov1or0,
    "Child is a girl" = BabyFemale,
    Suivi_administratif1_0
    #  StrataWave
  )

summary_baseline_variables_Comp <- tabVar %>%
  tbl_summary(
    by = Suivi_administratif1_0,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall() |>
  add_difference()%>%
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Non-Compliers",
                stat_2 ~ "Compliers") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Compliance**") %>% 
  add_significance_stars(thresholds = c(0.01, 0.05, 0.1)) %>%
  modify_table_body(~ .x %>% select(-std.error))



summary_baseline_variables_Comp %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1, part="body") %>%
  width(j=c(1), unit = "cm", width=3) %>%
  width(j=c(2:5), unit = "cm", width=2) %>%
  set_caption(caption = "Differences by compliance status to the support") %>% 
  add_footer_lines(
    "Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test.") 

#---------------------- Correlation --------------------


p_load(psych, corrplot)

# Select and recode the variables
variables <- MainDB %>% 
  mutate(
    HighSES = ifelse(Educ2 == "High-SES", 1, 0),
    MigrationBackground = ifelse(MigrationBackground == "No", 1, 0),
    Knowledge = ifelse(InfoBaseline == "High knowledge", 1, 0),
    NotPresentBias = ifelse(PresentOrientated == 0, 1, 0),
    PreviousEarlyChildcareUse = ifelse(UsedECEC == "Already used", 1, 0),
    Activity = ifelse(ActiveBaseline == "Active", 1, 0),
    BelieveInEarlyChildcareBenefits = ifelse(LikertReturnHK1or0 == "Yes", 1, 0),
    TrustInEarlyChildcare = ifelse(TrustCreche1or0 == "Yes", 1, 0),
    DescriptiveNorms = ifelse(DescriptiveNorms == "Yes", 1, 0),
    PrescriptiveNorms = ifelse(NormsOpposedYes == "No", 1, 0)
  ) %>% select(
    HighSES, MigrationBackground, Knowledge, NotPresentBias, PreviousEarlyChildcareUse, 
    Activity, BelieveInEarlyChildcareBenefits, TrustInEarlyChildcare, DescriptiveNorms, PrescriptiveNorms
  )

# Convert them into numerical variables
variables_num <- data.frame(lapply(variables, function(x) {
  if(is.factor(x)) as.numeric(x) - 1 else x
}))

# Remove variables with no variance (none but for some reason it creates a bug)
vars_with_variance <- names(which(sapply(variables_num, var, na.rm=TRUE) > 0))
variables_num_clean <- variables_num[, vars_with_variance]

tetrachoric_matrix <- tetrachoric(variables_num_clean)$rho

# Change the names so that they are prettier
nouveaux_noms <- c("SES", "Migration background", "Knowledge", 
                   "Present bias", "Previous early childcare use", 
                   "Activity", "Believe in early childcare benefits ", "Trust in early childcare", 
                   "Descriptive norms", "Prescriptive norms"
)

# Renames rows and cols
colnames(tetrachoric_matrix) <- nouveaux_noms
rownames(tetrachoric_matrix) <- nouveaux_noms

# Plot the correlation matrix
corrplot(tetrachoric_matrix, method="color", 
         type="upper", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,  # tl.srt : orientation
         tl.cex = 0.8,               # font size
         diag=FALSE)


#---------------------- RESULTS ---------------------------
#-------- IntentionActionSES -----------------

Control = MainDB %>% filter(Assignment == "Control")
library(fixest)
library(modelsummary)
cm <- c(  'Educ2Low-SES' = 'Low-SES \n(ref = High-SES)',
         "ECSPlanToBaselineTRUE" = "Intention to apply: Yes",  "ECSApp"= "Applied")

lm1 <- feols(as.numeric(ECSPlanToBaseline)~ Educ2 , data = MainDB, cluster = ~StrataWave)

lm2 <- feols(as.numeric(ECSApp)~  Educ2, data = Control, cluster = ~StrataWave)
lm5 <- feols(ECSApp~  Educ2 + ECSPlanToBaseline, data = Control, cluster = ~StrataWave)

lm3 <- feols(ECSUseYes~  Educ2, data = Control, cluster = ~StrataWave)
lm4 <- feols(ECSUseYes~  Educ2 + ECSApp, data = Control, cluster = ~StrataWave)



modelsummary(list(
  "Intention" = lm1,    
  "Application" = lm2, 
  "Application Intention" = lm5,
  "Access" = lm3, 
  "Access Apply" = lm4
  
), statistic = "{std.error} ({p.value})",
estimate="{estimate}{stars}  [{conf.low}, {conf.high}]",
gof_map = c("r.squared" ,"adj.r.squared" ,"nobs", "vcov.type", "FE"="FE: blocksMatWave" ),
stars = c("***"=.01,"**"=.05,"*"=.1),
title="",
notes=list("",
           "Coefficient, 95 % CI in brackets, standard errors, p-value or adjusted p-value in parenthesis")
,output = 'flextable',escape=TRUE, 
coef_map = cm
)|>
  bold(i = c(1),  part = "header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2, 4, 3, 5, 6),width=2.4,unit = "cm") 

#-------- IntentionActionMig -----------------

Control = MainDB %>% filter(Assignment == "Control")
library(fixest)
library(modelsummary)

cm <- c(  'MigrationBackgroundYes' = 'MigrationBackground: Yes',
          "ECSPlanToBaselineTRUE" = "Intention to apply: Yes",  "ECSApp"= "Applied")

lm1 <- feols(as.numeric(ECSPlanToBaseline)~ MigrationBackground , data = MainDB, cluster = ~StrataWave)

lm2 <- feols(as.numeric(ECSApp)~  MigrationBackground, data = Control, cluster = ~StrataWave)
lm5 <- feols(ECSApp~  MigrationBackground + ECSPlanToBaseline, data = Control, cluster = ~StrataWave)

lm3 <- feols(ECSUseYes~  MigrationBackground, data = Control, cluster = ~StrataWave)
lm4 <- feols(ECSUseYes~  MigrationBackground + ECSApp, data = Control, cluster = ~StrataWave)



modelsummary(list(
  "Intention" = lm1,    
  "Application" = lm2, 
  "Application Intention" = lm5,
  "Access" = lm3, 
  "Access Apply" = lm4
  
), statistic = "{std.error} ({p.value})",
estimate="{estimate}{stars}  [{conf.low}, {conf.high}]",
gof_map = c("r.squared" ,"adj.r.squared" ,"nobs", "vcov.type", "FE"="FE: blocksMatWave" ),
stars = c("***"=.01,"**"=.05,"*"=.1),
title="",
notes=list("",
           "Coefficient, 95 % CI in brackets, standard errors, p-value or adjusted p-value in parenthesis")
,output = 'flextable',escape=TRUE, 
coef_map = cm
) |> bold(i = c(1),  part = "header") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2, 4, 3, 5, 6),width=2.4,unit = "cm") 
#------ MECANISMS ---------------------------


#-------- MechanismsInformationCosts -----------------

# First etimate the ITT for applications
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSApp",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.App.Used <- GroupHeterogeneityFnCTRL(DB = PostDB,
  Outcome = "ECSApp",
  Heterogeneity= "UsedECEC",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSUseYes",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.Used <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSUseYes",
  Heterogeneity = "UsedECEC",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# Stack control for application
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Norms$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.App.Used$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.App.Info$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.App.Norms$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class



# Stack Itt for application
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.App.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.App.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.App.Norms$ModelSummary$glance
)

class(StackedITTApp) <- "modelsummary_list"   # define the class


# Stack control for use
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Norms$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.Use.Used$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.Use.Info$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.Use.Norms$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.Use.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.Use.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.Use.Norms$ModelSummary$glance
)

class(StackedITTUse) <- "modelsummary_list"   # define the class




# Step 2 : estimate the conditional ATTs of interest using the function
## First etimate the ITT for applications
Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "ECSApp",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.App.Used <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "ECSApp",
  Heterogeneity= "UsedECEC",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ATT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "ECSUseYes",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.Used <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "ECSUseYes",
  Heterogeneity = "UsedECEC",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "ECSUseYes",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")




# Stack Itt for application
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ATT.App.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ATT.App.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ATT.App.Norms$ModelSummary$glance
)

class(StackedATTApp) <- "modelsummary_list"   # define the class


# Stack ITT for use
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ATT.Use.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ATT.Use.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ATT.Use.Norms$ModelSummary$glance
)

class(StackedATTUse) <- "modelsummary_list"   # define the class




# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))



# Now T2 angainst C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information + support treatlent on early childcare application and access"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")


ModelT2C

#-------- MechanismsInformationCostsDaycare -----------------
## First etimate the ITT for applications
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "AppCreche",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.App.Used <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "AppCreche",
  Heterogeneity= "UsedECEC",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "AppCreche",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
  Outcome = "UseCreche",
  Heterogeneity = "DescriptiveNorms",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.Used <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "UseCreche",
  Heterogeneity = "UsedECEC",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "UseCreche",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# Stack control for application
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Norms$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.App.Used$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.App.Info$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.App.Norms$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class



# Stack Itt for application
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.App.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.App.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.App.Norms$ModelSummary$glance
)

class(StackedITTApp) <- "modelsummary_list"   # define the class


# Stack control for use
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Norms$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.Use.Used$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.Use.Info$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.Use.Norms$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ITT.Use.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ITT.Use.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ITT.Use.Norms$ModelSummary$glance
)

class(StackedITTUse) <- "modelsummary_list"   # define the class




# Step 2 : estimate the conditional ATTs of interest using the function
## First etimate the ITT for applications
Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "AppCreche",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.App.Used <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "AppCreche",
  Heterogeneity= "UsedECEC",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ATT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "AppCreche",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

### Now let's get the models for the use

Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "UseCreche",
  Heterogeneity = "DescriptiveNorms",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.Used <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "UseCreche",
  Heterogeneity = "UsedECEC",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "UseCreche",
  Heterogeneity = "InfoBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")




# Stack Itt for application
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ATT.App.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ATT.App.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ATT.App.Norms$ModelSummary$glance
)

class(StackedATTApp) <- "modelsummary_list"   # define the class


# Stack ITT for use
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Norms$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Descriptive Norms"),
                   Het.ATT.Use.Used$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Ever Used Early Childcare"),
                   Het.ATT.Use.Info$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Level of knowledge")),
  glance = Het.ATT.Use.Norms$ModelSummary$glance
)

class(StackedATTUse) <- "modelsummary_list"   # define the class




# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))



# Now T2 angainst C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.

                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")


ModelT2C


#------- MechanismPsychologicalCosts ----------------
# First etimate the ITT for applications
Het.ITT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.App.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "ECSApp",
  Heterogeneity = "TrustCreche",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "ECSApp",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")

### Now let's get the models for the use

Het.ITT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDB,
  Outcome = "ECSUseYes",
  Heterogeneity = "TrustCreche",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "ECSUseYes",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")


# Stack control for application
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.PresentOrientated$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.App.TrustCreche1or0$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.App.ActiveBaseline$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.App.PresentOrientated$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class


# Stack Itt for application
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.App.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.App.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.App.PresentOrientated$ModelSummary$glance
)

class(StackedITTApp) <- "modelsummary_list"   # define the class


# Stack control for use
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.PresentOrientated$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.Use.TrustCreche1or0$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.Use.ActiveBaseline$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.Use.PresentOrientated$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.Use.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.Use.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.Use.PresentOrientated$ModelSummary$glance
)

class(StackedITTUse) <- "modelsummary_list"   # define the class




# Step 2 : estimate the conditional ATTs of interest using the function
## First etimate the ITT for applications
Het.ATT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.App.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "ECSApp",
  Heterogeneity = "TrustCreche",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "ECSApp",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")

### Now let's get the models for the use

Het.ATT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "ECSUseYes",
  Heterogeneity = "TrustCreche",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "ECSUseYes",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")


# Stack Itt for application
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ATT.App.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ATT.App.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ATT.App.PresentOrientated$ModelSummary$glance
)

class(StackedATTApp) <- "modelsummary_list"   # define the class


# Stack ITT for use
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ATT.Use.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ATT.Use.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ATT.Use.PresentOrientated$ModelSummary$glance
)

class(StackedATTUse) <- "modelsummary_list"   # define the class




# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))



# Now T2 angainst C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Models are jointly estimating conditional averages in each pair of treatment arm.
Adjusted p-value and confidence intervals account for simultaneous inference across treatment arms.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")


ModelT2C


#------- MechanismPsychologicalCostsDaycare ----------------

# First etimate the ITT for applications
Het.ITT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "AppCreche",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.App.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDB ,
  Outcome = "AppCreche",
  Heterogeneity = "TrustCreche",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "AppCreche",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")

### Now let's get the models for the use

Het.ITT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "UseCreche",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDB,
  Outcome = "UseCreche",
  Heterogeneity = "TrustCreche",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ITT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                       Outcome = "UseCreche",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = TRUE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")


# Stack control for application
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.PresentOrientated$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.App.TrustCreche1or0$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.App.ActiveBaseline$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.App.PresentOrientated$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class


# Stack Itt for application
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.App.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.App.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.App.PresentOrientated$ModelSummary$glance
)

class(StackedITTApp) <- "modelsummary_list"   # define the class


# Stack control for use
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.PresentOrientated$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.Use.TrustCreche1or0$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.Use.ActiveBaseline$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.Use.PresentOrientated$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ITT.Use.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ITT.Use.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ITT.Use.PresentOrientated$ModelSummary$glance
)

class(StackedITTUse) <- "modelsummary_list"   # define the class




# Step 2 : estimate the conditional ATTs of interest using the function
## First etimate the ITT for applications
Het.ATT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "AppCreche",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.App.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
  Outcome = "AppCreche",
  Heterogeneity = "TrustCreche",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.App.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "AppCreche",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")

### Now let's get the models for the use

Het.ATT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "UseCreche",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.TrustCreche1or0 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
  Outcome = "UseCreche",
  Heterogeneity = "TrustCreche",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")

Het.ATT.Use.ActiveBaseline <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                       Outcome = "UseCreche",
                                                       Heterogeneity = "ActiveBaseline",
                                                       ITT = FALSE,
                                                       Weights = "WeightPS",
                                                       clusters = "StrataWave")


# Stack Itt for application
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ATT.App.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ATT.App.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ATT.App.PresentOrientated$ModelSummary$glance
)

class(StackedATTApp) <- "modelsummary_list"   # define the class


# Stack ITT for use
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.PresentOrientated$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Present biased"),
                   Het.ATT.Use.TrustCreche1or0$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Trust"),
                   Het.ATT.Use.ActiveBaseline$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Activity")),
  glance = Het.ATT.Use.PresentOrientated$ModelSummary$glance
)

class(StackedATTUse) <- "modelsummary_list"   # define the class




# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))



# Now T2 angainst C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
Models are jointly estimating conditional averages in each pair of treatment arm.
Adjusted p-value and confidence intervals account for simultaneous inference across treatment arms.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7, 8),width=2.4,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")


ModelT2C


#----------- InteractionSESActive -----------
Het.ITT.App.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.App.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ITT.Use.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.Use.ActiveEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                     mutate(ActiveEduc=interaction(ActiveBaseline,Educ2)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "ActiveEduc",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

# Separate the interaction terms
Het.ITT.App.ActiveEduc$ModelSummary$tidy <- Het.ITT.App.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ITT.App.ActiveEduc$ModelSummary0$tidy <- Het.ITT.App.ActiveEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ATT.App.ActiveEduc$ModelSummary$tidy <- Het.ATT.App.ActiveEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Activity","SES"))

Het.ITT.Use.ActiveEduc$ModelSummary$tidy <- Het.ITT.Use.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ITT.Use.ActiveEduc$ModelSummary0$tidy <- Het.ITT.Use.ActiveEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Activity","SES"))

Het.ATT.Use.ActiveEduc$ModelSummary$tidy <- Het.ATT.Use.ActiveEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Activity","SES"))

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table with the new interaction terms
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.ActiveEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.ActiveEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.ActiveEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.ActiveEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.ActiveEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.ActiveEduc$ModelSummary),
             shape = term + Activity + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and employment status at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")

#----------- ATTITUDES -----------



#----------  LikertReturnHK1or0 -------------------


# App itt              
Het.ITT.App.Dev <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "BelieveBenefits", 
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ITT.Use.Dev <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "BelieveBenefits",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


# App Att
Het.ATT.App.Dev <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "BelieveBenefits",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


Het.ATT.Use.Dev <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "BelieveBenefits",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Early childcare table
Het.ITT.App.Dev$ModelSummary0$tidy= Het.ITT.App.Dev$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Dev$ModelSummary$tidy= Het.ATT.App.Dev$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Dev$ModelSummary0$tidy= Het.ITT.Use.Dev$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Dev$ModelSummary$tidy= Het.ATT.Use.Dev$ModelSummary$tidy %>% filter(term == "T2-C")

cm <- c('T2-C'    = 'Information + Support vs Control')

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Dev$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Dev$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Dev$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Dev$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Dev$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Dev$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by beliefs in early childcare benefits for child development",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")



#-------------- BeliefsReturnDaycare --------------------------

Het.ATT.App.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "BelieveBenefits",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ITT.App.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "BelieveBenefits",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")
Het.ITT.Use.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "BelieveBenefits",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")


Het.ATT.Use.Dev.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "BelieveBenefits",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Daycare table 
Het.ITT.App.Dev.Daycare$ModelSummary0$tidy= Het.ITT.App.Dev.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Dev.Daycare$ModelSummary$tidy= Het.ATT.App.Dev.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Dev.Daycare$ModelSummary0$tidy= Het.ITT.Use.Dev.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Dev.Daycare$ModelSummary$tidy= Het.ATT.Use.Dev.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

cm <- c('T2-C'    = 'Information + Support vs Control')


modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Dev.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Dev.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Dev.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Dev.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Dev.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Dev.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by beliefs in early childcare benefits for child development",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#---------------- Attitudes ----------------
#################### For AttitudeScoreMoreThanMedian ####################
# App itt              
Het.ITT.App.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian", 
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ITT.App.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = TRUE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.Use.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ITT.Use.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = TRUE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# App Att
Het.ATT.App.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ATT.App.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ATT.Use.Attitude <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")

Het.ATT.Use.Attitude.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "AttitudeScoreMoreThanMedian",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Early childcare table
Het.ITT.App.Attitude$ModelSummary0$tidy= Het.ITT.App.Attitude$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Attitude$ModelSummary$tidy= Het.ATT.App.Attitude$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Attitude$ModelSummary0$tidy= Het.ITT.Use.Attitude$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Attitude$ModelSummary$tidy= Het.ATT.Use.Attitude$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Attitude$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Attitude$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Attitude$ModelSummary,
                  "Early childcare access_Control mean"       =Het.ITT.Use.Attitude$ModelSummary0,
                  "Early childcare access_ITT"                =Het.ITT.Use.Attitude$ModelSummary,
                  "Early childcare access_ATT"                =Het.ATT.Use.Attitude$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by attitudes towards early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

# Daycare table 
Het.ITT.App.Attitude.Daycare$ModelSummary0$tidy= Het.ITT.App.Attitude.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Attitude.Daycare$ModelSummary$tidy= Het.ATT.App.Attitude.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Attitude.Daycare$ModelSummary0$tidy= Het.ITT.Use.Attitude.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Attitude.Daycare$ModelSummary$tidy= Het.ATT.Use.Attitude.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Attitude.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Attitude.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Attitude.Daycare$ModelSummary,
                  "Daycare access_Control mean"       =Het.ITT.Use.Attitude.Daycare$ModelSummary0,
                  "Daycare access_ITT"                =Het.ITT.Use.Attitude.Daycare$ModelSummary,
                  "Daycare access_ATT"                =Het.ATT.Use.Attitude.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                         "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by attitudes towards early childcare services",
             notes=paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer")%>% 
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#----------- InteractionSESBeliefs -----------
# For Early Childcare
Het.ITT.App.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                      mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                    Outcome = "ECSApp",
                                                    Heterogeneity = "BeliefsEduc",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ATT.App.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                      mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                    Outcome = "ECSApp",
                                                    Heterogeneity = "BeliefsEduc",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ITT.Use.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                      mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                    Outcome = "ECSUseYes",
                                                    Heterogeneity = "BeliefsEduc",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ATT.Use.BeliefsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                      mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                    Outcome = "ECSUseYes",
                                                    Heterogeneity = "BeliefsEduc",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")


# Separate the interaction terms for Early Childcare
Het.ITT.App.BeliefsEduc$ModelSummary$tidy <- Het.ITT.App.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ITT.App.BeliefsEduc$ModelSummary0$tidy <- Het.ITT.App.BeliefsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ATT.App.BeliefsEduc$ModelSummary$tidy <- Het.ATT.App.BeliefsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Believe","SES"))

Het.ITT.Use.BeliefsEduc$ModelSummary$tidy <- Het.ITT.Use.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ITT.Use.BeliefsEduc$ModelSummary0$tidy <- Het.ITT.Use.BeliefsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ATT.Use.BeliefsEduc$ModelSummary$tidy <- Het.ATT.Use.BeliefsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))


# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.BeliefsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.BeliefsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.BeliefsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.BeliefsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.BeliefsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.BeliefsEduc$ModelSummary),
             shape = term + Believe + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and beliefs in the benefits of early childcare for child development",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")

#------------ InteractionBeliefEducDaycare ------------

# For Daycare
Het.ITT.App.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "AppCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = TRUE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ATT.App.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "AppCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = FALSE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ITT.Use.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "UseCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = TRUE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

Het.ATT.Use.BeliefsEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                              mutate(BeliefsEduc=interaction(LikertReturnHK1or0,Educ2)),
                                                            Outcome = "UseCreche",
                                                            Heterogeneity = "BeliefsEduc",
                                                            ITT = FALSE,
                                                            Weights = "WeightPS",
                                                            clusters = "StrataWave")

# Separate the interaction terms for Daycare
Het.ITT.App.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ITT.App.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0$tidy <- Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ATT.App.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ATT.App.BeliefsEduc.Daycare$ModelSummary$tidy %>%
  separate(Group, into=c("Believe","SES"))

Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0$tidy <- Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Believe","SES"))

Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary$tidy <- Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Believe","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Daycare
modelsummary(list("Daycare application_Control mean" = Het.ITT.App.BeliefsEduc.Daycare$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.App.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare application_ATT" = Het.ATT.App.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare access_Control mean" = Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.Use.BeliefsEduc.Daycare$ModelSummary,
                  "Daycare access_ATT" = Het.ATT.Use.BeliefsEduc.Daycare$ModelSummary),
             shape = term + Believe + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by level of education and Beliefss towards early childcare",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")

#----------- InteractionSESTrust -----------
# For Early Childcare
Het.ITT.App.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                  mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "TrustEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.App.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                    mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "TrustEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ITT.Use.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                    mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "TrustEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.Use.TrustEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                    mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "TrustEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")



# Separate the interaction terms for Early Childcare
Het.ITT.App.TrustEduc$ModelSummary$tidy <- Het.ITT.App.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.App.TrustEduc$ModelSummary0$tidy <- Het.ITT.App.TrustEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.App.TrustEduc$ModelSummary$tidy <- Het.ATT.App.TrustEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc$ModelSummary$tidy <- Het.ITT.Use.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc$ModelSummary0$tidy <- Het.ITT.Use.TrustEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.Use.TrustEduc$ModelSummary$tidy <- Het.ATT.Use.TrustEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))


# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.TrustEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.TrustEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.TrustEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.TrustEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.TrustEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.TrustEduc$ModelSummary),
             shape = term + Trust + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and Trusts towards early childcare",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")


#------------ InteractionTrustEducDaycare ------------

# For Daycare
Het.ITT.App.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                            mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                          Outcome = "AppCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = TRUE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ATT.App.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                            mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                          Outcome = "AppCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = FALSE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ITT.Use.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                            mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                          Outcome = "UseCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = TRUE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

Het.ATT.Use.TrustEduc.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                            mutate(TrustEduc=interaction(TrustCreche,Educ2)),
                                                          Outcome = "UseCreche",
                                                          Heterogeneity = "TrustEduc",
                                                          ITT = FALSE,
                                                          Weights = "WeightPS",
                                                          clusters = "StrataWave")

# Separate the interaction terms for Daycare
Het.ITT.App.TrustEduc.Daycare$ModelSummary$tidy <- Het.ITT.App.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.App.TrustEduc.Daycare$ModelSummary0$tidy <- Het.ITT.App.TrustEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.App.TrustEduc.Daycare$ModelSummary$tidy <- Het.ATT.App.TrustEduc.Daycare$ModelSummary$tidy %>%
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc.Daycare$ModelSummary$tidy <- Het.ITT.Use.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ITT.Use.TrustEduc.Daycare$ModelSummary0$tidy <- Het.ITT.Use.TrustEduc.Daycare$ModelSummary0$tidy %>% 
  separate(Group, into=c("Trust","SES"))

Het.ATT.Use.TrustEduc.Daycare$ModelSummary$tidy <- Het.ATT.Use.TrustEduc.Daycare$ModelSummary$tidy %>% 
  separate(Group, into=c("Trust","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Daycare
modelsummary(list("Daycare application_Control mean" = Het.ITT.App.TrustEduc.Daycare$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.App.TrustEduc.Daycare$ModelSummary,
                  "Daycare application_ATT" = Het.ATT.App.TrustEduc.Daycare$ModelSummary,
                  "Daycare access_Control mean" = Het.ITT.Use.TrustEduc.Daycare$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.Use.TrustEduc.Daycare$ModelSummary,
                  "Daycare access_ATT" = Het.ATT.Use.TrustEduc.Daycare$ModelSummary),
             shape = term + Trust + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by level of education and Trusts towards early childcare",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")


#---------------------- Norms --------------------


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "NormsOpposedYes", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

# App Att
Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "NormsOpposedYes",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


cm <- c('T2-C'    = 'Information + Support vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms$ModelSummary$tidy= Het.ATT.App.Norms$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms$ModelSummary$tidy= Het.ATT.Use.Norms$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary,
                  "Early childcare access_ATT"                   =Het.ATT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether the household expect friends and relatives to look at them askance if they use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
 Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")


#---------------------- NormsDaycare --------------------



Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ATT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
Het.ATT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms.Daycare$ModelSummary$tidy= Het.ATT.App.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms.Daycare$ModelSummary$tidy= Het.ATT.Use.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects of the information only treatment on application and access to daycare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")



#---------------------- NormsDaycareT1 -------------------


Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "NormsOpposedYes",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cmInfo <- c('T1-C'    = 'Information only vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cmInfo,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects of the information only treatment on application and access to daycare by whether the household expect friends and relatives to look at them askance if they are using early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")


#----------- InteractionSESNorms -----------


# For Early Childcare
Het.ITT.App.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.App.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ITT.Use.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

Het.ATT.Use.NormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                    mutate(NormsEduc=interaction(NormsOpposedYes,Educ2)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "NormsEduc",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

# Separate the interaction terms for Early Childcare
Het.ITT.App.NormsEduc$ModelSummary$tidy <- Het.ITT.App.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ITT.App.NormsEduc$ModelSummary0$tidy <- Het.ITT.App.NormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ATT.App.NormsEduc$ModelSummary$tidy <- Het.ATT.App.NormsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("Norms","SES"))

Het.ITT.Use.NormsEduc$ModelSummary$tidy <- Het.ITT.Use.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ITT.Use.NormsEduc$ModelSummary0$tidy <- Het.ITT.Use.NormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("Norms","SES"))

Het.ATT.Use.NormsEduc$ModelSummary$tidy <- Het.ATT.Use.NormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("Norms","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.NormsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.NormsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.NormsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.NormsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.NormsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.NormsEduc$ModelSummary),
             shape = term + Norms + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and perceived prescriptive norms",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")




#---------------------- NormsDecriptiveT1 --------------------


##Opposition to childcare
# App itt              
Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms", 
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")



Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")


cm <- c('T1-C'    = 'Information only vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms$ModelSummary0$tidy= Het.ITT.App.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms$ModelSummary0$tidy= Het.ITT.Use.Norms$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Norms$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Norms$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Norms$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Norms$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3, 9),part="body")

#---------------------- NormsDaycareDescriptive --------------------
Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")
Het.ATT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")


Het.ATT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cm <- c('T2-C'    = 'Information + Support vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Norms.Daycare$ModelSummary$tidy= Het.ATT.App.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Norms.Daycare$ModelSummary$tidy= Het.ATT.Use.Norms.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare application_ATT"           =Het.ATT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by  whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")

#----------------------- DescriptiveDaycareT1 ------------------------
Het.ITT.App.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.Use.Norms.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DescriptiveNorms",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

cm <- c('T1-C'    = 'Information only vs Control')

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Norms.Daycare$ModelSummary0$tidy= Het.ITT.App.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy= Het.ITT.Use.Norms.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

modelsummary(list("Daycare application_Control mean"  =Het.ITT.App.Norms.Daycare$ModelSummary0,
                  "Daycare application_ITT"           =Het.ITT.App.Norms.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Norms.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Norms.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by  whether more than half of friends and relatives use early childcare",
             notes=paste(
               "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%                
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>                
  width(j=c(4,5),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(6,3),part="body")


#----------- InteractionSESDescriptiveNorms -----------

# For Early Childcare
Het.ITT.App.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSApp",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = TRUE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ATT.App.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSApp",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = FALSE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ITT.Use.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSUseYes",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = TRUE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

Het.ATT.Use.DescriptiveNormsEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   
                                                               mutate(DescriptiveNormsEduc=interaction(DescriptiveNorms,Educ2)),
                                                             Outcome = "ECSUseYes",
                                                             Heterogeneity = "DescriptiveNormsEduc",
                                                             ITT = FALSE,
                                                             Weights = "WeightPS",
                                                             clusters = "StrataWave")

# Separate the interaction terms for Early Childcare
Het.ITT.App.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ITT.App.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.App.DescriptiveNormsEduc$ModelSummary0$tidy <- Het.ITT.App.DescriptiveNormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ATT.App.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ATT.App.DescriptiveNormsEduc$ModelSummary$tidy %>%
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.Use.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ITT.Use.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0$tidy <- Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

Het.ATT.Use.DescriptiveNormsEduc$ModelSummary$tidy <- Het.ATT.Use.DescriptiveNormsEduc$ModelSummary$tidy %>% 
  separate(Group, into=c("DescriptiveNorms","SES"))

# Coef Map
cm <- c('T2-C' = 'Information + Support vs Control')

# Table for Early Childcare
modelsummary(list("Early childcare application_Control mean" = Het.ITT.App.DescriptiveNormsEduc$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.App.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare application_ATT" = Het.ATT.App.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare access_Control mean" = Het.ITT.Use.DescriptiveNormsEduc$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.Use.DescriptiveNormsEduc$ModelSummary,
                  "Early childcare access_ATT" = Het.ATT.Use.DescriptiveNormsEduc$ModelSummary),
             shape = term + DescriptiveNorms + SES ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3, std.error=2, conf.int=2, "Chi 2"=2, "P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
                         "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to early childcare by level of education and descriptive social norms",
             notes = paste("
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using themethod. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_", opts = c("center-hspan")) |>
  bold(i=1, part = "header") %>%
  merge_at(j=3, part="header") |>
  merge_at(j=2, part="header") |>
  merge_at(j=1, part="header") |>
  merge_v(j=1, part="body") |>
  merge_v(j=2, part="body") |>
  merge_v(j=3, part="body") |>
  italic(i = c(1), part = "header") %>% 
  italic(j = c(1), part = "body") %>% 
  fontsize(size=9, part="footer") %>% 
  fontsize(size=10, part="body") %>% 
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j=c(5,6,8,9), width=2.4, unit = "cm") |>
  width(j=c(2,3,4,7), width=2.2, unit = "cm") %>% 
  hline(c(3,6,9, 12, 17), part="body")


#---------- STRUCTURAL BARRIERS ------------


#--------------- GraphReasonsNoECS ----------------------------

# create the data for the plot by SES
ecs_reasons_simple <- MainDB %>%
  filter(!is.na(ECSNoECSAppAnswer_long)) %>%
  group_by(ECSNoECSAppAnswer_long) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count) * 100, 
         total = sum(count)) %>%
  arrange(desc(count))


# Create the plot
ggplot(ecs_reasons_simple, 
       aes(x = reorder(ECSNoECSAppAnswer_long, percentage), 
           y = percentage, 
           fill = ECSNoECSAppAnswer_long)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Percentage (%)") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2,
            size = 4)  # Increase percentage labels size

#---------- ReasonsSES --------
ecs_reasons_simple <- MainDB %>%
  filter(!is.na(ECSNoECSAppAnswer_long)) %>%
  group_by(ECSNoECSAppAnswer_long, Educ2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count) * 100, 
         total = sum(count)) %>%
  arrange(desc(count))

# Create the plot
ggplot(ecs_reasons_simple, 
       aes(x = reorder(ECSNoECSAppAnswer_long, total), 
           y = total, 
           fill = Educ2)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Count") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  )# +
#geom_text(aes(label = sprintf("%.1f%%", percentage)), 
#  hjust = -0.2,
size = 4)  # Increase percentage labels size


#--------------- GraphReasonIdealSES ----------------------------

# Convert to character
MainDB$ECSIdealNotReasonClean <- as.character(MainDB$ECSIdealNotReasonClean)

# Calculate results
# By education level
results_educ <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$Educ2)

# Create plots
# For education level
create_reasons_plot(results_educ, "Reasons for Not Getting Preferred Childcare by SES")


#--------------- GraphReasonIdealMigration ----------------------------
# By migration background
results_migr <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$FrenchYNBaseline)

# For migration background
create_reasons_plot(results_migr, "Reasons for Not Getting Preferred Childcare by Migration Background")


#--------------- GraphReasonIdealAll ----------------------------


results_educ <- count_reasons_by_group(MainDB$ECSIdealNotReasonClean, MainDB$Educ2)

results <- results_educ %>% 
  group_by(reason) %>%
  summarise(count_tot = sum(count)) %>%
  mutate(   total = sum(count_tot),
            percentage = count_tot/sum(count_tot)*100) %>%
  arrange(desc(count_tot))

# Create the plot
ggplot(results, 
       aes(x = reorder(reason, percentage), 
           y = percentage, 
           fill = reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  #scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Main reasons why parents did not get an early childcare spot",
       x = "",
       y = "Percentage (%)") +
  coord_flip() +
  theme(
    legend.position = "none",
    # Increase axis text size
    axis.text = element_text(size = 12),
    # Increase title size
    plot.title = element_text(size = 13, face = "bold"),
    # Increase axis title size
    axis.title = element_text(size = 12)
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2,
            size = 4)  # Increase percentage labels size


#------ HetCoverageRateEarlyChildcare --------

## Intersectional effect depatement and education: early cgildcare application and access

# to trick the function into making intersection treatment effects, you can simply create the 

Het.ITT.App.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>% 
                                                  mutate(DepEduc=interaction(Dep,Educ2)),
                                                Outcome = "ECSApp",
                                                Heterogeneity = "DepEduc",
                                                ITT = TRUE,
                                                Weights = "WeightPS",
                                                clusters = "StrataWave")

Het.ATT.App.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%  
                                                  mutate(DepEduc=interaction(Dep,Educ2)),
                                                Outcome = "ECSApp",
                                                Heterogeneity = "DepEduc",
                                                ITT = FALSE,
                                                Weights = "WeightPS",
                                                clusters = "StrataWave")

Het.ITT.Use.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2  %>%
                                                  mutate(DepEduc=interaction(Dep,Educ2)),
                                                Outcome = "ECSUseYes",
                                                Heterogeneity = "DepEduc",
                                                ITT = TRUE,
                                                Weights = "WeightPS",
                                                clusters = "StrataWave")

Het.ATT.Use.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>%                                                   mutate(DepEduc=interaction(Dep,Educ2)),
                                                Outcome = "ECSUseYes",
                                                Heterogeneity = "DepEduc",
                                                ITT = FALSE,
                                                Weights = "WeightPS",
                                                clusters = "StrataWave")

# intersectional estimation, need to add another variable for heterogeneity

Het.ITT.App.DepEduc$ModelSummary$tidy <-Het.ITT.App.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ITT.App.DepEduc$ModelSummary0$tidy <-Het.ITT.App.DepEduc$ModelSummary0$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ATT.App.DepEduc$ModelSummary$tidy <-Het.ATT.App.DepEduc$ModelSummary$tidy %>%
  separate(Group,into=c("District","Education"))

Het.ITT.Use.DepEduc$ModelSummary$tidy <-Het.ITT.Use.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ITT.Use.DepEduc$ModelSummary0$tidy <-Het.ITT.Use.DepEduc$ModelSummary0$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ATT.Use.DepEduc$ModelSummary$tidy <-Het.ATT.Use.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))


# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Early childcare application and access by level of education and district of residence at baseline
modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.DepEduc$ModelSummary0,
                  "Early childcare application_ITT"=Het.ITT.App.DepEduc$ModelSummary,
                  "Early childcare application_ATT"=Het.ATT.App.DepEduc$ModelSummary,
                  "Early childcare access_Control mean"  =Het.ITT.Use.DepEduc$ModelSummary0,
                  "Early childcare access_ITT"=Het.ITT.Use.DepEduc$ModelSummary,
                  "Early childcare access_ATT"=Het.ATT.Use.DepEduc$ModelSummary),
             shape = term + District+Education ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to early childcare by level of education and district of residence at baseline (75: Paris, 93: Seine-Saint-Denis, 94: Val de Marne)",
             notes=paste("Sources:", SourcesStacked,
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall method.
 Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
"),
             output = 'flextable')%>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=3,part="header")|>
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2,part="body")|>
  merge_v(j=3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(5,6, 8, 9),width=2.4,unit = "cm")|>
  width(j=c(2,3,4,7),width=2.2,unit = "cm") %>% 
  hline(c(6,12, 18),part="body")




#------ HetCoverageRateDaycare --------


Het.ITT.AppCreche.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(DepEduc=interaction(Dep,Educ2)),
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DepEduc",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ATT.AppCreche.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(DepEduc=interaction(Dep,Educ2)),
                                                      Outcome = "AppCreche",
                                                      Heterogeneity = "DepEduc",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ITT.UseCreche.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(DepEduc=interaction(Dep,Educ2)),
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DepEduc",
                                                      ITT = TRUE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

Het.ATT.UseCreche.DepEduc <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(DepEduc=interaction(Dep,Educ2)),
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "DepEduc",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")

# intersectional estimation, need to add another variable for heterogeneity

Het.ITT.AppCreche.DepEduc$ModelSummary$tidy <-Het.ITT.AppCreche.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ITT.AppCreche.DepEduc$ModelSummary0$tidy <-Het.ITT.AppCreche.DepEduc$ModelSummary0$tidy %>% 
  separate(Group,into=c("District","Education"))
Het.ATT.AppCreche.DepEduc$ModelSummary$tidy <-Het.ATT.AppCreche.DepEduc$ModelSummary$tidy %>%
  separate(Group,into=c("District","Education"))

Het.ITT.UseCreche.DepEduc$ModelSummary$tidy <-Het.ITT.UseCreche.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ITT.UseCreche.DepEduc$ModelSummary0$tidy <-Het.ITT.UseCreche.DepEduc$ModelSummary0$tidy %>% 
  separate(Group,into=c("District","Education"))

Het.ATT.UseCreche.DepEduc$ModelSummary$tidy <-Het.ATT.UseCreche.DepEduc$ModelSummary$tidy %>% 
  separate(Group,into=c("District","Education"))

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Création du tableau pour les effets moyens sur l'application et l'accès à la garde d'enfants selon le niveau d'éducation et le district

modelsummary(list("Daycare application_Control mean"  =Het.ITT.AppCreche.DepEduc$ModelSummary0,
                  "Daycare application_ITT"=Het.ITT.AppCreche.DepEduc$ModelSummary,
                  "Daycare application_ATT"=Het.ATT.AppCreche.DepEduc$ModelSummary,
                  "Daycare access_Control mean"  =Het.ITT.UseCreche.DepEduc$ModelSummary0,
                  "Daycare access_ITT"=Het.ITT.UseCreche.DepEduc$ModelSummary,
                  "Daycare access_ATT"=Het.ATT.UseCreche.DepEduc$ModelSummary),
             shape = term + District+Education ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
             title="Average effects on application and access to daycare by level of education and district of residence at baseline  
             (75: Paris, 93: Seine-Saint-Denis, 94: Val de Marne)",
             notes=paste("Sources:", SourcesStacked,
                         "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
 Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table."),
             output = 'flextable')%>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=3,part="header")|>
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2,part="body")|>
  merge_v(j=3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(5,6, 8, 9),width=2.4,unit = "cm")|>
  width(j=c(2,3,4,7),width=2.2,unit = "cm") %>% 
  hline(c(6,12,18),part="body") 



#------ EarlyChildcareHetTableInformationOnly ------------

# Step 1 : estimate the conditional ITTs of interest using the function
## First estimate the ITT for applications
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# We get many results in each function:
# - ModelSummary0 - prepare a modelsummary table for the control group, basic table : modelsummary(NAME$ModelSummary0,shape=term+Group~model)
# - `Model 0` is the raw model presented in ModelSummary0
# - Estimation - gets the main estimates (ITTs or LATEs)
# - modelsummary prepare a modelsummary table for the main estimates, basic table : modelsummary(NAME$ModelSummary,shape=term+Group~model)
# - Tidy : the tidy version with both models

# Stack control for application - only SES and Migration background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - only SES and Migration background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - only SES and Migration background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - only SES and Migration background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Put that in a list - only ITT (no ATT)
TheModels <- list(StackedControlApp,
                  StackedITTApp,
                  StackedControlUse,
                  StackedITTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with an underscore to separate them after
names(TheModels) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                      paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                      paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                      paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"))

# We start with table with T1 
cmT1 <- c('T1-C' = 'Information-only vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information-only treatment by SES and migration background"

# Now the infamous model summary 
ModelT1CECS <- modelsummary(TheModels,
                            shape= Variable + Group ~ model,
                            fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                            estimate = '{estimate}{stars} ({std.error})',
                            statistic = c("conf.int",
                                          "adj.p.val. = {adj.p.value}"),
                            stars = c('*' = .1,'**' = .05, '***' = .01),
                            coef_map = cmT1,
                            gof_map = c('Fixed effects',"N"),
                            title=TheTitle,
                            notes=paste("Sources:", SourcesStacked,
                                        "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table." 
                            ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variableiable labels bold
  merge_at(j=1:2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,6),width=2.6,unit = "cm")|>
  width(j=c(1,2,3,5),width=2.2,unit = "cm") %>% 
  hline(i= c(3*c(1:3)), j=2:7, part="body") %>%
  hline(i=12, j=1:7, part="body")

ModelT1CECS


#------ DaycareTableInformationOnly ------------
# Step 1 : estimate the conditional ITTs of interest using the function
## First estimate the ITT for applications
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "AppCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "AppCreche",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "UseCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# We get many results in each function:
# - ModelSummary0 - prepare a modelsummary table for the control group, basic table : modelsummary(NAME$ModelSummary0,shape=term+Group~model)
# - `Model 0` is the raw model presented in ModelSummary0
# - Estimation - gets the main estimates (ITTs or LATEs)
# - modelsummary prepare a modelsummary table for the main estimates, basic table : modelsummary(NAME$ModelSummary,shape=term+Group~model)
# - Tidy : the tidy version with both models

# Stack control for application - only SES and Migration background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - only SES and Migration background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.App.Educ2C$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - only SES and Migration background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - only SES and Migration background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ITT.Use.Educ2C$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Put that in a list - only ITT (no ATT)
TheModels <- list(StackedControlApp,
                  StackedITTApp,
                  StackedControlUse,
                  StackedITTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with an underscore to separate them after
names(TheModels) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                      paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                      paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                      paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"))

# We start with table with T1 
cmT1 <- c('T1-C' = 'Information-only vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information-only treatment by SES and migration background"

# Now the infamous model summary 
ModelT1CDaycare <- modelsummary(TheModels,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT1,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block  level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall  method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table." 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1:3,part="body")|>
 italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,6),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,5),width=2.2,unit = "cm") %>% 
  hline(i= c(3*c(1:3)), j=2:7, part="body") %>%
  hline(i=12, j=1:7, part="body")




ModelT1CDaycare




#------ MechanismsGenericKnowledgeECTypes ------------

Het.ITT.Educ <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                         Outcome = "KnownNbTypeECS",
                                         Heterogeneity = "Educ2",
                                         ITT = TRUE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

Het.ATT.Educ <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                         Outcome = "KnownNbTypeECS",
                                         Heterogeneity = "Educ2",
                                         ITT = FALSE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

Het.ITT.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                        
                                        Outcome = "KnownNbTypeECS",
                                        Heterogeneity = "MigrationBackground",
                                        ITT = TRUE,
                                        Weights = "WeightPS",
                                        clusters = "StrataWave")

Het.ATT.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                        
                                        Outcome = "KnownNbTypeECS",
                                        Heterogeneity = "MigrationBackground",
                                        ITT = FALSE,
                                        Weights = "WeightPS",
                                        clusters = "StrataWave")



# Create T2 models and apply filtering for "T2-C" term
Het.ITT.EducT2 <- Het.ITT.Educ
Het.ATT.EducT2 <- Het.ATT.Educ
Het.ITT.MigT2 <- Het.ITT.Mig
Het.ATT.MigT2 <- Het.ATT.Mig


# Coefficient map to label "T2-C" as "Information + Support vs Control"
cm <- c('T2-C' = 'Information + Support vs Control')

# First Table: SES & Migration Background
modelsummary(list("Know that early childcare is subsidised_Control mean"  = Het.ITT.EducT2$ModelSummary0,
                  "Know that early childcare is subsidised_ITT"           = Het.ITT.EducT2$ModelSummary,
                  "Know that early childcare is subsidised_ATT"           = Het.ATT.EducT2$ModelSummary,
                  "Know that early childcare is subsidised_Control mean"  = Het.ITT.MigT2$ModelSummary0,
                  "Know that early childcare is subsidised_ITT"           = Het.ITT.MigT2$ModelSummary,
                  "Know that early childcare is subsidised_ATT"           = Het.ATT.MigT2$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value","nobs", "r.squared","adj.r.squared"),
             title="Heterogeneous treatment effects on the number of early childcare types known: SES & Migration background",
             notes=paste("Sources:", SourcesStacked,
                         "\n*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
                          Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
                          Adjusted p-value and confidence intervals account for simultaneous inference.
                          Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3, 9, 12), part = "body")

#------ MechanismsGenericKnowledgeSubsidies ------------

Het.ITT.Educ <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                         Outcome = "AffordSubsidiesEndline1or0",
                                         Heterogeneity = "Educ2",
                                         ITT = TRUE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

Het.ATT.Educ <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                         Outcome = "AffordSubsidiesEndline1or0",
                                         Heterogeneity = "Educ2",
                                         ITT = FALSE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

Het.ITT.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                        
                                        Outcome = "AffordSubsidiesEndline1or0",
                                        Heterogeneity = "MigrationBackground",
                                        ITT = TRUE,
                                        Weights = "WeightPS",
                                        clusters = "StrataWave")

Het.ATT.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                        
                                        Outcome = "AffordSubsidiesEndline1or0",
                                        Heterogeneity = "MigrationBackground",
                                        ITT = FALSE,
                                        Weights = "WeightPS",
                                        clusters = "StrataWave")



# Create T2 models and apply filtering for "T2-C" term
Het.ITT.EducT2 <- Het.ITT.Educ
Het.ATT.EducT2 <- Het.ATT.Educ
Het.ITT.MigT2 <- Het.ITT.Mig
Het.ATT.MigT2 <- Het.ATT.Mig

# Coefficient map to label "T2-C" as "Information + Support vs Control"
cm <- c('T2-C' = 'Information + Support vs Control')

# First Table: SES & Migration Background
modelsummary(list("Number of early childcare types known_Control mean"  = Het.ITT.EducT2$ModelSummary0,
                  "Number of early childcare types known_ITT"           = Het.ITT.EducT2$ModelSummary,
                  "Number of early childcare types known_ATT"           = Het.ATT.EducT2$ModelSummary,
                  "Number of early childcare types known_Control mean"  = Het.ITT.MigT2$ModelSummary0,
                  "Number of early childcare types known_ITT"           = Het.ITT.MigT2$ModelSummary,
                  "Number of early childcare types known_ATT"           = Het.ATT.MigT2$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1,'**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value","nobs", "r.squared","adj.r.squared"),
             title="Heterogeneous treatment effects on the number of early childcare types known: SES & Migration background",
             notes=paste("Sources:", SourcesStacked,
                         "\n*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
                          Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
                          Adjusted p-value and confidence intervals account for simultaneous inference.
                          Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3, 9, 12), part = "body")

#------ MechanismsActivexEverUseDaycare------------

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.
# Update model and variable names from FrenchYNBaseline to ActiveSES and from FrenchEduc to ActiveEduc

# Create interaction variable and estimate the models for ActiveSES
Het.ITT.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                        Outcome = "AppCreche",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                        Outcome = "UseCreche",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Intersectional estimation, adding another variable for heterogeneity

Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy %>%
  separate(Group, into = c("Activity", "SES"))

Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "SES"))

Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "SES"))



# change the name
cm <- c('T2-C'    = 'Information + Support vs Control')


# Summary table of results
modelsummary(list("Daycare application_Control mean"  = Het.ITT.AppCreche.ActiveSES$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.AppCreche.ActiveSES$ModelSummary,
                  "Daycare application_ATT" = Het.LATE.AppCreche.ActiveSES$ModelSummary,
                  "Daycare access_Control mean"  = Het.ITT.UseCreche.ActiveSES$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.UseCreche.ActiveSES$ModelSummary,
                  "Daycare access_ATT" = Het.LATE.UseCreche.ActiveSES$ModelSummary),
             shape = term + Activity + SES ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(# "Mean of DV",
               "Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
               "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by SES and mothers' activity at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the method. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%   # Separate headers
  bold(i = 1,  part = "header") %>%  # Variable labels bold
  merge_at(j = 3, part = "header") %>%
  merge_at(j = 2, part = "header") %>%
  merge_at(j = 1, part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  merge_v(j = 2, part = "body") %>%
  merge_v(j = 3, part = "body") %>%
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%  # center
  align(part = "body", align = "center") %>%  # center
  width(j = c(5,6,8,9), width = 2.4, unit = "cm") %>%
  width(j = c(2,3,4,7), width = 2.2, unit = "cm") %>%
  hline(c(6,12), part = "body") 


#------ MechanismsActivexEverUseEC------------

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.
# Update model and variable names from FrenchYNBaseline to ActiveSES and from FrenchEduc to ActiveEduc

# Create interaction variable and estimate the models for ActiveSES
Het.ITT.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                        Outcome = "ECSApp",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                         Outcome = "ECSApp",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                        Outcome = "ECSUseYes",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(ActiveBaseline, UsedECEC)),
                                                         Outcome = "ECSUseYes",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Intersectional estimation, adding another variable for heterogeneity

Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "EverUse"))

Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "EverUse"))

Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy %>%
  separate(Group, into = c("Activity", "EverUse"))

Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "EverUse"))

Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "EverUse"))

Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "EverUse"))



# change the name
cm <- c('T2-C'    = 'Information + Support vs Control')


# Summary table of results
modelsummary(list("Early childcare application_Control mean"  = Het.ITT.AppCreche.ActiveSES$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.AppCreche.ActiveSES$ModelSummary,
                  "Early childcare application_ATT" = Het.LATE.AppCreche.ActiveSES$ModelSummary,
                  "Early childcare access_Control mean"  = Het.ITT.UseCreche.ActiveSES$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.UseCreche.ActiveSES$ModelSummary,
                  "Early childcare access_ATT" = Het.LATE.UseCreche.ActiveSES$ModelSummary),
             shape = term + Activity + EverUse ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(# "Mean of DV",
               "Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
               "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by SES and mothers' activity at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the method. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%   # Separate headers
  bold(i = 1,  part = "header") %>%  # Variable labels bold
  merge_at(j = 3, part = "header") %>%
  merge_at(j = 2, part = "header") %>%
  merge_at(j = 1, part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  merge_v(j = 2, part = "body") %>%
  merge_v(j = 3, part = "body") %>%
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%  # center
  align(part = "body", align = "center") %>%  # center
  width(j = c(5,6,8,9), width = 2.4, unit = "cm") %>%
  width(j = c(2,3,4,7), width = 2.2, unit = "cm") %>%
  hline(c(6,12), part = "body") 

#------ MechanismsActivexEverUseEC------------

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.
# Update model and variable names from FrenchYNBaseline to ActiveSES and from FrenchEduc to ActiveEduc

# Create interaction variable and estimate the models for ActiveSES
Het.ITT.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(Educ, ActiveBaseline, UsedECEC)),
                                                        Outcome = "ECSApp",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.AppCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(Educ, ActiveBaseline, UsedECEC)),
                                                         Outcome = "ECSApp",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(Educ, ActiveBaseline, UsedECEC)),
                                                        Outcome = "ECSUseYes",
                                                        Heterogeneity = "ActiveEduc",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.UseCreche.ActiveSES <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveEduc = interaction(Educ, ActiveBaseline, UsedECEC)),
                                                         Outcome = "ECSUseYes",
                                                         Heterogeneity = "ActiveEduc",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

# Intersectional estimation, adding another variable for heterogeneity

Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("SES", "Activity", "EverUse"))

Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.AppCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("SES","Activity", "EverUse"))

Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.AppCreche.ActiveSES$ModelSummary$tidy %>%
  separate(Group, into = c("SES","Activity", "EverUse"))

Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("SES","Activity", "EverUse"))

Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy <- Het.ITT.UseCreche.ActiveSES$ModelSummary0$tidy %>% 
  separate(Group, into = c("SES","Activity", "EverUse"))

Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy <- Het.LATE.UseCreche.ActiveSES$ModelSummary$tidy %>% 
  separate(Group, into = c("SES", "Activity", "EverUse"))



# change the name
cm <- c('T2-C'    = 'Information + Support vs Control')


# Summary table of results
modelsummary(list("Early childcare application_Control mean"  = Het.ITT.AppCreche.ActiveSES$ModelSummary0,
                  "Early childcare application_ITT" = Het.ITT.AppCreche.ActiveSES$ModelSummary,
                  "Early childcare application_ATT" = Het.LATE.AppCreche.ActiveSES$ModelSummary,
                  "Early childcare access_Control mean"  = Het.ITT.UseCreche.ActiveSES$ModelSummary0,
                  "Early childcare access_ITT" = Het.ITT.UseCreche.ActiveSES$ModelSummary,
                  "Early childcare access_ATT" = Het.LATE.UseCreche.ActiveSES$ModelSummary),
             shape = term + SES + Activity + EverUse ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(# "Mean of DV",
               "Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
               "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by SES and mothers' activity at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the method. 
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%   # Separate headers
  bold(i = 1,  part = "header") %>%  # Variable labels bold
  merge_at(j = 3, part = "header") %>%
  merge_at(j = 2, part = "header") %>%
  merge_at(j = 1, part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  merge_v(j = 2, part = "body") %>%
  merge_v(j = 3, part = "body") %>%
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%  # center
  align(part = "body", align = "center") %>%  # center
  width(j = c(5,6,8,9), width = 2.4, unit = "cm") %>%
  width(j = c(2,3,4,7), width = 2.2, unit = "cm") %>%
  hline(c(3, 6,9, 12, 15, 18, 21), part = "body") 

#------ ROBUSTNESS------------
##--------------- SES -----------------
#------ RobustnessHTESESGraphsT1EarlyChildcareApp --------

# Nous créons ici les graphiques pour les vérifications de robustesse de l'analyse d'hétérogénéité par SES.
# Nous considérons différentes opérations du SES : 1) l'indice SES basé sur un score composite (éducation et occupation des deux parents), et 2) le ISEI de la mère.

## EARLY CHILDCARE
# Application ITT

Het.ITT.App.IseiMother <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                     mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                            HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "HigherThanMeadianISEIMother",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ITT.App.SES <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                              mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"),
                                                     HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                            Outcome = "ECSApp",
                                            Heterogeneity = "HigherThanMeadianSESIndex",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Application ATT
Het.ATT.App.IseiMother <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                     mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                            HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                   Outcome = "ECSApp",
                                                   Heterogeneity = "HigherThanMeadianISEIMother",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.App.SES <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                              mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                     HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                            Outcome = "ECSApp",
                                            Heterogeneity = "HigherThanMeadianSESIndex",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Access ITT

Het.ITT.Use.IseiMother <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                     mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                            HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "HigherThanMeadianISEIMother",
                                                   ITT = TRUE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ITT.Use.SES <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                              mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                     HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "HigherThanMeadianSESIndex",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Access ATT

Het.ATT.Use.IseiMother <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                     mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                            HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                   Outcome = "ECSUseYes",
                                                   Heterogeneity = "HigherThanMeadianISEIMother",
                                                   ITT = FALSE,
                                                   Weights = "WeightPS",
                                                   clusters = "StrataWave")

Het.ATT.Use.SES <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                              mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                     HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "HigherThanMeadianSESIndex",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

## DAYCARE

# Application ITT

Het.ITT.App.IseiMother.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                             mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                                    HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                           Outcome = "AppCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIMother",
                                                           ITT = TRUE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

Het.ITT.App.SES.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                                      mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                             HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "HigherThanMeadianSESIndex",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Application ATT

Het.ATT.App.IseiMother.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                             mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                                    HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                           Outcome = "AppCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIMother",
                                                           ITT = FALSE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

Het.ATT.App.SES.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                                      mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                             HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "HigherThanMeadianSESIndex",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Access ITT

Het.ITT.Use.IseiMother.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                             mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                                    HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                           Outcome = "UseCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIMother",
                                                           ITT = TRUE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

Het.ITT.Use.SES.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                                      mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                             HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "HigherThanMeadianSESIndex",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Access ATT

Het.ATT.Use.IseiMother.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIMother)) %>%  
                                                             mutate(HigherThanMeadianISEIMother = ifelse(HigherThanMeadianISEIMother == 1, "High", "Low"), 
                                                                    HigherThanMeadianISEIMother=factor(HigherThanMeadianISEIMother)),
                                                           Outcome = "UseCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIMother",
                                                           ITT = FALSE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

Het.ATT.Use.SES.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianSESIndex)) %>%  
                                                      mutate(HigherThanMeadianSESIndex = ifelse(HigherThanMeadianSESIndex == 1, "High", "Low"), 
                                                             HigherThanMeadianSESIndex=factor(HigherThanMeadianSESIndex)),
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "HigherThanMeadianSESIndex",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

# Création des graphiques

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT", "ATT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ITT <- bind_rows(
  Het.ITT.App.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.IseiMother$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)

# Fusionner les données ATT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ATT <- bind_rows(
  Het.ATT.App.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.IseiMother$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "ISEI Mother",  Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels)
)

# Filtrer les données ATT pour enlever la moyenne du groupe de contrôle
DataPlot_ATT <- DataPlot_ATT %>%
  filter(panel != "Control group")

# Combiner les deux DataFrames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T1-C!Apply for early childcare!ISEI Mother" = "ISEI Mother",
  "T1-C!Apply for early childcare!SES Index" ="SES Index")

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High", "Low", "High"))+
  scale_color_brewer("Heterogeneity",palette = "Dark2",limits = c("Low", "High", "Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )



#------ RobustnessHTESESGraphsT1EarlyChildcareAccess --------
# Graphs T1Access

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot <- bind_rows(
  Het.ITT.Use.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.IseiMother$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)%>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T1-C!Access early childcare!ISEI Mother" = "ISEI Mother",
  "T1-C!Access early childcare!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_color_brewer("Heterogeneity",palette = "Dark2",limits = c("Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )

#------ RobustnessHTESESGraphsT1DaycareApp ----------
# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot <- bind_rows(
  Het.ITT.App.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare application", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.IseiMother.Daycare$Tidy %>% mutate(Y = "Daycare application", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare application", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES.Daycare$Tidy %>% mutate(Y = "Daycare application", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T1-C!Daycare application!ISEI Mother" = "ISEI Mother",
  "T1-C!Daycare application!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High")) +
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High")) +
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuster le nombre de colonnes dans la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuster la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )
#------ RobustnessHTESESGraphsT1DaycareAccess --------
# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot <- bind_rows(
  Het.ITT.Use.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.IseiMother.Daycare$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES.Daycare$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T1-C!Access daycare!ISEI Mother" = "ISEI Mother",
  "T1-C!Access daycare!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High")) +
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High")) +
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuster le nombre de colonnes dans la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuster la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )

#------ RobustnessHTESESsTableT1ISEIMother --------

## EARLY CHILDCARE
# Filter only the T1-C term for the ITT and create new models with T1 suffix
Het.ITT.App.IseiMother.DaycareT1 <- Het.ITT.App.IseiMother.Daycare
Het.ITT.App.IseiMotherT1 <- Het.ITT.App.IseiMother
Het.ITT.Use.IseiMotherT1 <- Het.ITT.Use.IseiMother
Het.ITT.Use.IseiMother.DaycareT1 <- Het.ITT.Use.IseiMother.Daycare

# Apply filtering for T1-C term
Het.ITT.App.IseiMother.DaycareT1$ModelSummary0$tidy <- Het.ITT.App.IseiMother.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.App.IseiMotherT1$ModelSummary0$tidy <- Het.ITT.App.IseiMother$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.App.IseiMotherT1$ModelSummary$tidy <- Het.ITT.App.IseiMother$ModelSummary$tidy %>% filter(term == "T1-C")
Het.ITT.Use.IseiMotherT1$ModelSummary0$tidy <- Het.ITT.Use.IseiMother$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.IseiMotherT1$ModelSummary$tidy <- Het.ITT.Use.IseiMother$ModelSummary$tidy %>% filter(term == "T1-C")
Het.ITT.Use.IseiMother.DaycareT1$ModelSummary0$tidy <- Het.ITT.Use.IseiMother.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

# Coef Map for clear labels
cm <- c('T1-C' = 'Information-only vs Control')

# Creating the table for IseiMother (T1)
modelsummary(list("Application_Early childcare_Control mean"  = Het.ITT.App.IseiMotherT1$ModelSummary0,
                  "Application_Early childcare_ITT"           = Het.ITT.App.IseiMotherT1$ModelSummary,
                  "Application_Daycare_Control mean"          = Het.ITT.App.IseiMother.DaycareT1$ModelSummary0,
                  "Application_Daycare_ITT"                   = Het.ITT.App.IseiMother.DaycareT1$ModelSummary,
                  "Access_Early childcare_Control mean"       = Het.ITT.Use.IseiMother.DaycareT1$ModelSummary0,
                  "Access_Early childcare_ITT"                = Het.ITT.Use.IseiMother.DaycareT1$ModelSummary,
                  "Access_Daycare_Control mean"               = Het.ITT.Use.IseiMother.DaycareT1$ModelSummary0,
                  "Access_Daycare_ITT"                        = Het.ITT.Use.IseiMother.DaycareT1$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by ISEI Mother",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(i = c(2), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.2, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 1.9, unit = "cm") %>%
  hline(c(6, 3), part = "body") 

#------ RobustnessHTESESsTableT1CompositeSES --------

# Filter only the T1-C term for the ITT and create new models with T1 suffix
Het.ITT.App.SEST1 <- Het.ITT.App.SES
Het.ITT.App.SES.DaycareT1 <- Het.ITT.App.SES.Daycare
Het.ITT.Use.SEST1 <- Het.ITT.Use.SES
Het.ITT.Use.SES.DaycareT1 <- Het.ITT.Use.SES.Daycare

# Apply filtering for T1-C term
Het.ITT.App.SES.DaycareT1$ModelSummary0$tidy <- Het.ITT.App.SES.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.App.SEST1$ModelSummary0$tidy <- Het.ITT.App.SES$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.App.SEST1$ModelSummary$tidy <- Het.ITT.App.SES$ModelSummary$tidy %>% filter(term == "T1-C")
Het.ITT.Use.SEST1$ModelSummary0$tidy <- Het.ITT.Use.SES$ModelSummary0$tidy %>% filter(term == "T1-C")
Het.ITT.Use.SEST1$ModelSummary$tidy <- Het.ITT.Use.SES$ModelSummary$tidy %>% filter(term == "T1-C")
Het.ITT.Use.SES.DaycareT1$ModelSummary0$tidy <- Het.ITT.Use.SES.Daycare$ModelSummary0$tidy %>% filter(term == "T1-C")

# Coef Map for clear labels
cm <- c('T1-C' = 'Information-only vs Control')

# Creating the table for SES (T1)
modelsummary(list("Application_Early childcare_Control mean"  = Het.ITT.App.SEST1$ModelSummary0,
                  "Application_Early childcare_ITT"           = Het.ITT.App.SEST1$ModelSummary,
                  "Application_Daycare_Control mean"          = Het.ITT.App.SES.DaycareT1$ModelSummary0,
                  "Application_Daycare_ITT"                   = Het.ITT.App.SES.DaycareT1$ModelSummary,
                  "Access_Early childcare_Control mean"       = Het.ITT.Use.SEST1$ModelSummary0,
                  "Access_Early childcare_ITT"                = Het.ITT.Use.SEST1$ModelSummary,
                  "Access_Daycare_Control mean"               = Het.ITT.Use.SES.DaycareT1$ModelSummary0,
                  "Access_Daycare_ITT"                        = Het.ITT.Use.SES.DaycareT1$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by SES Index",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(i = c(2), part = "header") %>%
    italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.2, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 1.9, unit = "cm") %>%
  hline(c(6, 3), part = "body")


#------ RobustnessHTESESGraphsT2EarlychildcareApp --------

### early childcare

##### Applications

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T2-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT", "ATT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ITT <- bind_rows(
  Het.ITT.App.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.IseiMother$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)

# Fusionner les données ATT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ATT <- bind_rows(
  Het.ATT.App.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.IseiMother$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "ISEI Mother",  Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels)
)

# Filtrer les données ATT pour enlever la moyenne du groupe de contrôle
DataPlot_ATT <- DataPlot_ATT %>%
  filter(panel != "Control group")

# Combiner les deux DataFrames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T2-C!Apply for early childcare!ISEI Mother" = "ISEI Mother",
  "T2-C!Apply for early childcare!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_color_brewer("Heterogeneity",palette = "Dark2",limits = c("Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "Heterogeneous ITT and ATT of the information + personalised administrative support treatment \n on early childcare application",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )


#------ RobustnessHTESESGraphsT2EarlychildcareAccess --------

##### Use

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T2-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT", "ATT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ITT <- bind_rows(
  Het.ITT.App.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Early childcare access", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.IseiMother$Tidy %>% mutate(Y = "Early childcare access", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES$ModelSummary0$tidy %>% mutate(Y = "Early childcare access", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES$Tidy %>% mutate(Y = "Early childcare access", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)

# Fusionner les données ATT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.IseiMother$ModelSummary0$tidy %>% mutate(Y = "Early childcare access", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.IseiMother$Tidy %>% mutate(Y = "Early childcare access", panel = "ATT", Heterogeneity = "ISEI Mother",  Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.SES$ModelSummary0$tidy %>% mutate(Y = "Early childcare access", panel = "Control group", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.SES$Tidy %>% mutate(Y = "Early childcare access", panel = "ATT", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels)
)

# Filtrer les données ATT pour enlever la moyenne du groupe de contrôle
DataPlot_ATT <- DataPlot_ATT %>%
  filter(panel != "Control group")

# Combiner les deux DataFrames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T2-C!Early childcare access!ISEI Mother" = "ISEI Mother",
  "T2-C!Early childcare access!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_color_brewer("Heterogeneity",palette = "Dark2",limits = c("Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "Heterogeneous ITT and ATT of the information + personalised administrative support treatment \n on early childcare access",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )

#------ RobustnessHTESESTableT2ECISEIMotherEC --------
# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.IseiMotherT2 <- Het.ITT.App.IseiMother
Het.ATT.App.IseiMotherT2 <- Het.ATT.App.IseiMother
Het.ITT.Use.IseiMotherT2 <- Het.ITT.Use.IseiMother
Het.ATT.Use.IseiMotherT2 <- Het.ATT.Use.IseiMother

# Apply filtering for T2-C term (both ModelSummary and ModelSummary0)
Het.ITT.App.IseiMotherT2$ModelSummary$tidy <- Het.ITT.App.IseiMother$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.App.IseiMotherT2$ModelSummary0$tidy <- Het.ITT.App.IseiMother$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.IseiMotherT2$ModelSummary$tidy <- Het.ATT.App.IseiMother$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.IseiMotherT2$ModelSummary$tidy <- Het.ITT.Use.IseiMother$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.IseiMotherT2$ModelSummary0$tidy <- Het.ITT.Use.IseiMother$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.IseiMotherT2$ModelSummary$tidy <- Het.ATT.Use.IseiMother$ModelSummary$tidy %>% filter(term == "T2-C")

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Early Childcare (ISEI Mother)
modelsummary(list("Application_Control mean"  = Het.ITT.App.IseiMotherT2$ModelSummary0,
                  "Application_ITT"           = Het.ITT.App.IseiMotherT2$ModelSummary,
                  "Application_ATT"           = Het.ATT.App.IseiMotherT2$ModelSummary,
                  "Access_Control mean"       = Het.ITT.Use.IseiMotherT2$ModelSummary0,
                  "Access_ITT"                = Het.ITT.Use.IseiMotherT2$ModelSummary,
                  "Access_ATT"                = Het.ATT.Use.IseiMotherT2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on early childcare application and access by ISEI Mother",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body") 
#------ RobustnessHTESESTableT2ECSES --------
# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.SEST2 <- Het.ITT.App.SES
Het.ATT.App.SEST2 <- Het.ATT.App.SES
Het.ITT.Use.SEST2 <- Het.ITT.Use.SES
Het.ATT.Use.SEST2 <- Het.ATT.Use.SES

# Apply filtering for T2-C term (both ModelSummary and ModelSummary0)
Het.ITT.App.SEST2$ModelSummary$tidy <- Het.ITT.App.SES$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.App.SEST2$ModelSummary0$tidy <- Het.ITT.App.SES$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.SEST2$ModelSummary$tidy <- Het.ATT.App.SES$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.SEST2$ModelSummary$tidy <- Het.ITT.Use.SES$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.SEST2$ModelSummary0$tidy <- Het.ITT.Use.SES$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.SEST2$ModelSummary$tidy <- Het.ATT.Use.SES$ModelSummary$tidy %>% filter(term == "T2-C")

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Early Childcare (SES Index)
modelsummary(list("Application_Control mean"  = Het.ITT.App.SEST2$ModelSummary0,
                  "Application_ITT"           = Het.ITT.App.SEST2$ModelSummary,
                  "Application_ATT"           = Het.ATT.App.SEST2$ModelSummary,
                  "Access_Control mean"       = Het.ITT.Use.SEST2$ModelSummary0,
                  "Access_ITT"                = Het.ITT.Use.SEST2$ModelSummary,
                  "Access_ATT"                = Het.ATT.Use.SEST2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on early childcare application and access by SES Index",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body")

#------ RobustnessHTESESGraphsT2DaycareApp --------

##### Applications

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T2-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT", "ATT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ITT <- bind_rows(
  Het.ITT.App.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.IseiMother.Daycare$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.SES.Daycare$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)

# Fusionner les données ATT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ATT <- bind_rows(
  Het.ATT.App.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.IseiMother.Daycare$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "ISEI Mother",  Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.SES.Daycare$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels)
)

# Filtrer les données ATT pour enlever la moyenne du groupe de contrôle
DataPlot_ATT <- DataPlot_ATT %>%
  filter(panel != "Control group")

# Combiner les deux DataFrames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T2-C!Apply for daycare!ISEI Mother" = "ISEI Mother",
  "T2-C!Apply for daycare!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "Heterogeneous ITT and ATT of the information + personalised administrative support treatment \n on daycare application",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )

#------ RobustnessHTESESGraphsT2DaycareAccess --------

# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T2-C")
heterogeneity_levels <- c("ISEI Mother", "SES Index")
panel_levels <- c("Control group", "ITT", "ATT")

# Fusionner les données ITT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare access", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.IseiMother.Daycare$Tidy %>% mutate(Y = "Daycare access", panel = "ITT", Heterogeneity = "ISEI Mother", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare access", panel = "Control group", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.SES.Daycare$Tidy %>% mutate(Y = "Daycare access", panel = "ITT", Heterogeneity = "SES Index", Type = "ITT") %>% filter(term %in% term_levels)
)

# Fusionner les données ATT en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.IseiMother.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare access", panel = "Control group", Heterogeneity = "ISEI Mother", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.IseiMother.Daycare$Tidy %>% mutate(Y = "Daycare access", panel = "ATT", Heterogeneity = "ISEI Mother",  Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.SES.Daycare$ModelSummary0$tidy %>% mutate(Y = "Daycare access", panel = "Control group", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.SES.Daycare$Tidy %>% mutate(Y = "Daycare access", panel = "ATT", Heterogeneity = "SES Index", Type = "ATT") %>% filter(term %in% term_levels)
)

# Filtrer les données ATT pour enlever la moyenne du groupe de contrôle
DataPlot_ATT <- DataPlot_ATT %>%
  filter(panel != "Control group")

# Combiner les deux DataFrames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Créer un vecteur nommé pour les étiquettes de l'axe x
x_labels <- c(
  "T2-C!Daycare access!ISEI Mother" = "ISEI Mother",
  "T2-C!Daycare access!SES Index" = "SES Index"
)

# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y")  +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_color_brewer("Heterogeneity", palette = "Dark2", limits = c("Low", "High"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Ligne pointillée pour la date de randomisation
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "Heterogeneous ITT and ATT of the information + personalised administrative support treatment \n on daycare access",
    subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT/ATT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )

#------ RobustnessHTESESsTableT2DaycareISEIMother --------

# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.IseiMother.DaycareT2 <- Het.ITT.App.IseiMother.Daycare
Het.ATT.App.IseiMother.DaycareT2 <- Het.ATT.App.IseiMother.Daycare
Het.ITT.Use.IseiMother.DaycareT2 <- Het.ITT.Use.IseiMother.Daycare
Het.ATT.Use.IseiMother.DaycareT2 <- Het.ATT.Use.IseiMother.Daycare

# Apply filtering for T2-C term (both ModelSummary and ModelSummary0)
Het.ITT.App.IseiMother.DaycareT2$ModelSummary$tidy <- Het.ITT.App.IseiMother.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.App.IseiMother.DaycareT2$ModelSummary0$tidy <- Het.ITT.App.IseiMother.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.IseiMother.DaycareT2$ModelSummary$tidy <- Het.ATT.App.IseiMother.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.IseiMother.DaycareT2$ModelSummary$tidy <- Het.ITT.Use.IseiMother.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.IseiMother.DaycareT2$ModelSummary0$tidy <- Het.ITT.Use.IseiMother.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.IseiMother.DaycareT2$ModelSummary$tidy <- Het.ATT.Use.IseiMother.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Daycare (ISEI Mother)
modelsummary(list("Application_Control mean"  = Het.ITT.App.IseiMother.DaycareT2$ModelSummary0,
                  "Application_ITT"           = Het.ITT.App.IseiMother.DaycareT2$ModelSummary,
                  "Application_ATT"           = Het.ATT.App.IseiMother.DaycareT2$ModelSummary,
                  "Access_Control mean"       = Het.ITT.Use.IseiMother.DaycareT2$ModelSummary0,
                  "Access_ITT"                = Het.ITT.Use.IseiMother.DaycareT2$ModelSummary,
                  "Access_ATT"                = Het.ATT.Use.IseiMother.DaycareT2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by ISEI Mother",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body") 

#------ RobustnessHTESESsTableT2DaycareCompositeSES --------
# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.SES.DaycareT2 <- Het.ITT.App.SES.Daycare
Het.ATT.App.SES.DaycareT2 <- Het.ATT.App.SES.Daycare
Het.ITT.Use.SES.DaycareT2 <- Het.ITT.Use.SES.Daycare
Het.ATT.Use.SES.DaycareT2 <- Het.ATT.Use.SES.Daycare

# Apply filtering for T2-C term (both ModelSummary and ModelSummary0)
Het.ITT.App.SES.DaycareT2$ModelSummary$tidy <- Het.ITT.App.SES.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.App.SES.DaycareT2$ModelSummary0$tidy <- Het.ITT.App.SES.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.SES.DaycareT2$ModelSummary$tidy <- Het.ATT.App.SES.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.SES.DaycareT2$ModelSummary$tidy <- Het.ITT.Use.SES.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.SES.DaycareT2$ModelSummary0$tidy <- Het.ITT.Use.SES.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.SES.DaycareT2$ModelSummary$tidy <- Het.ATT.Use.SES.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Daycare (SES Index)
modelsummary(list("Application_Daycare_Control mean"  = Het.ITT.App.SES.DaycareT2$ModelSummary0,
                  "Application_Daycare_ITT"           = Het.ITT.App.SES.DaycareT2$ModelSummary,
                  "Application_Daycare_ATT"           = Het.ATT.App.SES.DaycareT2$ModelSummary,
                  "Access_Daycare_Control mean"       = Het.ITT.Use.SES.DaycareT2$ModelSummary0,
                  "Access_Daycare_ITT"                = Het.ITT.Use.SES.DaycareT2$ModelSummary,
                  "Access_Daycare_ATT"                = Het.ATT.Use.SES.DaycareT2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by SES Index",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body")

##-------------- Migration background --------------
#--------------- MigTablesEarlyChilcareInfoOnly -------------------------------

# First estimate the ITT for applications - Migration Background dimensions
Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSApp",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now estimate the ITT for access - Migration Background dimensions
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack control for application - Migration Background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.App.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - Migration Background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - Migration Background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.Use.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - Migration Background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedControlUse,
                       StackedITTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"))
# Now T2 against C
cmT2C <- c('T1-C'    = 'Information-only vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information-only treatment on early childcare application and access - Migration Background robustness checks"

# Now the infamous model summary 
ModelT1C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>%
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>
  width(j=c(1,2, 3),width=2.2,unit = "cm") %>% 
   hline(i = 3, j = 1:7, part="body") %>%
   hline(i = 6, j = 1:7, part="body") %>%
   hline(i = 9, j = 1:7, part="body") %>%
  hline(i = 12, j = 1:7, part="body") %>%
  hline(i = 15, j = 1:7, part="body") %>%
  hline(i = 18, j = 1:7, part="body")

ModelT1C

#--------------- MigTablesDaycareInfoOnly -------------------------------

# First estimate the ITT for daycare applications - Migration Background dimensions
Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "AppCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "AppCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "AppCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now estimate the ITT for daycare access - Migration Background dimensions
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "UseCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "UseCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack control for daycare application - Migration Background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.App.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for daycare application - Migration Background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for daycare use - Migration Background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.Use.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for daycare use - Migration Background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackground"),
                   Het.ITT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedControlUse,
                       StackedITTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"))
# Now T1 against C
cmT2C <- c('T1-C'    = 'Information-only vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information-only treatment on daycare application and access - Migration Background robustness checks"

# Now the infamous model summary 
ModelT1C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>%
  fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>
  width(j=c(1,2, 3),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:7, part="body") %>%
  hline(i = 6, j = 1:7, part="body") %>%
  hline(i = 9, j = 1:7, part="body") %>%
  hline(i = 12, j = 1:7, part="body") %>%
  hline(i = 15, j = 1:7, part="body") %>%
  hline(i = 18, j = 1:7, part="body")

ModelT1C
#--------------- MigTablesEarlyChilcare -------------------------------
# First estimate the ITT for applications - Migration Background dimensions
Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSApp",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSApp",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now estimate the ITT for access - Migration Background dimensions
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack control for application - Migration Background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.App.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for application - Migration Background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for use - Migration Background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.Use.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for use - Migration Background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Step 2 : estimate the conditional ATTs of interest using the function
# ATT for applications - Migration Background
Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSApp",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now ATT for use - Migration Background
Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack ATT for application - Migration Background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ATT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ATT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ATT.App.Mig$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for use - Migration Background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ATT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ATT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ATT.Use.Mig$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information + support treatment on early childcare application and access - Migration Background robustness checks"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")

ModelT2C

#--------------- MigTablesDaycare -------------------------------
# First estimate the ITT for daycare applications - Migration Background dimensions
Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "AppCreche",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "AppCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "AppCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now estimate the ITT for daycare access - Migration Background dimensions
Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "UseCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ITT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDB %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "UseCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack control for daycare application - Migration Background
StackedControlApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.App.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary0$glance
)
class(StackedControlApp) <- "modelsummary_list"   # define the class

# Stack ITT for daycare application - Migration Background
StackedITTApp <- list(
  tidy = bind_rows(Het.ITT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.App.Mig$ModelSummary$glance
)
class(StackedITTApp) <- "modelsummary_list"   # define the class

# Stack control for daycare use - Migration Background
StackedControlUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.Use.Mig12$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary0$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary0$glance
)
class(StackedControlUse) <- "modelsummary_list"   # define the class

# Stack ITT for daycare use - Migration Background
StackedITTUse <- list(
  tidy = bind_rows(Het.ITT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ITT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ITT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ITT.Use.Mig$ModelSummary$glance
)
class(StackedITTUse) <- "modelsummary_list"   # define the class

# Step 2 : estimate the conditional ATTs of interest using the function
# ATT for daycare applications - Migration Background
Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "AppCreche",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.App.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "AppCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.App.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "AppCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Now ATT for daycare use - Migration Background
Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackgroundBoth",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.Use.Mig12 <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "UseCreche",
                                              Heterogeneity= "MigrationBackgroundOneOfTheTwo",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

Het.ATT.Use.Mig2 <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% filter(!is.na(MigrationBackgroundParent2)),
                                             Outcome = "UseCreche",
                                             Heterogeneity = "MigrationBackgroundParent2",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")

# Stack ATT for daycare application - Migration Background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ATT.App.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ATT.App.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ATT.App.Mig$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for daycare use - Migration Background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundBoth"),
                   Het.ATT.Use.Mig12$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundOneOfTheTwo"),
                   Het.ATT.Use.Mig2$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="MigrationBackgroundParent2")),
  glance = Het.ATT.Use.Mig$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <-   list(StackedControlApp,
                       StackedITTApp,
                       StackedATTApp,
                       StackedControlUse,
                       StackedITTUse,
                       StackedATTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with it with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C'    = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects of the information + support treatment on daycare application and access - Migration Background robustness checks"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2),
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = c("conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars = c('*' = .1,'**' = .05, '***' = .01),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"N"),
                         title=TheTitle,
                         notes=paste("Sources:", SourcesStacked,
                                     "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Adjusted standard errors robust to cluster-heteroskedasticity at the block level.
Adjusted p-values and confidence intervals account for simultaneous inference using the Westfall method.
Joint significance test of null effect using Chi-2 test and p-values are reported at the bottom of the table.
                         " 
                         ),output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=1,part="header")|>
  merge_at(j=2,part="header")|>
  merge_v(j=1:3,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(1,2, 3, 6),width=2.2,unit = "cm") %>% 
  hline(i = 3, j = 1:9, part="body") %>%
  hline(i = 6, j = 1:9, part="body") %>%
  hline(i = 9, j = 1:9, part="body") %>%
  hline(i = 12, j = 1:9, part="body") %>%
  hline(i = 15, j = 1:9, part="body") %>%
  hline(i = 18, j = 1:9, part="body")

ModelT2C

#------RobustnessPostLassoGraphs -------


# First, run the regression without Lasso
ITT.UseCreche <- ITTSimultaneous(Y="UseCreche")
ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes")
ITT.ECSApp <- ITTSimultaneous(Y="ECSApp")
ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche")


## Bind them together 
MainITTResults <- bind_rows(ITT.ECSApp$Tidy %>% mutate(Outcome="Early childcare application",Y=Outcome),
                            ITT.ECSUseYes$Tidy %>% mutate(Outcome="Early childcare access",Y=Outcome),
                            ITT.UseCreche$Tidy %>% mutate(Outcome="Daycare access",Y=Outcome),
                            ITT.ECSAppCreche$Tidy   %>% mutate(Outcome="Daycare application",Y=Outcome),
                            #  ITT.ECSAppAssmat$Tidy   %>% mutate(Outcome="Apply for childminder")
) %>% rename("SubSample"="Var") %>% mutate(Model="ITT")


# Then Post Lasso on each outcome

# Run lasso on access to early childcare in general
ECSUseYesT1C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T1-C")
ECSUseYesT2C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-C")
ECSUseYesT2T1 <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSUseYesLasso <- bind_rows(ECSUseYesT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                ECSUseYesT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                ECSUseYesT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Early childcare access")

# Run lasso on application for early childcare in general
ECSAppT1C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T1-C")
ECSAppT2C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-C")
ECSAppT2T1 <- EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSAppLasso <- bind_rows(   ECSAppT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                ECSAppT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                ECSAppT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Early childcare application")



# Run lasso on access to daycare
USeCrecheT1C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T1-C")
USeCrecheT2C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-C")
USeCrecheT2T1 <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-T1")

#EstPostLasso()
## Stack the results
TidyUseCrecheLasso <- bind_rows(USeCrecheT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                USeCrecheT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                USeCrecheT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Daycare access")

# Run lasso on appliation for daycare
ECSAppCrecheT1C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T1-C")
ECSAppCrecheT2C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-C")
ECSAppCrecheT2T1 <- EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-T1")

#EstPostLasso()

TidyECSAppCrechepLasso <- bind_rows(   ECSAppCrecheT1C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
                                       ECSAppCrecheT2C$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
                                       ECSAppCrecheT2T1$`ITT Post lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
  mutate(Y="Daycare application")

# on récupère tous les résultats dans un dataframe et on garde que le coefficient des itt donc on filtre sur Z

ITT.PostLasso <- bind_rows(TidyUseCrecheLasso,
                           TidyECSAppCrechepLasso,
                           TidyECSAppLasso,
                           TidyECSUseYesLasso
) %>% filter(term=="Z")


# ITT.UseCreche <- ITTSimultaneous(Y="UseCreche")
# ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes")
# ITT.ECSApp <- ITTSimultaneous(Y="ECSApp")
# ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche")



# put all the result in the same dataframe
DbPlotITT <- ITT.PostLasso %>% mutate(Model="Post Lasso") %>% rename(point.conf.low=conf.low,point.conf.high=conf.high) %>% # 
  bind_rows(.,MainITTResults %>% mutate(Model="Fixed effects")
  ) %>% mutate(Y=fct_rev(Y)) # for ordering the labels in the plot


ggplot(DbPlotITT)+geom_pointrange(aes(x=interaction(SubSample,Y,sep="!"),
                                      y=estimate,ymin=point.conf.low,ymax=point.conf.high,color=SubSample,shape=Model),
                                  position = position_dodge(1))+ geom_crossbar(aes(y=estimate,x=interaction(SubSample,Y,sep="!"),fill=SubSample,ymin=conf.low,color=SubSample,
                                                                                   ymax=conf.high),position = position_dodge(1),alpha=.2,fatten = 2,width=.4)+
  scale_x_discrete( name = "Comparison")+coord_flip()+
  scale_color_viridis_d("Comparison",alpha=.8,option="A",end=.6)+
  scale_fill_viridis_d("Comparison",alpha=.8,option="A",end=.6)+
  scale_shape_manual("Model:",values = c(4:8))+
  guides(col = guide_legend(nrow = 4))+theme(legend.position = "right")+
  geom_hline(aes(yintercept = 0),linetype=c(2))+#Dashed line 
  ylab("Estimates")+
  labs(title="Intention to treat, main effects and robustness",
       #subtitle="Stacked OLS estimations",
       caption = paste("Sources:", SourcesStacked,
                       "
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Point indicates the ITT and the error bars indicate pointwise 95% CI.
Adjusted p-value and confidence intervals account for simultaneous inference using the ",ITT.ECSAppCreche$Correction, "method. 
The Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.
The Post-lasso use demeaned covairates selected in a first step by cross validation and
and estimated with OLS with these variables and interactions with the treatment and inverse probability weighting.
"))



#------ RobustnessHTESESsTableT1ISEIFather --------
# In this part, we look at what happens to the results if we use the father's ISEI instead of the mother's ISEI, 
# that is occupational status of the father instead of the mother. Because the mother is usually the one who takes care of the child,
# we expect that the father's ISEI will have a smaller effect on the outcome. This is what we find.

Het.ITT.App.IseiFather<- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),
                                                                                                                         HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "HigherThanMeadianISEIFather",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")




Het.ATT.App.IseiFather<- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                  Outcome = "ECSApp",
                                                  Heterogeneity = "HigherThanMeadianISEIFather",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")
#  itt              



Het.ITT.Use.IseiFather<- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"), HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "HigherThanMeadianISEIFather",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")





Het.ATT.Use.IseiFather<- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                  Outcome = "ECSUseYes",
                                                  Heterogeneity = "HigherThanMeadianISEIFather",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")



## DAYCARE

# Application ITT              


Het.ITT.App.IseiFather.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                           Outcome = "AppCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIFather",
                                                           ITT = TRUE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

Het.ATT.App.IseiFather.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                           Outcome = "AppCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIFather",
                                                           ITT = FALSE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")


Het.ITT.Use.IseiFather.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                           Outcome = "UseCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIFather",
                                                           ITT = TRUE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")

# ATT

Het.ATT.Use.IseiFather.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% filter(!is.na(HigherThanMeadianISEIFather)) %>%  mutate(HigherThanMeadianISEIFather = ifelse(HigherThanMeadianISEIFather == 1, "High", "Low"),HigherThanMeadianISEIFather=factor(HigherThanMeadianISEIFather)),
                                                           Outcome = "UseCreche",
                                                           Heterogeneity = "HigherThanMeadianISEIFather",
                                                           ITT = FALSE,
                                                           Weights = "WeightPS",
                                                           clusters = "StrataWave")


# Filter only the T1-C term for the ITT and create new models with T1 suffix
Het.ITT.App.IseiFather.DaycareT1 <- Het.ITT.App.IseiFather.Daycare
Het.ITT.App.IseiFatherT1 <- Het.ITT.App.IseiFather
Het.ITT.Use.IseiFatherT1 <- Het.ITT.Use.IseiFather
Het.ITT.Use.IseiFather.DaycareT1 <- Het.ITT.Use.IseiFather.Daycare

# Coef Map for clear labels
cm <- c('T1-C' = 'Information-only vs Control')

# Creating the table for IseiFather (T1)
modelsummary(list("Application_Early childcare_Control mean"  = Het.ITT.App.IseiFatherT1$ModelSummary0,
                  "Application_Early childcare_ITT"           = Het.ITT.App.IseiFatherT1$ModelSummary,
                  "Application_Daycare_Control mean"          = Het.ITT.App.IseiFather.DaycareT1$ModelSummary0,
                  "Application_Daycare_ITT"                   = Het.ITT.App.IseiFather.DaycareT1$ModelSummary,
                  "Access_Early childcare_Control mean"       = Het.ITT.Use.IseiFatherT1$ModelSummary0,
                  "Access_Early childcare_ITT"                = Het.ITT.Use.IseiFatherT1$ModelSummary,
                  "Access_Daycare_Control mean"               = Het.ITT.Use.IseiFather.DaycareT1$ModelSummary0,
                  "Access_Daycare_ITT"                        = Het.ITT.Use.IseiFather.DaycareT1$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by ISEI Father",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(i = c(2), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body")

#------ RobustnessHTESESTableT2ECISEIFatherEC --------
# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.IseiFatherT2 <- Het.ITT.App.IseiFather
Het.ATT.App.IseiFatherT2 <- Het.ATT.App.IseiFather
Het.ITT.Use.IseiFatherT2 <- Het.ITT.Use.IseiFather
Het.ATT.Use.IseiFatherT2 <- Het.ATT.Use.IseiFather

# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Early Childcare (ISEI Father)
modelsummary(list("Application_Control mean"  = Het.ITT.App.IseiFatherT2$ModelSummary0,
                  "Application_ITT"           = Het.ITT.App.IseiFatherT2$ModelSummary,
                  "Application_ATT"           = Het.ATT.App.IseiFatherT2$ModelSummary,
                  "Access_Control mean"       = Het.ITT.Use.IseiFatherT2$ModelSummary0,
                  "Access_ITT"                = Het.ITT.Use.IseiFatherT2$ModelSummary,
                  "Access_ATT"                = Het.ATT.Use.IseiFatherT2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on early childcare application and access by ISEI of the Father",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body") 



#------ RobustnessHTESESsTableT2DaycareISEIFather --------
# Filter only the T2-C term for the ITT and ATT, create new models with T2 suffix
Het.ITT.App.IseiFather.DaycareT2 <- Het.ITT.App.IseiFather.Daycare
Het.ATT.App.IseiFather.DaycareT2 <- Het.ATT.App.IseiFather.Daycare
Het.ITT.Use.IseiFather.DaycareT2 <- Het.ITT.Use.IseiFather.Daycare
Het.ATT.Use.IseiFather.DaycareT2 <- Het.ATT.Use.IseiFather.Daycare


# Coef Map for clear labels
cm <- c('T2-C' = 'Information + Support vs Control')

# Creating the table for Daycare (ISEI Father)
modelsummary(list("Application_Daycare_Control mean"  = Het.ITT.App.IseiFather.DaycareT2$ModelSummary0,
                  "Application_Daycare_ITT"           = Het.ITT.App.IseiFather.DaycareT2$ModelSummary,
                  "Application_Daycare_ATT"           = Het.ATT.App.IseiFather.DaycareT2$ModelSummary,
                  "Access_Daycare_Control mean"       = Het.ITT.Use.IseiFather.DaycareT2$ModelSummary0,
                  "Access_Daycare_ITT"                = Het.ITT.Use.IseiFather.DaycareT2$ModelSummary,
                  "Access_Daycare_ATT"                = Het.ATT.Use.IseiFather.DaycareT2$ModelSummary),
             shape = term + Group ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c("Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value", "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on daycare application and access by ISEI Father",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>   
  bold(i = 1, part = "header") %>%
  merge_at(j = 2, part = "header") |>
  merge_at(j = 1, part = "header") |>
  merge_v(j = 1, part = "body") |>
  merge_v(j = 2, part = "body") |>
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") |>
  align(part = "body", align = "center") |>
  width(j = c(4, 5, 7, 8), width = 2.7, unit = "cm") |>
  width(j = c(1, 2, 3, 6), width = 2, unit = "cm") %>%
  hline(c(6, 3), part = "body") 




#------- RobustnessClassicalOLS----------------------
# Function to rename coefficients in a fixest model
rename_coefficients <- function(model, new_names) {
  names(model$coefficients) <- new_names
  return(model)
}
#(i) Outcome variable : ECSApp : 
OLS_ECSApp <- feols(ECSApp ~ T1 + T2 | StrataWave, data = reg_MainDB, cluster = ~StrataWave)

#(ii) Outcome variable : ECSUseYes : 
OLS_ECSUseYes <- feols(ECSUseYes ~ T1 + T2 | StrataWave, data = reg_MainDB, cluster = ~StrataWave)

#(iii) Outcome variable : AppCreche : 
OLS_AppCreche <- feols(AppCreche ~ T1 + T2 | StrataWave, data = reg_MainDB, cluster = ~StrataWave)

#(iv) Outcome variable : UseCreche 
OLS_UseCreche <- OLS_UseCreche <- feols(UseCreche ~ T1 + T2 | StrataWave, data = reg_MainDB, cluster = ~StrataWave)


ols_models <- list(
  "Early childcare_Application" = OLS_ECSApp,
  "Early childcare_Access" = OLS_ECSUseYes,
  "Daycare_Application" = OLS_AppCreche,
  "Daycare_Access" = OLS_UseCreche
)


# Current names of the variables
current_names <- c("T1", "T2")

# New desired names
new_names <- c("Information-only treatment", "Information + support treatment")

# Rename the coefficients in each model
ols_models_renamed <- lapply(ols_models, function(model) {
  rename_coefficients(model, new_names)
})


# Create the table with the renamed models
ols_table <- modelsummary(ols_models_renamed,
                          fmt = fmt_statistic(estimate = 2, std.error = 2, conf.int = 2),
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", 'FE: Strata', 'Mean of DV'),
                          #add_rows = mean_df,
                          title = "Main outcomes with classical OLS",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          output = 'flextable') %>%
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%
  align(part = "body", align = "center") %>%
  width(j = c(2, 3, 4), width = 2.7, unit = "cm") %>%
  width(j = c(1), width = 2.4, unit = "cm")%>% 
  hline(c(2,4),part="body")




ols_table

#------ RobustnessOLSHTE --------

# Define lists to store the models for OLS
models_ols_ECSApp <- list()
models_ols_ECSUseYes <- list()
models_ols_AppCreche <- list()
models_ols_UseCreche <- list()

# List of dimensions of heterogeneity
dimensions <- c("High_SES", "NoMigrationBackground", "HighCoverageBaseline", "High_knowledge", "PresentOrientated", "HighCoverage_total", "UsedECEC", "HigherThanMeadianISEIMother", "HigherThanMeadianSESIndex")

# Function to rename coefficients in a fixest model
rename_coefficients <- function(model) {
  coef_names <- names(model$coefficients)
  coef_names <- gsub("T1", "Information-only treatment", coef_names)
  coef_names <- gsub("T2", "Information + support", coef_names)
  names(model$coefficients) <- coef_names
  return(model)
}

# (i) Outcome variable ECSApp
for (dim in dimensions) {
  formula <- as.formula(paste("ECSApp ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feols(formula, data = reg_MainDB, cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_ols_ECSApp[[dim]] <- model
}

# (ii) Outcome variable ECSUseYes
for (dim in dimensions) {
  formula <- as.formula(paste("ECSUseYes ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feols(formula, data = reg_MainDB, cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_ols_ECSUseYes[[dim]] <- model
}

# (iii) Outcome variable AppCreche
for (dim in dimensions) {
  formula <- as.formula(paste("AppCreche ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feols(formula, data = reg_MainDB, cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_ols_AppCreche[[dim]] <- model
}

# (iv) Outcome variable UseCreche
for (dim in dimensions) {
  formula <- as.formula(paste("UseCreche ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feols(formula, data = reg_MainDB, cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_ols_UseCreche[[dim]] <- model
}

#### Create and display the OLS summary tables ####

# Migration background (NoMigrationBackground)
dimension_to_extract <- "NoMigrationBackground"
models_migration <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

migration <- modelsummary(models_migration, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: Migration background (OLS)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part="body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

migration

# Level of knowledge (High_knowledge)
dimension_to_extract <- "High_knowledge"
models_knowledge <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

knowledge <- modelsummary(models_knowledge, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: Level of knowledge (OLS)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

knowledge

# Temporal orientation (PresentOrientated)
dimension_to_extract <- "PresentOrientated"
models_temporal <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

temporal <- modelsummary(models_temporal, 
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = 'conf.int',
                         stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                         gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                         title = "HTE: Temporal orientation (OLS)",
                         notes = "Standard errors are cluster-heteroskedasticity robust.",
                         output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

temporal

# Past early childcare use (UsedECEC)
dimension_to_extract <- "UsedECEC"
models_use <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

use <- modelsummary(models_use, 
                    estimate = '{estimate}{stars} ({std.error})',
                    statistic = 'conf.int',
                    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                    gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                    title = "HTE: Past use (OLS)",
                    notes = "Standard errors are cluster-heteroskedasticity robust.",
                    output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

use


# Migration background (NoMigrationBackground)
dimension_to_extract <- "NoMigrationBackground"
models_migration <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

migration <- modelsummary(models_migration, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: Migration background (OLS)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part="body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

migration

# Level of knowledge (High_knowledge)
dimension_to_extract <- "High_knowledge"
models_knowledge <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

knowledge <- modelsummary(models_knowledge, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: Level of knowledge (OLS)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

knowledge

# Temporal orientation (PresentOrientated)
dimension_to_extract <- "PresentOrientated"
models_temporal <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

temporal <- modelsummary(models_temporal, 
                         estimate = '{estimate}{stars} ({std.error})',
                         statistic = 'conf.int',
                         stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                         gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                         title = "HTE: Temporal orientation (OLS)",
                         notes = "Standard errors are cluster-heteroskedasticity robust.",
                         output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

temporal

# Past early childcare use (UsedECEC)
dimension_to_extract <- "UsedECEC"
models_use <- list(
  "Early childcare_Application" = models_ols_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_ols_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_ols_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_ols_UseCreche[[dimension_to_extract]]
)

use <- modelsummary(models_use, 
                    estimate = '{estimate}{stars} ({std.error})',
                    statistic = 'conf.int',
                    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                    gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                    title = "HTE: Past use (OLS)",
                    notes = "Standard errors are cluster-heteroskedasticity robust.",
                    output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

use

#------ RobustnessLogitMainTable --------


# Function to rename coefficients in a fixest model
rename_coefficients <- function(model, new_names) {
  names(model$coefficients) <- new_names
  return(model)
}
#faut convertir en facteur pour être sûr que ça fonctionne avec feols
reg_MainDB$StrataWave <- as.factor(reg_MainDB$StrataWave)

logit_ECSApp <- feglm(ECSApp ~ T1 + T2 | StrataWave, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)

#(ii) Outcome variable : ECSUseYes : 
logit_ECSUseYes <- feglm(ECSUseYes ~ T1 + T2 | StrataWave, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)

#(iii) Outcome variable : AppCreche : 
logit_AppCreche <- feglm(AppCreche ~ T1 + T2 | StrataWave, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)

#(iv) Outcome variable : UseCreche 
logit_UseCreche <- feglm(UseCreche ~ T1 + T2 | StrataWave, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)



logit_models <- list(
  "Early childcare_Application" = logit_ECSApp,
  "Early childcare_Access" = logit_ECSUseYes,
  "Daycare_Application" = logit_AppCreche,
  "Daycare_Access" = logit_UseCreche
)


# Current names of the variables
current_names <- c("T1", "T2")

# New desired names
new_names <- c("Information-only treatment", "Information + support treatment")

# Rename the coefficients in each model
logit_models <- lapply(logit_models, function(model) {
  rename_coefficients(model, new_names)
})

logit_table <- modelsummary(logit_models,
                            fmt = fmt_statistic(estimate = 2, std.error = 2, conf.int = 2),
                            estimate = '{estimate}{stars} ({std.error})',
                            statistic = 'conf.int',
                            stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                            gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                            #add_rows = mean_df,
                            title = "Replication of the outcomes with logit models",
                            notes = "Standard errors are cluster-heteroskedasticity robust.",
                            output = 'flextable', 
                            exponentiate = TRUE) %>% #exponentiated ceofficients
  theme_booktabs() %>%
  separate_header(split = "_", opts = c("center-hspan")) %>%
  italic(i = c(1), part = "header") %>%
  italic(j = c(1), part = "body") %>%
  fontsize(size = 9, part = "footer") %>%
  fontsize(size = 10, part = "body") %>%
  align(part = "header", align = "center") %>%
  align(part = "body", align = "center") %>%
  width(j = c(2, 3, 4), width = 2.7, unit = "cm") %>%
  width(j = c(1), width = 2.4, unit = "cm") %>% 
  hline(c(2,4),part="body")|> autofit()



logit_table


#------ RobustnessLogitHTE --------

# Define lists to store the models for Logit
models_logit_ECSApp <- list()
models_logit_ECSUseYes <- list()
models_logit_AppCreche <- list()
models_logit_UseCreche <- list()


# List of dimensions of heterogeneity
dimensions <- c("High_SES", "NoMigrationBackground", "HighCoverageBaseline", "High_knowledge", "PresentOrientated", "HighCoverage_total", "UsedECEC", "HigherThanMeadianISEIMother", "HigherThanMeadianSESIndex")

# Function to rename coefficients in a fixest model
rename_coefficients <- function(model) {
  coef_names <- names(model$coefficients)
  coef_names <- gsub("T1", "Information-only treatment", coef_names)
  coef_names <- gsub("T2", "Information + support", coef_names)
  names(model$coefficients) <- coef_names
  return(model)
}

# (i) Outcome variable ECSApp
for (dim in dimensions) {
  formula <- as.formula(paste("ECSApp ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feglm(formula, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_logit_ECSApp[[dim]] <- model
}

# (ii) Outcome variable ECSUseYes
for (dim in dimensions) {
  formula <- as.formula(paste("ECSUseYes ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feglm(formula, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_logit_ECSUseYes[[dim]] <- model
}

# (iii) Outcome variable AppCreche
for (dim in dimensions) {
  formula <- as.formula(paste("AppCreche ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feglm(formula, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_logit_AppCreche[[dim]] <- model
}

# (iv) Outcome variable UseCreche
for (dim in dimensions) {
  formula <- as.formula(paste("UseCreche ~ T1 *", dim, "+ T2 *", dim, "| StrataWave"))
  model <- feglm(formula, data = reg_MainDB, family = binomial(), cluster = ~StrataWave)
  model <- rename_coefficients(model)
  models_logit_UseCreche[[dim]] <- model
}

#### **Table 1. Socioeconomic status (OLS)**


## SES1 OLS Robusteness


# SES1 Logit Robustness
dimension_to_extract <- "HigherThanMeadianISEIMother"
models_ISEI_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

isei_logit <- modelsummary(models_ISEI_logit, 
                           estimate = '{estimate}{stars} ({std.error})',
                           statistic = 'conf.int',
                           stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                           gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                           title = "HTE: SES-Occupation (Logit-Odds Ratios)",
                           notes = "Standard errors are cluster-heteroskedasticity robust.",
                           exponentiate = TRUE,
                           output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>
  hline(c(10),part="body") %>% autofit()

isei_logit

# SES2 Logit Robustness
dimension_to_extract <- "HigherThanMeadianSESIndex"
models_ses_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

ses_logit <- modelsummary(models_ses_logit, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared", "Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: SES composite index (Logit-Odds Ratios)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          exponentiate = TRUE,
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|>
  hline(c(10),part="body") %>% autofit()

ses_logit



#Education logit
SES_ECSAppl <- models_logit_ECSApp[1]
SES_ECSAppl <- SES_ECSAppl$High_SES

SES_ECSUsel <- models_logit_ECSUseYes[1]
SES_ECSUsel <- SES_ECSUsel$High_SES

SES_AppCrechel <- models_logit_AppCreche[1]
SES_AppCrechel <- SES_AppCrechel$High_SES

SES_UseCrechel <- models_logit_UseCreche[1]
SES_UseCrechel <- SES_UseCrechel$High_SES

SESl <- modelsummary(list("Early childcare_Application" = SES_ECSAppl, 
                          "Early childcare_Access" = SES_ECSUsel, 
                          "Daycare_Application" = SES_AppCrechel, 
                          "Daycare_Access" = SES_UseCrechel), 
                     estimate = '{estimate}{stars} ({std.error})',
                     statistic = 'conf.int',
                     stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                     gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                     #add_rows = mean_df,
                     title = "HTE : SES (logit-Odds Ratios)",
                     notes = "Standard errors are cluster-heteroskedasticity robust.",
                     exponentiate = TRUE,
                     output = "flextable")  %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  #  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  # width(j=c(4,5,7,8),width=2.7,unit = "cm")|>
  # width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(10),part="body")%>% autofit()


SESl


#### **Table 2. Migration background **



# Migration background (NoMigrationBackground)
dimension_to_extract <- "NoMigrationBackground"
models_migration_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

migration_logit <- modelsummary(models_migration_logit, 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = 'conf.int',
                                stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                                gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                                title = "HTE: Migration background (Logit-Odds Ratios)",
                                notes = "Standard errors are cluster-heteroskedasticity robust.",
                                exponentiate = TRUE,
                                output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

migration_logit

# Level of knowledge (High_knowledge)
dimension_to_extract <- "High_knowledge"
models_knowledge_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

knowledge_logit <- modelsummary(models_knowledge_logit, 
                                estimate = '{estimate}{stars} ({std.error})',
                                statistic = 'conf.int',
                                stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                                gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                                title = "HTE: Level of knowledge (Logit-Odds Ratios)",
                                notes = "Standard errors are cluster-heteroskedasticity robust.",
                                exponentiate = TRUE,
                                output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

knowledge_logit

# Temporal orientation (PresentOrientated)
dimension_to_extract <- "PresentOrientated"
models_temporal_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

temporal_logit <- modelsummary(models_temporal_logit, 
                               estimate = '{estimate}{stars} ({std.error})',
                               statistic = 'conf.int',
                               stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                               gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                               title = "HTE: Temporal orientation (Logit-Odds Ratios)",
                               notes = "Standard errors are cluster-heteroskedasticity robust.",
                               exponentiate = TRUE,
                               output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

temporal_logit

# Past early childcare use (UsedECEC)
dimension_to_extract <- "UsedECEC"
models_use_logit <- list(
  "Early childcare_Application" = models_logit_ECSApp[[dimension_to_extract]],
  "Early childcare_Access" = models_logit_ECSUseYes[[dimension_to_extract]],
  "Daycare_Application" = models_logit_AppCreche[[dimension_to_extract]],
  "Daycare_Access" = models_logit_UseCreche[[dimension_to_extract]]
)

use_logit <- modelsummary(models_use_logit, 
                          estimate = '{estimate}{stars} ({std.error})',
                          statistic = 'conf.int',
                          stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
                          gof_map  = c("nobs", "r.squared", "adj.r.squared","Std.Errors", 'FE: StrataWave', 'Mean of DV'),
                          title = "HTE: Past early childcare use (Logit-Odds Ratios)",
                          notes = "Standard errors are cluster-heteroskedasticity robust.",
                          exponentiate = TRUE,
                          output = "flextable") %>%   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   
  bold(i=1,  part = "header") %>%  
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  merge_v(j=2, part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer") %>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                
  align(part = "body", align = "center")|> hline(c(10),part="body") %>% autofit()

use_logit


