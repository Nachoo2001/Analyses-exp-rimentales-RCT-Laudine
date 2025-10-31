#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#
#---------------------     Investigating how administrative burden and search costs        -----------------------------------#               
#--------------------                       affect social inequalities                     -----------------------------------#  
#---------------------      in early childcare access, a randomised controlled trial       -----------------------------------#
#                                                -------------#
#--------------------                            Main analysis                             -----------------------------------# 
#--------------------              Authors: Laudine Carbuccia & Arthur Heim                -----------------------------------#    
#--------------------                               Version Final                          -----------------------------------#  
#--------------------                               July 2025                              -----------------------------------#     
#-----------------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------------#


# Instructions and comments for replication
# This file is built with chunk labels than are called from the main Rmarkdown document.
# These chunks are to be launched after running the package loading and data wrangling scripts.



#------ AttentionAction --------

# The idea is to plot the average difference between high/low SES for 3 outcomes
# outcomes <- c("ECSPlanToBaseline", "ECSApp", "ECSUseYes")

# Keep only control group and recode outcome for intention to use as a dummy
DatFig2 <- MainDB %>% filter(Assignment=="Control") %>% mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0))

# stack database for each outcome to simultaneously estimate the differences across outcomes
DatFig2 <- bind_rows(DatFig2 %>% mutate(Outcome=ECSPlanToBaseline,model="ECSPlanToBaseline"),
                     DatFig2 %>% mutate(Outcome=ECSApp,model="ECSApp"),
                     DatFig2 %>% mutate(Outcome=ECSUseYes,model="ECSUseYes"),
) %>% mutate(model=factor(model,levels=c("ECSPlanToBaseline","ECSApp","ECSUseYes")))

# OLS regression of the outcome on the interaction of the model and the heterogeneous variable with model fixed effect
# and clustered standard error at the individual level. Fixed effects for within comparison and clustering to 
# account for the fact that answers for the same individual are correlated.
DifEduc <- feols(Outcome~i(model,Educ2,ref2 = "High-SES")|model,DatFig2,cluster = ~ResponseId)
DifMigration <- feols(Outcome~i(model,MigrationBackground,ref2 = "No")|model,DatFig2,cluster = ~ResponseId)

tidyDifEduc <- tidy(DifEduc,conf.int = T) %>% mutate(term=str_replace_all(term,"::",":")) %>% 
  separate(term,into = c("model","Outcome","heterogeneity","term"),sep=":")%>% select(-model)


tidyDifMigration <- tidy(DifMigration,conf.int = T) %>% mutate(term=str_replace_all(term,"::",":")) %>% 
  separate(term,into = c("model","Outcome","heterogeneity","term"),sep=":") %>% select(-model)

Stack.Intend.Gap <-bind_rows(tidyDifEduc,tidyDifMigration) %>% 
  mutate(OutcomeLabel=factor(case_when(str_detect(Outcome,"ECSPlanToBaseline")~"Intend to use",
                                       str_detect(Outcome,"ECSApp")~"Apply",
                                       str_detect(Outcome,"ECSUseYes")~"Access"),
                             levels=c("Intend to use","Apply","Access")),
         termPlot=ifelse(term=="Low-SES",'Gap by SES','Gap by migration background'),
         termPlot=factor(termPlot,
                         levels=c('Gap by SES','Gap by migration background')))



# Perform tests for both models
tidyglhtEduc <- perform_pairwise_tests(DifEduc, "Gap by SES")
tidyglhtMig <- perform_pairwise_tests(DifMigration, "Gap by migration background")

# to display the results of the test statistics of pairwise differences, we need to add y coordinates
# For that to be done without manually plugging values, we can use the results in Stack.Intend.Gap and also
# retrieve the labels for group variables

#table to match labels and generate Y coordinates
LabelVars <- Stack.Intend.Gap %>% 
  select(Outcome,OutcomeLabel,termPlot,term,upper = conf.high,lower = conf.low) %>% mutate(Outcome=str_remove(Outcome,"^\\s+"))

tidyglhtEduc <- tidyglhtEduc %>% left_join(.,LabelVars%>% filter(termPlot=="Gap by SES")%>% 
                                             select(Outcome,Group1Label = OutcomeLabel,upper1=upper,lower1=lower),
                                           by=c("group1"="Outcome")) %>% 
  left_join(.,LabelVars%>% filter(termPlot=="Gap by SES")%>% 
              select(Outcome,Group2Label = OutcomeLabel,upper2=upper,lower2=lower),
            by=c("group2"="Outcome")) %>% 
  mutate(Y=case_when(group1=="ECSPlanToBaseline" & group2=="ECSApp"~lower2*1.1,
                     group1=="ECSPlanToBaseline" & group2=="ECSUseYes"~upper1*.9,
                     group1=="ECSApp" & group2=="ECSUseYes"~upper1*.9,
  ))


tidyglhtMig <- tidyglhtMig %>% left_join(.,LabelVars%>% filter(termPlot=="Gap by migration background")%>% 
                                           select(Outcome,Group1Label = OutcomeLabel,upper1=upper,lower1=lower),
                                         by=c("group1"="Outcome")) %>% 
  left_join(.,LabelVars%>% filter(termPlot=="Gap by migration background")%>% 
              select(Outcome,Group2Label = OutcomeLabel,upper2=upper,lower2=lower),
            by=c("group2"="Outcome")) %>% 
  mutate(Y=case_when(group1=="ECSPlanToBaseline" & group2=="ECSApp"~lower2*1.1,
                     group1=="ECSPlanToBaseline" & group2=="ECSUseYes"~upper1*1.1,
                     group1=="ECSApp" & group2=="ECSUseYes"~upper1*.9,
  ))


# Combine all test results
all_pairwise_tests <- bind_rows(tidyglhtEduc, tidyglhtMig) %>% 
  mutate(label=paste("dif = ",round(estimate,2)," (",round(std.error,2),"), adj.p = ",round(adj.p.value,3),sep=""))


#---- MakeFig2 -----



Fig2 <- ggplot() + 
  geom_pointrange(data=Stack.Intend.Gap,
                  aes(x = interaction(OutcomeLabel,termPlot,sep="!"), y = estimate, color = OutcomeLabel,
                      ymin = conf.low, ymax = conf.high, shape = OutcomeLabel),
                  position = position_dodge(.5), size = 0.8) + 
  geom_hline(aes(yintercept = 0), linetype = 2, alpha = 0.5) +
  geom_segment(data = all_pairwise_tests,aes(x=interaction(Group1Label,het_var,sep="!")
                                             ,xend=interaction(Group2Label,het_var,sep="!"),
                                             y=Y,yend=Y,
  ),linetype = 3,arrow=arrow(length = unit(0.01,"cm")))+
  geom_text(data = all_pairwise_tests, 
            aes(x = interaction(Group1Label,het_var,sep="!"),
                y=Y+.01,
                label = label
            ),size=1.8,size.unit="mm",nudge_x = .5)+
  guides(x = guide_axis_nested(type = "box",key_range_auto(sep="!")))+
  scale_color_viridis_d("Coefficients \nand 95 % CI", alpha = .8, option = "A", end = .6) +
  scale_x_discrete(name = "Average demographic groups' differences in main outcomes") +
  scale_shape("Coefficients \nand 95 % CI") +
  labs(#subtitle = "Point estimates with 95% confidence intervals and pairwise comparisons",
       y = "Intention to action gaps (pp)",
#       caption=
#         "Sources: Control group only, N baseline =623, N endline = 494. Intention are measured at the baseline survey during pregnancy (Q4 2022); application and access 
#are measured at the endline survey one year after (Q4 2023).
#Notes: Coefficient and pointwise standard errors of OLS regressions of the outcome on the interaction of the model 
#and the heterogeneous variable with model fixed effect and clustered standard error at the individual level (CR2).
#Intention-to-action gap in early childcare application 
#and access gap in the control group across SES and migration background:
#- Gap by SES compares households (HH) in which the mother did not attend any kind of post-secondary education 
#with those in which the mother did.
#- Gap by migration background compares HH in which the mother was born abroad with HH in which the mother was born in France."
  )+theme_nhb

Fig2


#---- ExportFig2 ----#
save_nhb(Fig2, "Figures/Fig2.pdf", cols = 1,height_cm = 8.8)
#ggsave("Figures/Fig2.pdf", width = 8.8, height = 13, units = "cm", device = cairo_pdf)

rm(DifEduc,DifMigration,LabelVars,tidyDifEduc,TidyDifMigration,tidyGlhtEduc,tidyGlhtMig)


#----- TableITT -----

set.seed(999)


ITT.ECSApp <- ITTSimultaneous(Y="ECSApp",
                              treat="Z",
                              DB=PostDB,
                              Correction="Westfall",
                              sided.test ="two.sided",
                              weights="WeightPS")

ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche",
                                    treat="Z",
                                    DB=PostDB,
                                    sided.test ="two.sided",
                                    Correction="Westfall",
                                    weights="WeightPS")



ITT.UseCreche <- ITTSimultaneous(Y="UseCreche",
                                 treat="Z",
                                 DB=PostDB,
                                 Correction="Westfall",
                                 sided.test ="two.sided",
                                 weights="WeightPS")

ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes",
                                 treat="Z",
                                 DB=PostDB,
                                 Correction="Westfall",
                                 sided.test ="two.sided",
                                 weights="WeightPS")
#Coef Map for clear labels
cm <- c('T1-C'    = 'Information-only vs Control ',
        'T2-C'    = 'Information + Support vs Control',
        'T2-T1'   = 'Information + support vs Information-only',
        "Control mean" = "Mean control group")


MainResultTable1 =
  modelsummary(list(
    "Application_Early childcare"=ITT.ECSApp$ModelSummary,
    "Application_Daycare"=ITT.ECSAppCreche$ModelSummary,
    "Access_Early childcare"=ITT.ECSUseYes$ModelSummary,
    "Access_Daycare"=ITT.UseCreche$ModelSummary
  ),
  coef_map = cm,
  fmt=fmt_statistic(estimate=2, 
                    adj.p.value=3,
                    std.error=2,
                    conf.int=2,
                    "Chi 2"=2,
                    "P-value"=3), 
  estimate = '{estimate} ({std.error})',
  statistic = c("p = {p.value}","conf.int",
                "adj.p.val. = {adj.p.value}"),
  stars=FALSE,
  #stars = c('*' = .1,'**' = .05, '***' = .01),
  gof_map = c(
    "Covariates","Fixed effects","DF t-test","Chi 2","DF Chi2","P-value",
    "nobs", "r.squared","adj.r.squared"),
  #title="Intention-to-treat effects on the main outcomes",
#  notes=paste("Sources:", SourcesStacked,"      
#Each column jointly estimates the average differences between arms using fully-saturated stacked OLS regressions and inverse propensity score weights. Control means estimated separately by OLS.
#Standard errors in parentheses are cluster-heteroskedasticity robust (CR2) adjusted at the block level.
#Simultaneous inference procedures - implemented via general linear hypothesis testing (glht) - control for multiple comparisons with two-sided tests using Westfall adjustment. 
#Adjusted p-values and simultaneous confidence intervals in bracklets maintain familywise error rate at 5%. Joint chi-square test evaluates global treatment significance."),
  output = 'flextable') 



Table_1 <- 
MainResultTable1 %>% 
  merge_at(j=1,i=c(1:4),part="body")|>   
  merge_at(j=1,i=c(5:8),part="body")|>   
  merge_at(j=1,i=c(9:12),part="body") |>   
  merge_at(j=1,i=c(13:14),part="body") |>   
  #theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=1,i=c(1:2),part="header") %>% 
  #merge_v(j = 1,part="header") %>% 
  italic(i = c(2),  part = "header") %>% 
  bold(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3,4,5),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  hline(c(12,14),part="body") %>% 
  # Line spacing & padding
  padding(padding = 2, part = "all") %>%
  bg(i =  c(1,5,9,13), bg = "lightgrey",j = c(2:5)) %>% 
  line_spacing(space = 1)# %>%
  
  # Autofit and width (single-column: 8.8cm = ~3.46in)
  #autofit() %>%
 # width(width = 3.46, unit = "in") #%>%
  
Table_1

save_as_docx(Table_1,path = "Tables/Table_1.docx")

t1.b.ECSApp.T2C <- ITT.ECSApp$Tidy$estimate[ITT.ECSApp$Tidy$term=="T2-C"]
t1.se.ECSApp.T2C <- ITT.ECSApp$Tidy$std.error[ITT.ECSApp$Tidy$term=="T2-C"]
t1.p.ECSApp.T2C <- ITT.ECSApp$Tidy$p.value[ITT.ECSApp$Tidy$term=="T2-C"]
t1.ap.ECSApp.T2C <- ITT.ECSApp$Tidy$adj.p.value[ITT.ECSApp$Tidy$term=="T2-C"]
t1.slb.ECSApp.T2C <- ITT.ECSApp$Tidy$conf.low[ITT.ECSApp$Tidy$term=="T2-C"]
t1.sub.ECSApp.T2C <- ITT.ECSApp$Tidy$conf.high[ITT.ECSApp$Tidy$term=="T2-C"]

# Table 1 Early childcare application information vs control
t1.b.ECSApp.T1C <- ITT.ECSApp$Tidy$estimate[ITT.ECSApp$Tidy$term=="T1-C"]
t1.se.ECSApp.T1C <- ITT.ECSApp$Tidy$std.error[ITT.ECSApp$Tidy$term=="T1-C"]
t1.p.ECSApp.T1C <- ITT.ECSApp$Tidy$p.value[ITT.ECSApp$Tidy$term=="T1-C"]
t1.ap.ECSApp.T1C <- ITT.ECSApp$Tidy$adj.p.value[ITT.ECSApp$Tidy$term=="T1-C"]
t1.slb.ECSApp.T1C <- ITT.ECSApp$Tidy$conf.low[ITT.ECSApp$Tidy$term=="T1-C"]
t1.sub.ECSApp.T1C <- ITT.ECSApp$Tidy$conf.high[ITT.ECSApp$Tidy$term=="T1-C"]

# Table 1 Early childcare application information + support vs information 
t1.b.ECSApp.T2T1 <- ITT.ECSApp$Tidy$estimate[ITT.ECSApp$Tidy$term=="T2-T1"]
t1.se.ECSApp.T2T1 <- ITT.ECSApp$Tidy$std.error[ITT.ECSApp$Tidy$term=="T2-T1"]
t1.p.ECSApp.T2T1 <- ITT.ECSApp$Tidy$p.value[ITT.ECSApp$Tidy$term=="T2-T1"]
t1.ap.ECSApp.T2T1 <- ITT.ECSApp$Tidy$adj.p.value[ITT.ECSApp$Tidy$term=="T2-T1"]
t1.slb.ECSApp.T2T1 <- ITT.ECSApp$Tidy$conf.low[ITT.ECSApp$Tidy$term=="T2-T1"]
t1.sub.ECSApp.T2T1 <- ITT.ECSApp$Tidy$conf.high[ITT.ECSApp$Tidy$term=="T2-T1"]

# Table 1 Daycare application information + support vs information 
t1.b.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$estimate[ITT.ECSAppCreche$Tidy$term=="T2-T1"]
t1.se.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$std.error[ITT.ECSAppCreche$Tidy$term=="T2-T1"]
t1.p.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$p.value[ITT.ECSAppCreche$Tidy$term=="T2-T1"]
t1.ap.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$adj.p.value[ITT.ECSAppCreche$Tidy$term=="T2-T1"]
t1.slb.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$conf.low[ITT.ECSAppCreche$Tidy$term=="T2-T1"]
t1.sub.ECSAppCreche.T2T1 <- ITT.ECSAppCreche$Tidy$conf.high[ITT.ECSAppCreche$Tidy$term=="T2-T1"]

# Table 1 Daycare application information + support vs control 
t1.b.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$estimate[ITT.ECSAppCreche$Tidy$term=="T2-C"]
t1.se.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$std.error[ITT.ECSAppCreche$Tidy$term=="T2-C"]
t1.p.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$p.value[ITT.ECSAppCreche$Tidy$term=="T2-C"]
t1.ap.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$adj.p.value[ITT.ECSAppCreche$Tidy$term=="T2-C"]
t1.slb.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$conf.low[ITT.ECSAppCreche$Tidy$term=="T2-C"]
t1.sub.ECSAppCreche.T2C <- ITT.ECSAppCreche$Tidy$conf.high[ITT.ECSAppCreche$Tidy$term=="T2-C"]

# Table 1 Daycare application information + support vs control 
t1.b.UseCreche.T2C <- ITT.UseCreche$Tidy$estimate[ITT.UseCreche$Tidy$term=="T2-C"]
t1.se.UseCreche.T2C <- ITT.UseCreche$Tidy$std.error[ITT.UseCreche$Tidy$term=="T2-C"]
t1.p.UseCreche.T2C <- ITT.UseCreche$Tidy$p.value[ITT.UseCreche$Tidy$term=="T2-C"]
t1.ap.UseCreche.T2C <- ITT.UseCreche$Tidy$adj.p.value[ITT.UseCreche$Tidy$term=="T2-C"]
t1.slb.UseCreche.T2C <- ITT.UseCreche$Tidy$conf.low[ITT.UseCreche$Tidy$term=="T2-C"]
t1.sub.UseCreche.T2C <- ITT.UseCreche$Tidy$conf.high[ITT.UseCreche$Tidy$term=="T2-C"]
# Table 1 Early childcare application information + support vs control
t1.DF <- ITT.ECSApp$ModelSummary$glance$`DF t-test`
T1.c.ECSApp <- ITT.ECSApp$ModelSummary$tidy$estimate[ITT.ECSApp$ModelSummary$tidy$term=="Control mean"]


#------ BalanceTable ----------

## This chunk construct the balance table of covariates at baseline for the appendix
# We add more covariates for the second round, following the coments
tabVar <- MainDB  %>% 
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
    "Assignment" = Assignment,
    "Single-parent family" = SingleMum1or0,
    "Age of the mother" = Age,
    "Number of children in the household" = NumberOfChildren3,
    "The mother is born in France" = BornFr1or0,
    "The mother has a post-secondary education (high-SES)" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    "The household earns less than €2,500 per month" = FmilyEarnLessThan2500,
    "The mother is present orientated" = Discount501or0,
    "The mother is active at baseline" = Active1or0,
    "The mother wants to work after maternity leaves" = WorkPlanTo1or0,
    "The household has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "The mother wants to use early childcare" = PlanToUseECS1or0,  
    "The household has access to a computer" = ComputerYes1or0,
    "The mother lives in Paris" = DepParis1or0,
    "Early childcare coverage is high" = HighECSCov1or0,
    "Child is a girl" = BabyFemale,
  )

summary_baseline_variables_endline <- tabVar %>%
  tbl_summary(
    by = Assignment,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall() %>% #add_difference() %>% 
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Information only",
                stat_2 ~ "Information + support",
                stat_3 ~ "Control") %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3") ~ "**Assignment group**") %>% 
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>% 
  add_q(method = "BY",
        pvalue_fun = ~ style_pvalue(.x, digits = 3))


Table_2 <- summary_baseline_variables_endline %>% 
  as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_v(part = "header", j = 3) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1,part="body")|>  
  width(j=c(1),unit = "cm",width=3)|>
  width(j=c(2:7),unit = "cm",width=2)|>
#  set_caption(caption = "Baseline balance by treatment groups") %>% 
#  add_footer_lines(
#    "Sources:Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
#We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test. Q-value control for the false discovery rate (FDR) using the Benjamini and Yekutieli method.
#")  %>%  
  fontsize(size=7,part="all")

Table_2
save_as_docx(Table_2,path = "Tables/Table_2.docx")
#----- HetT2ITTATT ------------


## First etimate the ITT
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            ,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

## Estimate the ATT
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


## Estimate the ATT
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")
# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.EducMig <- bind_rows(DataPlot,DataPlotUse)

# PanelA <- ggplot(Data.Het.EducMig %>% filter(panel=="Control group") %>% mutate(panel=as.character(panel)))+
#   geom_pointrange(aes(
#     y=interaction(Het,Heterogeneity,sep="!"),
#     x=estimate,
#     xmin=point.conf.low,
#     xmax=point.conf.high,
#     shape=Group,
#     color=Group),position = position_dodge(.4))+
#   geom_crossbar(aes(
#     x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
#     fill = Group, xmin = conf.low,
#     color = Group, xmax = conf.high
#   ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
#   # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
#   #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
#   #               ),size=1.2,nudge_y = .4)+
#   #facet_wrap(~panel,scales="free_x")+
#   facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
#   # facet_wrap(~Y+panel,scales="free_x")+
#   # coord_flip()+
#  # geom_vline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
#   #           aes(xintercept = 0),linetype=c(2))+
#   xlab("")+
#   #scale_vline(aes(Yintercept=0))+
#   guides(y =guide_axis_nested(key_range_auto(sep="!"),angle = 90))+
#   scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
#   ) +
#   scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
#   scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
#   labs(y="Models",
#        x="Conditional outcomes without intervention",
#        #     caption = paste("Sources:", SourcesStacked,
#        #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
#        #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
#        #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
#        #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
#        # of pairwise comparisons and subgroups using the Westfall-Young method.",
#        #                     "\nAll models include block fixed effects")
#   )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))+#+ vis_theme
# theme(axis.title.y=element_blank(),
#       axis.text.y=element_blank())
# 
# PanelB <- ggplot(Data.Het.EducMig %>% filter(panel!="Control group"))+
#   geom_pointrange(aes(
#     y=interaction(Het,Heterogeneity,sep="!"),
#     x=estimate,
#     xmin=point.conf.low,
#     xmax=point.conf.high,
#     shape=Group,
#     color=Group),position = position_dodge(.4))+
#   geom_crossbar(aes(
#     x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
#     fill = Group, xmin = conf.low,
#     color = Group, xmax = conf.high
#   ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
#   # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
#   #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
#   #               ),size=1.2,nudge_y = .4)+
#   #facet_wrap(~panel,scales="free_x")+
#   facet_grid(rows=vars(fct_rev(Y)),cols=vars(panel))+
#   # facet_wrap(~Y+panel,scales="free_x")+
#   # coord_flip()+
#   geom_vline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
#              aes(xintercept = 0),linetype=c(2))+
#   xlab("")+
#   #scale_vline(aes(Yintercept=0))+
#   guides(y = guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
#   scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
#   ) +
#   scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
#   scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
#   labs(y="",
#        x="Conditional average treatment effects",
#        #     caption = paste("Sources:", SourcesStacked,
#        #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
#        #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
#        #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
#        #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
#        # of pairwise comparisons and subgroups using the Westfall-Young method.",
#        #                     "\nAll models include block fixed effects")
#   )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))#+ vis_theme
# 
# Fig3 <- ggarrange(PanelA,PanelB,align = "v",common.legend = TRUE,legend="bottom")



Fig3 <- ggplot(Data.Het.EducMig)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+

  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
   geom_vline(data=Data.Het.EducMig %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+
  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90),size=3)+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))+
  theme(axis.text.y = element_text(size = 6))
  #+#+ vis_theme
 # theme(axis.title.y=element_blank(),
  #      axis.text.y=element_blank())


Fig3

save_nhb(Fig3, "Figures/Fig3.pdf", cols = 2, height_cm = 6 )

#----- HetT2Daycare ------------


## First etimate the ITT
Het.ITT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")


Het.ITT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")

## Estimate the ATT
Het.ATT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")
Het.ATT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")



# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.AppCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")


## Estimate the ATT
Het.ATT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = FALSE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ATT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity = "MigrationBackground",
                                                  ITT = FALSE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")
# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.UseCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access daycare", panel = "ATT", Heterogeneity = "SES", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.UseCreche.Mig$Tidy %>% mutate(Y = "Access daycare", panel = "ATT", Heterogeneity = "Migration \nbackground", Type = "ATT") %>% filter(term %in% term_levels),
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.Daycare.EducMig <- bind_rows(DataPlot,DataPlotUse)


Fig4 <- ggplot(Data.Het.Daycare.EducMig)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  # coord_flip()+
   geom_vline(data=Data.Het.Daycare.EducMig %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+

  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))+#+ vis_theme
  theme(axis.text.y = element_text(size = 6))

Fig4


save_nhb(Fig4, "Figures/Fig4.pdf", cols = 2, height_cm = 6 )

#----- MechanismsInfo ------------

#Knowledge, previous ecs use and DescriptiveNorms as proxy for information costs

#UsedECEC 

## ITT App


Het.ITT.App.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDB ,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = TRUE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB ,
                                             Outcome = "ECSApp",
                                             Heterogeneity = "InfoBaseline",
                                             ITT = TRUE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")



Het.ITT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB ,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")




## ATT


Het.ATT.App.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                                 Outcome = "ECSApp",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ATT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                             Outcome = "ECSApp",
                                             Heterogeneity = "InfoBaseline",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")




Het.ATT.App.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                              Outcome = "ECSApp",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")




# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("Level of \nknowledge", "Used Early \nChildcare before","Share of people \nusing Early Childcare \naround")
panel_levels <- c("Control group", "ITT", "ATT")


# Het.ITT.App.UsedECEC
# Het.ITT.App.Info
# Het.ITT.App.Norms

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Info$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.UsedECEC$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.UsedECEC$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Norms$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Norms$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels)
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.Info$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Level of \nknowledge", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.UsedECEC$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Used Early \nChildcare before", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.Norms$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )


## ECS Use


Het.ITT.Use.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDB ,
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



Het.ITT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = TRUE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")




## ATT


Het.ATT.Use.UsedECEC <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                                 Outcome = "ECSUseYes",
                                                 Heterogeneity = "UsedECEC",
                                                 ITT = FALSE,
                                                 Weights = "WeightPS",
                                                 clusters = "StrataWave")


Het.ATT.Use.Info <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                             Outcome = "ECSUseYes",
                                             Heterogeneity = "InfoBaseline",
                                             ITT = FALSE,
                                             Weights = "WeightPS",
                                             clusters = "StrataWave")




Het.ATT.Use.Norms <- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
                                              Outcome = "ECSUseYes",
                                              Heterogeneity = "DescriptiveNorms",
                                              ITT = FALSE,
                                              Weights = "WeightPS",
                                              clusters = "StrataWave")

# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Info$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Info$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Level of \nknowledge", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Norms$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Norms$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.UsedECEC$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.UsedECEC$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Used Early \nChildcare before", Type = "ITT") %>% filter(term %in% term_levels)
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.Info$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Level of \nknowledge", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.Norms$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Share of people \nusing Early Childcare \naround", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.UsedECEC$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Used Early \nChildcare before", Type = "ATT") %>% filter(term %in% term_levels)
  
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


Data.Het.InfoFriction <- bind_rows(DataPlot,DataPlotUse) %>% mutate(Het =str_replace_all(Het," "," \n"))


Fig5 <- ggplot(Data.Het.InfoFriction)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  # coord_flip()+
  geom_vline(data=Data.Het.InfoFriction %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+
  
  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))#+#+ vis_theme
# theme(axis.title.y=element_blank(),
#      axis.text.y=element_blank())

Fig5


save_nhb(Fig5, "Figures/Fig5.pdf", cols = 2, height_cm = 8.7 )



#----- MechanismsPsych ------------


#Present biased trust et active pour psychological costs


#PresentOrientated 

## ITT App


Het.ITT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.App.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDB ,
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




## ATT


Het.ATT.App.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSApp",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.App.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
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




# Define the factors
term_levels <- c("T2-C")
heterogeneity_levels <- c("Present biased", "Trust","Activity")
panel_levels <- c("Control group", "ITT", "ATT")


# Het.ITT.App.PresentOrientated
# Het.ITT.App.Info
# Het.ITT.App.ActiveBaseline

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.TrustCreche1or0$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.TrustCreche1or0$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.PresentOrientated$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.PresentOrientated$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.ActiveBaseline$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.ActiveBaseline$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels)
)

# Merge ATTs in one DataFrame with the correct fator levels
DataPlot_ATT <- bind_rows(
  Het.ATT.App.TrustCreche1or0$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Trust", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.PresentOrientated$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Present biased", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.App.ActiveBaseline$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ATT", Heterogeneity = "Activity", Type = "ATT") %>% filter(term %in% term_levels)
)

# Combine both data frames
DataPlot <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )


## ECS Use


Het.ITT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1,"Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ITT.Use.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDB ,
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




## ATT


Het.ATT.Use.PresentOrientated <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(
  PresentOrientated=ifelse(PresentOrientated == 1, "Yes","No")),
  Outcome = "ECSUseYes",
  Heterogeneity = "PresentOrientated",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave")


Het.ATT.Use.TrustCreche1or0<- GroupHeterogeneityFnCTRL(DB = PostDBT2 ,
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

# Define the factors
term_levels <- c("T2-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Trust", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.TrustCreche1or0$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.TrustCreche1or0$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Trust", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.ActiveBaseline$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.ActiveBaseline$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Activity", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.PresentOrientated$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.PresentOrientated$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Present biased", Type = "ITT") %>% filter(term %in% term_levels)
  
)

# Merge ATTs in one DataFrame with the correct factor levels
DataPlot_ATT <- bind_rows(
  Het.ATT.Use.TrustCreche1or0$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Trust", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.ActiveBaseline$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Activity", Type = "ATT") %>% filter(term %in% term_levels),
  Het.ATT.Use.PresentOrientated$Tidy %>% mutate(Y = "Access early childcare", panel = "ATT", Heterogeneity = "Present biased", Type = "ATT") %>% filter(term %in% term_levels)
  
)

# Combine the two DataFrames
DataPlotUse <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


Data.Het.Psy <- bind_rows(DataPlot,DataPlotUse)


#%>% mutate(Het =str_replace_all(Het," "," \n"))


Fig6 <- ggplot(Data.Het.Psy)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+
  #facet_wrap(~panel,scales="free_x")+
  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
  # facet_wrap(~Y+panel,scales="free_x")+
  # coord_flip()+
  geom_vline(data=Data.Het.Psy %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+
  
  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))+
  theme(axis.text.y = element_text(size = 6))
  #+ vis_theme
# theme(axis.title.y=element_blank(),
#      axis.text.y=element_blank())

Fig6


save_nhb(Fig6, "Figures/Fig6.pdf", cols = 2, height_cm = 8.7 )


#------- Efficient_PostLassoMainFigure ----------------------

# Define outcomes and their labels
outcomes <- c("ECSApp", "ECSUseYes", "UseCreche", "AppCreche")
outcome_labels <- c("Early childcare application", "Early childcare access", 
                    "Daycare access", "Daycare application")

# Define subsamples
subsamples <- c("T1-C", "T2-C", "T2-T1")

# Create a grid of all combinations
analysis_grid <- expand_grid(
  outcome = outcomes,
  subsample = subsamples
) %>%
  mutate(outcome_label = factor(outcome, levels = outcomes, labels = outcome_labels))

# Function to run ITT analysis for a single outcome
run_itt_analysis <- function(outcome) {
  ITTSimultaneous(Y = outcome)
}

# Function to run double debiased analysis for a single outcome-subsample combination
run_double_debiased <- function(outcome, subsample) {
  EstDoubleDebiased(Y = outcome, Z = "Z", SubSample = subsample)
}

# Function to extract tidy results from double debiased output
extract_tidy_results <- function(dd_result, subsample, outcome_label, model_type) {
  model_name <- paste0(model_type, "_model")
  
  if (model_type == "debiased") {
    # For debiased model, we need to manually set the term to "Z"
    dd_result[[model_name]] %>% 
      tidy(conf.int = TRUE) %>% 
      mutate(SubSample = subsample, 
             Y = outcome_label,
             term = "Z")
  } else {
    # For comparison model, term is already "Z"
    dd_result[[model_name]] %>% 
      tidy(conf.int = TRUE) %>% 
      mutate(SubSample = subsample, 
             Y = outcome_label)
  }
}

# Step 1: Run ITT analyses for all outcomes
cat("Running ITT analyses...\n")
itt_results <- outcomes %>%
  set_names() %>%
  map(run_itt_analysis)

# Extract and combine ITT results
main_itt_results <- itt_results %>%
  imap_dfr(~ .x$Tidy %>% 
             mutate(Outcome = outcome_labels[which(outcomes == .y)],
                    Y = Outcome)) %>%
  rename("SubSample" = "Var") %>%
  mutate(Model = "ITT") %>%
  select(-c(conf.low, conf.high)) %>%
  rename(conf.low = point.conf.low, conf.high = point.conf.high)

# Step 2: Run double debiased analyses for all combinations
cat("Running double debiased analyses...\n")
dd_results <- analysis_grid %>%
  mutate(
    dd_result = map2(outcome, subsample, 
                     ~ {
                       cat("Processing", .x, "for", .y, "\n")
                       run_double_debiased(.x, .y)
                     })
  )

# Step 3: Extract results for both comparison and debiased models
cat("Extracting results...\n")

# Extract comparison model results (Post-lasso)
itt_post_lasso <- dd_results %>%
  mutate(
    tidy_result = pmap(list(dd_result, subsample, outcome_label), 
                       ~ extract_tidy_results(..1, ..2, ..3, "comparison"))
  ) %>%
  select(tidy_result) %>%
  unnest(tidy_result)

# Extract debiased model results
itt_double_debiased <- dd_results %>%
  mutate(
    tidy_result = pmap(list(dd_result, subsample, outcome_label), 
                       ~ extract_tidy_results(..1, ..2, ..3, "debiased"))
  ) %>%
  select(tidy_result) %>%
  unnest(tidy_result)

# Step 4: Combine all results for plotting
data_plot_lasso <- itt_post_lasso %>% 
  filter(term == "Z") %>% 
  mutate(Method = "Post double lasso") %>%
  bind_rows(main_itt_results %>% mutate(Method = "Basic")) %>%
  bind_rows(itt_double_debiased %>% mutate(Method = "Double debiased")) %>%
  mutate(
    ContrastLabel = case_when(
      SubSample == "T2-C" ~ "Information \n+ support \nvs. \nControl",
      SubSample == "T2-T1" ~ "Information \n+ support \nvs. \nInformation-only",
      SubSample == "T1-C" ~ "Information \nonly \nvs. \nControl"
    ),
    ContrastLabel = factor(ContrastLabel, levels = c(
      "Information \nonly \nvs. \nControl",
      "Information \n+ support \nvs. \nControl",
      "Information \n+ support \nvs. \nInformation-only"
    )),
    Y = factor(Y, levels = c(
      "Early childcare application",
      "Early childcare access",
      "Daycare application",
      "Daycare access"
    ))
  )

# Step 5: Create the plot
cat("Creating plot...\n")
Fig9 <- ggplot(data_plot_lasso) +
  geom_pointrange(
    aes(x = ContrastLabel,
        y = estimate, 
        ymin = conf.low, 
        ymax = conf.high, 
        color = Method, 
        shape = Method),
    position = position_dodge(.5)
  ) +
  geom_hline(aes(yintercept = 0), linetype = 2, alpha = 0.5) +
  scale_color_viridis_d("Method", alpha = .8, option = "A", end = .6) +
  scale_x_discrete(name = "Models") +
  scale_shape("Method") +
  facet_wrap(~Y) +
  labs(y = "Treatment effects (pp)")

# Display the plot
print(Fig9)

save_nhb(Fig9, "Figures/Fig9.pdf", cols = 2, height_cm = 8.7 )

## Optional: Save summary statistics
# cat("Analysis complete!\n")
# cat("Processed", nrow(analysis_grid), "double debiased analyses\n")
# cat("Processed", length(outcomes), "ITT analyses\n")

# Optional: Return the data for further use
# return(list(plot = plot_results, data = data_plot_lasso, dd_results = dd_results))


# #------- PostLassoMainFigure ----------------------
# 
# 
# # First, run the regression without Lasso
# ITT.UseCreche <- ITTSimultaneous(Y="UseCreche")
# ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes")
# ITT.ECSApp <- ITTSimultaneous(Y="ECSApp")
# ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche")
# 
# 
# ## Bind them together 
# MainITTResults <- bind_rows(ITT.ECSApp$Tidy %>% mutate(Outcome="Early childcare application",Y=Outcome),
#                             ITT.ECSUseYes$Tidy %>% mutate(Outcome="Early childcare access",Y=Outcome),
#                             ITT.UseCreche$Tidy %>% mutate(Outcome="Daycare access",Y=Outcome),
#                             ITT.ECSAppCreche$Tidy   %>% mutate(Outcome="Daycare application",Y=Outcome),
#                             #  ITT.ECSAppAssmat$Tidy   %>% mutate(Outcome="Apply for childminder")
# ) %>% rename("SubSample"="Var") %>% mutate(Model="ITT")
# 
# 
# # Then Post Lasso on each outcome
# 
# # Run lasso on access to early childcare in general
# ECSUseYesT1C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T1-C")
# ECSUseYesT2C <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-C")
# ECSUseYesT2T1 <- EstPostLasso(Y="ECSUseYes",Z="Z",SubSample = "T2-T1")
# 
# #EstPostLasso()
# 
# TidyECSUseYesLasso <- bind_rows(ECSUseYesT1C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
#                                 ECSUseYesT2C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
#                                 ECSUseYesT2T1$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
#   mutate(Y="Early childcare access")
# 
# # Run lasso on application for early childcare in general
# ECSAppT1C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T1-C")
# ECSAppT2C <-  EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-C")
# ECSAppT2T1 <- EstPostLasso(Y="ECSApp",Z="Z",SubSample = "T2-T1")
# 
# #EstPostLasso()
# 
# TidyECSAppLasso <- bind_rows(   ECSAppT1C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
#                                 ECSAppT2C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
#                                 ECSAppT2T1$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
#   mutate(Y="Early childcare application")
# 
# 
# 
# # Run lasso on access to daycare
# USeCrecheT1C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T1-C")
# USeCrecheT2C <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-C")
# USeCrecheT2T1 <- EstPostLasso(Y="UseCreche",Z="Z",SubSample = "T2-T1")
# 
# #EstPostLasso()
# ## Stack the results
# TidyUseCrecheLasso <- bind_rows(USeCrecheT1C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
#                                 USeCrecheT2C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
#                                 USeCrecheT2T1$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
#   mutate(Y="Daycare access")
# 
# # Run lasso on appliation for daycare
# ECSAppCrecheT1C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T1-C")
# ECSAppCrecheT2C <-  EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-C")
# ECSAppCrecheT2T1 <- EstPostLasso(Y="AppCreche",Z="Z",SubSample = "T2-T1")
# 
# #EstPostLasso()
# 
# TidyECSAppCrechepLasso <- bind_rows(   ECSAppCrecheT1C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T1-C"),
#                                        ECSAppCrecheT2C$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-C"),
#                                        ECSAppCrecheT2T1$`Post Lasso` %>% tidy(conf.int = TRUE) %>% mutate(SubSample="T2-T1")) %>% 
#   mutate(Y="Daycare application")
# 
# # on récupère tous les résultats dans un dataframe et on garde que le coefficient des itt donc on filtre sur Z
# 
# ITT.PostLasso <- bind_rows(TidyUseCrecheLasso,
#                            TidyECSAppCrechepLasso,
#                            TidyECSAppLasso,
#                            TidyECSUseYesLasso
# ) #%>% filter(term=="Z")
# 
# # MainITTResults has conf.low and conf.high with multiple testing adjustment
# # To compare pointwise estimates for OLS and post-lasso we need to make a few adjustments
# MainITTResults <- MainITTResults %>% select(-c(conf.low,conf.high)) %>% rename(conf.low=point.conf.low,conf.high=point.conf.high)
#                                             
#                                             
# dataPlotLasso <- ITT.PostLasso %>% filter(term=="Z") %>% mutate(Method="Post-lasso") %>% 
#   bind_rows(.,MainITTResults %>% mutate(Method="Basic")) %>% 
#   mutate(ContrastLabel=case_when(SubSample=="T2-C"~"Information \n+ support \nvs. \nControl",
#                                  SubSample=="T2-T1"~"Information \n+ support \nvs. \nInformation-only",
#                                  SubSample=="T1-C"~"Information \nonly \nvs. \nControl",
#   ),
#   ContrastLabel=factor(ContrastLabel,levels=c("Information \nonly \nvs. \nControl",
#                                               "Information \n+ support \nvs. \nControl",
#                                               "Information \n+ support \nvs. \nInformation-only"
#   )),
#   Y=factor(Y,levels=c("Early childcare application",
#                       "Early childcare access",
#                       "Daycare application",
#                       "Daycare access"
#   )))
#   
# 
# 
# ggplot(dataPlotLasso)+
#   geom_pointrange(
#     aes(x=ContrastLabel,#interaction(ContrastLabel,Y,sep="!"),
#         y=estimate,ymin=conf.low,ymax=conf.high,color=Method,shape=Method),position = position_dodge(.5))+
#   # guides(x = guide_axis_nested(type = "box",key_range_auto(sep="!")))+
#   geom_hline(aes(yintercept = 0), linetype = 2, alpha = 0.5) +
#   scale_color_viridis_d("Method", alpha = .8, option = "A", end = .6) +
#   scale_x_discrete(name = "Models") +
#   scale_shape("Method") +facet_wrap(~Y)+
#   
#   labs(y="Treatment effects (pp)")
# 
# 
# 
# 
# 

#------- LATE ------------
## Estimate the LATE for the 4 main outcomes and make the same table as with the 1st stage
FirstStage <- ITTSimNoControl(Y="D",DB=PostDBT2)
LATE.UseCreche   <- LATESimultaneous(Y="UseCreche")
LATE.ECSUseYes   <- LATESimultaneous(Y="ECSUseYes")
LATE.ECSApp      <- LATESimultaneous(Y="ECSApp")
LATE.ECSAppCreche <-LATESimultaneous(Y="AppCreche")


#Coef Map for clear labels
cm <- c( 'T2-C'    = 'Information + Support vs Control',
         'T2-T1'   = 'Information + support vs Information-only', 
         "Avg. cfct."= "Average counterfactual")


Table_3 <- 
modelsummary(list(
  "_First Stage"=FirstStage$ModelSummary,
  "Application_Early childcare"  =LATE.ECSApp$ModelSummary,
  "Application_Daycare"          =LATE.ECSAppCreche$ModelSummary,
  "Access_Early childcare"       =LATE.ECSUseYes$ModelSummary,
  "Access_Daycare"               =LATE.UseCreche$ModelSummary
),
fmt=fmt_statistic(estimate=2, 
                  adj.p.value=3,
                  std.error=2,
                  conf.int=2,
                  "Chi 2"=2,
                  "P-value"=3), 
estimate = '{estimate} ({std.error})',
statistic = c("p = {p.value}","conf.int",
              "adj.p.val. = {adj.p.value}"),
stars=FALSE,
#stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c(
  "Covariates","Fixed effects","Chi 2","DF","P-value",
  "nobs", "r.squared","adj.r.squared"),
coef_map=cm,
#title="Average treatment effect on the treated on the main outcomes",
#notes=paste("Sources:", SourcesStacked,
#            "
#Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
#Adjusted p-values and confidence intervals account for simultaneous inference using the",LATE.UseCreche$Correction, "method. 
#Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table.
#First stage reports OLS estimates of offering support on actual support on both comparison groups.
#Average treatment effects on the treated estimated jointly for both comparison by instrumenting administrative support in each comparison sample by assignment to T2 (centred by the pairwise instrument propensity score) interacted with the comparison sample dummy and block x wave x comparison fixed effects instrumenting themselved.
#Avg. Cfct. indicates the untreated compliers' average and is estimated by TSLS with (1-D)*Y as an outcome, (1-D) as the treatment variable instrumented by the centred assignment."
#),
output = 'flextable') %>% 
  # theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3,4,5,6),width=2.7,unit = "cm")|>
  bold(i=1,part="header") %>% 
  width(j=c(1),width=2.4,unit = "cm") %>% 
  merge_at(j=1,i=c(1,2),part="header") %>% 
  #merge_v(j=2,i=c(1,2),part="header") %>% 
  border_inner_h(border = NULL,part="header") %>% 
  merge_at(j=1,i=c(1:4),part="body")|>   
  merge_at(j=1,i=c(5:8),part="body")|>   
  #merge_at(j=1,i=c(9:10),part="body") |>   
  #merge_at(j=1,i=c(13:14),part="body") |>  
  padding(padding = 2, part = "all") %>%
  bg(i =  c(1,5), bg = "lightgrey",j = c(2:6)) %>% 
  #bg(i =  c(9), bg = "lightgrey",j = c(3:6)) %>% 
  line_spacing(space = 1) %>%
  #hline(i=2,border=NULL,part="header") %>% 
 # hline(i=1,j=c(4:6),border=NULL,part="header") %>% 
  #merge_v(i = c(6,7),j=1,part="body") %>% 
  hline(c(8),part="body")

Table_3
save_as_docx(Table_3,path = "Tables/Table_3.docx")

#----- HetT1ITTATT ------------


## First etimate the ITT
Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            ,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



# Define the factors
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)


# Combine both data frames
DataPlot <- DataPlot_ITT %>% 
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ITT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


# Define the factors
term_levels <- c("T1-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.Use.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Educ2C$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$ModelSummary0$tidy %>% mutate(Y = "Access early childcare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.Use.Mig$Tidy %>% mutate(Y = "Access early childcare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)

# Combine the two DataFrames
DataPlotUse <-DataPlot_ITT%>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.EducMigT1 <- bind_rows(DataPlot,DataPlotUse)


Fig7 <- ggplot(Data.Het.EducMigT1)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+
  
  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
  geom_vline(data=Data.Het.EducMigT1 %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+
  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))+#+ vis_theme
  theme(axis.text.y = element_text(size = 6))
# theme(axis.title.y=element_blank(),
#      axis.text.y=element_blank())


Fig7

save_nhb(Fig7, "Figures/Fig7.pdf", cols = 2, height_cm = 6 )

#------------ HetT2table ------------

# Heterogeneous effects of the information + support treatment on early childcare applications 
# Only including SES and Migration background dimensions

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


# Step 2 : estimate the conditional ATTs of interest using the function
## First estimate the ATT for applications - only SES and Migration background
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use - only SES and Migration background
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "ECSUseYes",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

# Stack ATT for application - only SES and Migration background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ATT.App.Educ2C$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for use - only SES and Migration background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ATT.Use.Educ2C$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <- list(StackedControlApp,
                     StackedITTApp,
                     StackedATTApp,
                     StackedControlUse,
                     StackedITTUse,
                     StackedATTUse
)

# Define labels
OutcomeLabel <- c("Early childcare application", "Early childcare access")

# Define the name of the models with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C' = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects by SES and migration background"

# Now the infamous model summary 
ModelT2C <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, 
                                           adj.p.value=3,
                                           std.error=2,
                                           conf.int=2,
                                           "Chi 2"=2,
                                           "P-value"=3), 
                         estimate = '{estimate} ({std.error})',
                         statistic = c("p = {p.value}","conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars=FALSE,
                         #stars = c('*' = .1,'**' = .05, '***' = .01),
                         # gof_map = c(
                         #   "Covariates","Fixed effects","Chi 2","DF","P-value",
                         #   "nobs", "r.squared","adj.r.squared"),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"nobs","r.squared","adj.r.squared"),
#                         title=TheTitle,
#                         notes=paste("Sources:", SourcesStacked,
#                                     "
#*= p<.1, **= p<.05, ***= p<.01 based on pointwise p-value.
#Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
#Models are jointly estimating conditional averages in each pair of treatment arm.
#Adjusted p-values and confidence intervals account for simultaneous inference across treatment arms.
#                         " ),
output = 'flextable') %>% 
  
theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=3,part="header")|>
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_at(j=1,i=1:16,part="body")|>
  #merge_at(j=1:3,i=1,part="footer")|>
  merge_at(j=2,i=1:8,part="body")|>
  merge_at(j=2,i=9:16,part="body")|>
  merge_at(j=3,i=1:4,part="body")|>
  merge_at(j=3,i=5:8,part="body")|>
  merge_at(j=3,i=9:12,part="body")|>
  merge_at(j=3,i=13:16,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4:9),width=2.3,unit = "cm")|>
  width(j=c(1,2, 3),width=2.1,unit = "cm") %>% 
  hline(c(4*c(1:4)),c(3:9),part="body") %>% 
  hline(c(8*c(1:2)),c(1:9),part="body") %>% 
  bg(i =  c(1,5,9,13), bg = "lightgrey",j = c(4:9)) 
#hline(c(3*c(1:24)),part="body")

ModelT2C

save_as_docx(ModelT2C,path = "Tables/Table_4.docx")

#------ DaycareApplicationAccessT1Graph ------------


## First etimate the ITT
Het.ITT.AppCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "AppCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")


Het.ITT.AppCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "AppCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")


# Define the factors
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration \nbackground")
panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct fator levels
DataPlot_ITT <- bind_rows(
  Het.ITT.AppCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Educ2C$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.AppCreche.Mig$Tidy %>% mutate(Y = "Apply for daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
)


# Combine both data frames
DataPlot <- DataPlot_ITT %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
  )



# Do the same graph for access to early childcare
## First estimate the ITT
Het.ITT.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                     Outcome = "UseCreche",
                                                     Heterogeneity = "Educ2",
                                                     ITT = TRUE,
                                                     Weights = "WeightPS",
                                                     clusters = "StrataWave")

Het.ITT.UseCreche.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                  Outcome = "UseCreche",
                                                  Heterogeneity= "MigrationBackground",
                                                  ITT = TRUE,
                                                  Weights = "WeightPS",
                                                  clusters = "StrataWave")


# Define the factors
term_levels <- c("T1-C")
#heterogeneity_levels <- c("SES", "Migration \nbackground", "Level of \nknowledge", "Temporal \norientation")
#panel_levels <- c("Control group", "ITT", "ATT")

# Merge ITTs in one DataFrame with the correct factor levels
DataPlot_ITT <- bind_rows(
  Het.ITT.UseCreche.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Educ2C$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "SES", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$ModelSummary0$tidy %>% mutate(Y = "Access daycare", panel = "Control group", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  Het.ITT.UseCreche.Mig$Tidy %>% mutate(Y = "Access daycare", panel = "ITT", Heterogeneity = "Migration \nbackground", Type = "ITT") %>% filter(term %in% term_levels),
  
)


# Combine the two DataFrames
DataPlotUse <- DataPlot_ITT %>% 
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Plot the graph with ordered factors

#### Here is what's new : 

Data.Het.Daycare.EducMig <- bind_rows(DataPlot,DataPlotUse)

Fig8 <- ggplot(Data.Het.Daycare.EducMig)+
  geom_pointrange(aes(
    y=interaction(Het,Heterogeneity,sep="!"),
    x=estimate,
    xmin=point.conf.low,
    xmax=point.conf.high,
    shape=Group,
    color=Group),position = position_dodge(.4))+
  geom_crossbar(aes(
    x = estimate, y = interaction(Het,Heterogeneity,sep="!"),
    fill = Group, xmin = conf.low,
    color = Group, xmax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  # geom_text(aes(x=estimate,y=interaction(Het,Heterogeneity,sep="!"),
  #               label = paste("ß (SE) =",round(estimate,2),"(",round(std.error,2),")","\nadj.p = ",round(adj.p.value,3))
  #               ),size=1.2,nudge_y = .4)+
  
  facet_grid(cols=vars(panel),rows=vars(fct_rev(Y)),scales = "free_x",axes="all_x",axis.labels = "all_x")+
  geom_vline(data=Data.Het.Daycare.EducMig %>% filter(panel!="Control group"),
             aes(xintercept = 0),linetype=c(2))+
  xlab("")+
  guides(y =guide_axis_nested(type = "box",key_range_auto(sep="!"),angle = 90))+
  scale_fill_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2" 
  ) +
  scale_color_brewer("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)", palette = "Dark2")+
  scale_shape("Coefficients (points), \n95% CI (error bars) \nand Westfall \nsimultaneous \n95% CI (box)")+
  labs(y="Models",
       x="Conditional average outcomes (left) and treatment effects (right)",
       #     caption = paste("Sources:", SourcesStacked,
       #                     "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
       #                     "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block level.",
       #                     "\nPoints indicate point estimates and the error bars indicate pointwise 95% Confidence Interval (CI).",
       #                     "\nBoxes around estimates indicate simultaneous 95% CI adjusted for multiple testing 
       # of pairwise comparisons and subgroups using the Westfall-Young method.",
       #                     "\nAll models include block fixed effects")
  )+theme_nhb +guides(fill=guide_legend(nrow=2,byrow=FALSE))#+#+ vis_theme
# theme(axis.title.y=element_blank(),
#      axis.text.y=element_blank())
Fig8
save_nhb(Fig8, "Figures/Fig8.pdf", cols = 1.5, height_cm = 6 )

#------------ HetT2tableDaycare ------------

# Heterogeneous effects of the information + support treatment on daycare applications 
# Only including SES and Migration background dimensions

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


# Step 2 : estimate the conditional ATTs of interest using the function
## First estimate the ATT for applications - only SES and Migration background
Het.ATT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "AppCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "AppCreche",
                                            Heterogeneity= "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

### Now let's get the models for the use - only SES and Migration background
Het.ATT.Use.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                               Outcome = "UseCreche",
                                               Heterogeneity = "Educ2",
                                               ITT = FALSE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")

Het.ATT.Use.Mig <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "UseCreche",
                                            Heterogeneity = "MigrationBackground",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")


# Stack ATT for application - only SES and Migration background
StackedATTApp <- list(
  tidy = bind_rows(Het.ATT.App.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ATT.App.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ATT.App.Educ2C$ModelSummary$glance
)
class(StackedATTApp) <- "modelsummary_list"   # define the class

# Stack ATT for use - only SES and Migration background
StackedATTUse <- list(
  tidy = bind_rows(Het.ATT.Use.Educ2C$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="SES"),
                   Het.ATT.Use.Mig$ModelSummary$tidy %>% select(-model) %>% mutate(Variable="Migration background")),
  glance = Het.ATT.Use.Educ2C$ModelSummary$glance
)
class(StackedATTUse) <- "modelsummary_list"   # define the class

# Put that in a list
TheModelsATT <- list(StackedControlApp,
                     StackedITTApp,
                     StackedATTApp,
                     StackedControlUse,
                     StackedITTUse,
                     StackedATTUse
)

# Define labels
OutcomeLabel <- c("Daycare application", "Daycare access")

# Define the name of the models with an underscore to separate them after
names(TheModelsATT) <- c(paste(OutcomeLabel[c(1)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(1)],"Conditional ATT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Avg. control",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ITT",sep="_"),
                         paste(OutcomeLabel[c(2)],"Conditional ATT",sep="_"))

# Now T2 against C
cmT2C <- c('T2-C' = 'Information + support vs control')

# Title for modelsummary
TheTitle = "Average gaps and heterogeneous treatment effects by SES and migration background (Yes = Migration background, No = Without migration background)"


# Now the infamous model summary 
Table5 <- modelsummary(TheModelsATT,
                         shape= Variable + Group ~ model,
                         fmt=fmt_statistic(estimate=2, 
                                           adj.p.value=3,
                                           std.error=2,
                                           conf.int=2,
                                           "Chi 2"=2,
                                           "P-value"=3), 
                         estimate = '{estimate} ({std.error})',
                         statistic = c("p = {p.value}","conf.int",
                                       "adj.p.val. = {adj.p.value}"),
                         stars=FALSE,
                         #stars = c('*' = .1,'**' = .05, '***' = .01),
                        #  gof_map = c(
                         #   "Covariates","Fixed effects","Chi 2","DF","P-value",
                          #  "nobs", "r.squared","adj.r.squared"),
                         coef_map = cmT2C,
                         gof_map = c('Fixed effects',"nobs", "r.squared","adj.r.squared"),
                         #                         title=TheTitle,
                         #                         notes=paste("Sources:", SourcesStacked,
                         #                                     "
                         #Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
                         #Models are jointly estimating conditional averages in each pair of treatment arm.
                         #Adjusted p-values and confidence intervals account for simultaneous inference across treatment arms.
                         #                         " ),
                         output = 'flextable') %>% 
  
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=3,part="header")|>
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_at(j=1,i=1:16,part="body")|>
  #merge_at(j=1:3,i=1,part="footer")|>
  merge_at(j=2,i=1:8,part="body")|>
  merge_at(j=2,i=9:16,part="body")|>
  merge_at(j=3,i=1:4,part="body")|>
  merge_at(j=3,i=5:8,part="body")|>
  merge_at(j=3,i=9:12,part="body")|>
  merge_at(j=3,i=13:16,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>%
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4:9),width=2.3,unit = "cm")|>
  width(j=c(1,2, 3),width=2.1,unit = "cm") %>% 
  hline(c(4*c(1:4)),c(3:9),part="body") %>% 
  hline(c(8*c(1:2)),c(1:9),part="body") %>% 
  bg(i =  c(1,5,9,13), bg = "lightgrey",j = c(4:9)) 

Table5

save_as_docx(Table5,path = "Tables/Table_5.docx")


#----- ATTRITIONTable ----


##### Tableau attrition

#### Attrition model by comparison arm ####%
M.Response.Stack <- feols(Responded~i(Z,SubSample,ref="0")|SubSampleStrata,StackedDB,weights = ~WeightPS,cluster = ~StrataWave)

# joint significance test
Glht.Response.Stacked <- glht(M.Response.Stack) 

# get the results in a tidy data frame
tidy.Glht.Response.Stacked <- left_join(tidy(Glht.Response.Stacked),tidy(confint(Glht.Response.Stacked)))

## Get the results in a tidy table for export with flextable and let's do a plot too.

tidy.Response.Stacked <- tidy(M.Response.Stack) %>% bind_cols(.,confint(M.Response.Stack)[1],
                                                              confint(M.Response.Stack)[2]) %>% 
  rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.Response.Stacked,by=c("term"="contrast","estimate","std.error")) %>% 
  mutate(term=str_remove_all(term,"Z::1:|\\(|\\)|SubSample::"))


# Also run a F test of joint null effect
ChisQTest <- glht(M.Response.Stack) %>% summary(.,test=Chisqtest())
# 
# as.data.frame(bind_cols(c("P-value"=  ChisQTest$test$pvalue,
#                           "Chi 2"= ChisQTest$test$SSH,
#                           "DF" = ChisQTest$test$df[1]
# )))


# prepare for modelsummary: baseline
modelAttritionStacked <- list(tidy=tidy.Response.Stacked, # list with tidy containing the dataframe with the estimates
                              glance=get_gof(M.Response.Stack) %>%  # statistics of the model
                                bind_cols(.,"Fixed effects"="X") %>% 
                                bind_cols(.,t(c("Chi 2"= ChisQTest$test$SSH,
                                                "DF" = ChisQTest$test$df[[1]],
                                                "P-value"=  ChisQTest$test$pvalue)
                                )))
class(modelAttritionStacked) <- "modelsummary_list"   # define the class



# Compute response rate in the comparison group (trick with OLS ;) )
ControlMean <- feols(ZO*Responded~i(ZO,SubSample,ref=0)|StrataWave^SubSample,StackedDB %>% mutate(ZO=1-Z),cluster = ~StrataWave)

# joint significance test
Glht.ControlMean <- glht(ControlMean) 

# get the results in a tidy data frame
tidy.Glht.ControlMean <- left_join(tidy(Glht.ControlMean),tidy(confint(Glht.ControlMean)))

# tidy the model 
tidy.ControlMean <- tidy(ControlMean) %>% bind_cols(.,confint(ControlMean)[1],
                                                    confint(ControlMean)[2]) %>% 
  rename("point.conf.low"="2.5 %","point.conf.high"="97.5 %") %>% left_join(.,tidy.Glht.ControlMean,by=c("term"="contrast","estimate","std.error")) %>% 
  mutate(term=str_remove_all(term,"ZO::1:|\\(|\\)|SubSample::"))

# prepare for modelsummary: baseline
AttritionControlMeans <- list(tidy=tidy.ControlMean, # list with tidy containing the dataframe with the estimates
                              glance=get_gof(ControlMean) %>% # statistics of the model
                                bind_cols(.,"Fixed effects"="X") 
)
class(AttritionControlMeans) <- "modelsummary_list"   # define the class


#Coef Map for clear labels
cm <- c('T1-C'    = 'Information-only vs Control ',
        'T2-C'    = 'Information + Support vs Control',
        'T2-T1'   = 'Information + support vs Information-only')

#### Modelsummary Attrition ####%


AttritionModel <- 
modelsummary(list("Reference \nmean"=AttritionControlMeans,"Differential \nAttrition"=modelAttritionStacked),
coef_map = cm,
fmt=fmt_statistic(estimate=2, 
                  adj.p.value=3,
                  std.error=2,
                  conf.int=2,
                  "Chi 2"=2,
                  "P-value"=3), 
estimate = '{estimate} ({std.error})',
statistic = c("p = {p.value}","conf.int",
              "adj.p.val. = {adj.p.value}"),
stars=FALSE,
#stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c(
  "Covariates","Fixed effects","Chi 2","DF","P-value",
  "nobs", "r.squared","adj.r.squared"),
#title="Intention-to-treat effects on the main outcomes",
#  notes=paste("Sources:", SourcesStacked,"      
#Each column jointly estimates the average differences between arms using fully-saturated stacked OLS regressions and inverse propensity score weights. Control means estimated separately by OLS.
#Standard errors in parentheses are cluster-heteroskedasticity robust (CR2) adjusted at the block level.
#Simultaneous inference procedures - implemented via general linear hypothesis testing (glht) - control for multiple comparisons with two-sided tests using Westfall adjustment. 
#Adjusted p-values and simultaneous confidence intervals in bracklets maintain familywise error rate at 5%. Joint chi-square test evaluates global treatment significance."),
output = 'flextable') 



Table_7 <- 
  AttritionModel %>% 
  merge_at(j=1,i=c(1:3),part="body")|>   
  merge_at(j=1,i=c(5:7),part="body")|>   
  merge_at(j=1,i=c(9:11),part="body") |>   
 # merge_at(j=1,i=c(13:14),part="body") |>   
  #theme_booktabs()|>
#  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
#  merge_at(j=1,i=c(1:2),part="header") %>% 
  #merge_v(j = 1,part="header") %>% 
#  italic(i = c(2),  part = "header") %>% 
  bold(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
 # width(j=c(2,3,4,5),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  hline(c(12),part="body") %>% 
  # Line spacing & padding
  padding(padding = 2, part = "all") %>%
  bg(i =  c(1,5,9), bg = "lightgrey",j = c(2:3)) %>% 
  line_spacing(space = 1)# %>%

# Autofit and width (single-column: 8.8cm = ~3.46in)
#autofit() %>%
# width(width = 3.46, unit = "in") #%>%

Table_7



save_as_docx(Table_7,path = "Tables/Table_7.docx")



#------ ActivityReduction ------------


Activity.ITT <-  ITTSimultaneous(Y="WorkMotherReduced", DB = PostDBT2 %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))))

Activity.LATE <-  LATESimultaneous(Y="WorkMotherReduced", DB = PostDBT2 %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))))

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.

activity.ITT <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))),
                                         Outcome = "WorkMotherReduced",
                                         Heterogeneity = "Educ2",
                                         ITT = TRUE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

activity.ATT <- GroupHeterogeneityFnCTRL(DB = PostDBT2%>% mutate(WorkMotherReduced = as.numeric(ifelse(WorkMotherReduced == TRUE, 1, 0))),
                                         Outcome = "WorkMotherReduced",
                                         Heterogeneity = "Educ2",
                                         ITT = FALSE,
                                         Weights = "WeightPS",
                                         clusters = "StrataWave")

# Select the relevant coef
activity.ITT$ModelSummary0$tidy <- activity.ITT$ModelSummary0$tidy  #%>% filter(term == "T2-C")
activity.ITT$ModelSummary$tidy <- activity.ITT$ModelSummary$tidy    #%>% filter(term == "T2-C")
activity.ATT$ModelSummary0$tidy <- activity.ATT$ModelSummary0$tidy  #%>% filter(term == "T2-C")

cm <- c('T2-C' = 'Information + Support vs Control',
        'T2-T1' = 'Information + Support vs Information only')

FigAct <- modelsummary(list("Reduced their activity_Control mean"  =activity.ITT$ModelSummary0,
                  "Reduced their activity_ITT"           =activity.ITT$ModelSummary,
                  "Reduced their activity_ATT"           =activity.ATT$ModelSummary),
             shape = term + Group ~ model,
#shape= Variable + Group ~ model,
fmt=fmt_statistic(estimate=2, 
                  adj.p.value=3,
                  std.error=2,
                  conf.int=2,
                  "Chi 2"=2,
                  "P-value"=3), 
estimate = '{estimate} ({std.error})',
statistic = c("p = {p.value}","conf.int",
              "adj.p.val. = {adj.p.value}"),
stars=FALSE,
#stars = c('*' = .1,'**' = .05, '***' = .01),
 gof_map = c(
   "Covariates","Fixed effects","Chi 2","DF","P-value",
   "nobs", "r.squared","adj.r.squared"),
coef_map = cm,
output = 'flextable'
#gof_map = c('Fixed effects',"N"),
#                         title=TheTitle,
#                         notes=paste("Sources:", SourcesStacked,
#                                     "
#Standard errors are cluster-heteroskedasticity robust adjusted at the block level.
#Models are jointly estimating conditional averages in each pair of treatment arm.
#Adjusted p-values and confidence intervals account for simultaneous inference across treatment arms.
#                         " ),
)

Table6 <- FigAct %>%   theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_v(j=1,part="body")|>
  #merge_v(j=2, part="body")|>
  merge_at(j=1,i=1:8,part="body")|>
  merge_at(j=1,i=9:16,part="body")|>
  merge_at(j=2,i=1:4,part="body")|>
  merge_at(j=2,i=5:8,part="body")|>
  merge_at(j=2,i=9:12,part="body")|>
  merge_at(j=2,i=13:16,part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm") %>% 
  hline(4*c(1,2,3,4),part="body") %>% 
  bg(i =  c(1,5,9,13), bg = "lightgrey",j = c(3:5)) 

Table6

save_as_docx(Table6,path = "Tables/Table_6.docx")



#------ MechanismsNewcomers ------------

##ever used early childcare
# App itt              

Het.ITT.App.Use <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "UsedECEC",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.App.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ITT.Use.Use <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "UsedECEC",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ITT.Use.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = TRUE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

#test <- CompareCoef(Het.ATT.UseCreche)


# App Att

Het.ATT.App.Use <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSApp",
                                            Heterogeneity = "UsedECEC",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.App.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "AppCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")

Het.ATT.Use.Use <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                            Outcome = "ECSUseYes",
                                            Heterogeneity = "UsedECEC",
                                            ITT = FALSE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")

Het.ATT.Use.Use.Daycare <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                    Outcome = "UseCreche",
                                                    Heterogeneity = "UsedECEC",
                                                    ITT = FALSE,
                                                    Weights = "WeightPS",
                                                    clusters = "StrataWave")





cm <- c('T2-C'    = 'Information + Support vs Control')

## EARLY CHILDCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Use$ModelSummary0$tidy= Het.ITT.App.Use$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Use$ModelSummary$tidy= Het.ATT.App.Use$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Use$ModelSummary0$tidy= Het.ITT.Use.Use$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Use$ModelSummary$tidy= Het.ATT.Use.Use$ModelSummary$tidy %>% filter(term == "T2-C")

Table_8 <- 
modelsummary(list("Early childcare application_Control mean"  =Het.ITT.App.Use$ModelSummary0,
                  "Early childcare application_ITT"           =Het.ITT.App.Use$ModelSummary,
                  "Early childcare application_ATT"           =Het.ATT.App.Use$ModelSummary,
                  "Early childcare access_Control mean"          =Het.ITT.Use.Use$ModelSummary0,
                  "Early childcare access_ITT"                   =Het.ITT.Use.Use$ModelSummary,
                  "Early childcare access_ATT"                   =Het.ATT.Use.Use$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, 
                               adj.p.value=3,
                               std.error=2,
                               conf.int=2,
                               "Chi 2"=2,
                               "P-value"=3), 
             estimate = '{estimate} ({std.error})',
             statistic = c("p = {p.value}","conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = FALSE,
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
#             title="Average effects on application and access to early childcare by past early childcare usage",
#             notes=paste("Sources:", SourcesStacked,
#                         "
#Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
#Adjusted p-value and confidence intervals account for simultaneous inference. 
#Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_at(j=1,i=c(1:8),part="body")|>
  merge_at(j=2,i=c(1:4),part="body")|>
  merge_at(j=2,i=c(5:8),part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(4,8),part="body") %>% 
  bg(i =  c(1,5), bg = "lightgrey",j = c(3:8)) 

Table_8
save_as_docx(Table_8,path = "Tables/Table_8.docx")

## DAYCARE
# filter only the T2-C term for the ITT and ATT
Het.ITT.App.Use.Daycare$ModelSummary0$tidy= Het.ITT.App.Use.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.App.Use.Daycare$ModelSummary$tidy = Het.ATT.App.Use.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")
Het.ITT.Use.Use.Daycare$ModelSummary0$tidy= Het.ITT.Use.Use.Daycare$ModelSummary0$tidy %>% filter(term == "T2-C")
Het.ATT.Use.Use.Daycare$ModelSummary$tidy = Het.ATT.Use.Use.Daycare$ModelSummary$tidy %>% filter(term == "T2-C")

Table9 <- 
modelsummary(list("Daycare application_Control mean"     =Het.ITT.App.Use.Daycare$ModelSummary0,
                  "Daycare application_ITT"              =Het.ITT.App.Use.Daycare$ModelSummary,
                  "Daycare application_ATT"              =Het.ATT.App.Use.Daycare$ModelSummary,
                  "Daycare access_Control mean"          =Het.ITT.Use.Use.Daycare$ModelSummary0,
                  "Daycare access_ITT"                   =Het.ITT.Use.Use.Daycare$ModelSummary,
                  "Daycare access_ATT"                   =Het.ATT.Use.Use.Daycare$ModelSummary),
             shape = term + Group ~ model,
             fmt=fmt_statistic(estimate=2, 
                               adj.p.value=3,
                               std.error=2,
                               conf.int=2,
                               "Chi 2"=2,
                               "P-value"=3), 
             estimate = '{estimate} ({std.error})',
             statistic = c("p = {p.value}","conf.int",
                           "adj.p.val. = {adj.p.value}"),
             stars = FALSE,
             coef_map = cm,
             gof_map = c(#"Mean of DV",
               "Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
               "nobs", "r.squared","adj.r.squared"),
#             title="Average effects on application and access to daycare by past early childcare usage",
#             notes=paste("Sources:", SourcesStacked,
#                         "
#Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
#Adjusted p-value and confidence intervals account for simultaneous inference. 
#Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
             output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  bold(i=1,  part = "header") %>%                # Variable labels bold
  merge_at(j=2,part="header")|>
  merge_at(j=1,part="header")|>
  merge_at(j=1,i=c(1:8),part="body")|>
  merge_at(j=2,i=c(1:4),part="body")|>
  merge_at(j=2,i=c(5:8),part="body")|>
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(4,5,7,8),width=2.4,unit = "cm")|>
  width(j=c(1,2,3,6),width=2,unit = "cm") %>% 
  hline(c(4,8),part="body") %>% 
  bg(i =  c(1,5), bg = "lightgrey",j = c(3:8)) 


Table9
save_as_docx(Table9,path = "Tables/Table_9.docx")


#------ MechanismsActivexSES------------

# to trick the function into making intersection treatment effects, you can simply create the interaction in the database entry you have there and then everything should work nicely.
  



