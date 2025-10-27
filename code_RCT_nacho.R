

### -------- Activation / Conversion ####

##### ------ Descriptive Table  #######

CrossTable <- MainDB %>%
  mutate(
    Category = case_when(
      ECSPlanToBaseline == TRUE  & ECSApp == 1 ~ "Activation (voulaient + ont appliqué)",
      ECSPlanToBaseline == TRUE  & ECSApp == 0 ~ "Voulaient + n'ont pas appliqué",
      ECSPlanToBaseline == FALSE & ECSApp == 1 ~ "Conversion (ne voulaient pas + ont appliqué)",
      ECSPlanToBaseline == FALSE & ECSApp == 0 ~ "Ne voulaient pas + n'ont pas appliqué",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Assignment, Category) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Assignment) %>%
  mutate(
    pct = n / sum(n),
    pct = scales::percent(pct, accuracy = 0.1)
  )

print(CrossTable, n = Inf)


## Summary = taux de conversion et activation selon le bras  
AppliedSummary <- MainDB %>%
  filter(ECSApp == 1) %>%
  mutate(
    Category = ifelse(ECSPlanToBaseline == TRUE, "Activation", "Conversion")
  ) %>%
  group_by(Assignment, Category) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Assignment) %>%
  mutate(
    pct = n / sum(n),
    pct = scales::percent(pct, accuracy = 0.1)
  )

print(AppliedSummary, n = Inf)
#Comment -> taux de activation semble nettement plus élevé pour tout les groupes



###### ---------- HET intention baseline ####
Het.ITT.App.Plan <- GroupHeterogeneityFnCTRL(
  DB = PostDB,
  Outcome = "ECSApp",
  Heterogeneity = "ECSPlanToBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

# Récupérer les effets par sous-groupe
ITT_by_group <- Het.ITT.App.Plan$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# n familles selon intention baseline
SubgroupSizes <- PostDB %>%
  group_by(ECSPlanToBaseline) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))

# Joindre aux estimés ITT
ITT_by_group <- ITT_by_group %>%
  left_join(SubgroupSizes, by = c("Group" = "ECSPlanToBaseline"))
print(ITT_by_group)

# Comment -> Parmi les familles qui voulaient déjà un mode d’accueil, le traitement augmente significativement la proba de candidater de +4,8 points de pourcentage (p = 0,025).
#.           En revanche, parmi les familles qui ne souhaitaient pas de mode d’accueil, l’effet est non significatif (p = 0,83).


## ATT
Het.ATT.App.Plan <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2,
  Outcome = "ECSApp",
  Heterogeneity = "ECSPlanToBaseline",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

# recup effets par sous groupes
ATT_by_group <- Het.ATT.App.Plan$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# Décomposition pondérée ATT
ATT_by_group <- ATT_by_group %>%
  left_join(SubgroupSizes, by = c("Group" = "ECSPlanToBaseline"))

print(ATT_by_group)
#Comment -> Parmi les compliers : l’effet est + important pour les familles intéressées en baseline (+8,4 points, p = 0,025).
#.          L’effet estimé pour les familles non intéressées est non significatif (p = 0,83).


###### ------------------ Graph Results ----------

term_levels <- c("T2-C")
heterogeneity_levels <- c("Baseline intention")
panel_levels <- c("Control group", "ITT", "ATT")

# ITT
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Plan$ModelSummary0$tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "Control group",
           Heterogeneity = "Baseline intention",
           Type = "ITT") %>%
    filter(term %in% term_levels),
  
  Het.ITT.App.Plan$Tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "ITT",
           Heterogeneity = "Baseline intention",
           Type = "ITT") %>%
    filter(term %in% term_levels)
)

# ATT
DataPlot_ATT <- Het.ATT.App.Plan$Tidy %>%
  mutate(Y = "Apply for early childcare",
         panel = "ATT",
         Heterogeneity = "Baseline intention",
         Type = "ATT") %>%
  filter(term %in% term_levels)

# Combine ITT + ATT
Data.Het.Plan <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

# Graphique

ggplot(Data.Het.Plan)+
  geom_pointrange(aes(
    x = interaction(Heterogeneity, Group, sep="!"), # Baseline intention (TRUE/FALSE)
    y = estimate,
    ymin = point.conf.low,
    ymax = point.conf.high,
    shape = Group,
    color = Group
  ), position = position_dodge(.4)) +
  
  geom_crossbar(aes(
    y = estimate, x = interaction(Heterogeneity, Group, sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  
  facet_grid(rows = vars(fct_rev(Y)), cols = vars(panel), scale = "free_x") +
  coord_flip() +
  geom_hline(data = Data.Het.Plan %>% filter(panel!="Control group"),
             aes(yintercept = 0), linetype = 2) +
  xlab("") +
  scale_x_discrete(guide = guide_axis_nested(delim = "!")) +
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2") +
  scale_shape("Heterogeneity") +
  labs(
    caption = paste(
      "Sources:", SourcesStacked,
      "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
      "\nStandard errors are cluster-robust at block level.",
      "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
      "\nBoxes = simultaneous 95% CI (Westfall-Young correction).",
      "\nAll models include block fixed effects."
    )
  ) + vis_theme




### ------- Concentration de l'activation ####

# Après voir que le traitement a globalement activé plutot que converti, voir si cette activation est
# concentrée sur des personnes qui n'avaient pas utilisé des modes d'acceuil auparavant


# Base restreinte: familles qui voulaient un mode d'accueil
PostDB_wanted <- PostDB %>% filter(ECSPlanToBaseline == TRUE)
PostDBT2_wanted <- PostDBT2 %>% filter(ECSPlanToBaseline == TRUE)


##### ---------- HET "déjà utilisé un ECEC" conditionnel à désirer un ECEC -------
Het.ITT.App.Used_Wanted <- GroupHeterogeneityFnCTRL(
  DB = PostDB_wanted,
  Outcome = "ECSApp",
  Heterogeneity = "UsedECEC",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

Het.ATT.App.Used_Wanted <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2_wanted,
  Outcome = "ECSApp",
  Heterogeneity = "UsedECEC",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

# Extraire ITT par sous-groupe
ITT_by_Used_Wanted <- Het.ITT.App.Used_Wanted$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# Extraire ATT par sous-groupe
ATT_by_Used_Wanted <- Het.ATT.App.Used_Wanted$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

print(ITT_by_Used_Wanted)
print(ATT_by_Used_Wanted)

# Group sizes
SubgroupSizes_Used <- PostDB %>%
  filter(ECSPlanToBaseline == TRUE) %>%
  group_by(UsedECEC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))

# Joindre ITT avec tailles de sous-groupes
Results_Used <- SubgroupSizes_Used %>%
  left_join(ITT_by_Used_Wanted, by = c("UsedECEC" = "Group")) %>%
  rename(ITT_estimate = estimate,
         ITT_se = std.error,
         ITT_pval = p.value) %>%
  left_join(ATT_by_Used_Wanted, by = c("UsedECEC" = "Group")) %>%
  rename(ATT_estimate = estimate,
         ATT_se = std.error,
         ATT_pval = p.value)

print(Results_Used)
#Commentaire -> Pour ceux qui voulaient un mode de garde en baseline,
#.              le traitement a eu un effet sur les novices et pas sur ceux qui 
#               avaient déjà utlisé des modes d'accueil.



#explorer pourquoi les gens qui n'avaient jamais utilisé de mode de garde ont tendance à + candidater. 



###### ------------------ Graph Results ----------

term_levels <- c("T2-C")
heterogeneity_levels <- c("Past use of childcare")
panel_levels <- c("Control group", "ITT", "ATT")

# ITT
DataPlot_ITT <- bind_rows(
  Het.ITT.App.Used_Wanted$ModelSummary0$tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "Control group",
           Heterogeneity = "Past use of childcare",
           Type = "ITT") %>%
    filter(term %in% term_levels),
  
  Het.ITT.App.Used_Wanted$Tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "ITT",
           Heterogeneity = "Past use of childcare",
           Type = "ITT") %>%
    filter(term %in% term_levels)
)

# ATT
DataPlot_ATT <- Het.ATT.App.Used_Wanted$Tidy %>%
  mutate(Y = "Apply for early childcare",
         panel = "ATT",
         Heterogeneity = "Past use of childcare",
         Type = "ATT") %>%
  filter(term %in% term_levels)

# Combine ITT + ATT
Data.Het.Used <- bind_rows(DataPlot_ITT, DataPlot_ATT) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )

ggplot(Data.Het.Used)+
  geom_pointrange(aes(
    x = interaction(Heterogeneity, Group, sep="!"), # Already used / Never used
    y = estimate,
    ymin = point.conf.low,
    ymax = point.conf.high,
    shape = Group,
    color = Group
  ), position = position_dodge(.4)) +
  
  geom_crossbar(aes(
    y = estimate, x = interaction(Heterogeneity, Group, sep="!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  
  facet_grid(rows = vars(fct_rev(Y)), cols = vars(panel), scale = "free_x") +
  coord_flip() +
  geom_hline(data = Data.Het.Used %>% filter(panel!="Control group"),
             aes(yintercept = 0), linetype = 2) +
  xlab("") +
  scale_x_discrete(guide = guide_axis_nested(delim = "!")) +
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2") +
  scale_shape("Heterogeneity") +
  labs(
    caption = paste(
      "Sources:", SourcesStacked,
      "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
      "\nStandard errors are cluster-robust at block level.",
      "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
      "\nBoxes = simultaneous 95% CI (Westfall-Young correction).",
      "\nAll models include block fixed effects."
    )
  ) + vis_theme



##### --------- HET Info baseline ----------


# Voir selon une autre mesure du niveau d'infiormation en baseline. Effet du traitement en fonction
# du niveau d'info en baseline pour ceux qui désiraient un mode d'accueil en baseline


# Heterogeneity selon info en baseline conditionnel à désirer un ECEC
Het.ITT.App.Used_info <- GroupHeterogeneityFnCTRL(
  DB = PostDB_wanted,
  Outcome = "ECSApp",
  Heterogeneity = "LevelInfoSubExPost",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

Het.ATT.App.Used_info <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2_wanted,
  Outcome = "ECSApp",
  Heterogeneity = "LevelInfoSubExPost",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

# Extraire ITT par sous-groupe
ITT_by_Used_info <- Het.ITT.App.Used_info$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# Extraire ATT par sous-groupe
ATT_by_Used_info <- Het.ATT.App.Used_info$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

print(ITT_by_Used_info)
print(ATT_by_Used_info)

#Comment -> Activation concentrée chez les mères les moins informées. +27,3pp de candidater à un mode d'accueil. +34,5pp chez les compliers

###### ------ Graph Results --------

term_levels <- c("T2-C")
heterogeneity_levels <- c("Information level")
panel_levels <- c("Control group", "ITT", "ATT")

# ITT 
DataPlot_ITT_info <- bind_rows(
  Het.ITT.App.Used_info$ModelSummary0$tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "Control group",
           Heterogeneity = "Information level",
           Type = "ITT") %>%
    filter(term %in% term_levels),
  
  Het.ITT.App.Used_info$Tidy %>%
    mutate(Y = "Apply for early childcare",
           panel = "ITT",
           Heterogeneity = "Information level",
           Type = "ITT") %>%
    filter(term %in% term_levels)
)

# ATT 
DataPlot_ATT_info <- Het.ATT.App.Used_info$Tidy %>%
  mutate(Y = "Apply for early childcare",
         panel = "ATT",
         Heterogeneity = "Information level",
         Type = "ATT") %>%
  filter(term %in% term_levels)

# Combine ITT + ATT 
Data.Het.Info <- bind_rows(DataPlot_ITT_info, DataPlot_ATT_info) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
    Group = fct_relevel(Group, "Aucun ou très bas", "Basique", "Bonne", "Excellente") # pour l’ordre
  )

# Graphique 
ggplot(Data.Het.Info) +
  geom_pointrange(aes(
    x = interaction(Heterogeneity, Group, sep = "!"),
    y = estimate,
    ymin = point.conf.low,
    ymax = point.conf.high,
    shape = Group,
    color = Group
  ), position = position_dodge(.4)) +
  
  geom_crossbar(aes(
    y = estimate, 
    x = interaction(Heterogeneity, Group, sep = "!"),
    fill = Group, 
    ymin = conf.low,
    ymax = conf.high,
    color = Group
  ), position = position_dodge(.6), alpha = .2, middle.linewidth = 2, width = .4) +
  
  facet_grid(rows = vars(fct_rev(Y)), cols = vars(panel), scale = "free_x") +
  coord_flip() +
  geom_hline(data = Data.Het.Info %>% filter(panel != "Control group"),
             aes(yintercept = 0), linetype = 2) +
  xlab("") +
  scale_x_discrete(guide = guide_axis_nested(delim = "!")) +
  scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  scale_color_brewer("Heterogeneity", palette = "Dark2") +
  scale_shape("Heterogeneity") +
  labs(
    caption = paste(
      "Sources:", SourcesStacked,
      "\nITT: Intention to Treat estimates; ATT: Average Treatment on the Treated estimates.",
      "\nStandard errors are cluster-robust at block level.",
      "\nPoints indicate point estimates and the error bars indicate pointwise 95% CI.",
      "\nBoxes = simultaneous 95% CI (Westfall-Young correction).",
      "\nAll models include block fixed effects."
    )
  ) + vis_theme



### -------- Mode accueil voulu / eu #####

# Harmoniser variables baseline et endline pour les matcher

# Mode de garde voulu baseline

MainDB <- MainDB %>%
  mutate(
    ECSType7_recoded = case_when(
      ECSType7 %in% "Crèches"               ~ "Crèches",
      ECSType7 %in% "Assistantes maternelles" ~ "Assistantes maternelles",
      ECSType7 %in% "Nounou"                ~ "Nounou",
      ECSType7 %in% "Haltes garderies"      ~ "Haltes garderies",
      ECSType7 %in% c("Autre", "Jardin d’enfant", "NSP") ~ "Autre",
      is.na(ECSType7) & ECSPlanToBaseline == FALSE ~ "Aucun mode",
      TRUE ~ NA_character_
    )
  )


# Mode de garde eu endline
MainDB <- MainDB %>%
  mutate(ECSType6_recoded = case_when(
    ECSType6Endline %in% c("Crèches Publiques", "Crèches Asso") ~ "Crèches",
    ECSType6Endline %in% c("Assistantes maternelles") ~ "Assistantes maternelles",
    ECSType6Endline %in% c("Nounou") ~ "Nounou",
    ECSType6Endline %in% c("Haltes garderies") ~ "Haltes garderies",
    ECSType6Endline %in% c("Autre") ~ "Autre",
    is.na(ECSType6Endline) ~ "Aucun mode",  #les NA
    TRUE ~ "Autre"
  ))


# Tableau mode d'accueil voulu selon SES
tab_baseline <- MainDB %>%
  filter(!is.na(ECSType7_recoded)) %>%
  count(Educ2, ECSType7_recoded) %>%
  group_by(Educ2) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

#Graphique
ggplot(tab_baseline, aes(x = ECSType7_recoded, y = prop, fill = Educ2)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Mode d’accueil voulu en baseline",
    y = "Proportion dans chaque groupe SES",
    fill = "SES",
    title = "Type de mode d’accueil voulu (baseline) selon SES"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Dark2")



# Créer variables croisant mode voulu et obtenu
MainDB <- MainDB %>%
  mutate(
    # ECSType7 = voulu baseline ; ECSType6 = obtenu endline
    got_wanted = as.integer(
      !is.na(ECSType7_recoded) & !is.na(ECSType6_recoded) &
        ECSType7_recoded != "Aucun mode" &
        ECSType6_recoded ==  ECSType7_recoded
    ),
    got_none_wanted = as.integer(
      ECSType7_recoded == "Aucun mode" & ECSType6_recoded == "Aucun mode"
    ),
    switched = as.integer(
      !is.na(ECSType7_recoded) & !is.na(ECSType6_recoded) &
        ECSType7_recoded != "Aucun mode" &
        ECSType6_recoded != "Aucun mode" &
        ECSType6_recoded !=  ECSType7_recoded
    ),
    got_none_unwanted = as.integer(  # voulait un mode, n'a eu aucun mode
      ECSType7_recoded != "Aucun mode" & !is.na(ECSType7_recoded) &
        ECSType6_recoded == "Aucun mode"
    ),
    got_one_unwanted = as.integer(   # (= got_mode_unwanted) ne voulait aucun mode, a eu un mode
      ECSType7_recoded == "Aucun mode" &
        ECSType6_recoded != "Aucun mode" & !is.na(ECSType6_recoded)
    )
  ) %>%
  mutate(
    outcome_match = case_when(
      got_wanted == 1        ~ "A eu le mode voulu",
      got_none_wanted == 1   ~ "Ne voulait aucun mode et n'en a pas eu",
      got_none_unwanted == 1 ~ "N'a eu aucun mode (alors qu'il en voulait un)",
      switched == 1          ~ "A switché vers un autre mode",
      got_one_unwanted == 1  ~ "A obtenu un mode sans en vouloir",
      TRUE                   ~ NA_character_
    )
  )



# Tableau "eu le mode voulu" et SES
CrossTab_outcomes <- MainDB %>%
  group_by(Educ2, outcome_match) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Educ2) %>%
  mutate(prop = n / sum(n))

print(CrossTab_outcomes, n = 30)


#Ajouter variables pour régressions
PostDB <- PostDB %>%
  left_join(MainDB %>% select(ResponseId, got_wanted, got_none_unwanted),
            by = "ResponseId")


# Régression got wanted x SES
lm_gotwanted <- lm_robust(
  got_wanted ~ Educ2 * Assignment,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_gotwanted)
# Comment -> mères low SES dans le groupe de contrôle ont proba de 11.9pp de moins d'avoir le mode d'accueil voulu en baseline, significatif au seuil de 5% (p value = 0,016). 
#            cela semble ne pas changer dans les groupe traités (interactions non significatives).


# Régression got wanted x SES
lm_got_non <- lm_robust(
  got_none_unwanted ~ Educ2 * Assignment,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_got_non)
# Comment -> mères low SES dans le groupe de contrôle ont proba de 14.3pp de plus de ne pas avoir eu de mode d'accueil alors qu'elles en voulaient un, significatif au seuil de 1% (p value = 0,002)
#.           traitement ne change pas ceci, cohérent car pas d'effet sur l'accès



# Maintenant on ajoute la variable "ECSGotIdealECS" dans l'analyse (déclarer en endline avoir le mode d'accueil idéal)


MainDB %>%
  group_by(Assignment, Educ2) %>%
  summarise(
    n = n(),
    prop_ideal = mean(as.integer(ECSGotIdealECS), na.rm = TRUE)
  )



# Régression gotideal x SES
lm_satisfied <- lm_robust(
  ECSGotIdealECS ~ Educ2 * Assignment,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_satisfied)
# Comment -> pas de différence significative entre SES dans le fait de déclarer avoir le mode idéal en endline



# Créer variable qui croise "eu le mode voulu" et "avoir le mode de garde idéal"
MainDB <- MainDB %>%
  mutate(
    got_wanted_ideal_cat = case_when(
      got_wanted == 1 & ECSGotIdealECS == TRUE  ~ "1. Wanted + Ideal",
      got_wanted == 1 & ECSGotIdealECS == FALSE ~ "2. Wanted + Not Ideal",
      got_wanted == 0 & ECSGotIdealECS == TRUE  ~ "3. Not Wanted + Ideal",
      got_wanted == 0 & ECSGotIdealECS == FALSE ~ "4. Not Wanted + Not Ideal",
      TRUE ~ NA_character_
    )
  )


# Tableau par SES
tab_plot <- MainDB %>%
  filter(!is.na(got_wanted_ideal_cat), !is.na(Educ2)) %>%
  group_by(Educ2, got_wanted_ideal_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Educ2) %>%
  mutate(share = n / sum(n))
print(tab_plot)


# Graphique
ggplot(tab_plot, aes(x = got_wanted_ideal_cat, y = share, fill = Educ2)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = ColorD) +
  labs(
    x = "Type de correspondance (objectif vs subjectif)",
    y = "Part dans chaque groupe SES",
    fill = "SES",
    title = "Correspondance entre mode voulu et mode jugé idéal, par SES"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

#Comment -> "Wanted + ideal": 30% familles High SES ont obtenu mode voulu + le jugent comme l'ideal
#                             contre 19% pour les Low SES
#           "Not Wanted + ideal": 9 points de % de plus chez les Low SES
#           "Not wanted + Not Ideal": plus fréquante chez low ses: 35% vs 29% pour high SES
# Interprétation possible (?): Mères classes favorisées + présentes dans catégorie idéale (wanted + ideal)
#                              même si échantillon faible, high SES + présentes catégorie "wanted + not ideal" (pourrait refléter un certain privilège?)
#                              Not wanted + ideal: low ses plus fréquantes, peut être ajustement des croyances?
#                              not wanted and not ideal: low SES plus représentés dans la catégorie qui révèle le plus d'insatisfaction




###### ------- HET SES ----------

Het.ITT.GotWanted <- GroupHeterogeneityFnCTRL(
  DB = PostDB,                 
  Outcome = "got_wanted",       
  Heterogeneity = "Educ2",    
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

#Add variable to the dataset
PostDBT2 <- PostDBT2 %>%
  left_join(MainDB %>% select(ResponseId, got_wanted),
            by = "ResponseId")

# ATT
Het.ATT.GotWanted <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2,               
  Outcome = "got_wanted",
  Heterogeneity = "Educ2",
  ITT = FALSE,                 
  Weights = "WeightPS",
  clusters = "StrataWave"
)


# ITT par SES
ITT_by_SES <- Het.ITT.GotWanted$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT par SES
ATT_by_SES <- Het.ATT.GotWanted$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

print(ITT_by_SES)
print(ATT_by_SES)

# Commentaire -> le traitement ne semble pas modifier la capacité d’obtenir effectivement le mode souhaité (malgré avoir activé)


###### ------- HET Migration ---------

# Tableau mode de garde voulu x Migration
MainDB %>%
  group_by(Assignment, FrenchYNBaseline) %>%
  summarise(
    n = n(),
    mean_got_wanted = mean(got_wanted, na.rm = TRUE)
  )
#Comment -> les mères qui sont nées en France matchent plus souvent (≈ 15 points de plus)


Het.ITT.GotWanted_M <- GroupHeterogeneityFnCTRL(
  DB = PostDB,                  
  Outcome = "got_wanted",       
  Heterogeneity = "FrenchYNBaseline",    
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

Het.ATT.GotWanted_M <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2,               
  Outcome = "got_wanted",
  Heterogeneity = "FrenchYNBaseline",
  ITT = FALSE,                 
  Weights = "WeightPS",
  clusters = "StrataWave"
)


# ITT par SES
ITT_by_migration <- Het.ITT.GotWanted_M$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT par SES
ATT_by_migration <- Het.ATT.GotWanted_M$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

print(ITT_by_migration)
print(ATT_by_migration)
#Comment -> Le traitement n'a pas modifié la capacité à avoir le mode d'accueil voulu selon l'origine de la mère






###### ------ Alluvial plot #########

library(ggalluvial)


# Groupe Controle

alluvial_controls <- MainDB %>%
  filter(Assignment == "Control") %>%
  filter(!is.na(ECSType7_recoded) & !is.na(ECSType6_recoded)) %>%
  select(ECSType7_recoded, ECSType6_recoded)


# reformat la base pour des axes + claires sur le graph
alluvial_controls_agg <- alluvial_controls %>%
  group_by(ECSType7_recoded, ECSType6_recoded) %>%
  summarise(Freq = n(), .groups = "drop")


# afficher les flux sur le graph
label_data__ <- alluvial_controls_agg %>%
  group_by(ECSType7_recoded) %>%
  arrange(ECSType7_recoded, ECSType6_recoded) %>%  #
  mutate(
    cumFreq = cumsum(Freq),
    y_label = cumFreq - Freq / 2,  # vertical center of each flow
    x_label = 1.5                  # entre baseline et endline 
  ) %>%
  ungroup()
print(label_data__)


# Graphique
ggplot(alluvial_controls_agg,
       aes(axis1 = ECSType7_recoded,
           axis2 = ECSType6_recoded,
           y = Freq)) +
  
  geom_alluvium(aes(fill = ECSType7_recoded), width = 1/12, alpha = 0.7) +
  
  geom_stratum(width = 1/12, fill = "gray95", color = "gray80") +
  
  geom_text(
    stat = "stratum",
    aes(label = paste(after_stat(stratum), "\n", after_stat(count))),
    size = 3.5
  ) +
  
  geom_text(
    data = label_data__ %>% filter(Freq >= 25),
    aes(x = x_label, y = y_label, label = Freq),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  scale_x_discrete(
    limits = c("Desired care (Baseline)", "Actual care (Endline)"),
    expand = c(0.15, 0.15)
  ) +
  
  labs(
    title = "Trajectories between desired and actual childcare (Controls)",
    x = "Stage",
    y = "Number of families"
  ) +
  theme_minimal()
#Comment -> Seul 168 des 413 qui voulaient un crèche en baseline l'ont eu.
#.          189 vont dans aucun mode
#           Assistant maternelle semble être l'atlernative la plus choisie (après crèche et "aucun mode") 




### ------- Traitement et type de crèche candidatée et eu ######

# Rappel: Le traitement augmente les candidatures à la crèche pour low SES et mères nées à l'étranger, mais accès semble uniquement augmenter pour les mère issues de l'immigration.
# Hypothèse : Pourquoi? ceci pourrait renforcer l'hypothèse des critères d'admission, car mères inactives plus présentes chez low SES que chez mères étrangères.


## Extraire les coefficients du HET sur Daycare

## APPLICATIONS

# ITT — by SES (Educ2)
ITT_by_SES_app <- Het.ITT.AppCreche.Educ2C$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ITT — by Migration
ITT_by_Mig_app <- Het.ITT.AppCreche.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT — by SES (Educ2)
ATT_by_SES_app <- Het.ATT.AppCreche.Educ2C$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT — by Migration
ATT_by_Mig_app <- Het.ATT.AppCreche.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)


combined_app <- bind_rows(
  ITT_by_SES_app  %>% mutate(estimand = "ITT", hetero = "SES"),
  ITT_by_Mig_app  %>% mutate(estimand = "ITT", hetero = "Migration"),
  ATT_by_SES_app  %>% mutate(estimand = "ATT", hetero = "SES"),
  ATT_by_Mig_app  %>% mutate(estimand = "ATT", hetero = "Migration")
) %>%
  relocate(estimand, hetero)

print(combined_app)



## ACCESS

# ITT — by SES (Educ2)
ITT_by_SES_use <- Het.ITT.UseCreche.Educ2C$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ITT — by Migration
ITT_by_Mig_use <- Het.ITT.UseCreche.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT — by SES (Educ2)
ATT_by_SES_use <- Het.ATT.UseCreche.Educ2C$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT — by Migration
ATT_by_Mig_use <- Het.ATT.UseCreche.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)


combined_use <- bind_rows(
  ITT_by_SES_use  %>% mutate(estimand = "ITT", hetero = "SES"),
  ITT_by_Mig_use  %>% mutate(estimand = "ITT", hetero = "Migration"),
  ATT_by_SES_use  %>% mutate(estimand = "ATT", hetero = "SES"),
  ATT_by_Mig_use  %>% mutate(estimand = "ATT", hetero = "Migration")
) %>%
  relocate(estimand, hetero)

print(combined_use)
# Comment -> treatment increases probability of access by 10.5 points for immigrants
#            and up to 17.5pp for compliers in the migration = Yes subgroup



###### ----------- Taux APP x ECS x Bras ---------


## Candidatures

# SES
table_candidatures_SES <- MainDB %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    p_crecheA = mean(AppCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(AppCrechePrivee, na.rm = TRUE),
    p_crechepub = mean(AppCrechePub, na.rm = TRUE),
    p_microcreche = mean(AppMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(AppParentale, na.rm = TRUE),
    p_familiale = mean(AppFamiliale, na.rm = TRUE),
    p_assmat = mean(AppAssMat, na.rm = TRUE),
    p_nounou = mean(AppNounou, na.rm = TRUE),
    p_MAM = mean(AppMAM, na.rm = TRUE),
    p_halte = mean(AppHG, na.rm = TRUE),
    p_partage = mean(AppPartage, na.rm = TRUE),
    p_hospitaliere = mean(AppHosp, na.rm = TRUE),
    p_autre = mean(AppAutre, na.rm = TRUE))
print(table_candidatures_SES)


# Migration Background
table_candidatures_migration <- MainDB %>%
  group_by(MigrationBackground, Assignment) %>%
  summarise(
    p_crecheA = mean(AppCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(AppCrechePrivee, na.rm = TRUE),
    p_crechepub = mean(AppCrechePub, na.rm = TRUE),
    p_microcreche = mean(AppMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(AppParentale, na.rm = TRUE),
    p_familiale = mean(AppFamiliale, na.rm = TRUE),
    p_assmat = mean(AppAssMat, na.rm = TRUE),
    p_nounou = mean(AppNounou, na.rm = TRUE),
    p_MAM = mean(AppMAM, na.rm = TRUE),
    p_halte = mean(AppHG, na.rm = TRUE),
    p_partage = mean(AppPartage, na.rm = TRUE),
    p_hospitaliere = mean(AppHosp, na.rm = TRUE),
    p_autre = mean(AppAutre, na.rm = TRUE))
print(table_candidatures_migration)



## Accès

#SES
table_accès_SES <- MainDB %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    p_crecheA = mean(UseCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(UsePrivée, na.rm = TRUE),
    p_crechepub = mean(UseCrechePub, na.rm = TRUE),
    p_microcreche = mean(UseMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(UseParentale, na.rm = TRUE),
    p_familiale = mean(UseFamiliale, na.rm = TRUE),
    p_assmat = mean(UseAssMat, na.rm = TRUE),
    p_nounou = mean(UseNounou, na.rm = TRUE),
    p_MAM = mean(UseMAM, na.rm = TRUE),
    p_halte = mean(UseHG, na.rm = TRUE),
    p_partage = mean(UsePartage, na.rm = TRUE),
    p_hospitaliere = mean(UseHosp, na.rm = TRUE),
    p_autre = mean(UseAutre, na.rm = TRUE))
print(table_accès_SES)


# Migrations
table_accès_migration <- MainDB %>%
  group_by(MigrationBackground, Assignment) %>%
  summarise(
    p_crecheA = mean(UseCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(UsePrivée, na.rm = TRUE),
    p_crechepub = mean(UseCrechePub, na.rm = TRUE),
    p_microcreche = mean(UseMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(UseParentale, na.rm = TRUE),
    p_familiale = mean(UseFamiliale, na.rm = TRUE),
    p_assmat = mean(UseAssMat, na.rm = TRUE),
    p_nounou = mean(UseNounou, na.rm = TRUE),
    p_MAM = mean(UseMAM, na.rm = TRUE),
    p_halte = mean(UseHG, na.rm = TRUE),
    p_partage = mean(UsePartage, na.rm = TRUE),
    p_hospitaliere = mean(UseHosp, na.rm = TRUE),
    p_autre = mean(UseAutre, na.rm = TRUE))
print(table_accès_migration)



# SES: join app & access and compute conversion = access/app
conv_SES <- table_candidatures_SES %>%
  inner_join(table_accès_SES,
             by = c("Educ2","Assignment"),
             suffix = c("_app","_use")) %>%
  transmute(
    Educ2, Assignment,
    conv_pub        = p_crechepub_use        / p_crechepub_app,
    conv_associative= p_crecheA_use          / p_crecheA_app,
    conv_privee     = p_crechePrivee_use     / p_crechePrivee_app,
    conv_micro      = p_microcreche_use      / p_microcreche_app
  )

# Same for Migration
conv_mig <- table_candidatures_migration %>%
  inner_join(table_accès_migration,
             by = c("MigrationBackground","Assignment"),
             suffix = c("_app","_use")) %>%
  transmute(
    MigrationBackground, Assignment,
    conv_pub        = p_crechepub_use        / p_crechepub_app,
    conv_associative= p_crecheA_use          / p_crecheA_app,
    conv_privee     = p_crechePrivee_use     / p_crechePrivee_app,
    conv_micro      = p_microcreche_use      / p_microcreche_app
  )

print(conv_SES)
print(conv_mig)



###### --- Profit vs Non Profit ---------

#joindre les non profit pour voir quel type de structure drive l'accès pour les migrants

# 1) Coder les variables
MainDB <- MainDB %>%
  mutate(
    # Applications
    App_nonprofit = as.integer(
      (coalesce(AppCrechePub,0L)==1L) |
        (coalesce(AppCrecheA,0L)==1L) |
        (coalesce(AppFamiliale,0L)==1L) |
        (coalesce(AppParentale,0L)==1L) |
        (coalesce(AppHosp,0L)==1L)
    ),
    App_profit = as.integer(
      (coalesce(AppCrechePrivee,0L)==1L) |
        (coalesce(AppMicroCreche,0L)==1L)
    ),
    
    # Access 
    Use_nonprofit = as.integer(
      (coalesce(UseCrechePub,0L)==1L) |
        (coalesce(UseCrecheA,0L)==1L) |
        (coalesce(UseFamiliale,0L)==1L) |
        (coalesce(UseParentale,0L)==1L) |
        (coalesce(UseHosp,0L)==1L)
    ),
    Use_profit = as.integer(
      (coalesce(`UsePrivée`,0L)==1L) |
        (coalesce(UseMicroCreche,0L)==1L)
    )
  )

# 2) SES × Assignment

# Applications
apps_SES <- MainDB %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    n = n(),
    app_nonprofit = mean(App_nonprofit),
    app_profit    = mean(App_profit),
    .groups = "drop"
  ) %>%
  mutate(
    app_nonprofit = round(app_nonprofit*100, 1),
    app_profit    = round(app_profit*100, 1)
  )

# Access
access_SES <- MainDB %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    n = n(),
    use_nonprofit = mean(Use_nonprofit),
    use_profit    = mean(Use_profit),
    .groups = "drop"
  ) %>%
  mutate(
    use_nonprofit = round(use_nonprofit*100, 1),
    use_profit    = round(use_profit*100, 1)
  )

# Conversion (access | applied)
conv_SES <- MainDB %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    conv_nonprofit = ifelse(sum(App_nonprofit)>0,
                            sum(Use_nonprofit==1 & App_nonprofit==1)/sum(App_nonprofit==1),
                            NA_real_),
    conv_profit    = ifelse(sum(App_profit)>0,
                            sum(Use_profit==1 & App_profit==1)/sum(App_profit==1),
                            NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    conv_nonprofit = round(conv_nonprofit*100, 1),
    conv_profit    = round(conv_profit*100, 1)
  )

# 3) Migration × Assignment

# Applications
apps_MIG <- MainDB %>%
  group_by(MigrationBackground, Assignment) %>%
  summarise(
    n = n(),
    app_nonprofit = mean(App_nonprofit),
    app_profit    = mean(App_profit),
    .groups = "drop"
  ) %>%
  mutate(
    app_nonprofit = round(app_nonprofit*100, 1),
    app_profit    = round(app_profit*100, 1)
  )

# Access
access_MIG <- MainDB %>%
  group_by(MigrationBackground, Assignment) %>%
  summarise(
    n = n(),
    use_nonprofit = mean(Use_nonprofit),
    use_profit    = mean(Use_profit),
    .groups = "drop"
  ) %>%
  mutate(
    use_nonprofit = round(use_nonprofit*100, 1),
    use_profit    = round(use_profit*100, 1)
  )

# Conversion (access | applied)
conv_MIG <- MainDB %>%
  group_by(MigrationBackground, Assignment) %>%
  summarise(
    conv_nonprofit = ifelse(sum(App_nonprofit)>0,
                            sum(Use_nonprofit==1 & App_nonprofit==1)/sum(App_nonprofit==1),
                            NA_real_),
    conv_profit    = ifelse(sum(App_profit)>0,
                            sum(Use_profit==1 & App_profit==1)/sum(App_profit==1),
                            NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    conv_nonprofit = round(conv_nonprofit*100, 1),
    conv_profit    = round(conv_profit*100, 1)
  )


###### ======= Stacked Tables ========

## SES summary (stack apps + access + conversion)
SES_summary <- apps_SES %>%
  inner_join(access_SES, by = c("Educ2","Assignment","n"),
             suffix = c("_app","_use")) %>%
  inner_join(conv_SES,  by = c("Educ2","Assignment")) %>%
  select(
    Educ2, Assignment, n,
    app_nonprofit, app_profit,
    use_nonprofit, use_profit,
    conv_nonprofit, conv_profit
  ) %>%
  arrange(Educ2, Assignment)

## Migration summary 
MIG_summary <- apps_MIG %>%
  inner_join(access_MIG, by = c("MigrationBackground","Assignment","n"),
             suffix = c("_app","_use")) %>%
  inner_join(conv_MIG,  by = c("MigrationBackground","Assignment")) %>%
  select(
    MigrationBackground, Assignment, n,
    app_nonprofit, app_profit,
    use_nonprofit, use_profit,
    conv_nonprofit, conv_profit
  ) %>%
  arrange(MigrationBackground, Assignment)

print(SES_summary)
print(MIG_summary)

# Comment -> for low SES: treatment highers the application rate for both profit and non profit, but access remains stable. Conversion rate lowers (as conversion is use / application)
#         -> for migrants: treatment increases application rate for both profit and non profit, and access also increases (specially driven from non profit).
#.        -> for High SES: treatement seems to switch them from using for profit to non profit 


# Hypothesis 2: So this could be because inactivity (which could go against admission criteria therefore harming access) is higher among low SES than among migrants.
# So we will see if inactivity rates differ between low SES and migrants.



###### ======= Taux d'activité des mères par SES et migration ==========

activity_stacked <- bind_rows(
  # Migration
  MainDB %>%
    filter(!is.na(MigrationBackground), !is.na(Active)) %>%
    mutate(group_var = "Migration", group = as.character(MigrationBackground)) %>%
    count(group_var, group, Active, name = "n") %>%
    group_by(group_var, group) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup(),
  # SES
  MainDB %>%
    filter(!is.na(Educ2), !is.na(Active)) %>%
    mutate(group_var = "SES", group = as.character(Educ2)) %>%
    count(group_var, group, Active, name = "n") %>%
    group_by(group_var, group) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
) %>%
  arrange(group_var, group, desc(Active))

print(activity_stacked)
# Comment -> les mères low SES et les mères migrantes semblent avoir le même taux d'inactivité. Donc inactifs pas plus représentés chez low SES que chez les migrants



### ========= HET on Profit / Non Profit ===============

PostDB <- PostDB %>%
  left_join(
    MainDB %>% select(ResponseId, Use_nonprofit, Use_profit),
    by = "ResponseId"
  )

PostDBT2 <- PostDBT2 %>%
  left_join(
    MainDB %>% select(ResponseId, Use_nonprofit, Use_profit),
    by = "ResponseId"
  )



# ITT — Non-profit
Het.ITT.NonProfit.SES <- GroupHeterogeneityFnCTRL(
  DB = PostDB, Outcome = "Use_nonprofit",
  Heterogeneity = "Educ2", ITT = TRUE,
  Weights = "WeightPS", clusters = "StrataWave"
)
Het.ITT.NonProfit.Mig <- GroupHeterogeneityFnCTRL(
  DB = PostDB, Outcome = "Use_nonprofit",
  Heterogeneity = "MigrationBackground", ITT = TRUE,
  Weights = "WeightPS", clusters = "StrataWave"
)

# ITT — For-profit
Het.ITT.Profit.SES <- GroupHeterogeneityFnCTRL(
  DB = PostDB, Outcome = "Use_profit",
  Heterogeneity = "Educ2", ITT = TRUE,
  Weights = "WeightPS", clusters = "StrataWave"
)
Het.ITT.Profit.Mig <- GroupHeterogeneityFnCTRL(
  DB = PostDB, Outcome = "Use_profit",
  Heterogeneity = "MigrationBackground", ITT = TRUE,
  Weights = "WeightPS", clusters = "StrataWave"
)

# ATT — Non-profit
Het.ATT.NonProfit.SES <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2, Outcome = "Use_nonprofit",
  Heterogeneity = "Educ2", ITT = FALSE,
  Weights = "WeightPS", clusters = "StrataWave"
)
Het.ATT.NonProfit.Mig <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2, Outcome = "Use_nonprofit",
  Heterogeneity = "MigrationBackground", ITT = FALSE,
  Weights = "WeightPS", clusters = "StrataWave"
)

# ATT — For-profit
Het.ATT.Profit.SES <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2, Outcome = "Use_profit",
  Heterogeneity = "Educ2", ITT = FALSE,
  Weights = "WeightPS", clusters = "StrataWave"
)
Het.ATT.Profit.Mig <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2, Outcome = "Use_profit",
  Heterogeneity = "MigrationBackground", ITT = FALSE,
  Weights = "WeightPS", clusters = "StrataWave"
)


# --- Extract the T2-C row from each object & label it ---

# ITT
ITT_NP_SES <- Het.ITT.NonProfit.SES$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "Non-profit", Effect = "ITT", Heterogeneity = "SES", .before = 1)

ITT_NP_MIG <- Het.ITT.NonProfit.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "Non-profit", Effect = "ITT", Heterogeneity = "Migration background", .before = 1)

ITT_FP_SES <- Het.ITT.Profit.SES$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "For-profit", Effect = "ITT", Heterogeneity = "SES", .before = 1)

ITT_FP_MIG <- Het.ITT.Profit.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "For-profit", Effect = "ITT", Heterogeneity = "Migration background", .before = 1)

# ATT
ATT_NP_SES <- Het.ATT.NonProfit.SES$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "Non-profit", Effect = "ATT", Heterogeneity = "SES", .before = 1)

ATT_NP_MIG <- Het.ATT.NonProfit.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "Non-profit", Effect = "ATT", Heterogeneity = "Migration background", .before = 1)

ATT_FP_SES <- Het.ATT.Profit.SES$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "For-profit", Effect = "ATT", Heterogeneity = "SES", .before = 1)

ATT_FP_MIG <- Het.ATT.Profit.Mig$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(Outcome = "For-profit", Effect = "ATT", Heterogeneity = "Migration background", .before = 1)






### ========= Double HET Migration x Activity ===========

Het.ITT.AppCreche.ActiveMigrant <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveMigrant = interaction(ActiveBaseline, MigrationBackground)),
                                                        Outcome = "AppCreche",
                                                        Heterogeneity = "ActiveMigrant",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.AppCreche.ActiveMigrant <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveMigrant = interaction(ActiveBaseline, MigrationBackground)),
                                                         Outcome = "AppCreche",
                                                         Heterogeneity = "ActiveMigrant",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")

Het.ITT.UseCreche.ActiveMigrant <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveMigrant = interaction(ActiveBaseline, MigrationBackground)),
                                                        Outcome = "UseCreche",
                                                        Heterogeneity = "ActiveMigrant",
                                                        ITT = TRUE,
                                                        Weights = "WeightPS",
                                                        clusters = "StrataWave")

Het.LATE.UseCreche.ActiveMigrant <- GroupHeterogeneityFnCTRL(DB = PostDBT2 %>% mutate(ActiveMigrant = interaction(ActiveBaseline, MigrationBackground)),
                                                         Outcome = "UseCreche",
                                                         Heterogeneity = "ActiveMigrant",
                                                         ITT = FALSE,
                                                         Weights = "WeightPS",
                                                         clusters = "StrataWave")


# Intersectional estimation, adding another variable for heterogeneity

Het.ITT.AppCreche.ActiveMigrant$ModelSummary$tidy <- Het.ITT.AppCreche.ActiveMigrant$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "Immigrant"))

Het.ITT.AppCreche.ActiveMigrant$ModelSummary0$tidy <- Het.ITT.AppCreche.ActiveMigrant$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "Immigrant"))

Het.LATE.AppCreche.ActiveMigrant$ModelSummary$tidy <- Het.LATE.AppCreche.ActiveMigrant$ModelSummary$tidy %>%
  separate(Group, into = c("Activity", "Immigrant"))

Het.ITT.UseCreche.ActiveMigrant$ModelSummary$tidy <- Het.ITT.UseCreche.ActiveMigrant$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "Immigrant"))

Het.ITT.UseCreche.ActiveMigrant$ModelSummary0$tidy <- Het.ITT.UseCreche.ActiveMigrant$ModelSummary0$tidy %>% 
  separate(Group, into = c("Activity", "Immigrant"))

Het.LATE.UseCreche.ActiveMigrant$ModelSummary$tidy <- Het.LATE.UseCreche.ActiveMigrant$ModelSummary$tidy %>% 
  separate(Group, into = c("Activity", "Immigrant"))



# change the name
cm <- c('T2-C'    = 'Information + Support vs Control')


# Summary table of results
modelsummary(list("Daycare application_Control mean"  = Het.ITT.AppCreche.ActiveMigrant$ModelSummary0,
                  "Daycare application_ITT" = Het.ITT.AppCreche.ActiveMigrant$ModelSummary,
                  "Daycare application_ATT" = Het.LATE.AppCreche.ActiveMigrant$ModelSummary,
                  "Daycare access_Control mean"  = Het.ITT.UseCreche.ActiveMigrant$ModelSummary0,
                  "Daycare access_ITT" = Het.ITT.UseCreche.ActiveMigrant$ModelSummary,
                  "Daycare access_ATT" = Het.LATE.UseCreche.ActiveMigrant$ModelSummary),
             shape = term + Activity + Immigrant ~ model,
             fmt = fmt_statistic(estimate = 2, adj.p.value = 3, std.error = 2, conf.int = 2, "Chi 2" = 2, "P-value" = 3), 
             estimate = '{estimate}{stars} ({std.error})',
             statistic = c("conf.int", "adj.p.val. = {adj.p.value}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = cm,
             gof_map = c(# "Mean of DV",
               "Covariates", "Fixed effects", "Mean F-stat 1st stage", "Chi 2", "P-value",
               "nobs", "r.squared", "adj.r.squared"),
             title = "Average effects on application and access to daycare by Migration Background and mothers activity at baseline",
             notes = paste("Sources:", SourcesStacked,
                           "
*= p<.1, **= p<.05, ***= p<.01 based on pointwise p-value.
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
# Commentaire -> no effect specifically on active migrants


# Export de la table


# label for the treatment contrast
cm <- c('T2-C' = 'Information + Support vs Control')

TableMig <- 
  modelsummary(
    list(
      "Daycare application_Control mean" = Het.ITT.AppCreche.ActiveMigrant$ModelSummary0,
      "Daycare application_ITT"          = Het.ITT.AppCreche.ActiveMigrant$ModelSummary,
      "Daycare application_ATT"          = Het.LATE.AppCreche.ActiveMigrant$ModelSummary,
      "Daycare access_Control mean"      = Het.ITT.UseCreche.ActiveMigrant$ModelSummary0,
      "Daycare access_ITT"               = Het.ITT.UseCreche.ActiveMigrant$ModelSummary,
      "Daycare access_ATT"               = Het.LATE.UseCreche.ActiveMigrant$ModelSummary
    ),
    shape = term + Activity + Immigrant ~ model,
    fmt = fmt_statistic(
      estimate   = 2, 
      adj.p.value= 3,
      std.error  = 2,
      conf.int   = 2,
      "Chi 2"    = 2,
      "P-value"  = 3
    ), 
    estimate  = '{estimate} ({std.error})',
    statistic = c("p = {p.value}", "conf.int", "adj.p.val. = {adj.p.value}"),
    stars     = FALSE,
    coef_map  = cm,
    gof_map   = c("Covariates","Fixed effects","Mean F-stat 1st stage","Chi 2","P-value",
                  "nobs","r.squared","adj.r.squared"),
    output    = 'flextable'
  ) |>
  theme_booktabs() |>
  separate_header(split = "_", opts = c("center-hspan")) |>
  bold(i = 1, part = "header") |>
  merge_at(j = 2, part = "header") |>
  merge_at(j = 3, part = "header") |>
  merge_at(j = 1, part = "header") |>
  # body merges – same pattern you used for Table10; tweak ranges if your row counts differ
  merge_at(j = 1, i = 1:16,  part = "body") |>
  merge_at(j = 2, i = 1:8,   part = "body") |>
  merge_at(j = 2, i = 9:16,  part = "body") |>
  merge_at(j = 3, i = 1:4,   part = "body") |>
  merge_at(j = 3, i = 5:8,   part = "body") |>
  merge_at(j = 3, i = 9:12,  part = "body") |>
  merge_at(j = 3, i = 13:16, part = "body") |>
  italic(i = 1, part = "header") |>
  italic(j = 1, part = "body") |>
  align(part = "header", align = "center") |>
  align(part = "body",   align = "center") |>
  width(j = c(4,5,7,8), width = 2.4, unit = "cm") |>
  width(j = c(1,2,3,6), width = 2.0, unit = "cm") |>
  hline(i = c(8,16), part = "body") |>
  hline(i = c(4,12), j = 3:9, part = "body") |>
  bg(i = c(1,5,9,13), j = 4:9, bg = "lightgrey")

TableMig

flextable::save_as_docx(TableMig, path = "TableMig.docx")












## ==================== Geography and access to daycare =====================

# Hypothesis 3: vu que le traitement ne semble pas avoir un effet différencié selon le taux d'activité, on fait l'hypothèse que
#            les migrants sont peut être concentrés dans des zones avec un meilleur taux de couverture


MainDB$dept_cat <- factor(
  MainDB$code_departement,
  levels = c(75, 93, 94),
  labels = c("Paris (75)", "Seine-Saint-Denis (93)", "Val-de-Marne (94)")
)


# Descriptive Table

migrant_dist <- MainDB %>%
  filter(MigrationBackground == "Yes") %>%
  count(dept_cat, name = "n_migrants") %>%
  mutate(
    pct_of_migrants = n_migrants / sum(n_migrants)
  ) %>%
  arrange(desc(pct_of_migrants))

migrant_dist


lowses_dist <- MainDB %>%
  filter(Educ2 == "Low-SES") %>%
  count(dept_cat, name = "n_lowses") %>%
  mutate(
    pct_of_lowses = n_lowses / sum(n_lowses)
  ) %>%
  arrange(desc(pct_of_lowses))

lowses_dist


desc_tbl_1 <- MainDB %>%
  group_by(dept_cat) %>%
  summarise(
    n_obs           = n(),
    n_lowses      = sum(Educ2 == "Low-SES", na.rm = TRUE),
    share_migrants  = n_lowses / n_obs,
    mean_TAUXCOUV   = mean(TAUXCOUV_COM, na.rm = TRUE),
    .groups = "drop"
  )

desc_tbl_1


# Add variables to PostDB
PostDBT2 <- PostDBT2 %>%
  left_join(
    MainDB %>%
      select(ResponseId, dept_cat, Use_nonprofit, Use_profit),
    by = "ResponseId"
  )


# Intéraction Migration x Département
lm_dept_mig <- lm_robust(
  UseCreche ~ MigrationBackground * dept_cat,
  data = PostDBT2,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_dept_mig)


# Utilisation Non Profit x Migration
lm_nonprofit_mig <- lm_robust(
  Use_nonprofit ~ MigrationBackground * Assignment,
  data = PostDBT2,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_nonprofit_mig)

# Utilisation Profit x Migration
lm_profit_mig<- lm_robust(
  Use_profit ~ MigrationBackground * Assignment,
  data = PostDBT2,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_profit_mig)

#Utilisation Non Profit x SES
lm_nonprofit_ses <- lm_robust(
  Use_nonprofit ~ Educ2 * Assignment,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_nonprofit_ses)

# Utilisation Profit x SES
lm_profit_ses <- lm_robust(
  Use_profit ~ Educ2 * Assignment,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(lm_profit_ses)






















#### ========== Alluvial Plots a terminar ================ 

# 0) Keep only Control and T2 arms
Alluvial_data <- MainDB %>%
  filter(Assignment %in% c("C","Control","T2")) %>%
  mutate(
    Arm = case_when(
      Assignment %in% c("C","Control") ~ "Control",
      Assignment == "T2"               ~ "T2",
      TRUE ~ NA_character_
    )
  )

# 1) Group applications (priority order, NA-safe)
Alluvial_data <- Alluvial_data %>%
  mutate(
    app_creches_asso = (coalesce(AppCrecheA,0) == 1 | coalesce(AppParentale,0) == 1),
    app_nounou       = (coalesce(AppPartage,0) == 1 | coalesce(AppNounou,0) == 1),
    app_assmat       = (coalesce(AppMAM,0) == 1     | coalesce(AppAssMat,0) == 1),
    app_creches_pub  = (coalesce(AppCrechePub,0) == 1 | coalesce(AppHosp,0) == 1 |
                          coalesce(AppFamiliale,0) == 1 | coalesce(AppHG,0) == 1),
    app_creches_priv = (coalesce(AppMicroCreche,0) == 1 | coalesce(AppCrechePrivee,0) == 1),
    
    Applied = case_when(
      app_creches_pub  ~ "Crèches Publiques",
      app_creches_priv ~ "Crèches Privées",
      app_creches_asso ~ "Crèches Asso",
      app_assmat       ~ "Assistantes maternelles",
      app_nounou       ~ "Nounou",
      TRUE             ~ "Aucune demande"
    ),
    
    Wanted = ECSType7_recoded,
    Used   = ECSType6Endline
  ) %>%
  mutate(
    SES = Educ2,  # "High-SES"/"Low-SES"
    Migrant = case_when(
      FrenchYNBaseline %in% c("Abroad","Else") ~ "Migrant",
      FrenchYNBaseline == "French"             ~ "Non-migrant",
      TRUE                                     ~ "(NA)"
    ),
    Arm = factor(Arm, levels = c("Control","T2"))
  )

# 2) Pre-aggregate flows
flows <- Alluvial_data %>%
  group_by(Arm, SES, Migrant, Wanted, Applied, Used) %>%
  summarise(n = n(), .groups = "drop")

# 3) Plot helper (facets by Arm to compare Control vs T2 within a subgroup)
plot_alluvial <- function(df, title = "") {
  ggplot(df, aes(axis1 = Wanted, axis2 = Applied, axis3 = Used, y = n)) +
    geom_alluvium(aes(fill = Wanted), alpha = 0.7, knot.pos = 0.4) +
    geom_stratum(width = 0.15, color = "grey20") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, vjust = -0.1) +
    scale_x_discrete(limits = c("Souhaité (baseline)", "Demandé", "Obtenu (endline)")) +
    labs(x = NULL, y = "Nombre", fill = "Souhaité (baseline)", title = title) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    facet_wrap(~ Arm, scales = "free_y")
}

# 4) Four within-subgroup comparisons (each facets Control vs T2)
p_low  <- flows %>% filter(SES == "Low-SES")        %>% plot_alluvial(title = "Low-SES: Control vs T2")
p_high <- flows %>% filter(SES == "High-SES")       %>% plot_alluvial(title = "High-SES: Control vs T2")
p_mig  <- flows %>% filter(Migrant == "Migrant")    %>% plot_alluvial(title = "Migrant: Control vs T2")
p_non  <- flows %>% filter(Migrant == "Non-migrant")%>% plot_alluvial(title = "Non-migrant: Control vs T2")

# Print
p_low
p_high
p_mig
p_non
















# Incidencia de la politica de la ciudad

# Ver cuantas observaciones hay por ciudad (ver si son suficientes)

# Y luego pensar en como podriamos ver el efecto de la politica de la ciudad


###### HET SES #####


# outcomes à tester = candidature et accès aux crèches publiques, associatives et privées 
vars_to_test <- c("AppCrechePub", "AppCrecheA", "AppCrechePrivee", "AppAssMat", "UseCrechePub", "UseCrecheA", "UsePrivée", "UseAssMat")


# ITT
results_list_ITT <- lapply(vars_to_test, function(v) {
  GroupHeterogeneityFnCTRL(
    DB = PostDB,
    Outcome = v,
    Heterogeneity = "Educ2",
    ITT = TRUE,
    Weights = "WeightPS",
    clusters = "StrataWave"
  )
})


# ATT
results_list_ATT <- lapply(vars_to_test, function(v) {
  GroupHeterogeneityFnCTRL(
    DB = PostDBT2,
    Outcome = v,
    Heterogeneity = "Educ2",
    ITT = FALSE,
    Weights = "WeightPS",
    clusters = "StrataWave"
  )
})


extract_tidy_T2 <- function(x, name){
  x$Tidy %>%
    filter(term == "T2-C") %>%
    select(Group, estimate, std.error, p.value) %>%
    mutate(Outcome = name)
}

names(results_list_ITT) <- c(
  "AppCrechePub",
  "AppCrecheA",
  "AppCrechePrivee",
  "AppAssMat",
  "UseCrechePub",
  "UseCrecheA",
  "UsePrivée",
  "UseAssMat",
)

names(results_list_ATT) <- c(
  "AppCrechePub",
  "AppCrecheA",
  "AppCrechePrivee",
  "AppAssMat",
  "UseCrechePub",
  "UseCrecheA",
  "UsePrivée",
  "UseAssMat"
)


ITT_table <- map2_dfr(results_list_ITT, names(results_list_ITT), extract_tidy_T2)
print(ITT_table)
#Comment -> Candidatures: effet + du traitement sur crèche publique uniquement pour low SES
#.                        effet + du traitement sur crèche associative pour low et high SES
#           Utilisation:  effet + du traitement sur crèche publique pour high SES
#                         effet + du traitement 

ATT_table <- map2_dfr(results_list_ATT, names(results_list_ATT), extract_tidy_T2)

print(ATT_table)
# Comment -> crèche privée et assmat ne semblent pas médier l'effet positif du traitement sur l'accès aux crèches pour high SES — non significatif.
# le traitement a augmenté les candidatures et l'accès aux crèches associatives pour les high SES.
# effet de substitution possible: shift des crèches privées vers crèches asso pour les high SES traités
# Pour low SES, le traitement augmente les candidatures pour les crèches publiques, mais pas d'effet sur l'accès à ces structures.



###### NO HET ########










# ITT
ITT_results_no_het_list <- map(vars_to_test, ~ ITTSimultaneous(
  Y = .x,
  treat = "Z",
  DB = PostDB,
  Correction = "Westfall",
  weights = "WeightPS"
))

names(ITT_results_no_het_list) <- vars_to_test

ITT_results_table <- map2_dfr(
  ITT_results_no_het_list,
  names(ITT_results_no_het_list),
  ~ .x$Tidy %>% 
    filter(term == "T2-C") %>% 
    select(estimate, std.error, p.value) %>% 
    mutate(Outcome = .y)
)

print(ITT_results_table)


# ATT

ATT_results_no_het_list <- map(vars_to_test, ~ LATESimultaneous(
  Y= .x,
  DB=PostDBT2,
  Correction="Westfall",
  weights="WeightPS"
))

names(ATT_results_no_het_list) <- vars_to_test


ATT_results_table <- map2_dfr(
  ATT_results_no_het_list,
  names(ATT_results_no_het_list),
  ~ .x$Tidy %>% 
    filter(term == "T2-C") %>% 
    select(estimate, std.error, p.value) %>% 
    mutate(Outcome = .y)
)

print(ATT_results_table)
# Comment -> le traitement augmente de manière significative les candidatures et l'accès aux crèches associatives
#.           effet positif marginal sur les candidatures aux crèches publiques, mais pas d'effet sur l'accès à ces structures.
  



##### ------- Traitement et effet sur High SES ######

#Rappel: 1. le traitement augmente l'accès pour les high SES aux crèches spécifiquement, mais pas pour les modes d'accueil en général. 
#.       2. l'effet positif des crèches est drivé par les crèches associatives
# Hypothèse: le traitement a fait switch les high SES d'autres modes d'acceuil distincts des crèches (assmat, nounou, etc) vers les crèches.


# Table desc taux de candidatures par type de mode d'accueil entre SES et bras de traitement
table_candidatures_ECS <- PremiersPasWithNonRespondents %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    p_crecheA = mean(AppCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(AppCrechePrivee, na.rm = TRUE),
    p_crechepub = mean(AppCrechePub, na.rm = TRUE),
    p_microcreche = mean(AppMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(AppCrecheParentale, na.rm = TRUE),
    p_familiale = mean(AppCrecheFamiliale, na.rm = TRUE),
    p_assmat = mean(AppAssMat, na.rm = TRUE),
    p_nounou = mean(AppNounou, na.rm = TRUE),
    p_MAM = mean(AppMAM, na.rm = TRUE),
    p_halte = mean(AppHG, na.rm = TRUE),
    p_partage = mean(AppPartage, na.rm = TRUE),
    p_hospitaliere = mean(AppHosp, na.rm = TRUE),
    p_autre = mean(AppAutre, na.rm = TRUE))

print(table_candidatures_ECS)



# Table des taux de accès par type de mode d'accueil entre SES et bras de traitement

table_accès_ECS <- PremiersPasWithNonRespondents %>%
  group_by(Educ2, Assignment) %>%
  summarise(
    p_crecheA = mean(UseCrecheA, na.rm = TRUE),
    p_crechePrivee = mean(UsePrivée, na.rm = TRUE),
    p_crechepub = mean(UseCrechePub, na.rm = TRUE),
    p_microcreche = mean(UseMicroCreche, na.rm = TRUE),
    p_crecheParentale = mean(UseParentale, na.rm = TRUE),
    p_familiale = mean(UseFamiliale, na.rm = TRUE),
    p_assmat = mean(UseAssMat, na.rm = TRUE),
    p_nounou = mean(UseNounou, na.rm = TRUE),
    p_MAM = mean(UseMAM, na.rm = TRUE),
    p_halte = mean(UseHG, na.rm = TRUE),
    p_partage = mean(UsePartage, na.rm = TRUE),
    p_hospitaliere = mean(UseHosp, na.rm = TRUE),
    p_autre = mean(UseAutre, na.rm = TRUE))
print(table_accès_ECS)



#distinction mode de garde collectif vs individuel

PremiersPasWithNonRespondents <- PremiersPasWithNonRespondents %>%
  mutate(
    # --- Candidatures ---
    AppCollectif = as.numeric(
      AppCrecheA |
        AppCrechePub |
        AppCrechePrivee |
        AppCrecheParentale |
        AppCrecheFamiliale |
        AppMicroCreche |
        AppHG |
        AppHosp |
        AppJE
    ),
    AppIndividuel = as.numeric(
      AppNounou |
        AppPartage |
        AppAssMat |
        AppMAM
    ),
    
    # --- Utilisation ---
    UseCollectif = as.numeric(
      UseCrecheA |
        UseCrechePub |
        UsePrivée |
        UseParentale |
        UseFamiliale |
        UseMicroCreche |
        UseHG |
        UseHosp |
        UseJE
    ),
    UseIndividuel = as.numeric(
      UseNounou |
        UsePartage |
        UseAssMat |
        UseMAM
    )
  )


PremiersPasWithNonRespondents %>%
  group_by(Assignment, Educ2) %>%
  summarise(
    a_collectif = mean(AppCollectif, na.rm = TRUE),
    a_individuel = mean(AppIndividuel, na.rm = TRUE),
    u_collectif = mean(UseCollectif),
    u_individuel = mean(UseIndividuel)
  )
# Comment -> 

PostDB <- PostDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, AppCollectif, AppIndividuel, UseCollectif, UseIndividuel),
            by = "ResponseId")

PostDBT2 <- PostDBT2 %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, AppCollectif, AppIndividuel, UseCollectif, UseIndividuel),
            by = "ResponseId")


Het.ITT.UseInd <- GroupHeterogeneityFnCTRL(
  DB = PostDB,                 
  Outcome = "UseIndividuel",       
  Heterogeneity = "Educ2",    
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)


ITT_by_SES <- Het.ITT.UseInd$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)
print(ITT_by_SES)
# Comment -> effet non significatif du traitement sur l'utilisation des modes individuels. 



table(PremiersPasWithNonRespondents$UseCreche)
table(PremiersPasWithNonRespondents$UseCrechePub)
table(PremiersPasWithNonRespondents$UseCrecheA)
table(PremiersPasWithNonRespondents$UsePrivée)
table(PremiersPasWithNonRespondents$UseCrecheDept)
table(PremiersPasWithNonRespondents$UseCrecheMuni)



df <- PremiersPasWithNonRespondents

# 1) Recompute the union from subtypes and compare to UseCreche
creche_cols <- c("UseCrecheA","UseCrecheDept", "UseCrecheMuni", "UsePrivée","UseParentale",
                 "UseFamiliale","UseMicroCreche","UseHosp")  # adjust list to your definition

df_chk <- df %>%
  mutate(across(all_of(creche_cols), ~replace_na(., 0)),
         UseCreche_from_subs = as.integer(rowSums(across(all_of(creche_cols))) > 0))

mismatch <- df_chk %>%
  filter(UseCreche == 0, UseCreche_from_subs == 1)

n_mismatch <- nrow(mismatch)
n_total <- nrow(df_chk)
message("Mismatches: ", n_mismatch, " / ", n_total)

# ---- 4) Which subtypes are 'on' in the mismatches? (counts & shares)
subtype_counts <- mismatch %>%
  summarise(across(all_of(creche_cols), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Subtype", values_to = "Count") %>%
  mutate(Share = Count / n_mismatch) %>%
  arrange(desc(Count))
print(subtype_counts)




























# Nombre total de candidatures par type de mode d'accueil

# On prend le nombre total de candidatures pour chaque ECS et par individu 
# (une mère peut candidater non seulement pour différents types de ECS, mais aussi pour plusieurs structures du même type)
PremiersPasWithNonRespondents <- PremiersPasWithNonRespondents %>%
  mutate(
    AppNCreche = AppNCrecheHops + AppNCrecheAsso + AppNCrecheParentale +
      AppNCrecheFamiliale + AppNCrechePrivee + AppNMicroCreche +
      AppNAutreCreche + AppNCrechePub,
    AppNAssMat = AppNAssMat + AppNMAM,   
    AppNHalte = AppNHG,
    AppNNounouTot = AppNNounou + AppNPartage,
    AppNAutreTot = AppNJE
  )

table(MainDB$AppAssMat)


#voir type de crèche auxquels les riches ont eu accès


# Ajouter variables à la base
MainDB <- MainDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, AppNCreche, AppNAssMat, AppNHalte, AppNNounouTot, AppNAutreTot),
            by = "ResponseId")


# remplacer les NA par 0 pour les candidatures
MainDB <- MainDB %>%
  mutate(
    AppNCreche = ifelse(is.na(AppNCreche), 0, AppNCreche),
    AppNAssMat = ifelse(is.na(AppNAssMat), 0, AppNAssMat),
    AppNHalte = ifelse(is.na(AppNHalte), 0, AppNHalte),
    AppNNounouTot = ifelse(is.na(AppNNounouTot), 0, AppNNounouTot),
    AppNAutreTot = ifelse(is.na(AppNAutreTot), 0, AppNAutreTot)
  )


# table descriptive pour voir n candidatures moyennes par type de ECS voulu
table_candidatures <- MainDB %>%
  group_by(Assignment, ECSType7_recoded) %>%
  summarise(
    n_familles = n(),
    mean_app_creche = mean(AppNCreche),
    total_app_creche = sum(AppNCreche),
    mean_app_assmat = mean(AppNAssMat),
    total_app_assmat = sum(AppNAssMat),
    mean_app_halte = mean(AppNHalte),
    total_app_halte = sum(AppNHalte),
    mean_app_nounou = mean(AppNNounouTot),
    total_app_nounou = sum(AppNNounouTot),
    mean_app_autre = mean(AppNAutreTot),
    total_app_autre = sum(AppNAutreTot)
  )
print(table_candidatures, n = 25)
#Comment -> Parmi ceux qui voulaient une crèche, taux de candidature pour ass mat + bas parmi groupes traitement
#           En T2 taux de app en crèche parmi ceux qui veulent ass mat + élevé que T2 et contrôle
#.          NA en T2 taux de candidatures en crèche + élevé


# tableau
table_modes_baseline <- MainDB %>%
  group_by(Assignment, ECSType7_recoded) %>%
  summarise(
    n_familles = n(),
    mean_app_creche = mean(AppCreche, na.rm = TRUE),
    total_app_creche = sum(AppCreche, na.rm = TRUE),
    mean_app_crecheA = mean(AppCrecheA, na.rm = TRUE),
    total_app_crecheA = sum(AppCrecheA, na.rm = TRUE),
    
    mean_app_assmat = mean(AppAssMat.y, na.rm = TRUE),
    total_app_assmat = sum(AppAssMat.y, na.rm = TRUE),
    
    mean_app_je = mean(AppJE, na.rm = TRUE),
    total_app_je = sum(AppJE, na.rm = TRUE),
    
    mean_app_autre = mean(AppAutre, na.rm = TRUE),
    total_app_autre = sum(AppAutre, na.rm = TRUE),
    
    mean_app_partage = mean(AppPartage, na.rm = TRUE),
    total_app_partage = sum(AppPartage, na.rm = TRUE),
    
    mean_app_hg = mean(AppHG, na.rm = TRUE),
    total_app_hg = sum(AppHG, na.rm = TRUE),
    
    mean_app_nounou = mean(AppNounou, na.rm = TRUE),
    total_app_nounou = sum(AppNounou, na.rm = TRUE),
    
    mean_app_hosp = mean(AppHosp, na.rm = TRUE),
    total_app_hosp = sum(AppHosp, na.rm = TRUE),
    
    mean_app_parentale = mean(AppParentale, na.rm = TRUE),
    total_app_parentale = sum(AppParentale, na.rm = TRUE),
    
    mean_app_crecheprivee = mean(AppCrechePrivee, na.rm = TRUE),
    total_app_crecheprivee = sum(AppCrechePrivee, na.rm = TRUE),
    
    mean_app_microcreche = mean(AppMicroCreche, na.rm = TRUE),
    total_app_microcreche = sum(AppMicroCreche, na.rm = TRUE),
    
    mean_app_mam = mean(AppMAM, na.rm = TRUE),
    total_app_mam = sum(AppMAM, na.rm = TRUE),
    
    mean_app_crechemuni = mean(AppCrecheMuni, na.rm = TRUE),
    total_app_crechemuni = sum(AppCrecheMuni, na.rm = TRUE),
    
    mean_app_crechedept = mean(AppCrecheDept, na.rm = TRUE),
    total_app_crechedept = sum(AppCrecheDept, na.rm = TRUE),
    
    mean_app_crechepub = mean(AppCrechePub, na.rm = TRUE),
    total_app_crechepub = sum(AppCrechePub, na.rm = TRUE),
    
    mean_app_crecheparentale = mean(AppCrecheParentale, na.rm = TRUE),
    total_app_crecheparentale = sum(AppCrecheParentale, na.rm = TRUE),
    
    mean_app_crechefamiliale = mean(AppCrecheFamiliale, na.rm = TRUE),
    total_app_crechefamiliale = sum(AppCrecheFamiliale, na.rm = TRUE),
    
    mean_app_autrecreche = mean(AppAutreCreche, na.rm = TRUE),
    total_app_autrecreche = sum(AppAutreCreche, na.rm = TRUE)
  )

print(table_modes_baseline, n = 50)




# juste goupe de contrôle
alluvial_controls_app <- MainDB %>%
  filter(Assignment == "Control")   


# Mettre en long pour faire alluvial plot (contrôles uniquement)
applications_long_ctrl <- alluvial_controls_app %>%
  select(ResponseId, ECSType7_recoded,
         AppNCreche, AppNAssMat, AppNHalte, AppNNounouTot, AppNAutreTot) %>%
  pivot_longer(
    cols = starts_with("AppN"),
    names_to = "AppliedType_raw",
    values_to = "NApplications"
  ) %>%
  filter(!is.na(NApplications) & NApplications > 0) %>%
  mutate(
    AppliedType = case_when(
      AppliedType_raw == "AppNCreche" ~ "Crèches",
      AppliedType_raw == "AppNAssMat" ~ "Assistantes maternelles",
      AppliedType_raw == "AppNHalte" ~ "Haltes garderies",
      AppliedType_raw == "AppNNounouTot" ~ "Nounou",
      AppliedType_raw == "AppNAutreTot" ~ "Autre",
      TRUE ~ NA_character_
    )
  )



# reformat la base pour des axes + claires sur le graph
alluvial_app_ctrl_agg <- applications_long_ctrl %>%
  group_by(ECSType7_recoded, AppliedType) %>%
  summarise(Freq = sum(NApplications), .groups = "drop")
print(alluvial_app_ctrl_agg)


# afficher les flux sur le graph
label_data_apps_ctrl <- alluvial_app_ctrl_agg %>%
  group_by(ECSType7_recoded) %>%
  mutate(
    cumFreq = cumsum(Freq),
    y_label = cumFreq - Freq / 2,
    x_label = 1.5
  ) %>%
  ungroup()
print(label_data_apps_ctrl)


# Graphique
ggplot(alluvial_app_ctrl_agg,
       aes(axis1 = ECSType7_recoded,
           axis2 = AppliedType,
           y = Freq)) +
  
  geom_alluvium(aes(fill = ECSType7_recoded), width = 1/12, alpha = 0.7) +
  
  geom_stratum(width = 1/12, fill = "gray95", color = "gray80") +
  
  geom_text(
    stat = "stratum",
    aes(label = paste(after_stat(stratum), "\n", after_stat(count))),
    size = 3.5
  ) +
  
  geom_text(
    data = label_data_apps_ctrl %>% filter(Freq >= 20),  # seuil ajustable
    aes(x = x_label, y = y_label, label = Freq),
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  scale_x_discrete(
    limits = c("Desired care (Baseline)", "Applied to (Endline)"),
    expand = c(0.15, 0.15)
  ) +
  
  labs(
    title = "Applications by type among Control group only",
    x = "Stage",
    y = "Number of applications"
  ) +
  theme_minimal()
#Comment -> 







# Voir nombre de types candidatées par bras de traitement et SES
table(PremiersPasWithNonRespondents$AppNbTypeECS)

PostDB <- PostDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, AppNbTypeECS),
            by = "ResponseId")

lm_n_types <- lm_robust(
  AppNbTypeECS ~ Assignment + Educ2,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)
summary(lm_n_types)
# Comment -> participants en T2 








##### -------- HET Subjective Mismatch (A TERMINER) ---------


Het.ITT.ideal <- GroupHeterogeneityFnCTRL(
  DB = PostDB,                  
  Outcome = "ECSGotIdealECS",       
  Heterogeneity = "Educ2",   
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

#Ajouter variable dans PostDBT2
PostDBT2 <- PostDBT2 %>%
  left_join(MainDB %>% select(ResponseId, ECSGotIdealECS),
            by = "ResponseId")

Het.ATT.ideal <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2,                
  Outcome = "ECSGotIdealECS",
  Heterogeneity = "Educ2",
  ITT = FALSE,                
  Weights = "WeightPS",
  clusters = "StrataWave"
)

# ITT par SES
ITT_ideal <- Het.ITT.ideal$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

# ATT par SES
ATT_ideal <- Het.ATT.ideal$Tidy %>%
  filter(term == "T2-C") %>%
  select(Group, estimate, std.error, p.value)

print(ITT_ideal)
print(ATT_ideal)

#Comment -> Traitement augmente le mismatch subjectif? 



#### -------------- Satisfaction du Traitement ########


# Ajouter variables de perception dans les bases
PostDB <- PostDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, PsySatisfaction),
            by = "ResponseId")

PostDB <- PostDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, TreatmentUsefulness),
            by = "ResponseId")



# régression sur la satisfaction du traitement
lm_treatement_sat <- lm_robust(
  PsySatisfaction ~ Assignment + Educ2,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)
summary(lm_treatement_sat)
# Comment -> pas d'effet significatif du traitement sur la satisfaction déclarée en endline

table(PostDB$PsySatisfaction, useNA = "ifany")


# régression sur l'utilité du traitement
lm_treatement_use <- lm_robust(
  TreatmentUsefulness ~ Assignment + Educ2,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)
summary(lm_treatement_use)
#Comment -> mères low SES trouvent le traitement 10 point plus utile, pas d'effet selon bras de traitement.



PostDB %>%
  group_by(Assignment) %>%
  summarise(
    n = sum(!is.na(TreatmentUsefulness)),
    mean_usefulness = mean(TreatmentUsefulness, na.rm = TRUE),
    sd_usefulness = sd(TreatmentUsefulness, na.rm = TRUE)
  )


PostDB %>%
  group_by(Assignment) %>%
  summarise(
    n = sum(!is.na(PsySatisfaction)),
    mean_usefulness = mean(PsySatisfaction, na.rm = TRUE),
    sd_usefulness = sd(PsySatisfaction, na.rm = TRUE)
  )


#### ------- Spill over effects ########


table(PremiersPasWithNonRespondents$ECSBroSisApp)
table(PremiersPasWithNonRespondents$ECSBrosisUseYes)
#Comment -> 48 mères disent avoir candidaté pour un autre de leurs enfants, et 66 disent qu'elles ont eu accès à un mode de garde pour un de leurs enfants
#.          candidatures avant le traitement?



# 169 mères ont un autre enfant en âge d'aller en crèche
SpilloverDB <- PremiersPasWithNonRespondents %>%
  filter(OtherBabyLessThan3 == "Oui")

SpilloverDB <- SpilloverDB %>%
  left_join(PostDB %>% select(ResponseId, WeightPS, StrataWave),
            by = "ResponseId")


# recoder ECSBroSisApp en binaire
SpilloverDB <- SpilloverDB %>%
  mutate(ECSBroSisApp = case_when(
    ECSBroSisApp == "Oui" ~ "1",
    ECSBroSisApp == "Non" ~ "0",
    TRUE ~ NA_character_  
  ))


# table
SpilloverDB %>%
  filter(!is.na(ECSBroSisApp)) %>%  
  group_by(Assignment, ECSBroSisApp) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))
# Comment -> 48 mères déclarent avoir candidaté pour un autre enfant.


# régression
lm_spillover <- lm_robust(
  ECSBroSisApp ~ Assignment,
  data = SpilloverDB,
  weights = WeightPS,
  clusters = StrataWave,
)
summary(lm_spillover)
#Comment -> pas d'effet significatif du traitement sur le fait de candidater pour un autre enfant



#### ------- Effort, Stress et Difficulté déclarée endline ########


MainDB <- MainDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, PsyStress, PsySatisfaction, PsyComplicated, ControlECS, ControlCreche),
            by = "ResponseId")


tableau_psy <- MainDB %>%
  group_by(Assignment, Educ2) %>%
  summarise(
    mean_stress = mean(PsyStress, na.rm = TRUE),
    sd_stress   = sd(PsyStress, na.rm = TRUE),
    mean_complicated = mean(PsyComplicated, na.rm = TRUE),
    sd_complicated   = sd(PsyComplicated, na.rm = TRUE),
    mean_satisfaction = mean(PsySatisfaction, na.rm = TRUE),
    sd_satisfaction   = sd(PsySatisfaction, na.rm = TRUE),
    mean_maxeffortECS = mean(ControlECS, na.rm = TRUE),
    sd_maxeffortECS   = sd(ControlECS, na.rm = TRUE),
    mean_maxeffortCreche = mean(ControlCreche, na.rm = TRUE),
    sd_maxeffortCreche   = sd(ControlCreche, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()
print(tableau_psy, n = 30)

table(MainDB$ControlECS)
table(MainDB$ControlCreche, useNA = "ifany")


# PsyComplicated = de 0 à 100, à quel point chercher un mode de garde vous a paru compliqué?
lm_complicated_het <- lm_robust(
  PsyComplicated ~ Assignment + Educ2,
  data = MainDB,
  clusters = StrataWave
)
summary(lm_complicated_het)
# Comment -> Low SES déclarent en moyenne que le processus a été moins compliqué de 5 points


# PsyStress = de 0 à 100, à quel point avez-vous trouvé stressant le processus de trouver un mode de garde?
lm_stress_het <- lm_robust(
  PsyStress ~ Assignment + Educ2,
  data = MainDB,
  clusters = StrataWave
)
summary(lm_stress_het)
#Comment -> low SES déclarent avoir trouvé de 10 points moins stressant le procès de trouver un mode de garde, mais pas d'effet du traitement


#PsySatisfaction = de 0 à 100, globalement, êtes-vous satisfaite du mode de garde que vous avez obtenu pour votre enfant?
lm_satisfaction_het <- lm_robust(
  PsySatisfaction ~ Assignment + Educ2,
  data = MainDB,
  clusters = StrataWave
)
summary(lm_satisfaction_het)
# Comment -> pas de différence significative selon bras de traitement et SES


# ControlECS = de 0 à 100, si vous faisiez le max d'effort pour avoir un mode de garde, quelles auraient été les chances d'en avoir une? 
lm_control_ECS <- lm_robust(
  ControlECS ~ Assignment + Educ2,
  data = MainDB,
  clusters = StrataWave
)
summary(lm_control_ECS)
# Comment -> pas d'effet significatif du traitement sur contrôle perçu, mais low SES déclarent avoir 8 points moins de chances d'avoir 


# Controlcrèche = de 0 à 100, si vous faisiez le max d'effort pour avoir accès à une crèche, quelles auraient été les chances d'en avoir une? 
lm_control_crèche <- lm_robust(
  ControlCreche ~ Assignment + Educ2,
  data = MainDB,
  clusters = StrataWave
)
summary(lm_control_crèche)



#### ------- Timing et accès (suite stats des rapport CNAF?) -------------

library(lubridate)

# Ajouter date de candidature dans les bases
MainDB <- MainDB %>%
  left_join(PremiersPasWithNonRespondents %>% select(ResponseId, ECSAppTiming),
            by = "ResponseId")

# Date de candidature 
MainDB <- MainDB %>%
  mutate(app_date = ymd(ECSAppTiming))

# Distance à la rentrée 2023
rentree <- ymd("2023-9-01") #septembre
MainDB <- MainDB %>%
  mutate(
    lead_days_rentree  = as.numeric(rentree - app_date),   # >0 = a candidaté avant septembre + la candidature est précoce, + la valeur est grande
    lead_months_rentree = lead_days_rentree / 30.44 # le passer en mois, 365,25 / 12 
  )

# Trimming pour éviter que des valeurs extrêmes mènent le modèle (mais ne sont pas exclues)
MainDB <- MainDB %>%
  mutate(
    lead_months_rentree_trim = case_when(
      is.na(lead_months_rentree) ~ NA_real_,
      lead_months_rentree < -3 ~ -3,     # après la rentrée
      lead_months_rentree > 18 ~ 18,     # > max 18 mois avant 
      TRUE ~ lead_months_rentree
    )
  )


# Catégories temporelles
MainDB <- MainDB %>%
  mutate(
    lead_pos = pmax(lead_months_rentree_trim, -3), 
    timing5  = cut(
      lead_pos,
      breaks = c(-3, 3, 6, 9, 12, Inf),             # 5 intervalles
      labels = c("<3m", "3–6m", "6–9m", "9–12m", ">12m"),
      right  = TRUE, include.lowest = TRUE, ordered_result = TRUE
    )
  )

# Nouvelle base
Analytic <- MainDB %>%
  select(ResponseId, ECSUseYes, Educ2, FrenchYNBaseline, Assignment,
         StrataWave, ECSType7_recoded, ECSPlanToBaseline, timing5) %>%
  left_join(
    PostDB %>% select(ResponseId, WeightPS),
    by = "ResponseId"
  )


# LPM avec catégories de timing (référence = "<3m")

Analytic <- Analytic %>%
  mutate(timing5 = relevel(factor(timing5, ordered = FALSE), ref = "<3m"))

m_bins <- lm_robust(
  ECSUseYes ~ timing5 + Educ2 + FrenchYNBaseline + Assignment,
  data = Analytic, clusters = StrataWave, weights = WeightPS
)
summary(m_bins)
#Comment -> par rapport à candidater 0–3 mois avant la rentrée, candidater 9 à 12 mois avant la rentrée est associé à 18pp d'accès alors que >12 mois avant est associé à +23 pp d’accès. Candidater jusqu'à 9 mois avant n'a pas d'effet significatif.


# Intéraction timing x SES
m_lpm_any_interac <- lm_robust(
  ECSUseYes ~ timing5 * Educ2 + FrenchYNBaseline + Assignment,
  data = Analytic, clusters = Analytic$StrataWave, weights = Analytic$WeightPS
)
summary(m_lpm_any_interac)
#Comment -> intéraction non significative. Candidater avec anticipation serait bénéfique de la même manière quelque soit SES.


# voir si + 12 mois sont à Paris
tapply(MainDB$Nom.Département, MainDB$timing5, summary)


# variables de perception endline: 


#####-------- HET Timing x SES  ########


# Est ce que le traitement améliore le timing?


# Cutoff = 1er avril 2023 (≈ 5 mois avant la rentrée du 1er sept)
cutoff <- ymd("2023-04-01")

# Date de candidature + dummy early_april
MainDB <- MainDB %>%
  mutate(
    app_date    = ymd(ECSAppTiming),
    early_avril = case_when(
      is.na(app_date)       ~ NA_integer_,
      app_date <= cutoff    ~ 1L,   # 
      TRUE                  ~ 0L
    )
  )


# Tableau dummy avant avril et SES
MainDB %>%
  group_by(Assignment, Educ2) %>%
  summarise(n = sum(!is.na(early_avril)),
            pct_early = mean(early_avril, na.rm=TRUE)) %>%
  arrange(Assignment, Educ2)
#Comment -> High SES tendent à avoir une date de candidature antérieure


# Ajouter variable dans les bases PostDB et PostDBT2
PostDB <- PostDB %>%
  left_join(MainDB %>% select(ResponseId, early_avril),
            by = "ResponseId")

PostDBT2 <- PostDBT2 %>%
  left_join(MainDB %>% select(ResponseId, early_avril),
            by = "ResponseId")

# ITT par SES (conditionnel en ayant appliqué)
Het.ITT.App.Early <- GroupHeterogeneityFnCTRL(
  DB = PostDB,
  Outcome = "early_avril",
  Heterogeneity = "Educ2",  
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

ITT_by_SES <- Het.ITT.App.Early$Tidy %>%
  filter(term %in% c("T2-C")) %>%   # garde T2-C 
  select(term, Group, estimate, std.error, p.value)

print(ITT_by_SES)
#Comment -> le traitement augmente les candidatures avant avril chez les low SES, de 12,5pp. Sans effet pour les high SES. 
#.          !! intervalles de confiance larges. Non significatif.


# ATT
Het.ATT.App.Early <- GroupHeterogeneityFnCTRL(
  DB = PostDBT2,
  Outcome = "early_avril",
  Heterogeneity = "Educ2",
  ITT = FALSE,
  Weights = "WeightPS",
  clusters = "StrataWave"
)

ATT_by_SES <- Het.ATT.App.Early$Tidy %>%
  filter(term %in% c("T2-C")) %>%   # garde T2-C 
  select(term, Group, estimate, std.error, p.value)
print(ATT_by_SES)


# Graphique
# --- Paramètres du plot
term_levels <- c("T2-C")   # si tu veux aussi T1, mets c("T2-C","T1-C")
heterogeneity_levels <- c("SES")
panel_levels <- c("Control group", "ITT", "ATT")

# --- ITT : panneau "Control group" + "ITT"
DataPlot_ITT_SES <- dplyr::bind_rows(
  Het.ITT.App.Early$ModelSummary0$tidy %>%
    dplyr::mutate(Y = "Candidature avant avril",
                  panel = "Control group",
                  Heterogeneity = "SES",
                  Type = "ITT") %>%
    dplyr::filter(term %in% term_levels),
  
  Het.ITT.App.Early$Tidy %>%
    dplyr::mutate(Y = "Candidature avant avril",
                  panel = "ITT",
                  Heterogeneity = "SES",
                  Type = "ITT") %>%
    dplyr::filter(term %in% term_levels)
)

DataPlot_ATT_SES <- Het.ATT.App.Early$Tidy %>%
  dplyr::mutate(Y = "Candidature avant avril",
                panel = "ATT",
                Heterogeneity = "SES",
                Type = "ATT") %>%
  dplyr::filter(term %in% term_levels)

Data.Het.SES <- dplyr::bind_rows(DataPlot_ITT_SES, DataPlot_ATT_SES) %>%
  dplyr::mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels),
    Group = forcats::fct_relevel(Group, "High-SES", "Low-SES")  # ordre voulu
  )

# --- Graphique 
ggplot2::ggplot(Data.Het.SES) +
  ggplot2::geom_pointrange(ggplot2::aes(
    x = interaction(Heterogeneity, Group, sep = "!"),
    y = estimate,
    ymin = point.conf.low,   # IC pointwise 95%
    ymax = point.conf.high,
    shape = Group,
    color = Group
  ), position = ggplot2::position_dodge(.4)) +
  
  ggplot2::geom_crossbar(ggplot2::aes(
    y = estimate,
    x = interaction(Heterogeneity, Group, sep = "!"),
    fill = Group,
    ymin = conf.low,         # IC simultanés (Westfall–Young)
    ymax = conf.high,
    color = Group
  ), position = ggplot2::position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  
  ggplot2::facet_grid(rows = ggplot2::vars(forcats::fct_rev(Y)),
                      cols = ggplot2::vars(panel), scale = "free_x") +
  ggplot2::coord_flip() +
  ggplot2::geom_hline(data = Data.Het.SES %>% dplyr::filter(panel != "Control group"),
                      ggplot2::aes(yintercept = 0), linetype = 2) +
  ggplot2::xlab("") +
  scale_x_discrete(labels = function(x) gsub("!", "\n", x, fixed = TRUE)) +  
  ggplot2::scale_fill_brewer("Heterogeneity", palette = "Dark2") +
  ggplot2::scale_color_brewer("Heterogeneity", palette = "Dark2") +
  ggplot2::scale_shape("Heterogeneity") +
  ggplot2::labs(
    caption = paste(
      "Sources:", SourcesStacked,
      "\nITT: Intention to Treat; ATT: Average Treatment on the Treated.",
      "\nSE cluster-robustes (StrataWave).",
      "\nBarres fines: IC pointwise 95%; boîtes: IC simultanés 95% (Westfall–Young)."
    )
  ) + vis_theme















## POUBELLE ou A TERMINER :)

## ITT low information baseline (sur les deux groupes traités) 
PostDB <- PostDB %>%
  mutate(
    T12 = as.integer(Assignment %in% c("T1", "T2")), 
    T2_only = as.integer(Assignment == "T2")        
  )


PostDB <- PostDB %>%
  mutate(InfoBaseline = relevel(factor(InfoBaseline), ref = "Low knowledge"))

m_info <- lm_robust(
  ECSApp ~ T12 * InfoBaseline,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(m_info)

m_info_T2 <- lm_robust(
  ECSApp ~ T2_only * InfoBaseline,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(m_info_T2)


extract_effects <- function(model, name){
  tidy(model) %>%
    filter(grepl("T12|T2_only", term)) %>%
    mutate(model = name)
}

res_T12 <- extract_effects(m_info, "T12 (T1+T2)")
res_T2  <- extract_effects(m_info_T2, "T2 only")

effects_comp <- bind_rows(res_T12, res_T2) %>%
  mutate(
    estimate = round(estimate*100, 1),
    std.error = round(std.error*100, 1),
    p.value = signif(p.value, 3)
  )

print(effects_comp)

PostDB %>%
  group_by(InfoBaseline) %>%
  summarise(
    treated_T12 = sum(T12 == 1, na.rm = TRUE),
    treated_T2  = sum(T2_only == 1, na.rm = TRUE),
    control     = sum(T12 == 0, na.rm = TRUE)
  )


m_compare <- lm_robust(
  ECSApp ~ T12 * InfoBaseline + T2_only * InfoBaseline,
  data = PostDB,
  weights = WeightPS,
  clusters = StrataWave
)

summary(m_compare)

library(clubSandwich)
library(lmtest)

V_CR2 <- vcovCR(m_compare, cluster = PostDB$StrataWave, type = "CR2")


# Test de H0: effet T12_low = effet T2_low
linearHypothesis(
  m_compare,
  "T12 = T2_only",
  vcov. = V_CR2  
)


# est ce que ils avaient un autre enfant en age de aller en crèche (hors l'enfant premierspas). otherbabylessthan3. ECSbrosisApp ( vérifier que quand yes dans mode de garde, il y a bien una valeur dans est ce qu'ils ont candidaté. )Verifier que pour tous les brosis il y a un yes (brossisapp), spillover sur les autres enfants.

# questions page 103 sur psysatisfaction diffference par bras de traitement, stats des entre high low ses. 



table(MainDB$SecondGenerationFather)

# 300 personnes qui disent ne pas avoir origiine migratoire mais qui sont second generation immigrants. splitter entre les français voir graph 
# heterogeneite traitement french second gen, immigrant
# voir variable 


