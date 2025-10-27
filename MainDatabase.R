# Import and clean the data to update the database with new columns needed for the R&R
# Source databases are not available due to confidentiality issues

# Laudine : if you need to go back to the previous import code, see Sauvegarde or on Github


# ===== Setup =====
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(here)

# ----- Load bases -----
BaselineFull <- read_csv(here("Data","BaselineWithRefusals.csv")) 

# Endline recoded
MainDB <- read_csv(here("Data","PremiersPasWithNonRespondents112024_final.csv")) %>% # we take this database because it's the one generated from the corrected cleaning of endline data surveys 
  mutate(
    NormsBaseline = factor(
      NormsBaseline,
      levels = c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"),
      ordered = TRUE
    ),
    Dep = as.factor(Dep)
  )

MainDatabaseWithoutIds <- read_csv(here("Data","MainDatabaseWithoutIds.csv")) # database with 


# Import the file with the corrected variables

BaselineCorrected <- read_csv(here("Data","BaselinePremiersPas_corrected.csv")) %>% 
  select(ResponseId, Income, FmlyIncome, NormsOpposedN) # we select the variables we want


# Import the variables of interest in the dataframe
MainDB <- MainDB %>% left_join(BaselineCorrected, by = "ResponseId") %>% 
  select(-c(FmlyIncomeBaseline,IncomeBaseline, FmilyEarnLessThan2500 )) %>%  # we remove the variables with mistakes
  mutate(
    FmilyEarnLessThan2500 = ifelse((FmlyIncome == "+8000" | FmlyIncome == "+8000" | FmlyIncome == "2500-3000"  
                                    | FmlyIncome == "3000-4500" | FmlyIncome == "4500-5000" | FmlyIncome == "5000-6000"
                                    |  FmlyIncome == "6000-7000" | FmlyIncome == "7000-8000" | FmlyIncome == "Ne veut pas répondre"), 
                                   "No",
                                   "Yes"), 
    FmilyEarnLessThan2500 = as.factor(FmilyEarnLessThan2500))

MainDB <- MainDB %>%
  mutate(
    # Relabel language
    FrenchYNBaseline = recode(FrenchYNBaseline,
                              "Else" = "Abroad"),  # "French" reste "French"
    
    # Relabel SES
    Educ2 = recode(Educ2,
                   "Bac" = "Low-SES",
                   "Sup" = "High-SES"),
    
    # Make High-SES the reference level
    Educ2 = factor(Educ2, levels = c("High-SES", "Low-SES"))
  )



# ===== Endline-derived vars from the trusted base =====
MainDB <- MainDB %>%
  mutate(
    # Baby born after Jan 1, 2023? (adjust if you meant 2022 cutoff)
    BabyBornJanuary = case_when(
      !is.na(BabyBirthDate) & BabyBirthDate > as.Date("2022-12-31") ~ "After January",
      is.na(BabyBirthDate) ~ "Don't know",
      TRUE ~ "Before January"
    )
  )


# Likert recode + composite attitude score
MainDB <- MainDB %>%
  mutate(
    LikertReturnHK1or0 = if_else(
      LikertReturnHKBaseline %in% c("Assez d’accord","Très d’accord"), 1L, 0L,
      missing = 0L
    ),
    AttitudesScore = (coalesce(TrustCreche1or0, 0) + LikertReturnHK1or0) / 2,
    AttitudeScoreMoreThanMedian = if_else(
      AttitudesScore > median(AttitudesScore, na.rm = TRUE), "Yes", "No",
      missing = NA_character_
    ),
    TrustCreche1or0    = if_else(TrustCreche1or0 == 1, "Yes", "No", missing = NA_character_),
    LikertReturnHK1or0 = if_else(LikertReturnHK1or0 == 1, "Yes", "No", missing = NA_character_)
  )

# ===== Bring in Endline_data + Endline_extra extras =====
Endline_data  <- read_csv(here("Data","Endline.csv"))
Endline_extra <- read_csv(here("Data","Endline_extra.csv"))

# Keep the first two rows aside if needed for question texts
details_values <- Endline_data[1:2, ]

# Drop first two rows (header/info rows) in both files
Endline_data  <- Endline_data[-c(1:2), ]
Endline_extra <- Endline_extra[-c(1:2), ]

# Merge, keep only needed fields
Endline_add <- bind_rows(Endline_data, Endline_extra) %>%
  select(ExternalReference, ECSNoECSAppAnswer, ECSIdealNotReason, ECSIdealNotReason_5_TEXT) %>%
  # Dedupe: 1 row per ExternalReference, prefer rows with a non-missing ECSNoECSAppAnswer
  group_by(ExternalReference) %>%
  slice_max(order_by = !is.na(ECSNoECSAppAnswer), n = 1, with_ties = FALSE) %>%
  ungroup()

# Attach to MainDB by ResponseId
MainDB <- MainDB %>%
  left_join(Endline_add, by = c("ResponseId" = "ExternalReference")) %>%
  mutate(
    ECSNoECSAppAnswer_simple = case_when(
      is.na(ECSNoECSAppAnswer) ~ NA_character_,
      ECSNoECSAppAnswer == "Non, on attend encore" ~ "Still waiting/No Answer",
      ECSNoECSAppAnswer == "Oui, on a été rejetés" ~ "Rejected",
      ECSNoECSAppAnswer == "Oui, on est sur la liste d’attente" ~ "Waiting list",
      ECSNoECSAppAnswer %in% c(
        "Oui,  on été acceptés dans au moins 1 mais il y a eu un problème",
        "Oui,  on été acceptés dans au moins 1 mais on a refusé (à cause d'un problème, etc.)",
        "Oui,  on été acceptés mais on a dû repousser à cause de problèmes (de santé)",
        "Oui, on été acceptés, mais on a déménagé"
      ) ~ "Other"
    ),
    ECSNoECSAppAnswer_long = case_when(
      is.na(ECSNoECSAppAnswer) ~ NA_character_,
      ECSNoECSAppAnswer == "Oui, on est sur la liste d’attente" ~ "Waiting list",
      ECSNoECSAppAnswer == "Non, on attend encore" ~ "Still waiting/No Answer",
      ECSNoECSAppAnswer == "Oui, on a été rejetés" ~ "Rejected",
      ECSNoECSAppAnswer == "Oui,  on été acceptés mais on a dû repousser à cause de problèmes (de santé)" ~ "Health issues",
      ECSNoECSAppAnswer == "Oui, on été acceptés, mais on a déménagé" ~ "Moved out",
      ECSNoECSAppAnswer == "Oui,  on été acceptés dans au moins 1 mais il y a eu un problème" ~ "Issue with the application",
      ECSNoECSAppAnswer == "Oui,  on été acceptés dans au moins 1 mais on a refusé (à cause d'un problème, etc.)" ~ "Declined because inadequate"
    ),
    ECSIdealNotReason_recoded = case_when(
      str_detect(ECSIdealNotReason_5_TEXT, "correspondaient pas|J'ai eu la place maiq que pour un de mes enfants|mais pas à temps plein|dispo que pour qq mois|ne sont pas agrées|Mauvais contact|même crèche municipale|pas aimé la crèche|séparer ses bébés") ~ "Inadequate",
      str_detect(ECSIdealNotReason_5_TEXT, "obligatoirement 2 jours|pour 3j/sem") ~ "Incompatible working hours",
      str_detect(ECSIdealNotReason_5_TEXT, "déménag") | str_detect(ECSNoECSAppAnswer, "déménag") ~ "Moved out",
      str_detect(ECSIdealNotReason_5_TEXT, "santé") | str_detect(ECSNoECSAppAnswer, "santé") ~ "Health issues",
      str_detect(ECSIdealNotReason_5_TEXT, "trop tard|lui a été donnée|peur de ne rien trouver|la mairie leur a donné|bien avec l'ass mat|la crèche beaucoup plus tôt|marche acquise|date limite|reprendre le travail|déjà chez une assmat|un peu plus longtemps|elle ne marche pas encore|trop petit|au cours de l'anné|septembre était déjà terminée|été engagé auprès de l'ass mat|place pour Avy|après l'adaptation|période ou a besoin|Trop tot") ~ "Timing",
      str_detect(ECSIdealNotReason_5_TEXT, "loin du domicile|près du lieu de résidence|trop loin|proche du domicile|géographique") ~ "Too far away",
      str_detect(ECSIdealNotReason_5_TEXT, "Administrativement compliqué|dossier non actualisé|encore fait les démarches|s'est trompée|Dossier non traité|pas fait les inscriptions|pas terminé mes recherches|pouvoir candidater|faire les démarches|assistant social|pas candidaté|Problème d'inscription|joindre les crèches|situation") ~ "Paperwork too heavy",
      str_detect(ECSIdealNotReason_5_TEXT, "inscriptions étaient passées|délai trop court|vient de faire la demande|garder avec moi|auprès de moi au début|pas prévu de reprendre|trop tard|encore entrepris les recherches|rester avec bébé|garder par quelqu'un d'autre|à la mairie|a été annulé") ~ "Applied too late",
      str_detect(ECSIdealNotReason_5_TEXT, "Attend|Attente|d'attente|attend une place|en attente que place|j'attends une réponse du mode de garde|Pas de réponse|PAS DE NOUVELLES|Pas beaucoup d'assmat") ~ "Still waiting/No Answer",
      str_detect(ECSIdealNotReason_5_TEXT, "j'avais d'autres plans|2 refus") ~ "Rejected",
      str_detect(ECSIdealNotReason_5_TEXT, "pas droit vu qu'elle ne travaille pas|pas éligible") ~ "Perceived Not Eligible",
      str_detect(ECSIdealNotReason_5_TEXT, "crèche serait trop chère|perte d'argent") ~ "Too expensive",
      str_detect(ECSIdealNotReason_5_TEXT, "m'informer|Je ne savais pas|pas renseignée|pas connaissance|Pas le temps pour se renseigne") ~ "Lack of information",
      str_detect(ECSIdealNotReason_5_TEXT, "père|partenaire") ~ "Father's disagreed",
      is.na(ECSIdealNotReason_5_TEXT) ~ NA_character_,
      TRUE ~ "Other"
    ),
    ECSIdealNotReasonClean = case_when(
      is.na(ECSIdealNotReason) & is.na(ECSNoECSAppAnswer_long) ~ NA_character_,
      is.na(ECSIdealNotReason) & !is.na(ECSNoECSAppAnswer_long) ~ ECSNoECSAppAnswer_long,
      str_detect(ECSIdealNotReason, "Autre raison") & !is.na(ECSNoECSAppAnswer_long) ~ paste0(
        str_replace(ECSIdealNotReason, "Autre raison.", as.character(ECSIdealNotReason_recoded)),
        ",", ECSNoECSAppAnswer_long
      ),
      str_detect(ECSIdealNotReason, "Autre raison") & is.na(ECSNoECSAppAnswer_long) ~
        str_replace(ECSIdealNotReason, "Autre raison.", as.character(ECSIdealNotReason_recoded)),
      !is.na(ECSNoECSAppAnswer_long) ~ paste0(ECSIdealNotReason, ",", ECSNoECSAppAnswer_long),
      TRUE ~ ECSIdealNotReason
    )
  )

# ===== Add only the columns missing in PremiersPas from MainDatabaseWithoutIds =====
extra_vars <- setdiff(names(MainDatabaseWithoutIds), names(MainDB))

right_side <- MainDatabaseWithoutIds %>%
  select(any_of(c("ResponseId", extra_vars))) %>%
  distinct(ResponseId, .keep_all = TRUE)  # ensure many-to-one

MainDB <- MainDB %>%
  left_join(right_side, by = "ResponseId", relationship = "many-to-one")

# ===== Sanity checks =====
stopifnot(!any(duplicated(MainDB$ResponseId)))              # no respondent duplication
# ===== Write out =====
write_csv(MainDB, here("Data","MainDatabase_final.csv"))
