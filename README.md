# visualisation
library(readxl)
df <- read_excel("C:/Users/YOGA X390/Downloads/Cytomegalovirus.xlsx")


library(dplyr)

# Afficher un aperçu de la base de données
str(df)
summary(df)

#changer les 0, 1 par leurs codes
df$sex <- factor(df$sex, levels = c(0, 1), labels = c("female", "male"))
df$race <- factor(df$race, levels = c(0, 1), labels = c("African American", "White"))
df$`diagnosis type` <- factor(df$`diagnosis type`, levels = c(0, 1), labels = c("Lymphoid", "Myeloid"))
df$`prior radiation` <- factor(df$`prior radiation`, levels = c(0, 1), labels = c("No", "Yes"))
df$`prior transplant` <- factor(df$`prior transplant`, levels = c(0, 1), labels = c("No", "Yes"))
df$`recipient cmv` <- factor (df$`recipient cmv` , levels = c(0, 1), labels = c("Negative", "Positive"))
df$`donor cmv` <- factor(df$`donor cmv`, levels = c(0, 1), labels = c("Negative", "Positive"))
df$`donor sex` <- factor(df$`donor sex`, levels = c(0, 1), labels = c("Female", "Male"))
df$`C1/C2` <- factor (df$`C1/C2`, levels = c(0, 1), labels = c("Heterozygous", "Homozygous"))
df$cmv <- factor(df$cmv, levels = c(0, 1), labels = c("No", "Yes"))
df$agvhd <- factor(df$agvhd, levels = c(0, 1), labels = c("No", "Yes"))
df$cgvhd <- factor(df$cgvhd, levels = c(0, 1), labels = c("No", "Yes"))

table(df$sex)


#enlever les valeurs aberrantes 
df$age[df$age == 138] <- NA

#convertir les id en caracteres 
df$ID <- as.character(df$ID)

str(df)



# Analyser chaque variable du dataframe `df`
for (col in names(df)) {
  cat("\n===========================================\n")
  cat("Variable:", col, "\n")
  cat("===========================================\n")
  
  # Si la variable est numérique et continue (plus de 2 valeurs uniques)
  if (is.numeric(df[[col]]) && length(unique(df[[col]])) > 2) {
    cat("\n--- Statistiques pour variable numérique continue ---\n")
    cat("Moyenne:", mean(df[[col]], na.rm = TRUE), "\n")
    cat("Médiane:", median(df[[col]], na.rm = TRUE), "\n")
    cat("Écart-type:", sd(df[[col]], na.rm = TRUE), "\n")
    cat("Variance:", var(df[[col]], na.rm = TRUE), "\n")
    cat("Minimum:", min(df[[col]], na.rm = TRUE), "\n")
    cat("Maximum:", max(df[[col]], na.rm = TRUE), "\n")
    cat("Étendue:", diff(range(df[[col]], na.rm = TRUE)), "\n")
    cat("Quantiles (25%, 50%, 75%):\n")
    print(quantile(df[[col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
    cat("IQR (Interquartile Range):", IQR(df[[col]], na.rm = TRUE), "\n")
    
    # Si la variable est numérique binaire (codée en 0 et 1 ou avec 2 valeurs uniques)
  } else if (is.numeric(df[[col]]) && length(unique(df[[col]])) <= 2) {
    cat("\n--- Statistiques pour variable numérique binaire ---\n")
    cat("Fréquence des valeurs:\n")
    print(table(df[[col]], useNA = "ifany"))
    cat("Proportion de 1:", mean(df[[col]] == 1, na.rm = TRUE), "\n")
    cat("Proportion de 0:", mean(df[[col]] == 0, na.rm = TRUE), "\n")
    
    # Si la variable est catégorielle (caractère)
  } else if (is.character(df[[col]])) {
    cat("\n--- Statistiques pour variable catégorielle ---\n")
    cat("Fréquence des catégories:\n")
    print(table(df[[col]], useNA = "ifany"))
    cat("Nombre de catégories uniques:", length(unique(df[[col]])), "\n")
    
    # Si la variable est logique
  } else if (is.logical(df[[col]])) {
    cat("\n--- Statistiques pour variable logique ---\n")
    cat("Fréquence des valeurs (TRUE/FALSE):\n")
    print(table(df[[col]], useNA = "ifany"))
    cat("Proportion de TRUE:", mean(df[[col]], na.rm = TRUE), "\n")
    
    # Si la variable contient des valeurs manquantes
  }
  if (any(is.na(df[[col]]))) {
    cat("\n--- Valeurs manquantes ---\n")
    cat("Nombre de valeurs manquantes:", sum(is.na(df[[col]])), "\n")
    cat("Pourcentage de valeurs manquantes:", round(mean(is.na(df[[col]])) * 100, 2), "%\n")
  }
}


#creation de deux graphiques :

#Graphique 1 : Boxplot de la dose de cellules TNC selon le sexe et le type de diagnostic
library(ggplot2)

# Création du boxplot
ggplot(na.omit(df), aes(x = sex, y = `TNC dose`, fill = `diagnosis type`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Distribution de la dose de cellules TNC par sexe et type de diagnostic",
    x = "Sexe du receveur",
    y = "Dose de cellules TNC (x10^8/kg)",
    fill = "Type de diagnostic"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#meme graphique en violin

ggplot(na.omit(df), aes(x = sex, y = `TNC dose`, fill = `diagnosis type`)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Distribution de la dose de cellules TNC par sexe et type de diagnostic",
    x = "Sexe du receveur",
    y = "Dose de cellules TNC (x10^8/kg)",
    fill = "Type de diagnostic"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Graphique 2 : Histogramme de l'âge avec couleur par le statut de CMV

library(dplyr)
library(ggplot2)

# Filtrer pour enlever les valeurs manquantes
df_filtered <- na.omit(df %>% select(age, `recipient cmv`))

# Calcul des pourcentages par bin d'âge et statut CMV
df_hist <- df_filtered %>%
  group_by(age_bin = cut(age, breaks = seq(0, max(age, na.rm = TRUE), by = 5)), `recipient cmv`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Création de l'histogramme avec pourcentages
ggplot(df_hist, aes(x = age_bin, y = count, fill = `recipient cmv`)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(
    title = "Distribution des âges selon le statut CMV du receveur",
    x = "Âge",
    y = "Nombre de patients",
    fill = "Statut CMV"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


