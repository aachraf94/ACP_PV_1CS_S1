# Installer les bib en cas ou vous avez pas les installé
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("openxlsx")


# Charger les bibliothèques nécessaires
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(openxlsx)

# Étape 1 : Charger les données -------------------------------------------------------------------------------------
# Récupérer le répertoire de travail actuel
current_dir <- getwd()

# Construire automatiquement le chemin du fichier en utilisant getwd()
file_path1 <- paste0(current_dir, "/Data/Affectation Spécialité 2023_2024.xlsx")
file_path2 <- paste0(current_dir, "/Data/PV_1CS_2023_S1.xlsx")

# Charger les données depuis les fichiers
data <- read.xlsx(file_path2)
affectation_data <- read.xlsx(file_path1)

head(data)
head(affectation_data)


# Fusionner les deux jeux de données sur "Matricule" avec left join
merged_data <- merge(data, affectation_data[, c("Matricule", "Affectation")], 
                     by = "Matricule", all.x = TRUE)

# Afficher un aperçu des données fusionnées
head(merged_data)

# Save marged_data as csv in data folder
write.csv(merged_data, file = "Data/Output1_merged_data.csv", row.names = FALSE)





# Étape 3 : Prétraitement des données ------------------------------------------------------------------------------

# Garder uniquement les lignes où la colonne "Situation" est égale à "Inscrit" donc supprimer "Abandon" et "Congé académique (année blanche) pour raisons médicales"
filtered_data <- merged_data[merged_data$Situation == "Inscrit", ]

# Retirer les colonnes "Situation", "Groupe_S1", "Rang_S1", "Moy_S1", et "Groupe_S2" car elles ne sont pas pertinentes pour l'ACP
filtered_data <- filtered_data[, !colnames(filtered_data) %in% c("Situation", "Groupe_S1", "Rang_S1", "Moy_S1", "Groupe_S2")]

# Remplir les valeurs manquantes dans la colonne "Affectation" avec "NonAdmis2CS"
filtered_data$Affectation[is.na(filtered_data$Affectation)] <- "NonAdmis2CS"

# Afficher un aperçu des données traitées pour s'assurer que les étapes ont bien été exécutées
head(filtered_data)

# Si vous souhaitez enregistrer les données dans un fichier CSV pour des usages ultérieurs
write.csv(filtered_data, file = "Data/Output2_preprocessed_data.csv", row.names = FALSE)





# Création d'un index unique pour chaque ligne du dataframe ------------------------------------------------
# Combiner les colonnes "Matricule" et "Affectation" pour créer une nouvelle colonne "Index"
# Utilisation de paste pour concaténer les deux colonnes avec un underscore comme séparateur
filtered_data$Index <- paste(filtered_data$Matricule, filtered_data$Affectation, sep = "_")


# La colonne "Index" devient l'identifiant unique pour chaque ligne du dataframe pour nous aider aprés dans la vis
rownames(filtered_data) <- filtered_data$Index


# Supprimer les colonnes "Matricule", "Affectation", "Index" et "Ne_S1" si elles ne sont plus nécessaires
filtered_data <- filtered_data[, !colnames(filtered_data) %in% c("Matricule", "Affectation", "Index", "Ne_S1")]

# Afficher un aperçu des données pour s'assurer que l'index est bien défini
head(filtered_data)

# Enregistrer le dataframe modifié dans un fichier CSV avec row.names=TRUE pour inclure l'index
write.csv(filtered_data, file = "Data/Output3_indexed_data.csv", row.names = TRUE)


# Définir les coefficients des modules
coefficients <- c(SYS1 = 5, RES1 = 4, ANUM = 4, RO = 3, ORG = 3, LANG1 = 2, IGL = 5, THP = 4)

# Appliquer la pondération aux colonnes correspondantes
for (module in names(coefficients)) {
  filtered_data[[module]] <- filtered_data[[module]] * coefficients[module]
}

# Normaliser les données pondérées
normalized_data <- scale(filtered_data)

head(normalized_data)

# Enregistrer les données normalisées dans un fichier CSV
write.csv(normalized_data, file = "Data/Output4_normalized_data.csv", row.names = TRUE)


# Étape 4 : Analyse en Composantes Principales (ACP) -----------------------------------------------------------
# Appliquer l'ACP sur les données normalisées
acp_result <- PCA(normalized_data, graph = FALSE)

# Visualiser la variance expliquée par les composantes principales
fviz_screeplot(acp_result, addlabels = TRUE, ylim = c(0, 50))

# Visualiser les individus sur les deux premières composantes principales
fviz_pca_ind(acp_result,
             col.ind = "cos2", # Coloration selon la qualité de représentation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Éviter le chevauchement des étiquettes

# Visualiser les variables sur les deux premières composantes principales
fviz_pca_var(acp_result,
             col.var = "contrib", # Coloration selon la contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Biplot des individus et des variables
fviz_pca_biplot(acp_result, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#674268") # Couleur des individus


#Précisier mon position et mon binôme dans le biplot
# Ajouter une colonne pour la couleur des individus, Par défaut, tout le monde est en gris
individual_colors <- rep("gray", nrow(normalized_data))

# Modifier la couleur des points spécifiques
highlighted_points <- c("21/0298_ST2", "21/0326_ST2")
individual_colors[rownames(normalized_data) %in% highlighted_points] <- "red"

# Créer le biplot avec la coloration personnalisée
fviz_pca_biplot(acp_result, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = individual_colors, # Couleur personnalisée des individus
                palette = c("gray", "red")) # Palette utilisée



