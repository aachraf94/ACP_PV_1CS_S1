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
    by = "Matricule", all.x = TRUE
)

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
summary(acp_result)

# Sauvegarder les résultats de l'ACP dans un fichier RDS
saveRDS(acp_result, file = "Data/Result1_acp_result.rds")

# Visualiser la variance expliquée par les composantes principales
fviz_screeplot(acp_result, addlabels = TRUE, ylim = c(0, 50))

# Visualiser les individus sur les deux premières composantes principales
fviz_pca_ind(acp_result,
    col.ind = "cos2", # Coloration selon la qualité de représentation
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
) # Éviter le chevauchement des étiquettes

# Visualiser les variables sur les deux premières composantes principales
fviz_pca_var(acp_result,
    col.var = "contrib", # Coloration selon la contribution
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
)

# Biplot des individus et des variables
fviz_pca_biplot(acp_result,
    repel = TRUE,
    col.var = "#2E9FDF", # Couleur des variables
    col.ind = "#674268"
) # Couleur des individus


# Précisier mon position et mon binôme dans le biplot
# Ajouter une colonne pour la couleur des individus, Par défaut, tout le monde est en gris
individual_colors <- rep("gray", nrow(normalized_data))

# Modifier la couleur des points spécifiques
highlighted_points <- c("21/0298_ST2", "21/0326_ST2")
individual_colors[rownames(normalized_data) %in% highlighted_points] <- "red"

# Créer le biplot avec la coloration personnalisée
fviz_pca_biplot(acp_result,
    repel = TRUE,
    col.var = "#2E9FDF", # Couleur des variables
    col.ind = individual_colors, # Couleur personnalisée des individus
    palette = c("gray", "red")
) # Palette utilisée



# Étape 5 : Colorer chaque spécialité par un coleur --------------------------------------------------------------------------------
# Extraire la spécialité de l'identifiant
specialty <- gsub(".*_", "", rownames(normalized_data))

# Générer une palette de couleurs unique pour chaque spécialité
specialty_colors <- as.factor(specialty)
palette_colors <- rainbow(length(levels(specialty_colors)))

# Créer un biplot avec des couleurs par spécialité
fviz_pca_biplot(acp_result,
    repel = TRUE,
    col.var = "#2E9FDF", # Couleur des variables
    col.ind = specialty_colors, # Couleur personnalisée des individus
    palette = palette_colors
) +
    ggtitle("Biplot ACP avec spécialités")



# Étape 6 : Gestion des données aberrantes -------------------------------------------------------------------------------

# Identifier les individus avec des scores anormaux sur les composantes principales
outlier_scores <- acp_result$ind$coord[, 1:2] # Scores des deux premières composantes
z_scores_outliers <- scale(outlier_scores) # Calcul des z-scores pour les scores ACP


is_outlier <- apply(z_scores_outliers, 1, function(x) any(abs(x) > 1.75))


# Extraire les indices des individus aberrants
outlier_indices <- which(is_outlier)

# Afficher les individus identifiés comme aberrants
outliers <- rownames(normalized_data)[outlier_indices]
print("Individus aberrants détectés :")
print(outliers)

# Supprimer les individus aberrants des données normalisées
cleaned_data <- normalized_data[!rownames(normalized_data) %in% outliers, ]

# Réappliquer l'ACP sur les données nettoyées
acp_result_cleaned <- PCA(cleaned_data, graph = FALSE)
summary(acp_result_cleaned)

# Visualiser les résultats de l'ACP après nettoyage
fviz_screeplot(acp_result_cleaned, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_ind(acp_result_cleaned,
    col.ind = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
)
fviz_pca_var(acp_result_cleaned,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
)

# Enregistrer les données nettoyées et les résultats ACP dans des fichiers
write.csv(cleaned_data, file = "Data/Output5_cleaned_data.csv", row.names = TRUE)
saveRDS(acp_result_cleaned, file = "Data/Result2_acp_cleaned.rds")




# Étape 7 : S'identifier dans le biplot  ---------------------------------------------------------------------------

# Identifier votre observation
highlight <- "21/0298_ST2"

# Créer une palette où votre observation a une couleur spécifique
custom_colors <- ifelse(rownames(cleaned_data) == highlight, "red", "gray")

# Générer le biplot avec la couleur personnalisée
fviz_pca_biplot(
    acp_result_cleaned,
    repel = TRUE,
    col.var = "#2E9FDF", # Couleur des variables
    col.ind = custom_colors, # Couleur personnalisée pour chaque individu
    palette = NULL
) +
    ggtitle("Biplot ACP avec mise en évidence de 21/0298_ST2 (Données nettoyées)")



# Étape 8 : Analyse des groupes de spécialités -------------------------------------------------------------------
# Extraire les spécialités des identifiants
specialty_cleaned <- gsub(".*_", "", rownames(cleaned_data))

# Générer une palette unique pour chaque spécialité
specialty_colors_cleaned <- as.factor(specialty_cleaned)
palette_specialty_colors <- rainbow(length(levels(specialty_colors_cleaned)))

# Créer un biplot coloré par spécialité
fviz_pca_biplot(
    acp_result_cleaned,
    repel = TRUE,
    col.var = "#ff0000", # Couleur des variables
    col.ind = specialty_colors_cleaned, # Couleur par spécialité
    palette = palette_specialty_colors
) +
    ggtitle("Biplot ACP avec les groupes colorés par spécialité (Données nettoyées)")
