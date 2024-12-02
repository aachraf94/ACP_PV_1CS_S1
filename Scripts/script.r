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
file_path1 <- paste0(current_dir, "/Data/LISTE_Affectations_2022.xlsx")
file_path2 <- paste0(current_dir, "/Data/PV_1CS_2021_S1.xlsx")

# Charger les données depuis les fichiers
data <- read.xlsx(file_path2)
affectation_data <- read.xlsx(file_path1)

head(data)
head(affectation_data)


# Fusionner les deux jeux de données sur "Matricule" avec left join
merged_data <- merge(data, affectation_data, by = "Matricule", all.x = TRUE)

# Réorganiser la colonne "Spécialité" pour qu'elle soit en deuxième position
merged_data <- merged_data[, c("Matricule", "Spécialité", setdiff(names(merged_data), c("Matricule", "Spécialité")))]

# Afficher un aperçu des données fusionnées
head(merged_data)

# Save marged_data as csv in data folder
write.csv(merged_data, file = "Data/Output1_merged_data.csv", row.names = FALSE)





# Étape 3 : Prétraitement des données ------------------------------------------------------------------------------

# Copier les données fusionnées dans une nouvelle variable pour prétraitement
processed_data <- merged_data

# Supprimer les colonnes inutiles "Moy_S1", "Rang_S1" 
processed_data <- processed_data[, !(names(processed_data) %in% c("Moy_S1", "Rang_S1", "Ne_S1"))]

# Remplacer les valeurs manquantes dans la colonne "Spécialité" par "Non Assignée"
processed_data$Spécialité[is.na(processed_data$Spécialité)] <- "Info Non Disponible"

# Normaliser uniquement les colonnes numériques pour l'ACP (exclure les qualitatives comme "Matricule", "Spécialité", "Groupe_S1")
numeric_cols <- sapply(processed_data[, -c(1:3)], is.numeric)
processed_data[, -c(1:3)][, numeric_cols] <- scale(processed_data[, -c(1:3)][, numeric_cols])

# Afficher un aperçu des données après prétraitement
head(processed_data)

# Enregistrer les données prétraitées dans un fichier CSV
write.csv(processed_data, file = "Data/Output2_preprocessed_data.csv", row.names = FALSE)
