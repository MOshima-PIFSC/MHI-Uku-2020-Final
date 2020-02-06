library(FishLife)

vignette("tutorial","FishLife")

Predict = Plot_taxa( Search_species(Genus="Aprion",Species="virescens")$match_taxonomy, mfrow=c(2,2) )

Predict[[1]]$Mean_pred



