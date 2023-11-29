library(tidyverse)
library(dplyr)
library(rgl)
data<-read.table("D:/0stuff/3D_point_clouds/60x-1p5x-SD_Z-oversampled-100nmsteps-2_3D_point_clouds_full_image_stats.txt", header = TRUE,sep = "\t")


euclidean_distance <- function(coord1,coord2) {
return(sqrt((coord2$V1 - coord1$V1)^2 + (coord2$V2 - coord1$V2)^2))
}

coord_strings <- data$Centre_of_mass

# Split and extract X and Y coordinates for each row
coordinates <- sapply(coord_strings, function(coord_string) {
  coords <- as.numeric(strsplit(coord_string, " - ")[[1]])
  cbind(x = coords[1], y = coords[2])
})

tcoordinates<-as.data.frame(t(coordinates))



    n <- nrow(tcoordinates)
  result_matrix <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Extract the vectors from the data frame
      char1 <- tcoordinates[i,]
      char2 <- tcoordinates[j,]



      result_matrix[i, j] <- euclidean_distance(char1, char2)

      # result_matrix[j - 1, i - 1] <- rand_idx
      # Print progress
      cat("Calculated hamming distance", i, "vs", j, "\n")
    }}


  give_featureidentities <- function(result_matrix) {
    result_df <- as.data.frame(result_matrix)
    groupinglist <- list()
    # Create a logical matrix for each condition
    condition_1 <- result_matrix <= 50
   grouping <- which(condition_1, arr.ind = TRUE)

    return(grouping)
  }
table<-give_featureidentities(result_matrix = result_matrix)
dataid<-data
for (i in nrow(table):1) {
combo<-table[i,]
dataid[combo["row"],3]<-dataid[combo["col"],3]
}
cellnum<-unique(dataid$CellID)
#gives each cell comparable arbitrary Z values
dataz <- dataid %>%
  group_by(CellID) %>%
  mutate(ConsecutiveID = row_number())



# Scale and add Z index columns to data
# Number of columns with names starting with "Outline_Orientated_coordinates_X_"
num_cols <- sum(grepl("Outline_OrientedCoordinates_X_", names(dataz)))
for (i in 0:(num_cols-1)){
  dataz[(paste0("Outline_OrientedCoordinates_Z_",i))] <- dataz$ConsecutiveID * 1.38
}

#initialise list
cellpointarray<-list()
#make point cloud arrays for each cell
for (i in 1:length(cellnum)){
cellrows<-dataz[dataz$CellID == paste0(cellnum[i]), ]

# Select the rows and columns
cellpointsX<- cellrows[ , paste0("Outline_OrientedCoordinates_X_", 0:(num_cols-1))]
Xresult_vector <- as.vector(unlist(apply(cellpointsX, 1, unlist)))
cellpointsY<- cellrows[ , paste0("Outline_OrientedCoordinates_Y_", 0:(num_cols-1))]
Yresult_vector <- as.vector(unlist(apply(cellpointsY, 1, unlist)))
cellpointsZ<- cellrows[ , paste0("Outline_OrientedCoordinates_Z_", 0:(num_cols-1))]
Zresult_vector <- as.vector(unlist(apply(cellpointsZ, 1, unlist)))
df<-cbind(Xresult_vector,Yresult_vector,Zresult_vector)
cellpointarray[[i]]<- df
}
df<- cellpointarray[[7]]
plot3d(x = df[,1],y = df[,2],z =  df[,3],type = "p", col = "blue", size = 3)


#############################
#optimised euclidian distance fuzzy clusterer
cellpos<-1
max_cells_per_field <- 30
celldistthreshold<- 50
while(length(unique(dataid$CellID))>max_cells_per_field){
 for (i in 1:nrow(tcoordinates)) {
   testcell<-tcoordinates[cellpos,]
   othercell<-tcoordinates[i,]
   celldist<- euclidean_distance(testcell, othercell)
   if (celldist < celldistthreshold){
     dataid[i,3]<-dataid[cellpos,3]

   }
 }
  cellpos <- cellpos + 1
}



cellnum<-unique(dataid$CellID)
#gives each cell comparable arbitrary Z values
dataz <- dataid %>%
  group_by(CellID) %>%
  mutate(ConsecutiveID = row_number())


