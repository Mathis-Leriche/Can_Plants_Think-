####Data analysis
'this script aim to compute the angle between the vectors obtained with the 
coordinates given by SLEAP'

###libraries
install.packages("dplyr")
install.packages("magrittr")
install.packages("geometry")

library(dplyr)
library(magrittr)
library(geometry)
library(tidyverse)

###data importation
project.dir<-"D:\Timelapse\Sunflowers_Training" #Don't forget to change the directory depending on the file you are working on

HA_Data_tr<-read.csv(file='HA_Data_training.csv', header=TRUE, sep= ';')

###Pre-visualisation

summary (HA_Data_tr)

#To separate the column for each plants 
plante_1=HA_Data_tr[AT_Data_tr$track=="track_0",]
plante_2=HA_Data_tr[AT_Data_tr$track=="track_1",]
plante_3=HA_Data_tr[AT_Data_tr$track=="track_2",]

#To figure out the number of rows for each plants
nrow(plante_1)
nrow(plante_2)
nrow(plante_3)

###To compute the vectors from the coordinates

##Convert data to a geometry object : To make points
Base <- as.matrix(AT_Data_tr %>% select(Base_HA.x, Base_HA.y))
L1_G <- as.matrix(AT_Data_tr %>% select(L1_G.x, L1_G.y))
L1_D <- as.matrix(AT_Data_tr %>% select(L1_D.x, L1_D.y))
L2_G <- as.matrix(AT_Data_tr %>% select(L2_G.x, L2_G.y))
L2_D <- as.matrix(AT_Data_tr %>% select(L2_D.x, L2_D.y))
IN1_HA <- as.matrix(AT_Data_tr %>% select(IN1_HA.x, IN1_HA.y))
IN2_HA <- as.matrix(AT_Data_tr %>% select(IN2_HA.x, IN2_HA.y))

# Function to compute vectors between points
'DO NOT TOUCH THIS FUNCTION (it bites)'
compute_vectors <- function(base_point, leaf_point) {
  vector <- leaf_point - base_point
  return(vector)
}

# Compute vectors for each leaf point
vector_L1G <- compute_vectors(IN1_HA, L1_G)#Leaf 1G
vector_L1D <- compute_vectors(IN1_HA, L1_D)#Leaf 1D
vector_L2G <- compute_vectors(IN2_HA, L2_G)#Leaf 2G
vector_L2D <- compute_vectors(IN2_HA, L2_D)#Leaf 2D
vector_1 <- compute_vectors(L1_G, L1_D)#Horizontal vector between L1G and L1D
vector_2 <- compute_vectors(L2_D, L2_D)#Horizontal vector between L2G and L2D
vector_IN <- compute_vectors(IN1_HA, IN2_HA)#Between IN1 and IN2

# Ref vectors : don't forget to adapt the number of rows
y_axis_vector <- cbind(rep(0, 7204), rep(1, 7204))#reference : 7204 is the number of rows in each vectors
colnames(y_axis_vector) <- c("OY.x", "OY.y")

# Print the results (Not mandatory)
#print("Vectors:")
#print(vector_1E)
#print(vector_2I)
#print(vector_3I)
#print(vector_4E)
#print(y_axis_vector)

##Angles

# Function to calculate angles between a vector and a reference vector
calculate_angles <- function(vector, reference_vector) {
  # Calculate angles between vector and reference vector
  angles_radians <- atan2(reference_vector[, 2], reference_vector[, 1]) - atan2(vector[, 2], vector[, 1])
  
  # Convert angles to degrees
  angles_degrees <- angles_radians * (180 / pi)
  
  return(angles_degrees)
}

# Assuming you have matrices named vector1, vector2, vector3, ...
# and you want to choose a reference vector for each calculation

# List to store angles
angles_list <- list()

# List to store data frames
angles_df_list <- list()

#Computing of the angle
angles_degrees_L1G.L1D <- calculate_angles(vector_L1G,vector_L1D)#Angle between leaves 1
angles_degrees_L2G.L2D <- calculate_angles(vector_L2G,vector_L2D)#Angle between leaves 2
angles_degrees_L1.Y <- calculate_angles(vector_1,y_axis_vector)#Orientation of leaves 1
angles_degrees_L2.Y <- calculate_angles(vector_2,y_axis_vector)#Orientation of leaves 2
angles_degrees_IN.Y <- calculate_angles(vector_IN,y_axis_vector)#Between Y_axis and IN

#stocking the angles into lists
angles_list[[1]] <- angles_degrees_L1G.L1D
angles_list[[2]] <- angles_degrees_L2G.L2D
angles_list[[3]] <- angles_degrees_L1.Y
angles_list[[4]] <- angles_degrees_L2.Y
angles_list[[5]] <- angles_degrees_IN.Y

#Making dataframes out of the lists
angles_df_L1G.L1D <- data.frame(Vector = paste("angle_L1G.L1D"), Angle = angles_degrees_L1G.L1D)
angles_df_L2G.L2D <- data.frame(Vector = paste("angle_L2G.L2D"), Angle = angles_degrees_L2G.L2D)
angles_df_L1.Y <- data.frame(Vector = paste("angle_L1.Y"), Angle = angles_degrees_L1.Y)
angles_df_L2.Y <- data.frame(Vector = paste("angle_L2.Y"), Angle = angles_degrees_L2.Y)
angles_df_IN.Y <- data.frame(Vector = paste("angle_IN.Y"), Angle = angles_degrees_IN.Y)

#Making lists out of those dataframes
angles_df_list[[1]] <- angles_df_L1G.L1D
angles_df_list[[2]] <- angles_df_L2G.L2D
angles_df_list[[3]] <- angles_df_L1.Y
angles_df_list[[4]] <- angles_df_L2.Y
angles_df_list[[5]] <- angles_df_IN.Y

#This combine the five list 
result_df <- do.call(cbind, angles_df_list)

#We put our data in a csv file so as to analyze it later in the code
write.csv(result_df, "angles_result.csv", row.names = FALSE)

##Reorganization of the data so as to have what we want
#New importation
HA_angles_data<-read.csv(file='angles_result.csv', header=TRUE, sep= ',')

#We put the name of the angle in the label of each angle columns
HA_angle_table <- HA_angles_data %>%
  pivot_wider(names_from = Vector, values_from = Angle)

#Now we have an angle by row instead of a list of angles in a single row
HA_angle_table <- HA_angle_table %>%
  unnest(cols = starts_with("Angle"))

#From the table created, we separate the angle from each individuals (repetition)
ind_1<-HA_angle_table %>%
  slice(1:1801)%>%
  rename_all(~paste0("ind_1_", .))

ind_2<-HA_angle_table %>%
  slice(1802:3602)%>%
  rename_all(~paste0("ind_2_", .))

ind_3<-HA_angle_table %>%
  slice(3603:5403)%>%
  rename_all(~paste0("ind_3_", .))

#We put them together by columns
Final_table<-cbind(ind_1,ind_2,ind_3)

# We set up a time sequence 
starting_hour <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
interval_30s <- seq(starting_hour, by = "30 sec", length.out = nrow(Final_table))

# We add the "time" column to "final_table"
Final_table$time <- interval_30s

#Add the mean of each angle

total_mean_angle <- Final_table %>%
  mutate(
    mean_L1G.L1D = rowMeans(select(., starts_with("ind_1_angle_L1G.L1D"), starts_with("ind_2_angle_L1G.L1D"), starts_with("ind_3_angle_L1G.L1D"),na.rm=TRUE)),
    mean_L2G.L2D = rowMeans(select(., starts_with("ind_1_angles_L2G.L2D"), starts_with("ind_2_angles_L2G.L2D"), starts_with("ind_3_anglesL2G.L2D"),na.rm=TRUE)),
    mean_1.Y = rowMeans(select(., starts_with("ind_1_angle_1.Y"), starts_with("ind_2_angle_1.Y"), starts_with("ind_3_angle_1.Y"),na.rm=TRUE)),
    mean_2.Y = rowMeans(select(., starts_with("ind_1_angle_2.Y"), starts_with("ind_2_angle_2.Y"), starts_with("ind_3_angle_2.Y"),na.rm=TRUE)),
    mean_IN.Y = rowMeans(select(., starts_with("ind_1_angle_IN.Y"), starts_with("ind_2_angle_IN.Y"), starts_with("ind_3_angle_IN.Y"),na.rm=TRUE))
  ) %>%
  select(mean_1E.4E, mean_2I.3I, mean_E.Y, mean_I.Y)

#Assemble the two tables

Final_table <- cbind(Final_table, total_mean_angle[, c("mean_L1G.L1D ", "mean_L2G.L2D", "mean_1.Y","mean_2.Y" , "mean_IN.Y")])

# Show reults
print(total_mean_angle)
print(Final_table)

#LAST CSV WITH THE RESULTS
#Watch the name of the csv !!!
write.csv(Final_table, "HAS2P2_angles_result_training.csv", row.names = FALSE)










