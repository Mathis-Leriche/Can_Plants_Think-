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
project.dir<-"C:\\Users\\33646\\OneDrive\\Documents\\GitHub\\Artificial intelligence\\Can_Plants_Think-\\Arabidopsis"

AT_Data_tr<-read.csv(file='C:\\Users\\33646\\OneDrive\\Documents\\GitHub\\Artificial intelligence\\Can_Plants_Think-\\Arabidopsis\\AT_S2_P1.csv', header=TRUE, sep= ',')


###Pre-visualisation

summary (AT_Data_tr)

#To separate the column for each plants 
plante_1=AT_Data_tr[AT_Data_tr$track=="1",]
plante_2=AT_Data_tr[AT_Data_tr$track=="2",]
plante_3=AT_Data_tr[AT_Data_tr$track=="3",]
plante_4=AT_Data_tr[AT_Data_tr$track=="4",]

#To figure out the number of rows for each plants
nrow(plante_1)
nrow(plante_2)
nrow(plante_3)
nrow(plante_4)

###To compute the vectors from the coordinates

##Convert data to a geometry object : To make points
Base <- as.matrix(AT_Data_tr %>% select(Base.x, Base.y))
Leaf_1E <- as.matrix(AT_Data_tr %>% select(Leaf_E1.x, Leaf_E1.y))
Leaf_2I <- as.matrix(AT_Data_tr %>% select(Leaf_I2.x, Leaf_I2.y))
Leaf_3I <- as.matrix(AT_Data_tr %>% select(Leaf_I3.x, Leaf_I3.y))
Leaf_4E <- as.matrix(AT_Data_tr %>% select(Leaf_E4.x, Leaf_E4.y))

# Function to compute vectors between points
'DO NOT TOUCH THIS FUNCTION (it bites)'
compute_vectors <- function(base_point, leaf_point) {
  vector <- leaf_point - base_point
  return(vector)
}

# Compute vectors for each leaf point
vector_1E <- compute_vectors(Base, Leaf_1E)
vector_2I <- compute_vectors(Base, Leaf_2I)
vector_3I <- compute_vectors(Base, Leaf_3I)
vector_4E <- compute_vectors(Base, Leaf_4E)
vector_E <- compute_vectors(Leaf_1E, Leaf_4E)
vector_I <- compute_vectors(Leaf_2I, Leaf_3I)

# Ref vectors : don't forget to adapt the number of rows
y_axis_vector <- cbind(rep(0, 456), rep(1, 456))#reference : 7204 is the number of rows in each vectors
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
angles_degrees_1E.4E <- calculate_angles(vector_1E,vector_4E)#rosette ext
angles_degrees_2I.3I <- calculate_angles(vector_2I,vector_3I)#rosette int
angles_degrees_E.Y <- calculate_angles(vector_E,y_axis_vector) #orientation ext
angles_degrees_I.Y <- calculate_angles(vector_I,y_axis_vector) #orientation int

#stocking the angles into lists
angles_list[[1]] <- angles_degrees_1E.4E
angles_list[[2]] <- angles_degrees_2I.3I
angles_list[[3]] <- angles_degrees_E.Y
angles_list[[4]] <- angles_degrees_I.Y

#Making dataframes out of the lists
angles_df_1E.4E <- data.frame(angle_1E.4E = angles_degrees_1E.4E)
angles_df_2I.3I <- data.frame(angle_2I.3I = angles_degrees_2I.3I)
angles_df_E.Y <- data.frame(angle_E.Y = angles_degrees_E.Y)
angles_df_I.Y <- data.frame(angle_I.Y = angles_degrees_I.Y)

#Making lists out of those dataframes
angles_df_list[[1]] <- angles_df_1E.4E
angles_df_list[[2]] <- angles_df_2I.3I
angles_df_list[[3]] <- angles_df_E.Y
angles_df_list[[4]] <- angles_df_I.Y

#This combine the four list 
result_df <- do.call(cbind, angles_df_list)

#From the table created, we separate the angle from each individuals (repetition)
ind_1<-result_df %>%
  filter(row_number() %% 4 == 1) %>%
  rename_all(~paste0("ind_1_", .))

ind_2<-result_df %>%
  filter(row_number() %% 4 == 2) %>%
  rename_all(~paste0("ind_2_", .))

ind_3<-result_df %>%
  filter(row_number() %% 4 == 3) %>%
  rename_all(~paste0("ind_3_", .))

ind_4<-result_df %>%
  filter(row_number() %% 4 == 0) %>%
  rename_all(~paste0("ind_4_", .))

#We put them together by columns
Final_table<-cbind(ind_1,ind_2,ind_3,ind_4)

# We set up a time sequence 
starting_hour <- as.POSIXct("2023-12-08 16:00:00", tz = "UTC")
interval_1h <- seq(starting_hour, by = "1 hour", length.out = nrow(Final_table))

# We add the "time" column to "final_table"
Final_table$time <- interval_1h

#Add the mean of each angle

total_mean_angle <- Final_table %>%
  mutate(
    mean_1E.4E = rowMeans(select(., starts_with("ind_1_angle_1E.4E"), starts_with("ind_2_angle_1E.4E"), starts_with("ind_3_angle_1E.4E"), starts_with("ind_4_angle_1E.4E")),na.rm=TRUE),
    mean_2I.3I = rowMeans(select(., starts_with("ind_1_angle_2I.3I"), starts_with("ind_2_angle_2I.3I"), starts_with("ind_3_angle_2I.3I"), starts_with("ind_4_angle_2I.3I")),na.rm=TRUE),
    mean_E.Y = rowMeans(select(., starts_with("ind_1_angle_E.Y"), starts_with("ind_2_angle_E.Y"), starts_with("ind_3_angle_E.Y"), starts_with("ind_4_angle_E.Y")),na.rm=TRUE),
    mean_I.Y = rowMeans(select(., starts_with("ind_1_angle_I.Y"), starts_with("ind_2_angle_I.Y"), starts_with("ind_3_angle_I.Y"), starts_with("ind_4_angle_I.Y")),na.rm=TRUE)
    
  ) %>%
  select(mean_1E.4E, mean_2I.3I, mean_E.Y, mean_I.Y)

#Assemble the two tables

Final_table <- cbind(Final_table, total_mean_angle[, c("mean_1E.4E", "mean_2I.3I", "mean_E.Y","mean_I.Y")])

# Show reults
print(total_mean_angle)
print(Final_table)

#LAST CSV WITH THE RESULTS
write.csv(Final_table, "ATS2P1_angles.csv", row.names = FALSE)
