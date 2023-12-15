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
project.dir<-"C:\\Users\\33646\\OneDrive\\Documents\\GitHub\\Artificial intelligence\\Can_Plants_Think-\\Tabacco" #Don't forget to change the directory depending on the file you are working on

AT_Data_tr<-read.csv(file='NT_S2_P3.csv', header=TRUE, sep= ',')


###Pre-visualisation

summary (AT_Data_tr)

#To separate the column for each plants 
plante_1=AT_Data_tr[AT_Data_tr$track=="1",]
plante_2=AT_Data_tr[AT_Data_tr$track=="2",]
plante_3=AT_Data_tr[AT_Data_tr$track=="3",]


#To figure out the number of rows for each plants
nrow(plante_1)
nrow(plante_2)
nrow(plante_3)

###To compute the vectors from the coordinates

##Convert data to a geometry object : To make points
Base <- as.matrix(AT_Data_tr %>% select(Base_NT.x, Base_NT.y))
L1 <- as.matrix(AT_Data_tr %>% select(L1.x, L1.y))
L2 <- as.matrix(AT_Data_tr %>% select(L2.x, L2.y))
L3 <- as.matrix(AT_Data_tr %>% select(L3.x, L3.y))
IN1 <- as.matrix(AT_Data_tr %>% select(IN1_NT.x, IN1_NT.y))
IN2 <- as.matrix(AT_Data_tr %>% select(IN2_NT.x, IN2_NT.y))
IN3 <- as.matrix(AT_Data_tr %>% select(IN3_NT.x, IN3_NT.y))

# Function to compute vectors between points
'DO NOT TOUCH THIS FUNCTION (it bites)'
compute_vectors <- function(base_point, leaf_point) {
  vector <- leaf_point - base_point
  return(vector)
}

# Compute vectors for each leaf point
vector_L1 <- compute_vectors(Base, L1)
vector_L2 <- compute_vectors(Base, L2)
vector_L3 <- compute_vectors(Base, L3)
vector_IN1.2 <- compute_vectors(IN1, IN2)
vector_IN2.3 <- compute_vectors(IN2,IN3)
vector_INB.1 <- compute_vectors(Base, IN1)

# Ref vectors : don't forget to adapt the number of rows
y_axis_vector <- cbind(rep(0, 282), rep(1, 282))#reference : 7204 is the number of rows in each vectors
colnames(y_axis_vector) <- c("OY.x", "OY.y")

# Print the results (Not mandatory)
#print("Vectors:")
#print(vector_L1)
#print(vector_L2)
#print(vector_L3)
#print(vector_IN1.2)
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
angles_degrees_L1.Y <- calculate_angles(vector_L1,y_axis_vector)#rosette ext
angles_degrees_L2.Y <- calculate_angles(vector_L2,y_axis_vector)#rosette int
angles_degrees_L3.Y <- calculate_angles(vector_L3,y_axis_vector) #orientation ext
angles_degrees_IN23.Y <- calculate_angles(vector_IN2.3,y_axis_vector) #orientation int

#stocking the angles into lists
angles_list[[1]] <- angles_degrees_L1.Y
angles_list[[2]] <- angles_degrees_L2.Y
angles_list[[3]] <- angles_degrees_L3.Y
angles_list[[4]] <- angles_degrees_IN23.Y

#Making dataframes out of the lists
angles_df_L1.Y <- data.frame(angle_L1.Y = angles_degrees_L1.Y)
angles_df_L2.Y <- data.frame(angle_L2.Y = angles_degrees_L2.Y)
angles_df_L3.Y <- data.frame(angle_L3.Y = angles_degrees_L3.Y)
angles_df_IN23.Y <- data.frame(angle_IN23.Y = angles_degrees_IN23.Y)

#Making lists out of those dataframes
angles_df_list[[1]] <- angles_df_L1.Y
angles_df_list[[2]] <- angles_df_L2.Y
angles_df_list[[3]] <- angles_df_L3.Y
angles_df_list[[4]] <- angles_df_IN23.Y

#This combine the four list 
result_df <- do.call(cbind, angles_df_list)

#From the table created, we separate the angle from each individuals (repetition)
ind_1<-result_df %>%
  slice(1:94)%>%
  rename_all(~paste0("ind_1_", .))

ind_2<-result_df %>%
  slice(95:189)%>%
  rename_all(~paste0("ind_2_", .))

ind_3<-result_df %>%
  slice(189:282)%>%
  rename_all(~paste0("ind_3_", .))

#We put them together by columns
Final_table<-cbind(ind_1,ind_2,ind_3)

# We set up a time sequence 
starting_hour <- as.POSIXct("2023-12-08 16:00:00", tz = "UTC")
interval_1h <- seq(starting_hour, by = "1 hour", length.out = nrow(Final_table))

# We add the "time" column to "final_table"
Final_table$time <- interval_1h

#Add the mean of each angle

total_mean_angle <- Final_table %>%
  mutate(
    mean_L1.Y = rowMeans(select(., starts_with("ind_1_angle_L1.Y"), starts_with("ind_2_angle_L1.Y"), starts_with("ind_3_angle_L1.Y")), na.rm = TRUE),
    mean_L2.Y = rowMeans(select(., starts_with("ind_1_angle_L2.Y"), starts_with("ind_2_angle_L2.Y"), starts_with("ind_3_angle_L2.Y")), na.rm = TRUE),
    mean_L3.Y = rowMeans(select(., starts_with("ind_1_angle_L3.Y"), starts_with("ind_2_angle_L3.Y"), starts_with("ind_3_angle_L3.Y")), na.rm = TRUE),
    mean_IN23.Y = rowMeans(select(., starts_with("ind_1_angle_IN23.Y"), starts_with("ind_2_angle_IN23.Y"), starts_with("ind_3_angle_IN23.Y")), na.rm = TRUE)
  ) %>%
  select(mean_L1.Y, mean_L2.Y, mean_L3.Y, mean_IN23.Y)

#Assemble the two tables

Final_table <- cbind(Final_table, total_mean_angle[, c("mean_L1.Y", "mean_L2.Y", "mean_L3.Y","mean_IN23.Y")])

# Show reults
print(total_mean_angle)
print(Final_table)

#LAST CSV WITH THE RESULTS
write.csv(Final_table, "Angles_NTS2P3.csv", row.names = FALSE)