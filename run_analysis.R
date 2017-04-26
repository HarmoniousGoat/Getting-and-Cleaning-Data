# run_analysis.R
# Example file path to downloaded zip file: "/Users/Max/Desktop/R Files/"

## TRAIN DATA FRAME CREATION
make_train_df <- function(path){
        
        set <- readLines(file.path(path, "/UCI HAR Dataset/train/X_train.txt"))
        f <- index(set)
        set <- trimws(set, which = c("left"))
        set <- split(strsplit(set, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 561)
        set <- unlist(set)
        set <- as.numeric(unlist(set))
        set_mean <- tapply(set, g, mean)
        set_sd <- tapply(set, g, sd)
        
        Activity <- readLines(file.path(path, "/UCI HAR Dataset/train/y_train.txt")) # Activity Label (1-6)
        Activity <- gsub("1", "Walking", Activity) 
        Activity <- gsub("2", "Walking Upstairs", Activity) 
        Activity <- gsub("3", "Walking Downstairs", Activity) 
        Activity <- gsub("4", "Sitting", Activity) 
        Activity <- gsub("5", "Standing", Activity) 
        Activity <- gsub("6", "Laying", Activity) 
        Subject_ID <- readLines(file.path(path, "/UCI HAR Dataset/train/subject_train.txt")) # Subject ID (1-30)
        Experiment_Type <- rep("Training", times = 7352) # Add a variable to distinguish when merged.
        
        # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
        body_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt"))
        f <- index(body_acc_x)
        body_acc_x <- trimws(body_acc_x, which = c("left"))
        body_acc_x <- split(strsplit(body_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        body_acc_x <- unlist(body_acc_x)
        body_acc_x <- as.numeric(unlist(body_acc_x))
        body_acc_x_mean <- tapply(body_acc_x, g, mean)
        body_acc_x_sd <- tapply(body_acc_x, g, sd)
        
        body_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt"))
        f <- index(body_acc_y)
        body_acc_y <- trimws(body_acc_y, which = c("left"))
        body_acc_y <- split(strsplit(body_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        body_acc_y <- unlist(body_acc_y)
        body_acc_y <- as.numeric(unlist(body_acc_y))
        body_acc_y_mean <- tapply(body_acc_y, g, mean)
        body_acc_y_sd <- tapply(body_acc_y, g, sd)
        
        body_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt"))
        f <- index(body_acc_z)
        body_acc_z <- trimws(body_acc_z, which = c("left"))
        body_acc_z <- split(strsplit(body_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        body_acc_z <- unlist(body_acc_z)
        body_acc_z <- as.numeric(unlist(body_acc_z))
        body_acc_z_mean <- tapply(body_acc_z, g, mean)
        body_acc_z_sd <- tapply(body_acc_z, g, sd)
        
        # The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.
        gyro_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"))
        f <- index(gyro_acc_x)
        gyro_acc_x <- trimws(gyro_acc_x, which = c("left"))
        gyro_acc_x <- split(strsplit(gyro_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        gyro_acc_x <- unlist(gyro_acc_x)
        gyro_acc_x <- as.numeric(unlist(gyro_acc_x))
        gyro_acc_x_mean <- tapply(gyro_acc_x, g, mean)
        gyro_acc_x_sd <- tapply(gyro_acc_x, g, sd)
        
        gyro_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt"))
        f <- index(gyro_acc_y)
        gyro_acc_y <- trimws(gyro_acc_y, which = c("left"))
        gyro_acc_y <- split(strsplit(gyro_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        gyro_acc_y <- unlist(gyro_acc_y)
        gyro_acc_y <- as.numeric(unlist(gyro_acc_y))
        gyro_acc_y_mean <- tapply(gyro_acc_y, g, mean)
        gyro_acc_y_sd <- tapply(gyro_acc_y, g, sd)
        
        gyro_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt"))
        f <- index(gyro_acc_z)
        gyro_acc_z <- trimws(gyro_acc_z, which = c("left"))
        gyro_acc_z <- split(strsplit(gyro_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        gyro_acc_z <- unlist(gyro_acc_z)
        gyro_acc_z <- as.numeric(unlist(gyro_acc_z))
        gyro_acc_z_mean <- tapply(gyro_acc_z, g, mean)
        gyro_acc_z_sd <- tapply(gyro_acc_z, g, sd)
        
        # The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
        # Every row shows a 128 element vector.
        total_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt"))
        f <- index(total_acc_x)
        total_acc_x <- trimws(total_acc_x, which = c("left"))
        total_acc_x <- split(strsplit(total_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        total_acc_x <- unlist(total_acc_x)
        total_acc_x <- as.numeric(unlist(total_acc_x))
        total_acc_x_mean <- tapply(total_acc_x, g, mean)
        total_acc_x_sd <- tapply(total_acc_x, g, sd)
        
        total_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt"))
        f <- index(total_acc_y)
        total_acc_y <- trimws(total_acc_y, which = c("left"))
        total_acc_y <- split(strsplit(total_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        total_acc_y <- unlist(total_acc_y)
        total_acc_y <- as.numeric(unlist(total_acc_y))
        total_acc_y_mean <- tapply(total_acc_y, g, mean)
        total_acc_y_sd <- tapply(total_acc_y, g, sd)
        
        total_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt"))
        f <- index(total_acc_z)
        total_acc_z <- trimws(total_acc_z, which = c("left"))
        total_acc_z <- split(strsplit(total_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(7352, 128)
        total_acc_z <- unlist(total_acc_z)
        total_acc_z <- as.numeric(unlist(total_acc_z))
        total_acc_z_mean <- tapply(total_acc_z, g, mean)
        total_acc_z_sd <- tapply(total_acc_z, g, sd)
        
        ## Smash it all together into a dataframe. 
        train <- cbind(Subject_ID, Experiment_Type, Activity, set_mean, set_sd, 
                       body_acc_x_mean, body_acc_x_sd, body_acc_y_mean, body_acc_y_sd, body_acc_z_mean, body_acc_z_sd, 
                       gyro_acc_x_mean, gyro_acc_x_sd, gyro_acc_y_mean, gyro_acc_y_sd, gyro_acc_z_mean, gyro_acc_z_sd,
                       total_acc_x_mean, total_acc_x_sd, total_acc_y_mean, total_acc_y_sd, total_acc_z_mean, total_acc_z_sd)
        
        assign("Train",train,envir = .GlobalEnv)
}

## TEST DATA FRAME CREATION
make_test_df <- function(path){
        set <- readLines(file.path(path, "/UCI HAR Dataset/test/X_test.txt"))
        f <- index(set)
        set <- trimws(set, which = c("left"))
        set <- split(strsplit(set, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 561)
        set <- unlist(set)
        set <- as.numeric(unlist(set))
        set_mean <- tapply(set, g, mean)
        set_sd <- tapply(set, g, sd)
        
        Activity <- readLines(file.path(path, "/UCI HAR Dataset/test/y_test.txt")) # 1-6 is type of activity
        Activity <- gsub("1", "Walking", Activity) 
        Activity <- gsub("2", "Walking Upstairs", Activity) 
        Activity <- gsub("3", "Walking Downstairs", Activity) 
        Activity <- gsub("4", "Sitting", Activity) 
        Activity <- gsub("5", "Standing", Activity) 
        Activity <- gsub("6", "Laying", Activity) 
        Subject_ID <- readLines(file.path(path, "/UCI HAR Dataset/test/subject_test.txt")) # Subject ID
        Experiment_Type <- rep("Test", times = 2947) # Add a variable to distinguish when merged.
        
        
        # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
        body_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"))
        f <- index(body_acc_x)
        body_acc_x <- trimws(body_acc_x, which = c("left"))
        body_acc_x <- split(strsplit(body_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        body_acc_x <- unlist(body_acc_x)
        body_acc_x <- as.numeric(unlist(body_acc_x))
        body_acc_x_mean <- tapply(body_acc_x, g, mean)
        body_acc_x_sd <- tapply(body_acc_x, g, sd)
        
        body_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt"))
        f <- index(body_acc_y)
        body_acc_y <- trimws(body_acc_y, which = c("left"))
        body_acc_y <- split(strsplit(body_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        body_acc_y <- unlist(body_acc_y)
        body_acc_y <- as.numeric(unlist(body_acc_y))
        body_acc_y_mean <- tapply(body_acc_y, g, mean)
        body_acc_y_sd <- tapply(body_acc_y, g, sd)
        
        body_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt"))
        f <- index(body_acc_z)
        body_acc_z <- trimws(body_acc_z, which = c("left"))
        body_acc_z <- split(strsplit(body_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        body_acc_z <- unlist(body_acc_z)
        body_acc_z <- as.numeric(unlist(body_acc_z))
        body_acc_z_mean <- tapply(body_acc_z, g, mean)
        body_acc_z_sd <- tapply(body_acc_z, g, sd)
        
        # The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.
        gyro_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"))
        f <- index(gyro_acc_x)
        gyro_acc_x <- trimws(gyro_acc_x, which = c("left"))
        gyro_acc_x <- split(strsplit(gyro_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        gyro_acc_x <- unlist(gyro_acc_x)
        gyro_acc_x <- as.numeric(unlist(gyro_acc_x))
        gyro_acc_x_mean <- tapply(gyro_acc_x, g, mean)
        gyro_acc_x_sd <- tapply(gyro_acc_x, g, sd)
        
        gyro_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt"))
        f <- index(gyro_acc_y)
        gyro_acc_y <- trimws(gyro_acc_y, which = c("left"))
        gyro_acc_y <- split(strsplit(gyro_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        gyro_acc_y <- unlist(gyro_acc_y)
        gyro_acc_y <- as.numeric(unlist(gyro_acc_y))
        gyro_acc_y_mean <- tapply(gyro_acc_y, g, mean)
        gyro_acc_y_sd <- tapply(gyro_acc_y, g, sd)
        
        gyro_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt"))
        f <- index(gyro_acc_z)
        gyro_acc_z <- trimws(gyro_acc_z, which = c("left"))
        gyro_acc_z <- split(strsplit(gyro_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        gyro_acc_z <- unlist(gyro_acc_z)
        gyro_acc_z <- as.numeric(unlist(gyro_acc_z))
        gyro_acc_z_mean <- tapply(gyro_acc_z, g, mean)
        gyro_acc_z_sd <- tapply(gyro_acc_z, g, sd)
        
        # The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
        # Every row shows a 128 element vector.
        total_acc_x <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"))
        f <- index(total_acc_x)
        total_acc_x <- trimws(total_acc_x, which = c("left"))
        total_acc_x <- split(strsplit(total_acc_x, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        total_acc_x <- unlist(total_acc_x)
        total_acc_x <- as.numeric(unlist(total_acc_x))
        total_acc_x_mean <- tapply(total_acc_x, g, mean)
        total_acc_x_sd <- tapply(total_acc_x, g, sd)
        
        total_acc_y <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt"))
        f <- index(total_acc_y)
        total_acc_y <- trimws(total_acc_y, which = c("left"))
        total_acc_y <- split(strsplit(total_acc_y, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        total_acc_y <- unlist(total_acc_y)
        total_acc_y <- as.numeric(unlist(total_acc_y))
        total_acc_y_mean <- tapply(total_acc_y, g, mean)
        total_acc_y_sd <- tapply(total_acc_y, g, sd)
        
        total_acc_z <- readLines(file.path(path, "/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt"))
        f <- index(total_acc_z)
        total_acc_z <- trimws(total_acc_z, which = c("left"))
        total_acc_z <- split(strsplit(total_acc_z, "\\s+"), f, drop = TRUE)
        g <- gl(2947, 128)
        total_acc_z <- unlist(total_acc_z)
        total_acc_z <- as.numeric(unlist(total_acc_z))
        total_acc_z_mean <- tapply(total_acc_z, g, mean)
        total_acc_z_sd <- tapply(total_acc_z, g, sd)
        
        ## Smash it all together into a dataframe. 
        test <- cbind(Subject_ID, Experiment_Type, Activity, set_mean, set_sd, 
                      body_acc_x_mean, body_acc_x_sd, body_acc_y_mean, body_acc_y_sd, body_acc_z_mean, body_acc_z_sd, 
                      gyro_acc_x_mean, gyro_acc_x_sd, gyro_acc_y_mean, gyro_acc_y_sd, gyro_acc_z_mean, gyro_acc_z_sd,
                      total_acc_x_mean, total_acc_x_sd, total_acc_y_mean, total_acc_y_sd, total_acc_z_mean, total_acc_z_sd)
        
        assign("Test",test,envir = .GlobalEnv)
        
}

## MERGE TEST & TRAIN DATA FRAMES
merge_df <- function(){
        Smartphones_Dataset <- merge(Test, Train, all = TRUE) # Merge the data frames
        Smartphones_Dataset$Subject_ID <- as.numeric(as.character(Smartphones_Dataset$Subject_ID)) # Change Subject ID to Numeric Class for Ordering
        Smartphones_Dataset <- arrange(Smartphones_Dataset, Subject_ID) # Order by Subject ID
        Smartphones_Dataset <- rename(Smartphones_Dataset, Set_Mean = set_mean, Set_SD = set_sd, 
                                      Body_Acceleration_X.Axis_Mean = body_acc_x_mean, Body_Acceleration_X.Axis_SD = body_acc_x_sd,
                                      Body_Acceleration_Y.Axis_Mean = body_acc_y_mean, Body_Acceleration_Y.Axis_SD = body_acc_y_sd,
                                      Body_Acceleration_Z.Axis_Mean = body_acc_z_mean, Body_Acceleration_Z.Axis_SD = body_acc_z_sd,
                                      Angular_Velocity_X.Axis_Mean =  gyro_acc_x_mean, Angular_Velocity_X.Axis_SD = gyro_acc_x_sd,
                                      Angular_Velocity_Y.Axis_Mean = gyro_acc_y_mean, Angular_Velocity_Y.Axis_SD = gyro_acc_y_sd,
                                      Angular_Velocity_Z.Axis_Mean = gyro_acc_z_mean, Angular_Velocity_Z.Axis_SD = gyro_acc_z_sd,
                                      Total_Acceleration_X.Axis_Mean = total_acc_x_mean, Total_Acceleration_X.Axis_SD = total_acc_x_sd,
                                      Total_Acceleration_Y.Axis_Mean = total_acc_y_mean, Total_Acceleration_Y.Axis_SD = total_acc_y_sd,
                                      Total_Acceleration_Z.Axis_Mean = total_acc_z_mean, Total_Acceleration_Z.Axis_SD = total_acc_z_sd
        )
        Smartphones_Dataset$Set_Mean <- as.numeric(as.character(Smartphones_Dataset$Set_Mean))
        Smartphones_Dataset$Set_SD <- as.numeric(as.character(Smartphones_Dataset$Set_SD))
        Smartphones_Dataset$Body_Acceleration_X.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_X.Axis_Mean))
        Smartphones_Dataset$Body_Acceleration_X.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_X.Axis_SD))
        Smartphones_Dataset$Body_Acceleration_Y.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_Y.Axis_Mean))
        Smartphones_Dataset$Body_Acceleration_Y.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_Y.Axis_SD))
        Smartphones_Dataset$Body_Acceleration_Z.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_Z.Axis_Mean))
        Smartphones_Dataset$Body_Acceleration_Z.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Body_Acceleration_Z.Axis_SD))
        Smartphones_Dataset$Angular_Velocity_X.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_X.Axis_Mean))
        Smartphones_Dataset$Angular_Velocity_X.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_X.Axis_SD))
        Smartphones_Dataset$Angular_Velocity_Y.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_Y.Axis_Mean))
        Smartphones_Dataset$Angular_Velocity_Y.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_Y.Axis_SD))
        Smartphones_Dataset$Angular_Velocity_Z.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_Z.Axis_Mean))
        Smartphones_Dataset$Angular_Velocity_Z.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Angular_Velocity_Z.Axis_SD))
        Smartphones_Dataset$Total_Acceleration_X.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_X.Axis_Mean))
        Smartphones_Dataset$Total_Acceleration_X.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_X.Axis_SD))
        Smartphones_Dataset$Total_Acceleration_Y.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_Y.Axis_Mean))
        Smartphones_Dataset$Total_Acceleration_Y.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_Y.Axis_SD))
        Smartphones_Dataset$Total_Acceleration_Z.Axis_Mean <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_Z.Axis_Mean))
        Smartphones_Dataset$Total_Acceleration_Z.Axis_SD <- as.numeric(as.character(Smartphones_Dataset$Total_Acceleration_Z.Axis_SD))
        
        assign("Smartphones_Dataset",Smartphones_Dataset,envir = .GlobalEnv)
}

#---------------------------------------------------------------------------

## CREATE AN AVERAGE OF EACH VARIABLE, FOR EACH ACTIVITY, AND EACH SUBJECT

# Averages of Both
avg_both <- function(){
        grouped <- group_by(Smartphones_Dataset, Activity, Subject_ID)
        Average_Activity.Subject <- summarize(
                grouped, Set_Mean = mean(Set_Mean), Set_SD = mean(Set_SD),
                Body_Acceleration_X.Axis_Mean = mean(Body_Acceleration_X.Axis_Mean), Body_Acceleration_X.Axis_SD = mean(Body_Acceleration_X.Axis_SD),
                Body_Acceleration_Y.Axis_Mean = mean(Body_Acceleration_Y.Axis_Mean), Body_Acceleration_Y.Axis_SD = mean(Body_Acceleration_Y.Axis_SD),
                Body_Acceleration_Z.Axis_Mean = mean(Body_Acceleration_Z.Axis_Mean), Body_Acceleration_Z.Axis_SD = mean(Body_Acceleration_Z.Axis_SD),
                Angular_Velocity_X.Axis_Mean =  mean(Angular_Velocity_X.Axis_Mean), Angular_Velocity_X.Axis_SD = mean(Angular_Velocity_X.Axis_SD),
                Angular_Velocity_Y.Axis_Mean = mean(Angular_Velocity_Y.Axis_Mean), Angular_Velocity_Y.Axis_SD = mean(Angular_Velocity_Y.Axis_SD),
                Angular_Velocity_Z.Axis_Mean = mean(Angular_Velocity_Z.Axis_Mean), Angular_Velocity_Z.Axis_SD = mean(Angular_Velocity_Z.Axis_SD),
                Total_Acceleration_X.Axis_Mean = mean(Total_Acceleration_X.Axis_Mean), Total_Acceleration_X.Axis_SD = mean(Total_Acceleration_X.Axis_SD),
                Total_Acceleration_Y.Axis_Mean = mean(Total_Acceleration_Y.Axis_Mean), Total_Acceleration_Y.Axis_SD = mean(Total_Acceleration_Y.Axis_SD),
                Total_Acceleration_Z.Axis_Mean = mean(Total_Acceleration_Z.Axis_Mean), Total_Acceleration_Z.Axis_SD = mean(Total_Acceleration_Z.Axis_SD))
        assign("Average_Activity.Subject",Average_Activity.Subject,envir = .GlobalEnv)
}

# Averages of Activity Only
avg_activity <- function(){
        grouped_act <- group_by(Smartphones_Dataset, Activity)
        Average_Activity <- summarize(
                grouped_act, Set_Mean = mean(Set_Mean), Set_SD = mean(Set_SD),
                Body_Acceleration_X.Axis_Mean = mean(Body_Acceleration_X.Axis_Mean), Body_Acceleration_X.Axis_SD = mean(Body_Acceleration_X.Axis_SD),
                Body_Acceleration_Y.Axis_Mean = mean(Body_Acceleration_Y.Axis_Mean), Body_Acceleration_Y.Axis_SD = mean(Body_Acceleration_Y.Axis_SD),
                Body_Acceleration_Z.Axis_Mean = mean(Body_Acceleration_Z.Axis_Mean), Body_Acceleration_Z.Axis_SD = mean(Body_Acceleration_Z.Axis_SD),
                Angular_Velocity_X.Axis_Mean =  mean(Angular_Velocity_X.Axis_Mean), Angular_Velocity_X.Axis_SD = mean(Angular_Velocity_X.Axis_SD),
                Angular_Velocity_Y.Axis_Mean = mean(Angular_Velocity_Y.Axis_Mean), Angular_Velocity_Y.Axis_SD = mean(Angular_Velocity_Y.Axis_SD),
                Angular_Velocity_Z.Axis_Mean = mean(Angular_Velocity_Z.Axis_Mean), Angular_Velocity_Z.Axis_SD = mean(Angular_Velocity_Z.Axis_SD),
                Total_Acceleration_X.Axis_Mean = mean(Total_Acceleration_X.Axis_Mean), Total_Acceleration_X.Axis_SD = mean(Total_Acceleration_X.Axis_SD),
                Total_Acceleration_Y.Axis_Mean = mean(Total_Acceleration_Y.Axis_Mean), Total_Acceleration_Y.Axis_SD = mean(Total_Acceleration_Y.Axis_SD),
                Total_Acceleration_Z.Axis_Mean = mean(Total_Acceleration_Z.Axis_Mean), Total_Acceleration_Z.Axis_SD = mean(Total_Acceleration_Z.Axis_SD))
        assign("Average_Activity",Average_Activity,envir = .GlobalEnv)
}

# Averages of Subjects Only
avg_subjects <- function(){
        grouped_subject <- group_by(Smartphones_Dataset, Subject_ID)
        Average_Subject <- summarize(grouped_subject, Set_Mean = mean(Set_Mean), Set_SD = mean(Set_SD),
                                     Body_Acceleration_X.Axis_Mean = mean(Body_Acceleration_X.Axis_Mean), Body_Acceleration_X.Axis_SD = mean(Body_Acceleration_X.Axis_SD),
                                     Body_Acceleration_Y.Axis_Mean = mean(Body_Acceleration_Y.Axis_Mean), Body_Acceleration_Y.Axis_SD = mean(Body_Acceleration_Y.Axis_SD),
                                     Body_Acceleration_Z.Axis_Mean = mean(Body_Acceleration_Z.Axis_Mean), Body_Acceleration_Z.Axis_SD = mean(Body_Acceleration_Z.Axis_SD),
                                     Angular_Velocity_X.Axis_Mean =  mean(Angular_Velocity_X.Axis_Mean), Angular_Velocity_X.Axis_SD = mean(Angular_Velocity_X.Axis_SD),
                                     Angular_Velocity_Y.Axis_Mean = mean(Angular_Velocity_Y.Axis_Mean), Angular_Velocity_Y.Axis_SD = mean(Angular_Velocity_Y.Axis_SD),
                                     Angular_Velocity_Z.Axis_Mean = mean(Angular_Velocity_Z.Axis_Mean), Angular_Velocity_Z.Axis_SD = mean(Angular_Velocity_Z.Axis_SD),
                                     Total_Acceleration_X.Axis_Mean = mean(Total_Acceleration_X.Axis_Mean), Total_Acceleration_X.Axis_SD = mean(Total_Acceleration_X.Axis_SD),
                                     Total_Acceleration_Y.Axis_Mean = mean(Total_Acceleration_Y.Axis_Mean), Total_Acceleration_Y.Axis_SD = mean(Total_Acceleration_Y.Axis_SD),
                                     Total_Acceleration_Z.Axis_Mean = mean(Total_Acceleration_Z.Axis_Mean), Total_Acceleration_Z.Axis_SD = mean(Total_Acceleration_Z.Axis_SD))
        assign("Average_Subject",Average_Subject,envir = .GlobalEnv)
}




















