
ReadMe
Guide to R script: run_analysis.R

=============================================================================
Data Used:
Human Activity Recognition Using Smartphones Dataset (Version 1.0)
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Data
set.zip
=============================================================================
Required R Packages:
- dtplyr
- dplyr
- plyr
- zoo

=============================================================================
Overview:

The run_analysis.R script allows you to take the Human Activity Recognition
Using Smartphones Dataset (Version 1.0), clean it, and summarize it. In the
script file you will find six functions. They are:

— make_train_df(path)
— make_test_df(path)
— merge_df()

— avg_both()
— avg_activity()
— avg_subjects()

The first three functions first retrieve both main datasets, then tidy them,
then merge them into one final tidy data frame. The second three functions take
the merged, tidy data frame and average the variables based on activity,
subjects, or both. 


Descriptions of what the functions do:
============================================================================

—make_train_df(path): 
This function takes a file path to the downloaded zip file as an argument. 
It reads all of the “training” data into R from the dataset. It then changes
the activity labels from numbers to their corresponding descriptions and adds a
variable tagging all rows as “Training” data to maintain clarity after merging.
Many variables contain large strings containing number vectors as values. In
order to calculate the mean and standard deviation of each of these vectors,
make_train_df() takes each string from each variable, parses each into separate
grouped strings, converts these new strings into numeric values, maintains the
original groupings, and then takes both the mean and standard deviation of
these numeric vectors, returning the answer in new columns. This process is
done for each number variable 	in the dataset, except for Subject_ID.The
resulting data frame is called “Train” and is ordered by Subject ID in
ascending order.  

— make_test_df(path):
This function does the exact same as make_train_df(), but for the “test” data.
The only difference is that it adds a variable tagging all rows as “Test” in 
order to maintain clarity after merging. The resulting data frame is called 
“Test”.

— merge_df():
This function takes no arguments. When executed, it takes the data frames 
“train” and “test” and merges them into a single data frame called 
“Smartphones_Dataset”. It takes each column variable that has number values and
changes the class from “factor” to “numeric”. It then renames the column names 
to be more descriptive of the variables. Finally it orders the data frame by 
Subject ID in ascending order. If you get an error in rename, reload dtplyr and 
dplyr packages... they are known to be buggy and interfere with other packages.

— avg_both():
This function takes no arguments. When executed, it groups the data frame 
created by merge_df() first by Activity and then by Subject ID. It returns a 
new data frame called “Average_Activity.Subject” that contains the mean values 
for each column in “Smartphones_Dataset” grouped first by Activity and then by 
Subject ID.

— avg_activity():
This function takes no arguments. When executed, it groups the data created by 
merge_df() by Activity. It returns a new data frame called “Average_Activity” 
that contains the mean values for each column in “Smartphones_Dataset” grouped 
by Activity.

— avg_subjects():
This function takes no arguments. When executed, it groups the data created by 
merge_df() by Subject ID. It returns a new data frame called “Average_Subjects”
that contains the mean values for each column in “Smartphones_Dataset” grouped 
by Subject ID.

