# Semantix_DataScience_Test

The repository contains the files below:

1. 01 Import Data into aws postgreSQL.sql
2. 02 Load data in R.R
3. 03 Question 1A.R
4. 04 Question 1B.R
5. 05 Question 2.R
6. 06 Question 3.R
7. 07 Question 4.R
8. 08 Question 5.R
9. 09 Question 6.R

All analysis can be done using the R scripts alone given the dataset was simple and small; however working with a reference database is more representative of what would be considered best practice for larger and more complex data assets.  This being said, if a reader wishes to only use R, all information presented in the deck was produced there as well.

In addition, some simple data analysis and visualisation were done using google sheets. 

Those can be found at the link below: https://docs.google.com/spreadsheets/d/1BUjuyxspgMEwV7A666VHJqGdR7E9aSb8i53hF1NEkmg/edit#gid=1283633795

The answers to the test can be read in the presentation below available in English and Portuguese.

English: https://docs.google.com/presentation/d/17zOlXk-AypkE7ygAY5-w2_jewbtOSkpD9DRx6McM1_M/edit#slide=id.g5d5dfdcc4a_0_164

Portuguese: https://docs.google.com/presentation/d/1OuhUx9JR-fv6uWUV0TnSOqD8gZFg8DNWU9VW9TYi3mM/edit#slide=id.g5d7a74c385_0_19

Information about running the scripts:

1. [01 Import Data into aws postgreSQL.sql] Pre-requisite to have a running postgreSQL database. Some psql commands can be found in comments and must be run in the terminal directly due to some Amazon RDS user restrictions to import the data.

2. [02 Load data in R.R] must be run before any other R scripts.

3. Scripts[3-8] can be run independently after running script [02 Load data in R.R].

4. Scripts [02 Load data in R.R] and [04 Question 1B.R] must be run before running script [09 Question 6.R]

Every file is commented and contains both outputs and various data checks to provide confidence in the presented results. As such, those files were designed to be run in a Gui such as pgadmin for SQL files or RStudio for .R files.

