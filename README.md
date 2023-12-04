# Analyzing library data occupancy data
## Aim: 
### Develop 6 stages of the Data Analytics Life Cycle with the aim of analyzing Yale-NUS library occupancy data in order to reveal insights into when the library is the busiest and which sections of the library are under-used.
## Objectives:
 - create a data collection method that can be used by students working for the library
 - clean data using Python
 - prepare data for analysis by exporting it in Excel format
 - analyze occupancy data using Excel and Python
 - data visualizations using Excel
 - a monthly report with key insights and findings
 - find out which library areas are under-used by analyzing the popularity of each seat (based on occupancy count)
 
## Data Collection
Data is collected hourly during library opening hours by students working for the library by manually filling in the qualtrics form. This data collection method is secure as only those with the password to the form could collect data; however, it is also prone to human errors. The dataset is also secure on the qualtrics website and raw data could be downloaded in the CSV format. 

## Data Cleaning
Data Cleaning is performed in Python. In this stage I handle missing data, check for duplicates and outliers, which usually indicate a human error in the data collection process.   

## Data Processing
Data Processing is also performed in Python. The data processing step involves declaring several functions that help turn raw data into proper and easily understandable parameters such as the function monthfinder, which turns raw input data into the names of the months. Processed data is then exported into the Excel file for further analysis.

## Data Analysis and Visualizations
Data analysis involved performing several EDAs and visualizing occupancy counts in various sections of the library throughout the day and how some of the metrics such as average occupancy on weekends and weekdays compare to other months. 
Below are two examples of such visualizations. 
<p align="center">
  <img width="600" height="400" src="image/total_weekday.PNG">
  <img width="600" height="400" src="image/total_weekend.PNG">
</p>

## Reporting and Actions
Main insights and visualizations are included in a monthly report prepared at the start of each month. This report is then shared with the library managers in order to optimize the usage of library spaces. Based on the insight into occupancy levels in various library spaces, I presented solutions to increase the usage of the main 4 library areas by 20% and spearheaded with the guidance of my manager a major reallocation of 30 library student assistants to different shifts based on how busy the library is during a library shift. 
