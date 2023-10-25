## Aim: 
### Analyze Yale-NUS library occupancy data in order to reveal insights into when the library is the busiest and which sections of the library are under-used.
## Objectives:
 - create a data collection method that can be used by students working for the library
 - clean data using Python
 - prepare data for analysis by exporting it in Excel format
 - analyze occupancy data using Excel and Python
 - data vizualizations using Excel
 - a monthly report with key insights and findings
 - find out which library areas are under-used by analyzing the popularity of each seat (based on occupancy count)
 
## Data collection
Data is collected hourly during library opening hours by students working for the library by manually filling in the qualtrics form. This data collection method is secure as only those with password to the form could collect data; however, it is also prone to human errors. The dataset is also secure on the qualtrics website and raw data could be downloaded in the csv format. 

## Data processing
In order to process raw data, I used Python, even though at first R was used. Data processing step involves declaring a number of functions that help turn raw data into proper and easily understandable parameters. This step involved handling missing data, checking for outliers, which usually indicated a human error during data collection and checking for duplicates. Processed data was then exported into the excel file.

## Data Analysis and Vizualizations
Data analysis involved performing a number of EDAs and vizualizing occupancy counts in various sections of the library throughout the day and how some of the metrics such as average occupancy on weekends and weekdays compares to other months. 

## Reporting and actions taken
Main insights and vizualizations are included in a monthly report prepared at the start of following month. This report is saved in the Microsoft word format and shared with the library managers. 
