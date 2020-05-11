# Scripts

In this folder I will keep my R Scripts.

`data_dic`: The script I used to clean the data dictionary. It was a multi-page `xlsx` sheet, so I saved each into a separate `rds`. The College Scorecard website makes the data dictionary available for downloading [here](https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx). 

`data_memo_exp`: The script I used to do a bit of exploration for my data memo. I used the uncleaned [Institutions](https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv) and [Field of Study](https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Field-Data-Elements.csv) data for some of my analysis. I also used the cleaned data dictionary.

`main-data-cleaning`: The script I used to clean and store my project data set. I used the [Institutions](https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv) data set in conjunction with the cleaned data dictionary. My method consisted of hand choosing around 121 variables to keep out of the initial 1,982. I also filtered the data to exclude all for-profit institutions and institutions that did not primarily give 4 year degrees. 