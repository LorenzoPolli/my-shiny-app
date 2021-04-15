# MyShinyApp
It includes my project in R for the exam of the R module from the Coding for Data Science Master course.

The idea behind the Shiny App I developed is to give the possibility to visualize some interactive data about a **US Superstore Sales dataset**.
The dataset is composed by **18 columns** representing the variables and **9800 rows** representing the data.

The main columns analyzed are **State**, **Region**, **Order Date**, **Segment** and **Sales**. 
The goal is to give the user the possibility to interact with **three input widgets**: the first one allows him/her to **choose a specific Segment** among three choices, the second to select **the Region** corresponding to a determined set of US States, **and** the last one to select **the range of dates** which corresponds to the period of sales.

A **bar chart** representing the Aggregate Sales by US State for the selected segment is plotted on the top right part of the page.
A **line graph** representing the Aggregate Regional Sales by Date is plotted on the bottom right part of the page.

A few **analytics results** are added to the bottom left part of the page in order to support the results displayed by the two plots.


Packages used: `shiny`, `ggplot2`, `tidyverse`, `dplyr`, `extrafont`.
In case you need to install one of them, please, just run on RStudio the code `install.packages(packagename)`, where `packagename` correspond to the name of one of the packages mentioned above.
i.e. One of the package you may need to install is `extrafont`. After having installed it, to make it effective in Rstudio, run the command `font_import()`. This will load in the extrafont database a set of the most common fonts. 

#### To guide you through the application I prepared a few examples. 
- By default, the Shiny App will show results about Segment = "Corporate", Region = "Central" and Date Range that goes from "2015-01-03" to "2018-12-30" (the very first and very last dates in the dataset).
- Otherwise, you can select Segment = "Consumer", type in the Region box = "East", and select the Date Range from "2015-01-01" to "2016-01-01".
- Or even Segment = "Home Office", Region = "West", and Date Range from "2017-06-15" to "2018-08-15".

Finally, I invite you to select the input as you better prefer, and to enjoy the app by looking at the results displayed! 



Note: to have a neat experience, I suggest you to open the app full page. 
