# Comparing Georgian Banks
## *An example of web scraping, data wrangling and data visualization with R*

### Step1: Downloading Banks' Quarterly Reports

Script: 01-harvestGeoBanks.R  

1. Ensure that following packages are installed:  
	+ "httr"  
	+ "XML"  
	+ "rvest"  
	+ "selectr"  
2. Parameter if needed the output directory in the script and create in it a folder called "Banks"  
3. Run the script using the command *source("01-harvestGeoBanks.R")*

### Step 2: Transforming raw data into structured data

Script: 02-transformGeoBanks.R  

1. Ensure that following packages are installed:  
	+ "magrittr"  
	+ "readxl"  
	+ "stringdist"  
	+ "parsedate"  
2. Parameter if needed the source folders (dir_reports, dir_banknames) and the output folder in the script  
3. Run the script using the command *source("02-transformGeoBanks.R")*

### Step 3: Generating the charts embedded in a HTML file

Script: 03-GeoBanks.Rmd  

1. Ensure that following packages are installed:  
	+ "knitr"  
	+ "googleVis"  
2. Parameter if needed the input folder in the script  
3. Run the conversion of the script in Rmd to html using the command *knit2html("03-GeoBanks.Rmd","index.html")*  
4. **Important:** it is not possible to visualize the motion chart in local. To see it, it must be published on the web.  

For any issue, encountered bug or for any question, feel free to [contact me](mailto:js@jsdev.net).
