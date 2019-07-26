# Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. 
#In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM 
#and for tracking the emissions of this pollutant into the atmosphere. 
#Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. 
#This database is known as the National Emissions Inventory (NEI). 
# For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. 


##################################
# The data used for this assignment are for 1999, 2002, 2005, and 2008 : https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.z
##################################

################################### QUESTION 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
##################################


plot1 <- function()
{
  
  # Initialize path values
  dataFile <- "exdata_data_NEI_data/summarySCC_PM25.rds"
  #  pathunzipped <- "exdata_data_NEI_data"
  zipFilename <- 'exdata_data_NEI_data.zip'
  
  # Check if zipfile has been already 1) dowloaded and 2)unzipped : iIF NOT,download & unzip 
  #  zip file exists? IF NOT, downlod it
  if (!file.exists(dataFile)) 
  {
    if(!file.exists(zipFilename)) 
    { 
      zipFileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
      download.file(zipFileUrl, zipFilename)
    }
    unzip(zipFilename)
  }
  
  # Reading data from source file & loading in dataframe :  NEI_Data, dataframe with PM2.5 emissions in US in 1999 /2002/2005/2008
  
  NEI_Data   <- readRDS('exdata_data_NEI_data/summarySCC_PM25.rds')
  
  #
 
  # calculate total emission / year for period in scope in NEI_Data,  columns 'Emissions' & 'year'
  
  totEmiss <- aggregate(Emissions ~ year , NEI_Data, FUN = sum )
  

  
  
  # plotting graph1 : emissions/year 
  with (totEmiss, barplot(height = Emissions, width = 1, space = NULL, names.arg = year, col = 1:4,
                          xlab = "year", ylab = "PM2.5 Annual Emission" , main = expression("Total PM2.5 Annual Emissions in USA - 1999/2008")))

  # Copying plot into a .png file & closing device
  dev.copy(png,file="plot1.png",width=480, height=480)
  dev.off()
  
}