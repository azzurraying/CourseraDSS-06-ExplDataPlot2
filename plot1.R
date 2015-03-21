# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system,
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Download and unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, dest = "NEIdata.zip", method = "curl")
unzip("NEIdata.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Make variable "year" a factor
NEI1 <- NEI
NEI1$year <- as.factor(NEI$year)
years <- levels(NEI1$year)

# Create a vector storing sum of emissions for each year
total_annual_em <- numeric(length(years))
for(i in 1:length(years)) {
    year_index <- which(NEI1$year == years[i])
    total_annual_em[i] <- sum(NEI1$Emissions[year_index], na.rm = TRUE)
}

# Recreate dataframe for plot
total_annual_em1 <- as.data.frame(cbind(years, total_annual_em))
total_annual_em1$total_annual_em <- as.numeric(as.character(total_annual_em1$total_annual_em))

# Create plot
png(file = "plot1.png") # open PNG device, create PNG file in working directory

with(total_annual_em1, plot(as.character(years), total_annual_em,
                            type = "p",
                            main = "Total PM2.5 emission over four years",
                            xlab = "Year",
                            ylab = "Total annual PM2.5 emission (tonnes)",
                            xaxp  = c(1999, 2008, 3)))

dev.off() # close PNG file device