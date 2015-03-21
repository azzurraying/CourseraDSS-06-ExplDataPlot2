# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

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
total_annual_em_Bal <- numeric(length(years))

# Create a vector storing sum of emissions in Baltimore for each year
for(i in 1:length(years)) {
    Baltimore_year_index <- which(NEI1$year == years[i] & NEI$fips == "24510")
    total_annual_em_Bal[i] <- sum(NEI1$Emissions[Baltimore_year_index], na.rm = TRUE)
}

# Recreate dataframe for plot
total_annual_em_Bal1 <- as.data.frame(cbind(years, total_annual_em_Bal))
total_annual_em_Bal1$total_annual_em_Bal <- as.numeric(as.character(total_annual_em_Bal1$total_annual_em_Bal))

# Create plot
png(file = "plot2.png") # open PNG device, create PNG file in working directory

with(total_annual_em_Bal1, plot(as.character(years), total_annual_em_Bal,
                                type = "p",
                                main = "Total PM2.5 emission over four years in Baltimore",
                                xlab = "Year",
                                ylab = "Total annual PM2.5 emission in Baltimore (tonnes)",
                                xaxp  = c(1999, 2008, 3)))

dev.off() # close PNG file device