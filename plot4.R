# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Download and unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, dest = "NEIdata.zip", method = "curl")
unzip("NEIdata.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Compared to "Short.Name" variable, EI.Sector" provides clearer classification of emission sources.
# This variable is used to find emission sources coming from coal.
# Or a comparison can be made with calculations using "Short.Name" variable.
coal_index <- grep("[Cc]oal", SCC$EI.Sector)
SCC_coal <- SCC$SCC[coal_index]

# Make variable "year" a factor
NEI1 <- NEI
NEI1$year <- as.factor(NEI$year)
# Split dataset by years
NEI_yr <- split(NEI1, NEI1$year)

# Create vector storing sums of emissions from coal for each year
NEI_coal_em <- numeric(length(SCC_coal))
Total_annual_coal_em <- numeric(4)
for(i in 1:4) {
    for(j in 1:length(SCC_coal)) {
        NEI_coal_em[j] <- sum(NEI_yr[[i]]$Emissions[NEI_yr[[i]]$SCC == SCC_coal[j]])
    }
    Total_annual_coal_em[i] <- sum(NEI_coal_em)
}

# Recreate dataframe for plot
total_annual_em_coal1 <- as.data.frame(cbind(years, Total_annual_coal_em))
total_annual_em_coal1$Total_annual_coal_em <- as.numeric(as.character(total_annual_em_coal1$Total_annual_coal_em))

# Create plot
png(file = "plot4.png") # open PNG device, create PNG file in working directory

par(mar = c(5, 6, 4, 2))
with(total_annual_em_coal1, plot(as.character(years), Total_annual_coal_em,
                                 type = "p",
                                 main = "Total PM2.5 emission over four years from coal sources",
                                 xlab = "Year",
                                 ylab = "Total annual PM2.5 emission\nfrom coal sources (tonnes)",
                                 xaxp  = c(1999, 2008, 3)))

dev.off() # close PNG file device