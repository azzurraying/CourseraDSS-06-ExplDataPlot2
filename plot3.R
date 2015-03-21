# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# Download and unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, dest = "NEIdata.zip", method = "curl")
unzip("NEIdata.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Make variables "year" and "type" factors
NEI1 <- NEI
NEI1$year <- as.factor(NEI$year)
years <- levels(NEI1$year)
NEI1$type <- as.factor(NEI$type)
type <- levels(NEI1$type)

# Subset data to contain only Baltimore data
NEI_Bal <- NEI1[NEI1$fips == "24510", ]
# Split Baltimore dataset by years
NEI_yr_Bal <- split(NEI_Bal, NEI_Bal$year)

# Create a vector storing 4 types of emissions over 4 years
total_em_type <- matrix(nrow = length(years), ncol = length(type))
for(i in 1:length(years)) {
    for(j in 1:length(type)) {
        total_em_type[i, j] <- sum(NEI_yr_Bal[[i]]$Emissions[NEI_yr_Bal[[i]]$type == type[j]])
    }
}

# Re-arrange vector to create a tidy dataset
total_em_type_Bal1 <- as.data.frame(cbind(rep(years, length(type)),
                                          sapply(total_em_type, c),
                                          sapply(sapply(type, rep, length(years)), c)),
                                    row.names = FALSE)
colnames(total_em_type_Bal1) <- c("Year", "Emission", "Source")

# Create plot
library(ggplot2)
png(file = "plot3.png")
    g <- ggplot(total_em_type_Bal1, aes(x = Year, y = Emission))
    g1 <- g + geom_point() + facet_grid(. ~ Source)
    g2 <- g1 + labs(x = "Year") + labs(y = "Total annual PM2.5 emission in Baltimore (tonnes)") + labs(title = "PM2.5 emission from four sources over four years in Baltimore")
    print(g2)
dev.off()

