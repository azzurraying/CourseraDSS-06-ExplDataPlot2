# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

# Download and unzip data
fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, dest = "NEIdata.zip", method = "curl")
unzip("NEIdata.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# The "EI.Sector" variable is used to find emission sources coming from vehicles.
vehicle_index <- grep("[Vv]ehicles", SCC$EI.Sector)
SCC_veh <- SCC$SCC[vehicle_index]

# Make variable "year" a factor
NEI1 <- NEI
NEI1$year <- as.factor(NEI$year)
# Subset data to contain only LA data
NEI_LA <- NEI1[NEI1$fips == "06037", ]
# Split LA dataset by years
NEI_yr_LA <- split(NEI_LA, NEI_LA$year)

# Create vector storing sums of emission in LA from vehicles, over 4 years
NEI_em_veh_LA <- numeric(length(SCC_veh))
Total_annual_em_veh_LA <- numeric(4)
for(i in 1:4) {
    for(j in 1:length(SCC_veh)) {
        NEI_em_veh_LA[j] <- sum(NEI_yr_LA[[i]]$Emissions[NEI_yr_LA[[i]]$SCC == SCC_veh[j]])
    }
    Total_annual_em_veh_LA[i] <- sum(NEI_em_veh_LA)
}

# Recreate dataframe for plot
total_annual_em_veh_LA1 <- as.data.frame(cbind(years, Total_annual_em_veh_LA))
total_annual_em_veh_LA1$Total_annual_em_veh_LA <- as.numeric(as.character(total_annual_em_veh_LA1$Total_annual_em_veh_LA))

# Create plot
png(file = "plot6.png") # open PNG device, create PNG file in working directory

# NOTE: dataframe "Total_annual_em_veh_Bal" is created from sourcing code in "plot5.R".
par(mar = c(5, 6, 5, 2))
with(total_annual_em_veh_Bal1, plot(as.character(years), Total_annual_em_veh_Bal,
                                    type = "p",
                                    main = "Total PM2.5 emission over four years from motor vehicles\nin Baltimore and Los Angeles",
                                    xlab = "Year",
                                    ylab = "Total annual PM2.5 emission\nfrom motor vehicles (tonnes)",
                                    ylim = c(50, 5000),
                                    xaxp  = c(1999, 2008, 3)))

with(total_annual_em_veh_LA1, points(as.character(years), Total_annual_em_veh_LA,
                                     col = "red",
                                     type = "p"))

legend("right",
       col = c("black", "red"),
       lty = 1,
       legend = c("Baltimore", "Los Angeles")) # Annotate

dev.off() # close PNG file device
