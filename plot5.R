# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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
# Subset data to contain only Baltimore data
NEI_Bal <- NEI1[NEI1$fips == "24510", ]
# Split Baltimore dataset by years
NEI_yr_Bal <- split(NEI_Bal, NEI_Bal$year)

# Create vector storing sums of emission in Baltimore from vehicles, over 4 years
NEI_em_veh_Bal <- numeric(length(SCC_veh))
Total_annual_em_veh_Bal <- numeric(4)
for(i in 1:length(years)) {
    for(j in 1:length(SCC_veh)) {
        NEI_em_veh_Bal[j] <- sum(NEI_yr_Bal[[i]]$Emissions[NEI_yr_Bal[[i]]$SCC == SCC_veh[j]])
    }
    Total_annual_em_veh_Bal[i] <- sum(NEI_em_veh_Bal)
}

# Recreate dataframe for plot
total_annual_em_veh_Bal1 <- as.data.frame(cbind(years, Total_annual_em_veh_Bal))
total_annual_em_veh_Bal1$Total_annual_em_veh_Bal <- as.numeric(as.character(total_annual_em_veh_Bal1$Total_annual_em_veh_Bal))

# Create plot
png(file = "plot5.png") # open PNG device, create PNG file in working directory

par(mar = c(5, 6, 5, 2))
with(total_annual_em_veh_Bal1, plot(as.character(years), Total_annual_em_veh_Bal,
                                    type = "p",
                                    main = "Total PM2.5 emission over four years\nin Baltimore from motor vehicles",
                                    xlab = "Year",
                                    ylab = "Total annual PM2.5 emission in Baltimore\nfrom motor vehicles (tonnes)",
                                    xaxp  = c(1999, 2008, 3)))

dev.off() # close PNG file device
