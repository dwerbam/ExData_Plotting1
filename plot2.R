# install a loads needed libraries
installif <- function(p) {
    if (!p %in% rownames(installed.packages()))
        install.packages(p)
    TRUE
}
sapply(c("dplyr", "lubridate"), installif)
library(dplyr)
library(lubridate)

# fetch the zip file from internet and extract content (if needed)
fileUrl <-
    "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
destZipFile <- './EPC.ZIP'
if (!file.exists(destZipFile)) {
    download.file(fileUrl, destfile = destZipFile, method = "curl")
}
if (!file.exists("./household_power_consumption.txt"))
    unzip(destZipFile)

energy <- read.table(
    "./household_power_consumption.txt",
    sep = ";",
    col.names = c(
        "Date",
        "Time",
        "Global_active_power",
        "Global_reactive_power",
        "Voltage",
        "Global_intensity",
        "Sub_metering_1",
        "Sub_metering_2",
        "Sub_metering_3"
    ),
    colClasses = c(
        "character",
        "character",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double",
        "double"
    ),
    header = FALSE,
    skip = 21000,
    nrows = 54000,
    na.strings = c("?")
)

parsed <- energy %>%
    ## skipping to read dates from 2007/02/01 to 2007/02/02 aprox.
    filter(grepl("0?[1|2]/0?2/2007", Date)) %>%
    mutate(dt = dmy_hms(paste(Date, Time))) %>%
    select(-c(Date, Time))

dim(parsed) ## 2880 observations = 1 sample x min for 2 days

png(file = "plot2.png",
    width = 480,
    height = 480)
with(
    parsed,
    plot(
        dt,
        Global_active_power,
        xlab = "",
        ylab = "Global Active Power (kilowatts)",
        type = "l"
    )
)

dev.off()
