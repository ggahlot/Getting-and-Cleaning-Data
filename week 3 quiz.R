library(httr) 


data <- read.csv("getdata-data-ss06hid.csv")
logicalvector <- data$ACR==3 & data$AGS==6
first3 <- which(logicalvector)[1:3] # which() treats NAs as FALSE
first3


install.packages("jpeg")

jpeg
direccion2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
archivo2 <- "jeff.jpg"
download.file(direccion2, archivo2, method="curl")
foto <- readJPEG("getdata-jeff.jpg", native = TRUE)
quantile(foto)




library(data.table)

direccion3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
archivo3 <- "GDP.csv"
download.file(direccion3, archivo3, method="curl")
GDP <- data.table(read.csv("GDP.csv", skip = 4, nrows = 191))
GDP <- GDP[X != ""]
GDP <- GDP[, list(X, X.1, X.3, X.4)]
setnames(GDP, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP", "Long.Name", "GDP"))

direccion4 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
archivo4 <- "EDSTATS_Country.csv"
download.file(direccion4, archivo4, method="curl")
EDSTATS <- data.table(read.csv("EDSTATS_Country.csv"))

data2 <- merge(GDP, EDSTATS, all = TRUE, by = c("CountryCode"))

sum(!is.na(unique(data2$rankingGDP)))

data2[order(rankingGDP, decreasing = TRUE), list(CountryCode, Long.Name.x, Long.Name.y, rankingGDP, GDP)][13]



data2[, mean(rankingGDP, na.rm = TRUE), by = Income.Group]


breaks <- quantile(data2$rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)
data2$quantileGDP <- cut(data2$rankingGDP, breaks = breaks)
data2[Income.Group == "Lower middle income", .N, by = c("Income.Group", "quantileGDP")]
