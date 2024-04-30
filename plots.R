data <- read.csv("~/Cours/MLDM/2ndsemester/DMKD/Project/world_ISO.csv")
duolingo <- read.csv("~/Cours/MLDM/2ndsemester/DMKD/Project/duolingo_ISO.csv")

library("rworldmap")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

data$Population <- strtoi(gsub(',', '', data$Population))

map <- joinCountryData2Map(data, joinCode="ISO3", nameJoinColumn="ISO", nameCountryColumn="Country", verbose=TRUE)

mapCountryData(map, nameColumnToPlot = "Population", addLegend=FALSE)

map2 <- joinCountryData2Map(duolingo, joinCode="ISO3", nameJoinColumn="ISO3", nameCountryColumn="country", verbose=TRUE)

mapCountryData(map2, nameColumnToPlot="pop1_2023", catMethod="Categorical", colourPalette = "rainbow")

duolingo$withoutEnglish <- duolingo$pop1_2023

duolingo <- within(duolingo, withoutEnglish[withoutEnglish == "English"] <- pop2_2023[withoutEnglish == "English"])

map3 <- joinCountryData2Map(duolingo, joinCode="ISO3", nameJoinColumn="ISO3", nameCountryColumn="country", verbose=TRUE)

mapCountryData(map3, nameColumnToPlot="withoutEnglish", catMethod="Categorical", colourPalette = "rainbow")