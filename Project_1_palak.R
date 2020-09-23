---
  title: "MUSA 508 Assignment 1"
author: "Palak Agarwal & Shivani Rai"
date: "September 18th, 2020"
output:
  html_document:
  toc: true
toc_float: true
---

#importing the library

library(rjson)
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(rlist)
options(scipen=999)
options(tigris_class = "sf")

#data import - bart lines and crime data 

sfo <- read.csv('C:/Users/agarw/Documents/MUSA508/MUSA-508-Assignment-1/Data/final_sfo_lines.csv')
crime1 <- fromJSON(file = 'Data/ala_crime.json')
crime2 <- fromJSON(file = 'Data/cc_crime.json')
crime3 <- fromJSON(file = 'Data/sc_crime.json')
crime4 <- fromJSON(file = 'Data/sfo_crime.json')
crime5 <- fromJSON(file = 'Data/sm_crime.json')

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1.5),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1.5),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f6d908","#d6c054","#878dac","#3574e2","#005fc4","#004b9a","#b2a681")

#-------- CENSUS DATA-------


census_api_key("aea3dee2d96acb5101e94f3dcfa1b575f73d093a", overwrite = TRUE)

#-------Data Wrangling for County Data-------------

# ---- Year 2009 tracts -----

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B19013_001E",
                                             "B25058_001E","B01001_006E","B01001_007E",
                                             "B01001_008E","B01001_009E","B01001_010E",
                                             "B01001_011E","B01001_012E","B01001_013E",
                                             "B01001_014E","B01001_015E","B01001_016E",
                                             "B01001_030E","B01001_031E","B01001_032E",
                                             "B01001_033E","B01001_034E","B01001_035E",
                                             "B01001_036E","B01001_037E","B01001_038E",
                                             "B01001_039E","B01001_040E","B12001_004E",
                                             "B12001_013E","B02001_003E","B02001_004E",
                                             "B02001_005E","B02001_006E","B03001_003E"), 
          year=2009, state=06, county=c(001,075,081,013,085), geometry=T) %>% 
  st_transform('ESRI:102241')

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         Blacks = B02001_003,
         AmInd = B02001_004,
         Asian = B02001_005,
         Hawaiian = B02001_006,
         Hispanic = B03001_003,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         WorkingClassM1517 = B01001_006,
         WorkingClassM1819 = B01001_007,
         WorkingClassM20 = B01001_008,
         WorkingClassM21 = B01001_009,
         WorkingClassM2224 = B01001_010,
         WorkingClassM2529 = B01001_011,
         WorkingClassM3034 = B01001_012,
         WorkingClassM3539 = B01001_013,
         WorkingClassM4044 = B01001_014,
         WorkingClassM4549 = B01001_015,
         WorkingClassM5054 = B01001_016,
         WorkingClassF1517 = B01001_030,
         WorkingClassF1819 = B01001_031,
         WorkingClassF20 = B01001_032,
         WorkingClassF21 = B01001_033,
         WorkingClassF2224 = B01001_034,
         WorkingClassF2529 = B01001_035,
         WorkingClassF3034 = B01001_036,
         WorkingClassF3539 = B01001_037,
         WorkingClassF4044 = B01001_038,
         WorkingClassF4549 = B01001_039,
         WorkingClassF5054 = B01001_040,
         MaleMarried = B12001_004,
         FemaleMarried = B12001_013
         )

tracts209 <- 
  tracts09 %>%
  mutate(pctWorking = ifelse(TotalPop > 0, ((WorkingClassM1517 + WorkingClassM1819 + WorkingClassM20 
                                             + WorkingClassM21 + WorkingClassM2224 + WorkingClassM2529 
                                             + WorkingClassM3034 + WorkingClassM3539 + WorkingClassM4044 
                                             + WorkingClassM4549 + WorkingClassM5054 + WorkingClassF1517 
                                             + WorkingClassF1819 + WorkingClassF20 + WorkingClassF21 
                                             + WorkingClassF2224 + WorkingClassF2529 + WorkingClassF3034 
                                             + WorkingClassF3539 + WorkingClassF4044 + WorkingClassF4549 
                                             + WorkingClassF5054)/TotalPop * 100), 0),
         pctMarried = ifelse(TotalPop > 0, ((FemaleMarried + MaleMarried) / TotalPop * 100), 0),
         pctWhite = ifelse(TotalPop > 0, Whites / TotalPop * 100, 0),
         pctOtherRace = ifelse(TotalPop > 0, ((Blacks + AmInd +Asian
                                               + Hawaiian + Hispanic) / TotalPop * 100), 0),
         
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleMarried,-MaleMarried,-Blacks,-AmInd,-Asian,-Hawaiian,-Hispanic,
                -WorkingClassM1517,-WorkingClassM1819,-WorkingClassM20,-WorkingClassM21,-WorkingClassM2224,-WorkingClassM2529, 
                -WorkingClassM3034,-WorkingClassM3539,-WorkingClassM4044,-WorkingClassM4549,-WorkingClassM5054,-WorkingClassF1517, 
                -WorkingClassF1819,-WorkingClassF20,-WorkingClassF21,-WorkingClassF2224,-WorkingClassF2529,-WorkingClassF3034,
                -WorkingClassF3539,-WorkingClassF4044,-WorkingClassF4549,-WorkingClassF5054)

#----- Finding max values of popluation and rent to normalise the data 
# as the tract is not truly representative of the data near the station------

maxtotpop_2009 <- max(tracts209$TotalPop)
maxrent_2009 <- max(tracts209$MedRent , na.rm = T)
tracts2009 <- 
  tracts209 %>%
  mutate( normpop = TotalPop/maxtotpop_2009,
          normrent = MedRent/maxrent_2009)

# ---- Year 2017 tracts -----

tracts217 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B19013_001E",
                                             "B25058_001E","B01001_006E","B01001_007E",
                                             "B01001_008E","B01001_009E","B01001_010E",
                                             "B01001_011E","B01001_012E","B01001_013E",
                                             "B01001_014E","B01001_015E","B01001_016E",
                                             "B01001_030E","B01001_031E","B01001_032E",
                                             "B01001_033E","B01001_034E","B01001_035E",
                                             "B01001_036E","B01001_037E","B01001_038E",
                                             "B01001_039E","B01001_040E","B12001_004E",
                                             "B12001_013E","B02001_003E","B02001_004E",
                                             "B02001_005E","B02001_006E","B03001_003E"), 
          year=2017, state=06, county=c(001,075,081,013,085), geometry=T) %>% 
  st_transform('ESRI:102241') %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         Blacks = B02001_003,
         AmInd = B02001_004,
         Asian = B02001_005,
         Hawaiian = B02001_006,
         Hispanic = B03001_003,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         WorkingClassM1517 = B01001_006,
         WorkingClassM1819 = B01001_007,
         WorkingClassM20 = B01001_008,
         WorkingClassM21 = B01001_009,
         WorkingClassM2224 = B01001_010,
         WorkingClassM2529 = B01001_011,
         WorkingClassM3034 = B01001_012,
         WorkingClassM3539 = B01001_013,
         WorkingClassM4044 = B01001_014,
         WorkingClassM4549 = B01001_015,
         WorkingClassM5054 = B01001_016,
         WorkingClassF1517 = B01001_030,
         WorkingClassF1819 = B01001_031,
         WorkingClassF20 = B01001_032,
         WorkingClassF21 = B01001_033,
         WorkingClassF2224 = B01001_034,
         WorkingClassF2529 = B01001_035,
         WorkingClassF3034 = B01001_036,
         WorkingClassF3539 = B01001_037,
         WorkingClassF4044 = B01001_038,
         WorkingClassF4549 = B01001_039,
         WorkingClassF5054 = B01001_040,
         MaleMarried = B12001_004,
         FemaleMarried = B12001_013
  ) %>%
  mutate(pctWorking = ifelse(TotalPop > 0, ((WorkingClassM1517 + WorkingClassM1819 + WorkingClassM20 
                                             + WorkingClassM21 + WorkingClassM2224 + WorkingClassM2529 
                                             + WorkingClassM3034 + WorkingClassM3539 + WorkingClassM4044 
                                             + WorkingClassM4549 + WorkingClassM5054 + WorkingClassF1517 
                                             + WorkingClassF1819 + WorkingClassF20 + WorkingClassF21 
                                             + WorkingClassF2224 + WorkingClassF2529 + WorkingClassF3034 
                                             + WorkingClassF3539 + WorkingClassF4044 + WorkingClassF4549 
                                             + WorkingClassF5054)/TotalPop * 100), 0),
         pctMarried = ifelse(TotalPop > 0, ((FemaleMarried + MaleMarried) / TotalPop * 100), 0),
         pctWhite = ifelse(TotalPop > 0, Whites / TotalPop *100, 0),
         pctOtherRace = ifelse(TotalPop > 0, ((Blacks + AmInd +Asian
                                               + Hawaiian + Hispanic) / TotalPop * 100), 0),
         year = "2017") %>%
  dplyr::select(-Whites,-FemaleMarried,-MaleMarried,-Blacks,-AmInd,-Asian,-Hawaiian,-Hispanic,
                -WorkingClassM1517,-WorkingClassM1819,-WorkingClassM20,-WorkingClassM21,-WorkingClassM2224,-WorkingClassM2529, 
                -WorkingClassM3034,-WorkingClassM3539,-WorkingClassM4044,-WorkingClassM4549,-WorkingClassM5054,-WorkingClassF1517, 
                -WorkingClassF1819,-WorkingClassF20,-WorkingClassF21,-WorkingClassF2224,-WorkingClassF2529,-WorkingClassF3034,
                -WorkingClassF3539,-WorkingClassF4044,-WorkingClassF4549,-WorkingClassF5054)

#----- Finding max values of popluation and rent to normalise the data 
# as the tract is not truly representative of the data near the station------

maxtotpop_2017 <- max(tracts217$TotalPop)
maxrent_2017 <- max(tracts217$MedRent , na.rm = T)
tracts2017 <- 
  tracts217 %>%
  mutate( normpop = TotalPop/maxtotpop_2017,
          normrent = MedRent/maxrent_2017)

# --- Combining 09 and 17 data ----

finalTract <- rbind(tracts2009,tracts2017)

#-------Converting the bart data into spatial data---------------------------

# The BART data we had was in a csv with no coordinate column
# Hence we first separated that string value to get the X and Y

x <- vector(mode='list', length = 105)
y <- vector(mode='list', length = 105)
spli <- strsplit(sfo$Location, ",")
for (val in spli){
  print(val)
  print(val[[1]])
  print(val[[2]])
  x <- append(x, val[[1]])
  y <- append(y, val[[2]])
  
}
sfo_new <- 
  sfo %>%
  mutate(X = x[106:210],
         Y = y[106:210]) %>%
  dplyr::select(-Abbreviation, -Location, -Description)
crs_tract09 <- st_crs(tracts2009)
sfo_spatial <- st_as_sf(sfo_new, coords = c("X","Y"), crs = 4326, agr = "constant")
sfo_spatial <- sfo_spatial %>% st_transform(st_crs(tracts2009))

# Plotting the lines on the bay area tracts

ggplot() + 
  geom_sf(data=st_union(tracts2009), colour = '#efefef') +
  geom_sf(data=sfo_spatial, 
          aes(colour = Line_colour), 
          show.legend = "point", size= 1.2) +
  labs(title="Bart Stops", 
       subtitle="Bay Area, CA", 
       caption="Figure 1.0") +
  mapTheme()

# Create buffers around Bart stops -
# Both a buffer for each stop, and a union of the buffers...
# and bind these objects together

bartBuffers <- 
  rbind(
    st_buffer(sfo_spatial, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(sfo_spatial, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=bartBuffers) +
  geom_sf(data=sfo_spatial, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 1.1") +
  mapTheme()

# Create an sf object with ONLY the unioned buffer
buffer <- filter(bartBuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas
clip <- 
  st_intersection(buffer, tracts2009) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

ggplot() +
  geom_sf(data = clip) +
  labs(title="Clipped Tracts", 
         subtitle="Bay Area, CA", 
         caption="Figure 2.0") +
  theme(plot.title = element_text(size=22))

# Do a spatial selection to see which tracts touch the buffer
selection <- 
  tracts2009[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

ggplot() +
  geom_sf(data = selection) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  labs(title="Selected Tracts", 
         subtitle="Bay Area, CA", 
         caption="Figure 2.1") +
  theme(plot.title = element_text(size=22))

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts2009)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts2009, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

ggplot() +
  geom_sf(data = selectCentroids) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  labs(title="Selected Tracts with centroids in the buffer zone", 
       subtitle="Bay Area, CA", 
       caption="Figure 2.2") +
  theme(plot.title = element_text(size=22))


myData  <- rbind(selectCentroids, clip) %>%
  rbind(., selection)

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together and add a new column TOD.
# Note that this contains a correct 2009-2017 inflation calculation

finalTract.group <- 
  rbind(
    st_centroid(finalTract)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(finalTract) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(finalTract)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(finalTract) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent))

#----- Now that we have our data lets visualize it --------
#---- First lets visualize it as maps

#----- Plot which are the TOD tracts

ggplot(finalTract.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "TOD vs. Non-TOD Groups",
       subtitle="Bay Area, CA", 
       caption="Figure 3.0") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#----- Plot rent as a function of year and TOD

ggplot(finalTract.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1)+
  scale_fill_manual(values = palette5,
                    labels = qBr(finalTract.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Dollars\nBay Area, CA", caption="Figure 3.1") +
  facet_wrap(~year + TOD)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#-----Plotted number of married people in the tract as function of year and TOD

ggplot(finalTract.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(pctMarried))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(finalTract.group, "pctMarried"),
                    name = "Married as percentage \n(Quintile Breaks)") +
  labs(title = "Married 2009-2017", subtitle = "Bay area, CA", caption="Figure 3.2") +
  facet_wrap(~year + TOD)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#-----Plotted number racial composition in the tract as function of year and TOD

ggplot(finalTract.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(pctWhite))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(finalTract.group, "pctWhite"),
                    name = "Racial Composition \n(Quintile Breaks)") +
  labs(title = "Racial Composition 2009-2017", subtitle = "Bay area, CA", caption="Figure 3.3") +
  facet_wrap(~year + TOD)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#-----Plotted number the number of people in the working industry in the tract as function of year and TOD

ggplot(finalTract.group)+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(aes(fill = q5(pctWorking))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(finalTract.group, "pctWorking"),
                    name = "Working class \n(Quintile Breaks)") +
  labs(title = "Working Class 2009-2017", subtitle = "Bay area, CA", caption="Figure 3.4") +
  facet_wrap(~year + TOD)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#---- Noew lets visualize this data as a table and bar graph

#Table 
finalTract.Summary <- 
  st_drop_geometry(finalTract.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_Racial Composition= mean(pctWhite, na.rm = T),
            Percent_Working Class= mean(pctWorking, na.rm = T),
            Percent_Married = mean(pctMarried, na.rm = T),
            Percent_MedIn = mean(MedHHInc, na.rm = T))

kable(finalTract.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 1.0")

#Bar plot
finalTract.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", nrow=1) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space", subtitle = "Bay area, CA", caption="Figure 4.0") +
  plotTheme() + theme(legend.position="bottom")



#------- Graduated symbol map------

# Create an sf object with ONLY the buffers
buffers <- filter(bartBuffers, Legend=="Buffer")
buffer_spatial <- buffers %>% st_transform(st_crs(tracts2009))

# For this part of the assignment, we chose to take each
# buffer and find the tracts it intersects with. After that
# we summed the normalized population and rent values to find the 
# population and rent charatcer.

x <- vector(mode='list', length = 105)
y <- vector(mode='list', length = 105)

for( val in c(1:105))
{
  #print(buffer_spatial[[val,2]])
  clip1 <- 
    st_intersection(buffer_spatial[val,1], tracts2009) %>%
    dplyr::select(normpop,normrent) %>%
    mutate(Selection_Type = "Clip")
  selection1 <- 
    tracts2009[buffer_spatial[val,1],] %>%
    dplyr::select(normpop,normrent) %>%
    mutate(Selection_Type = "Spatial Selection")
  selectCentroids1 <-
    st_centroid(tracts2009)[buffer_spatial[val,1],] %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(tracts2009, GEOID)) %>%
    st_sf() %>%
    dplyr::select(normpop,normrent) %>%
    mutate(Selection_Type = "Select by Centroids")
  sum_sb <- max(selectCentroids1$normpop)
  sum_sc <- max(selectCentroids1$normrent)
  x <- append(x, sum_sb)
  y <- append(y, sum_sc)
}

pograd <- sfo_spatial %>%
  mutate(Sum_Popnor = as.numeric(x[106:210]),
         Sum_Rentnor = as.numeric(y[106:210]))
popgrad <- pograd %>%
  mutate(Sum_Popnorm = Sum_Popnor * 100,
         Sum_Rentnorm = Sum_Rentnor * 100)

#----- Now lets plot the population and rent 
# as graduated symbol map

#Population
ggplot()+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(data=popgrad, 
          aes(size = Sum_Popnorm), 
          color = "red")+
  scale_size_area(name="", max_size = 4.5) + 
  guides(size=guide_legend("Population")) +
  labs(
    title = "Population as a fucntion of distance from stops",
    subtitle = "Bay Area, CA",
    caption = "Figure 5.0"
  )+
  mapTheme()

ggplot()+
  geom_sf(data = st_union(tracts2009))+
  geom_sf(data=popgrad, 
          aes(size = Sum_Rentnorm), 
          color = "red")+
  scale_size_area(name="", max_size = 4.5) + 
  guides(size=guide_legend("Total Venues")) +
  labs(
    title = "Rent as a fucntion of distance from stops",
    subtitle = "Bay Area, CA",
    caption = "Figure 5.1"
  )+
  mapTheme()

#------- GEOM LINE---------

#Function MultipleRingBuffer
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}

#Creating the buffer around the transit stops
finalTracts.rings <-
  st_join(st_centroid(dplyr::select(finalTract, GEOID, year)), 
          multipleRingBuffer(st_union(sfo_spatial), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(finalTract, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(Distance = distance / 5280) #convert to miles

#Finding the mean of rent within the same distance
finalTracts.rings_summary <-
  finalTracts.rings %>%
  st_drop_geometry() %>%
  group_by(Distance, year) %>%
  summarize(Rent = mean(MedRent, na.rm=T)) 

#Plotting the data
ggplot(data=finalTracts.rings_summary,
       aes(x = Distance, y = Rent, colour = year)) +
  geom_line()+
  geom_point()


#-------------Crime data-----------
