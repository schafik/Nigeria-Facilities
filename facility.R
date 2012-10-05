setwd("/Users/salahchafik/Documents/Modi Labs/Nigeria/Data Collection Status/survey data")
getwd()
library('ggplot2')
library('plyr')
library('gplots') 


row.names=FALSE
education <- read.csv("Education_05_06_2012_2012_08_16.csv", header = TRUE)
education2 <- read.csv("Education_17_04_2012_2012_08_21.csv", header = TRUE)
education3 <- read.csv("Education_22_05_2012_2012_08_21.csv", header = TRUE)
health <- read.csv("Health_05_06_2012_2012_10_03_16_48_41.csv", header = TRUE)
health2 <- read.csv("Health_17_04_2012_2012_10_03_16_49_35.csv", header = TRUE)
health3 <- read.csv("Health_22_05_2012_2012_10_03_16_51_22.csv", header = TRUE)
water <- read.csv("Water_05_06_2012_2012_08_16.csv", header = TRUE)
water2 <- read.csv("Water_22_05_2012_2012_08_21.csv", header = TRUE)
water3 <- read.csv("Water_24_04_2012_2012_08_21.csv", header = TRUE) 

e1 <- education[c('mylga_state', 'mylga', 'ward', 'community', 'level_of_education', 'school_name', 'school_address')]     
e2 <- education2[c('mylga_state', 'mylga', 'ward', 'community', 'level_of_education', 'school_name', 'school_address')]
e3 <- education3[c('mylga_state', 'mylga', 'ward', 'community', 'level_of_education', 'school_name', 'school_address')]

h1 <- health[c('mylga', 'mylga_state', 'ward', 'community', 'facility_type', 'facility_name', 'facility_address')]    
h2 <- health2[c('mylga', 'mylga_state', 'ward', 'community', 'facility_type', 'facility_name', 'facility_address')]
h3 <- health3[c('mylga', 'mylga_state', 'ward', 'community', 'facility_type', 'facility_name', 'facility_address')] 

w1 <- water[c('mylga', 'mylga_state', 'ward', 'community', 'water_source_type', 'water_scheme_type', 'lift_mechanism')]       
w2 <- water2 [c('mylga', 'mylga_state', 'ward', 'community', 'water_source_type', 'water_scheme_type', 'lift_mechanism')]
w3 <- water3 [c('mylga', 'mylga_state', 'ward', 'community', 'water_source_type', 'water_scheme_type', 'lift_mechanism')]

e <- rbind(e1,e2,e3)   
h <- rbind(h1,h2,h3)
w <- rbind(w1,w2,w3)

h$facility_address <- substr(gsub(". +", ".", (gsub(", +", ",", as.character(h$facility_address)))), 0, 40)
h$facility_name <- substr(gsub(". +", ".", (gsub(", +", ",", as.character(h$facility_name)))), 0, 40)

#e$facility_address <- substr(gsub(". +", ".", (gsub(", +", ",", as.character(e$facility_address)))), 0, 40)
#w$facility_address <- substr(gsub(". +", ".", (gsub(", +", ",", as.character(w$facility_address)))), 0, 40)


row.names(e) <- NULL
row.names(h) <- NULL
row.names(w) <- NULL


#####CLEANING#####

#education
clean_e <- subset(e, (level_of_education!="157") & (level_of_education!="TRUE") & (level_of_education!="")
                  & (level_of_education!="CHALAWA") & (level_of_education!="0") & (level_of_education!="3.58593E+14") 
                  & (level_of_education!="FALSE") & (level_of_education!="benue") & (level_of_education!="n/a")
                  & (level_of_education!="yes") & (level_of_education!="131") & (level_of_education!="60_119_min")
                  & (level_of_education!="Infinity") 
                  )

##water##
clean_w <- subset(w, (water_source_type!="420.5") & (water_source_type!="8.95821313") & (water_source_type!="6.77697973 9.27850141 263.79998779296875 5.0")
                  & (water_source_type!="Kore") & (water_source_type!="Owerri") & (water_source_type!="adamawa")
                  & (water_source_type!="bor05323942 ") & (water_source_type!="north_central") & (water_source_type!="yes")
                  & (water_source_type!="year_round") & (water_source_type!="tarka") & (water_source_type!="no")
                  & (water_source_type!="cannot_determine") & (water_source_type!="boreh2:01:33.026+01") & (water_source_type!="auyo")
                  & (water_source_type!="TRUE") & (water_source_type!="Mbada/tiev") 
                  & (water_scheme_type!="12.13035459 9.07871357 420.5 5.0")
                  & (water_scheme_type!="228.1999969") & (water_scheme_type!="FALSE")
                  & (water_scheme_type!="TRUE") & (water_scheme_type!="sabon_birni")
                  & (water_scheme_type!="5") & (water_scheme_type!="Gunduwawa") & (water_scheme_type!="adamawa")
                  & (water_scheme_type!="wat/a") & (water_scheme_type!="yes")
                  & (water_scheme_type!="outlet_within_100m") & (water_scheme_type!="borehole_tube_well")
                  & (water_scheme_type!="6/14/2012") & (water_scheme_type!="KALGWAI") & (water_scheme_type!="no")
                  & (lift_mechanism!="gatafa") & (lift_mechanism!="FALSE") & (lift_mechanism!="Wamnune") & (lift_mechanism!="auyo")
                  & (lift_mechanism!="shanono") & (lift_mechanism!="katsina") & (lift_mechanism!="n2-07-29T11:24:53.912+01")
                  & (lift_mechanism!="water_source") & (lift_mechanism!="year_round") & (lift_mechanism!="43082097-85c6-4a0c-b1d8-81137aaf125d")
                  & (lift_mechanism!="9.27850141") & (lift_mechanism!="TRUE") & (lift_mechanism!="borehole_tube_well")
                  & (lift_mechanism!="karaye") & (lift_mechanism!="protected_dug_well") & (lift_mechanism!="tsanyawa")
                  & (lift_mechanism!="wurno") & (lift_mechanism!="yes") & (lift_mechanism!="no")
                  & (lift_mechanism!="benue") & (lift_mechanism!="Xxx") & (lift_mechanism!="Galadi")
                  & (lift_mechanism!="6.213E+14") & (lift_mechanism!="1343319271739.jpg")
                  )

#renaming erroneous values
levels(clean_w$water_scheme_type)[levels(clean_w$water_scheme_type=="water_sourc")] <- "water_source"


################Generating Text Plots#################
####education
e_1 <- dlply(clean_e, .(mylga), function(smalldf) { 
  smalldf$level_of_education <- as.factor(as.character(smalldf$level_of_education, smalldf$community))
  smalldf
})

levels(e$mylga)[levels(e$mylga)=="n/a"] <- "NA"

for (lga in e$mylga) {
  e_lga_df <- e_1[[lga]]
  pdf(paste("educationoutput/", lga, "_education.pdf", sep=""), width=12)
  textplot(e_lga_df[order(e_lga_df$community, e_lga_df$level_of_education),][-c(1,2)], show.rownames = FALSE, mfrow=50)
  title(lga, cex=1.25)
  dev.off()
}

####health
h_1 <- dlply(h, .(mylga), function(smalldf) { 
  smalldf$facility_type <- as.factor(as.character(smalldf$facility_type))
  smalldf
})
        
levels(h$mylga)[levels(h$mylga)=="n/a"] <- "NA"

for (lga in h$mylga) {
  h_lga_df <- h_1[[lga]]
  pdf(paste("healthoutput/", lga, "_health.pdf", sep=""), width=12)
  textplot(h_lga_df[order(h_lga_df$community, h_lga_df$facility_type),][-c(1,2)], show.rownames = FALSE, mfrow=50)
  title(lga, cex=1.25)
  dev.off()
}

####water
w_1 <- dlply(clean_w, .(mylga), function(smalldf) { 
  smalldf$facility_type <- as.factor(as.character(smalldf$water_source_type, smalldf$community))
  smalldf
})

levels(clean_w$mylga)[levels(clean_w$mylga)=="n/a"] <- "NA"

for (lga in clean_w$mylga) {
  w_lga_df <- w_1[[lga]]
  pdf(paste("wateroutput/", lga, "_water.pdf", sep=""), width=12)
  textplot(w_lga_df[order(w_lga_df$community, w_lga_df$water_scheme_type),][-c(1,2)], show.rownames = FALSE, mfrow=50)
  title(lga, cex=1.25)
  dev.off()
}

