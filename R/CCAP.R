###This script is supplementary material for "Tampa Bay habitat protection and restoration opportunities, as identified using analysis of C-CAP land cover maps" by CA Craig, KR Radabaugh, MW Beck, K Flaherty-Walia, ET Sherwood.

#Author: CA Craig

#This script was used to calculate summary statistics of each C-CAP-derived GIS layer by acreage, in US survey acres. Code sections are organized by native and restorable habitat categories.




######################################################################
#####################CURRENT EXTENT###################################
######################################################################

#import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_NativeHabitat_Table <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_NativeHabitat_Table.csv", col_types = cols(gridcode = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(CCAP2021_TBEP_NativeHabitat_Table)


##import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_TotalRestOpp_Table <- read_csv("CCAP2021_TBEP_TotalRestOpp_Table.csv", col_types = cols(gridcode = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_Table)

native <-CCAP2021_TBEP_NativeHabitat_Table
restore <- CCAP2021_TBEP_TotalRestOpp_Table

#calculate acreage sums for native habitats, by C-CAP habitat type ('gridcode')
nathabs <- aggregate(native$Acres , by=list(Group=native$gridcode), FUN=sum)
#change 'Acres' values to full number format
nathabs$x <- format(nathabs$x, scientific=FALSE)
nathabs

#calculate acreage sums for restorable habitats, by C-CAP habitat type ('gridcode')
resthabs <- aggregate(restore$acres , by=list(Group=restore$gridcode), FUN=sum)
#change 'acres' values to full number format
resthabs$x <- format(resthabs$x, scientific=FALSE)
resthabs




######################################################################
#####################ON EXISTING CONSERVATION LANDS###################
######################################################################

#On native habitats

#import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_Native_Exist_Cons_Lands <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_Native_Exist_Cons_Lands.csv", 
 col_types = cols(gridcode = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(CCAP2021_TBEP_Native_Exist_Cons_Lands)

#calculate acreage sums for native habitats, by C-CAP habitat type ('gridcode')
nativecons <- CCAP2021_TBEP_Native_Exist_Cons_Lands
natconshabs <- aggregate(nativecons$Acres , by=list(Group=nativecons$gridcode), FUN=sum)
#change 'Acres' values to full number format
natconshabs$x <- format(natconshabs$x, scientific=FALSE)
natconshabs

#on restorable habitats

##tidal wetlands of existing restoration opportunities
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_TotalRestOpp_TidalWetland_ExisCons <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_TotalRestOpp_TidalWetland_ExisCons.csv", col_types = cols(gridcode_1 = col_factor(levels = c("5", "6", "7", "8", "11", "19", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_TidalWetland_ExisCons)

#calculate acreage sums for tidal wetland habitats, by C-CAP habitat type ('gridcode_1')
tw <- CCAP2021_TBEP_TotalRestOpp_TidalWetland_ExisCons
twhabs <- aggregate(tw$acres , by=list(Group=tw$gridcode_1), FUN=sum)
twhabs

##freshwater wetlands on existing restoration opportunities
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_TotalRestOpp_FW_ExisCons_Table <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_TotalRestOpp_FW_ExisCons_Table.csv", col_types = cols(gridcode_1 = col_factor(levels = c("5","6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_FW_ExisCons_Table)

#calculate acreage sums for freshwater wetland habitats, by C-CAP habitat type ('gridcode_1')
fw <- CCAP2021_TBEP_TotalRestOpp_FW_ExisCons_Table
fwhabs <- aggregate(fw$acres , by=list(Group=fw$gridcode_1), FUN=sum)
fwhabs

##non-coastal uplands on existing restoration opportunities
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_TotalRestOpp_NCUpland_ExisCons_Table <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_TotalRestOpp_NCUpland_ExisCons_Table.csv",col_types = cols(gridcode = col_factor(levels = c("5", "6", "7", "8", "11", "19", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_NCUpland_ExisCons_Table)

#calculate acreage sums for non-coastal upland habitats, by C-CAP habitat type ('gridcode')
up <- CCAP2021_TBEP_TotalRestOpp_NCUpland_ExisCons_Table
uphabs <- aggregate(up$acres , by=list(Group=up$gridcode), FUN=sum)
uphabs

##coastal uplands on existing restoration opportunities
#import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_TotalRestOpp_CoastalUpland_ExisCons_Table <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_TotalRestOpp_CoastalUpland_ExisCons_Table.csv", col_types = cols(gridcode = col_factor(levels = c("5", 
"6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_CoastalUpland_ExisCons_Table)

#calculate acreage sums for coastal upland habitats, by C-CAP habitat type ('gridcode')
cu <- CCAP2021_TBEP_TotalRestOpp_CoastalUpland_ExisCons_Table
cuhabs <- aggregate(cu$acres , by=list(Group=cu$gridcode), FUN=sum)
cuhabs
#change 'acres' values to full number format
cuhabs$x <- format(cuhabs$x, scientific=FALSE)
cuhabs



######################################################################
#####################ON PROPOSED CONSERVATION LANDS###################
######################################################################

#on native habitat
#import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_NativeHab_2021ProposedConsLands_Intersect <- read_csv("CCAP2021_TBEP_NativeHab_2021ProposedConsLands_Intersect.csv", col_types = cols(gridcode = col_factor(levels = c("8",  "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(CCAP2021_TBEP_NativeHab_2021ProposedConsLands_Intersect)

#calculate acreage sums for coastal upland habitats, by C-CAP habitat type ('gridcode')
nativeFLUCCSprop <- CCAP2021_TBEP_NativeHab_2021ProposedConsLands_Intersect
natprophabs <- aggregate(nativeFLUCCSprop$Acres , by=list(Group=nativeFLUCCSprop$gridcode), FUN=sum)
#change 'acres' values to full number format
natprophabs$x <- format(natprophabs$x, scientific=FALSE)
natprophabs


#On restorable habitats
library(readr)
CCAP2021_TBEP_TotalRestOpp_PropCons_Table <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_TotalRestOpp_PropCons_Table.csv",col_types = cols(gridcode = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_TotalRestOpp_PropCons_Table)

#calculate acreage sums restorable habitats, by C-CAP habitat type ('gridcode')
restprop <- CCAP2021_TBEP_TotalRestOpp_PropCons_Table
restprophabs <- aggregate(restprop$acres , by=list(Group=restprop$gridcode), FUN=sum)
#change 'acres' values to full number format
restprophabs$x <- format(restprophabs$x, scientific=FALSE)
restprophabs


#############################################################################
#######################unprotected lands in coastal stratum##################
#############################################################################

#On native habitats
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_Unprotected_Native_Coastal <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_Unprotected_Native_Coastal.csv", col_types = cols(gridcode_1 = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(CCAP2021_TBEP_Unprotected_Native_Coastal)

#calculate acreage sums native habitats, by C-CAP habitat type ('gridcode_1')
natunp <- CCAP2021_TBEP_Unprotected_Native_Coastal
natunphabs <- aggregate(natunp$Acres , by=list(Group=natunp$gridcode_1), FUN=sum)
#change 'Acres' values to full number format
natunphabs$x <- format(natunphabs$x, scientific=FALSE)
natunphabs


#on restorable habitats
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_Reservation_Restorable_Table <- read_csv("CCAP2021_TBEP_Reservation_Restorable_Table.csv", col_types = cols(gridcode_1 = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_Reservation_Restorable_Table)

#calculate acreage sums on restorable habitats, by C-CAP habitat type ('gridcode_1')
restunp <- CCAP2021_TBEP_Reservation_Restorable_Table
restunphabs <- aggregate(restunp$acres_1 , by=list(Group=restunp$gridcode_1), FUN=sum)
#change 'acres_1' values to full number format
restunphabs$x <- format(restunphabs$x, scientific=FALSE)
restunphabs


######################################################################################################
###########ACREAGE EXTENT OF NEWLY IDENTIFIED LANDS (NOT FOUND IN LULC-DERIVED MAPS)###################
#######################################################################################################

#on native habitats
#EXISTING CONSERVATION LANDS-NATIVE
#import csv file, change 'gridcode_1' column type to factor
library(readr)
NewOppExistConsNative <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/NewOppExistConsNative.csv", col_types = cols(gridcode = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(NewOppExistConsNative)

#calculate acreage sums on native habitats, by C-CAP habitat type ('gridcode')
necn <- NewOppExistConsNative
necnacre <- aggregate(necn$Acres , by=list(Group=necn$gridcode), FUN=sum)
#change 'Acres' values to full number format
necnacre$x <- format(necnacre$x, scientific=FALSE)
necnacre

#PROPOSED CONSERVATION LANDS-NATIVE
#import csv file, change 'gridcode' column type to factor
library(readr)
NewOppExistConsNative <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/NewOppPropConsNative.csv", col_types = cols(gridcode = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(NewOppPropConsNative)

#calculate acreage sums on native habitats, by C-CAP habitat type ('gridcode')
npcn <- NewOppPropConsNative
npcnacre <- aggregate(npcn$Acres , by=list(Group=npcn$gridcode), FUN=sum)
#change 'Acres' values to full number format
npcnacre$x <- format(npcnacre$x, scientific=FALSE)
npcnacre

#PROPOSED RESERVATION LANDS-NATIVE
#import csv file, change 'gridcode_1' column type to factor
library(readr)
NewOppReservationNative <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/NewOppReservationNative.csv",col_types = cols(gridcode_1 = col_factor(levels = c("8", "11", "12", "13", "14", "15", "22", "16", "17", "18", "23"))))
View(NewOppReservationNative)

#calculate acreage sums on native habitats, by C-CAP habitat type ('gridcode')
nprn <- NewOppReservationNative
nprnacre <- aggregate(nprn$Acres , by=list(Group=nprn$gridcode_1), FUN=sum)
#change 'Acres' values to full number format
nprnacre$x <- format(nprnacre$x, scientific=FALSE)
nprnacre

#on restorable habitats
#EXISTING CONSERVATION LANDS-RESTORABLE
#import csv file, change 'gridcode' column type to factor
library(readr)
CCAP2021_TBEP_NewOpp_Restorable_Exis_Cons <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_NewOpp_Restorable_Exis_Cons.csv", col_types = cols(gridcode = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_NewOpp_Restorable_Exis_Cons)

#calculate acreage sums on restorable habitats, by C-CAP habitat type ('gridcode')
necr <- CCAP2021_TBEP_NewOpp_Restorable_Exis_Cons
necracre <- aggregate(necr$Acres , by=list(Group=necr$gridcode), FUN=sum)
#change 'Acres' values to full number format
necracre$x <- format(necracre$x, scientific=FALSE)
necracre


#PROPOSED CONSERVATION LANDS-RESTORABLE
#import csv file, change 'gridcode_12' column type to factor
library(readr)
CCAP2021_TBEP_NewOpp_RestPropCons <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_NewOpp_RestPropCons.csv", col_types = cols(gridcode_12 = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_NewOpp_RestPropCons)


#calculate acreage sums on restorable habitats, by C-CAP habitat type ('gridcode_12')
npcr <- CCAP2021_TBEP_NewOpp_RestPropCons
npcracre <- aggregate(npcr$acres_12 , by=list(Group=npcr$gridcode_12), FUN=sum)
#change 'acres_12' values to full number format
npcracre$x <- format(npcracre$x, scientific=FALSE)
npcracre

#PROPOSED RESERVATION LANDS-RESTORABLE
#import csv file, change 'gridcode_1' column type to factor
library(readr)
CCAP2021_TBEP_NewOpp_ReservationRest <- read_csv("ArcGIS/C-CAP/TBEP_CCAP/CCAP2021_TBEP_NewOpp_ReservationRest.csv", 
col_types = cols(gridcode_1 = col_factor(levels = c("5", "6", "7", "8", "11", "20"))))
View(CCAP2021_TBEP_NewOpp_ReservationRest)

#calculate acreage sums on restorable habitats, by C-CAP habitat type ('gridcode_1')
nprr <- CCAP2021_TBEP_NewOpp_ReservationRest
nprracre <- aggregate(nprr$acres_1 , by=list(Group=nprr$gridcode_1), FUN=sum)
#change 'acres_1' values to full number format
nprracre$x <- format(nprracre$x, scientific=FALSE)
nprracre

