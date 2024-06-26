# AYK-Chinook
Repository for examining environmental drivers and diversity of responses of Chinook salmon populations in the Arctic-Yukon-Kuskokwim region

## Project contacts
Megan Feddern, mfeddern@alaska.edu
Rebecca Shaftel, rshaftel@alaska.edu
Erik Schoen, eschoen@alaska.edu
Curry Cunningham, cjcunningham@alaska.edu

###DATA-SPECIFIC INFORMATION FOR: EBS_Pollock.csv

1. Number of variables: 7
2. Number of cases/rows: 58
3. Variable List: 
    Year: year abundance was measured
    SSB: female spawning biomass
    CV_SSB: coefficient of variation for the female spawning biomass estimate
    Recruitment: age 1 recruitment
    CV_Rec: Coeffcient of variation for age 1 recruitment estimate
    Age_3_Biomass: biomass estimate for pollock age 3+
    CV_Age3: Coefficient of variation for the age 3+ biomass estimate
4. Missing data codes: 
    NA
5. Abbreviations used: 
    SSB; Spawning Stock Biomass
6. Other relevant information:
    Data is from: Assessment of the Walleye Pollock Stock in the Eastern Bering Sea, 2020. https://apps-afsc.fisheries.noaa.gov/refm/docs/2020/EBSPollock.pdf
    
###DATA-SPECIFIC INFORMATION FOR: SeaIceIndices.csv

1. Number of variables: 3
2. Number of cases/rows: 40
3. Variable List: 
    Year: year the ice indices was measured
    ICIA: I cover index that is the acerage ice concentration in a 2 degree by 2 degree box (56N-58N, 163W-165W) from January 1 to 31 March 1980-2017
    IRI: Bering sea ice retreat index. Number of days after 15 of March when the sea ice covers 10% of the 2 degree by 2 degree box (56N-58N and 163W-165W) in teh southeast Bering Sea 
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from:
    https://www.beringclimate.noaa.gov/data/index.php
    Spatial delineations were those described in:
     Yasumiishi et al. 2020 Differential north-south response of juvenile Chinook salmon (Oncorhynchus tshawytscha) marine growth to ecosystem change in teh eastern Bering Sea, 1974-2020, ICES Journal of Marine Science 77(1), 216-229.

###DATA-SPECIFIC INFORMATION FOR: NCEP_NCAR_SST.csv

1. Number of variables: 7
2. Number of cases/rows: 76
3. Variable List: 
    Year: year SST was measured
    EarlySummer_North: Average SST from June-August
        Latitude Range: 60.1N to 65N
        Longitude Range: 165W to 172.5W
    LateSummer_North: Average SST from September-October
        Latitude Range: 60.1N to 65N
        Longitude Range: 165W to 172.5W
    EarlySummer_South: Average SST from June-August
        Latitude Range: 60N to 56.2N
        Longitude Range: 161.2W to 172.5W
    LateSummer_South: Average SST from September-October
        Latitude Range: 60N to 56.2N
        Longitude Range: 161.2W to 172.5W
    Winter_South: Average SST from January-March
        Latitude Range: 60N to 56.2N
        Longitude Range: 161.2W to 172.5W
    Winter_North: Average SST from January-March
        Latitude Range: 60N to 56.2N
        Longitude Range: 161.2W to 172.5W
    Winter_combined: average SST from January-March for SEBS and NEBS
    EarlySummer_combined: average SST from June-August for SEBS and NEBS
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from:
    Kalnay, E. and Coauthors, 1996: The NCEP/NCAR Reanalysis 40-year Project. Bull. Amer. Meteor. Soc., 77, 437-471. https://psl.noaa.gov/cgi-bin/data/timeseries/timeseries1.pl
    Spatial delineations were those described in:
     Yasumiishi et al. 2020 Differential north-south response of juvenile Chinook salmon (Oncorhynchus tshawytscha) marine growth to ecosystem change in teh eastern Bering Sea, 1974-2020, ICES Journal of Marine Science 77(1), 216-229.

###DATA-SPECIFIC INFORMATION FOR: PDO.csv

1. Number of variables: 14
2. Number of cases/rows: 119
3. Variable List: 
    Year: year PDO was measured
    JAN-DEC: monthly Pacific Decadal Oscillation Index
    PDO_Annual_Mean: annual PDO mean
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from:
   
###DATA-SPECIFIC INFORMATION FOR: NPGO.csv

1. Number of variables: 3
2. Number of cases/rows: 824
3. Variable List: 
    Year: year NPGO was measured
    Month: month NPGO was measured
    NPGO: North Pacific Gyre Oscillation index measured monthly
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from:http://www.o3d.org/npgo/
        
###DATA-SPECIFIC INFORMATION FOR: Borealization_dfa_trend.csv

1. Number of variables: 4
2. Number of cases/rows: 51
3. Variable List: 
    t: year DFA trend was measured
    estimate: estimate of the DFA trend
    conf.low: lower confidence interval of trend
    conf.high: higher confidence interval for trend
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from: Mike Litzow, snow crab project in prep. Index is the DFA trend from 1) ice cover, 2) Bottom temperature, 3) Ice-edge bloom, 4) open water bloom 5) bloom timing, 6) bloom size, 7) phytoplankton size, 8) pseudocalanus, 9) Calanus glacialis 10) hematodinium 11) Pacific cod, 12) Arctic groundfish
   
###DATA-SPECIFIC INFORMATION FOR: uwindMonthly.csv

1. Number of variables:  13   
2. Number of cases/rows: 75
3. Variable List: 
    Year: year wind was measured
    1-12: month wind vector "u" was measured where 1 corresponds to January and 12 corresponds to December for the monthly mean. Measurements were taken at the surface at 60N and 170W representing the cross-shelf wind vector as described by Danielson et al. 2012 and Stachura et al. 2014. Positive values correspond to SE Ekman transport along the shelf
    
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from: NCEP-NCAR Reanalysis 1 https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html

###DATA-SPECIFIC INFORMATION FOR: vwindMonthly.csv

1. Number of variables:  13   
2. Number of cases/rows: 75
3. Variable List: 
    Year: year wind was measured
    1-12: month wind vector "v" was measured where 1 corresponds to January and 12 corresponds to December for the monthly mean. Measurements were taken at the surface at 60N and 170W representing the along-shelf wind vector as described by Danielson et al. 2012 and Stachura et al. 2014. Positive values correspond to SE Ekman transport onto the shelf
    
4. Missing data codes: 
    NA
5. Abbreviations used: 
6. Other relevant information:
    Data is from: NCEP-NCAR Reanalysis 1 https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html

###DATA-SPECIFIC INFORMATION FOR: salmonabundanceOkeetal

1. Number of variables: 7
2. Number of cases/rows: 71
3. Variable List: 
    Year: year abundance was measured 1951-2015
    chumTotalAK: Abundance of chum salmon returning to Alaska including natural and hatchery catch and escapement in millions of fish
    pinkTotalAK:Abundance of pink salmon returning to Alaska including natural and hatchery catch and escapement in millions of fish  
    sockTotalAK:Abundance of sockeye salmon returning to Alaska. including natural and hatchery catch and escapement in millions of fish
    chumTotal:North Pacific abundance of returning chum salmon. including natural and hatchery catch and escapement in millions of fish
    pinkTotal:North Pacific abundance of returning pink salmon. including natural and hatchery catch and escapement in millions of fish
    sockTotal:North Pacific abundance of returning sockeye salmon. including natural and hatchery catch and escapement in millions of fish
4. Missing data codes: 
    NA corresponds to years number were not reported
5. Abbreviations used: 
6. Other relevant information:
    Data is from: Summarized by Ruggerone, G. T. & Irvine, J. R. Numbers and biomass of natural- and hatchery-origin Pink Salmon, Chum Salmon, and Sockeye Salmon in the North Pacific Ocean, 1925 – 2015. Mar. Coast. Fish. Dyn. Manag. Ecosyst. Sci. 10, 152–168 (2018) and accessed from https://knb.ecoinformatics.org/view/doi:10.5063/F1N29V9T via Oke et al. 2018.
