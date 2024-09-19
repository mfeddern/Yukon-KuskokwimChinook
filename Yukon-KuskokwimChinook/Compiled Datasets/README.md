# Body size and early marine conditions drive changes in Chinook salmon productivity across northern latitude ecosystems

[https://doi.org/10.5061/dryad.9w0vt4bqm](https://doi.org/10.5061/dryad.9w0vt4bqm)

## Description of the data and file structure

This repository contains estimates of Chinooks salmon spawner and recruit abundance for 26 populations in the Yukon and Kuskokwim regions of Alaska and Canada. This repository also contains indices of environmental conditions evaluated for their effect on Chinook salmon productivity. 

### Files and variables

#### File: FreshwaterIndices_Feddernetal.csv

**Description:** Indices of freshwater conditions hypothesized to impact Chinook salmon productivity throughout their lifecycle. Watershed specific indices were developed for 26 Chinook salmon populations across the Yukon-Kuskokwim region  from DAYMET and GloFAS models.

##### Variables

* Population: River for which the index was derived associated with a specific Chinook salmon spawning population
* Region: Subregion for which each Population is located
* year: Calendar year associated with the index
* max5dprcp_spawn: Maximum 5-day precipitation from August - November associated with spawning/incubation lifestage; unit = cm
* mnprcp_rear: Median daily precipitation from May - September associated with the juvenile rearing lifestage; unit = cm
* maxq_spawn: Maximum daily streamflow from August - November associated with egg incubation; unit = m^3/s
* medq_rear: Median daily streamflow from May - September associated with juvenile rearing; unit = m^3/s
* mean_swe: Mean snowpack in April represented by snow-water equivalent associated with juvenile rearing; unit  = cm
* cdd_rear: Cumulative degree days (base temperature 0 degree C) from May - September associated with juvenile rearing; unit = degree day
* maxDaily_spawn: Maximum daily stream temperature from August - November during spawning/incubation; unit = degree C
* maxDaily_migrate: Maximum daily stream temperature from July - August during spawning migration; unite = degree C
* maxWeekly_migrate: Maximum weekly stream temperature from July - August during spawning migration.daysGT17_migrate; unit = degree C
* cddGT17_migrate: Cumulative degree days (base temperature 17 degree C) from July - August during spawning migration; unit = degree day

#### File: StandardizedData\_Feddernetal.csv

**Description:** Standardized indices with brood year offset of freshwater and marine conditions hypothesized to impact Chinook salmon productivity throughout their lifecycle and model estimated spawner and recruit abundance and associated uncertainty from previous studies. Notably, these spawner and recruit data represent relative indices are not suitable for analyses that require absolute metrics (i.e., stock assessments); use of these indices should carefully consider the assumptions made in the associated source models which are included in the "Dataset Description" and cite the associated source material. Each covariate dataset is standardized by subtracting the region (all populations), subregion, basin or population-level mean and dividing by the standard deviation such that the mean is 0 and standard deviation is 1. 

##### Variables

* Year: Brood year of the Chinook salmon population
* Population: Chinook salmon population 
* Subregion: Subregion associated with each Chinook salmon population (Kuskokwim, Yukon (US), or Yukon (CA))
* RiverBasin: River Basin associated with each Chinook salmon population (either Yukon River or Kuskokwim River)
* logspawners: log transformed estimate of the number of spawners that returned during the brood year estimated from previous modeling efforts (see repository description for the data source)
* logrecruits: log transformed estimate of the number of recruits produced during the brood year estimated from previous modeling efforts (see repository description for the data source)
* spawn.error: model estimated uncertainty for the number of spawners (from previous research, see repository description for data source).
* rec.error: model estimated uncertainty for the number of recruits (from previous research, see repository description for data source). 
* SummerSST: RiverBasin specific mean sea surface temperature from June - August in the Bering sea. Standardized by RiverBasin with a +2 year brood year offset.
* WinterSST: RiverBasin specific mean sea surface temperature from January - March in the Bering sea. Standardized by RiverBasin with a +3 year brood year offset.
* MarineCompetitorsIndex: Latent trend derived from dynamic factor analysis that included pink and chum salmon abundance, biomass of Walleye pollock, and Walleye pollock recruitment. Stadardized across the entire region with a +3 year brood year offset.
* IceConcentrationIndex: Average ice concentration in a 2 degree by 2 degree box (56°N-58°N, 163°W-165°W) from January 1 to March 31 standardized across the entire region with a +2 year brood year offset.
* BodySize: Latent trend of body size of adult spawners across populations in each RiverBasin with a +0 year brood year offset.
* Uwind: Mean monthly zonal wind vector "u" at the surface at 60°N and 170°W. Standardized across the entire region with +2 year brood year offset.
* RiverIceBreakup: Date of mainstem river ice breakup standardized by subregion with +2 year brood year offset.
* maxq_spawn: Maximum daily streamflow from August - November associated with egg incubation. Standardized by each population with +0 year brood year offset.
* medq_rear: Median daily streamflow from May - September associated with juvenile rearing. Standardized by each population with +0 year brood year offset.
* maxDaily_spawn_stand: Maximum daily stream temperature from August - November during spawning/incubation. Standardized by each population with +0 year brood year offset.
* maxDaily_migrate: Maximum daily stream temperature from July - August during spawning migration. Standardized by each population with +0 year brood year offset.
* cdd_rear: Cumulative degree days (base temperature 0 degree C) from May - September associated with juvenile rearing. Standardized by each population with +1 year brood year offset.

#### File: goodnewsSR.csv

**Description:** 

##### Variables

* year: Brood year
* pop: Chinook salmon population
* quantity: categorical classification of abundance as either spawners or recruits
* mean: mean annual abundance

## Code/software

The datasets were processed using R and Stan

## Access information

This is a collated dataset of various potential covariates and modeled Chinook salmon spawner and recruitment estimates investigated for Feddern et al. "Body size and early marine conditions drive changes in Chinook salmon productivity across northern latitude ecosystems". The authors of this dataset were not the original collectors of these data, but instead collated them from publicly available datasets, reports, and published papers. They are archived here for reproducibility and the original sources should be acknowledged. We are very grateful to the people who originally collected these data and developed the associated models and run reconstructions. The data were collected from: 

This is a collated dataset of various potential covariates and modeled Chinook salmon spawner and recruitment estimates investigated for Feddern et al. "Body size and early marine conditions drive changes in Chinook salmon productivity across northern latitude ecosystems". The authors of this dataset were not the original collectors of these data, but instead collated them from publicly available datasets, reports, and published papers. They are archived here for reproducibility and the original sources should be acknowledged. We are very grateful to the people who originally collected these data and developed the associated models and run reconstructions. The data were collected from: 

Complete code can be found at: [doi.org/10.5281/zenodo.13696302](https://doi.org/10.5281/zenodo.13696302)

***Chinook salmon spawner and recruitment data***

Chena and Salcha Rivers: [https://www.adfg.alaska.gov/FedAidPDFs/FMS24-02.pdf](https://www.adfg.alaska.gov/FedAidPDFs/FMS24-02.pdf) see Table 1 and Table 2

Kuskokwim River Basin: [https://doi.org/10.1139/cjfas-2019-0281](https://doi.org/10.1139/cjfas-2019-0281) 

East Fork of the Adreafsky and Gisasa River: [https://doi.org/10.3996/072019-JFWM-064](https://doi.org/10.3996/072019-JFWM-064) via [https://doi.org/10.3996/072019-JFWM-064.S4](https://doi.org/10.3996/072019-JFWM-064.S4)

Canadian Yukon River Basin: [https://doi.org/10.1002/eap.2709](https://doi.org/10.1002/eap.2709) see  [https://doi.org/10.5281/zenodo.6625526](https://doi.org/10.5281/zenodo.6625526)

***Marine Covariates***

Wind and summer SST: [https://psl.noaa.gov/rest/data.noaa.ersst.v5.html](https://psl.noaa.gov/rest/data.noaa.ersst.v5.html)  accessed 2022-05-08

Winter SST: [https://github.com/MattCallahan-NOAA/SST-shiny](https://github.com/MattCallahan-NOAA/SST-shiny) accessed 2023-06-10

River Ice Breakup: [https://arcticdata.io/catalog/view/doi:10.18739/A28W38388](https://arcticdata.io/catalog/view/doi:10.18739/A28W38388) accessed 2022-05-08

Chum and Pink salmon abundance: [https://knb.ecoinformatics.org/view/doi:10.5063/F1N29V9T](https://knb.ecoinformatics.org/view/doi:10.5063/F1N29V9T) accessed 2022-05-08

Sea Ice Cover: [https://www.beringclimate.noaa.gov/data](https://www.beringclimate.noaa.gov/data) accessed 2022-05-08 

Walleye Pollock biomass and recruitment: [https://www.npfmc.org/library/safe-reports/](https://www.npfmc.org/library/safe-reports/) accessed 2022-05-08

Body size: [https://knb.ecoinformatics.org/view/doi:10.5063/F1707ZTM](https://knb.ecoinformatics.org/view/doi:10.5063/F1707ZTM) accessed 2022-05-08

***Freshwater Covariates***

Temperature data derived from Daymet: [https://daymet.ornl.gov/](https://daymet.ornl.gov/) accessed 2022-11-26

Streamflow derived from GloFAS: [https://essd.copernicus.org/articles/12/2043/2020/](https://essd.copernicus.org/articles/12/2043/2020/)  via [https://doi.org/10.24381/cds.a4fdd6b9](https://doi.org/10.24381/cds.a4fdd6b9) accessed 2022-11-26
