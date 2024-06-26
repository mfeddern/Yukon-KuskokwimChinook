# AYK Chinook Data
This folder contains Chinook salmon data 

## Goodnews Folder
This contains data for the Goodnews reconstruction developed for this project

## Kuskokwim and Canadian Yukon
File names: "kusko-multi-SRA-ests.csv" and "yukon-multi-SRA-estscsv"

The model that produces these filse is described completely in [Staton et al. (2020)](https://cdnsciencepub.com/doi/10.1139/cjfas-2019-0281) where it was applied to 13 populations in the Kuskokwim River basin. We also applied the model to Yukon River basin Chinook populations (Canadian-origin only) -- see Connors et al. (In Press).

The two csv files contain the output, and contain the columns:

* `quantity`: the type of quantity from the model, one of:
* `S`: escapement to a given population in a given year
* `logR`: log recruitment to a given population in a given year
* `logR_resid`: log recruitment residual for a given population in a given year, which follows an AR(1) process
* `alpha`: Ricker productivity parameter for a given population: max. expected recruits/spawner
* `beta`: Ricker capacity parameter for a given population: inverse of spawners that produce maximum expected recruitment
* `S_msy`: escapement expected to produce maximum sustained yield for a given population
* `U_msy`: exploitation rate expected to produce maximum sustained yield for a given population
* `sigma_R`: log-normal SD of the white-noise portion of the AR(1) recruitment process noise
* `pop`: the population name
* `year`: the year of spawning; for `quantity == "S"`, this is the calendar year that fish spawned; for `quantity %in% c("logR", "logR_resid")`, this is the year in which those fish were spawned. So `logR` and `S` in the same year make one spawner-recruit pair.
* `mean`: The posterior mean of the quantity
* `sd`: The posterior SD of the quantity

The Kuskokwim output file contains two additional columns:

* `model_id`: one of `SSM-vm`, `SSM-vM`, `SSM-Vm`, `SSM-VM`; see Staton et al. (2021) for model definitions. The Yukon analysis used a slightly modified version of model `SSM-VM`.
* `scenario`: one of
* `base`: the base assumptions used to fit the model, results presented in main text
*  `vuln`: an alternate assumption for how populations have been fished unequally in the past, results presented in a supplement
* `ess`: an alternate assumption for how the age composition data should be weighted, results presented in a supplement

### Note: Information about Years

The Kuskokwim models were fitted to data collected calendar (return) years 1976 -- 2017. The Yukon model was fitted to data collected in calendar years 1985 - 2019.

However, for the Kuskokwim, you'll notice that only years 1976 -- 2013 are included and that only years 1985 -- 2015 are included. Age-4 is the minimum age of return, and so the model does not produce recruitment states for years beyond the 4th-to-last observed spawning year -- those recruits have not been observed yet (at the time of model fitting). Further, although these models estimate the first 7 (maximum age-of-return) recruitment states, these events do not have corresponding spawner abundances, so they have been omitted.

Further, the last 3 brood years do not have completely observed recruitment events. E.g., for the Kuskokwim, the recruitment event for brood year 2013 was observed as age-4 returning in 2017, but the age-5, age-6, age-7 components have not yet returned (as of 2017), and so the model must estimate the remaining recruitment abundance of these ages that are not linked to data. Likewise, brood year 2012 has only age-4 and age-5 components observed, brood year 2011 has only age-4, age-5, and age-6 components observed; 2010 was the last brood year where all returning adults had been observed. So although brood years 2011, 2012, and 2013 are included in this output, be warned that they are only partially informed by observed data. The same applies for the Yukon for brood years 2013, 2014, and 2015.

## Giasa and East Fork Andreafsky
File names: "giasaandreafskySR.csv" 

The model that produces these filse is described completely in [Brown et al. (2020)](https://meridian.allenpress.com/jfwm/article/11/2/377/436141/Population-Trends-for-Chinook-and-Summer-Chum) where it was applied to both Chum and Chinook salmon in the EF Andreafsky and Giasa. See supplemental file "10.3996072019-jfwm-064.s4".

These brood year production data were collected between 1994 and 2016 from weirs in the East Fork (EF) Andreafsky and Gisasa rivers, Yukon River tributaries in Alaska. These data include the river, species, brood year, the estimated number of spawners, which is the escapement from weir passage (reconstructed escapement estimates for the 2001 season in the EF Andreafsky River were taken from Fleishman and Evenson 2010 for Chum Salmon and from Siegel 2017 for Chinook Salmon for these production analyses and those values are highlighted), the standard error (SE) of the escapement estimates for years in which some missed segments of the runs were reconstructed, estimates of the number of fish produced by the spawners in age classes 4 - 7 for Chinooks (3 - 6 for Chums), SEs for the number of fish in each age class, an estimate of the number of recruits produced that brood year, and the SEs of the recruitment estimates.

## Chena and Salcha
File names: "chenasalchaSRA.csv" 

The model that produces this file is described completely in Joy et al. in review. 