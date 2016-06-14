Santiago Urban tree analysis
======================


Code for the paper:

**Escobedo J Francisco, Palmas-Pérez S, Dobbs C, Gezan S, Hernandez J. 2016. Spatio-Temporal Changes in Structure for a Mediterranean Urban Forest: Santiago, Chile 2002 to 2014. Forests 7, 121.** [Link](http://dx.doi.org/10.3390/f7060121)




- *GrndCvrClassification.R*: computes correlation between ground covers and reponse variables such as number of species and number of trees

- *Mortality-ResproutingTest.rmd*: Tries to find differences in mortality and resprouting according to tree type

- *Santiago_LandUse_Trees.Rmd*: Probably main file. Finds correlation between BA Biomass, Tree cover and with estrato. Correlation between BA, Biomass, Tree Cover with comuna, lat/long. Correlation between BA, Biomass, Tree Cover with groundcover

- *Santiago_PrepareDataframes.Rmd*: prepares de dataframes from the original data

- *Santiago_Tree_Growth.Rmd*: Script to fit DAP, BA and Biomass growth model per species or classification of species The arboles_ growth.xls file is generated from Santiago_PrepareDataframes.Rmd

- *helpers.R*: function to estimate biomass for each tree according to species.





