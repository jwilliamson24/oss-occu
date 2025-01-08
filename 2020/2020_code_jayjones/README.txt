Jay Jones || 9/7/2024

The enclosed files, with minor changes to file pathways, were used to fit the models 
and produce results for the Garcia et al. 2020 manuscript, and should allow you to 
reproduce everything. Note that the source data came from multiple different individual
datasets. Those data were joined in a separate program to produce the 
'OSS ENES packaged data thru 2019.RData' file enclosed here.

Here are some comments on the enclosed files:
1. OSS ENES packaged data thru 2019.RData: all data required to run the analysis
2. JAGS BACI models 2019.r: JAGS model definition functions.
3. BACI analysis 2013-2019.r: this file reads in the data, sources the JAGS models and fits 
the models to the data.
4. Summary of fitted model results 2013-19.r: this file reads the fitted model objects saved in 
step #3 and creates summaries for the publication.