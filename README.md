# Correcting inferences for volunteer-collected data with geospatial sampling bias
Repository containing reproducible code belonging to the manuscript _"Correcting inferences for volunteer-collected data with geospatial sampling bias"_ by Peter Lugtig, Annemarie Timmers, and Erik-Jan van Kesteren

## Reproducing the analysis
This is an R project with packages managed by `renv`, on R version `4.1.2`. Clone or download this repository, enter the folder and open the project file (`night_globe.Rproj`) in RStudio. Then, if you have installed the `renv` package, run `renv::restore()` to obtain the right versions of all packages used.

The starting point is the file [`01_data_loading.R`](./01_data_loading.R)

## Extended description
The Globe at Night project contains volunteer-collected data about the brightness of the sky. For example, in Pennsylvania in 2020, the following observations were made:
![](/img/raw_gan.png)

If researchers want to make inferences about how bright the night sky is in Pennsylvania, it would be optimal to observe the night sky at random locations in the state. The volunteer data is not randomly distributed: there is sampling bias. In this repository, we correct for such sampling bias by using geospatial covariates and geospatial models to predict sky brightness throughout Pennsylvania. 

As covariates, we use moon illumination, cloud cover as well as land use data from [https://www.mrlc.gov/](https://www.mrlc.gov/). 

![](/img/raw_landuse.png)

In addition, we have information about where the main roads lie in Pennsylvania. This is data from OpenStreetMaps:

![](/img/raw_highway.png)

Using different models with increasing levels of complexity we obtain the following predicted sky brightness values:

![](/img/model_predictions.png)

We validate these internally, using leave-one-out cross-validation. There, we conclude that the most complex model (model 8, with all covariates and kriging) leads to the best out-of-sample prediction performance.

We can additionally externally validate these models by comparing them against (log-)skyglow measurements derived from satellite imagery:

![](/img/skyglow.png)

![](/img/external_validation.png)

## Contact

This project is developed and maintained by the [ODISSEI Social Data
Science (SoDa)](https://odissei-data.nl/nl/soda/) team.

<img src="word_colour-l.png" alt="SoDa logo" width="250px"/>

Do you have questions, suggestions, or remarks? File an issue in the
issue tracker or feel free to contact [Erik-Jan van
Kesteren](https://github.com/vankesteren)
([@ejvankesteren](https://twitter.com/ejvankesteren))

