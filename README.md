# Inference from volunteer data: Globe at Night


## Correcting inferences in volunteer data using geospatial covariates.

The Globe at Night project contains volunteer-collected data about the brightness of the sky. For example, in Pennsylvania in 2020, the following observations were made:
![](/img/raw_gan.png)

If researchers want to make inferences about how bright the night sky is in Pennsylvania, it would be optimal to observe the night sky at random locations in the state. The volunteer data is not randomly distributed: there is sampling bias. In this repository, we correct for such sampling bias by using geospatial covariates and geospatial models to predict sky brightness throughout Pennsylvania. 

As covariates, we use moon illumination as well as land use data from [https://www.mrlc.gov/](https://www.mrlc.gov/). 

![](/img/raw_landuse.png)

Using different models with increasing levels of complexity we obtain the following predicted sky brightness values:

![](/img/model_predictions.png)

We can then externally validate these models by comparing them against (log-)skyglow measurements derived from satellite imagery:

![](/img/skyglow.png)

The results show that the model with kriging and land use covariates best approximates the log-skyglow in Pennsylvania (R² = .264).

![](/img/comparison.png)

## Contact

This project is developed and maintained by the [ODISSEI Social Data
Science (SoDa)](https://odissei-data.nl/nl/soda/) team.

<img src="word_colour-l.png" alt="SoDa logo" width="250px"/>

Do you have questions, suggestions, or remarks? File an issue in the
issue tracker or feel free to contact [Erik-Jan van
Kesteren](https://github.com/vankesteren)
([@ejvankesteren](https://twitter.com/ejvankesteren))

