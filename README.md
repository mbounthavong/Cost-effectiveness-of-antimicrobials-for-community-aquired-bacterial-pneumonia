# Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia
Files for the study evaluating the cost-effectiveness of antimicrobials for community-acquired pneumonia. 

Our decision tree model is located in the [R code for decision tree folder](https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20code%20for%20decision%20tree/Decision%20tree%20model_Github_version.R). 

Several R functions were used to generate the figures and simulations and are located in the [R Functions folder](https://github.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/tree/main/R%20Functions). These functions were developed by the [Decision Analysis in R for Technologies in Health (DARTH)](https://darthworkgroup.com/) group. 

We made several assumptions regarding the parameters for our distribution assignments. For example, we did not have the standard deviations for several probability parameters, so we had to make an assumption about the alpha and beta parameters. To verify that our assumptions were reasonable, we visually inspected the simulated beta distributions for its shape and form. Details regarding our distributions and the visual plots are available in our [R Markdown Code for distributions folder](https://raw.githubusercontent.com/mbounthavong/Cost-effectiveness-of-antimicrobials-for-community-aquired-bacterial-pneumonia/main/R%20Markdown%20code%20for%20distributions/CEA_lefamulin_distributions.Rmd)



Citations:

- Alarid-Escudero F, Krijkamp E, Pechlivanoglou P, Jalal H, Kao SYZ, Yang A, Enns EA. A need for change! A coding framework for improving transparency in decision modeling. PharmacoEcon. 2019;37(11):1329–1339.

- Jalal HJ, Pechlivanoglou P, Krijkamp E, Alarid-Escudero F, Enns EA, Hunink MG.  An overview of R in Health Decision Sciences. Med Decis Making. 2017;37(7): 735-46.

- Suen S-C, Goldhaber-Fiebert JD. An Efficient, Noniterative Method of Identifying the Cost-Effectiveness Frontier. Med Decis Making. 2015;2–6. 
