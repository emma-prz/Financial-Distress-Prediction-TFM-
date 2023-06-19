# Financial-distress-prediction-TFM-MUCSS
This repository contains my Master Thesis for the Master's in Computational Social Sciences (UC3M): *Financial distress prediction: a data-driven approach for Spanish companies* . It has all the developed code and a dummy dataset that allows to run it. 

**WARNING!** Since the original data used [CBI-BELab](https://www.bde.es/wbe/es/para-ciudadano/servicios/belab/contenido/microdatos-disponibles/microdatos-de-empresas/microdatos-empresas-individuales-cbi.html) cannot be published, a small sample (primarily anonymized and shuffled) is provided. 
Thus, the results obtained with this data are *completely random!*

## Project structure
   There are 4 main folders:
 -  **Codigo**: contains the code. The *0.MASTER.R* file executes the whole project sourcing all the required parts (and loading all libraries needed at the beginning).
 The R project (*Codigo.Rproj*) to run properly the whole code in the folders structure is also in this folder. 
 -  **Salidas**: contains tables with summary statistics and other results such as performance metrics.
 -  **Graficos**: contains the generated graphs included in the final document.
 -  **Datos**: contains the initial dummy data (*bajas_cbi.R*). It also keeps the intermediate data files that are created in the code.
    
## Code structure 
The code is divided in 4 parts according to the stages of the project, which appear in the *0.MASTER.R* script as follows:
- *Part 1*: Generates subsamples of prosperous and closed firms, and the full sample
- *Part 2*: Performs the distress analysis which provides the definition for financial hardship
- *Part 3*: Matching and imputation techniques applied to the training and testing samples
- *Part 4*: Models training and evaluation 

Inside of each of these sections there are different R scripts which carry out the different tasks. The intermediate data sets and all outputs are generated whitin these R scripts and saved to their corresponding folders. To avoid saving files, the lines which export results can be commented. Otherwise, intermediate files can be later used to run parts of the code without running the previous omes. 

## Reproducibility
Download the full repository in zip format. By opening the *Codigo.Rproj* and then running the *0.MASTER.R* the project will be fully executed. Throughout the project many plots and graphs are displayed on the screen (correlation plots, missing resumes, density plots,...) but only those used for the final document are saved.


