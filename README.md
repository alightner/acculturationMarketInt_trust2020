# Code and supplementary repository for Lightner and Hagen (2020) EHS paper

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4118454.svg)](https://doi.org/10.5281/zenodo.4118454) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4118474.svg)](https://doi.org/10.5281/zenodo.4118474)

Here is the analysis code with the required R environment for reproducing the paper, "Acculturation and market integration are associated with greater trust among Tanzanian Maasai pastoralists". Note that we use the [renv package](https://rstudio.github.io/renv/articles/renv.html) to ensure reproducibility across different machines and at different times.

Supplementary materials are included here as a pdf, titled `supplementary.pdf`. They are also linked to a stable and long-term repo on zenodo, referenced in the main paper and [linked here](https://doi.org/10.5281/zenodo.4118474).

The rendered paper from this repo can be compared to the pdf of the original, which is also included in this repo. It is titled `acculturationMarketInt_paper.pdf`, whereas the rendered version will be generated as `trust-advice-writeup.pdf`, rendered when running the `trust-advice-writeup.rmd` file. (The "final" version is distinct only because we made a slight Latex change to the formatting of authors and institutions at the top of the title page.)

The required dataset, downloaded automatically with renv in this project, is the [monduliDataset data package](https://github.com/alightner/monduliDataset). As discussed in the data package readme, slight alterations were made to certain variables in the publicly available version of the data to prevent participant identifiability. These variables were not included in the confirmatory results, and will therefore not affect them. They do, however, slightly change the exploratory results (not in a substantial or qualitatively different way), as the reader might notice when reproducing this paper. See the monduliDataset readme for more information.

The preregistration for this study can be found here: [https://osf.io/5p7ut](https://osf.io/5p7ut)