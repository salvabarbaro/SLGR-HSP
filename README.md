# README

## Project Overview
This repository contains replication files for the paper:

**"Evaluating the Effectiveness of Intergovernmental Earmarking Grants: Insights from the German Higher-Education Pact"**

**Author:** Salvatore Barbaro  
**Published in:** *State and Local Government Review*  

## Table of Contents
1. [Introduction](#introduction)
2. [Requirements](#requirements)
3. [Setup Instructions](#setup-instructions)
4. [Data Sources](#data-sources)
5. [Replication Workflow](#replication-workflow)
6. [Scripts Overview](#scripts-overview)
7. [Citation](#citation)
8. [Contact Information](#contact-information)

## Introduction
This project aims to replicate the analysis presented in the paper, focusing on the evaluation of earmarked intergovernmental grants within the German Higher-Education Pact. The analysis is divided into five parts, each accompanied by a dedicated script for replication.

## Requirements
- **Operating System:** Debian / Ubuntu / Tuxedo OS   
- **Programming Language:** R  (tested on version 4.1.2 "Bird Hippie")
- **Main Libraries/Packages Used:**
  - dplyr (piping language)
  - wiesbaden (for data access)

Please ensure these dependencies are installed and properly configured on your system.

## Setup Instructions
1. Clone this repository:
   ```bash
   git clone git@github.com:salvabarbaro/SLGR-HSP.git
   ```
2. Navigate to the project directory:
   ```bash
   cd <repository-name>
   ```
3. Install necessary R packages by running:
   ```R
   install.packages(c("dplyr", "wiesbaden"))
   ```
4. Configure your access credentials for the Federal Statistical Office data as described in the [Data Sources](#data-sources) section.

## Data Sources
The data used in this analysis is sourced from the Federal Statistical Office, available free of charge. However, an authorization is required. Once successfully registered, you can use the `wiesbaden` package for immediate access.

**Steps to access data:**
1. Register for an account at the Federal Statistical Office.
2. Obtain your username and password.
3. Configure the `wiesbaden` package to access data using your credentials.

## Replication Workflow
The analysis is divided into five parts. Each part corresponds to a separate R script:

1. **Part 1:** [AdvRel.R]  
   *Analysis of Student-Staff Ratio (Advising Relationship)*
2. **Part 2:** [facultygroups.R]  
   *Mann-Kendall Tests on STEM faculties*
3. **Part 3:** [fedgrants.R]  
   *Distribution of federal grants across states*
4. **Part 4:** [stem.R]  
   *Entrants in STEM (in German: MINT-faculties) (Trend analysis and Regressions)*
5. **Part 5:** [regression.R]  
   *Regression Analyses*

### Execution
Run each script in order, ensuring that all dependencies are met and data access is correctly configured. Use the following command for each R script:
```bash
Rscript <script-name>.R
```

## Scripts Overview
| Script Name       | Description                                     |
|-------------------|-------------------------------------------------|
| AdvRel.R          | Analysis of Student-Staff Ratio                 |
| facultygroups.R   | Mann-Kendall Tests on STEM faculties            |
| fedgrants.R       | Distribution of federal grants across states    |
| stem.R            | Trend analysis and Regressions                  |
| regression.R      | OLS-Regression Analyses                         |

## Citation
If you use this repository, please cite the original paper as follows:

**Salvatore Barbaro** (2024). "Evaluating the Effectiveness of Intergovernmental Earmarking Grants: Insights from the German Higher-Education Pact." *State and Local Government Review*. DOI: https://doi.org/10.1177/0160323X241274263

## Contact Information
For any questions or further assistance, please contact:

**Salvatore Barbaro**  
sbarbaro@uni-mainz.de  
Johannes-Gutenberg University, Mainz
http://decision-making.economics.uni-mainz.de

---

**Last Updated:** Mainz, September 24, 2024
