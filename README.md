# North Korean Detainees Human Rights Violations Analysis

This repository contains the analysis of human rights violations against North Korean detainees, based on data from the [NKPD (North Korean Prison Database)](https://nkpd.io/en/library/?q=(allAggregations:!f,from:0,includeUnpublished:!f,limit:30,order:asc,sort:title,treatAs:number,unpublished:!f)).

## Overview

We investigate human rights violations experienced by North Korean detainees, focusing on the network relationships between perpetrators. The dataset provides detailed information on documented cases of abuse, including perpetrator profiles and their roles in the detainee system. Our analysis aims to uncover patterns and potential networks of complicity within the North Korean regime.

## Data Source

The data used in this project is sourced from the NKPD, a comprehensive database of North Korean prison camps and human rights violations.

- **Link to the data:** [NKPD Database](https://nkpd.io/en/library/?q=(allAggregations:!f,from:0,includeUnpublished:!f,limit:30,order:asc,sort:title,treatAs:number,unpublished:!f))
- **Data format:** CSV (pre-processed for analysis)

## Project Structure

- `data/`: Contains the raw and pre-processed datasets.
- `R/`: R scripts for data processing, network analysis, and visualizations.
- `Outputs/`: Stores outputs from data and R, such as figures, tables, etc.
- `README.md`: This document.

## Key Findings

- **Human Rights Violations:** The analysis highlights systematic patterns of abuse and violations committed against detainees.
- **Perpetrator Networks:** By mapping the relationships among perpetrators, we observe organized complicity and hierarchical structures within the prison system.

## How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/nk-detainees-human-rights.git
   ```
   
2. Open RStudio or run R in the terminal and install the required packages by running:
   ```r
   install.packages(c("dplyr", "tidyr", "ggplot2", "igraph", "readr"))
   ```

Run the R scripts in the scripts/ directory to replicate the analysis.

## License
This project is licensed under the MIT License. See the LICENSE file for details.
