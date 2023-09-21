# memoire-pot-growth

This repo contains all the code to reproduce analyses that will be the backbone of my master's thesis. The first attempts to explore these data were made in [this folder](https://github.com/clessn/riding-volatility/tree/main/memoire). This repo is built upon the explorations made in this folder and organizes it in a cleaner way.

# Structure of the folders

## code

List of the folders and their function/description.
- `refining` : the scripts in this folder are used to generate the semi-simulated, analysis-ready datasets (marts) by wrangling and merging different tables in the `data/warehouse` folder.
- `analysis` : this folder contains the scripts that take data from the `data/marts` folder and perform different analyses. 

## data
**the `data` folder is in the `_SharedFolder_memoire-pot-growth`, which is available on request.**

List of the folders and their function/description.
- `warehouse` : this folder is used to stock warehouse tables. Datasets in this folder are structured tables from different sources (public opinion surveys, expert surveys, census data). They are not ready to be analyzed. They are mostly treated in the `code/
- 
