# Publication- README.md 

# Focuses on creating a publication of my MRes

### Code:   

| Script       | Function     | Input     | Output    |
| :------------- | :----------: | -----------: |-----------: |
|  **`formatting_datasets.R`** |  Takes all of the preliminary datasets and creates usable and comparable datasets  |  BPV 1 and 2 datasets and acoustic dataset from overlap data in drop box  and the satellite_tags_sept_20 dataset in the data file |  BPV_hour_no_dg_sept.csv: all BPV coordinates rounded up EXCLUDING DIEGO GARCIA, BPV_hour_INCLUDE_dg_sept.csv: same dataset but INCLUDES DIEGO GARCIA, acoustic_no_elas_Sept: acoustic dataset with only elasmobranches, formattted, satellite_sept: sat dataset formatted, acoustic_BPV_dataset and satellite_BPV_dataset. csv: datasets combined by time and hour   |
|  **`overlap_sat_acoustic.R`** |  Takes the files made from formatting_datasets.R and runs overlap analysis  |  Output files from fprmatting_datasets.R  | Overlap datasets for analysis found in /data/overlap are the raw overlap datasets and in the folder of all_days is where the summary_sharks datasets are but with additional days filled in. With the exception of satellite tag dataset where there is only the raw summary shark output |




### Data: 
    
Files

	satellite_tags_sept_20.csv: initial dataset from wildlife computers    
	BPV_hour_NO_dg_sept.csv: this is the dataset to use for analysis and has been used for combining BPV datasets as it excludes DG
	BPV_hour_INCLUDE_dg_sept: Incase we want a BPV dataset with DG
	acoustic_no_elas_sept: acoustic dataset that excludes species that arent elasmobranches
	formatted_satellite_Sept: satellite dataset that is comparable 
	acoustic_BPV_datasets: acoustic and BPV dataset ready for overlap analysis 
	satellite_BPV_datasets: satellite and BPV dataset ready for overlap analysis 
	
#####Folder: overlap: 

2016_stations_acoustic_overlap_dataset.csv: raw overlap dataset from just stations installed in 2016
acoustic_overlap_dataset_allstations: raw overlap dataset with all stations 
satellite_overlap_dataset: raw satllite overlap dataset 

######/overlap: all_ days: Basically the summary shark dataframes but with added days for acoustic 

    
    
    
    