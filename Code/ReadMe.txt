This document details what each code file does. The order that the code should be run in is identified by the number at the front of the file. These files contain commented out sections of code used for visualisation, output extraction and other iterations. Where data is input or output, destinations need to be set. Legacy Code and Investigations can be located in Archive_R Folder.

Order of Reproducible Code Workflow:

000_Load_Libraries - Loads Required Libraries for Code.

001_Extract_STATS19_2016 - Extracts the STATS19 collision file for 2016.

002_Download_webTRIS_Site_Locations_JSON - Downloads and stores the webTRIS site locations for all MIDAS sites.

003_Subset_STATS19_Data-Near_Sensors - Identifies STATS19 Dataset on NTIS Network and within 2km of webTRIS sites.

004_Download_MIDAS_Gold_Traffic_Data_2016 - Downloads the MIDAS Gold from the Halogen Portal, Requires Username and Password.

005_extract_to_tcd - Converts the MIDAS Gold dataset from .tcd.bz2 to .tcd ready for extraction.

006_Extract_Test_Sites_MIDAS_Gold_3_Day_Windows - Extracts the MIDAS Gold dataset using the oneminutetrafficdata package. Note: Data needs to be in a .tcd format.

007_Load_MIDAS_Gold - Loads the Extracted MIDAS Gold data into the workflow.

008_Formatting_MIDAS_Gold_Dataset - Formats MIDAS Gold dataset.

009_Identify_Collision_Proximity_In_Traffic - Sets 1 when within an Hour Before or After a collision and within 500m of that collision and creates Additional Aggregated Columns.

010_Random_Forest_Prediction_Investigation - Random Forest Model Approach.

Extra Code:

101_Visualisation_Example_Collision - Visualises example collision in MIDAS Gold Dataset.