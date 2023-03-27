# SIR-pancreatic-cancer-BDIPMN
A method to calculate the number of expected cases and therefore the standardized incidence ratio of pancreatic cancer in a cohort of patients under surveillance.

The attached R Markdown serves as a guide to replicate the methods used in the associated study "Surveillance for Presumed BD-IPMN of the Pancreas: Stability, Size, and Age Identify Targets for Discontinuation". 
Please cite our study if you are planning on using the shared code.

The Markdown is stored in the "docs" folder. The .rmd file is the markdown file, also rendered in .html and .pdf files.

In the markdown a random generated dataset is used. The dataset to be used for this analysis is a long dataset, where every row correspond to a new patient and every column to a different variable. The variables that are called by the program are: DOB, DSTART, DLAST (respectively date of birth, starting date of surveillance, last date of surveillance or date of the event), ID (numeric), Sex (string, "FEMALE" or "MALE"), Insitution (in the example data from Singapore "SGH" is managed differently from data from other coutries in list A - see markdown), 
