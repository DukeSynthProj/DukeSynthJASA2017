The contents of this repository form the Synthetic Data software 
supplement to

"A Framework for Sharing Confidential Research Data, Applied to 
Investigating Differential Pay by Race in the U.S. Government"

Forward comments and questions regarding program design and execution to 
afb26@stat.duke.edu

To generate a synthetic version of the OPM dataset the user has to 
follow the following steps:

1) Split OPM dataset into 29 RData files. Each file comprises the 
available information for each of the 29 variables to be synthesized.  
Place the  29 RData files inside the Aux_Files folder.  Each file has 
to be named  bs_ZZZZ.RData, where ZZZZ has to be replaced by the name 
of the variable. Each RData file contains a data frame named  bs_ZZZZ.  
Replace ZZZZ with the name of the variable. The rows and columns in 
these data frames represent employees and years, respectively. Thus, 
each bs_ZZZZ should have 3,511,824 rows and 24 columns.

2) Specify the right paths in each of the R script files in the Code 
folder.

3)  Open Terminal and execute the commands in Code/00.Commands.txt

Note: the code is tailored for a computer with a 2.60GHz processor, 
48 cores, and 263 GB RAM running  Linux operating system and R version 3.3.2. 
The synthesis process takes about four days. The files containing the 
synthetic version of bs_ZZZZ are located in the Synthetic_Files folder.

