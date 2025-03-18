NOTE THAT THESE FILES REPLICATE ONLY THE TABLES AND GRAPHS OF THE MAIN PAPER. RANDOM NOISE HAS BEEN ADDED TO PARTICIPANT RESPONSES SO NO-ONE SCOOPS US! We plan on making the raw data available when we no longer risk being scooped.

Run the files in the following order. There are Python, R and Stata files. 


1. "qualtrics_to_stata.do" takes the raw qualtrics output and transforms it to 'regression ready' data. We provide access to the cleaning code for transparency, though ** THOSE WITHOUT ACCESS TO THE RAW DATA FILES CANNOT RUN THIS. ** 
2. gini.R is used to calculate Gini coefficients. While the three .csv files are provided such that users do not have to run gini.R, those wanting to replicate the results from scratch should run gini.R prior to 'replication.do' as it feeds into Figure 5. 
3. "replication.do" runs the analysis.
4. 'tex_tables.ipynb' is a python file. The regression output in the table is not the standard output. Coz stata is ... difficult I can't wrangle the tables the way I want, so outputting the results in text files, and re-ordering the coefficients in python ready for a .tex table.
4a). 'helper_functions.py' provides some helper functions, but doesn't need to be run. It needs to be in the same file as 'tex_tables.ipynb'.
Note that the python files do not need to be run, the results are also presented in Stata (albeit, in an equivalent, but different manner to that in the paper).


