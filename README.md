# DataScienceCapstoneProject
Capstone Project for Data Science Specialization through Coursera.org

The following files were used in the creation of my final prjoject which
was to build a shiny app that could predict then next word in a line of text.

The shiny app can be found here:https://careisin.shinyapps.io/DataScienceCapstoneProject/

After you type in a phrase and click "predict" the first time, it will take a while because it has to read in data and set up formulas, but after the first time it will
run much faster.

The server.R file is the culmination of the following R scripts

* Capstone_GetData.R
   - reads in the data, samples it, and sets up the Corpus
* Capstone_CleanData.R
   - cleans the corpus of profanity, nubers, punctuation, etc.
* Capstone_Ngram.R
   - Creates the Ngram models that were used in the algorithm
* Capstone_Good-Turing.R
   - Builds a Good-Turning table based on the frequencies of frequncies
* Capstone_PredictionModel.R
   - Creates some tables and the prediction algorithm that was used in the server.R file
   
Special thanks to Jay Gendron, whose provided some wisdom and guidance from his time in
the capstone project. 
