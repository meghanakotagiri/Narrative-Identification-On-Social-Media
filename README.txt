Readme file for usage of Narrative identification project
---------------------------------------------------------
 
Commands for installing all the prerequisite packages are mentioned in init.R
opinions.R contains the code for identifying different narratives of a topic and finding the abstraction and sentiment for each of those narratives. 
The function final_clusters in opinions.R file has to be called with 3 parameters 1. search term  2.filename 3. number of tweets to be fetched from the file.
The output of opinions.R will be 4 files: 
    1.  A csv file containing all the narratives 
    2.  A text file containing the average silhouette width of all narratives combined and the silhouette widths of each individual narrative
    3.  An xlsx file containing for each narrative, the terms in it, sentiment value of the terms, manhattan distance of the terms from the centroid        of the cluster of that narrative, term frequency of each term and (term frequency/manhattan distance) value for each term
    4.  An xlsx file containing for each narrative, top 20 tweets ranked in the order of their manhattan distance from centroid, tweet count of that        tweet, abstraction of that narrative and the sentiment corresponding to that abstraction
 
In brief, the steps to run:
    1. Run init.R
    2. Run opinions.R after changing the parameters inside final_clusters function(call at the last line of the file)