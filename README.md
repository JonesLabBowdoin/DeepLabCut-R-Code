# DeepLabCut-R-Code
This is the code used to process the DeepLabCut outputs for summer 2021

This code was used to process multiple DeepLabCut csv files into one excel workbook ready for data analysis. In summation, it loops through every csv file, and creates dataframes containing all instances of a bee object being within the defined x,y areas of a flower object. It then lists these "visits" in sequential order and calculates the time spent within each flower object and the time spent flying. The rest of the code deals with formatting the final output. This code requires labelled reference points for each video so that the positions of the flower objects can be determined. However the code will still work if reference points are not required (albeit with minor modifications). 
