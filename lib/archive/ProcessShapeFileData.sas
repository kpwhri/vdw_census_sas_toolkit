/*********************************************/

* A bit of character-to-numeric conversion makes note2err not work;
* A one-to-one merge without a by statment makes mergenoby=error not work.;
options mergenoby=warn;

* Directory with unzipped shapefiles;
%let TIGERPATH=\\local\MakeShapefiles\data;

* Output data set directory;
%let DATASETPATH=&ROOT.\download;

* Output data set name prefix;
%let DATASETNAME=washington_;

* Compile macro program;
%include '\\local\TIGER2Geocode (9.4).sas';
* Invoke TIGER import macro;
%TIGER2GEOCODE;

* Rename x, y to long, lat for 9.4 processing;
* The stock GFK datasets use x, y for Miller II projected latitudes.;
* proc datasets lib=&DATASETLIBNAME;
* 	modify &DATASETNAME.P;
* 	rename x=long y=lat;
* run;
* quit;