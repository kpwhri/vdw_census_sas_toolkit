# Indexes
Indexes can often tell us more about a population than a single variable in the Census. When using them, here are the questions that come to my mind:
1. How was it made? SAS?
1. What was the intended purpose for use?
1. How much has it been used?
1. What is the "grain"? County? Tract? Zip? Block Group? What are the consequences of including nested features? 
1. Was it made with ACS 1, 3, or 5 year extract?


## Area Deprivation Index (Wisconsin University)
* Link - 
* Unit of analysis - Block Group
* Years Released - 2015, 2019
* ACS Version - 1,3,5????

### Linkability with VDW
Limited. Block groups are a bit more homogenous than tracts and you can't simply pick one as the rank values of block groups within a tract can be wildly different. It might be possible to get an initial to get counts of people within a given block group to see how you could _weight_ a rank, but I don't really know what measure of center would be appropriate (possibly just a weighted mean). Would need some validation with a statistician to confirm that this is a reasonable approach.

### Links
https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&as_vis=1&q=area+deprivation+index&btnG=

## Neighborhood Deprivation Index (Messer)
* Link
* Unit of analysis
* Years Released
* ACS Version - 1,3,5????

### Linkability with VDW
Good. KP has been using the Messer Neighborhood Deprivation Index for a long time and has found it to be useful.

### Links
* https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&as_vis=1&q=messer+neighborhood+deprivation+index+%22kaiser+permanente%22&btnG=


## Social Vulnerability Index (CDC)

### Info
* Link - https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2018.html
* Unit of analysis
* Years Released
* ACS Version - 1,3,5????

### Good uses

### Caveats
It is literally made in Excel. I've asked for code, but there is none to be had. 

```
When replicating SVI using Microsoft Excel or similar software, results may differ slightly from databases  on the SVI website or ArcGIS Online. This is due to variation in the number of decimal places used by the different software programs. For purposes of automation, we developed SVI using SQL programming language. Because the SQL programming language uses a different level of precision compared to Excel and similar software, reproducing SVI in Excel may marginally differ from the SVI databases downloaded from the SVI website. For future iterations of SVI, beginning with SVI 2018, we plan to modify the SQL automation process for constructing SVI to align with that of Microsoft Excel. If there are any questions, please email the SVI Coordinator at svi_coordinator@cdc.gov.
```
