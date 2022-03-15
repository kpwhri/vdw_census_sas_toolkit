# Introduction
The reason this repository exists is to create a series of tools for HCSRN programmers to use VDW Census Demographics data consistently. Please feel free to give them a try and post feedback directly on github. If you are not quite sure on how to clone a repository, please send an email to alphonse.derus@kp.org.

# Macros

## %fetch_census_demog 
This macro takes a series of parameters and returns a dataset with the original dataset + whatever census demographics are available (i.e., not dependent on any particular implementation). 

**NOTE: We will need to update when 2020 demog is available**

Example usage below:

```sas
* define target dataset;
%let acs5yr = acs.acs_demog_v;
%fetch_census_demog(
                input_ds = &_vdw_census_loc. (obs=100) /*YOUR data*/
                , idvar = mrn /*Have it in your dataset-  Do not change unless you are using study_id or something*/
                , geocode_var = geocode /*Have it in your dataset- join your dataset to &_vdw_census_loc where  indexdate between loc_start and loc_end - do not touch*/
                , index_date = today() /*You should change this - needs to be a DATE*/
                , years_prior_tolerance = 5 /*Recommended settings*/
                , years_after_tolerance = 3 /*Recommended settings*/
                , demog_data_src = &_vdw_census_demog_acs. /*switch to new file*/
                , demog_geo_var = geocode /*Do not change*/
                , census_yr_var = census_year /*Do not change*/
                , outds = work.outds /*Where do you want it to go? */
                ) ;
```

## %fetch_ruca_2010_from_usda
This macro takes two parameters and returns a dataset containing the USDA Rural-Urban Commuting Area (RUCA) codes. RUCA codes are typically generated 3 years after the last decennial, so 2020 codes could be available in 2023. This could have been parameterized to take previous RUCA vintages, but the grain is very different, and currently VDW only has 2010-2019 data. It might be the case that we want to have a more abstract solution in the future. 

The preview parameter runs a proc contents and prints the first five observations of the returned dataset.

Please reference values found in the [Documentation](https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/).

Example usage below:
```sas
%let outdset = work.ruca2010;
* this reads as run the macro, store the result in the default work library, and output to the ruca2010 dataset in the work library. Allow the default preview and infomode values so I can see a contents and top 5 values from the output dataset and have an INFO statement in the log that tells me about the documentation;
%fetch_ruca_2010_from_usda(&outdset);
```