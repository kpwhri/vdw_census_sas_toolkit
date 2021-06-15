# Introduction
The reason this repository exists is to create a series of tools for HCSRN programmers to use VDW Census Demographics data consistently. Please feel free to give them a try and post feedback directly on github. If you are not quite sure on how to clone a repository, please send an email to alphonse.derus@kp.org.

# Macros

## %fetch_census_demog 
This macro takes a series of parameters and returns a dataset with the original dataset + whatever census demographics are available (i.e., not dependent on any particular implementation). Example usage below:

```
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
