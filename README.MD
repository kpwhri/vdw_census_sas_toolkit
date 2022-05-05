# Introduction
The reason this repository exists is to create a series of tools for HCSRN programmers to use VDW Census Demographics data consistently. Please feel free to give them a try and post feedback directly on github. If you are not quite sure on how to clone a repository, please send an email to alphonse.derus@kp.org.

When you want to get the latest and greatest macros/updates here, just do a "git pull". 

# Specifications
[Census Location](census_loc_specs.md) - The person + location + time table. 

[Census Demographics](census_demog_acs_specs.md) - Where we get sociodemographics.

# Macros

## %assess_census_demog_acs
This macro assesses the local copy of _&_vdw_census_demog_acs_ and returns a printout of the (census_year, geocode_boundary_year) values available.

## %join_census_loc
This macro performs a left join from an input population (MRN) with a specified index date (either a variable in the dataset or a hard coded date. Default value is today()).

### Example 1 - Explicit index_date out of enrollment {#join_ex_1}
```sas
join_census_loc(in_dset, out_dset, index_date, days_tolerance_pre=0, days_tolerance_post=0, debug=false);
%join_census_loc(
                &_vdw_enroll(where=('31jan2022'd between enr_start and enr_end) obs=10000) /*in_dset*/
                ,SampleJoin                     /*outdset*/
                , index_date = '31jan2022'd     /*explicit index_date */
                , days_tolerance_pre=90         /*how far back can we look for a location period? */
                , days_tolerance_post=90        /*how far forward can we look for a location period? */
                , debug=true                    /*do we want to look at some key frequencies */
                );
```


### Example 2 - Explicit index_date {join_ex_2}

```sas
* make a temp table
data _tmp;
    set &_vdw_enroll(where=(enr_start between '01JUN2018'd and '01JAN2019'd) obs=10000);
    format idx yymmddd10.;
    do i='01jan2017'd to '31dec2021'd by 15;
        idx = i;
        output;
    end;
run;

%join_census_loc(_tmp
                ,SampleJoin_2
                , index_date = idx
                , debug=false);
```

### Example 3 - Explicit index_date {join_ex_3}
```sas

%join_census_loc(_tmp
                ,SampleJoin_3
                , debug=true);
```

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