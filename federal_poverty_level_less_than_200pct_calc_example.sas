* Example of federal poverty level < 200% calculation;
%let pov_lt200_lst = POV_LT_50 POV_50_74 POV_75_99 POV_100_124 POV_125_149 POV_150_174 POV_175_184 POV_185_199;
%fetch_census_demog(
                        input_ds = &input_ds /*YOUR data*/
                        , idvar = mrn /*Have it in your dataset- Do not change unless you are not using MRN as your ID variable*/
                        , geocode_var = geocode /*Have it in your dataset- Do not change unless you call it something else*/
                        , index_date = today() /*You should change this- needs to be a DATE*/
                        , years_prior_tolerance = 5 /*Recommended settings - Because ACS is a rolling average 2018 is really 2014-2018; this setting would allow joins as early as 2013 with 2018 only*/
                        , years_after_tolerance = 3 /*Recommended settings - Because ACS is a rolling average 2018 is really 2014-2018; this setting would allow joins as late as 2021 with 2018 only*/
                        , demog_data_src = &_vdw_census_demog_acs. (keep= census_year geocode &pov_lt200_lst.) /*kind of dataset neutral, but the assumption is that we are using data that contains 5 year ACS extracts*/
                        , demog_geo_var = geocode /* VDW uses the term geocode, Census uses geoid, some people say FIPS */
                        , census_yr_var = census_year /* variable in your demog dataset that contains the release year (e.g., 2018 would be 2014-2018) */
                        , outds = work.outds /*Where do you want it to go? */
                        , debug = true
                        ) ;
%let lst_comma = %sysfunc(tranwrd(%quote(&pov_lt200_lst.),%str( ),%str(,)));

%put INFO: lst_comma = &lst_comma.;

data outds_pov;
    set outds;
    fpl_lt_200_pct = sum(&lst_comma.);
run;

proc univariate data = outds_pov; 
    output out=pov_uni;
    var &pov_lt200_lst. fpl_lt_200_pct;
    id mrn;
    histogram fpl_lt_200_pct /kernel (c=SJPI);
run;




