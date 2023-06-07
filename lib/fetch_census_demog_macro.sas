* Census location - &_vdw_census_loc -- just like enrollment;
%macro fetch_census_demog(
                        input_ds = &_vdw_census_loc. (obs=100) /*YOUR data*/
                        , idvar = mrn /*Have it in your dataset- Do not change unless you are not using MRN as your ID variable (e.g., event level data)*/
                        , geocode_var = geocode /*Have it in your dataset- Do not change unless you call it something else*/
                        , index_date = today() /*You should change this- needs to be a DATE*/
                        , years_prior_tolerance = 5 /*Recommended settings - Because ACS is a rolling average 2018 is really 2014-2018; this setting would allow joins as early as 2013 with 2018 only*/
                        , years_after_tolerance = 3 /*Recommended settings - Because ACS is a rolling average 2018 is really 2014-2018; this setting would allow joins as late as 2021 with 2018 only*/
                        , demog_data_src = &_vdw_census_demog_acs. /*kind of dataset neutral, but the assumption is that we are using data that contains 5 year ACS extracts*/
                        , demog_geo_var = geocode /* VDW uses the term geocode, Census uses geoid, some people say FIPS */
                        , census_yr_var = census_year /* variable in your demog dataset that contains the release year (e.g., 2018 would be 2014-2018) */
                        , outds = work.outds /*Where do you want it to go? */
                        , debug = false
                        ) ;
    * Step 1 - Assess the years of ACS data available and get some boundaries based on tolerances (tol).;
    proc sql;
        create table work.census_years_temp_ as 
        select distinct
            &census_yr_var. as census_year
            , floor(&census_yr_var./10)*10 as geocode_boundary_year
            , &census_yr_var. - &years_prior_tolerance. as census_year_lowest
            , &census_yr_var. + &years_after_tolerance. as census_year_highest
        from &demog_data_src
        ;
    quit;

    * Step 2 - Identify the desired census years from the input parameters.;
    proc sql;
        create table work.census_select_temp_ as 
        select
            ids.&idvar.
            , substr(ids.&geocode_var.,1,11) as geocode
            , year(&index_date.) as index_year
            , &index_date as index_date
            , tol.census_year
            , tol.geocode_boundary_year
            , tol.census_year_lowest
            , tol.census_year_highest
            , case 
                when length(ids.&geocode_var.) < 11 then 'WARNING: No tract level match'
                when length(ids.&geocode_var.) > 11 then 'OK: Note, only tract returned'
                when length(ids.&geocode_var.) = 11 then 'OK: Exact'
                end as match_flag
            , case when missing(tol.census_year) then 'Out of range' else put(tol.census_year,4.) end as year_select
        from &input_ds ids
        left join work.census_years_temp_ tol on
            year(&index_date.) = tol.census_year
            or 
            (year(&index_date.) < tol.census_year
                and year(&index_date.)>= tol.census_year_lowest)
            or 
            (year(&index_date.) > tol.census_year
                and year(&index_date.) <= tol.census_year_highest)
        group by ids.&idvar.
        having abs(year(&index_date)-tol.census_year) = min(abs(year(&index_date)-tol.census_year));
        ;
        * Step 3 - Lastly, join to your census demog table;
        create table &outds. as
        select 
            cst.&idvar
            , cst.index_year
            , cst.index_date format = yymmdd10.
            , cst.census_year_lowest
            , cst.census_year_highest 
            , cst.match_flag
            , cst.year_select 
            , dds.*
        from work.census_select_temp_ cst 
        left join &demog_data_src dds on 
            dds.&demog_geo_var = cst.geocode
            and cst.census_year = dds.census_year
        ;
    quit;

    %if &debug. = false %then %do;
    %end;
    %else %if &debug = true %then %do;
        title4 'Frequency of demographic match flags';
            proc freq data = work.census_select_temp_;
                tables
                    match_flag
                ;
            run;
        title4;
    %end;

%mend fetch_census_demog ;
