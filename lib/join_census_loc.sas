%macro join_census_loc(in_dset, out_dset, index_date, days_tolerance_pre=0, days_tolerance_post=0, debug=false);
    /*
    This macro performs a left join from an input population (MRN) with a specified index date (either a variable in the dataset or a hard coded date. Default value is today()).
    Input parameters:
      in_dset - name of the input dataset
      out_dset - name of the output dataset
      index_date a specified index date (either a variable in the dataset or a hard coded date)
    Default value is today()).

    %isBlank ref - https://communities.sas.com/t5/SAS-Programming/How-to-check-if-a-macro-variable-is-blank/td-p/160327
    */
    %macro isBlank(param);
        %sysevalf(%superq(param)=,boolean)
    %mend isBlank;

    %if %isBlank(&index_date) = 1 %then %do;
        %let index_date = "&sysdate."d;
    %end;

    %if not(&index_date = index_date) %then %do;
        %let _sql_idx = , &index_date. as index_date;
    %end;
    %else %do;
        %let _sql_idx = ;
    %end;

    proc sql;
        create table &out_dset. (drop=mrn_t) as 
        select 
            ids.*
            , loc.*
            &_sql_idx.
            , case 
                when &index_date.  between loc.loc_start and loc.loc_end then 'Match - Exact'
                when &index_date. - &days_tolerance_pre between loc.loc_start and loc.loc_end then 'Match - Meets pre days tolerance'
                when &index_date. + &days_tolerance_post between loc.loc_start and loc.loc_end then 'Match - Meets post days tolerance'
                else 'No match found for record' end as match_type
        from &in_dset ids
        left  join &_vdw_census_loc(rename=(mrn=mrn_t)) loc on
            ids.mrn = loc.mrn_t
            and 
            (
                &index_date. between loc.loc_start and loc.loc_end
                or
                (&index_date. - &days_tolerance_pre) between loc.loc_start and loc.loc_end
                or 
                (&index_date. + &days_tolerance_post) between loc.loc_start and loc.loc_end
            )                
    ;
    quit;

    * print out some frequencies in the event that there is something wrong - geocode_boundary_year match_type;
    %if &debug = true %then %do;
        
        proc contents data = &out_dset. order = varnum;
        run;

        proc freq data = &out_dset.;
            tables
                geocode_boundary_year
                match_type
                index_date
            ;
            format index_date year4.;
        run;
    %end;
    %else %do;
    %end;

%mend join_census_loc;