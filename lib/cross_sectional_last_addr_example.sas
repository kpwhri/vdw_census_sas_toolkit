
%macro last_addr_yr(inset, outset, year, no_zip_post=true);
    %if &no_zip_post. = true. %then %do;
        %let zip_restrict = where match_strength not in (1,2,3) and geolevel not in ('Z','P');
    %end;
    %else %do;
        %let zip_restrict = ;
    %end;

    proc sql;
        create table _tmp as 
        select 
            src.*
            , case 
                when cl.mrn is not null then 'MATCH'
                else 'NO MATCH' 
                end as cl_match
            , cl.geocode
            , cl.geocode_boundary_year
            , cl.loc_start
            , cl.loc_end
        from &inset. src
        left join &_vdw_census_loc cl on
            src.mrn = cl.mrn 
            and &year. between year(cl.loc_start) and year(cl.loc_end)
        &zip_restrict.
        order by 
            src.mrn
            , cl.loc_start
        ;
    quit;

    proc sort data = _tmp out = &outset. nodupkey;
        by mrn descending loc_start;
    run;
%mend last_addr_yr;


%macro loc_to_demog(inset, outset, input_year);
    proc sql;
        create table &outset. as 
        select *
        from &inset. src
        left join &_vdw_census_demog_acs. acs on
            src. geocode = acs.geocode
            and src.geocode_boundary_year = acs.geocode_boundary_year
        where 
            acs.census_year = &input_year.
        ;
    quit;
%mend loc_to_demog;

%macro fetch_ruca2010_from_usda(outdset, ruca_dir, preview=true, infomode=true, switch=w);
    /* produces a tract level dataset with RUCA codes baked in.*/
    %if &switch. = w %then %do;
        %let sw = \;
    %end;
    %else %do;
        %let sw = /;
    %end;
    %if %length(&ruca_dir)=0 %then %do;
        %let ruca_dir=%sysfunc(getoption(work));
    %end;
    
    filename ruca_raw "&ruca_dir.&sw.ruca.xlsx";

    proc http 
        url="https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx?v=290.9"
        out=ruca_raw
        ;
    run;

    proc import 
        datafile=ruca_raw
        out=&outdset
        dbms=xlsx
        replace
        ;
        range="'Data'$A2:I74004";
        datarow=3;
    run;

    data &outdset.;
        set &outdset.;
        rename var4 = geocode;
        geocode_boundary_year = 2010;
    run;

    %if &preview = true %then %do;
        %put INFO: Preview mode set to &preview..;
        proc contents data=&outdset;
        run;

        proc print data=&outdset (obs=5);
        run;
    %end;

    %if &infomode=true %then %do;
        %put INFO: Info mode set to &infomode..;
        %put INFO: Please use the documentation here: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/;
    %end;

%mend fetch_ruca2010_from_usda;
