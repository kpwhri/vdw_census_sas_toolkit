%macro fetch_ruca2010_from_usda(outdset, ruca_dir, preview=true,infomode=true);

    %if %length(&ruca_dir)=0 %then %do;
        %let ruca_dir=%sysfunc(getoption(work));
    %end;
    
    filename ruca_raw "&ruca_dir.\ruca.xlsx";

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
