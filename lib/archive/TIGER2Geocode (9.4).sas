/*------------------------------------------------------------------------------*
 | Name     : TIGER2GEOCODE
 |
 | Title    : Import US Census Bureau TIGER/Line shapefiles and create lookup
 |            data sets for street level geocoding in SAS 9.4 or later.
 |
 | Product  : SAS/GRAPH - PROC GEOCODE
 |
 | System   : Windows and Unix
 |
 | Release  : SAS 9.4 or later required
 |
 | History  : 29Jan10/V1: For third maintenance release SAS 9.2 (TS2M3).
 |            07Jul11/V2: Renamed TIGER2GEOCODE and added support for
 |                        TIGER 2010 variable names.
 |            05Aug11/V3: Use PROC MAPIMPORT to read DBF files instead of PROC IMPORT.
 |            11Jan12/V4: Skip American Samoa as it has no FEATNAMES file.
 |                        Add 2010 to Census tract/block/blkgrp var labels.
 |                        Last version for pre-9.4 lookup data format.
 |            03Aug12/V5: Drop FIPS codes from 'M' data set and output
 |                        state/place names for 9.4 GEOCODE lookup format.
 |                        Remove unused IMPORTPATH macro var.
 |                        Change ZIP var label to 'ZIP/Post code'.
 |            08Oct12/v6: Skip streets with empty parens () for name.
 |            22Mar13/v7: Add trap if running pre-9.4 SAS.
 |            01May13/v8: Add option to ignore pre-9.4 trap.
 |            14Nov13/v9: Add extra MDS obs if '&' or 'and' in street name.
 |                        Remove superfluous MDS obs pointing to same SDS.
 |                        Clean up dirty type prefix/suffix from TIGER featnames.
 |                        Add obs for alternate street names from TIGER files.
 |                        Set house numbers greater than MACLONG to missing.
 |            02Jun14/v10: Include ZCTA values in import. 
 |                         Add city name only if city from ZCTA and ZIP agree.
 |                         Drop duplicate county boundary chains.
 |                         Stop duplicating X/Y values for left/right sides.
 |                         Use most recent state/county vars (STATEFP/COUNTYFP).
 |                         Correct labels for 2009 Census vars.
 |            22Jan15/v11: Replace invalid ZIPs with ZCTA values.
 |                         Add MDS obs where city is a Census Designated Place (CDP).
 |                         Handle Hawaii Interstate H-1 as IH-1.
 |                         Do not move type prefix from street name to PreTypAbrv.
 |                         Do not move type suffix from street name to SufTypAbrv.
 |                         Do not move direction prefix if 2nd word is '&' or 'and'.
 |                         Clean specific dirty featnames during import.
 |                         Do not add extra 'Saint' obs if 'St' last in street name.
 |            28May15/v12: Reenable moving direction prefix/suffix out of street name.
 |
 | Input    : TIGER/Line shapefiles, shape index files and dbf files
 | Data     : downloaded from the US Census Bureau and unzipped into a
 | Source   : common directory. See the TIGERPATH macro variable.
 |
 | Required : TIGER/Line files required for each county of interest:
 | TIGER    :    EDGES.SHP     - All Lines shapefile
 | Files    :    EDGES.SHX     - Shapefile index
 |               EDGES.DBF     - All Lines attribute data table
 |               FACES.DBF     - Topological Faces relationship table
 |               FEATNAMES.DBF - Feature Names relationship table
 |            TIGER/Line file required for each state:
 |               PLACE.SHP     - Incorporated cities and CDPs
 |            Other TIGER/Line file types are ignored by the import program.
 |
 | Required : TIGERPATH      - Location of unzipped TIGER/Line files to
 | Macro    :                  import. Libref TIGER is set to this location.
 | Vars     : DATASETPATH    - Location to write geocoding lookup data sets
 |                             created. If the directory does not exist it
 |                             will be created. Libref name in macro variable
 |                             DATASETLIBNAME is set to this location.
 |
 | Optional : DATASETNAME    - Optional prefix for final lookup data sets.
 | Macro    :                  If not specified, 'US' is the default prefix.
 | Vars     : DATASETLIBNAME - Optional libname for lookup data set location
 |                             specified in DATASETPATH. If not specified,
 |                             LOOKUP is default libref.
 |          : ANYVERSION     - Option to run under any SAS release. Allows
 |                             9.3 SAS to import 9.4 lookup data format.
 |
 | FACES    : The default variables imported from FACES files vary by TIGER
 | Vars     : release year. The default names can be changed by defining the
 |            appropriate SAS macro variable. For example, use '%let TRACTVAR=xxx;'
 |            where 'xxx' is the FACES variable to be imported for Census Tracts.
 |            ------------- -------    ---------- ---------  ------------------------
 |            SAS Macro Var 2007-09    2010       2011-2019  Description
 |            ------------- -------    ---------- ---------  ------------------------
 |            STATEVAR      STATEFP    STATEFP10  STATEFP    State FIPS codes
 |            COUNTYVAR     COUNTYFP   COUNTYFP10 COUNTYFP   County FIPS codes
 |            TRACTVAR      TRACTCE00  TRACTCE10  TRACTCE10  Census Tracts
 |            BLKGRPVAR     BLKGRPCE00 BLKGRPCE10 BLKGRPCE10 Census Block Groups
 |            BLOCKVAR      BLOCKCE00  BLOCKCE10  BLOCKCE10  Census Blocks
 |            PLACEVAR      PLACEFP    PLACEFP10  PLACEFP    Census-designated Places
 |            ZCTAVAR       ZCTA5CE    ZCTA5CE10  ZCTACE10   ZIP Code Tabulation Area
 |
 | Output   : 1) Two data sets containing names of each TIGER file to import
 |               are written in the TIGERPATH location:
 |                 TIGER._counties_to_import - EDGES, FACES and FEATNAMES filenames 
 |                                             for each county
 |                 TIGER._states_to_import   - PLACE filenames for each state
 |            2) Separate data sets for each imported county are written to the
 |               WORK library. Data sets are named using the state name
 |               and FIPS code. For example, the data set for Sussex County,
 |               Delaware (state FIPS 10, county FIPS 005) would be
 |               WORK.COUNTY_DE_10005, and the data set for 
 |               Weber County, Utah would be WORK.COUNTY_UT_49057.
 |            3) Two data sets of all counties and states imported are written to 
 |               the DATASETPATH directory. The libname is either the default LOOKUP
 |               or an alternate specified with the DATASETLIBNAME input parameter: 
 |                 LOOKUP._county_data_sets - County data sets created
 |                 LOOKUP._state_data_sets  - State PLACE files imported
 |            4) Various scratch data sets are written to the WORK library.
 |            5) Three final data sets used by PROC GEOCODE for street method
 |               geocoding are written to the DATASETPATH directory.
 |               Values stored in DATASETLIBNAME and DATASETNAME are used
 |               for the libref and base data set names:
 |                  [DATASETLIBNAME].[DATASETNAME][M|S|P]
 |               The letters M, S, and P are appended to the data set names.
 |               For example, if you use the default libname LOOKUP and the
 |               default data set prefix US, the three data set names are:
 |                  LOOKUP.USM
 |                  LOOKUP.USS
 |                  LOOKUP.USP
 |               If you imported only Delaware TIGER files and specified these
 |               macro var values:
 |                  %let DATASETNAME=DELAWARE_;
 |                  %let DATASETLIBNAME=GEOCODE;
 |               The final lookup data set names would be:
 |                  GEOCODE.DELAWARE_M
 |                  GEOCODE.DELAWARE_S
 |                  GEOCODE.DELAWARE_P
 |
 | Summary  : 1) Download Census Bureau TIGER/Line files for the desired US
 |               counties and states from www.census.gov/geo/www/tiger.
 |            2) Unzip all downloaded TIGER files into a single directory.
 |            3) Set required TIGERPATH macro var to that location.
 |            4) Set required DATASETPATH macro var to directory where the
 |               final PROC GEOCODE lookup data sets are to be written.
 |            5) Specify desired optional macro vars from above list.
 |            6) Submit TIGER2GEOCODE.sas file to compile macro programs.
 |            7) Invoke the TIGER2GEOCODE macro program.
 |            8) Check log for errors or warnings.
 |            9) Review geocoding lookup data sets.
 |            10) Test lookup data using PROC GEOCODE street method.
 |                See PROC GEOCODE documentation.
 |            11) Create backup of PROC GEOCODE lookup data sets.
 |            12) If no longer needed, delete temporary files and data sets:
 |                   a) Downloaded TIGER/Line zip files
 |                   b) Unzipped TIGER files in TIGERPATH
 |                   c) Individual county data sets in WORK
 |                   d) Interim files and data sets in WORK
 |            12) Lookup data sets are ready for street level geocoding.
 |                See LOOKUPSTREET= option in PROC GEOCODE doc for instructions.
 |
 | Example  : %let TIGERPATH=C:\Geocoding\TIGER files;   %* Directory with unzipped shapefiles
 |            %let DATASETPATH=C:\Geocoding\Lookup data; %* Output data set directory
 |            %let DATASETNAME=DELAWARE_;                %* Output data set name prefix
 |            %include 'C:\Geocoding\TIGER2GEOCODE.sas'; %* Compile macro program
 |            %TIGER2GEOCODE                             %* Invoke TIGER import macro
 *------------------------------------------------------------------------------*/
%macro TIGER2GEOCODE /
       des='Import TIGER shapefiles to create PROC GEOCODE street method lookup data';

  /*--- Initialize global variables, check for required and optional
        input variables, determine operating system, set librefs and
        create needed directories. */
  %InitializeImport
  %if &TIGERERROR=yes %then %goto MacroExit;

  /*--- List the names of all files in the TIGERPATH folder
        and use a pipe to transfer that list to SAS.
        The command to list the files varies by operating system. */
  %if &OSNAME=UNIX %then %do;
    /*--- Use Unix 'ls' command to list files. */
    filename filelist pipe %unquote(%str(%'ls "&TIGERPATH"%'));
  %end;
  %else %if &OSNAME=WINDOWS %then %do;
    /*--- Use Windows 'dir' command to list files. */
    filename filelist pipe %unquote(%str(%'dir "&TIGERPATH\*" /b%'));
  %end;

  /*--- Loop through the piped list of file names and put each into
        sequential macro variable, e.g. &FNAME1, &FNAME2, ... &FNAMEn.
        Each &FNAMEi variable contains a TIGER file name with extension,
        e.g. 'tl_2009_10001_edges.shp' or 'tl_2009_10005_faces.dbf'. The
        number of macro vars containing file names is stored in &FILETOTAL. */
  data _null_;
    infile filelist truncover end=finalObs;
    length fname $50;
    input fname;      /* Get TIGER file name from pipe */
    retain i j 1;
    /*--- Import only the files needed for street level geocoding. */
    if index(fname, '_edges.shp ') |
       index(fname, '_faces.dbf ') |
       index(fname, '_featnames.dbf ') then do;
      /*--- File is a required type. For now, skip any files for FIPS code 
            60030 which is American Samoa. The Census Bureau does not release a 
            FEATNAMES file for that territory, so we cannot create street-level
            lookup data for it. If they ever begin providing this file type for
            Samoa, this 'if' and its corresponding 'end' can be removed. */
      if ^index(fname, '_60030_') then do;
        /*--- Put name in numbered FNAMEi macro var and increment file counter. */
        call symputx('FNAME' || trim(left(put(i,8.))), trim(fname));
        i+1;
      end;
    end;
    else if index(fname, '_place') & index(fname, '.shp ') then do;
      /*--- Put PLACE file name in numbered PNAMEi macro var and increment counter.
            Note: The above place file name check was split into two strings 
                  because 2010 place file names are: tl_2010_01_place10.shp 
                  But other TIGER years use: tl_2011_01_place.shp */
      call symputx('PNAME' || trim(left(put(j,8.))), trim(fname));
      j+1;
    end;
    /*--- If last file, put final counts into macro variables. */
    if finalObs then do;
      call symputx('FILETOTAL',  trim(left(put(i-1,8.))), 'g');
      call symputx('PLACETOTAL', trim(left(put(j-1,8.))), 'g');
    end;
  run;

  /*--- If required geocoding TIGER files not found, exit. */
  %if &FILETOTAL=0 %then %do;
     %put ERROR: There are no county TIGER shapefiles to import;
     %put ERROR- in the TIGERPATH location:;
     %put ERROR-   &TIGERPATH;
     %let TIGERERROR=yes;
  %end;
  %if &PLACETOTAL=0 %then %do;
     %put ERROR: There are no state PLACE shapefiles to import;
     %put ERROR- in the TIGERPATH location:;
     %put ERROR-   &TIGERPATH;
     %let TIGERERROR=yes;
  %end;
  %if TIGERERROR=yes %then
     %goto MacroExit;

  /*--- Loop through the filenames in the &FNAMEi macro variables and put
        them into a data set. These will be the TIGER files to import. */
  data TIGER._counties_to_import
             (label="TIGER county files to import with TIGER2GEOCODE macro"
              keep=path filename state stctyfips);
    length path $ 260 filename $ 50 state $ 24 stctyfips $ 5;
    label path      = 'Path to TIGER file'
          filename  = 'TIGER file to import'
          state     = 'State Name'
          stctyfips = 'State/County FIPS';
    %do i=1 %to &FILETOTAL;
      path      = symget('TIGERPATH');
      varname   = resolve('FNAME' || trim(left(put(&i, 8.))));
      filename  = symget(varname);
      stctyfips = scan(filename, 3, '_');
      state     = fipnamel(input(substr(stctyfips, 1, 2), 8.));
      output;
    %end;
  run;

  /*--- The filenames in the above data set should have been output
        in alphabetical order. But the import code depends on that
        being true, so sort it just to be sure. */
  proc sort data=TIGER._counties_to_import;
    by filename;
  run;

  /*--- Verify that all required TIGER files were unzipped for each county.
        Each county requires three primary TIGER files to create
        the PROC GEOCODE lookup data sets:
           EDGES.SHP
           FACES.DBF
           FEATNAMES.DBF
        The EDGES.SHP file also requires two associated files as well:
           EDGES.SHX - a shape index file
           EDGES.DBF - an attribute data file */
  data _null_;
    set TIGER._counties_to_import end=finalObs;
    retain fileIdx 1 totalNum 0 prevState prevfips tigerPath;
    length expectedType $ 24 prevState $ 24 prevfips $ 5 tigerPath $ 1000;
    if _n_=1 | fileIdx=1 then do;
      tigerPath = symget('TIGERPATH');
      prevState = state;
      prevFIPS  = stctyfips;
    end;
    /*--- Make sure primary TIGER files are present and in expected order. */
    if (fileIdx=1 & ^index(filename, 'edges.shp ')) |
       (fileIdx=2 & ^index(filename, 'faces.dbf ')) |
       (fileIdx=3 & ^index(filename, 'featnames.dbf ')) then
      /*--- Expected file not found. */
      goto ErrorMsg1;
    else
      /*--- File is valid. */
      totalNum+1;
    /*--- If current file is an EDGES.SHP shapefile, make sure its
          associated EDGES.SHX and EDGES.DBF files are both present. */
    if index(filename, 'edges.shp ') then do;
      assocfile = tranwrd(filename, '.shp', '.shx');
      if fileexist(trim(tigerPath) || assocfile)=0 then
        goto ErrorMsg2;
      assocfile = tranwrd(filename, '.shp', '.dbf');
      if fileexist(trim(tigerPath) || assocfile)=0 then
        goto ErrorMsg2;
    end;
    /*--- Increment or reset file name index. */
    if fileIdx=3 then
      fileIdx = 1;
    else
      fileIdx + 1;

    /*--- If not three primary files per county, something is missing. */
    if finalObs & mod(totalNum, 3) then
      goto ErrorMsg1;
    return;

  ErrorMsg1:
    /*--- One of the county's primary TIGER files is missing. */
    select(fileIdx);
      when(1) do;
        expectedType  = 'EDGES.SHP';
        expectedState = state;
        expectedfips  = stctyfips;
      end;
      when(2) do;
        expectedType  = 'FACES.DBF';
        expectedState = state;
        expectedfips  = stctyfips;
      end;
      when(3) do;
        expectedType  = 'FEATNAMES.DBF';
        expectedState = prevState;
        expectedfips  = prevfips;
      end;
      otherwise do;
        expectedType  = 'UNDETERMINED';
        expectedState = state;
        expectedfips  = stctyfips;
      end;
    end;
    /*--- Print name of missing primary file to log. */
    prefix       = substr(filename, 1, 8);
    expectedFile = trim(left(prefix))       ||
                   trim(left(expectedfips)) ||
                   '_'                      ||
                   left(expectedType);
    expectedObs  = totalNum + 1;
    put 'ERROR: TIGER file ' expectedFile 'for ' expectedState;
    put 'ERROR- was not found. It should be observation ' expectedObs 'in';
    put 'ERROR- TIGER._counties_to_import. Either add the missing file';
    put 'ERROR- or remove all other TIGER files for ' expectedState;
    put 'ERROR- FIPS ' expectedfips 'from the TIGERPATH directory:';
    put "ERROR- &TIGERPATH";
    call symput('TIGERERROR','yes');
    stop;

ErrorMsg2:
    /*--- Print name of missing associated file to log. */
    expectedObs = totalNum;
    put 'ERROR: TIGER file ' assocfile 'for ' state;
    put 'ERROR- was not found. It is an associated file for the';
    put 'ERROR- ' filename 'file in observation ' expectedObs 'in';
    put 'ERROR- TIGER._counties_to_import. Either add the missing file';
    put 'ERROR- or remove all other TIGER files for ' state;
    put 'ERROR- FIPS ' stctyfips 'from the TIGERPATH directory:';
    put "ERROR- &TIGERPATH";
    call symput('TIGERERROR','yes');
    stop;
  run;
  %if &TIGERERROR=yes %then %goto MacroExit;

  /*--- Loop through the PLACE filenames in the &PNAMEi macro variables and
        put them into a data set. These are state PLACE files to import. */
  data TIGER._states_to_import
             (label="TIGER PLACE files to import with TIGER2GEOCODE macro"
              keep=path filename state stfips);
    length path $ 260 filename $ 50 state $ 24 stfips $ 2;
    label path     = 'Path to TIGER file'
          filename = 'PLACE file to import'
          state    = 'State Name'
          stfips   = 'State FIPS';
    %do i=1 %to &PLACETOTAL;
      path     = symget('TIGERPATH');
      varname  = resolve('PNAME' || trim(left(put(&i, 8.))));
      filename = symget(varname);
      stfips   = scan(filename, 3, '_');
      state    = fipnamel(input(stfips, 8.));
      output;
    %end;
  run;

  /*--- Make sure we have state PLACE files for all the counties. */
  data StatesForCounties (drop=statePrv);
    set tiger._counties_to_import (keep=state filename stctyfips path);
    statePrv = lag1(state);
    if state ^= statePrv then output;
  run;

  proc sort data=StatesForCounties;
    by state;
  run;

  proc sort data=tiger._states_to_import;
    by state;
  run;

  data tiger._missing_place_files (label="States where required TIGER &TIGERYEAR PLACE files not found"
                                   drop=filename stctyfips stfips MissingStateNum);
    merge statesForCounties tiger._states_to_import (keep=state stfips filename);
    by state;
    retain MissingStateNum 0;
    label MissingPlaceFile = 'TIGER PLACE file not found'
          path             = 'Folder searched for TIGER files'
          state            = 'State Name';
    if missing(stfips) then do;
       MissingPlaceFile = substr( filename, 1, 8 ) || 
                         trim(substr( stctyfips, 1, 2 )) ||
                         '_place.shp';
      MissingStateNum + 1;
      output;
      call symputx( 'MISSINGSTATENUM', MissingStateNum, 'g' );
    end;
  run;

  %if %eval(&MISSINGSTATENUM > 0) %then %do;
    %put ERROR: Required state PLACE files not found in TIGER input data.;
    %put ERROR- For each county to be imported, you must also have the matching;
    %put ERROR- state PLACE shapefile. See the TIGER._missing_place_files data set;
    %put ERROR- for the required shapefile names and location.; 
    %let tigererror=yes;
    %goto MacroExit;
  %end;

  /*--- Import all state PLACE files to create linkages between Census Designated
        Places (CDPs) and incorporated cities/towns. */
  %ImportPlaceFiles
  %if &TIGERERROR=yes %then %goto MacroExit;

  /*--- Import all the county TIGER files. */
  %ImportTIGERFiles
  %if &TIGERERROR=yes %then %goto MacroExit;

  /*--- Combine all the county data sets and generate the three street
        method lookup data sets. */
  %CreateStreetGeocodeData

  /*--- Apply final sorting, indexes and var labels. */
  %FinalizeDataSets

%MacroExit:
  /*--- Print summary to log. */
  %if &TIGERERROR=no %then %do;
    /*--- Lookup data sets were created successfully. */
    %if &COUNTYTOTAL=1 %then
      %put NOTE: TIGER2GEOCODE macro imported &COUNTYTOTAL county.;
    %else
      %put NOTE: TIGER2GEOCODE macro imported &COUNTYTOTAL counties.;
    %put NOTE: &FILETOTAL county EDGES/FACES/FEATNAMES files and;
    %put NOTE- &PLACETOTAL state PLACE files were read from TIGERPATH:;
    %put NOTE-   &TIGERPATH;
    %put NOTE: STREET method lookup data sets for use with;
    %put NOTE- PROC GEOCODE 9.4 and later are in DATASETPATH:;
    %put NOTE-   &DATASETPATH;
    %put NOTE- Lookup data set names are:;
    %put NOTE-   &DATASETLIBNAME..&DATASETNAME.M;
    %put NOTE-   &DATASETLIBNAME..&DATASETNAME.S;
    %put NOTE-   &DATASETLIBNAME..&DATASETNAME.P;
    %put NOTE- CDP/city data set is:;
    %put NOTE-   &DATASETLIBNAME..CDP_CITY;
    %put NOTE- Data set listing counties imported is:;
    %put NOTE-   &DATASETLIBNAME.._COUNTY_DATA_SETS;
    %put NOTE: After validating lookup data, files in TIGERPATH;
    %put NOTE- can be archived or deleted.;
    %if %eval(&BADZIPS > 0) %then %do;
      %put WARNING: &BADZIPS invalid ZIP codes found in TIGER data.;
      %put WARNING- They are in the &DATASETLIBNAME..INVALID_ZIPS data set.;
    %end;
  %end;
  %else
    /*--- Errors occurred. */
    %put ERROR: TIGER2GEOCODE macro failed.;
%mend TIGER2GEOCODE;

/*------------------------------------------------------------------------------*
 | Name:    InitializeImport
 | Purpose: Verify required macro variables are present, check for optional
 |          macro variables, determine operating system, set librefs, and
 |          create needed directories.
 | Input:   Macro vars specified by user with require parameters.
 | Output:  n/a
 *------------------------------------------------------------------------------*/
%macro InitializeImport;
  /*--- Declare global macro variables.                                */
  %global OSNAME          /* Is operating system Windows or Unix?      */
          TIGERYEAR       /* Year of the TIGER release                 */
          TIGERERROR      /* Did fatal error occur?                    */
          FILETOTAL       /* Number of TIGER files imported            */
          COUNTYTOTAL     /* Number of counties imported               */
          PLACETOTAL      /* Number of state PLACE files imported      */
          NAMEMAX         /* Length needed for NAME var                */
          NAME2MAX        /* Length needed for NAME2 var               */
          CITYMAX         /* Length needed for CITY var                */
          CITY2MAX        /* Length needed for CITY2 var               */
          PREDIRABRVMAX   /* Length needed for PREDIRABRV var          */
          SUFDIRABRVMAX   /* Length needed for SUFDIRABRV var          */
          PRETYPABRVMAX   /* Length needed for PRETYPABRV var          */
          SUFTYPABRVMAX   /* Length needed for SUFTYPABRV var          */
          STATE           /* 2-char state postal abbreviation          */
          STATEVAR        /* STATE FIPS variable name in FACES.DBF     */
          COUNTYVAR       /* COUNTY FIPS variable name in FACES.DBF    */
          TRACTVAR        /* Census TRACT variable name in FACES.DBF   */
          BLKGRPVAR       /* Census BLKGRP variable name in FACES.DBF  */
          BLOCKVAR        /* Census BLOCK variable name in FACES.DBF   */
          PLACEVAR        /* Census PLACE variable name in FACES.DBF   */
          ZCTAVAR         /* 5-digit ZIP Code Tabulation Area in FACES */
          COUNTYVARLABEL  /* COUNTY FIPS variable label in SDS         */
          TRACTVARLABEL   /* Census TRACT variable label in SDS        */
          BLKGRPVARLABEL  /* Census BLKGRP variable label in SDS       */
          BLOCKVARLABEL   /* Census BLOCK variable label in SDS        */
          ZCTAVARLABEL    /* ZCTA label in SDS                         */
          BADZIPS         /* Number of invalid ZIP codes in TIGER data */
          MISSINGSTATENUM /* Number of state PLACE files not found     */
          CLEANCHARS      /* Chars to remove from street/city names    */
          ANYVERSION;     /* Allow macro to run on pre-9.4 SAS         */

  /*--- Initialize global vars. */
  %let TIGERERROR      = no;
  %let FILETOTAL       = 0;
  %let COUNTYTOTAL     = 0;
  %let PLACETOTAL      = 0;
  %let BADZIPS         = 0;
  %let MISSINGSTATENUM = 0;
  %let CLEANCHARS      = " -/.,_'"; /* Note first character is a blank space */

  /*--- Remove any accumulator data sets from previous runs. */
  proc datasets kill nolist lib=work;
  run;

  /*--- GEOCODE uses lookup data with place/state names instead of FIPS codes
        for SAS 9.4 and later. If SAS version earlier than 9.4, complain.
        This check can be turned off if ANYVERSION=yes. That will allow you
        to import the 9.4 lookup data with any SAS release, but those data
        sets will be usable only by PROC GEOCODE in 9.4 or later. */ 
  %if (^%symexist(ANYVERSION) | %upcase(&ANYVERSION)^=YES) &
        %eval(&sysver < 9.4) %then %do; 
    %put ERROR: This version of the TIGER2GEOCODE macro runs only on SAS 9.4;
    %put ERROR- or later. PROC GEOCODE in your SAS release (&sysver) requires;
    %put ERROR- FIPS codes in the street lookup data which are not generated by;
    %put ERROR- this version of TIGER2GEOCODE. Download the TIGER2GEOCODE program;
    %put ERROR- for your SAS release from MapsOnline.;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- Determine operating system. */
  %if %index(&sysscpl, HP)    = 1 |
      %index(&sysscpl, Linux) = 1 |
      %index(&sysscpl, AIX)   = 1 |
      %index(&sysscpl, Sun)   = 1 |
      %index(&sysscpl, OSF1)  = 1 %then %do;
    /*--- Running some type of Unix. */
    %let OSNAME = UNIX;
  %end;
  %else %if %index(&sysscp, WIN) = 1 |
            %index(&sysscp, DNT) = 1 %then %do;
    /*--- Running a version of Windows. */
    %let OSNAME = WINDOWS;
  %end;
  %else %do;
    /*--- Operating system is not supported. */
    %put ERROR: TIGER2GEOCODE macro runs only on Windows or Unix.;
    %put ERROR: SYSSCPL=&sysscpl is not supported.;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- Was required TIGERPATH var specified? */
  %if ^%symexist(TIGERPATH) %then %do;
    %put ERROR: Use macro variable TIGERPATH to specify location;
    %put ERROR- where downloaded TIGER files were unzipped.;
    %let TIGERERROR = yes;
    %return;
  %end;
  /*--- Even though TIGERPATH was specified, is it empty? */
  %else %if %length(&TIGERPATH)=0 %then %do;
    %put ERROR: Macro variable TIGERPATH is empty. Use it to specify;
    %put ERROR- location where downloaded TIGER files were unzipped.;
    %let TIGERERROR = yes;
    %return;
  %end;
  /*--- TIGERPATH cannot contain a comma. It will break prxchange() below. */
  %else %if %sysfunc(indexc(&TIGERPATH, ',')) %then %do;
    %put ERROR: TIGERPATH location cannot contain a comma (',').;
    %put ERROR- Remove all commas from that directory path.;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- Make sure Windows path uses backslashes (C:\Temp) and Unix path
        uses forward slashes (/usr/data). Then check the last character in
        TIGERPATH. If not a slash, append correct slash because we later
        append file names to that path. */
  %if &OSNAME=UNIX %then %do;
    %let TIGERPATH = %sysfunc(prxchange(s/\\/\//, -1, &TIGERPATH));
    %if %qsubstr(&TIGERPATH,%length(&TIGERPATH),1)^=%str(/) %then
      %let TIGERPATH = %qtrim(&TIGERPATH)/;
  %end;
  %else %do;
    %let TIGERPATH = %sysfunc(prxchange(s/\//\\/, -1, &TIGERPATH));
    %if %qsubstr(&TIGERPATH,%length(&TIGERPATH),1)^=%str(\) %then
      %let TIGERPATH = %qtrim(&TIGERPATH)\;
  %end;

  /*--- If required TIGERPATH location does not exist, nothing to import. */
  %if %sysfunc(fileexist("&TIGERPATH"))=0 %then %do;
    %put ERROR: Location specified by macro variable TIGERPATH not found:;
    %put ERROR-   &TIGERPATH;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- Set TIGER libref to location where files were unzipped. */
  libname TIGER "&TIGERPATH";
  %if &syslibrc %then %do;
    %put ERROR: Cannot set libref TIGER to location specified;
    %put ERROR- by macro variable TIGERPATH:;
    %put ERROR-   &TIGERPATH;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- Clean out old data sets in TIGER library. */
  proc datasets nolist nowarn lib=tiger memtype=data; 
    delete _counties_to_import;
    delete _missing_place_files;
    delete _states_to_import;
  run;

  /*--- Was required DATASETPATH specified? */
  %if ^%symexist(DATASETPATH) %then %do;
    %put ERROR: Use macro variable DATASETPATH to specify location;
    %put ERROR- to write PROC GEOCODE lookup data sets.;
    %let TIGERERROR = yes;
    %return;
  %end;
  /*--- Even though DATASETPATH was specified, is it empty? */
  %else %if %length(&DATASETPATH)=0 %then %do;
    %put ERROR: Macro variable DATASETPATH is empty. Use it to specify;
    %put ERROR- location to write PROC GEOCODE lookup data sets.;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- If required DATASETPATH directory does not exist, create it.
        Set SAS options to close OS shell and return to SAS afterwards. */
  %if %sysfunc(fileexist("&DATASETPATH"))=0 %then %do;
    %if &OSNAME=WINDOWS %then %do;
      option noxwait xsync;
    %end;
    x "%str(mkdir %"&DATASETPATH%")";
    /*--- Make sure directory was actually created. */
    %if %sysfunc(fileexist("&DATASETPATH"))=0 %then %do;
      %put ERROR: DATASETPATH directory to write PROC GEOCODE lookup data;
      %put ERROR- sets could not be created:;
      %put ERROR-   &DATASETPATH;
      %put ERROR- Specify a writeable location with macro variable DATASETPATH.;
      %let TIGERERROR = yes;
      %return;
    %end;
  %end;

  /*--- If optional DATASETNAME var not specified, use default name prefix. */
  %if ^%symexist(DATASETNAME) %then %do;
    %global DATASETNAME;
    %let DATASETNAME = US;
    %put NOTE: Default data set name prefix "US" used for lookup data;
    %put NOTE- sets. An alternate prefix can be specified with macro;
    %put NOTE- variable DATASETNAME.;
  %end;

  /*--- If optional DATASETLIBNAME var not specified, use default libname. */
  %if ^%symexist(DATASETLIBNAME) %then %do;
    %global DATASETLIBNAME;
    %let DATASETLIBNAME = LOOKUP;
    %put NOTE: Default libname &DATASETLIBNAME assigned to PROC GEOCODE;
    %put NOTE- lookup data sets in DATASETPATH location. An alternate;
    %put NOTE- library name can be specified with macro variable DATASETLIBNAME.;
  %end;

  /*--- Set libref to DATASETPATH location. */
  libname &DATASETLIBNAME "&DATASETPATH";
  %if &syslibrc %then %do;
    %put ERROR: Cannot set libref &DATASETLIBNAME to location specified;
    %put ERROR- by macro varible DATASETPATH:;
    %put ERROR-   &DATASETPATH;
    %let TIGERERROR = yes;
    %return;
  %end;

  /*--- SASHELP.PLFIPS is used to convert state/place FIPS codes into
        text strings. */
  data plfips;
    set sashelp.plfips (rename=(name=city name2=city2)
                        keep=state name name2 place);
    len = length(city2);
  run;

  /*--- Adding LEN (length of CITY2) to the sort means that when a
        place has two names, we keep the shorter first one. 
        This keeps 'DOVER' and drops 'CITY OF DOVER'. */
  proc sort data=plfips out=plfips (drop=len);
    by state place len;
  run;

  data plfips (keep=state place city city2
               label='Converts FIPS codes to state and city names');
    set plfips;
    statePrv = lag1(state);
    placePrv = lag1(place);
    /*--- If this FIPS code is a dup of the prior place, drop it. */
    if place=placePrv & state=statePrv then do;
      return;
    end;
    city = propcase(city);
    output;
  run;

  /*--- Create table of zip codes using SASHELP.ZIPCODE for use in 
        ImportFaces() to fill in missing city/state names values 
        on street segments. */
  data zipcode;
    set sashelp.zipcode (keep=zip city city2 statecode);
    label ZIP   = 'ZIP Code'
          ZCTA  = 'ZIP Code Tabulation Area'
          city  = 'City name'
          city2 = 'City name (normalized)';
    format zcta z5.;
    zcta = zip;
  run;
%mend InitializeImport;

/*------------------------------------------------------------------------------*
 | GetDefaultVars: Set var names to import from the TIGER files.
 *------------------------------------------------------------------------------*/
%macro GetDefaultVars / des='Initialize var names for TIGER files';
  /*--- Set default var names and labels based on TIGER release.  */
  %if %eval(&TIGERYEAR < 2010) %then %do;
    /*--- TIGER files are earlier than 2010.                      */
    %let STATEVAR    = STATEFP;    /* Use current STATE FIPS var  */
    %let COUNTYVAR   = COUNTYFP;   /* Use current COUNTY FIPS var */
    %let TRACTVAR    = TRACTCE00;  /* Use Census TRACT var        */
    %let BLKGRPVAR   = BLKGRPCE00; /* Use Census BLOCK GROUP var  */
    %let BLOCKVAR    = BLOCKCE00;  /* Use Census BLOCK var        */
    %let PLACEVAR    = PLACEFP;    /* Use current PLACE var       */
    %let ZCTAVAR     = ZCTA5CE;    /* Use current ZCTA5 var       */
    %let TRACTVARLABEL  = Census 2000 Tract; 
    %let BLKGRPVARLABEL = Census 2000 Block Group;
    %let BLOCKVARLABEL  = Census 2000 Block; 
  %end;
  %else %if %eval(&TIGERYEAR = 2010) %then %do;
    /*--- TIGER files are 2010.                                       */
    %let STATEVAR    = STATEFP10;  /* Use Census 2010 STATE FIPS var  */
    %let COUNTYVAR   = COUNTYFP10; /* Use Census 2010 COUNTY FIPS var */
    %let TRACTVAR    = TRACTCE10;  /* Use Census 2010 TRACT var       */
    %let BLKGRPVAR   = BLKGRPCE10; /* Use Census 2010 BLOCK GROUP var */
    %let BLOCKVAR    = BLOCKCE10;  /* Use Census 2010 BLOCK var       */
    %let PLACEVAR    = PLACEFP10;  /* Use Census 2010 PLACE var       */
    %let ZCTAVAR     = ZCTA5CE10;  /* Use Census 2010 ZCTA5 var       */
    %let TRACTVARLABEL  = Census 2010 Tract; 
    %let BLKGRPVARLABEL = Census 2010 Block Group;
    %let BLOCKVARLABEL  = Census 2010 Block; 
  %end;
  %else %if %eval(&TIGERYEAR > 2019) %then %do;
    /*--- Need to investigate possible var name changes. */
    %put ERROR: TIGERYEAR > 2019. Census variable names may have;
    %put ERROR- to be changed for a new TIGER release.;
    %put ERROR- See var names in %GetDefaultVars macro.;
    %let TIGERERROR = yes;
  %end;
  %else %do;
    /*--- TIGER files are 2011-2019.                                  */
    %let STATEVAR    = STATEFP;    /* Use current STATE FIPS var      */
    %let COUNTYVAR   = COUNTYFP;   /* Use current COUNTY FIPS var     */
    %let TRACTVAR    = TRACTCE10;  /* Use Census 2010 TRACT var       */
    %let BLKGRPVAR   = BLKGRPCE10; /* Use Census 2010 BLOCK GROUP var */
    %let BLOCKVAR    = BLOCKCE10;  /* Use Census 2010 BLOCK var       */
    %let PLACEVAR    = PLACEFP;    /* Use current PLACE var           */
    %let ZCTAVAR     = ZCTA5CE10;  /* Use Census 2010 ZCTA5 var       */
    %let TRACTVARLABEL  = Census 2010 Tract; 
    %let BLKGRPVARLABEL = Census 2010 Block Group;
    %let BLOCKVARLABEL  = Census 2010 Block; 
  %end;
  /*--- These labels do not include the TIGER year. */
  %let COUNTYVARLABEL = County FIPS Code;
  %let ZCTAVARLABEL   = ZIP Code Tabulation Area; 
%mend GetDefaultVars;

/*------------------------------------------------------------------------------*
 | Name:    ImportTIGERFiles
 | Purpose: Calls macro programs to import individual TIGER files.
 | Input:   TIGER._counties_to_import - Data set listing all county TIGER files
 |                                   to be imported.
 | Output:  1) A separate data set for each county which contains the vars
 |             needed for street geocoding.
 |          2) DATASETLIBNAME._county_data_sets - Data set listing names of
 |             individual county data sets that were created.
 *------------------------------------------------------------------------------*/
%macro ImportTIGERFiles / des='Import TIGER edges/faces/featnames files';
  /*--- Get release year of the TIGER files for use in labels.
        It is put into the TIGERYEAR macro var. */
  %GetTIGERYEAR
  %if &TIGERERROR=yes %then %return;

  /*--- Initialize var names which can vary by TIGER release. */
  %GetDefaultVars
  %if &TIGERERROR=yes %then %return;

  /*--- Loop thru all TIGER files in TIGERPATH location.
        Note: This requires that the files listed in the input data set
              are present, sorted and each county has an EDGE.SHP, FACES.DBF
              and a FEATNAMES.DBF file. This was verified previously. */
  data &DATASETLIBNAME.._county_data_sets
       (label="Individual TIGER &TIGERYEAR county data sets created by TIGER2GEOCODE macro"
        keep=state stateid stctyfips tigerYear dataset tigerPath);
    set TIGER._counties_to_import end=finalObs;
    length tigerYear $ 4     /* Release year for TIGER files      */
           stateid   $ 2     /* 2-char state name abbreviation    */
           dataset   $ 32    /* Name of combined data set         */
           tigerPath $ 1000; /* Location of TIGER files to import */
    label tigerYear = 'TIGER Release'
          stateid   = 'State'
          dataset   = 'Data set of combined county Edge/Face/Featname TIGER files'
          tigerPath = 'Location of unzipped TIGER files';
    retain tigerYear tigerPath importedNum;

    /*--- Where are files to import and what release year are they? */
    if _n_=1 then do;
      tigerPath   = symget('TIGERPATH');
      importedNum = 0;
      %if %symexist(TIGERYEAR) %then %do;
        tigerYear = symget('TIGERYEAR');
      %end;
    end;

    /*--- Import TIGER edge, face and feature name files into data sets.
          Note: If other TIGER file types are added, they need to be
                processed in alphabetical order by name. */
    if index(filename, 'edges.shp') then
      call execute('%importEdges(' || trim(path) || ',' || filename || ')');
    else if index(filename, 'faces.dbf') then
      call execute('%importFaces(' || trim(path) || ',' || filename || ')');
    else if index(filename, 'featnames.dbf') then do;
      call execute('%importFeatnames(' || trim(path) || ',' || filename || ')');
      /*--- FEATNAMES.DBF is final TIGER file for this county. Combine the
            individual edge/faces/featnames data sets into one for the county. */
      stctyfips = scan(filename, 3, '_');
      stateid   = fipstate(substr(stctyfips, 1, 2));
      state     = stnamel(stateid);
      dataset   = 'county_' || stateid || '_' || stctyfips;
      label     = 'Combined edge/faces/featnames TIGER files for ' ||
                  stateid || ' ' || stctyfips;
      call execute('%CombineCountyData(' || dataset || ',' || label || ')');
      if symget('TIGERERROR')='yes' then stop;
      output;
      importedNum + 1;
    end;
    /*--- If no county files imported, get out. */
    if finalObs & importedNum=0 then do;
      call symput('TIGERERROR', 'yes');
      put 'ERROR: ImportTIGERFiles macro did not create any county data sets.';
    end;
  run;
%mend ImportTIGERFiles;

/*-------------------------------------------------------------------------*
 |  ImportEdges: Imports All Lines shapefiles (edges.shp).
 *-------------------------------------------------------------------------*/
%macro ImportEdges( path, filename ) / des='Import EDGES.SHP shapefiles';

  /*--- Import geocoding vars from 'all lines' shapefile. */
  proc mapimport out    = WORK.edges
                 infile = "&path.&filename";
       /*--- X, Y and SEGMENT vars imported by default.
             Specify other vars required for geocoding here. */
       select tlid tfidl tfidr roadflg mtfcc
              lfromadd ltoadd rfromadd rtoadd zipl zipr;
  run;

  /*--- Count number of points in each tlid segment which becomes the
        N var value in the M data set and output a file with only the
        coordinates which will become the P data set. */
  data WORK.edges_left  (keep=tlid zipl lfromadd ltoadd tfidl
                         rename=(zipl=zip lfromadd=fromadd ltoadd=toadd))
       WORK.edges_right (keep=tlid zipr rfromadd rtoadd tfidr
                         rename=(zipr=zip rfromadd=fromadd rtoadd=toadd))
       WORK.tlid_n      (keep=tlidPrv n
                         rename=(tlidPrv=tlid))
       WORK.xy          (keep=tlid x y);
    set WORK.edges end=finalObs;
    where roadflg          = 'Y' &  /* Keep only street segments */
          index(mtfcc,'S') = 1;     /* Do not keep waterways or boundaries */
    /*--- Keep only one obs per tlid. */
    retain tlidPrv N;
    /*--- Initialize at first obs. */
    if _n_=1 then do;
      N       =  -1;
      tlidPrv = tlid;
      output WORK.edges_left WORK.edges_right;
    end;
    n+1;
    /*--- At start of next tlid segment, output and reinitialize counter. */
    if tlid ^= tlidPrv then do;
      output WORK.edges_left WORK.edges_right WORK.tlid_n;
      N       = 0;
      tlidPrv = tlid;
    end;
    /*--- Final segment count must also be output. */
    if finalObs then do;
      N + 1;
      output WORK.tlid_n;
    end;
    /*--- Output coordinates for each obs. */
    output WORK.xy;
  run;

  proc sort data=WORK.tlid_n;
    by tlid;
  run;

%mend ImportEdges;

/*-------------------------------------------------------------------------*
 |  ImportFaces: Imports Polygonal Faces (faces.dbf) files.
 *-------------------------------------------------------------------------*/
%macro ImportFaces( path, filename ) / des='Import FACES.DBF files';

  /*--- Import topological faces relationship file. */
  proc mapimport datafile = "&path.&filename"
                 out      = WORK.faces;
  run;

  data WORK.faces2 (drop=&STATEVAR &PLACEVAR &ZCTAVAR);
    set WORK.faces (keep=tfid &STATEVAR &COUNTYVAR &TRACTVAR
                         &BLKGRPVAR &BLOCKVAR &PLACEVAR &ZCTAVAR);
    /*--- Convert FIPS code to 2-char state abbreviation. */
    length state $2;
    retain state;
    if _n_ = 1 then
      state = fipstate(&STATEVAR);
    /*--- Need numeric FIPS place and ZCTA values. */
    format place zcta z5.;
    place = input(&PLACEVAR, 5.0);
    zcta  = input(&ZCTAVAR,  5.0);
  run;

  proc sort data=WORK.faces2;
    by place;
  run;

  /*--- Merge with copy of SASHELP.PLFIPS from which duplicate place names
        were removed. This converts FIPS codes to state and city names.*/
  data WORK.faces2 (drop=place);
    merge WORK.faces2 (in=a) WORK.plfips;
    by state place;
    if a;
  run;

  /*--- Rearrange faces by TFID for attaching to the edges. */
  proc sort data=WORK.faces2;
    by tfid;
  run;

  /*--- Add left faces to edges. */
  proc sort data=WORK.edges_left;
    by tfidl;
  run;

  data WORK.edges_faces_left (drop=tfidl);
    merge WORK.edges_left (in=a)
          WORK.faces2     (rename=(tfid=tfidl));
    by tfidl;
    /*--- Drop obs without a county fips value. Those are streets on a county
          boundary and have obs in both county's TIGER files. One county file 
          has complete data with valid values. But the adjoining county file 
          has a duplicate obs with partially missing values. If the duplicate
          obs is not dropped, it causes the street's FIRST row linkage value
          to be greater than its LAST value which is invalid. */
    if a & ^missing(&COUNTYVAR);
  run;

  /*--- Add right faces to edges. */
  proc sort data=WORK.edges_right;
    by tfidr;
  run;

  data WORK.edges_faces_right (drop=tfidr);
    merge WORK.edges_right (in=a)
          WORK.faces2      (rename=(tfid=tfidr));
    by tfidr;
    /*--- Drop superfluous obs on county boundary line. */
    if a & ^missing(&COUNTYVAR);
  run;
%mend ImportFaces;

/*-------------------------------------------------------------------------*
 |  ImportFeatnames: Imports Feature Names relationships (featnames.dbf).
 *-------------------------------------------------------------------------*/
%macro ImportFeatnames( path, filename ) / des='Import FEATNAMES.DBF files';

  /*--- Import feature names file. */
  proc mapimport datafile = "&path.&filename"
                 out      = featnames;
  run;

  /*--- Remove unwanted feature segments and clean up the street names. */
  data featnames2 (drop=beg);
    set featnames (keep=tlid name predirabrv sufdirabrv prequalabr
                             pretypabrv suftypabrv paflag mtfcc);
    where mtfcc  ^= 'S1630' &   /* Drop freeway ramp/cloverleaf             */
          mtfcc  ^= 'S1710' &   /* Drop pedestrian trail/walkway            */
          mtfcc  ^= 'S1720' &   /* Drop pedestrian stairway                 */
          mtfcc  ^= 'S1750' &   /* Drop private drive                       */
          name   ^= '()'    &   /* Drop empty street names                  */
          ^missing(name)    &   /* Drop blank street names                  */
          index(mtfcc,'S') = 1; /* Keep streets, no waterways or boundaries */
    /*--- Keep the primary street name or any secondary name that is
          not just a route number. */
    if paflag='P' | anyalpha(name);
    /*--- Data entry error puts 'and and' into some names. Remove one of them. */
    beg = index(upcase(name), ' AND AND ');
    if beg then
      name = substr(name, 1, beg+4) || substr(name, 15);
    /*--- Most TIGER street type suffixes are abbreviations, e.g.
          St, Ave or Blvd. For some reason TIGER files also use 'Roadway' as
          a street type abbreviation. We add 'RDWY' to the GCTYPE data
          set, so any 'Roadway' types must be converted here to 'Rdwy'. */
    if suftypabrv='Roadway' then
      suftypabrv = 'Rdwy';
  run;

  /*--- Clean up street names having strings in parentheses. */
  data featnames3 (drop=pos beg end str rep nameOrig name1 i);
    set featnames2;
    length str $24 rep $12 nameOrig $100;
    /*--- If street is unnamed, do not output. */
    name1 = name;
    if index(upcase(name1), 'UNNAMED') then return;
    /*--- If name contains an open paren, examine the parenthetical string. */
    if indexc(name, '(') then do;
      /*--- Convert directional abbreviation to full length direction. */
      str='N'; rep='North'; link DirPreSuf; if pos then goto Next;
      str='W'; rep='West';  link DirPreSuf; if pos then goto Next;
      str='E'; rep='East';  link DirPreSuf; if pos then goto Next;
      str='S'; rep='South'; link DirPreSuf; if pos then goto Next;
      /*--- Montana Forest Development/Service Road (Fdr/Fs Rd). */
      str='Fdr';   link Montana; if pos then goto Next;
      str='Fs Rd'; link Montana; if pos then goto Next;
      /*--- Remove parens from various abbreviations. */
      str='Bus';    link RemoveParens; if pos then goto Next;
      str='Alt';    link RemoveParens; if pos then goto Next;
      str='Bypass'; link RemoveParens; if pos then goto Next;
      str='Link';   link RemoveParens; if pos then goto Next;
      str='Old';    link RemoveParens; if pos then goto Next;
      str='Spur';   link RemoveParens; if pos then goto Next;
      str='Nueva';  link RemoveParens; if pos then goto Next;
      str='Truck';  link RemoveParens; if pos then goto Next;
      str='Detour'; link RemoveParens; if pos then goto Next;
      str='Scenic'; link RemoveParens; if pos then goto Next;
      /*--- Convert specified strings to something simpler. */
      str='Bus Loop';      rep='Bus'; link ConvertText; if pos then goto Next;
      str='Alternate Rte'; rep='Alt'; link ConvertText; if pos then goto Next;
      /*--- Move type from within parens into prefix or suffix field. */
      str='State Hwy'; rep=''; link TypPreSuf; if pos then goto Next;
      /*--- If first two words designate a county road number (e.g. 'Cr 2013'),
            make two obs. First will be the street name enclosed by the parens. 
            Second will be the county road number using the proper
            abbreviation from SASHELP.GCTYPE. */
      if scan(name, 1) = 'Cr' & ^anyalpha(scan(name, 2)) then do;
        beg = indexc(name, '('); 
        end = indexc(name, ')');
        if beg & end then do;  
          nameOrig = name;
          /*--- Grab string enclosed by parens. If it contains a street type,
                it is likely an alternate street name so we output it. */
          name = substr(nameOrig, beg+1, end-beg-1);
          str='Av';   link CheckCr; if pos then goto Next;
          str='Rd';   link CheckCr; if pos then goto Next;
          str='Ln';   link CheckCr; if pos then goto Next;
          str='Dr';   link CheckCr; if pos then goto Next;
          str='St';   link CheckCr; if pos then goto Next;
          str='Tr';   link CheckCr; if pos then goto Next;
          str='Ave';  link CheckCr; if pos then goto Next;
          str='Cir';  link CheckCr; if pos then goto Next;
          str='Blvd'; link CheckCr; if pos then goto Next;
          /*--- Could not decipher what was in the parens. We know 
                'Cr' is the first word and is followed by a numeric word. 
                Drop the 'Cr' from the road number, grab the highway number  
                for the street name, and put 'Co Rd' into the PreTypAbrv field.
                Then blow off everything within the parens. */
          name       = scan(nameOrig, 2);
          PreTypAbrv = 'Co Rd';
          SufTypAbrv = '';
          PreDirAbrv = '';
          SufDirAbrv = '';
          goto Next;
        end;
      end;
      /*--- If first two words designate a federal route number (e.g. 'Fas 344'), 
            drop that designation. Use the street name enclosed by the parens.
            See if second word is a numeric. */
      if ^anyalpha(scan(name, 2)) then do;
        name1 = scan(name, 1);
        if upcase(name1) in('FAP', 'FAS', 'FCR') then do;
          beg = indexc(name, '(');
          end = indexc(name, ')');
          if beg & end then do;
            /*--- Grab string enclosed by parens. */
            name = substr(name, beg+1, end-beg-1);
            goto Next;
          end;
        end;
      end;
      /*--- Does NAME have an open paren but no closing paren? */
      pos = indexc(name, '(');
      if pos & ^indexc(name, ')') then do;
        if pos = 1 then do;
          /*--- Open paren is first char. Drop it and keep remainder. */
          name = substr(name, 2);
          goto Next;
        end;
        /*--- Open paren is beyond the first char. Get rid of the
              open paren and everything after it. */
        name = substr(name, 1, pos-1);
        goto Next;
      end;  /* NAME has open but no closing paren */
      /*--- Is '(Aka ' in the name? */
      pos = index(name, '(Aka ');
      if pos = 1 then do;
        /*--- '(Aka ' is at front of name. Is there a closing paren? */
        pos = index(name, ')');
        if pos then do;
          /*--- Remove leading '(Aka ' and closing paren. */
          name = substr(name, 6, pos-6);
          goto Next;
        end;
        else do;
          /*--- No closing paren. Just remove leading '(Aka '. */
          name = substr(name, 6);
          goto Next;
        end;
      end;
      else if pos>1 then do;
        /*--- '(Aka ' not at front of street name. Remove it and all
              chars after that opening paren. */
        name = substr(name, 1, pos-1);
        goto Next;
      end;
      /*--- Finally, discard all remaining parenthecized chars. */
      nameOrig = name;
      name     = substr(nameOrig, 1, indexc(nameOrig, '(')) || 
                 substr(nameOrig, indexc(nameOrig, ')'));
      name     = translate(trim(name), '  ', '()');
    end;  /* NAME has an open parenthesis */
/*--- Clean up new name, write it out and increment to next obs. */
Next:
    name = compbl(trim(left(name)));
    output;
    return;
/*--- Some Florida streets have a County Road number followed by the
      street name inside parentheses. Output two obs, one for each. */
CheckCr:
    /*--- If target street type not in this NAME, no need to go farther. */
    if ^indexw(name, str) then return;
    array dir[4] $1 _temporary_ ('N' 'E' 'W' 'S');
    do i=1 to 4;
      pos = indexw(name, dir[i]);
      if pos = length(name) then leave;
    end;
    if pos & i< 4 then do;
      /*--- Last char within the parens is a direction suffix. 
            Move it from street name to the SufDirAbrv field. */
      SufDirAbrv = dir[i];
      name       = substr(name,1,pos-1);
    end;
    /*--- If the target street type is found in the text enclosed by parens, 
          put the type into the SufTypAbrv field and output the street name
          taken from between the parens. */
    pos = indexw(name, str);
    /*--- Make sure the type string is at the end of the name. We do not
          want to misinterpret the 'Dr' in 'Dr. ML King Blvd' as drive. */
    if pos = length(name)-length(str)+1 then do;
      SufTypAbrv = str;
      PreTypAbrv = '';
      SufTypAbrv = '';
      SufDirAbrv = '';
      name       = substr(name, 1, pos-1);
      output;
      /*--- We know 'Cr' is the first word and is followed by a numeric word. 
            Drop the 'Cr' from the road number, grab the highway number for 
            the street name, and put 'Co Rd' into the PreTypAbrv field. */
      name       = scan(nameOrig, 2);
      PreTypAbrv = 'Co Rd';
      SufTypAbrv = '';
    end;
    else 
      pos = 0;
    return;
/*--- Convert a string to something simpler. */
ConvertText:
    pos = index(name, '(' || trim(str) || ')');
    if pos then
      name = substr(name, 1, pos-1) || ' ' || rep;
    return;
/*--- Process Montana forest road abbreviation. */
Montana:
    pos = indexw(left(name), '(' || trim(str) || ')');
    if ^pos then return;
    if pos = 1 & missing(PreTypAbrv) then do;
      PreTypAbrv = upcase(str);
      name       = transtrn(name, '(' || trim(str) || ')', '');
    end;
    else
      name = transtrn(name, '(' || trim(str) || ')', ' ' || trim(rep) || ' ');
    return;
/*--- Strip parentheses and keep enclosed text. */
RemoveParens:
    pos=index(name, '(' || trim(str) || ')');
    if pos then
      name = translate(trim(name), '  ', '()');
    return;
/*--- Move street type from within parens into prefix or suffix field. */
TypPreSuf:
    pos = index(left(name), '(' || trim(str) || ')');
    if pos then do;
      if pos = 1 & missing(PreDirAbrv) then do;
        PreDirAbrv = str;
        name       = transtrn(name, '(' || trim(str) || ')', '');
      end;
      else if pos > 1 & missing(SufDirAbrv) then do;
        SufDirAbrv = str;
        name       = transtrn(name, '(' || trim(str) || ')', '');
      end;
      else
        name = transtrn(name, '(' || trim(str) || ')', ' ' || trim(rep) || ' ');
    end;
    return;
/*--- Convert directional abbreviation (N) to full length direction (North). */
DirPreSuf:
    pos = index(left(name), '(' || trim(str) || ')');
    if pos then do;
      if pos = 1 & missing(PreDirAbrv) then do;
        PreDirAbrv = str;
        name       = transtrn(name, '(' || trim(str) || ')', '');
      end;
      else if pos > 1 & missing(SufDirAbrv) then do;
        SufDirAbrv = str;
        name       = transtrn(name, '(' || trim(str) || ')', '');
      end;
      else
        name = transtrn(name, '(' || trim(str) || ')', ' ' || trim(rep) || ' ');
    end;
    return;
  run;

  /*--- Street names in TIGER data are suppose to have the direction
        prefix in the PreDirAbrv var and the direction suffix in SufDirAbrv,
        but sometimes it is erroneously included in the NAME var, e.g. 'NE Maynard',
        'Edinburg South' or '1910 N'. Since PROC GEOCODE will look
        for directions in PreDirAbrv/DufDirAbrv, move them if necessary. */
  data featnames4 (drop=i word) ;
    set featnames3;
    /*--- Direction abbreviations and full length names. Need to use only U.S.
          directions as TIGER data is U.S. only. */
    array DirAbrv[8] $ 2 _temporary_
      ('N' 'E' 'S' 'W' 'NE' 'SE' 'SW' 'NW');
    array DirName[8] $ 9 _temporary_
      ('NORTH' 'EAST' 'SOUTH' 'WEST' 'NORTHEAST' 'SOUTHEAST' 'SOUTHWEST' 'NORTHWEST');

    /*--- Check first word for possible direction prefix. */
    if countw(name)>1 & missing(PreDirAbrv) then do;
      word = upcase(scan(name, 1));
      /*--- If first word is a direction abbreviation, move to PreDirAbrv. */
      do i=1 to dim(DirAbrv);
        if word = DirAbrv[i] then do;
          PreDirAbrv = DirAbrv[i];
          name       = substr(name, length(DirAbrv[i])+2);
          leave;
        end;
      end;
      /*--- If first word is full length direction, move abbreviation to PreDirAbrv. */
      if missing(PreDirAbrv) then do i=1 to dim(DirAbrv);
        if word = DirName[i] then do;
          PreDirAbrv = DirAbrv[i];
          name       = substr(name, length(DirName[i])+2);
          leave;
        end;
      end;
    end;

    /*--- Check last word for possible direction suffix. */
    if countw(name)>1 & missing(SufDirAbrv) then do;
      /*--- Get last word in street name. */
      word = upcase(scan(name, -1));
      /*--- If last word is a direction abbreviation, move to SufDirAbrv. */
      do i=1 to dim(DirAbrv);
        if word = DirAbrv[i] then do;
          SufDirAbrv = DirAbrv[i];
          name       = substr(name, 1, length(name)-length(DirAbrv[i])-1);
          leave;
        end;
      end;
      /*--- If last word is a full length direction, move abbreviation to SufDirAbrv. */
      if missing(SufDirAbrv) then do i=1 to dim(DirAbrv);
        if word = DirName[i] then do;
          SufDirAbrv = DirAbrv[i];
          name       = substr(name, 1, length(name)-length(DirName[i])-1);
          leave;
        end;
      end;
    end;

    /*--- Check first char for possible direction prefix abbreviation. */
    if countw(name)=1 & missing(predirabrv) then do;
      len = length(name)-1;
      if len>1 & ^notdigit(trim(name), 2) then do;
        /*--- All chars past first char are digits. */
        firstChar = substr(name, 1, 1);
        do i=1 to dim(DirAbrv)/2;
          if firstChar = DirAbrv[i] then do;
            name       = substr(trim(name), 2);
            PreDirAbrv = DirAbrv[i];
            leave;
          end;
        end;
      end;
    end;

    /*--- Check last char for direction suffix abbreviation. */
    if countw(name)=1 & missing(sufdirabrv) then do;
      len = length(name)-1;
      if len>1 then do;
        word = substr(name, 1, len);
        if ^notdigit(trim(word)) then do;
          /*--- All chars up to next-to-last char are digits. */
          lastChar = substr(name, len+1);
          do i=1 to dim(DirAbrv)/2;
            if lastChar = DirAbrv[i] then do;
              name       = word;
              SufDirAbrv = DirAbrv[i];
              leave;
            end;
          end;
        end;
      end;
    end;

    /*--- Check first/last chars for possible direction prefix/suffix abbreviations. */
    if countw(name)=1 & missing(predirabrv) & missing(sufdirabrv) then do;
      len = length(name)-2;
      if len>1 then do;
        word = substr(name, 2, len);
        if ^notdigit(trim(word)) then do;
          /*--- All chars between first and last chars are digits. 
                See if first char is a directio prefix. */
          firstChar = substr(name, 1, 1);
          lastChar  = substr(name, len+2, 1);
          do i=1 to dim(DirAbrv)/2;
            if firstChar = DirAbrv[i] then do;
              firstDir = DirAbrv[i];
              leave;
            end;
          end;
          /*--- If first char was a direction, check the last char, too. */
          if ^missing(firstDir) then do i=1 to dim(DirAbrv)/2;
            if lastChar = DirAbrv[i] then do;
              /*--- Both first and last chars were directions. */
              SufDirAbrv = DirAbrv[i];
              PreDirAbrv = firstDir;
              name       = word;
              leave;
            end;
          end;
        end;
      end;
    end;
  run;

  /*--- Scrub featnames for specific problems encountered in TIGER data. */
  data featnames_final (drop=len word pos);
    set featnames4;
    predirabrv = upcase(predirabrv);
    /*--- Does street name have more than one word? */
    if countw(name)>1 then do;
      /*--- Some feature names contain non-address strings, e.g. 'I-40 EB'.
            Need to check the last word and drop it if necessary. */
      call scan(name, -1, pos, len);
      word = upcase(substrn(name, pos, len));
      if word in('WB', 'EB', 'NB', 'SB') then
        name = substr(name, 1, length(name)-len-1);
      /*--- Does address have a direction prefix? */
      else if ^missing(predirabrv) then do;
        /*--- Some streets have the direction prefix split across vars. 
              For example SE Deringer Loop, Prineville, OR 97754 had the 'SE'
              split as in predirabrv='S' & name='E Deringer'. Look for instances
              where NE, NW, SE or SW are split and move the first word in the
              NAME var into the PREDIRABRV var. */
        word = upcase(scan(name, 1));
        if predirabrv='N' then do;
          if word='E' then do;
            predirabrv = 'NE';
            name       = substr(name, 3);
          end;
          else if word='W' then do;
            predirabrv = 'NW';
            name       = substr(name, 3);
          end;
        end;
        if predirabrv='S' then do;
          if word='E' then do;
            predirabrv = 'SE';
            name       = substr(name, 3);
          end;
          else if word='W' then do;
            predirabrv = 'SW';
            name       = substr(name, 3);
          end;
        end;
        /*--- Get current first word of street name. */
        word = upcase(scan(name, 1));
        if predirabrv=word then
        /*--- First word of street name is same as direction prefix.
              For example, the TIGER file for Peoria County, IL contained
              direction prefix 'SW' and name 'SW Water' which turned into
              'SW SW Water'. Strip that redundant first word. */
        name = substr(name, length(word)+2);
      end;  /* Street has a direction prefix  */
    end;    /* Street name has multiple words */
    /*--- Make sure moving parts about did not leve any leading/trailing blanks. */
    name = trim(left(name));
  run;

  proc sort data=featnames_final;
    by tlid;
  run;

  /*--- Add street names and ZIPs to left side street segments. */
  proc sort data=edges_faces_left;
    by tlid;
  run;

  data edges_faces_featnames_left (drop=zip_char);
    merge edges_faces_left (in=a rename=(zip=zip_char))
          featnames_final;
    by tlid;
    if a;
    /*--- If no street name, it is useless for street method geocoding. */
    if missing(name) then 
      delete;
    /*--- We need either a zip, zcta or city name for street geocoding. */
    if ^missing(zip_char) | ^missing(zcta) | ^missing(city2);
    /*--- Convert ZIP to numeric. */
    format zip z5.;
    zip = input(zip_char, 5.0);
    /*--- Special case for Hawaii Interstate Highways. */
    if state = 'HI' & PreTypAbrv='I-' & index(name, 'H-') then
      name = substr(name, 3 );
  run; 

  /*--- Use ZIP/ZCTA to fill in missing CITY names. This changes the
        meaning of the CITY var. In the original TIGER data, CITY is
        filled in for street segments only if street is inside the city limits.
        CITY values for segments outside are missing. But everyone has
        a city mailing address even if outside of a city proper.
        So we use the ZIP/ZCTA values in SASHELP.ZIPCODE to assign CITY
        values to those streets which are outside of any city limits. */
  proc sort data=edges_faces_featnames_left;
    by zip;
  run;

  /*--- Get city name for the ZIP code. */
  data edges_faces_featnames_left2;
    merge edges_faces_featnames_left (in=a) 
          zipcode (drop=zcta rename=(city=city_from_zip city2=city2_from_zip
                                     statecode=statecode_from_zip));
    by zip;
    if a;
  run;

  /*--- Now get city name for the ZCTA value. */
  proc sort data=edges_faces_featnames_left2;
    by zcta;
  run;

  /*--- Use the city name from the ZIP/ZCTA value to fill in missing left side CITY name strings. */
  data edges_faces_featnames_left3 (drop=city_from_zip city2_from_zip city_from_zcta city2_from_zcta statecode_from_zcta)
       alternates_left             (drop=city_from_zip city2_from_zip city_from_zcta city2_from_zcta statecode_from_zcta);
    merge edges_faces_featnames_left2 (in=a) 
          zipcode (drop=zip rename=(city=city_from_zcta city2=city2_from_zcta
                                    statecode=statecode_from_zcta));
    by zcta;
    if a;
    /*--- Set missing CITY value with ZIP/ZCTA only if city names from ZIP and ZCTA agree
          and if potential city is in the current state. */
    if missing(city) then do;
      /*--- If CITY values from ZIP and ZCTA are same and both are in same
            STATE, assign that CITY value. */
      if city2_from_zip = city2_from_zcta & state = statecode_from_zcta then do;
        city  = city_from_zcta;
        city2 = city2_from_zcta;
      end;
      /*-- If ZIP has a CITY but ZCTA does not, use ZIP CITY value. */
      else if ^missing(city2_from_zip) & missing(city2_from_zcta) &
              state = statecode_from_zip then do;
        city  = city_from_zip;
        city2 = city2_from_zip;
      end;
      /*--- If ZIP has no CITY but ZCTA does, use ZCTA CITY value. */
      else if missing(city2_from_zip) & ^missing(city2_from_zcta) &
              state = statecode_from_zcta then do;
        city  = city_from_zcta;
        city2 = city2_from_zcta;
      end;
    end;
    side = 'L';
    if paflag='P' then output edges_faces_featnames_left3;
    else               output alternates_left;
  run; 

  /*--- Now do same for the right side of the street. */
  proc sort data=edges_faces_right;
    by tlid;
  run;

  data edges_faces_featnames_right (drop=zip_char);
    merge edges_faces_right (in=a rename=(zip=zip_char))
          featnames_final;
    by tlid;
    if a;
    /*--- If no street name, it is useless for street method geocoding. */
    if missing(name) then 
      delete;
    /*--- We need either a zip, zcta or city name for street geocoding. */
    if ^missing(zip_char) | ^missing(zcta) | ^missing(city2);
    /*--- Convert ZIP to numeric. */
    format zip z5.;
    zip = input(zip_char, 5.0);
    /*--- Special case for Hawaii Interstate Highways. */
    if state = 'HI' & PreTypAbrv='I-' & index(name, 'H-') then
      name = substr(name, 3 );
  run; 

  /*--- Get city name for the ZIP code. */
  proc sort data=edges_faces_featnames_right;
    by zip;
  run;

  data edges_faces_featnames_right2;
    merge edges_faces_featnames_right (in=a) 
          zipcode (drop=zcta rename=(city=city_from_zip city2=city2_from_zip
                                          statecode=statecode_from_zip));
    by zip;
    if a;
  run;

  /*--- Now get city name for the ZCTA value. */
  proc sort data=edges_faces_featnames_right2;
    by zcta;
  run;

  /*--- Use the city name from the ZIP/ZCTA value to fill in missing right side CITY name strings. */
  data edges_faces_featnames_right3 (drop=city_from_zip city2_from_zip city_from_zcta city2_from_zcta statecode_from_zcta)
       alternates_right             (drop=city_from_zip city2_from_zip city_from_zcta city2_from_zcta statecode_from_zcta);
    merge edges_faces_featnames_right2 (in=a) 
          zipcode (drop=zip rename=(city=city_from_zcta city2=city2_from_zcta
                                    statecode=statecode_from_zcta));
    by zcta;
    if a;
    /*--- Set missing CITY value with ZIP/ZCTA only if city names from ZIP and ZCTA agree
          and if potential city is in the current state. */
    if missing(city) then do;
      /*--- If CITY values from ZIP and ZCTA are same and both are in same
            STATE, assign that CITY value. */
      if city2_from_zip = city2_from_zcta & state = statecode_from_zcta then do;
        city  = city_from_zcta;
        city2 = city2_from_zcta;
      end;
      /*-- If ZIP has a CITY but ZCTA does not, use ZIP CITY value. */
      else if ^missing(city2_from_zip) & missing(city2_from_zcta) &
              state = statecode_from_zip then do;
        city  = city_from_zip;
        city2 = city2_from_zip;
      end;
      /*--- If ZIP has no CITY but ZCTA does, use ZCTA CITY value. */
      else if missing(city2_from_zip) & ^missing(city2_from_zcta) &
              state = statecode_from_zcta then do;
        city  = city_from_zcta;
        city2 = city2_from_zcta;
      end;
    end;
    side = 'R';
    if paflag='P' then output edges_faces_featnames_right3;
    else               output alternates_right;
  run; 

  /*--- Clean street names and convert char vars to numerics. */
  %CleanData( edges_faces_featnames_left3,  featnames_primary_left    )
  %CleanData( alternates_left,              featnames_alternate_left  )
  %CleanData( edges_faces_featnames_right3, featnames_primary_right   )
  %CleanData( alternates_right,             featnames_alternate_right )
%mend ImportFeatnames;



/*-------------------------------------------------------------------------*
 |  Name:     CleanData
 |  Purpose:  Scrub street names, house numbers, etc. 
 |            Also convert several vars from char to numeric.
 *-------------------------------------------------------------------------*/
%macro CleanData(dsin, dsout);

  /*--- List of non-numeric characters to be stripped from house numbers.
        These are usually apartment or suite tags.
        Note: The first character in the TRASH list is a blank space. */
  %let TRASH=%nrstr( #-/ABCDEFGHIJKLMONPQRSTUVWXYZ);
  /*--- Set largest 32-bit long variable value allowed to compare with house numbers. */
  %let MACLONG=2147483647;

  data &dsout (drop=blkgrp_char block_char tract_char county_char
                    fromadd_char toadd_char pos);
    set &dsin (rename=(&BLKGRPVAR=blkgrp_char &BLOCKVAR=block_char
                       &TRACTVAR=tract_char &COUNTYVAR=county_char
                       fromadd=fromadd_char toadd=toadd_char));

    /*--- Clean up house number strings and convert to numerics. */
    if ^missing(fromadd_char) then do;
      pos = notdigit(fromadd_char);
      /*--- If first character in house number string is not numeric, strip
            non-digits off front of it. For example, convert 'BL25' to '25'. */
      if pos=1 then
        fromadd_char = compress(fromadd_char, "&TRASH");
      /*--- But if house number string contains non-numeric characters beyond
            the first position, truncate everything past that position and
            keep the leading numerics. For example, convert '1234#5' to '1234'.*/
      else
        fromadd_char = substr(fromadd_char, 1, pos-1);
    end;
    /*--- Convert house number string to numeric var. */
    fromadd = input(fromadd_char, best.);

    /*--- Now do the same for the to-house number string. */
    if ^missing(toadd_char) then do;
      pos = notdigit(toadd_char);
      if pos=1 then
        toadd_char = compress(toadd_char, "&TRASH");
      else
        toadd_char = substr(toadd_char, 1, pos-1);
    end;
    toadd = input(toadd_char, best.);

    /*--- If end of street house number range value is huge, it is not a normal
          house number. just set it to missing. */
    if fromadd >= &MACLONG then fromadd = .;
    if toadd   >= &MACLONG then toadd   = .;

    /*--- Some streets have zero as an end-of-range house number. 
          Change those to a 1 or 2 depending on whether that side
          of the street is even or odd. */
    if fromadd=0 & toadd>0 then do;      /* Beginning number is zero */
    if mod(toadd, 2) then fromadd=1;     /* Ending number is even    */
    else                  fromadd=2;     /* Rnding number is odd     */
    end;
    else if fromadd>0 & toadd=0 then do; /* Ending number is zero    */
      if mod(fromadd, 2) then toadd=1;   /* Beginning number is odd  */
      else                    toadd=2;   /* Beginning number is even */
    end;

    /*--- Convert other character vars to numeric vars. */
    &BLKGRPVAR = input(blkgrp_char, 4.0);
    &BLOCKVAR  = input(block_char,  4.0);
    &TRACTVAR  = input(tract_char,  6.0);
    &COUNTYVAR = input(county_char, 3.0);

    /*--- Upcase street name for where-clause use, strip extraneous
          characters that break the where-clause and remove blanks. */
    name2 = upcase(compress(name, &CLEANCHARS));

    /*--- If NAME2 missing, NAME contains only garbage characters. */
    if missing( name2 ) then
      delete;
  run;
%mend CleanData;


/*-------------------------------------------------------------------------*
 |  CombineCountyData: Combines imported edge, face and feature name
 |                     data sets into a single data set for the county.
 *-------------------------------------------------------------------------*/
%macro CombineCountyData(dataset, label);

  /*--- Write final data set for this county's imported and cleaned data.
        The CreateStreetGeocodeData() macro combines all of these individual
        county data sets into the actual street-level lookup data sets. */
  data streets_all;
    set featnames_primary_left
        featnames_alternate_left 
        featnames_primary_right
        featnames_alternate_right;
        format city city2 state;
  run;

  /*--- Sort for merging the point count var (N) back in. */
  proc sort data = streets_all
            out  = &dataset;
    by tlid;
  run;

  /*--- Add the point count var (N) to the data. */
  data &dataset (label="&label");
    merge &dataset (in=a) tlid_n;
    by tlid;
    if a;
  run;

  /*--- Put this county's coordinates in the overall coordinate data set. */
  proc append data = xy
              out  = all_xy;
  run;
%mend CombineCountyData;


/*-------------------------------------------------------------------------*
 |  Name:     CreateStreetGeocodeData
 |  Purpose:  Combine all the county data sets created by the
 |            CombineCountyData() macro and create the three lookup data
 |            sets for PROC GEOCODE's street level geocoding method.
 |  Output:   Three lookup data sets. If DATASETNAME var specified, that
 |            value is used as the data set name prefix. The character 'M',
 |            'S' and 'P' will be appended to that prefix to create the
 |            complete data set name. If DATASETNAME not specified, the
 |            default prefix is 'US' and the data sets are USM, USS and USP.
 *-------------------------------------------------------------------------*/
%macro CreateStreetGeocodeData;

  /*--- Put names of the county data sets that were imported into sequential
        macro vars, e.g. &COUNTY1, &COUNTY2, ... &COUNTYn. The number of
        macro vars containing county data set names is in &COUNTYTOTAL. */
  data _null_;
    set &DATASETLIBNAME.._county_data_sets end=finalObs;
    retain countytotal 0;
    countytotal + 1;
    call symput('COUNTY' || left(put(countytotal, 8.)), 'WORK.' || trim(dataset));
    if finalObs then
      call symputx('COUNTYTOTAL', countytotal, 'g');
  run;

  /*--- Combine all individual county data sets into one data set. */
  data all_counties (label="Combined edge/face/featname data for &COUNTYTOTAL counties");
    /*--- Loop thru county data set names in macro vars. */
    set %do i=1 %to &COUNTYTOTAL; &&county&i %end; ;
  run;

  /*--- Replace invalid ZIP codes with ZCTA values. */
  %CheckZIPs( all_counties )

  /*--- Sort by geocoding lookup vars. 
        Note: We re-sort the MDS again below. If by-vars change here, check below, too. */
  proc sort data=all_counties;
    by name2 zip state city2 tlid side;
  run;

  /*--- Generate the lookup data sets MDS/SDS with row linkages.
        Also write the LASTOBS data set which will be merged back in, and 
        determine max lengths for street prefix/suffix strings. */
  data &DATASETNAME.M (keep=first name name2 state city city2 zip zcta)
       &DATASETNAME.S (keep=side n fromadd toadd &TRACTVAR &BLKGRPVAR 
                            &BLOCKVAR &COUNTYVAR mtfcc predirabrv sufdirabrv 
                            pretypabrv suftypabrv tlid Sorder)
       lastObs        (keep=last);
    set all_counties end=finalObs;

   /*--- Track max string lengths. */
    retain predirabrvMax sufdirabrvMax pretypabrvMax suftypabrvMax
           first last Sorder;

    name2Prv      = lag1(name2);
    zipPrv        = lag1(zip);
    city2Prv      = lag1(city2);
    statePrv      = lag1(state);
    prequalabrPrv = lag1(prequalabr);
    sidePrv       = lag1(side);
    tlidPrv       = lag1(tlid);

    /*--- Initialize and output first MDS/SDS obs. */
    if _n_=1 then do;
      Sorder        = 1;
      first         = 1;
      last          = 1;
      predirabrvMax = length(predirabrv); /* Initialize street */
      sufdirabrvMax = length(sufdirAbrv); /* prefix/suffix     */
      pretypabrvMax = length(pretypabrv); /* string lengths    */
      suftypabrvMax = length(suftypabrv);
      output &DATASETNAME.M
             &DATASETNAME.S;
      Sorder + 1;
      return;
    end;

    /*--- If a new street/zip/city/state, write it to MDS. */
    if name2      ^= name2Prv |
       zip        ^= zipPrv   |
       city2      ^= city2Prv |
       state      ^= statePrv |
       prequalabr ^= prequalabrPrv then do;
      if tlid ^= tlidPrv | side ^= sidePrv then
        first = last + 1;
      if ^missing(prequalabr) then do;
        name  = trim(left(prequalabr)) || ' ' || left(name);
        name2 = upcase(compress(prequalabr)) || left(name2);
      end;
      output &DATASETNAME.M
             lastObs;
    end;

    /*--- If another segment of same street, add an obs to SDS. */
    if tlid ^= tlidPrv |
       side ^= sidePrv then do;
      output &DATASETNAME.S;
      last   + 1;
      Sorder + 1;
      /*--- Check max lengths of character var strings. */
      predirabrvMax = max(predirabrvMax, length(predirabrv));
      sufdirabrvMax = max(sufdirabrvMax, length(sufdirAbrv));
      pretypabrvMax = max(pretypabrvMax, length(pretypabrv));
      suftypabrvMax = max(suftypabrvMax, length(suftypabrv));
    end;

    /*--- Append final row linkage number and put var lengths in macros. */
    if finalObs then do;
      output WORK.lastObs;
      call symputx('PREDIRABRVMAX', predirabrvMax, 'g');
      call symputx('SUFDIRABRVMAX', sufdirabrvMax, 'g');
      call symputx('PRETYPABRVMAX', pretypabrvMax, 'g');
      call symputx('SUFTYPABRVMAX', suftypabrvMax, 'g');
    end;
  run;

  /*--- Add the var LAST to MDS. FIRST and LAST are the two vars
        that link to rows in the SDS. Also do some error checking. */
  data &DATASETNAME.M (drop=error);
    merge &DATASETNAME.M lastObs end=finalObs;
    retain error 0;

    /*--- LAST row link value cannot be less than the FIRST row link. */
    if last<first then do;
      put "ERROR: &DATASETLIBNAME..&DATASETNAME.M_BAD_LINKS has incorrect FIRST/LAST row links:";
      put "ERROR- OBS   = " _n_;
      put "ERROR- FIRST = " first;
      put "ERROR- LAST  = " last;
      error+1;
    end;

    /*--- If any invalid row links, stop. */
    if error>9 | (finalObs & error) then do;
      call symput('TIGERERROR', 'yes');
      stop;
    end;
  run;

  /*--- If data set has bad row links, stop now. */
  %if &TIGERERROR=yes %then %do;
    data &DATASETLIBNAME..&DATASETNAME.M_BAD_LINKS (label='Intermediate result with incorrect FIRST/LAST row links.');
      set &DATASETNAME.M1;
    run;
    %return;
  %end;

  /*--- Check MDS for street name with numeric elements: 1st Street, Fifth Avenue, Ten-Ten Highway. 
        If detected, keep the original obs but also add a new MDS obs using the alternate name so
        either will be found by the geocoder.
        Examples:  1ST & FIRST
                   5TH & FIFTH
                   TEN-TEN & 1010
        We also determine the maximum lengths of the name/name2 strings so we can
        reduce those var sizes to the minimum lengths necessary.

        Also check for varants of 'Martin Luther King' and add extra obs to cover
        the most common variations. */

  /*--- Set dimensions of the numeric name arrays used below. */
  %let LONGDIM=118;  /* longNum[] and longWord[] array size   */
  %let SHORTDIM=27;  /* shortNum[] and shortWord[] array size */
  %let LONG2DIM=88;  /* long2Word[] array size                */

  data &DATASETNAME.M (drop=i modShort modLong nameUpcase nameMax name2Max name2Orig);
    set &DATASETNAME.M end=finalObs;

    /*--- Initialize two arrays of equivalent long numeric names, e.g. 1ST and FIRST.
          If new elements added, do so in pairs and adjust the LONGDIM value above. */
    array longNum[&LONGDIM] $ 5 _temporary_ 
      ('1ST'  '2ND'  '2D'  '3RD'  '3D'  '4TH'  '5TH'  '6TH'  '7TH'  '8TH'  '9TH'  '10TH'
       '11TH' '12TH'       '13TH'       '14TH' '15TH' '16TH' '17TH' '18TH' '19TH' '20TH' 
       '21ST' '22ND' '22D' '23RD' '23D' '24TH' '25TH' '26TH' '27TH' '28TH' '29TH' '30TH'
       '31ST' '32ND' '32D' '33RD' '33D' '34TH' '35TH' '36TH' '37TH' '38TH' '39TH' '40TH'
       '41ST' '42ND' '42D' '43RD' '43D' '44TH' '45TH' '46TH' '47TH' '48TH' '49TH' '50TH'
       '51ST' '52ND' '52D' '53RD' '53D' '54TH' '55TH' '56TH' '57TH' '58TH' '59TH' '60TH'
       '61ST' '62ND' '62D' '63RD' '63D' '64TH' '65TH' '66TH' '67TH' '68TH' '69TH' '70TH'
       '71ST' '72ND' '72D' '73RD' '73D' '74TH' '75TH' '76TH' '77TH' '78TH' '79TH' '80TH'
       '81ST' '82ND' '82D' '83RD' '83D' '84TH' '85TH' '86TH' '87TH' '88TH' '89TH' '90TH'
       '91ST' '92ND' '92D' '93RD' '93D' '94TH' '95TH' '96TH' '97TH' '98TH' '99TH' '100TH');
    array longWord[&LONGDIM] $ 15 _temporary_ 
      ('FIRST'         'SECOND'         'SECOND'          'THIRD'          'THIRD'         'FOURTH'
       'FIFTH'         'SIXTH'          'SEVENTH'         'EIGHTH'         'NINTH'         'TENTH'
       'ELEVENTH'      'TWELFTH'                          'THIRTEENTH'                     'FOURTEENTH' 
       'FIFTEENTH'     'SIXTEENTH'      'SEVENTEENTH'     'EIGHTEENTH'     'NINETEENTH'    'TWENTIETH' 
       'TWENTY-FIRST'  'TWENTY-SECOND'  'TWENTY-SECOND'   'TWENTY-THIRD'   'TWENTY-THIRD'  'TWENTY-FOURTH'
       'TWENTY-FIFTH'  'TWENTY-SIXTH'   'TWENTY-SEVENTH'  'TWENTY-EIGHTH'  'TWENTY-NINTH'  'THIRTIETH'
       'THIRTY-FIRST'  'THIRTY-SECOND'  'THIRTY-SECOND'   'THIRTY-THIRD'   'THIRTY-THIRD'  'THIRTY-FOURTH'
       'THIRTY-FIFTH'  'THIRTY-SIXTH'   'THIRTY-SEVENTH'  'THIRTY-EIGHTH'  'THIRTY-NINTH'  'FORTIETH'
       'FORTY-FIRST'   'FORTY-SECOND'   'FORTY-SECOND'    'FORTY-THIRD'    'FORTY-THIRD'   'FORTY-FOURTH' 
       'FORTY-FIFTH'   'FORTY-SIXTH'    'FORTY-SEVENTH'   'FORTY-EIGHTH'   'FORTY-NINTH'   'FIFTIETH' 
       'FIFTY-FIRST'   'FIFTY-SECOND'   'FIFTY-SECOND'    'FIFTY-THIRD'    'FIFTY-THIRD'   'FIFTY-FOURTH'
       'FIFTY-FIFTH'   'FIFTY-SIXTH'    'FIFTY-SEVENTH'   'FIFTY-EIGHTH'   'FIFTY-NINTH'   'SIXTIETH'
       'SIXTY-FIRST'   'SIXTY-SECOND'   'SIXTY-SECOND'    'SIXTY-THIRD'    'SIXTY-THIRD'   'SIXTY-FOURTH'
       'SIXTY-FIFTH'   'SIXTY-SIXTH'    'SIXTY-SEVENTH'   'SIXTY-EIGHTH'   'SIXTY-NINTH'   'SEVENTIETH'
       'SEVENTY-FIRST' 'SEVENTY-SECOND' 'SEVENTY-SECOND'  'SEVENTY-THIRD'  'SEVENTY-THIRD' 'SEVENTY-FOURTH'
       'SEVENTY-FIFTH' 'SEVENTY-SIXTH'  'SEVENTY-SEVENTH' 'SEVENTY-EIGHTH' 'SEVENTY-NINTH' 'EIGHTIETH'
       'EIGHTY-FIRST'  'EIGHTY-SECOND'  'EIGHTY-SECOND'   'EIGHTY-THIRD'   'EIGHTY-THIRD'  'EIGHTY-FOURTH'
       'EIGHTY-FIFTH'  'EIGHTY-SIXTH'   'EIGHTY-SEVENTH'  'EIGHTY-EIGHTH'  'EIGHTY-NINTH'  'NINETIETH'
       'NINETY-FIRST'  'NINETY-SECOND'  'NINETY-SECOND'   'NINETY-THIRD'   'NINETY-THIRD'  'NINETY-FOURTH'
       'NINETY-FIFTH'  'NINETY-SIXTH'   'NINETY-SEVENTH'  'NINETY-EIGHTH'  'NINETY-NINTH'  'HUNDRETH');

    /*--- Initialize two arrays of equivalent short numeric names, e.g. 9 and NINE. 
          If new elements added, do so in pairs and adjust the SHORTDIM value above. */
    array shortNum[&SHORTDIM] $ 2 _temporary_ 
      ( '1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9' '10'
       '11' '12' '13' '14' '15' '16' '17' '18' '19' '20'
       '30' '40' '50' '60' '70' '80' '90');
    array shortWord[&SHORTDIM] $ 10 _temporary_
      ('ONE' 'TWO' 'THREE' 'FOUR' 'FIVE' 'SIX' 'SEVEN' 'EIGHT' 'NINE' 'TEN'
       'ELEVEN' 'TWELVE' 'THIRTEEN' 'FOURTEEN' 'FIFTEEN' 'SIXTEEN' 'SEVENTEEN' 'EIGHTEEN' 'NINETEEN' 'TWENTY'
       'THIRTY' 'FORTY' 'FIFTY' 'SIXTY' 'SEVENTY' 'EIGHTY' 'NINETY');

    /*--- Initialize array of equivalent long numeric names having two words, e.g. SIXTY FIRST.
          If new elements added, the LONG2DIM value above. */
    array long2Word[&LONG2DIM] $ 15 _temporary_ 
      ('TWENTY FIRST'  'TWENTY SECOND'  'TWENTY SECOND'   'TWENTY THIRD'   'TWENTY THIRD'  'TWENTY FOURTH'
       'TWENTY FIFTH'  'TWENTY SIXTH'   'TWENTY SEVENTH'  'TWENTY EIGHTH'  'TWENTY NINTH' 
       'THIRTY FIRST'  'THIRTY SECOND'  'THIRTY SECOND'   'THIRTY THIRD'   'THIRTY THIRD'  'THIRTY FOURTH'
       'THIRTY FIFTH'  'THIRTY SIXTH'   'THIRTY SEVENTH'  'THIRTY EIGHTH'  'THIRTY NINTH' 
       'FORTY FIRST'   'FORTY SECOND'   'FORTY SECOND'    'FORTY THIRD'    'FORTY THIRD'   'FORTY FOURTH' 
       'FORTY FIFTH'   'FORTY SIXTH'    'FORTY SEVENTH'   'FORTY EIGHTH'   'FORTY NINTH' 
       'FIFTY FIRST'   'FIFTY SECOND'   'FIFTY SECOND'    'FIFTY THIRD'    'FIFTY THIRD'   'FIFTY FOURTH'
       'FIFTY FIFTH'   'FIFTY SIXTH'    'FIFTY SEVENTH'   'FIFTY EIGHTH'   'FIFTY NINTH' 
       'SIXTY FIRST'   'SIXTY SECOND'   'SIXTY SECOND'    'SIXTY THIRD'    'SIXTY THIRD'   'SIXTY FOURTH'
       'SIXTY FIFTH'   'SIXTY SIXTH'    'SIXTY SEVENTH'   'SIXTY EIGHTH'   'SIXTY NINTH' 
       'SEVENTY FIRST' 'SEVENTY SECOND' 'SEVENTY SECOND'  'SEVENTY THIRD'  'SEVENTY THIRD' 'SEVENTY FOURTH'
       'SEVENTY FIFTH' 'SEVENTY SIXTH'  'SEVENTY SEVENTH' 'SEVENTY EIGHTH' 'SEVENTY NINTH' 
       'EIGHTY FIRST'  'EIGHTY SECOND'  'EIGHTY SECOND'   'EIGHTY THIRD'   'EIGHTY THIRD'  'EIGHTY FOURTH'
       'EIGHTY FIFTH'  'EIGHTY SIXTH'   'EIGHTY SEVENTH'  'EIGHTY EIGHTH'  'EIGHTY NINTH' 
       'NINETY FIRST'  'NINETY SECOND'  'NINETY SECOND'   'NINETY THIRD'   'NINETY THIRD'  'NINETY FOURTH'
       'NINETY FIFTH'  'NINETY SIXTH'   'NINETY SEVENTH'  'NINETY EIGHTH'  'NINETY NINTH');

    /*--- Track max lengths of street names. */
    retain nameMax name2Max -999;

    /*--- No need to upcase street name multiple times. */
    nameUpcase = upcase(name);

    /*--- Loop thru two-word numeric name array. */
    do i=1 to &LONG2DIM;
      /*--- If 'Twenty Second' is in street name, insert hyphen to make it 'Twenty-Second'.
            If left in two words we'd wind up with '20 2nd' as a street name. */
      if index(nameUpcase, translate(long2Word[i], long2word[i], nameUpcase)) then do;
        name       = translate(propcase(trim(name)), '-', ' ');
        nameUpcase = upcase(name);
        leave;
      end;
    end;

    /*--- Write out original MDS record as-is and check string lengths and look
          for '&' and 'and' in name. */
    output;
    link CheckAnds;

    /*--- Track if this observation gets changed. */
    modShort = modLong = 0;

    /*--- Loop thru short numeric name arrays. */
    do i=1 to &SHORTDIM;
      /*--- If 'THREE' is in street name, change it to '3'.
            We do not look for the converse, i.e. do not replace '3' with 'THREE'. */
      if indexw(nameUpcase, shortWord[i]) then do;
        name     = tranwrd(name, propcase(trim(shortWord[i])), propcase(trim(shortNum[i])));
        name2    = tranwrd(name2, trim(shortWord[i]), trim(shortNum[i]));
        modShort = 1;
      end;
    end;

    /*--- Loop thru long numeric name arrays. */
    do i=1 to &LONGDIM;
      /*--- If '7th' detected, replace with 'Seventh'. */
      if indexw(nameUpcase, longNum[i]) then do;
        name       = tranwrd(name, propcase(trim(longNum[i])), propcase(trim(longWord[i])));
        name2      = tranwrd(name2, trim(longNum[i]), compress(trim(longWord[i]),' -'));
        nameUpcase = upcase(name);
        modLong    = 1;
      end;
      if modLong then do; link CheckAnds; output; leave; end;

      /*--- If 'Twenty-Second' is in street name, change to '22nd'. */
      if indexw(nameUpcase, longWord[i]) then do;
        name       = tranwrd(name, propcase(trim(longWord[i])), propcase(trim(longNum[i])));
        name2      = tranwrd(name2, trim(longWord[i]), trim(longNum[i]));
        nameUpcase = upcase(name);
        modLong    = 1;
      end;
      if modLong then do; link CheckAnds; output; leave; end;

      /*--- If 'TwentySecond' is in street name, change to '22nd'. */
      if indexw(nameUpcase, compress(longWord[i],' -')) then do;
        name       = tranwrd(name, propcase(compress(trim(longWord[i]),' -')), propcase(trim(longNum[i])));
        name2      = tranwrd(name2, compress(trim(longWord[i]),' -'), trim(longNum[i]));
        nameUpcase = upcase(name);
        modLong    = 1;
      end;
      if modLong then do; link CheckAnds; output; leave; end;
    end; /* do i=1 to &LONGDIM; */

    if modShort then do;
      output;
      nameUpcase = upcase(name);
      link CheckAnds; 
    end;

    /*--- If street name includes 'Saint', output an extra obs to
          cover 'St'. Original name was output at top. */
    if findw(nameUpcase, 'SAINT') then do;
      name  = tranwrd(name,  'Saint', 'St');
      name2 = tranwrd(name2, 'SAINT', 'ST');
      output;
      link CheckAnds; 
    end;

    /*--- If street name has 'St' as word, and it is not the last word,
          output an extra obs to cover 'Saint'. If 'ST' is the last word
          it is likely 'street'. Original name was output at top. */
    if findw(nameUpcase, 'ST') & scan(nameUpcase, -1) ^= 'ST' then do;
      name  = tranwrd(name,  'St', 'Saint');
      name2 = tranwrd(name2, 'ST', 'SAINT');
      output;
      link CheckAnds; 
    end; 

    /*--- If street name includes 'Mount', output an extra obs to
          cover 'Mt'. Original name was output at top. */
    if findw(nameUpcase, 'MOUNT') then do;
      name  = tranwrd(name,  'Mount', 'Mt');
      name2 = tranwrd(name2, 'MOUNT', 'MT');
      output;
      link CheckAnds; 
    end;

    /*--- If street name includes 'Mt', output an extra obs to
          cover 'Mount'. Original name was output at top. */
    if findw(nameUpcase, 'MT') then do;
      name  = tranwrd(name,  'Mt', 'Mount');
      name2 = tranwrd(name2, 'MT', 'MOUNT');
      output;
      link CheckAnds; 
    end;

    /*--- If street name includes 'Fort' and that is not the entire
          street name, output an extra obs to cover 'Ft'. 
          Original name was output at top. */
    if findw(nameUpcase, 'FORT') and nameUpcase^='FORT' then do;
      name  = tranwrd(name,  'Fort', 'Ft');
      name2 = tranwrd(name2, 'FORT', 'FT');
      output;
      link CheckAnds; 
    end;

    /*--- If street name includes 'Ft', output an extra obs to 
          cover 'Fort'. Original name was output at top. */
    if findw(nameUpcase, 'FT') then do;
      name  = tranwrd(name,  'Ft', 'Fort');
      name2 = tranwrd(name2, 'FT', 'FORT');
      output;
      link CheckAnds; 
    end;

    /*--- If street name includes 'University' and that is not the entire
          street name, output an extra obs to cover 'Univ'. 
          Original name was output at top. */
    if findw(nameUpcase, 'UNIVERSITY') and nameUpcase^='UNIVERSITY' then do;
      name  = tranwrd(name,  'University', 'Univ');
      name2 = tranwrd(name2, 'UNIVERSITY', 'UNIV');
      output;
      link CheckAnds; 
    end;

    /*--- If street name includes 'Univ', output an extra obs to 
          cover 'University'. Original name was output at top. */
    if findw(nameUpcase, 'UNIV') then do;
      name  = tranwrd(name,  'Univ', 'University');
      name2 = tranwrd(name2, 'UNIV', 'UNIVERSITY');
      output;
      link CheckAnds; 
    end;

    /*--- If street name is a form of 'Martin Luther King', output extra 
          obs to cover its most common variants. */
    if findw(nameUpcase, 'MARTIN') & findw(nameUpcase, 'KING') then do;
      /*--- Original name was output at top, but save that name. */
      name2Orig = name2;
      /*--- Try most common variants. If not the original name, output to MDS. */
      name2 = 'DRMARTINLUTHERKING';
      if name2 ^= name2Orig then do;
        name = 'Dr Martin Luther King';
        output;
        link CheckAnds; 
      end;
      name2 = 'DRMARTINLUTHERKINGJR';
      if name2 ^= name2Orig then do;
        name = 'Dr Martin Luther King Jr';
        output;
        link CheckAnds; 
      end;
      name2 = 'MARTINLUTHERKING';
      if name2 ^= name2Orig then do;
        name = 'Martin Luther King';
        output;
        link CheckAnds; 
      end;
      name2 = 'MARTINLUTHERKINGJR';
      if name2 ^= name2Orig then do;
        name = 'Martin Luther King Jr';
        output;
        link CheckAnds; 
      end;
      name2 = 'MARTINLKING';
      if name2 ^= name2Orig then do;
        name = 'Martin L King';
        output;
        link CheckAnds; 
      end;
      name2 = 'MARTINLKINGJR';
      if name2 ^= name2Orig then do;
        name = 'Martin L King Jr';
        output;
        link CheckAnds; 
      end;
    end;
    return; /* Loop to top of data step */

CheckAnds:
    /*--- Check for new max length in obs just written out. */
    link CheckLengths;

    /*--- If street name includes 'and' and that is not the entire
          street name, output an extra obs to cover '&'. */
    if findw(nameUpcase, 'AND') and nameUpcase^='AND' then do;
      name       = tranwrd(name, 'and', '&');
      name2      = upcase(compress(name, &CLEANCHARS));
      nameUpcase = upcase(name);
      output;
      /*--- No need to check max lengths. '&' is shorter than 'and'. */
    end;

    /*--- If name includes '&', output extra obs to cover 'and'. */
    else if findw(nameUpcase, '&') then do;
      name       = tranwrd(name,  '&', 'and');
      name2      = tranwrd(name2, '&', 'AND');
      nameUpcase = upcase(name);
      output;
      /*--- Check for new max length in obs just written out. */
      link CheckLengths;
    end;
    return; /* CheckAnds */

CheckLengths:
    /*--- Track max lengths of street, city and state names. 
          At final observation, put values into macro vars. */
    nameMax  = max(nameMax,  length(name));
    name2Max = max(name2Max, length(name2));
    if finalObs then do;
      call symputx('NAMEMAX',  nameMax,  'g');
      call symputx('NAME2MAX', name2Max, 'g');
    end;
    return; /* CheckLengths */
  run;

  /*--- Create PDS. */
  data &DATASETLIBNAME..&DATASETNAME.P (keep=x y)
       tlid_start                      (keep=tlid start);
    set all_xy;
    tlidPrv = lag1(tlid);
    if tlid ^= tlidPrv then do;
      start = _N_;
      output tlid_start;
    end;
    output &DATASETLIBNAME..&DATASETNAME.P;

  run;

  proc sort data=&DATASETNAME.S;
    by tlid;
  run;

  proc sort data=tlid_start;
    by tlid;
  run;

  /*--- Merge START values into SDS. */
  data &DATASETNAME.S;
    merge &DATASETNAME.S (in=a) tlid_start;
    by tlid;
    if a;
    label start="First obs in &DATASETNAME.P data set";
  run;

  /*--- ImportEdges() wrote the XY data set with all of the points for both 
        sides of each street. Each county's XY points were accumulated in ALL_XY. 
        TLID_START was then created from ALL_XY. While processing the streets,
        some sides were dropped as they lacked sufficient information for geocoding.
        However, the coordinates for those sides are still in TLID_START. The above
        merge inserts duplicate observations into the SDS. These inserted obs
        break the row links from the MDS (FIRST and LAST). Therefore to restore
        the linkages, we must delete duplicate obs. We will leave the extra coordinates
        for the deleted street sides in the PDS. Removing them would require redoing
        the row link from the SDS (START), and that would be quite ugly. */
  data &DATASETNAME.S (drop=tlidPrv SorderPrv);
    set &DATASETNAME.S;
    tlidPrv   = lag1(tlid);
    SorderPrv = lag1(Sorder);
    if tlid=tlidPrv & Sorder=SorderPrv then delete;
  run;

  /*--- Put SDS back into original order. */
  proc sort data=&DATASETNAME.S
            out=&DATASETNAME.S (drop=Sorder);
    by Sorder;
  run;

  /*--- Sort MDS to add extra obs if the city name is a CDP. */
  proc sort data=&DATASETNAME.M out=MDS;
    by state city2;
  run;

  /*--- Read CDP-city linkages created earlier. Change var names so we can
        differentiate them after the merge. */
  data CDP_city (rename=(cdp=city cdp2=city2 StateForCDP=state));
    set &DATASETLIBNAME..CDP_city (keep=dist CDP city StateForCity StateForCDP
                                   rename=(city=newcity stateforcity=newstate));
    /*--- Make clean copy of CDP value. */
   cdp2 = upcase(compress(cdp, &CLEANCHARS));
  run;

  proc sort data=CDP_city out=CDP_city_sorted;
    by state city2;
  run;

  /*--- Merge CDPs with MDS obs. */
  data MDS2;
    merge MDS (in=a) CDP_city_sorted;
    by state city2;
    if a;
  run;

  /*--- Where an MDS obs has a CDP for city name, add an obs using the city. */
  data &DATASETNAME.M (drop=newCity newState dist cityMax city2Max);
    set MDS2 end=finalObs;
    retain cityMax &CITYMAX city2Max &city2Max;
    /*--- Write out the current street as-is. */
    output;
    link CheckLengths;
    /*--- If current obs uses a CDP for city name, write out a duplicate
          obs using the name of the nearest incorporated city. */
    if ^missing(newCity) then do;
      city  = newCity;
      state = newState;
      city2 = upcase(compress(city, &CLEANCHARS));
      output;
      link CheckLengths;
    end;
    if finalObs then do;
      call symputx( 'CITYMAX',  cityMax,  'g');
      call symputx( 'CITY2MAX', city2Max, 'g');
    end;
    return;
CheckLengths:
    cityMax  = max(cityMax,  length(city));
    city2Max = max(city2Max, length(city2));
    return;
  run;

  /*--- Shorten street name/prefix/suffix vars to minimum required. Turn off log message 
        warning of possible data truncation. We know the new lengths are sufficient. */
  options varlenchk=nowarn;
  data &DATASETLIBNAME..&DATASETNAME.M;  /* Shorten street name vars     */
    length name  $ &NAMEMAX              /* Max NAME string length       */
           name2 $ &NAME2MAX             /* Max NAME2 string length      */
           city  $ &CITYMAX              /* Max CITY string length       */
           city2 $ &CITY2MAX;            /* Max CITY2 string length      */
    set &DATASETNAME.M;
  run; 
  data &DATASETLIBNAME..&DATASETNAME.S;  /* Shorten prefix/suffix vars   */
    length predirabrv $ &PREDIRABRVMAX   /* Max PREDIRABRV string length */
           sufdirabrv $ &SUFDIRABRVMAX   /* Max SUFDIRABRV string length */
           pretypabrv $ &PRETYPABRVMAX   /* Max PRETYPABRV string length */
           suftypabrv $ &SUFTYPABRVMAX;  /* Max SUFTYPABRV string length */
    set &DATASETNAME.S;
  run;
  options varlenchk=warn;
%mend CreateStreetGeocodeData;

/*-------------------------------------------------------------------------*
 |  Name:     CheckZIPS
 |  Purpose:  Look for any invalid ZIP codes in the TIGER files. If found,
 |            they are replaced by the ZCTA value.
 |  Note:     The ZIP value is replaced by ZCTA only if:
 |              1) ZIP code from TIGER file is invalid (not in 
 |                 SASHELP.ZIPCODE or is missing),
 |              2) TIGER file ZCTA is valid (it is in SASHELP.ZIPCODE), and
 |              3) The CITY and STATE from the TIGER file are the same CITY
 |                 and STATE for the valid ZCTA value in SASHELP.ZIPCODE.
 *-------------------------------------------------------------------------*/
%macro CheckZIPS( ds );
  /*--- Get unique ZIP values from MDS. */
  proc freq data=&ds noprint;
    table zip / out=zip_freqs;
  run;

  proc sort data=zip_freqs;
    by zip;
  run;

  /*--- Find any TIGER ZIPs that are not in SASHELP.ZIPCODE. */
  data invalid_zips (keep=zip invalid_zip);
    merge zip_freqs (in=a keep=zip) 
          sashelp.zipcode (keep=zip city statecode);
    by zip;
    /*--- If statecode not assigned, ZIP is not valid. */
    if a & missing(statecode);
    invalid_zip = 'y';
  run;

  proc sort data=&ds out=zip_sorted;
    by zip;
  run;

  /*--- Tag the bad ZIPs in the MDS. */
  data mds_invalid_zips;
    merge zip_sorted (in=a) invalid_zips;
    by zip;
    if a;
  run;

  /*--- Get unique ZCTA values from MDS. */
  proc freq data=&ds noprint;
    table zcta / out=zcta_freqs;
  run;

  proc sort data=zcta_freqs;
    by zcta;
  run;

  /*--- Find ZCTAs from MDS that are in SASHELP.ZIPCODE. */
  data valid_zctas (keep=zcta valid_zcta city2Alt statecodeAlt);
    merge zcta_freqs (in=a keep=zcta)
          sashelp.zipcode (keep=zip city2 statecode 
                           rename=(zip=zcta city2=city2Alt statecode=statecodeAlt));
    by zcta;
    /*--- If statecodeAlt not assigned, ZCTA is not valid. */
    if a & ^missing(statecodeAlt);
    valid_zcta = 'y';
  run;

  proc sort data=valid_zctas;
    by zcta;
  run;

  proc sort data=mds_invalid_zips;
    by zcta;
  run;

  /*--- Tag the good ZCTAs in the MDS and then replace any invalid ZIPs. */
  data &ds;
    merge mds_invalid_zips (in=a) valid_zctas;
    by zcta;
    if a;
    /*--- Replace ZIP with the ZCTA value under these conditions. */
    if invalid_zip = 'y'      &          /* MDS ZIP value is bad  */
       valid_zcta  = 'y'      &          /* MDS ZCTA is good      */
       city2       = city2Alt &          /* City names match      */  
       state       = statecodeAlt then   /* States also match     */
      zip = zcta;
  run;
%mend CheckZIPs;

/*-------------------------------------------------------------------------*
 |  Name:     FinalizeDataSets
 |  Purpose:  Apply final sorting, indexing and labels.
 |  Output:   Final lookup data sets.
*-------------------------------------------------------------------------*/
%macro FinalizeDataSets;

  /*--- Re-sort MDS by geocoding lookup vars because we added obs and modified ZIPs. 
        Note: We sorted earlier. If by-vars change here, may need to do so there, too. */
  proc sort data=&DATASETLIBNAME..&DATASETNAME.M;
    by name2 zip state city2;
  run;

  /*--- Force the MDS vars to be in a specific order. */
  data &DATASETLIBNAME..&DATASETNAME.M;
    label  Name  = "Street name"
           Name2 = "Street name (normalized)"
           City  = "City name"
           City2 = "City name (normalized)"
           State = "State abbreviation"
           ZIP   = "ZIP Code"
           ZCTA  = "&ZCTAVARLABEL"
           First = "First obs in &DATASETNAME.S data set"
           Last  = "Last obs in &DATASETNAME.S data set";
    set &DATASETLIBNAME..&DATASETNAME.M;
    /*--- Trap bad FIRST/LAST values. */
    if missing(first) then do;
      put "ERROR: Missing FIRST value in data set &DATASETLIBNAME..&DATASETNAME.M.";
      put 'ERROR- See observation ' _n_ '.';
      call symput('TIGERERROR', 'yes');
      output;
      stop;
    end;
    if missing(last) then do;
      put "ERROR: Missing LAST value in data set &DATASETLIBNAME..&DATASETNAME.M.";
      put 'ERROR- See observation ' _n_ '.';
      call symput('TIGERERROR', 'yes');
      output;
      stop;
    end;
    output;
  run;

  /*--- Create composite indexes of MDS vars used in street method
        where-clauses, label lookup data sets, and set cases for final var names. */
  proc datasets lib=&DATASETLIBNAME;
    modify &DATASETNAME.M (label="Primary street lookup data for PROC GEOCODE (TIGER &TIGERYEAR.)");
      rename state = MapIDNameAbrv;
  run;
    modify &DATASETNAME.S (label="Secondary street lookup data for PROC GEOCODE (TIGER &TIGERYEAR.)");
      format   predirabrv sufdirabrv pretypabrv suftypabrv;
      informat predirabrv sufdirabrv pretypabrv suftypabrv;
      rename   predirabrv  = PreDirAbrv
               sufdirabrv  = SufDirAbrv
               pretypabrv  = PreTypAbrv
               suftypabrv  = SufTypAbrv
               side        = Side
               fromadd     = FromAdd
               toadd       = ToAdd
               &BLKGRPVAR  = BlkGrp
               &BLOCKVAR   = Block
               &TRACTVAR   = Tract
               &COUNTYVAR  = CountyFp
               start       = Start;
      label    Side        = "Side of street"
               N           = "Number of obs in &DATASETNAME.P data set"
               FromAdd     = "Beginning house number"
               ToAdd       = "Ending house number"
               Tract       = "&TRACTVARLABEL"
               BlkGrp      = "&BLKGRPVARLABEL"
               Block       = "&BLOCKVARLABEL"
               CountyFp    = "&COUNTYVARLABEL"
               PreDirAbrv  = "Street direction prefix"
               SufDirAbrv  = "Street direction suffix"
               PreTypAbrv  = "Street type prefix"
               SufTypAbrv  = "Street type suffix"
               MTFCC       = "MAF/TIGER Feature Class Code"
               TLID        = "TIGER/Line ID";
  run;
    modify &DATASETNAME.P (label="Tertiary street lookup data for PROC GEOCODE (TIGER &TIGERYEAR.)");
    label      X           = 'Longitude (degrees)'
               Y           = 'Latitude (degrees)';
  run;
    modify &DATASETNAME.M;
      index create Name2_Zip                 = (Name2 ZIP);                 /* street+zip search        */
      index create Name2_Zcta                = (Name2 ZCTA);                /* street+zcta search       */
      index create Name2_MapIDNameAbrv_City2 = (Name2 MapIDNameAbrv City2); /* street+city+state search */
  run;
quit;
%mend FinalizeDataSets;

/*------------------------------------------------------------------------------*
 | Name:    ImportPLACEFiles
 | Purpose: Calls macro programs to import individual TIGER files.
 | Input:   TIGER._states_to_import - Data set listing all county TIGER files
 |                                    to be imported.
 | Output:  1) A separate data set for each county which contains the vars
 |             needed for street geocoding.
 |          2) DATASETLIBNAME._county_data_sets - Data set listing names of
 |             individual county data sets that were created.
 *------------------------------------------------------------------------------*/

%macro ImportPlaceFiles / des='Import TIGER PLACE files';
  /*--- Loop thru all PLACE files in TIGERPATH location. */
  data &DATASETLIBNAME.._state_data_sets
       (label="State PLACE data sets for &TIGERYEAR TIGER files created by TIGER2GEOCODE macro");
    set TIGER._states_to_import end=finalObs;
    retain importedNum 0 year;
    if _n_ = 1 then
      year = scan(filename, 2, '_');
    put year=;
    /*--- Import one PLACE file. */
    call execute('%ImportPlaceFile(' || trim(path) || ',' 
                                     || filename   || ','
                                     || year       || ')');
    /*--- Count files imported. */
    importedNum + 1;
    /*--- If no state PLACE files imported, get out. */
    if finalObs & importedNum=0 then do;
      call symput('TIGERERROR', 'yes');
      put 'ERROR: ImportPlaceFiles macro did not create any state data sets.';
    end;
  run;
  %if &TIGERERROR=yes %then %return;

  /*--- Create data set CDPs and their nearest city/town. */
  %LinkCDPandCity
%mend ImportPLACEFiles;

/*--------------------------------------------------------------------*
 | ImportPlaceFile: Imports PLACE file for one state and appends it
 |                  to accumulated PLACE data set.
 *--------------------------------------------------------------------*/
%macro ImportPlaceFile( path, filename, year );
  /*--- Import current state PLACE file. */
  proc mapimport datafile="&path.&filename" out=place;
  run;

  /*--- Keep only one obs per polygon. 2010 PLACE files had unique var
        names and other TIGER years used common names. */
  data place1 (drop=placefpPrv
               keep=intptlat intptlon mtfcc name placefp statefp);
    set place;
    /*--- Convert 2010 var names to what rest of code expects. */
    if &YEAR = 2010 then do;
      intptlat = intptlat10;
      intptlon = intptlon10;
      mtfcc    = mtfcc10;
      name     = name10;
      placefp  = placefp10; 
      statefp  = statefp10;
    end;
    /*--- Keep only one obs per polygon. */
    placefpPrv  = lag1(placefp);
    if placefp ^= placefpPrv;
  run;

  /*--- Append this state to the data set of all states. */
  proc append data=place1 base=place_all;
  run;
%mend ImportPlaceFile;

/*--------------------------------------------------------------------*
 | LinkCDPandCity: Create data set listing each CDP and its nearest
 |                 incorporated city or town.
 *--------------------------------------------------------------------*/
%macro LinkCDPandCity;
  /*--- Put CDPs and incoporated places into separate data sets. */
  data place_city (label='Census places that are incorporated cities/towns'
                   drop=mtfcc
                   rename=(intptlat=cityLat
                           intptlon=cityLong
                           name=cityName
                           placefp=cityPlacefp
                           statefp=cityStatefp))
       place_cdp (label='Census Designated Places (CDPs)'
                  drop=mtfcc
                  rename=(intptlat=cdpLat
                          intptlon=cdpLong
                          name=cdpName
                          placefp=cdpPlacefp
                          statefp=cdpStatefp));
  set place_all;
  /*--- Separate CDPs and incorporated cities/towns. */
  select(mtfcc);
    when ('G4110') output place_city;
    when ('G4210') output place_cdp;
    otherwise do;
      put 'ERROR: Unknown MTFCC of ' mtfcc ' in obs ' _n_
           ' of WORK.PLACE_ALL data set.';
      put 'ERROR- MTFCC should be G4110 (incorporated place) or G4210 (CDP).';
      call symput('TIGERERROR', 'yes');
      stop;
    end;
  end;
  run;

  /*--- Create data set linking each CDP to its nearest incoporated place. */
  data cdp_city (keep=minDist CDP StateForCDP City StateForCity 
                      cdpLat cdpLong cityLat cityLong
                 rename=(minDist=Dist));
    label CDP          = 'Census Designated Place'
          StateForCDP  = 'CDP State' 
          City         = 'Incorporated Place (city/town)'
          StateForCity = "City's State"
          minDist      = 'CDP-City Distance (miles)'
          CDPLat       = 'CDP Latitude'
          CDPLong      = 'CDP Longitude'
          CityLat      = 'City Latitude'
          CityLong     = 'City Longitude';
    length StateForCDP StateForCity $2;
    /*--- Read each CDP. */
    set place_cdp end=finalObs;
    retain minDist 9999 cityMax cdpMax 0 CDP StateForCDP City StateForCity;
    format minDist 12.2;
    /*--- No incorporated cities in Hawaii. Only CDPs. Nearest cities are
          in California which are not reasonable for a link. */
    if cdpStateFp = '15' then do;
      stateForCDP = 'HI';
      goto OutputObs;
    end;
    /*--- For each CDP, compute distance to each city/town. 
          Yes, this is brute force, but it's not necessary very often. */
    do i=1 to numObs;
      set place_city point=i nobs=numObs;
      curDist = geodist(cdpLat, cdpLong, cityLat, cityLong, 'DM');
      /*--- If closer than previous best, remember it. */
      if curDist < minDist then do;
        minDist      = curDist;
        CDP          = cdpName;
        StateForCDP  = fipstate(cdpStatefp);
        City         = cityName;
        StateForCity = fipstate(cityStatefp);
      end;
    end;
    /*--- Save the nearest obs and reset distance. */
  OutputObs:
    output;
    minDist = 9999;
    cityMax = max(cityMax, length(city));
    cdpMax  = max(cdpMax,  length(cdp));
    if finalObs then do;
      call symputx('CITYMAX', cityMax, 'g');
      call symputx('CDPMAX',  cdpMax,  'g');
    end;
  run;

  /*--- Minimize city/cdp var lengths. */
  options varlenchk=nowarn;
  data &DATASETLIBNAME..CDP_city (label="Census Designated Places (CDP) with nearest incorporated city/town from &TIGERYEAR TIGER PLACE files");
    length city $ &CITYMAX
           cdp  $ &CDPMAX;
    set CDP_city;
  run;
  options varlenchk=warn;

  /*--- Sort final data set for merging. */
  proc sort data=&DATASETLIBNAME..CDP_city;
    by StateForCDP CDP;
  run;
%mend LinkCDPandCity;


/*------------------------------------------------------------------------------*
 | Name:    GetTigerYear
 | Purpose: Determine release year of TIGER data for use in data set labels.
 | Input:   Data set TIGER._counties_to_import
 | Output:  Macro var TIGERYEAR.
 | Note:    Year is parsed from file name of TIGER file, e.g.
 |          tl_2008_10001_faces.dbf.
 *------------------------------------------------------------------------------*/
%macro GetTigerYear / des='Determine release year of TIGER data';
  data _null_;
    set TIGER._counties_to_import;
    /*--- Parse the TIGER file name prefix to get the second word. */
    tigerYear = scan(filename, 2, '_');
    /*--- If non-numeric values found, TIGER file name format changed. */
    if notdigit(trim(tigerYear)) then do;
      put 'ERROR: Cannot determine TIGER release year from filename=' filename;
      put 'ERROR- TIGERYEAR value parsed from filename=' TIGERYEAR;
      put 'ERROR- See "filename" variable in data set TIGER._counties_to_import.';
      call symput( 'TIGERERROR', 'yes' );
    end;
    /*--- Numeric value found. */
    else
      call symputx('TIGERYEAR', tigerYear, 'g');
    /*--- No need to do this repeatedly. Once is sufficient. */
    stop;
  run;
%mend GetTigerYear;
/*
%let tigeryear=;
%let format=;
dm 'clear log';
%let datasetname=test_;
%let tigerpath=C:\Users\sasepo\Geocoding\test\Unzipped TIGER files;
%let datasetpath=C:\Users\sasepo\Geocoding\test;
%tiger2geocode
*/

