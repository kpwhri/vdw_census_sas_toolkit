# Historical methods
Here is a workflow that was used previously for geocoding at KPWHRI:
1. Download shapefiles from US Census using Ruby. See DownloadShapefileData.rb in the lib/archive folder.
1. Process shapefiles from above using SAS. See ProcessShapeFileData.sas and TIGER2Geocode (9.4).sas in the lib/archive folder.
1. Incorporate in a proc geocode call. See example below

## Proc Geocode with SAS
Here is how you might call proc geocode with tiger/line files downloaded with ruby and processed with SAS:
```sas
sasfile geodata.washington_m.data open;
proc geocode
  method=STREET
  data=&TEMPDIR..enrolled_wa
  out=&TEMPDIR..geocoded_wa_gentle
  lookupstreet=geodata.washington_m /* Use just WA data in memory */
  lookupcity=&TEMPDIR..uscity_all
  lookup=interim.zipcode
  addresscityvar=city
  addresszipvar=zip
  addressstatevar=state
  addressvar=address
  attribute_var=(countyfp tract block);
run;
sasfile geodata.washington_m close;
```

