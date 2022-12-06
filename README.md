## GLMs R script
The R script "GLMs_airsamples.R" contains the code used to generate the GLM results presented in the manuscript "Landscape-scale exposure to multiazole-resistant Aspergillus fumigatus bioaerosols".

------------
## Air sample metadata
The .csv files "XXXX_UKonly_landcover1-21_tasmax" contain the metadata analysed in the GLMs, related to the citizen science samples collected, where XXXX refers to sampling round (SS18 = summer 2018, AE18 = autumn 2018, WS18 = winter 2018, SA19 = spring 2019).

Due to GDPR rules regarding sharing personal data, postcodes have been removed from these files and longitude and latitude coordinates have been rounded to 2 decimal places. This gives the location that each air sample was collected from correct to the nearest 1km, without giving away the exact location of the citizen scientist's home or workplace.

The key to these .csv files:
Code = the unique code assigned to each air sample, which corresponds to the Aspergillus fumigatus isolate(s) grown from it.
Location_n = each citizen scientist collected two air samples, which were assigned locations 1 and 2 (final digit in Code).
Country = for the purpose of this study, all samples were collected in the UK.
Latitude = latitude that air sample was collected from, accurate to 2 decimal places.
Longitude = longitude that air sample was collected from, accurate to 2 decimal places.
Date_colle = date that air sample was collected.
number_col = number of Aspergillus fumigatus colonies grown from air sample.
Afum_01 = growth of Aspergillus fumigatus colonies from air sample coded as 0/1.
ARAf_numbe = number of azole-resistant Aspergillus fumigatus (ARAf) colonies grown from air sample.
ARAf_01 = growth of azole-resistant Aspergillus fumigatus (ARAf) colonies from air sample coded as 0/1.
sample_Afu = Afum_01 + ARAf_01 (such that 0 is no colonies, 1 is A. fumigatus colonies and 2 is ARAf colonies).
SAMPLE_1_e = landcover classifications 1-21 calculated using air sample latitude/longitude and UKCEH 2019 Landcover Map.
urban_rura = landcover classifications categorised as rural (1-19) or urban (20 or 21).
tasmax = maximum daily temperature according to Met Office HadUK-Grid dataset (https://www.metoffice.gov.uk/research/climate/maps-and-data/data/haduk-grid/datasets) at air sampling location on date sample was collected.

-------------------------
## UK postcodes
All in-use UK postcodes, at the time analysis was carried out, were downloaded from https://www.doogal.co.uk/PostcodeDownloads.php.

-------------------------
## UK composter locations
The locations of active composters with open windrow or outdoor activity, at the time analysis was carried out, were downloaded from SEPA (https://www.sepa.org.uk/data-visualisation/waste-sites-and-capacity-tool/), Natural Resources Wales (http://lle.gov.wales/catalogue/item/EnvironmentalPermittingRegulationsWasteSites/?lang=en) and Northern Ireland Environment Agency (https://public-registers.daera-ni.gov.uk/waste-licenses). Composter locations within England were provided by PHE directly and can be made available on request with the agreement of PHE.

-------------------------
## Percentage arable cover
"GB_uniqpc_2kmbuffer_arable.csv" and "NI_uniqpc_2kmbuffer_arable.csv" contain the percentage arable cover in the 2km surrounding each air sampling location. Postcode has been removed for GDPR reasons so these would need to be matched to the air sample metadata using latitude and longitude coordinates. arab_perc (GB) and ni-lcm-2_1 (NI) are the columns with percentage arable cover.
