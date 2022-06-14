Reprocessed GOES AMVs from 1995 to mid-2013
19 June 2014

GOES AMV files

The dataset contains hourly IR (~10.7 µ), WV (~6.5 µ), SWIR (~3.9 µ), and 
Visible winds. Each AMV is tagged with the sensor and, for WV, whether 
it's clear-sky or cloudy. These are from the operational GOES-East and 
GOES-West satellites: GOES-8 through GOES-15; Years: 1995 to 2013.
SWIR winds are only available in night scenes and only below 700 hPa.
VIS winds are only available below 600 hPa.

The times of the AMVs will vary for a single satellite image, because the 
time is from the satellite image scanline. This time is from the middle image
of the triplet used to track the features. 

The AMV reprocessing was done by sector: GOES-West N. Hemi,  GOES-West S. Hemi,
 GOES-East N. Hemi, GOES-E S. Hemi.

The files can be found here:
http://tropic.ssec.wisc.edu/archive/data/goes_reprocess/wind_files/
ftp://ftp.ssec.wisc.edu/velden/winds/wind_files/

Each gzip tar file contains one month of hourly, GOES winds. The file names are formatted:
[YYYYMM]-GOES-[E or W]-[NH or SH].tar.gz

YYYY is the year
MM is the two digit month
"E or W" for GOES-East or GOES-West
"NH or SH" for Northern Hemisphere or Southern Hemisphere

You should be able to use 'wget' or 'curl' to retrieve the files from the http server.

The naming convention used for the individual text files you should find after 
uncompressing/unpacking the monthly gzipped tar files is:
GOES[E or W]-[NH or SH]-[YYYYMMDD]-[HH]
"E or W" for GOES-East or GOES-West
"NH or SH" for Northern Hemisphere or Southern Hemisphere
YYYY is the year
MM is the two digit month
DD is the two digit day of the month
HH is the nominal two digit hour (UTC)

========================================

The format of each text file:

type: VIS, IR, WV, WVCS (clear sky WV), SWIR (short wave IR)
sat: GOES8, 9, 10, 11, 12, 13, 14, 15
day: yyyymmdd - year, 2-digit month, day of month
hms: hhmm - AMV time in hour, minute (UTC)
lat: latitude (-90 90)
lon: longitude (-180 180, *WEST POSITIVE*)
pre: AMV pressure
spd: speed in m/s
dir: meteorological wind direction (0 to 360 degrees)
rff: recursive filter flag (50 to 100)
qiwf: QI with forecast (0 to 1)
qinf: QI without forecast (0 to 1)
zen: satellite zenith angle in degrees
ch: cloud height method: WIN, HIST, H2O, CO2, BASE
