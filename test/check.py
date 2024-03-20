#!/usr/bin/env python

import xarray as xr
import sys
import numpy as np

# get command-line arguments:
filename1 = sys.argv[1]
filename2 = sys.argv[2]

# print(f"Comparing {filename1} with {filename2}")

ds1 = xr.open_dataset(filename1)
ds2 = xr.open_dataset(filename2)

# loop over all variables in file and find maximum error
max_delta = 0
for v in ds1.variables:
    if v != "time":
        delta = np.abs(ds1[v] - ds2[v]).values.max()
        max_delta = max(delta, max_delta)
# print(f"Maximum difference={max_delta}")

print(f"{max_delta}") # curly braces bracket name of a variable

if max_delta > 1.E-10 : 
    sys.exit(0)
else :
    sys.exit(1)
