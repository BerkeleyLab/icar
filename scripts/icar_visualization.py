# IPython log file

import xarray as xr
import matplotlib.pyplot as plt

ds=xr.open_dataset("output_simple/icar_out_000001_2000-10-01_00-00-00.nc") # image 000001 domain partition
print(ds)
ds["precipitation"][-1].plot() # -1 refers to last time step
ds["precipitation"][-1].plot(vmax=2) # set max of color scale (for a longer time, try vmax ~ 2000)

# write an image to a file
for i in range(len(ds.time)):
    plt.clf()
    ds["precipitation"][i].plot(vmax=2)
    plt.pause(0.1) # 0.1 sec pause between frames
    plt.savefig("file_name.png") # edit this to insert frame number into file name to prevent overwrites 
