# IPython log file

import os
import xarray as xr
import matplotlib.pyplot as plt

data_dir_path=os.environ.get("TRAINING_DATA")
ds=xr.open_mfdataset(data_dir_path + "/training_input-image-_*.nc") # temporarily hardcoded file
print(ds)
ds["precipitation"][-1].plot() # -1 refers to last time step
ds["precipitation"][-1].plot(vmax=2) # set max of color scale (for a longer time, try vmax ~ 2000)

# write an image to a file
for i in range(len(ds.time)):
    plt.clf()
    ds["precipitation"][i].plot(vmax=2)
    plt.pause(0.1) # 0.1 sec pause between frames
    plt.savefig("file_name" + str(i) + ".png")
