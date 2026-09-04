# IPython log file

import os
import sys
import xarray as xr 
import matplotlib.pyplot as plt
import matplotlib.animation as animation

data_dir_path=os.environ.get("TRAINING_DATA")
ds=xr.open_mfdataset(data_dir_path + "/training_input-image-_*.nc") # temporarily hardcoded file
print(ds)

fig, ax = plt.subplots()
def func(frame):
    plt.clf()
    ds["precipitation"][frame].plot(vmax=2) #vmax sets max of color scale

#anim = animation.FuncAnimation(fig,func,len(ds.time))
anim = animation.FuncAnimation(fig,func,60)

anim.save(sys.argv[1],'ffmpeg',30) 
