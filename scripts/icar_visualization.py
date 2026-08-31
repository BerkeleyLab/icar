# IPython log file

import os
import xarray as xr 
import matplotlib.pyplot as plt
import matplotlib.animation as animation
 
data_dir_path=os.environ.get("TRAINING_DATA")
ds=xr.open_mfdataset(data_dir_path + "/training_input-image-_*.nc") # temporarily hardcoded file
print(ds)
ds["precipitation"][ -1].plot() # -1 refers to last time step
#ds["precipitation"][-1].plot(vmax=2) # set max of color scale (for a longer time, try vmax ~ 2000)


fig, ax = plt.subplots()
def func(frame):
    plt.clf()
    ds["precipitation"][frame].plot(vmax=2)
   # plt.pause(0.1) #  0.1 sec pause between frames

#anim = animation.FuncAnimation(fig,func,len(ds.time))
anim = animation.FuncAnimation(fig,func,60)

anim.save('animation02 .mp4','ffmpeg',30) 
