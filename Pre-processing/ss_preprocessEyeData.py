import sys
import numpy as np
t = np.loadtxt(sys.argv[1]+'frame_table.csv')
t = (t-1)/120
print(t)
np.save(sys.argv[1]+'eye0_timestamps.npy', t)
np.save(sys.argv[1]+'pupil_timestamps.npy', t)
np.save(sys.argv[1]+'world_timestamps.npy', t)
