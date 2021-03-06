#!/usr/bin/env python3

import os
import sys
import glob
import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":

	# Check input parameters
	if len(sys.argv) != 3:
		print("alamo_snapsPlot.py:: error: Invalid argument list")
		print(" ")
		print("Usage:")
		print(" ")
		print("  ./alamo_snapsPlot.py <snaps_path> <images_path>")
		print(" ")
		print("by: Mauri Favaron - 2018")
		sys.exit(1)
	snapsPath  = sys.argv[1]
	imagesPath = sys.argv[2]
	
	# Find snaps in snaps path
	snaps = glob.glob(os.path.join(snapsPath, "snap_*.bin"))
	if len(snaps) <= 0:
		print("alamo_snapsPlot.py:: error: Snaps path contains no snaps")
		sys.exit(2)
	try:
		guideFile = os.path.join(snapsPath, "guide.txt")
		f = open(guideFile, "r")
		lines = f.readlines()
		f.close()
	except:
		print("alamo_snapsPlot.py:: error: No or invalid guide file")
		sys.exit(3)
	if len(lines) <= 0:
		print("alamo_snapsPlot.py:: error: No or invalid guide file")
		sys.exit(3)
		
	# Get plotting extrema from the guide file
	data = np.loadtxt(guideFile)
	xmin = data[0]
	xmax = data[1]
	ymin = data[2]
	ymax = data[3]
	zmin = data[4]
	zmax = data[5]
	dx   = data[6]
	dy   = data[7]
	amax = data[8]
	
	# Process snapshot files
	dt = np.dtype([('x', '<f4'),('y', '<f4'),('z', '<f4'),('q', '<f4'),('t', '<f4')])
	for snap in sorted(snaps):
	
		# Form output image name
		snapName  = os.path.basename(snap).replace('bin', 'png')
		imageName = os.path.join(imagesPath, snapName)
		
		# Get data from snapshot, and retrieve particles coordinates from it
		snapData = np.fromfile(snap, dtype=np.float32)
		if snapData.size > 0:
			snapMatrix = snapData.reshape((-1,5), order='C')
			xp = snapMatrix[:,0]
			yp = snapMatrix[:,1]
			ap = snapMatrix[:,4] / amax
			xpmin    = np.min(xp)
			ypmin    = np.min(yp)
			xpmax    = np.max(xp)
			ypmax    = np.max(yp)
		else:
			xp       = np.array([])
			yp       = np.array([])
			ap       = np.array([])
			xpmin    = 0.0
			ypmin    = 0.0
			xpmax    = 0.0
			ypmax    = 0.0
		
		# Plot data
		fig = plt.figure()
		ax = fig.add_subplot(111, aspect='equal')
		levels = np.array([(0,0,0,a) for a in ap])
		ax.scatter(xp, yp, s=0.1, color=levels)
		plt.xlim(xmin,xmax)
		plt.ylim(ymin,ymax)
		plt.savefig(imageName)
		plt.close()
		
		# Inform users
		print("Processed: %s - Min: (%f,%f)  Max: (%f,%f)" % (snap, xpmin, ypmin, xpmax, ypmax))
		
