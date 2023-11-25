#!/bin/bash
DATAPATH='/home/spitschan/Desktop/SpectroSense/Pre_analyzed/SP112'
EYETRACKPATH=$DATAPATH'/gazecalibration/000/'
# Copy files to in/
mkdir $EYETRACKPATH/in
cp $DATAPATH/spectra/*.jpg $EYETRACKPATH/in

# Resize and convert to .jpg
mogrify -resize 320x240 $EYETRACKPATH/in/*.jpg

rm *timestamps*

# Preliminaries
mkdir $EYETRACKPATH/out/
ffmpeg -i $EYETRACKPATH/eye0.mp4 -qscale 0 $EYETRACKPATH/out/eye0_%07d.jpg


LASTFILE=`ls $EYETRACKPATH/out/ | tail -1`
STARTIDX0=${LASTFILE:5:7}
STARTIDX1=$(echo $STARTIDX0 | sed 's/^0*//')
STARTIDX=$(($STARTIDX1+1))
NUMFILES=`ls $EYETRACKPATH/in/` | wc -l

NFRAMESPERIMAGE=50
C=0

# Copy stills over
echo "Original file,Renamed file,Frame #" > $EYETRACKPATH/correspondence_table.csv
for ii in `find $EYETRACKPATH/in/ -name '*.jpg' -exec basename {} \; | sort`
	do echo $ii","eye0_$((STARTIDX+C)).jpg,$((STARTIDX+C)) >> $EYETRACKPATH/correspondence_table.csv
	for ij in $(seq 1 $NFRAMESPERIMAGE)
	do FILENAME=`printf "eye0_%07d\n" $((STARTIDX+C))`
	cp $EYETRACKPATH/in/$ii $EYETRACKPATH/out/$FILENAME.jpg
	C=$((C+1))
	done
done

echo `seq 1 $((STARTIDX+C-1))` > $EYETRACKPATH/frame_table.csv

# Make new video
mv $EYETRACKPATH/eye0.mp4 $EYETRACKPATH/eye0.mp4_orig
ffmpeg -i $EYETRACKPATH/out/eye0_%07d.jpg -an -q:v 3 -pix_fmt yuv420p -vcodec mjpeg $EYETRACKPATH/eye0.mp4

# Delete pupil data
rm -rf $EYETRACKPATH/pupil.pldata

# Run python script
python ss_preprocessEyeData.py $EYETRACKPATH

# Run pupil player
pupil_player $EYETRACKPATH/

# Produce output
python ss_combineEyeResults.py $EYETRACKPATH

# Produce final output
python ss_generateFinalReport.py $DATAPATH $EYETRACKPATH 
