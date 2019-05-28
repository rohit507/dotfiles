#!/bin/sh
# Automatic stylus (pen) orientation for HP Spectre x360
# adapted by Efrem Rensi
#  This is a fix for an apparent bug in Cinnamon whereby System Settings >> General >> Automatic Screen Rotation
#   Rotates the screen and touch functionality, but the pen orientation does not rotate

# Based on chadm's script at https://linuxappfinder.com/blog/auto_screen_rotation_in_ubuntu.

# Receives input from monitor-sensor (part of iio-sensor-proxy package) and sets the touchscreen
# orientation based on the accellerometer positionn. We assume that the display rotation is 
# handled by Linux Mint 18.1, Cinnamon 3.2.7. If this is not the case, add the appropriate
# xrandr command into each case block.

# This script should be added to startup applications for the user.


PEN=18
TOUCH=10

# The device number PEN=18 is obtained by running
#   xinput
# and reading the output, which for my HP Spectre x360 is:
# ⎡ Virtual core pointer                      id=2    [master pointer  (3)]
# ⎜   ↳ Virtual core XTEST pointer                id=4    [slave  pointer  (2)]
# ⎜   ↳ ELAN2514:00 04F3:2592                     id=11   [slave  pointer  (2)]
# ⎜   ↳ ELAN2514:00 04F3:2592 Pen Pen (0)         id=18   [slave  pointer  (2)]
# ⎜   ↳ SynPS/2 Synaptics TouchPad                id=15   [slave  pointer  (2)]
# + other lines


# Kill any existing monitor-sensor instance, for example if manually invoking
# from a terminal for testing.
killall monitor-sensor

# Launch monitor-sensor and store the output in a RAM based file that can be checked by the rest of the script.
# We use the RAM based file system to save wear where an SSD is being used.
monitor-sensor > /dev/shm/sensor.log 2>&1 &

# Parse output of monitor sensor to get the new orientation whenever the log file is updated
# Possibles are: normal, bottom-up, right-up, left-up
# Light data will be ignored
while inotifywait -e modify /dev/shm/sensor.log; do

# Read the last few lines that were added to the file and get the last orientation line.
ORIENTATION=$(tail /dev/shm/sensor.log | grep 'orientation' | tail -1 | grep -oE '[^ ]+$')

echo "$ORIENTATION"
# Set the actions to be taken for each possible orientation
case "$ORIENTATION" in

bottom-up)
xinput set-prop $PEN 'Coordinate Transformation Matrix' -1 0 1 0 -1 1 0 0 1;
xinput set-prop $TOUCH 'Coordinate Transformation Matrix' -1 0 1 0 -1 1 0 0 1;;

normal)
xinput set-prop $PEN 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1;
xinput set-prop $TOUCH 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1;;

right-up)
xinput set-prop $PEN 'Coordinate Transformation Matrix' 0 1 0 -1 0 1 0 0 1;
xinput set-prop $TOUCH 'Coordinate Transformation Matrix' 0 1 0 -1 0 1 0 0 1;;

left-up)
:q
xinput set-prop $TOUCH 'Coordinate Transformation Matrix' 0 -1 1 1 0 0 0 0 1;;

esac
done

# On stopping this script, don't forget that "monitor-sensor" is still running - hence the "killall" !

