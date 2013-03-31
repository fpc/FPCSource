#! /bin/sh

DOSBOX_TEST=/home/nickysn/dosbox_test
DOSBOX=dosbox

# TODO: generate dosbox.conf with $DOSBOX_TEST/dosroot in the autoexec mount section

rm -rf $DOSBOX_TEST/dosroot
mkdir $DOSBOX_TEST/dosroot
cp $1 $DOSBOX_TEST/dosroot/test.exe
cp $DOSBOX_TEST/exitcode.exe $DOSBOX_TEST/dosroot/exitcode.exe
$DOSBOX -exit -conf $DOSBOX_TEST/dosbox.conf
exit `cat $DOSBOX_TEST/dosroot/EXITCODE.TXT`
