if [ -e ./fpdoc ]; then
  FPDOC=./fpdoc
fi 
NEWERDOC=`find bin -newer ./fpdoc -type f | xargs -r ls -t | head -1`
if [ ! -z "$NEWERDOC" ]; then
  FPDOC="$NEWERDOC"
fi 
echo "Using fpdoc executable $FPDOC"
echo "Writing output to fpdoctest"
$FPDOC --package=fpdoc --input='-S2 testunit.pp' --output=fpdoctest --format=html --warn-no-node -v --descr=testunit.xml
