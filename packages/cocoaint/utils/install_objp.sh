sudo svn co http://svn.freepascal.org/svn/fpc/branches/objc /Developer/ObjectivePascal/fpc
cd /Developer/ObjectivePascal/fpc
sudo make distclean -j 3
make FPC=/usr/local/lib/fpc/2.2.4/ppc386 OPT="-ap" distclean all -j 2
export NEWFPCVERSION=`./compiler/ppc386 -iV`
sudo make FPC=`pwd`/compiler/ppc386 install
sudo make distclean -j 3
make FPC=/usr/local/lib/fpc/$NEWFPCVERSION/ppc386 OPT="-ap" CPU_TARGET=powerpc all -j 2
sudo make FPC=`pwd`/compiler/ppcrossppc CROSSINSTALL=1 install
sudo mv /usr/local/lib/fpc/$NEWFPCVERSION/ppcrossppc /usr/local/lib/fpc/$NEWFPCVERSION/ppcppc
sudo cp -R /Developer/ObjectivePascal/fpc/packages/cocoaint/src /Developer/ObjectivePascal/Units
sudo chmod 0777 /Developer/ObjectivePascal/Units/uikit
php /Developer/ObjectivePascal/fpc/packages/cocoaint/utils/parser.php -iphone -root="/Developer/ObjectivePascal/Units"