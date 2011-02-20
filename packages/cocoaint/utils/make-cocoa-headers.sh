#!/bin/bash

# halt on error
set -e

# convert basic cocoa headers
php parser.php -cocoa -root=`pwd`/cocoa-skel/src
# convert webkit headers
php parser.php -all -frameworks=foundation,webkit -root=`pwd`/cocoa-skel/src
# convert coredata headers
php parser.php -all -frameworks=foundation,coredata -root=`pwd`/cocoa-skel/src
# correct some translations the automatic translation cannot handle
patch -p0 < patches/cocoa-coredata-webkit.patch
# empty two headers that exist both in foundation and in appkit, and that
# thereby cause "duplicate identifier" errors
echo > `pwd`/cocoa-skel/src/foundation/NSAttributedString.inc
echo > `pwd`/cocoa-skel/src/foundation/NSAffineTransform.inc
echo
echo The headers have been converted and placed under
echo `pwd`/cocoa-skel/src.
echo If you wish to compile them, execute the following commands:
echo cp ../Makefile cocoa-skel
echo cd cocoa-skel
echo make OPT=\"-O2 -ap\"
echo
echo The compiled files can be installed afterwards using \"make install\"
echo if required. If another compiler binary than the default one has to
echo be used, specify the alternative compiler binary location in the
echo last command by adding FPC=/full/path/to/ppcbinary
echo
