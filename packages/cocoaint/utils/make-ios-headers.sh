#!/bin/bash

# halt on error
set -e

# change
SDKBASEPATH=/Developer/Platforms/iPhoneOS.Platform/Developer/SDKs/iPhoneOS4.2.sdk

if [ $# -ne 0 ]; then
  SDKBASEPATH="$1"
fi

if [ ! -d "$SDKBASEPATH" ]; then
  echo iOS SDK base path \"$SDKBASEPATH\"
  echo not found, specify it as the first parameter to this script.
  echo Note that this parser version has only been verified to work with the
  echo iOS SDK 4.2
  echo
  exit 1
fi

# convert uikit and related framework headers
php parser.php -objp -all -frameworks=foundation,quartzcore,opengles,uikit -root=`pwd`/uikit-skel/src -framework_path="$SDKBASEPATH"/System/Library/Frameworks

# correct some translations the automatic translation cannot handle
patch -p0 < patches/uikit-4.2.patch
# rename badly named unit and include file
sed -e 's/AnonClassDefinitionsQuartzcore/AnonClassDefinitionsUikit/' < uikit-skel/src/AnonClassDefinitionsQuartzcore.pas > uikit-skel/src/AnonClassDefinitionsUikit.pas
mv uikit-skel/src/quartzcore/AnonIncludeClassDefinitionsQuartzcore.inc uikit-skel/src/uikit/AnonIncludeClassDefinitionsUikit.inc
rm uikit-skel/src/AnonClassDefinitionsQuartzcore.pas
# empty two include files that are part of the RTL objcbase unit
echo > uikit-skel/src/foundation/NSObject.inc
echo > uikit-skel/src/foundation/NSZone.inc

echo
echo The headers have been converted and placed under
echo `pwd`/uikit-skel/src.
echo If you wish to compile them, execute the following commands:
echo cd uikit-skel/src
echo ppcarm -XR$SDKBASEPATH -FD`echo $SDKBASEPATH|sed -e 's+/SDKs/[^/]*sdk$++'`/usr/bin -O2 -ap -Cfvfpv2 iPhoneAll.pas
echo
echo The compiled files can be installed afterwards by copying iPhoneAll.{o,ppu} and
echo AnonClassDefinitionsUikit.{o,ppu} to a directory in your FPC unit path.
echo
