#!/bin/bash
fpdoc --package=reslib --format=html \
  --input=../src/resource.pp --descr=resource.xml \
  --input=../src/resourcetree.pp --descr=resourcetree.xml \
  --input=../src/resdatastream.pp --descr=resdatastream.xml \
  --input=../src/resfactory.pp --descr=resfactory.xml \
  --input=../src/resreader.pp --descr=resreader.xml \
  --input=../src/reswriter.pp --descr=reswriter.xml \
  --input=../src/bitmapresource.pp --descr=bitmapresource.xml \
  --input=../src/acceleratorsresource.pp --descr=acceleratorsresource.xml \
  --input=../src/groupresource.pp --descr=groupresource.xml \
  --input=../src/groupiconresource.pp --descr=groupiconresource.xml \
  --input=../src/groupcursorresource.pp --descr=groupcursorresource.xml \
  --input=../src/stringtableresource.pp --descr=stringtableresource.xml \
  --input=../src/versionconsts.pp --descr=versionconsts.xml \
  --input=../src/versiontypes.pp --descr=versiontypes.xml \
  --input=../src/versionresource.pp --descr=versionresource.xml \
  --input=../src/cofftypes.pp --descr=cofftypes.xml \
  --input=../src/coffreader.pp --descr=coffreader.xml \
  --input=../src/coffwriter.pp --descr=coffwriter.xml \
  --input=../src/winpeimagereader.pp --descr=winpeimagereader.xml \
  --input=../src/elfconsts.pp --descr=elfconsts.xml \
  --input=../src/elfreader.pp --descr=elfreader.xml \
  --input=../src/elfwriter.pp --descr=elfwriter.xml \
  --input=../src/machotypes.pp --descr=machotypes.xml \
  --input=../src/machoreader.pp --descr=machoreader.xml \
  --input=../src/machowriter.pp --descr=machowriter.xml \
  --input=../src/externaltypes.pp --descr=externaltypes.xml \
  --input=../src/externalreader.pp --descr=externalreader.xml \
  --input=../src/externalwriter.pp --descr=externalwriter.xml \
  --input=../src/dfmreader.pp --descr=dfmreader.xml


