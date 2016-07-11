#!/bin/sh
# 
# Command to regenerate the interface files. 
# For this to work, the utils/convertedmx.lpi project must haven been compiled.
# That project needs WST (Webservice Toolkit) 
utils/convertedmx -a 'microsoft.graph=' -d v4 -i xml/msgraph.xml -o src/msgraph.pp -x ''
utils/convertedmx -d v2 -i xml/sharepoint.xml -o src/sharepoint.pp -x ''
