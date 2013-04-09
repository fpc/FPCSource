This package provides support for DBase files.
It is derived from the upstream package at
http://sourceforge.net/projects/tdbf/
It is supported by FPC developers and the upstream maintainers.

Support from other tdbf users may be available on the tdbf forum on
SourceForge: http://sourceforge.net/projects/tdbf/forums/forum/107245


TDbf readme:

See history.txt for changelog.
See history.txt for version number, latest version is at the top.
See INSTALL for installation procedure.
License is LGPL (Library General Public License); see COPYING.LIB for details.

Development notes/additions to end user documentation

property RecNo: approximate record number. Does not take deleted records into account. Used mainly in grids.

File format references:
Visual FoxPro:
http://msdn.microsoft.com/en-us/library/d863bcf2%28v=vs.80%29.aspx

especially this for table structure:
http://msdn.microsoft.com/en-US/library/st4a0s68%28v=vs.80%29.aspx
note however that the file type/magic number at offset 0 is incorrect.
A community member amended these with correct numbers. See bottom of page

Visual FoxPro 9 data types
http://msdn.microsoft.com/en-US/library/ww305zh2%28v=vs.80%29.aspx

Visual FoxPro 9 specific changes:
http://foxcentral.net/microsoft/WhatsNewInVFP9_Chapter09.htm

FoxPro 2.x:
http://support.microsoft.com/kb/98743/en-us

Flagship/FoxPro/Clipper/DBase III..V .dbf file format description
ftp://fship.com/pub/multisoft/flagship/docu/dbfspecs.txt
