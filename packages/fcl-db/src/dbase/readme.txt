This package provides support for DBase files.
It is derived from the upstream package at
http://sourceforge.net/projects/tdbf/
It is supported by FPC developers and the upstream maintainers.

Support from other tdbf users may be available on the tdbf forum on
SourceForge: http://sourceforge.net/projects/tdbf/forums/forum/107245


TDbf readme:

See history.txt for changelog, and version number.
See INSTALL for installation procedure.
License is LGPL (Library General Public License); see COPYING.LIB for details.

Development notes
=================
(Includes possible additions to end user documentation)

property RecNo: approximate record number. Does not take deleted records into account. Used mainly in grids.

File format references:
Flagship/FoxPro/Clipper/DBase III..V .dbf file format description
ftp://fship.com/pub/multisoft/flagship/docu/dbfspecs.txt

DBase IV language drivers 
- Russian list: ...
http://www.autopark.ru/ASBProgrammerGuide/DBFSTRUC.HTM
... English extract:
- http://shapelib.maptools.org/codepage.html

- as supported by ArcPad .shp files (basically DBase IV) (page 128):
http://downloads.esri.com/support/documentation/pad_/ArcPad_RefGuide_dec2007.pdf


FoxPro 2.x:
http://support.microsoft.com/kb/98743/en-us
Data type:
P Picture (foxpro/vfoxpro specific)

Visual FoxPro:
http://msdn.microsoft.com/en-us/library/d863bcf2%28v=vs.80%29.aspx

especially this for table structure:
http://msdn.microsoft.com/en-US/library/st4a0s68%28v=vs.80%29.aspx
note however that the file type/magic number at offset 0 is incorrect.
A community member amended these with correct numbers. See bottom of page

Visual FoxPro 6 internal structures, _NULLFIELDS etc:
http://www.dfpug.de/buecher/fundamentals/Hack6/S1C2.HTM

Visual Foxpro 8 info about autoincrement:
http://msdn.microsoft.com/en-us/library/aa976850%28v=VS.71%29.aspx

Visual FoxPro 9 data types
http://msdn.microsoft.com/en-US/library/ww305zh2%28v=vs.80%29.aspx

Visual FoxPro 9 specific changes:
http://foxcentral.net/microsoft/WhatsNewInVFP9_Chapter09.htm
New data types:
V Varchar/varchar binary (in Visual FoxPro 9) 1 byte up to 254 bytes.
	Same storage as char (padded spaces) but padding is removed on display
W Blob (Visual FoxPro 9), 4 bytes in a table; stored in .fpt
Q Varchar (binary) (in Visual Foxpro 9):
	accepts null, up to 254 characters (stored as padded with spaces), no code page translations
	note varchar (binary)<>varbinary
