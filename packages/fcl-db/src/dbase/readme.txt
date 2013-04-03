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
A community member amended these, and these values match other sources:
FoxBASE/dBase II: 0x02
FoxBASE+/FoxPro/Dbase III plus, no memo: 0x03
Visual FoxPro: 0x30
Visual FoxPro, autoincrement enabled: 0x31
Visual FoxPro, Varchar, Varbinary, or Blob-enabled: 0x32
dBASE IV SQL table files, no memo: 0x43
dBASE IV SQL system files, no memo: 0x63
FoxBASE+/dBASE III PLUS, with memo: 0x83
dBASE IV with memo: 0x8B
dBASE IV SQL table files, with memo: 0xCB
FoxPro 2.x (or earlier) with memo: 0xF5
FoxBASE: 0xFB
