1. fpXMLXSDExport
=================
This export module provides export to various forms of XML, selectable by the ExportFormat setting.
Note that not exporting indexes appears as a limitation in most formats below, but indexes will probably need to be changed anyway in an new environment/database.

1.1 AccessCompatible
====================
This format generates Microsoft Access XP/2002-2010 (and presumably SQL Server) compatible XML.
You can specify to include or exclude inline XSD data definition with the CreateXSD setting. Recommended to leave it to true so Access can build up the table structure. If you have an existing compatible table, you wouldn't need the XSD.
Index definitions are exported in this format.
The Access format as emitted by Access XP at least uses the decimal separator of the computer the export is running on. E.g. a US locale will emit 42.42, while a Dutch locale will emit 42,42. If you want to override the decimal separator to a value regardless of locale (recommended if exchanging data across multiple locales), please set the DecimalSeparator (e.g. to '.').
Limitations:
* Import of BLOB/binary type data in Access is problematic as Access has no binary datatype, only an OLE datatype. Access stores OLE metadata, after that the binary object. This entire packet is exported as base64 data.
We cannot replicate this functionality, so we just base64 encode the BLOB, and lose the OLE metadata. If you want to, in Access, you can probably programmatically read the data as blob data and copy it to a "real" OLE field. See e.g. http://support.microsoft.com/?kbid=103257 for hints on how to save the contents of the export BLOB to a file.
* Date/time field data outside of Access limits are discarded (a NULL is inserted).
* GUIDs are exported as text as Access has no equivalent field.
* Large numeric values may overflow Access import capabilities. Patches welcome.

1.2 ADONETCompatible
====================
This format generates XML compatible with the .Net framework (specifically the ADO.NET data access libraries). It should work for .Net versions 2 to 4; version 1/1.1 has not been tested.
In this format you can also specify XSD or no XSD using the CreateXSD setting.
This output format is fairly generic and could be usable for import in other applications, as well.
Limitations:
* Index definitions are not exported in this format.

1.3 ExcelCompatible
===================
This format generates Microsoft Excel XP/2002-2010 compatible XML.
This format recreates the data as Excel worksheet data. It does not include formatting and formulas, just plain data.
The CreateXSD setting has no effect in this format - the Excel XML format doesn't seem to support compatible metadata.
Limitations:
* Index definitions are not exported in this format.
* Binary data is not supported in the Excel XML format.
* The Boolean (true/false) data format is not supported in Excel; instead 0/1 integers are exported.
* Line endings within text cells/fields (generated in Excel by pressing Alt-Enter) are not supported as we'd need to output &#10; in the XML which is not possible using current FCL XML libraries.
An alternative to using this format could be the fpSpreadsheet components.

1.4 DelphiClientDataset
=======================
This format generates Delphi ClientDataset compatible XML.
This format allows Delphi applications to use the ClientDataset.LoadFromFile method to read the data.
It has been written based on TurboDelphi (Delphi 2006) behaviour and exports UTF-8 encoded XML.
The CreateXSD setting has no effect in this format - Clientdataset metadata is always exported.
Limitations:
* Index definitions are not exported in this format.
* Variants are exported as binary data; patches for conversion are welcome.
* Apparently, ClientDatasets have insert new data and update data modes; as of now, only the insert new data mode is supported.

2. Other settings
=================
As the XML formats used above define how date/time formats, boolean formats etc are used, these general export settings have no effect:
- BooleanFalse
- BooleanTrue
- DateFormat
- DateTimeFormat
- DecimalSeparator (only has effects in Access compatible export mode, no effect in other modes)
- CurrencyDigits
- CurrencySymbol
- IntegerFormat
- TimeFormat

3. License
==========
The fpXMLXSDExport module is freeware, licensed under the MIT license: all use free, but no liability accepted.
It is also licensed under the FreePascal modified LGPL license, so take your pick, but don't blame me for things that go wrong.

Reinier Olislagers, 2011