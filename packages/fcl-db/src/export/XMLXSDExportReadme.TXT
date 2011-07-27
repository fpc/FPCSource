fpXMLXSDExport
==============
This export module provides export to various forms of XML, selectable by the ExportFormat setting:

1. AccessCompatible
Microsoft Access XP/2002-2010 and SQL Server compatible XML
You can specify to include or exclude inline XSD data definition with the CreateXSD setting. Recommended to leave it to true so Access can build up the table structure. If you have an existing compatible table, you wouldn't need the XSD.
Index definitions are exported in this format.
The Access format as emitted by Access XP at least uses the decimal separator of the computer the export is running on. E.g. a US locale will emit 42.42, while a Dutch locale will emit 42,42. If you want to override the decimal separator to a value regardless of locale (recommended if exchanging data across multiple locales), please set the DecimalSeparator.
Import of BLOB/binary type data is problematic as Access has no binary datatype, only an OLE datatype. Access stores OLE metadata, after that the binary object.
This entire packet is exported as base64 data.
We cannot replicate this functionality, so we just base64 encode the BLOB, and lose the OLE metadata. If you want to, in Access, you can probably programmatically read the data as blob data and copy it to a "real" OLE field.

2. ADONETCompatible
XML compatible with the .Net framework (specifically the ADO.NET data access libraries), versions 2 to 4
In this format you can also specify XSD or no XSD using the CreateXSD setting.
This output format is fairly generic and could be usable for import in other applications, as well.
Index definitions are not exported in this format.

3. ExcelCompatible
Microsoft Excel XP/2002-2010 compatible XML
This format recreates the data as Excel worksheet data. It does not include formatting and formulas, just plain data.
Index definitions are not exported in this format.
The CreateXSD setting has no effect in this format.
The Boolean (true/false) data format is not supported in Excel.

3. DelphiClientDataset
Delphi ClientDataset compatible XML
This format allows Delphi applications to use the ClientDataset.LoadFromFile method to read the data.
It has been written based on TurboDelphi (Delphi 2006) behaviour.
Index definitions are not exported in this format.
The CreateXSD setting has no effect in this format.
Apparently, ClientDatasets have insert new data and update data modes; as of now, only the insert new data mode is supported.

Other settings:
As the XML formats used above defines how date/time formats, boolean formats etc are used, thes general export settings have no effect:
BooleanFalse
BooleanTrue
DateFormat
DateTimeFormat
DecimalSeparator (only has effects in Access compatible export mode, no effect in other modes)
CurrencyDigits
CurrencySymbol
IntegerFormat
TimeFormat

License
=======
The fpXMLXSDExport module is freeware, licensed under the MIT license: all use free, but no liability accepted.
It is also licensed under the FreePascal license, so take your pick, but don't blame me for things that go wrong.

Reinier Olislagers, 2011