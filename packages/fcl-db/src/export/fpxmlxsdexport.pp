unit fpXMLXSDExport;

{Output XML with XSD file suitable for input in:
- databases (e.g. Microsoft Access, Microsoft SQL Server)
- spreadsheets (e.g. Microsoft Excel)
- .Net ADO.NET database applications
}
{
Data type conversions I'm not sure about are marked with todo:
the export will probably work but might not give you the results you want. Patches welcome.
You'll probably need to modify both the part where the XSD datatype info is generated as well as the handling of data output.

I've added possible tasks to do using Lazarus to do list items; priority 1 (high) to 10 (low), categories Must have, Should have, Nice to have
I've marked source code with todo if there is something that could be looked at but seems not essential for functionality.

See the readme file for more details.

(c) Reinier Olislagers 2011, MIT license: all use permitted but no liability accepted.
Also licensed according to FreePascal/FCL license.
Take your pick, but don't blame me if something goes wrong ;)
}

{ TODO 7 -oAnybody -cNice to have : Apparently, ClientDatasets have insert new data and update data modes; as of now, only the insert new data mode is supported. Implement other modes.}
{ TODO 8 -oAnybody -cNice to have : Test Microsoft SQL Server xml import with Access format xml, using e.g. openxml or SSIS }
{ TODO 7 -oAnybody -cNice to have : Inherited settings
BooleanFalse
BooleanTrue
DateTimeFormat
CurrencyDigits
CurrencySymbol
DateFormat
IntegerFormat
TimeFormat
are not needed. Is there any way to remove them from the settings?}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpDBExport, DOM, XMLWrite, base64, typinfo;

type
  TXMLExportFormat = (AccessCompatible, ADONETCompatible, DelphiClientDataset,
    ExcelCompatible);
  //Possible export types.
  {
  AccessCompatible should cover at least Microsoft Access XP/2002, 2003, 2007, and 2010
  ADONETCompatible should cover at least .Net framework 2, 3, 3.5 and 4. ADONETCompatible is the most clearcut, simple export, and could be use for other environments as well.
  DelphiClientDataset is based on TurboDelphi (Delphi 2006) unicode XML export; additions/improvements for other versions are welcome.
  ExcelCompatible should cover at least Microsoft Excel XP/2002, 2003, 2007, and 2010
  }

  { TODO 9 -oAnyone -cNice to have : If we can support databases that store timezone info with date/time, e.g. Oracle, add an option StoredInDatabase }
  { TXMLXSDFormatSettings }
  TXMLXSDFormatSettings = class(TExportFormatSettings)
  private
    FExportFormat: TXMLExportFormat;
    FXSDUsed: boolean;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property CreateXSD: boolean read FXSDUsed write FXSDUsed;
    property ExportFormat: TXMLExportFormat read FExportFormat write FExportFormat;
  end;

  { TCustomXMLXSDExporter }
  TCustomXMLXSDExporter = class(TCustomFileExporter)
  const
    DefaultDatasetName = 'Table1';
    // Name used for the exported table/dataset if no other name can be found.
  private
    FANode: TDOMNode; //Just a placeholder for a node which can be reused
    FDatasetExportName: string; //Table name to be used in export
    FFormatSettingsOriginal: TFormatSettings;
    FOutputDoc: TXMLDocument;
    FRootNode: TDOMNode; //Root node for XML doc
    FRowDataNode: TDOMNode;
    //Node at the beginning of each data row. Contains actual field data.
    FTableDataParentNode: TDOMNode;
    //dataroot element; Parent node for actual table data; named ROWDATA in DelphiClientDataset
    FXSDTableListNode: TDOMNode;
    //Contains xs:element node with list of tables to be exported
    FXSDSchemaNode: TDOMNode; //Top node for XSD
    fXSDTableAnnotationNode: TDOMNode;
    //Contains xs:annotation for a table, which contains appinfo node with primary key info etc.
    FXSDTableAppinfoNode: TDOMNode; //Contains xs:appinfo node for a table
    FXSDTableDataColumnParent: TDOMNode;
    //xs:sequenc node; contains all columns for a table.
    FXSDTableNode: TDOMNode;
    //Contains xs:Element for table, inside: primary key info etc, table data types
    FXSDTableDataTypesNode: TDOMNode;
    //Contains xs:ComplexType element listing data types for table
    FXSDUsed: boolean;
    procedure ExportFieldData(const EF: Texportfielditem); //Export for Access, ADO.Net
    procedure ExportFieldDataClientDataset(const EF: Texportfielditem);
    //Export for DelphiClientDataset
    procedure ExportFieldDataExcel(const EF: Texportfielditem);
    //Export for Access, ADO.Net
    function GetXMLFormatsettings: TXMLXSDFormatSettings;
    procedure SetXMLFormatSettings(const AValue: TXMLXSDFormatSettings);
  protected
    function CreateFormatSettings: TCustomExportFormatSettings; override;
    procedure DoBeforeExecute; override;
    procedure DoAfterExecute; override;
    procedure DoDataRowStart; override;
    procedure DoDataHeader; override;
    procedure DoDataFooter; override;
    procedure DoDataRowEnd; override;
    procedure ExportField(EF: TExportFieldItem); override;
  public
    property FormatSettings: TXMLXSDFormatSettings
      read GetXMLFormatsettings write SetXMLFormatSettings;
  end;

  TXMLXSDExporter = class(TCustomXMLXSDExporter)
  published
    property FileName;
    property Dataset;
    property ExportFields;
    property FromCurrent;
    property RestorePosition;
    property FormatSettings;
    property OnExportRow;
  end;

procedure RegisterXMLXSDExportFormat;
procedure UnRegisterXMLXSDExportFormat;

const
  SXMLXSD = 'XMLXSD';
  SXMLXSDExtensions = '.xml';

resourcestring
  SXMLXSDDescription = 'Unicode XML file with XSD';

implementation


{ TCustomXMLXSDExporter }

function TCustomXMLXSDExporter.GetXMLFormatsettings: TXMLXSDFormatSettings;
begin
  Result := TXMLXSDFormatSettings(inherited FormatSettings);
end;

procedure TCustomXMLXSDExporter.ExportFieldData(const EF: Texportfielditem);
var
  Fieldnode: Tdomnode;
begin
  Fieldnode := Foutputdoc.CreateElement(Utf8decode(EF.Fieldname));
  //If you change the export types here, you'll also have to change the
  //metadata export in the XSD export section...
  case EF.Field.Datatype of
    Ftarray:
    begin
      //ftArray: maybe Firebird array type. For now, pretend it's a string.
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftadt:
    begin
      //ftAdt: No idea what this does; pretend it's a string
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftautoinc:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftbcd:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftboolean:
    begin
      case FormatSettings.ExportFormat of
        AccessCompatible: Fanode :=
            Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
        DelphiClientDataset:
        begin
          if (EF.Field.AsBoolean = True) then
          begin
            Fanode := Foutputdoc.Createtextnode(Utf8decode('TRUE'));
          end
          else
          begin
            Fanode := Foutputdoc.Createtextnode(Utf8decode('FALSE'));
          end;
        end;

        else //ADO.net in this case
        begin
          if (EF.Field.AsBoolean = True) then
          begin
            Fanode := Foutputdoc.Createtextnode(Utf8decode('true'));
          end
          else
          begin
            Fanode := Foutputdoc.Createtextnode(Utf8decode('false'));
          end;
        end;
      end;
    end;

    Ftblob:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftbytes:
    begin
      //ftbytes is some kind of blob field
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftcurrency:
    begin
      //make sure there's no currency symbol in there...
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftcursor:
    begin
      //No idea what this does; pretend it's a string.
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftdataset:
    begin
      //No idea what this does; pretend it's a string
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftdate:
    begin
      //Apparently no conversion of local time to UTC
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T00:00:00"', EF.Field.AsDateTime));
    end;

    Ftdatetime:
    begin
      //Apparently no conversion of local time to UTC
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', EF.Field.AsDateTime));
    end;

    Ftdbaseole:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftfixedchar:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftfixedwidechar:
    begin
      Fanode :=
        Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    FtFloat:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    FtFmtbcd:
    begin
      //Assuming some kind of BCD/Binary Coded Decimal, so this might just work
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftfmtmemo:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftgraphic:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftguid:
    begin
      //Might even work, depending on the .AsString implementation for GUID
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftidispatch:
    begin
      //No idea what this does; pretend it's a string.
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftinteger:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftinterface:
    begin
      //No idea what this does; pretend it's a string.
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftlargeint:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftmemo:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftorablob:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftoraclob:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftparadoxole:
    begin
      //Hope the FPC library gives the right version of Base64...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftreference:
    begin
      //No idea what this does; pretend it's a string
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftsmallint:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftstring:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Fttime:
    begin
      //Just guessing here. Maybe needs a date part, too.
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('hh":"nn":"ss', EF.Field.AsDateTime));
    end;

    Fttimestamp:
    begin
      //Apparently no conversion of local time to UTC
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', EF.Field.AsDateTime));
    end;

    Fttypedbinary:
    begin
      //Assume ftTypedBinary is just a binary/blob
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftvarbytes:
    begin
      //VariBytes: assume just a binary/blob...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftvariant:
    begin
      // ftVariant: pretend it's a string & just hope for the best...
      Fanode := Foutputdoc.Createtextnode(Encodestringbase64(EF.Field.AsString));
    end;

    Ftwidememo:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftwidestring:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftword:
    begin
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftunknown:
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;
    else
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));

    end;
  end; //case
  FieldNode.AppendChild(FAnode);
  FRowDataNode.AppendChild(Fieldnode);
end;

procedure TCustomXMLXSDExporter.ExportFieldDataClientDataset(const EF: Texportfielditem);

begin
  //If you change the export types here, you'll also have to change the
  //metadata export elsewhere...
  case EF.Field.Datatype of
    Ftarray:
    begin
      //ftArray: maybe Firebird array type. For now, pretend it's a memo.
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftadt:
    begin
      //ftAdt: No idea what this does; pretend it's a memo
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftautoinc:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftbcd:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftboolean:
    begin
      if (EF.Field.AsBoolean = True) then
      begin
        TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
          Utf8decode('TRUE'));
      end
      else
      begin
        TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
          Utf8decode('FALSE'));
      end;
    end;

    Ftblob:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftbytes:
    begin
      //ftbytes is some kind of blob field
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftcurrency:
    begin
      //make sure there's no currency symbol in there...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftcursor:
    begin
      //No idea what this does; pretend it's a memo.
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftdataset:
    begin
      //No idea what this does; pretend it's a memo
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftdate:
    begin
      //Apparently no conversion of local time to UTC
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Formatdatetime('yyyymmdd', EF.Field.AsDateTime));
    end;

    Ftdatetime:
    begin
      //Apparently no conversion of local time to UTC
      { TODO 5 -oAnyone -cShould have : This probably needs to be fixed to include time info }
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Formatdatetime('yyyymmdd', EF.Field.AsDateTime));
    end;

    Ftdbaseole:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftfixedchar:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftfixedwidechar:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    FtFloat:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    FtFmtbcd:
    begin
      //Assuming some kind of BCD/Binary Coded Decimal, so this might just work
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftfmtmemo:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftgraphic:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftguid:
    begin
      //Might even work, depending on the .AsString implementation for GUID
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftidispatch:
    begin
      //No idea what this does; pretend it's a memo.
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftinteger:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftinterface:
    begin
      //No idea what this does; pretend it's a memo.
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftlargeint:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftmemo:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftorablob:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftoraclob:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftparadoxole:
    begin
      //Hope the FPC library gives the right version of Base64...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftreference:
    begin
      //No idea what this does; pretend it's a memo
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftsmallint:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftstring:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Fttime:
    begin
      { TODO 5 -oAnyone -cShould have : This probably needs to be fixed to include time info }
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Formatdatetime('yyyymmdd', EF.Field.AsDateTime));
    end;

    Fttimestamp:
    begin
      { TODO 5 -oAnyone -cShould have : This probably needs to be fixed to include time info }
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Formatdatetime('yyyymmdd', EF.Field.AsDateTime));
    end;

    Fttypedbinary:
    begin
      //Assume ftTypedBinary is just a binary/blob
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftvarbytes:
    begin
      //VariBytes: assume just a binary/blob...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Encodestringbase64(EF.Field.AsString));
    end;

    Ftvariant:
    begin
      // ftVariant: pretend it's a memo & just hope for the best...
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftwidememo:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftwidestring:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftword:
    begin
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;

    Ftunknown:
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;
    else
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      TDOMElement(FRowDataNode).SetAttribute(Utf8decode(EF.ExportedName),
        Utf8decode(EF.Field.AsString));
    end;
  end; //case
end;

procedure TCustomXMLXSDExporter.ExportFieldDataExcel(const EF: Texportfielditem);
var
  FieldNode: TDOMNode;
  CellNode: TDOMNode;
begin
  CellNode := FoutputDoc.CreateElement(Utf8decode('Cell'));
  FRowDataNode.AppendChild(CellNode);

  FieldNode := FOutputDoc.CreateElement(UTF8Decode('Data'));
  //If you change the export types here, you'll also have to change the
  //metadata export in the XSD export section...
  case EF.Field.Datatype of
    Ftarray:
    begin
      //ftArray: maybe Firebird array type. For now, pretend it's a string.
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftadt:
    begin
      //ftAdt: No idea what this does; pretend it's a string
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftautoinc:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftbcd:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftboolean:
    begin
      // Boolean datatype is not supported, so just use numeric 0/1 output
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      if (EF.Field.AsBoolean = True) then
      begin
        Fanode := Foutputdoc.Createtextnode(Utf8decode('1'));
      end
      else
      begin
        Fanode := Foutputdoc.Createtextnode(Utf8decode('0'));
      end;
    end;

    ftBLOB:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftbytes:
    begin
      //ftbytes is some kind of blob field
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftcurrency:
    begin
      //make sure there's no currency symbol in there...
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftcursor:
    begin
      //No idea what this does; pretend it's a string.
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftdataset:
    begin
      //No idea what this does; pretend it's a string
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftdate:
    begin
      //Apparently no conversion of local time to UTC
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'DateTime');
      FANode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T00:00:00"', EF.Field.AsDateTime));
      TDOMElement(CellNode).SetAttribute('ss:StyleID', 's23');
    end;

    Ftdatetime:
    begin
      //Apparently no conversion of local time to UTC
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'DateTime');
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', EF.Field.AsDateTime));
      TDOMElement(CellNode).SetAttribute('ss:StyleID', 's21');
    end;

    Ftdbaseole:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftfixedchar:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftfixedwidechar:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode :=
        Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    FtFloat:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    FtFmtbcd:
    begin
      //Assuming some kind of BCD/Binary Coded Decimal, so this might just work
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftfmtmemo:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftgraphic:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftguid:
    begin
      //Might even work, depending on the .AsString implementation for GUID
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftidispatch:
    begin
      //No idea what this does; pretend it's a string.
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftinteger:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftinterface:
    begin
      //No idea what this does; pretend it's a string.
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftlargeint:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftmemo:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftorablob:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftoraclob:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftparadoxole:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftreference:
    begin
      //No idea what this does; pretend it's a string
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftsmallint:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftstring:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Fttime:
    begin
      //Just guessing here. Maybe needs a date part, too.
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'DateTime');
      Fanode := Foutputdoc.CreateTextnode(
        Formatdatetime('"1899-12-31T"hh":"nn":"ss', EF.Field.AsDateTime));
      //Apparently Day 0 in Excel land.
      TDOMElement(CellNode).SetAttribute('ss:StyleID', 's22');
    end;

    Fttimestamp:
    begin
      //Apparently no conversion of local time to UTC
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'DateTime');
      Fanode := Foutputdoc.Createtextnode(
        Formatdatetime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', EF.Field.AsDateTime));
      TDOMElement(CellNode).SetAttribute('ss:StyleID', 's21');
    end;

    Fttypedbinary:
    begin
      //Assume ftTypedBinary is just a binary/blob
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftvarbytes:
    begin
      //VariBytes: assume just a binary/blob...
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftvariant:
    begin
      // ftVariant: pretend it's a string & just hope for the best...
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(UTF8Decode('Binary import not supported'));
    end;

    Ftwidememo:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftwidestring:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    Ftword:
    begin
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'Number');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;

    FtUnknown:
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;
    else
    begin
      //raise Exception.Create('Unknown datatype for dataset field while exporting.');
      TDOMElement(FieldNode).SetAttribute('ss:Type', 'String');
      Fanode := Foutputdoc.Createtextnode(Utf8decode(EF.Field.AsString));
    end;
  end; //case
  CellNode.AppendChild(FieldNode);
  FieldNode.AppendChild(FANode);
end;

procedure TCustomXMLXSDExporter.SetXMLFormatSettings(
  const AValue: TXMLXSDFormatSettings);
begin
  inherited FormatSettings := AValue;
end;

function TCustomXMLXSDExporter.CreateFormatSettings: TCustomExportFormatSettings;
begin
  Result := TXMLXSDFormatSettings.Create(False);
end;

procedure TCustomXMLXSDExporter.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
  //Don't know if this should go somewhere else, but this works:
  //Exported table/datasetname
  if DataSet.Name = '' then
  begin
    // Maybe there's another way to get a name?
    if Self.FileName = '' then
    begin
      FDatasetExportName := DefaultDatasetName;
    end
    else
    begin
      FDatasetExportName := DefaultDatasetName;
      //todo: need to sanitize filename if we want to use it (no .s, no special characters)
      //todo: would be nice to leave out the extension.
      //FDatasetExportName := ExtractFileName(Self.FileName);
    end;
  end
  else
  begin
    FDatasetExportName := DataSet.Name;
  end;

  FOutputDoc := TXMLDocument.Create;
  case FormatSettings.ExportFormat of
    AccessCompatible:
    begin
      {Though UTF-8 is the default in XML, the original Access XP export
      explicitly specifies it, so we'll do so, too.
      However, note that current FCL DOM doesn't generate an encoding attribute even though we supply it.}
      FOutputDoc.Encoding := UTF8Decode('UTF-8');
    end;
    ADONETCompatible:
    begin
      { This has a standalone=yes attribute in the first line, but that is not required for import in ADO.Net 2 or 4}
    end;
    DelphiClientDataset:
    begin
      { This has a standalone=true attribute and encoding="UTF-8" in the first line, but current FPC XML code doesn't support these attributes}
      {Though UTF-8 is the default in XML, the original Access XP export
      explicitly specifies it, so we'll do so, too.
      However, note that current FCL DOM doesn't generate an encoding attribute even though we supply it.}
      FOutputDoc.Encoding := UTF8Decode('UTF-8');
    end;
  end;

  OpenTextFile;

  // Set up settings
  FXSDUsed := FormatSettings.CreateXSD;

  //Decimal separator - only relevant for Access export:
  //probably can use DefaultFormatsettings too, have to test:
  FFormatSettingsOriginal := SysUtils.FormatSettings;
  case FormatSettings.ExportFormat of
    AccessCompatible:
    begin
      // For interoperable XML output, use locale decimal separator if we use the older Access XP format.
      // This can be overridden by user specification
      if (FormatSettings.DecimalSeparator <> '') then
      begin
        SysUtils.FormatSettings.DecimalSeparator := FormatSettings.DecimalSeparator;
      end;
    end;
    else
    begin
      // ADO.Net, Delphi, Excel export format seem to always expect a . which is much saner.
      // This means you can import a file exported on a computer with another locale setting ;)
      SysUtils.FormatSettings.DecimalSeparator := '.';
    end;
  end;
  SysUtils.FormatSettings.DateSeparator := '-';
end;

procedure TCustomXMLXSDExporter.DoAfterExecute;
var
  TargetFile: Text;
begin
  // Actually write the XML file to disk:
  TargetFile := TextFile;
  WriteXMLFile(FOutputDoc, TargetFile);
  FOutputDoc.Free;
  CloseTextFile;
  //Restore original, client locale specific setting for formatting
  SysUtils.FormatSettings := FFormatSettingsOriginal;
  inherited DoAfterExecute;
end;

procedure TCustomXMLXSDExporter.DoDataRowStart;
begin
  // Start data node
  case FormatSettings.ExportFormat of
    DelphiClientDataset: FRowDataNode := FOutputDoc.CreateElement(UTF8Decode('ROW'));
    ExcelCompatible: FRowDataNode := FOutputDoc.CreateElement(UTF8Decode('Row'));
    else
      FRowDataNode := FOutputDoc.CreateElement(UTF8Decode(FDatasetExportName));
  end;
end;


procedure TCustomXMLXSDExporter.DoDataHeader;
// Exports metadata, such as column definitions.
// Yes, this is an awfully long procedure which
// might be refactored into various parts.
// Let's first get it to work.
var
  ItemCounter: integer;
  Index: TIndexDef;
  ColumnNode: TDOMNode; //xs:element node, contains column metadata; parent for column
  DelphiFieldsNode: TDOMNode; // For DelphiClientDataset: <FIELDS> tag
  DelphiMetadataNode: TDOMNode; // For DelphiClientDataSet: <METADATA> tag
  IndexDefs: TIndexDefs;
  ExcelStyles: TDOMNode; //  <Styles> node for Excel export
  StyleElement: TDOMNode; //   <Style ss:ID="bla"> node for Excel export

  procedure ExportIndexesAccessXP;
  var
    DescendingIndex: string;
    FieldCounter: integer;
    IndexCounter: integer;
    IndexFieldsList: TStringList;
    IndexDirection: string;
  begin
    if FXSDUsed then
    begin
      { TODO 9 -oAnyone -cNice to have : Following part taken from
  http://wiki.lazarus.freepascal.org/How_to_write_in-memory_database_applications_in_Lazarus/FPC
  Is there a better way to find out if there are indexes? }
      // Apparently, in e.g. MySQL fcl-db, IndexDefs only has one index, DEFAULT_ORDER;
      // use ServerIndexDefs instead
      if IsPublishedProp(DataSet, 'ServerIndexDefs') then
      begin
        IndexDefs := GetObjectProp(DataSet, 'ServerIndexDefs') as TIndexDefs;
        //Ensure IndexDefs are up-to-date:
        IndexDefs.Update;
        //for each indexdef in
        for IndexCounter := 0 to IndexDefs.Count - 1 do
        begin
          Index := IndexDefs[IndexCounter];
          // Only add index if it is based on at least one field
          if (Index.Fields <> '') then
          begin
            FANode := FOutputDoc.CreateElement('od:index');
            FXSDTableAppinfoNode.AppendChild(FANode);
            if Index.Name = '' then
            begin
              TDOMElement(FANode).SetAttribute('index-name',
                UTF8Decode('idx' + StringReplace(Index.Fields, ';',
                '_', [rfReplaceAll, rfIgnoreCase]) + IntToStr(Index.ID)));
              //Avoids risk for name collision by adding collection id.
            end
            else
            begin
              TDOMElement(FANode).SetAttribute('index-name', UTF8Decode(Index.Name));
            end;

            { TODO 6 -oAnyone -cShould have : Really should get a list of exported field names (which are IIRC user-selectable) because they can differ from dataset field names...
  This would involve building a mapping table + extraction/lookup functionality...
  First see if the index functionality works at all. }
            // Semicolon-delimited list of fields=> space separated list of fields
            TDOMElement(FANode).SetAttribute('index-key',
              StringReplace(Index.Fields, ';', ' ',
              [rfReplaceAll, rfIgnoreCase]));

            // Access XP XML format does not have support for descending indexes, but Access 2010 does, so we'll put it in and hope XP will ignore it:
            IndexDirection := '';
            DescendingIndex := ';' + Index.DescFields + ';';
            //So we can search on it properly
            IndexFieldsList := TStringList.Create;
            try
              IndexFieldsList.Delimiter := ';';
              IndexFieldsList.StrictDelimiter := True;
              IndexFieldsList.DelimitedText := Index.Fields;
              for FieldCounter := 0 to IndexFieldsList.Count - 1 do
              begin
                if AnsiPos(';' + IndexFieldsList[FieldCounter] + ';',
                  DescendingIndex) > 0 then
                begin
                  //Descending
                  IndexDirection := IndexDirection + 'desc '; //note trailing space
                end
                else
                begin
                  // Not a descending index, so it probably is ascending ;)
                  IndexDirection := IndexDirection + 'asc '; //note trailing space
                end;
              end;
            finally
              IndexFieldsList.Free;
            end;
            IndexDirection := Trim(IndexDirection); //Get rid of trailing space
            TDOMElement(FANode).SetAttribute('order', IndexDirection);

            if ixPrimary in Index.Options then
            begin
              TDOMElement(FANode).SetAttribute('primary', 'yes');
            end
            else
            begin
              TDOMElement(FANode).SetAttribute('primary', 'no');
            end;

            if ixUnique in Index.Options then
            begin
              TDOMElement(FANode).SetAttribute('unique', 'yes');
            end
            else
            begin
              TDOMElement(FANode).SetAttribute('unique', 'no');
            end;

            { TODO 9 -oAnyone -cNice to have : Find out what clustered means in the XML output, and implement that}
            TDOMElement(FANode).SetAttribute('clustered', 'no');
          end; //Index usable for export
        end; //end index iteration
      end; //RTTI Published indexes
    end; //XSD used
  end;

  procedure FieldMetadataAccessXP;
  var
    SimpleTypeNode: TDOMNode; //xs:simpletype node, child of ColumnNode
    RestrictionNode: TDOMNode; //xs:restriction node; contains datatype
  begin
    //Data types for each field:
    ColumnNode := FOutputDoc.CreateElement('xs:element');
    TDOMElement(ColumnNode).SetAttribute(
      'name', UTF8Decode(ExportFields.Fields[ItemCounter].ExportedName));
    FXSDTableDataTypesNode.AppendChild(ColumnNode);

    SimpleTypeNode := FOutputDoc.CreateElement('xs:simpleType');
    ColumnNode.AppendChild(SimpleTypeNode);

    RestrictionNode := FOutputDoc.CreateElement('xs:restriction');
    SimpleTypeNode.AppendChild(RestrictionNode);

    //Todo: we might have to tweak field conversion a bit.
    case ExportFields.Fields[ItemCounter].Field.DataType of
      ftArray: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftADT: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftAutoInc:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'autonumber');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'int');
        TDOMElement(ColumnNode).SetAttribute('od:autoUnique', 'yes');
        TDOMElement(ColumnNode).SetAttribute('od:nonNullable', 'yes');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:integer');
      end;

      ftBCD: //use same as float
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'double');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'float');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
      end;

      ftBoolean:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'yesno');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'bit');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('od:nonNullable', 'yes');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:byte');
      end;

      ftBlob:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftBytes:
        //Seems to be some type of blob field
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftCurrency:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'currency');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'money');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
      end;

      ftCursor: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftDataSet: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftDate:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:timeInstant');
      end;

      ftDateTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:timeInstant');
      end;

      ftDBaseOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftFixedChar: //pretend it's a string - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftFixedWideChar: //pretend it's a string - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftFloat:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'double');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'float');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
      end;

      ftFMTBcd: //pretend it's a float.
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'double');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'float');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
      end;

      ftFmtMemo: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftGraphic:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftGuid: //basically a 38 character fixed width string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'replicationid');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'uniqueidentifier');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '38');
        RestrictionNode.AppendChild(FANode);
      end;

      ftIDispatch: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftInteger: //might be jet integer/longinteger, and sql smallint/int
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'longinteger');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        //TDOMElement(RestrictionNode).SetAttribute('base', 'xs:integer'); //only for jet integer/sql smallint
      end;

      ftInterface: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftLargeint:
        //might be jet integer/longinteger, and sql smallint/int
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'longinteger');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        //TDOMElement(RestrictionNode).SetAttribute('base', 'xs:integer'); //only for jet integer/sql smallint
      end;

      ftMemo: //variable length string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftOraBlob:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftOraClob: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftParadoxOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftReference: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftSmallint:
        //might be jet integer/longinteger, and sql smallint/int
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'longinteger');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        //TDOMElement(RestrictionNode).SetAttribute('base', 'xs:integer'); //only for jet integer/sql smallint
      end;

      ftString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'text');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'nvarchar');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value',
          IntToStr(ExportFields.Fields[ItemCounter].Field.Size));
        RestrictionNode.AppendChild(FANode);
      end;

      ftTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:timeInstant');
      end;

      ftTimeStamp:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'datetime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:timeInstant');
      end;

      ftTypedBinary:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftVarBytes: //suppose it's some kind of blob type of thing: todo: find out
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'oleobject');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'image');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:binary');
        FANode := FOutputDoc.CreateElement('xs:encoding');
        TDOMElement(FANode).SetAttribute('value', 'base64');
        RestrictionNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '1476395008');
        //Pulled this out of an Access example. Don't know how this is calculated.
        RestrictionNode.AppendChild(FANode);
      end;

      ftVariant: //let's treat it like a memo.
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftWideMemo:
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;

      ftWideString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'text');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'nvarchar');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value',
          IntToStr(ExportFields.Fields[ItemCounter].Field.Size));
        RestrictionNode.AppendChild(FANode);
      end;

      ftWord: //might be jet integer/longinteger, and sql smallint/int
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'longinteger');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        //TDOMElement(RestrictionNode).SetAttribute('base', 'xs:integer'); //only for jet integer/sql smallint
      end;

      ftUnknown:
        //raise Exception.Create('Unknown datatype for dataset field while exporting.');
        //Treat like memo field
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //todo: Pulled this out of Access XP sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;
      else //Unknown data type, just treat it as a memo and hope for the best.
      begin
        TDOMElement(ColumnNode).SetAttribute('od:jetType', 'ntext');
        TDOMElement(ColumnNode).SetAttribute('od:sqlSType', 'memo');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');

        TDOMElement(RestrictionNode).SetAttribute('base', 'xs:string');

        FANode := FOutputDoc.CreateElement('xs:maxLength');
        TDOMElement(FANode).SetAttribute('value', '536870910');
        //Pulled this out of Access sample, don't know how this is calculated
        RestrictionNode.AppendChild(FANode);
      end;
    end; //case
  end; //Fill Function

  procedure FieldMetadataADONET;
  begin
    //Data types for each field:
    ColumnNode := FOutputDoc.CreateElement('xs:element');
    TDOMElement(ColumnNode).SetAttribute(
      'name', UTF8Decode(ExportFields.Fields[ItemCounter].ExportedName));
    FXSDTableDataTypesNode.AppendChild(ColumnNode);

    //Todo: we might have to tweak field conversion a bit.
    case ExportFields.Fields[ItemCounter].Field.DataType of
      ftArray: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftADT: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftAutoInc: //seems to be treated like a simple int
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftBCD: //use same as float
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftBoolean:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:boolean');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftBlob:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftBytes: //Seems to be some kind of blob field
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftCurrency: //map to decimal
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:decimal');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftCursor: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftDataSet: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftDate:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:dateTime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftDateTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:dateTime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftDBaseOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftFixedChar: //pretend it's a string - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftFixedWideChar: //pretend it's a string - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftFloat: //map to double, could possibly be mapped to float, too...
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftFMTBcd: //pretend it's a float.
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:double');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftFmtMemo: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftGraphic: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftGuid: //basically a 38 character fixed width string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
        TDOMElement(ColumnNode).SetAttribute('msdata:DataType',
          'System.Guid, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089');
        //Corresponds to ADO.Net 2.0 version. Should work in later versions
      end;

      ftIDispatch: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftInteger:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftInterface: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftLargeint:
        //Maybe this datatype is bigger than an xs:int? No, should still be mapped to integer I suppose
        //Integer is a derived datatype of decimal: see http://www.w3.org/TR/xmlschema-2/
        // so probably int is too, I hope...
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftMemo: //variable length string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftOraBlob: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftOraClob: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftParadoxOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftReference: //pretend it's a string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftSmallint:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:dateTime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftTimeStamp:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:dateTime');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftTypedBinary: //blob type of thing?
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftVarBytes: //suppose it's some kind of blob type of thing...
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:base64Binary');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftVariant: //let's treat it like a memo.
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftWideMemo:
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftWideString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftWord: //map to integer
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:int');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;

      ftUnknown:
        //raise Exception.Create('Unknown datatype for dataset field while exporting.');
        //Treat like memo field
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;
      else //Unknown data type, just treat it as a memo and hope for the best.
      begin
        TDOMElement(ColumnNode).SetAttribute('type', 'xs:string');
        TDOMElement(ColumnNode).SetAttribute('minOccurs', '0');
      end;
    end; //case
  end; //ADO.Net Fill Function

  procedure FieldMetadataClientDataset;
  begin
    //Data types for each field:
    ColumnNode := FOutputDoc.CreateElement('FIELD');
    TDOMElement(ColumnNode).SetAttribute(
      'attrname', UTF8Decode(ExportFields.Fields[ItemCounter].ExportedName));
    { TODO 8 -oAnyone -cNice to have : Implement support for required fields (required="true" attribute) }
    DelphiFieldsNode.AppendChild(ColumnNode);


    { TODO 7 -oAnyone -cNice to have : Todo: we might have to tweak field conversion; have mapped a lot of types to memo and binary. Please update }
    case ExportFields.Fields[ItemCounter].Field.DataType of
      ftArray: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftADT: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftAutoInc: //int and required, and todo:
        //support a line like this
        //<PARAMS AUTOINCVALUE="3" CHANGE_LOG="1 0 4 2 0 4"/>
        //as a sibling of the <FIELDS> list
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'i4');
        TDOMElement(ColumnNode).SetAttribute('required', 'true');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Autoinc');
      end;

      ftBCD: //use same as float??? or own bcd type? Don't know this one
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bcd');
      end;

      ftBoolean:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'boolean');
      end;

      ftBlob:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftBytes: //Seems to be some kind of blob field
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftCurrency:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'r8');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Money');
      end;

      ftCursor: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftDataSet: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftDate:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'date');
      end;

      ftDateTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'dateTime');
      end;

      ftDBaseOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftFixedChar: //pretend it's a memo - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftFixedWideChar: //pretend it's a memo - maybe fixed width?
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftFloat:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'r8');
      end;

      ftFMTBcd: //pretend it's a float.
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'r8');
      end;

      ftFmtMemo: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftGraphic: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftGuid: //basically a 38 character fixed width string
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'string');
        TDOMElement(ColumnNode).SetAttribute('WIDTH', '38');
      end;

      ftIDispatch: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftInteger:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'i4');
      end;

      ftInterface: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftLargeint:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'i8');
      end;

      ftMemo: //variable length string
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftOraBlob: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftOraClob: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftParadoxOle: //blob type of thing
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftReference: //pretend it's a memo
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftSmallint:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'i2');
      end;

      ftString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'string');
        TDOMElement(ColumnNode).SetAttribute('WIDTH',
          UTF8Decode(IntToStr(ExportFields.Fields[ItemCounter].Field.Size)));
      end;

      ftTime:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'time');
      end;

      ftTimeStamp:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'dateTime');
      end;

      ftTypedBinary: //blob type of thing?
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftVarBytes: //suppose it's some kind of blob type of thing...
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Binary');
      end;

      ftVariant: //let's treat it like a memo.
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftWideMemo:
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;

      ftWideString: //fixed length or at least max length string
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'string');
        TDOMElement(ColumnNode).SetAttribute('WIDTH',
          UTF8Decode(inttostr(ExportFields.Fields[ItemCounter].Field.Size)));
      end;

      ftWord: //map to integer
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'i8');
      end;

      ftUnknown:
        //raise Exception.Create('Unknown datatype for dataset field while exporting.');
        //Treat like memo field
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;
      else //Unknown data type, just treat it as a memo and hope for the best.
      begin
        TDOMElement(ColumnNode).SetAttribute('fieldtype', 'bin.hex');
        TDOMElement(ColumnNode).SetAttribute('SUBTYPE', 'Text');
      end;
    end; //case
  end; //Clientdataset Fill Function

begin
  // Root node, before any XSD or data:
  case FormatSettings.ExportFormat of
    AccessCompatible: FRootNode := FOutputDoc.CreateElement('root');
    ADONetCompatible: FRootNode := FOutputDoc.CreateElement('NewDataSet');
    DelphiClientDataset:
    begin
      FRootNode := FOutputDoc.CreateElement('DATAPACKET');
      TDOMElement(FRootNode).SetAttribute('Version', '2.0');
      //Todo: don't know if another version is necessary for other Delphi versions
    end;
    ExcelCompatible:
    begin
      FRootNode := FOutputDoc.CreateElement('Workbook');
      TDOMElement(FRootNode).SetAttribute(
        'xmlns', 'urn:schemas-microsoft-com:office:spreadsheet');
      TDOMElement(FRootNode).SetAttribute(
        'xmlns:ss', 'urn:schemas-microsoft-com:office:spreadsheet');
      // Add some style info for date/time fields that will be referered to by
      // the data cells

      ExcelStyles := FOutputDoc.CreateElement('Styles');
      FRootNode.AppendChild(ExcelStyles);

      StyleElement := FOutputDoc.CreateElement('Style');
      TDOMElement(StyleElement).SetAttribute('ss:ID', 's21');

      FANode := FOutputDoc.CreateElement('NumberFormat');
      TDOMElement(FANode).SetAttribute('ss:Format', 'General Date');
      StyleElement.AppendChild(FANode);
      ExcelStyles.AppendChild(StyleElement);

      StyleElement := FOutputDoc.CreateElement('Style');
      TDOMElement(StyleElement).SetAttribute('ss:ID', 's22');
      FANode := FOutputDoc.CreateElement('NumberFormat');
      TDOMElement(FANode).SetAttribute('ss:Format', 'Short Time');
      StyleElement.AppendChild(FANode);
      ExcelStyles.AppendChild(StyleElement);

      StyleElement := FOutputDoc.CreateElement('Style');
      TDOMElement(StyleElement).SetAttribute('ss:ID', 's23');
      FANode := FOutputDoc.CreateElement('NumberFormat');
      TDOMElement(FANode).SetAttribute('ss:Format', 'Short Date');
      StyleElement.AppendChild(FANode);
      ExcelStyles.AppendChild(StyleElement);
    end;
  end;
  FOutputDoc.AppendChild(FRootNode); //save root node

  if FXSDUsed then
  begin
    case FormatSettings.ExportFormat of
      AccessCompatible:
      begin
        {
        Access XP/2002 exports this line, which is not supported by Visual Studio 2002 (and probably
        ADO.NET. See:
        http://support.microsoft.com/kb/307422
        So one would use
        TDomElement(FRootNode).SetAttribute('xmlns:xs',
          'http://www.w3.org/2001/XMLSchema');
        However, Access XP chokes on importing the newer version, so leave it as is.
        }
        TDomElement(FRootNode).SetAttribute('xmlns:xs',
          UTF8Decode('http://www.w3.org/2000/10/XMLSchema'));
        TDOMElement(FRootNode).SetAttribute('xmlns:od',
          UTF8Decode('urn:schemas-microsoft-com:officedata'));
      end;
      else
      begin
        //Not necessary in ADO.Net, DelphiClientDataset, Excel
      end;
    end;//export format case
  end; //xsd used


  // Inline XSD:
  if FXSDUsed then
  begin
    case FormatSettings.ExportFormat of
      AccessCompatible:
      begin
        // Include XSD schema:
        FXSDSchemaNode := FOutputDoc.CreateElement('xs:schema');
        FRootNode.AppendChild(FXSDSchemaNode); //save schema node

        // Add table list. If we're exporting an FPC dataset, there
        // will only be one table.
        FXSDTableListNode := FOutputDoc.CreateElement('xs:element');
        TDOMElement(FXSDTableListNode).SetAttribute('name', UTF8Decode('dataroot'));
        FXSDSchemaNode.AppendChild(FXSDTableListNode);

        FANode := FOutputDoc.CreateElement('xs:complexType');
        FXSDTableListNode.AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:choice');
        TDOMElement(FANode).SetAttribute('maxOccurs', 'unbounded');
        FXSDTableListNode.ChildNodes.Item[0].AppendChild(FANode);

        FANode := FOutputDoc.CreateElement('xs:element');
        TDOMElement(FANode).SetAttribute('ref', UTF8Decode(FDatasetExportName));
        FXSDTableListNode.ChildNodes.Item[0].ChildNodes.Item[0].AppendChild(FANode);

        //Start table metadata/datatype list:
        FXSDTableNode := FOutputDoc.CreateElement('xs:element');
        TDOMElement(FXSDTableNode).SetAttribute('name', UTF8Decode(FDatasetExportName));
        FXSDSchemaNode.AppendChild(FXSDTableNode);

        //Add nodes for primary key/index info
        FXSDTableAnnotationNode := FOutputDoc.CreateElement('xs:annotation');
        FXSDTableNode.AppendChild(FXSDTableAnnotationNode);

        FXSDTableAppinfoNode := FOutputDoc.CreateElement('xs:appinfo');
        FXSDTableAnnotationNode.AppendChild(FXSDTableAppinfoNode);

        // Add node for column name and data type info
        FXSDTableDataColumnParent := FOutputDoc.CreateElement('xs:sequence');
        FXSDTableNode.ChildNodes.Item[0].AppendChild(FXSDTableDataColumnParent);
      end;
      ADONetCompatible:
      begin
        // Include XSD schema:
        FXSDSchemaNode := FOutputDoc.CreateElement('xs:schema');
        TDOMElement(FXSDSchemaNode).SetAttribute('id', UTF8Decode('NewDataSet'));
        TDOMElement(FXSDSchemaNode).SetAttribute('xmlns', UTF8Decode(''));
        TDOMElement(FXSDSchemaNode).SetAttribute('xmlns:xs',
          UTF8Decode('http://www.w3.org/2001/XMLSchema'));
        TDOMElement(FXSDSchemaNode).SetAttribute('xmlns:msdata',
          UTF8Decode('urn:schemas-microsoft-com:xml-msdata'));
        FRootNode.AppendChild(FXSDSchemaNode); //save schema node

        // Add table list. If we're exporting an FPC dataset, there
        // will only be one table.
        FANode := FOutputDoc.CreateElement('xs:element');
        TDOMElement(FANode).SetAttribute('name', UTF8Decode('NewDataSet'));
        TDOMElement(FANode).SetAttribute('msdata:IsDataSet', 'true');
        TDOMElement(FANode).SetAttribute('msdata:UseCurrentLocale', 'true');
        FXSDSchemaNode.AppendChild(FANode);

        FXSDTableListNode := FOutputDoc.CreateElement('xs:complexType');
        FANode.AppendChild(FXSDTableListNode);

        FANode := FOutputDoc.CreateElement('xs:choice');
        TDOMElement(FANode).SetAttribute('minOccurs', '0');
        TDOMElement(FANode).SetAttribute('maxOccurs', 'unbounded');
        FXSDTableListNode.AppendChild(FANode);

        //Start table metadata/datatype list:
        FXSDTableNode := FOutputDoc.CreateElement('xs:element');
        TDOMElement(FXSDTableNode).SetAttribute('name', UTF8Decode(FDatasetExportName));
        FXSDTableListNode.ChildNodes.Item[0].AppendChild(FXSDTableNode);
        FXSDTableDataColumnParent := FXSDTableNode;
        //In ADO.NET, no extra layers in between
      end;
    end; //export format case
  end; //XSD used

  case FormatSettings.ExportFormat of
    AccessCompatible:
    begin
      // Add data root node below root node, as a sibling to xsd schema:
      FTableDataParentNode := FOutputDoc.CreateElement(UTF8Decode('dataroot'));
      if (FormatSettings.CreateXSD = True) then
      begin
        TDOMElement(FTableDataParentNode).SetAttribute(
          'xmlns:xsi', 'http://www.w3.org/2000/10/XMLSchema-instance');
      end
      else
      begin
        { No XSD format apparently has a different namespace, but it isn't used further on,
        so might not be even necessary...}
        TDOMElement(FTableDataParentNode).SetAttribute(
          'xmlns:od', UTF8Decode('urn:schemas-microsoft-com:officedata'));
      end;
      FRootNode.AppendChild(FTableDataParentNode);
    end;
    ADONetCompatible:
    begin
      { In this mode, the table data starts directly, no dataroot present, so set this to the proper node so other nodes can be added }
      FTableDataParentNode := FRootNode;
    end;
    DelphiClientDataset:
    begin
      { Create a ROWDATA node under the root node, add all ROW nodes under that}
      FTableDataParentNode:=FOutputDoc.CreateElement(UTF8Decode('ROWDATA'));
      FRootNode.AppendChild(FTableDataParentNode);

    end;
    ExcelCompatible:
    begin
      // Add data root node below root node
      FANode := FOutputDoc.CreateElement('Worksheet');
      TDOMElement(FANode).SetAttribute(
        'ss:Name', UTF8Decode(FDatasetExportName));
      FRootNode.AppendChild(FANode);

      FTableDataParentNode := FOutputDoc.CreateElement('Table');
      FANode.AppendChild(FTableDataParentNode);
    end;
  end; //export format case

  if FXSDUsed then
  begin
    case FormatSettings.ExportFormat of
      AccessCompatible:
      begin
        FANode := FOutputDoc.CreateElement('xs:complexType');
        FXSDTableNode.AppendChild(FANode);

        FXSDTableDataTypesNode := FOutputDoc.CreateElement('xs:sequence');
        FANode.AppendChild(FXSDTableDataTypesNode);
      end;
      ADONetCompatible:
      begin
        FANode := FOutputDoc.CreateElement('xs:complexType');
        FXSDTableNode.AppendChild(FANode);

        FXSDTableDataTypesNode := FOutputDoc.CreateElement('xs:sequence');
        FANode.AppendChild(FXSDTableDataTypesNode);
      end;
    end; //export format case

    for ItemCounter := 0 to ExportFields.Count - 1 do
    begin
      case FormatSettings.ExportFormat of
        AccessCompatible:
        begin
          FieldMetadataAccessXP; //Only do this if XSD is specified
        end;
        ADONetCompatible:
        begin
          FieldMetadataADONET; //Only do this if XSD is specified
        end;
      end; //export format case
    end; //column iteration
  end; //xsd inline

  //Metadata/indexes
  case FormatSettings.ExportFormat of
    AccessCompatible:
    begin
      ExportIndexesAccessXP;
    end;
    ADONetCompatible:
    begin
      { ADO.NET (Framework 2) export doesn't include indexes.}
    end;
    DelphiClientDataset:
    begin
      //First some metadata stuff
      DelphiMetadataNode := FOutputDoc.CreateElement('METADATA');
      FRootNode.AppendChild(DelphiMetadataNode);

      DelphiFieldsNode := FOutputDoc.CreateElement('FIELDS');
      DelphiMetadataNode.AppendChild(DelphiFieldsNode);
      for ItemCounter := 0 to ExportFields.Count - 1 do
      begin
        FieldMetadataClientDataset; //Always do this
      end;
    end;
  end; //export format case
end;

procedure TCustomXMLXSDExporter.DoDataFooter;

begin
  inherited DoDataFooter;
end;

procedure TCustomXMLXSDExporter.DoDataRowEnd;

begin
  //Close off FDataRowNode by adding it to TableData node
  FTableDataParentNode.AppendChild(FRowDataNode);
end;

procedure TCustomXMLXSDExporter.Exportfield(EF: Texportfielditem);
begin
  case FormatSettings.ExportFormat of
    DelphiClientDataset: ExportFieldDataClientDataset(EF);
    ExcelCompatible: ExportFieldDataExcel(EF);
    else
      ExportFieldData(EF);
  end;
end;

{ TXMLXSDFormatSettings }

procedure TXMLXSDFormatSettings.Assign(Source: TPersistent);

var
  XS: TXMLXSDFormatSettings;

begin
  // Default settings:
  { TODO 4 -oAnyone -cShould have : Is this the proper way of assigning default values to settings? }
  FExportFormat := ExcelCompatible;
  { It's a toss-up what the data format should be.
  I suppose people are more likely to have Excel installed & are more likely to be more interested in Excel import compared to the other options.
  }
  {
  Assume times stored in databases are set to local time.
  This is really application-dependent. Desktop database apps will probably store in local time.
  Bigger international server environments will probably store UTC time to avoid corruption.
  If available, we should use database field timezone information.
  }

  CreateXSD := True;
  DecimalSeparator := char(''); //Don't override decimal separator by default

  if Source is TXMLXSDFormatSettings then
  begin
    Xs := TXMLXSDFormatSettings(Source);
    FExportFormat := XS.FExportFormat;
    CreateXSD := XS.CreateXSD;
  end;
  inherited Assign(Source);
end;

procedure RegisterXMLXSDExportFormat;

begin
  ExportFormats.RegisterExportFormat(SXMLXSD, SXMLXSDDescription,
    SXMLXSDExtensions, TXMLXSDExporter);
end;

procedure UnRegisterXMLXSDExportFormat;

begin
  ExportFormats.UnregisterExportFormat(SXMLXSD);
end;

end.

