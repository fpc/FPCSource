{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c)  1999 Sebastian Guenther, sguenther@gmx.de

    Implementation of TXMLConfig class
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TXMLConfig enables applications to use XML files for storing their
  configuration data
}

{$MODE objfpc}
{$H+}

unit XMLCfg;

interface
uses DOM, XMLRead, XMLWrite;

type

  {"APath" is the path and name of a value: A XML configuration file is
   hierarchical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  TXMLConfig = class
  protected
    doc: TXMLDocument;
    FileName: String;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure Flush;    // Writes the XML file
    function  GetValue(const APath, ADefault: String): String;
    function  GetValue(const APath: String; ADefault: Integer): Integer;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean;
    procedure SetValue(const APath, AValue: String);
    procedure SetValue(const APath: String; AValue: Integer);
    procedure SetValue(const APath: String; AValue: Boolean);
  end;


// ===================================================================

implementation

uses SysUtils;


constructor TXMLConfig.Create(const AFileName: String);
var
  f: File;
  cfg: TDOMElement;
begin
  FileName := AFileName;
  Assign(f, AFileName);
  {$I-}
  Reset(f, 1);
  {$I+}
  if IOResult = 0 then begin
    try
      ReadXMLFile(doc, f);
    except
      on e: EXMLReadError do
        WriteLn(StdErr, 'Warning: XML config parsing error: ', e.Message);
    end;
    Close(f);
  end;

  if doc = nil then
    doc := TXMLDocument.Create;

  cfg :=TDOMElement(doc.FindNode('CONFIG'));
  if cfg = nil then begin
    cfg := doc.CreateElement('CONFIG');
    doc.AppendChild(cfg);
  end;
  doc.SetDocumentElement(cfg);
end;

destructor TXMLConfig.Destroy;
begin
  Flush;
  inherited Destroy;
end;

procedure TXMLConfig.Flush;
var
  f: Text;
begin
  Assign(f, FileName);
  Rewrite(f);
  WriteXMLFile(doc, f);
  Close(f);
end;

function TXMLConfig.GetValue(const APath, ADefault: String): String;
var
  node, subnode, attr: TDOMNode;
  i: Integer;
  name, path: String;
begin
  node := doc.DocumentElement;
  path := APath;
  while True do begin
    i := Pos('/', path);
    if i = 0 then break;
    name := Copy(path, 1, i - 1);
    path := Copy(path, i + 1, Length(path));
    subnode := node.FindNode(name);
    if subnode = nil then begin
      Result := ADefault;
      exit;
    end;
    node := subnode;
  end;
  attr := node.Attributes.GetNamedItem(path);
  if attr = nil then
    Result := ADefault
  else
    Result := attr.NodeValue;
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Integer): Integer;
begin
  Result := StrToInt(GetValue(APath, IntToStr(ADefault)));
end;

function TXMLConfig.GetValue(const APath: String; ADefault: Boolean): Boolean;
var
  s: String;
begin
  if ADefault then
    s := 'True'
  else
    s := 'False';

  s := GetValue(APath, s);

  if UpperCase(s) = 'TRUE' then
    Result := True
  else if UpperCase(s) = 'FALSE' then
    Result := False
  else
    Result := ADefault;
end;

procedure TXMLConfig.SetValue(const APath, AValue: String);
var
  node, subnode, attr: TDOMNode;
  i: Integer;
  name, path: String;
begin
  node := doc.DocumentElement;
  path := APath;
  while True do begin
    i := Pos('/', path);
    if i = 0 then break;
    name := Copy(path, 1, i - 1);
    path := Copy(path, i + 1, Length(path));
    subnode := node.FindNode(name);
    if subnode = nil then begin
      subnode := doc.CreateElement(name);
      node.AppendChild(subnode);
    end;
    node := subnode;
  end;
  attr := node.Attributes.GetNamedItem(path);
  if attr = nil then begin
    attr := doc.CreateAttribute(path);
    node.Attributes.SetNamedItem(attr);
  end;
  attr.NodeValue := AValue;
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TXMLConfig.SetValue(const APath: String; AValue: Boolean);
begin
  if AValue then
    SetValue(APath, 'True')
  else
    SetValue(APath, 'False');
end;


end.


{
  $Log$
  Revision 1.6  2000-01-06 01:20:37  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:11  peter
    * moved to packages dir

  Revision 1.4  1999/12/22 13:38:01  sg
  * Lots of cosmetic changes (strings -> const AnsiStrings etc.)

  Revision 1.3  1999/07/25 16:24:13  michael
  + Fixes from Sebastiam Guenther - more error-proof

  Revision 1.2  1999/07/09 21:05:50  michael
  + fixes from Guenther Sebastian

  Revision 1.1  1999/07/09 08:35:09  michael
  + Initial implementation by Sebastian Guenther

}
