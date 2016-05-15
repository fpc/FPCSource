{
    This file is part of the Free Component Library

    Implementation of TXMLConfig class
    Copyright (c) 1999 - 2005 by Sebastian Guenther, sg@freepascal.org
    Modified in 2007 by Sergei Gorelkin, sergei_gorelkin@mail.ru

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

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ENDIF}

unit XMLConf;

interface

uses
  SysUtils, Classes, DOM, XMLRead, XMLWrite;

resourcestring
  SWrongRootName = 'XML file has wrong root element name: expected "%s" but was "%s"';

type
  EXMLConfigError = class(Exception);
  TPathFlags = set of (pfHasValue, pfWriteAccess);

  {"APath" is the path and name of a value: A XML configuration file is
   hierachical. "/" is the path delimiter, the part after the last "/"
   is the name of the value. The path components will be mapped to XML
   elements, the name will be an element attribute.}

  { TXMLConfig }

  TXMLConfig = class(TComponent)
  private
    FFilename: String;
    FStartEmpty: Boolean;
    FRootName: DOMString;
    FDummy: DOMString;
    FPathStack: array of DOMString;
    FPathCount: Integer;
    FPathDirty: Boolean;
    FElement: TDOMElement;
    procedure DoSetFilename(const AFilename: String; ForceReload: Boolean);
    procedure SetFilename(const AFilename: String);
    procedure SetStartEmpty(AValue: Boolean);
    procedure SetRootName(const AValue: DOMString);
    function DoFindNode(const APath: DOMString; var Ident: DOMString;
      Flags: TPathFlags): TDomElement;
    function FindNode(const APath: DOMString; out Ident: DOMString;
      Flags: TPathFlags): TDOMElement;
  protected
    Doc: TXMLDocument;
    FModified: Boolean;
    FReadOnly: Boolean;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Flush;    // Writes the XML file
    procedure OpenKey(const aPath: DOMString);
    procedure CloseKey;
    procedure ResetKey;
    procedure SaveToFile(Const AFileName: string);
    procedure SaveToStream(S : TStream);
    procedure LoadFromFile(Const AFileName: string);
    procedure LoadFromStream(S : TStream);

    function  GetValue(const APath: DOMString; const ADefault: DOMString): DOMString; overload;
    function  GetValue(const APath: DOMString; ADefault: Integer): Integer; overload;
    function  GetValue(const APath: DOMString; ADefault: Boolean): Boolean; overload;
    procedure SetValue(const APath: DOMString; const AValue: DOMString); overload;
    procedure SetValue(const APath: DOMString; AValue: Integer); overload;
    procedure SetValue(const APath: DOMString; AValue: Boolean); overload;

    procedure SetDeleteValue(const APath: DOMString; const AValue, DefValue: DOMString); overload;
    procedure SetDeleteValue(const APath: DOMString; AValue, DefValue: Integer); overload;
    procedure SetDeleteValue(const APath: DOMString; AValue, DefValue: Boolean); overload;

    procedure DeletePath(const APath: DOMString);
    procedure DeleteValue(const APath: DOMString);
    property Modified: Boolean read FModified;
  published
    property Filename: String read FFilename write SetFilename;
    property StartEmpty: Boolean read FStartEmpty write SetStartEmpty;
    property RootName: DOMString read FRootName write SetRootName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;


// ===================================================================

implementation

constructor TXMLConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRootName := 'CONFIG';
  Doc := TXMLDocument.Create;
  Doc.AppendChild(Doc.CreateElement(FRootName));
end;

destructor TXMLConfig.Destroy;
begin
  if Assigned(Doc) then
  begin
    Flush;
    Doc.Free;
  end;
  inherited Destroy;
end;

procedure TXMLConfig.Clear;
begin
  Doc.ReplaceChild(Doc.CreateElement(FRootName), Doc.DocumentElement);
end;

procedure TXMLConfig.Flush;
begin
  if Modified and not FReadOnly then
    if (FFileName<>'') then
      SaveToFile(FFilename)
end;

procedure TXMLConfig.SaveToFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(F);
    FFileName:=AFileName;
  finally
    F.Free;
  end;
end;

procedure TXMLConfig.SaveToStream(S: TStream);
begin
  WriteXMLFile(Doc,S);
  FModified := False;
end;

procedure TXMLConfig.LoadFromFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenread or fmShareDenyWrite);
  try
    ReadXMLFile(Doc, AFilename);
    FFileName:=AFileName;
  finally
    F.Free;
  end;
end;

procedure TXMLConfig.LoadFromStream(S: TStream);
begin
  ReadXMLFile(Doc,S);
  FModified := False;
  if (Doc.DocumentElement.NodeName<>FRootName) then
    raise EXMLConfigError.CreateFmt(SWrongRootName,[FRootName,Doc.DocumentElement.NodeName]);
end;

function TXMLConfig.GetValue(const APath: DOMString; const ADefault: DOMString): DOMString;
var
  Node: TDOMElement;
  Attr: TDOMAttr;
  Ident: DOMString;
begin
  Result := ADefault;

  Node := FindNode(APath, Ident, [pfHasValue]);
  if Assigned(Node) then
  begin
    Attr := Node.GetAttributeNode(Ident);
    if Assigned(Attr) then
      Result := Attr.NodeValue;
  end;
end;

function TXMLConfig.GetValue(const APath: DOMString; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(GetValue(APath, ''),ADefault);
end;

function TXMLConfig.GetValue(const APath: DOMString; ADefault: Boolean): Boolean;
var
  s: DOMString;
begin
  s := GetValue(APath, '');

  if WideSameText(s, 'TRUE') then
    Result := True
  else if WideSameText(s, 'FALSE') then
    Result := False
  else
    Result := ADefault;
end;

procedure TXMLConfig.SetValue(const APath: DOMString; const AValue: DOMString);
var
  Node: TDOMElement;
  Attr: TDOMAttr;
  Ident: DOMString;
begin
  Node := FindNode(APath, Ident, [pfHasValue, pfWriteAccess]);

  Attr := Node.GetAttributeNode(Ident);
  if (Attr = nil) or (Attr.NodeValue <> AValue) then
  begin
    Node[Ident] := AValue;
    FModified := True;
  end;
end;

procedure TXMLConfig.SetDeleteValue(const APath: DOMString; const AValue, DefValue: DOMString);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TXMLConfig.SetValue(const APath: DOMString; AValue: Integer);
begin
  SetValue(APath, IntToStr(AValue));
end;

procedure TXMLConfig.SetDeleteValue(const APath: DOMString; AValue,
  DefValue: Integer);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TXMLConfig.SetValue(const APath: DOMString; AValue: Boolean);
begin
  if AValue then
    SetValue(APath, 'True')
  else
    SetValue(APath, 'False');
end;

procedure TXMLConfig.SetDeleteValue(const APath: DOMString; AValue,
  DefValue: Boolean);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TXMLConfig.DeletePath(const APath: DOMString);
var
  Node: TDomNode;
  Ident: DOMString;
begin
  Node := FindNode(APath, Ident, []);
  if Assigned(Node) and Assigned(Node.ParentNode) then
  begin
    Node.ParentNode.RemoveChild(Node);
    FPathDirty := True;
    FElement := nil;
    FModified := True;
  end;
end;

procedure TXMLConfig.DeleteValue(const APath: DOMString);
var
  Node: TDOMElement;
  Ident: DOMString;
  Parent: TDOMNode;
begin
  Node := FindNode(APath, Ident, [pfHasValue]);
  if Assigned(Node) then
  begin
    if Assigned(Node.GetAttributeNode(Ident)) then
    begin
      Node.RemoveAttribute(Ident);
      FModified := True;
    end;
    while (Node.FirstChild=nil) and Assigned(Node.ParentNode)
      and Assigned(Node.ParentNode.ParentNode) do
    begin
      if Node.HasAttributes then
        Break;
      Parent := Node.ParentNode;
      Parent.RemoveChild(Node);
      Node := TDOMElement(Parent);
      FPathDirty := True;
      FElement := nil;
      FModified := True;
    end;
  end;
end;

procedure TXMLConfig.Loaded;
begin
  inherited Loaded;
  if Length(Filename) > 0 then
    DoSetFilename(Filename,True);              // Load the XML config file
end;

// TODO: copied from dom.pp, make public there and delete here
function CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
var i: integer;
begin
  Result:=l1-l2;
  i:=0;
  while (i<l1) and (Result=0) do begin
    Result:=ord(s1[i])-ord(s2[i]);
    inc(i);
  end;
end;

function TXMLConfig.FindNode(const APath: DOMString; out Ident: DOMString;
  Flags: TPathFlags): TDOMElement;
var
  I: Integer;
begin
  if FPathDirty then
  begin
    for I := 0 to FPathCount-1 do
      FElement := DoFindNode(FPathStack[I], FDummy, Flags - [pfHasValue]);
    if Assigned(FElement) then FPathDirty := False;
  end;
  Result := DoFindNode(APath, Ident, Flags);
end;

function TXMLConfig.DoFindNode(const APath: DOMString; var Ident: DOMString;
  Flags: TPathFlags): TDomElement;
var
  StartPos, EndPos: integer;
  PathLen: integer;
  Child: TDOMNode;
begin
  if Assigned(FElement) and (Length(APath) > 0) and (APath[1] <> '/') then
    Result := FElement
  else
    Result := Doc.DocumentElement;

  PathLen := Length(APath);
  StartPos := 1;
  if APath[StartPos] = '/' then Inc(StartPos);
  while Assigned(Result) do
  begin
    EndPos := StartPos;
    while (EndPos <= PathLen) and (APath[EndPos] <> '/') do
      Inc(EndPos);
    if (EndPos > PathLen) and (pfHasValue in Flags) then
    begin
      SetString(Ident, PWideChar(@APath[StartPos]), PathLen-StartPos+1);
      exit;
    end;
    if EndPos = StartPos then
      break;
    Child := Result.FirstChild;
    while Assigned(Child) and not ((Child.NodeType = ELEMENT_NODE)
      and (0 = CompareDOMStrings(DOMPChar(TDOMElement(Child).TagName), @APath[StartPos],
                                 Length(TDOMElement(Child).TagName), EndPos-StartPos))) do
        Child := Child.NextSibling;
    if (Child = nil) and (pfWriteAccess in Flags) then
    begin
      Child := Doc.CreateElementBuf(@APath[StartPos], EndPos-StartPos);
      Result.AppendChild(Child);
    end;
    Result := TDOMElement(Child);
    StartPos := EndPos + 1;
    if StartPos > PathLen then
      exit;
  end;
  Result := nil;
end;

procedure TXMLConfig.DoSetFilename(const AFilename: String; ForceReload: Boolean);
begin
  if (not ForceReload) and (FFilename = AFilename) then
    exit;
    
  Flush;
  FreeAndNil(Doc);
  if csLoading in ComponentState then
    exit;
  if FileExists(AFilename) and not FStartEmpty then
    LoadFromFile(AFilename)
  else if not Assigned(Doc) then
    begin
    FFileName:=AFileName;
    Doc := TXMLDocument.Create;
    Doc.AppendChild(Doc.CreateElement(FRootName))
    end;
end;

procedure TXMLConfig.SetFilename(const AFilename: String);
begin
  DoSetFilename(AFilename, False);
end;

procedure TXMLConfig.SetRootName(const AValue: DOMString);
var
  Cfg, Root: TDOMElement;
begin
  if AValue <> FRootName then
  begin
    FRootName := AValue;
    Root := Doc.DocumentElement;
    Cfg := Doc.CreateElement(AValue);
    while Assigned(Root.FirstChild) do
      Cfg.AppendChild(Root.FirstChild);
    Doc.ReplaceChild(Cfg, Root);
    FModified := True;
  end;
end;

procedure TXMLConfig.SetStartEmpty(AValue: Boolean);
begin
  if AValue <> StartEmpty then
  begin
    FStartEmpty := AValue;
    if (not AValue) and not Modified then
      DoSetFilename(Filename, True);
  end;
end;

procedure TXMLConfig.CloseKey;
begin
  if FPathCount > 0 then
  begin
    FPathStack[FPathCount-1] := '';
    Dec(FPathCount);
    FElement := nil;
    FPathDirty := True;
  end;
end;

procedure TXMLConfig.OpenKey(const aPath: DOMString);
begin
  if aPath <> '' then
  begin
    if FPathCount >= Length(FPathStack) then
      SetLength(FPathStack, FPathCount + 5);

    FPathStack[FPathCount] := aPath;
    Inc(FPathCount);
    FElement := nil;
    FPathDirty := True;
  end;
end;

procedure TXMLConfig.ResetKey;
var
  I: Integer;
begin
  for I := Length(FPathStack)-1 downto 0 do
    FPathStack[I] := '';
  FElement := nil;    
  FPathDirty := False;
  FPathCount := 0;
end;






end.
