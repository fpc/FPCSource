{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements creating a XSD file from SDO metadata

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit xsd_generator;

interface

uses
  Classes, SysUtils, TypInfo,
  {$IFNDEF FPC}xmldom, sdo_win_xml,{$ELSE}DOM, sdo_fpc_xml,{$ENDIF}
  sdo;

type

  TGeneratorOption = ( xgoIgnorembeddedArray );
  TGeneratorOptions = set of TGeneratorOption;

  EXsdGeneratorException = class(Exception) end;

  IGenerator = interface
    ['{F69523B3-A6FF-4BFB-9ACB-D4B9F32DBCA9}']
    procedure Execute(
      ASymTable   : ISDODataObject;
      AModuleName : string
    );
  end;

  IXsdGenerator = interface(IGenerator)
    ['{FBFF92BC-B72B-4B85-8D16-379F9E548DDB}']
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;
  end;

  { TCustomXsdGenerator }

  TCustomXsdGenerator = class(
    TInterfacedObject,
    IInterface,
    IGenerator,
    IXsdGenerator
  )
  private
    FDocument : TDOMDocument;
    FOptions: TGeneratorOptions;
    FShortNames : TStrings;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;virtual;abstract;
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;

    procedure GenerateEnum(AContainer, ASymbol : ISDODataObject);
    procedure GenerateComplex(AContainer, ASymbol : ISDODataObject);
    procedure Execute(
      ASymTable   : ISDODataObject;
      AModuleName : string
    );

    procedure Prepare(
      ASymTable,
      AModule   : ISDODataObject
    );virtual;
    property Document : TDOMDocument read FDocument;
    property Options : TGeneratorOptions read FOptions;
  public
    constructor Create(const ADocument : TDOMDocument);overload;
    constructor Create(
      const ADocument : TDOMDocument;
      const AOptions : TGeneratorOptions
    );overload;
    destructor Destroy();override;
  end;

  { TXsdGenerator }

  TXsdGenerator = class(TCustomXsdGenerator)
  private
    FSchemaNode : TDOMElement;
  protected
    function GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;override;
    procedure Prepare(ASymTable, AModule : ISDODataObject);override;
  end;

  function GetNameSpaceShortName(
    const ANameSpace    : string;
          ADocument : TDOMDocument;
    const APreferedList : TStrings
  ):string;

  function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;{$IFDEF USE_INLINE}inline;{$ENDIF}

implementation
uses
  sdo_xsdintf, xsd_consts, Contnrs, StrUtils, sdo_types, sdo_parserutils;

function FindAttributeByValueInNode(
  const AAttValue        : string;
  const ANode            : TDOMNode;
  out   AResAtt          : string;
  const AStartIndex      : Integer = 0;
  const AStartingWith    : string = ''
):boolean;
var
  i,c : Integer;
  b : Boolean;
begin
  AResAtt := '';
  if Assigned(ANode) and Assigned(ANode.Attributes) then begin
    b := ( Length(AStartingWith) = 0);
    c := Pred(ANode.Attributes.Length);
//    if ( AStartIndex >= 0 ) then
  //    i := AStartIndex;
    for i := AStartIndex to c do begin
      if AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) and
         ( b or ( Pos(AStartingWith,ANode.Attributes.Item[i].NodeName) = 1 ))
      then begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function GetNameSpaceShortName(
  const ANameSpace    : string;
        ADocument : TDOMDocument;
  const APreferedList : TStrings
):string;
begin
  if FindAttributeByValueInNode(ANameSpace,ADocument.DocumentElement,Result,0, s_xmlns) then begin
    Result := Copy(Result,Length(s_xmlns+':')+1,MaxInt);
  end else begin
    if ( APreferedList <> nil ) then
      Result := Trim(APreferedList.Values[ANameSpace]);
    if ( Result = '' ) then
      Result := Format('ns%d',[GetNodeListCount(ADocument.DocumentElement.Attributes)]) ;
    ADocument.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,Result]),ANameSpace);
  end;
end;

function CreateElement(const ANodeName : DOMString; AParent : TDOMNode; ADoc : TDOMDocument):TDOMElement;//inline;
begin
  Result := ADoc.CreateElement(ANodeName);
  AParent.AppendChild(Result);
end;

{ TCustomXsdGenerator }

procedure TCustomXsdGenerator.Execute(
  ASymTable   : ISDODataObject;
  AModuleName : string
);
var
  i : Integer;
  locModule : ISDODataObject;
  locList : ISDODataObjectList;
  locElement : ISDODataObject;
begin
  if ( ASymTable = nil ) then
    raise EXsdGeneratorException.Create('Invalid symbol table.');
  locModule := FindModule(ASymTable,AModuleName);
  if ( locModule = nil ) then
    raise EXsdGeneratorException.CreateFmt('Unable to find module : "%s".',[AModuleName]);
  Prepare(ASymTable,locModule);
  locList := locModule.getList(s_Type);
  for i := 0 to locList.size() - 1 do begin
    locElement := locList.getDataObject(i);
    if locElement.getBoolean(s_IsComplex) then
      GenerateComplex(ASymTable,locElement)
    else if (locElement.getList(s_EnumValue).size() > 0) then
      GenerateEnum(ASymTable,locElement);
  end
end;

procedure TCustomXsdGenerator.Prepare(
  ASymTable,
  AModule   : ISDODataObject
);
begin

end;

constructor TCustomXsdGenerator.Create(const ADocument : TDOMDocument);
begin
  Create(ADocument,[]);
end;

constructor TCustomXsdGenerator.Create(
  const ADocument: TDOMDocument;
  const AOptions: TGeneratorOptions
);
var
  sl : TStringList;
begin
  if ( ADocument = nil ) then
    raise EXsdGeneratorException.Create('Invalid document.');
  FDocument := ADocument;
  FOptions := AOptions;
  FShortNames := TStringList.Create();
  sl := TStringList(FShortNames);
  //sl.Sorted := True;
  sl.Duplicates := dupIgnore;
end;

procedure TCustomXsdGenerator.SetPreferedShortNames(const ALongName, AShortName: string);
begin
  FShortNames.Values[ALongName] := AShortName;
end;

function TCustomXsdGenerator.GetPreferedShortNames() : TStrings;
begin
  Result := FShortNames;
end;

destructor TCustomXsdGenerator.Destroy();
begin
  FreeAndNil(FShortNames);
  inherited;
end;

procedure TCustomXsdGenerator.GenerateEnum(AContainer, ASymbol : ISDODataObject);
var
  typItm : ISDODataObject;
  valueList : ISDODataObjectList;
  ns_shortName, s : string;
  defSchemaNode, resNode, restrictNode : TDOMElement;
  i : Integer;
  unitExternalName : string;
{$IFDEF SDO_HANDLE_DOC}
  ls : TStrings;
{$ENDIF SDO_HANDLE_DOC}
begin
  if (ASymbol = nil) then
    Exit;
  typItm := ASymbol;
  unitExternalName := GetTypeNameSpace(typItm);
  if FindAttributeByValueInNode(unitExternalName,Document.DocumentElement,ns_shortName) then begin
    ns_shortName := Copy(ns_shortName,Length(s_xmlns+':')+1,MaxInt);
  end else begin
    ns_shortName := Format('ns%d',[GetNodeListCount(Document.DocumentElement.Attributes)]) ;
    Document.DocumentElement.SetAttribute(Format('%s:%s',[s_xmlns,ns_shortName]),unitExternalName);
  end;
  defSchemaNode := GetSchemaNode(Document) as TDOMElement;

  s := Format('%s:%s',[s_xs_short,s_simpleType]);
  resNode := CreateElement(s,defSchemaNode,Document);
  resNode.SetAttribute(s_name, typItm.getString(sdo_xsdintf.s_Name)) ;
{$IFDEF SDO_HANDLE_DOC}
  ls := AContainer.Properties.FindList(typItm);
  if ( ls <> nil ) then begin
    i := ls.IndexOfName(s_documentation);
    if ( i >= 0 ) then
      GenerateDocumentation(resNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
  end;
{$ENDIF SDO_HANDLE_DOC}

  s := Format('%s:%s',[s_xs_short,s_restriction]);
  restrictNode := CreateElement(s,resNode,Document);
  restrictNode.SetAttribute(s_base,Format('%s:%s',[s_xs_short,'string'])) ;
  valueList := typItm.getList(s_EnumValue);
  for i := 0 to (valueList.size() - 1) do begin
    s := Format('%s:%s',[s_xs_short,s_enumeration]);
    CreateElement(s,restrictNode,Document).SetAttribute(
      s_value,
      valueList.getString(i)
    );
  end;
end;

type TTypeCategory = ( tcComplexContent, tcSimpleContent );
procedure TCustomXsdGenerator.GenerateComplex(AContainer, ASymbol: ISDODataObject);

  function TypeHasSequence(const AClassType : ISDODataObject; const ACategory : TTypeCategory) : Boolean;
  var
    kc, k : PtrInt;
    pl : ISDODataObjectList;
    p : ISDODataObject;
  begin
    Result := False;
    pl := AClassType.getList(s_Property);
    kc := pl.size();
    if (kc > 0) then begin
      for k := 0 to kc - 1 do begin
        p := pl.getDataObject(k);
        if not p.getBoolean(s_IsAttribute) then begin
          if ( ACategory = tcSimpleContent ) then begin
            raise EXsdGeneratorException.CreateFmt(
                    'Invalid type definition, a simple type cannot have "not attribute" properties : "%s"',
                    [AClassType.getString(sdo_xsdintf.s_Name)]
                  );
          end;
          Result := True;
        end;
      end;
    end;
  end;

  procedure ProcessPropertyExtendedMetadata(const AProp : ISDODataObject; const APropNode : TDOMElement);
  var
    ls : ISDODataObjectList;
    tagObj : ISDODataObject;
    line, ns, ns_short, localName, attName, attValue : TSDOString;
    k, q : PtrInt;
  begin
    ls := AProp.getList(s_Tag);
    if (ls.size() > 0) then begin
      for k := 0 to ls.size() - 1 do begin
        tagObj := ls.getDataObject(k);
        line := tagObj.getString(sdo_xsdintf.s_Name);
        q := Pos('#',line);
        if ( q > 0 ) then begin
          ns := Copy(line,1,Pred(q));
          localName := Copy(line,Succ(q),MaxInt);
          ns_short := GetNameSpaceShortName(ns,Document,GetPreferedShortNames());
          attName := Format('%s:%s',[ns_short,localName]);
          line := tagObj.getString(sdo_xsdintf.s_Value);
          q := Pos('#',line);
          if ( q > 0 ) then begin
            ns := Copy(line,1,Pred(q));
            localName := Copy(line,Succ(q),MaxInt);
            ns_short := GetNameSpaceShortName(ns,Document,GetPreferedShortNames());
            attValue := Format('%s:%s',[ns_short,localName]);
          end else begin
            attValue := line;
          end;
          APropNode.SetAttribute(attName,attValue);
        end;
      end;
    end;
  end;

  procedure ProcessXsdAny(const AContentNode : TDOMElement; const AXsdInfo : string);
  var
    xsdAnyNode : TDOMElement;
    ss : string;
    locLS : TStringList;
  begin
    xsdAnyNode := CreateElement(Format('%s:%s',[s_xs_short,s_any]),AContentNode,Document);
    locLS := TStringList.Create();
    try
      locLS.Delimiter := ';';
      locLS.DelimitedText := AXsdInfo;
      ss := locLS.Values[s_processContents];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_processContents,ss);
      ss := locLS.Values[s_minOccurs];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_minOccurs,ss);
      ss := locLS.Values[s_maxOccurs];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_maxOccurs,ss);
    finally
      locLS.Free();
    end;
  end;

  procedure ProcessXsdAnyAttribute(const AContentNode : TDOMElement; const AXsdInfo : string);
  var
    xsdAnyNode : TDOMElement;
    ss : string;
    locLS : TStringList;
  begin
    xsdAnyNode := CreateElement(Format('%s:%s',[s_xs_short,s_anyAttribute]),AContentNode,Document);
    locLS := TStringList.Create();
    try
      locLS.Delimiter := ';';
      locLS.DelimitedText := AXsdInfo;
      ss := locLS.Values[s_processContents];
      if not IsStrEmpty(ss) then
        xsdAnyNode.SetAttribute(s_processContents,ss);
    finally
      locLS.Free();
    end;
  end;

var
  cplxNode, sqcNode, derivationNode, defSchemaNode : TDOMElement;

  procedure ProcessProperty(const AProp : ISDODataObject);
  var
    p : ISDODataObject;
    s : string;
    propNode : TDOMElement;
    propTypItm, propItmUltimeType : ISDODataObject;
    prop_ns_shortName : string;
  begin
    p := AProp;
    if p.getBoolean(sdo_xsdintf.s_IsAttribute) then begin
      s := Format('%s:%s',[s_xs_short,s_attribute]);
      if Assigned(derivationNode) then
        propNode := CreateElement(s,derivationNode,Document)
      else
        propNode := CreateElement(s,cplxNode,Document);
    end else begin
      s := Format('%s:%s',[s_xs_short,s_element]);
      propNode := CreateElement(s,sqcNode,Document);
    end;
    propNode.SetAttribute(s_name,p.getString(sdo_xsdintf.s_Name));
    propTypItm := p.getDataObject(s_DataType);
    if Assigned(propTypItm) then begin
      if propTypItm.getBoolean(sdo_xsdintf.s_Unresolved) then
        propTypItm := Find( AContainer,propTypItm.getString(sdo_xsdintf.s_NameSpace),propTypItm.getString(sdo_xsdintf.s_Name));
      propItmUltimeType := GetUltimeType(propTypItm);
      s := propTypItm.getString(sdo_xsdintf.s_Name);
      prop_ns_shortName := GetNameSpaceShortName(GetTypeNameSpace(propTypItm),Document,GetPreferedShortNames());
      propNode.SetAttribute(s_type,Format('%s:%s',[prop_ns_shortName,s]));
      if (Length(p.getString(s_DefaultValue)) > 0) then
        propNode.SetAttribute(s_default,p.getString(s_DefaultValue));
      if p.getBoolean(s_IsAttribute) then begin
        if (p.getInteger(s_PropertyMinOccurs) > 0) then
          propNode.SetAttribute(s_use,'required');
      end else begin
        if (p.getInteger(s_PropertyMinOccurs) = 0) or (p.getInteger(s_PropertyMinOccurs) <> 1) then
          propNode.SetAttribute(s_minOccurs,p.getString(s_PropertyMinOccurs));
        if (p.getInteger(s_PropertyMaxOccurs) = MaxInt) then
          propNode.SetAttribute(s_maxOccurs,s_unbounded)
        else if (p.getInteger(s_PropertyMaxOccurs) > 1) then
          propNode.SetAttribute(s_maxOccurs,p.getString(s_PropertyMaxOccurs));
      end;
    end;
    ProcessPropertyExtendedMetadata(p,propNode);
  end;

var
  typItm : ISDODataObject;
  s : string;
  i : Integer;
  typeCategory : TTypeCategory;
  hasSequence : Boolean;
  trueParent : ISDODataObject;
  hasXsdAny, hasXsdAnyAtt : Boolean;
  xsdAnyString, xsdAnyAttString : TSDOString;
  pl : ISDODataObjectList;
begin
  if (ASymbol = nil) then
    Exit;
  typItm := ASymbol;
  GetNameSpaceShortName(
    AContainer.getDataObject(s_CurrentModule).getString(sdo_xsdintf.s_NameSpace),
    Document,GetPreferedShortNames()
  );
  defSchemaNode := GetSchemaNode(Document) as TDOMElement;

  s := Format('%s:%s',[s_xs_short,s_complexType]);
  cplxNode := CreateElement(s,defSchemaNode,Document);
  cplxNode.SetAttribute(s_name, typItm.getString(sdo_xsdintf.s_Name)) ;

{$IFDEF SDO_HANDLE_DOC}
  ls := AContainer.Properties.FindList(typItm);
  if ( ls <> nil ) then begin
    i := ls.IndexOfName(s_documentation);
    if ( i >= 0 ) then
      GenerateDocumentation(cplxNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
  end;
{$ENDIF SDO_HANDLE_DOC}

  typeCategory := tcComplexContent;
  derivationNode := nil;
  hasSequence := True;
  trueParent := typItm.getDataObject(sdo_xsdintf.s_BaseType);
  if (trueParent <> nil) then begin
    if trueParent.getBoolean(s_Unresolved) then
      trueParent := Find(AContainer,trueParent.getString(sdo_xsdintf.s_NameSpace), trueParent.getString(sdo_xsdintf.s_Name));
    if (trueParent <> nil) then begin
      if (trueParent.getByte(s_ElementKind) = sdo_xsdintf.ELEMENT_KIND_VARIABLE) then
        trueParent := GetUltimeType(trueParent);
      if not trueParent.getBoolean(s_IsComplex) then
        typeCategory := tcSimpleContent;
      if ( typeCategory = tcSimpleContent ) then begin
        derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_simpleContent]),cplxNode,Document);
        derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_extension]),derivationNode,Document);
      end else begin
        derivationNode := CreateElement(Format('%s:%s',[s_xs_short,s_extension]),cplxNode,Document);
      end;
      s := Trim(GetNameSpaceShortName(GetTypeNameSpace(trueParent),Document,GetPreferedShortNames()));
      if ( Length(s) > 0 ) then
        s := s + ':';
      s := s + trueParent.getString(sdo_xsdintf.s_Name);
      derivationNode.SetAttribute(s_base,s);
      hasSequence := False;
    end;
  end;
  pl := typItm.getList(s_Property);
  if (pl.size() > 0) then
    hasSequence := TypeHasSequence(typItm,typeCategory);
  hasXsdAny := False;
  hasXsdAnyAtt := False;
  if (typeCategory = tcComplexContent) then begin
    hasXsdAny := FindTag(typItm,Format('%s#%s',[s_xs,s_any]),xsdAnyString);
    if hasXsdAny then begin
      if not hasSequence then
        hasSequence := True;
    end;
    hasXsdAnyAtt := FindTag(typItm,Format('%s#%s',[s_xs,s_anyAttribute]),xsdAnyAttString);
  end;
  if hasSequence then begin
    s := Format('%s:%s',[s_xs_short,s_sequence]);
    if Assigned(derivationNode) then
      sqcNode := CreateElement(s,derivationNode,Document)
    else
      sqcNode := CreateElement(s,cplxNode,Document);
  end else begin
    sqcNode := nil;
  end;

  for i := 0 to pl.size() -1 do
    ProcessProperty(pl.getDataObject(i));
  if hasXsdAny then
    ProcessXsdAny(sqcNode,xsdAnyString);
  if hasXsdAnyAtt then
    ProcessXsdAnyAttribute(cplxNode,xsdAnyAttString);
end;

{ TXsdGenerator }

function TXsdGenerator.GetSchemaNode(ADocument : TDOMDocument) : TDOMNode;
begin
  Result := FSchemaNode;
end;

procedure TXsdGenerator.Prepare(ASymTable, AModule : ISDODataObject);
var
  unitExternalName : string;
begin
  inherited Prepare(ASymTable, AModule);
  unitExternalName := AModule.getString(s_namespace);
  FSchemaNode := CreateElement(s_schema,Document,Document);
  FSchemaNode.SetAttribute(s_targetNamespace,unitExternalName);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_xs_short]),s_xs);
  FSchemaNode.SetAttribute(Format('%s:%s',[s_xmlns,s_tns]),unitExternalName);
end;


  
end.
