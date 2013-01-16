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
{$mode objfpc}
{$h+}
unit pas_generator;

interface

uses
  Classes, SysUtils, TypInfo, sdo, contnrs;

type

  { TPasIdentifier }

  TPasIdentifier = Class(TPersistent)
  private
    FName: String;
    FNameSpace: String;
    FOutputName: String;
    function GetOutputName: String;
  Public
    Constructor create; virtual;
    Constructor CreateNamed(Const AName,ANameSpace : String);
  Published
    Property OutputName : String Read GetOutputName Write FOutputName;
    Property Name : String Read FName Write Fname;
    Property NameSpace : String Read FNameSpace Write FNameSpace;
  end;

  { TPasIdentifierList }

  TPasIdentifierList = Class(TObjectList)
  private
    function GetI(AIndex : Integer): TPasIdentifier;
    procedure SetI(AIndex : Integer; AValue: TPasIdentifier);
  Public
    Property Identifier[AIndex : Integer] : TPasIdentifier Read GetI Write SetI; default;
  end;

  { TPasEnumeration }

  TPasEnumeration = Class(TPasIdentifier)
  private
    FValues: Tstrings;
    procedure SetValues(AValue: Tstrings);
  Public
    Constructor Create; override;
    Destructor Destroy; override;
  Published
    Property Values : Tstrings Read FValues Write SetValues;
  end;

  { TPasArray }

  TPasArray = CLass(TPasIdentifier)
  private
    FElement: TPasidentifier;
    FTypeName: String;
  Public
    Property Elemtent : TPasidentifier Read FElement Write FElement;
  Published
    Property ElementName : String Read FTypeName Write FTypeName;
  end;

  { TPasProperty }

  TPasProperty = Class(TPasIdentifier)
  private
    FDefaultValue: String;
    FIsRequired: Boolean;
    FMaxOccurs: Integer;
    FMinOccurs: Integer;
    FTypeName: String;
    FTypeNameSpace: String;
  Published
    Property TypeName : String Read FTypeName Write FTypeName;
    Property TypeNameSpace : String Read FTypeNameSpace Write FTypeNameSpace;
    Property DefaultValue : String Read FDefaultValue Write FDefaultValue;
    Property IsRequired : Boolean Read FIsRequired write FIsRequired;
    Property MinOccurs : Integer read FMinOccurs Write FMinOccurs;
    Property MaxOccurs : Integer Read FMaxOccurs Write FMaxOccurs;
  end;

  { TPasPropertyList }

  TPasPropertyList = Class(TPasIdentifierList)
  private
    function GetI(AIndex : Integer): TPasProperty;
    procedure SetI(AIndex : Integer; AValue: TPasProperty);
  Public
    Property Properties[AIndex : Integer] : TPasProperty Read GetI Write SetI;default;
  end;

  { TPasClass }

  TPasClass = Class(TPasIdentifier)
  private
    FMembers: TPasPropertyList;
    FParentName: String;
    FParentNameSpace: String;
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Property Members : TPasPropertyList Read FMembers Write FMembers;
  Published
    Property ParentName : String Read FParentName Write FParentName;
    Property ParentNameSpace : String Read FParentNameSpace Write FParentNameSpace;
  end;



  TPasGeneratorOption = ( xgoIgnorembeddedArray );
  TPasGeneratorOptions = set of TPasGeneratorOption;

  EPasGeneratorException = class(Exception) end;

  IGenerator = interface
    ['{F69523B3-A6FF-4BFB-9ACB-D4B9F32DBCA9}']
    procedure Execute(
      ASymTable   : ISDODataObject;
      AModuleName : string
    );
  end;

  IPasGenerator = interface(IGenerator)
    ['{FBFF92BC-B72B-4B85-8D16-379F9E548DDB}']
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;
    Function GetUnitName : String;
    Procedure SetUnitname(Const AValue : String);
    Property UnitName : String Read GetUnitName Write setUnitName;
  end;

  { TCustomPasGenerator }
  TTypeCategory = ( tcComplexContent, tcSimpleContent );


  TCustomPasGenerator = class(
    TInterfacedObject,
    IInterface,
    IGenerator,
    IPasGenerator
  )
  private
    FStream : TStream;
    FOptions: TPasGeneratorOptions;
    FUnitName: String;
    FShortNames : TStrings;
    FIndent : Integer;
    FIdentifiers : TPasIdentifierList;
    procedure ProcessProperty(AContainer : ISDODataObject; Const AClass : TPasClass; const AProp: ISDODataObject);
{    procedure ProcessXsdAny(const Prop : TPasProperty; const AXsdInfo: string);
    procedure ProcessXsdAnyAttribute(const Prop : TPasProperty;
      const AXsdInfo: string);
}    function TypeHasSequence(const AClassType: ISDODataObject;
      const ACategory: TTypeCategory): Boolean;
  protected
    Procedure Error(Const AMsg : String);
    Procedure Error(Const Fmt : String; Args : Array of const);
    Procedure Line(Const ALine : String);
    Procedure Line(Const Fmt : String; Args : Array of const);
    Procedure Indent;
    Procedure Undent;
    procedure SetPreferedShortNames(const ALongName, AShortName : string);
    function GetPreferedShortNames() : TStrings;
    procedure GenerateEnum(AContainer, ASymbol : ISDODataObject);
    procedure GenerateComplex(AContainer, ASymbol : ISDODataObject);
    Function GetUnitName : String;
    Procedure SetUnitname(Const AValue : String);
    Procedure StartInterface; virtual;
    procedure Execute(
      ASymTable   : ISDODataObject;
      AModuleName : string
    );

    procedure Prepare(
      ASymTable,
      AModule   : ISDODataObject
    );virtual;
    property Options : TPasGeneratorOptions read FOptions;
  public
    constructor Create(const AStream : TStream);overload;
    constructor Create(
      const AStream : TStream;
      const AOptions : TPasGeneratorOptions
    );overload;
    destructor Destroy();override;
  end;

  { TPasGenerator }

  TPasGenerator = class(TCustomPasGenerator)
  private
  protected
    procedure Prepare(ASymTable, AModule : ISDODataObject);override;
  end;


implementation

uses
  sdo_xsdintf, xsd_consts,  StrUtils, sdo_types, sdo_parserutils;

{ TPasClass }

constructor TPasClass.Create;
begin
  inherited Create;
  FMembers:=TPasPropertyList.Create(True);
end;

destructor TPasClass.Destroy;
begin
  FreeAndNil(FMembers);
  inherited Destroy;
end;

{ TPasPropertyList }

function TPasPropertyList.GetI(AIndex: Integer): TPasProperty;
begin
  Result:=Items[AIndex] as TPasProperty
end;

procedure TPasPropertyList.SetI(AIndex: Integer; AValue: TPasProperty);
begin
  Items[AIndex]:=AValue;
end;

function TPasIdentifier.GetOutputName: String;
begin
  Result:=FoutputName;
  If Result='' then
    Result:=FName;
end;

constructor TPasIdentifier.create;
begin
  // Do nothing
end;

constructor TPasIdentifier.CreateNamed(Const AName, ANameSpace : string);
begin
  FName:=AName;
  FNameSpace:=ANameSpace;
  Create;
end;

{ TPasIdentifierList }

function TPasIdentifierList.GetI(AIndex : Integer): TPasIdentifier;
begin
  Result:=Items[AIndex] as TPasIdentifier;
end;

procedure TPasIdentifierList.SetI(AIndex : Integer; AValue: TPasIdentifier);
begin
  Items[AIndex]:=AValue;
end;

{ TPasEnumeration }

procedure TPasEnumeration.SetValues(AValue: Tstrings);
begin
  if FValues=AValue then Exit;
  FValues.Assign(AValue);
end;

constructor TPasEnumeration.Create;
begin
  inherited Create;
  FValues:=TStringList.Create;
end;

destructor TPasEnumeration.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;


{ TCustomPasGenerator }

procedure TCustomPasGenerator.Execute(
  ASymTable   : ISDODataObject;
  AModuleName : string
);
var
  i : Integer;
  locModule : ISDODataObject;
begin
  if ( ASymTable = nil ) then
    Error('Invalid symbol table.');
  locModule := FindModule(ASymTable,AModuleName);
  if ( locModule = nil ) then
    Error('Unable to find module : "%s".',[AModuleName]);
  If (FUnitName='') then
    FUnitName:=AModuleName;
  Prepare(ASymTable,locModule);
  StartInterface;
  For I:=0 to FIDentifiers.Count-1 do
    Writeln(FIdentifiers[i].Name, ' in ',FIdentifiers[i].NameSpace);
end;

procedure TCustomPasGenerator.Prepare(
  ASymTable,
  AModule   : ISDODataObject
);
Var
  i : Integer;
  locList : ISDODataObjectList;
  locElement : ISDODataObject;
begin
  locList := AModule.getList(s_Type);
  for i := 0 to locList.size() - 1 do
    begin
    locElement := locList.getDataObject(i);
    if locElement.getBoolean(s_IsComplex) then
      GenerateComplex(ASymTable,locElement)
    else if (locElement.getList(s_EnumValue).size() > 0) then
      GenerateEnum(ASymTable,locElement);
    end
end;

constructor TCustomPasGenerator.Create(const AStream: TStream);
begin
  Create(AStream,[]);
end;

constructor TCustomPasGenerator.Create(const AStream: TStream;
  const AOptions: TPasGeneratorOptions);
var
  sl : TStringList;
begin
  if ( AStream = nil ) then
    raise EPasGeneratorException.Create('Invalid document.');
  FStream := AStream;
  FOptions := AOptions;
  sl := TStringList.Create();
  //sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  FShortNames:=Sl;
  FIdentifiers:=TPasIdentifierList.Create;
end;

procedure TCustomPasGenerator.Error(const AMsg: String);
begin
  raise EPasGeneratorException.Create(AMsg);
end;

procedure TCustomPasGenerator.Error(const Fmt: String; Args: array of const);
begin
  raise EPasGeneratorException.CreateFmt(Fmt,Args);
end;

procedure TCustomPasGenerator.Line(const ALine: String);

Var
  S : String;
begin
  if (ALine<>'') then
    S:=StringOfChar(' ',FIndent)+ALine;
  S:=S+sLineBreak;
  FStream.WriteBuffer(S[1],Length(s)*SizeOf(Char));
end;

procedure TCustomPasGenerator.Line(const Fmt: String; Args: array of const);
begin
  Line(Format(Fmt,Args));
end;

procedure TCustomPasGenerator.Indent;
begin
  Inc(FIndent,2);
end;

procedure TCustomPasGenerator.Undent;
begin
  If (FIndent>0) then
    Dec(FIndent,2);
end;

procedure TCustomPasGenerator.SetPreferedShortNames(const ALongName, AShortName: string);
begin
  FShortNames.Values[ALongName] := AShortName;
end;

function TCustomPasGenerator.GetPreferedShortNames() : TStrings;
begin
  Result := FShortNames;
end;

destructor TCustomPasGenerator.Destroy();
begin
  FreeAndNil(FShortNames);
  inherited;
end;

procedure TCustomPasGenerator.GenerateEnum(AContainer, ASymbol : ISDODataObject);
var
  typItm : ISDODataObject;
  valueList : ISDODataObjectList;
  s : string;
  i : Integer;
{$IFDEF SDO_HANDLE_DOC}
  ls : TStrings;
{$ENDIF SDO_HANDLE_DOC}
  ET : TPasEnumeration;

begin
  if (ASymbol = nil) then
    Exit;
  typItm := ASymbol;
  {$IFDEF SDO_HANDLE_DOC}
    ls := AContainer.Properties.FindList(typItm);
    if ( ls <> nil ) then begin
      i := ls.IndexOfName(s_documentation);
      if ( i >= 0 ) then
        GenerateDocumentation(resNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
    end;
  {$ENDIF SDO_HANDLE_DOC}
  S:=typItm.getString(sdo_xsdintf.s_Name)+' = (';
  ET:=TPasEnumeration.CreateNamed(S,'');
  Self.FIdentifiers.Add(Et);
  valueList := typItm.getList(s_EnumValue);
  for i := 0 to (valueList.size() - 1) do
    ET.Values.Add(valueList.getString(i));
end;

function TCustomPasGenerator.TypeHasSequence(const AClassType : ISDODataObject; const ACategory : TTypeCategory) : Boolean;
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
          Error(  'Invalid type definition, a simple type cannot have "not attribute" properties : "%s"',
                  [AClassType.getString(sdo_xsdintf.s_Name)]
                );
        end;
        Result := True;
      end;
    end;
  end;
end;


procedure TCustomPasGenerator.ProcessProperty(AContainer : ISDODataObject; Const AClass : TPasClass; const AProp : ISDODataObject);
var
  p : ISDODataObject;
  s : string;
  pt, propItmUltimeType : ISDODataObject;
  prop_ns_shortName : string;
  PP : TPasProperty;

begin
  p := AProp;
  PP:=TPasProperty.CreateNamed(p.getString(sdo_xsdintf.s_Name),'');
  AClass.Members.Add(pp);
  if p.getBoolean(sdo_xsdintf.s_IsAttribute) then
    begin
{    s := Format('%s:%s',[s_xs_short,s_attribute]);
    if Assigned(derivationNode) then
      propNode := CreateElement(s,derivationNode,Document)
    else
      propNode := CreateElement(s,cplxNode,Document);}
    end;

  pt := p.getDataObject(s_DataType);
  if Assigned(pt) then
    begin
    if pt.getBoolean(sdo_xsdintf.s_Unresolved) then
      pt := Find( AContainer,pt.getString(sdo_xsdintf.s_NameSpace),pt.getString(sdo_xsdintf.s_Name));
    propItmUltimeType := GetUltimeType(pt);
    PP.TypeName:=pt.getString(sdo_xsdintf.s_Name);
    PP.TypeNamespace := GetTypeNameSpace(pt);
    PP.DefaultValue:=p.getString(s_DefaultValue);
    if p.getBoolean(s_IsAttribute) then
      begin
      PP.IsRequired:=(p.getInteger(s_PropertyMinOccurs)>0);
      end
    else
      begin
      PP.MinOccurs:=p.getInteger(s_PropertyMinOccurs);
      PP.MaxOccurs:=p.getInteger(s_PropertyMaxOccurs);
      end;
    end;
//  ProcessPropertyExtendedMetadata(p,propNode);
end;

procedure TCustomPasGenerator.GenerateComplex(AContainer, ASymbol: ISDODataObject);

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
  PC : TpasClass;
  CN,CNS : String;
begin
  if (ASymbol = nil) then
    Exit;
  typItm := ASymbol;
  PC:=TPasClass.CreateNamed(typItm.getString(sdo_xsdintf.s_Name),AContainer.getDataObject(s_CurrentModule).getString(sdo_xsdintf.s_NameSpace));
  Self.FIdentifiers.Add(PC);
{$IFDEF SDO_HANDLE_DOC}
  ls := AContainer.Properties.FindList(typItm);
  if ( ls <> nil ) then begin
    i := ls.IndexOfName(s_documentation);
    if ( i >= 0 ) then
      GenerateDocumentation(cplxNode,DecodeLineBreak(ls.ValueFromIndex[i]),ADocument);
  end;
{$ENDIF SDO_HANDLE_DOC}

  typeCategory := tcComplexContent;
  hasSequence := True;
  trueParent := typItm.getDataObject(sdo_xsdintf.s_BaseType);
  if (trueParent <> nil) then
    begin
    if trueParent.getBoolean(s_Unresolved) then
      trueParent := Find(AContainer,trueParent.getString(sdo_xsdintf.s_NameSpace), trueParent.getString(sdo_xsdintf.s_Name));
    if (trueParent <> nil) then
      begin
      if (trueParent.getByte(s_ElementKind) = sdo_xsdintf.ELEMENT_KIND_VARIABLE) then
        trueParent := GetUltimeType(trueParent);
      if not trueParent.getBoolean(s_IsComplex) then
        typeCategory := tcSimpleContent;
      PC.ParentNameSpace:=GetTypeNameSpace(trueParent);
      PC.ParentName:=trueParent.getString(sdo_xsdintf.s_Name);
      hasSequence := False;
      end;
    end;
  pl := typItm.getList(s_Property);
  if (pl.size() > 0) then
    hasSequence := TypeHasSequence(typItm,typeCategory);
  hasXsdAny := False;
  hasXsdAnyAtt := False;
  if (typeCategory = tcComplexContent) then
    begin
    hasXsdAny := FindTag(typItm,Format('%s#%s',[s_xs,s_any]),xsdAnyString);
    if hasXsdAny then
      begin
      if not hasSequence then
        hasSequence := True;
      end;
    hasXsdAnyAtt := FindTag(typItm,Format('%s#%s',[s_xs,s_anyAttribute]),xsdAnyAttString);
    end;

  for i := 0 to pl.size() -1 do
    ProcessProperty(typItm,PC,pl.getDataObject(i));
{
if hasXsdAny then
    ProcessXsdAny(sqcNode,xsdAnyString);
  if hasXsdAnyAtt then
    ProcessXsdAnyAttribute(cplxNode,xsdAnyAttString);
}end;

function TCustomPasGenerator.GetUnitName: String;
begin
  Result:=FUnitName;
end;

procedure TCustomPasGenerator.SetUnitname(const AValue: String);
begin
  FUnitName:=AValue;
end;

procedure TCustomPasGenerator.StartInterface;
begin
end;

{ TPasGenerator }


procedure TPasGenerator.Prepare(ASymTable, AModule : ISDODataObject);
begin
  inherited Prepare(ASymTable, AModule);
//  unitExternalName := AModule.getString(s_namespace);
end;


  
end.
