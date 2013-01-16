{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements XSD to SDO translation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_xsd_helper;

interface
uses SysUtils, Classes,
     sdo, sdo_type, sdo_datafactory;

type

  { TXSDHelper }

  TXSDHelper = class(TInterfacedObject, IInterface, IXSDHelper)
  private
    FDataFactory : ISDODataFactory;
  protected
    procedure LoadFromStream(AStream : TStream);
    procedure LoadFromFile(const AFileName : string);
    procedure LoadFromString(const ASchemaString : string);

    procedure Generate(
            ATypeList : ISDOTypeList;
            ADestStream : TStream;
      const ATargetNamespace : string
    );overload;
    function Generate(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string
    ) : string;overload;
    procedure Generate(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string;
      const AFileName : string
    );overload;
    procedure GenerateCode(
            ATypeList : ISDOTypeList;
            ADestStream : TStream;
      const ATargetNamespace : string
    );overload;
    function GenerateCode(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string
    ) : string;overload;
    procedure GenerateCode(
            ATypeList : ISDOTypeList;
      const ATargetNamespace : string;
      const AFileName : string
    );overload;

    function  GetDataFactory() : ISDODataFactory;
  public
    constructor Create(ADataFactory : ISDODataFactory);
  end;

implementation
uses
   sdo_types, sdo_parserutils,
{$IFNDEF FPC}
   xmldom, sdo_win_xml,
{$ELSE}
   DOM, XmlRead, XmlWrite, sdo_fpc_xml,
{$ENDIF}
   sdo_xsdparser, sdo_imp_utils, xsd_generator, xsd_consts, sdo_consts,
   sdo_xsdintf, pas_generator;

const
  SDO_SPECIAL_TYPES = [CharacterType, CurrencyType];

type
  TPasTreeSdoConverter = class
  private
    FDataFactory : ISDODataFactory;
    FTree : ISDODataObject;
    FTypeList : ISDOTypeListEx;
  private
    function FindType(const AUri, AName : string) : ISDOType;

    function TranslateType(AType : ISDODataObject) : ISDOType;
    function TranslateClass(AElement : ISDODataObject) : ISDOType;
    function TranslateEnum(AElement : ISDODataObject) : ISDOType;
    function TranslateVariable(AElement : ISDODataObject) : ISDOType;

    procedure TranslateModule(AModule : ISDODataObject);
  public
    constructor Create(
      ATree : ISDODataObject;
      ADataFactory : ISDODataFactory
    );
    procedure Execute();
  end;

  TSdoPasConverter = class
  private
    FDataFactory : ISDODataFactory;
    FTree : ISDODataObject;
    FTypeList : ISDOTypeList;
  private
    function FindType(const AUri, AName : string) : ISDODataObject;
    function FindOrCreateModule(const ANamespace : string) : ISDODataObject;

    function TranslateType(const AType : ISDOType) : ISDODataObject;
    function TranslateAlias(const AType : ISDOType) : ISDODataObject;
    function TranslateObject(const AType : ISDOType) : ISDODataObject;
    function TranslateChangeSummary(const AType : ISDOType) : ISDODataObject;
  public
    constructor Create(
      ATree : ISDODataObject;
      ADataFactory : ISDODataFactory;
      ATypeList : ISDOTypeList
    );
    procedure Execute();
  end;

procedure PasTreeToSdoTypes(
  ATree : ISDODataObject;
  ADataFactory : ISDODataFactory
);
var
  loc_converter : TPasTreeSdoConverter;
begin
  loc_converter := TPasTreeSdoConverter.Create(ATree,ADataFactory);
  try
    loc_converter.Execute();
  finally
    loc_converter.Free();
  end;
end;

procedure SdoTypesToPasTree(
  ATree : ISDODataObject;
  ADataFactory : ISDODataFactory;
  ATypeList : ISDOTypeList
);
var
  loc_converter : TSdoPasConverter;
begin
  loc_converter := TSdoPasConverter.Create(ATree,ADataFactory,ATypeList);
  try
    loc_converter.Execute();
  finally
    loc_converter.Free();
  end;
end;

{ TXSDHelper }

constructor TXSDHelper.Create(ADataFactory : ISDODataFactory);
begin
  if ( ADataFactory = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataFactory');
  FDataFactory := ADataFactory;
end;

procedure TXSDHelper.Generate(
        ATypeList : ISDOTypeList;
        ADestStream : TStream;
  const ATargetNamespace : string
);
var
  doc : TXMLDocument;
  gnrt : IXsdGenerator;
  tree : ISDODataObject;
  lst : ISDOTypeList;
  locXsdFactory :ISDODataFactory;
begin
  if ( ADestStream = nil ) then
    raise ESDOIllegalArgumentException.Create('ADestStream');
  if Assigned(ATypeList) then
    lst := ATypeList
  else
    lst := FDataFactory.getTypes();
  locXsdFactory := TSDODataFactory.Create();
  AddTypeTree(locXsdFactory);
  doc := nil;
  try
    tree := locXsdFactory.CreateNew(s_XsdParserNS,s_TypeTreeType); 
    AddXsdTypes(tree);
    SdoTypesToPasTree(tree,FDataFactory,lst);
    doc := CreateDoc();
    gnrt := TXsdGenerator.Create(doc,[xsd_generator.xgoIgnorembeddedArray]);
    gnrt.SetPreferedShortNames(sdo_namespace,s_sdo);
    gnrt.Execute(tree,ATargetNamespace);
    WriteXMLFile(doc,ADestStream);
  finally
    ReleaseDomNode(doc);
    tree := nil;
  end;
end;

function TXSDHelper.Generate(
        ATypeList : ISDOTypeList;
  const ATargetNamespace : string
) : string;
var
  locStream : TStringStream;
begin
  locStream := TStringStream.Create('');
  try
    Generate(ATypeList,locStream,ATargetNamespace);
    Result := locStream.DataString;
  finally
    locStream.Free();
  end;
end;

procedure TXSDHelper.Generate(
        ATypeList : ISDOTypeList;
  const ATargetNamespace : string;
  const AFileName : string
);
var
  locStream : TFileStream;
begin
  locStream := TFileStream.Create(AFileName,fmCreate);
  try
    Generate(ATypeList,locStream,ATargetNamespace);
  finally
    locStream.Free();
  end;
end;

procedure TXSDHelper.GenerateCode(ATypeList: ISDOTypeList;
  ADestStream: TStream; const ATargetNamespace: string);
var
  gnrt : IPasGenerator;
  tree : ISDODataObject;
  lst : ISDOTypeList;
  locXsdFactory :ISDODataFactory;
begin
  if ( ADestStream = nil ) then
    raise ESDOIllegalArgumentException.Create('ADestStream');
  if Assigned(ATypeList) then
    lst := ATypeList
  else
    lst := FDataFactory.getTypes();
  locXsdFactory := TSDODataFactory.Create();
  AddTypeTree(locXsdFactory);
  tree := locXsdFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
  try
    AddXsdTypes(tree);
    SdoTypesToPasTree(tree,FDataFactory,lst);
    gnrt := TPasGenerator.Create(ADestStream,[pas_generator.xgoIgnorembeddedArray]);
    gnrt.SetPreferedShortNames(sdo_namespace,s_sdo);
    gnrt.Execute(tree,ATargetNamespace);
  finally
    tree := nil;
  end;
end;

function TXSDHelper.GenerateCode(ATypeList: ISDOTypeList;
  const ATargetNamespace: string): string;
var
  locStream : TStringStream;
begin
  locStream := TStringStream.Create('');
  try
    Generate(ATypeList,locStream,ATargetNamespace);
    Result := locStream.DataString;
  finally
    locStream.Free();
  end;
end;

procedure TXSDHelper.GenerateCode(ATypeList: ISDOTypeList;
  const ATargetNamespace: string; const AFileName: string);
var
  locStream : TFileStream;
begin
  locStream := TFileStream.Create(AFileName,fmCreate);
  try
    Generate(ATypeList,locStream,ATargetNamespace);
  finally
    locStream.Free();
  end;
end;

function TXSDHelper.GetDataFactory() : ISDODataFactory;
begin
  Result := FDataFactory;
end;

procedure TXSDHelper.LoadFromFile(const AFileName: string);
var
  FileStream: TStream;
begin
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free();
  end;
end;

procedure AddSDOTypes(const ATree : ISDODataObject);
var
  locClassType : ISDODataObject;
  locSdoTable : ISDODataObject;
begin
  locSdoTable := ATree.createDataObject(s_Module);
  locSdoTable.setBoolean(s_Native,True);
  locSdoTable.setString(s_Name,'sdo');
  locSdoTable.setString(s_NameSpace,sdo_namespace);
  ATree.getList(s_Module).append(locSdoTable);

  locClassType := locSdoTable.createDataObject(s_Type);
  locClassType.setString(s_Name,SDOTypeDefaultTypeNames[ChangeSummaryType]);
  locSdoTable.getList(s_Type).append(locClassType);
end;


procedure TXSDHelper.LoadFromStream(AStream: TStream);
var
  doc : TXMLDocument;
  prsr : IXsdParser;
  treeFactory : ISDODataFactory;
  tree : ISDODataObject;
begin
  if ( AStream = nil ) then
    raise ESDOIllegalArgumentException.Create('AStream');
  doc := nil;
  try
    ReadXMLFile(doc,AStream);
    treeFactory := TSDODataFactory.Create();
    AddTypeTree(treeFactory);
    tree := treeFactory.CreateNew(s_XsdParserNS,s_TypeTreeType);
    AddXsdTypes(tree);
    AddSDOTypes(tree);
    prsr := TXsdParser.Create(doc,tree,'');
    prsr.ParseTypes();
    PasTreeToSdoTypes(tree,GetDataFactory());
  finally
    ReleaseDomNode(doc);
    tree := nil;
  end;
end;

procedure TXSDHelper.LoadFromString(const ASchemaString: string);
var
  locStream : TStream;
begin
  locStream := TStringStream.Create(ASchemaString);
  try
    locStream.Position := 0;
    LoadFromStream(locStream);
  finally
    locStream.Free();
  end;
end;

{ TPasTreeSdoConverter }

constructor TPasTreeSdoConverter.Create(
  ATree: ISDODataObject;
  ADataFactory : ISDODataFactory
);
begin
  if ( ATree = nil ) then
    raise ESDOIllegalArgumentException.Create('ATree');
  if ( ADataFactory = nil ) then
    raise ESDOIllegalArgumentException.Create('ADataFactory');
  FTree := ATree;
  FDataFactory := ADataFactory;
  FTypeList := FDataFactory.getTypes() as ISDOTypeListEx;
end;

procedure TPasTreeSdoConverter.Execute();
var
  i, c : PtrInt;
  mdl : ISDODataObject;
  mdlLs : ISDODataObjectList;
begin
  mdlLs := FTree.getList(s_Module);
  c := mdlLs.size();
  for i := 0 to Pred(c) do begin
    mdl := mdlLs.getDataObject(i);
    if not mdl.getBoolean(s_Native) then
      TranslateModule(mdl);
  end;
end;

function TPasTreeSdoConverter.FindType(const AUri, AName: string): ISDOType;
begin
  Result := FTypeList.find(AUri,AName);
  if ( Result = nil ) and AnsiSameText(AUri,s_xs) then
    Result := FTypeList.find(sdo_namespace,AName);
end;

function TPasTreeSdoConverter.TranslateVariable(AElement: ISDODataObject): ISDOType;
var
  new_type : ISDOType;
  loc_uri, loc_name : string;
  dest_type : ISDOType;
begin
  loc_uri := GetVariableNameSpace(AElement);
  loc_name := AElement.getString(s_Name);
  new_type := FindType(loc_uri,loc_name);
  if ( new_type = nil ) then begin
    dest_type := TranslateType(AElement.getDataObject(s_DataType));
    if AnsiSameText(loc_uri,dest_type.getURI()) then begin
      new_type := dest_type;
      FDataFactory.setAlias(dest_type.getURI(),dest_type.getName(),loc_name);// dest_type.setAlias(loc_name);
    end else begin
      FDataFactory.AddType(
        loc_uri,
        loc_name,
        dest_type.getFlags()
      );
      new_type := FDataFactory.getType(loc_uri,loc_name);
      FDataFactory.setBaseType(new_type,dest_type);
    end;
  end;
  Result := new_type;
end;

{function TPasTreeSdoConverter.TranslateArray(AElement: TPasArrayType): ISDOType;
var
  new_type : ISDOType;
  loc_uri, loc_name : string;
  item_type : ISDOType;
  typeflgs : TTypeFlags;
  propFlags : TPropertyFlags;
  ls : TStrings;
begin
  loc_uri := FTree.GetNameSpace(AElement);
  loc_name := FTree.GetExternalName(AElement);
  new_type := FindType(loc_uri,loc_name);
  if ( new_type = nil ) then begin
    item_type := TranslateType(AElement.ElType);
    typeflgs := [];
    ls := FTree.Properties.FindList(AElement);
    if ( ls <> nil ) and
       ( ( ls.IndexOfName(s_xsd_namespace + '#' + s_any) >= 0 ) or
         ( ls.IndexOfName(s_xsd_namespace + '#' + s_anyAttribute) >= 0 )
       )
    then begin
      Include(typeflgs,tfIsOpen);
    end;
    FDataFactory.AddType(loc_uri,loc_name,typeflgs);
    new_type := FDataFactory.getType(loc_uri,loc_name);
    propFlags := [pfIsMany];
    if item_type.isDataObjectType() then
      Include(propFlags,pfIsContainment (* TODO : what for array of ID ( array of object reference ) ? *));
    FDataFactory.addProperty(new_type,FTree.GetArrayItemExternalName(AElement),item_type,propFlags);
  end;
  Result := new_type;
end;}

function TPasTreeSdoConverter.TranslateClass(AElement: ISDODataObject) : ISDOType;
var
  new_type : ISDOType;

  function FindSpecialType(const AProp : ISDODataObject) : ISDOType;
  var
    line, ns, localName : string;
    k : PtrInt;
  begin
    Result := nil;
    line := Trim(FindTag(AProp,Format('%s#%s',[sdo_namespace,sdo_consts.s_propertyType])));
    if ( Length(line) > 0 ) then begin
      k := Pos('#',line);
      if ( k > 0 ) then begin
        ns := Copy(line,1,Pred(k));
        localName := Copy(line,Succ(k),MaxInt);
        Result := FDataFactory.getTypes().find(ns,localName);
        if ( Result = nil ) then
          raise ESDOTypeNotFoundException.Create(Format('%s#%s',[ns,localName]));
      end;
    end;
  end;

  function FindReferenceType(const AProp : ISDODataObject) : ISDOType;
  begin
    Result := FindSpecialType(AProp);
  end;

  procedure ProcessProperty(const AProp : ISDODataObject);
  var
    p : ISDODataObject;
    pt, tmp : ISDODataObject;
    loc_isArray, loc_isReadOnly, loc_isContainment, loc_isReference, loc_isNullable : Boolean;
    ptSDO : ISDOType;
    flgs : TPropertyFlags;
  begin
    loc_isReference := False;
    p := AProp;
    pt := p.getDataObject(s_DataType);
    ptSDO := FindReferenceType(AProp);
    if pt.getBoolean(s_Unresolved) then begin
      tmp := Find(FTree,GetTypeNameSpace(pt),pt.getString(s_Name));
      if (tmp <> nil) then
        pt := tmp;
    end;
    if (pt.getString(s_Name) = s_anyURI) and (GetTypeNameSpace(pt) = s_xs) then begin
      //ptSDO := FindReferenceType(AProp);
      if ( ptSDO <> nil ) then
        loc_isReference := True;
    end;
    pt := GetUltimeType(pt);
    loc_isArray := (p.getInteger(s_PropertyMaxOccurs) > 1);
    if (pt.getString(s_Name) = s_anyURI) and (GetTypeNameSpace(pt) = s_xs) then begin
      //ptSDO := FindReferenceType(AProp);
      if ( ptSDO <> nil ) then
        loc_isReference := True;
    end;
    if (pt.getString(s_Name) = SDOTypeDefaultTypeNames[ChangeSummaryType]) and
       (GetTypeNameSpace(pt) = sdo_namespace)
    then begin
      loc_isReadOnly := True;
      loc_isContainment := False;
      ptSDO := FDataFactory.getType(sdo_namespace,s_changeSummary);
    end else begin
      loc_isReadOnly := False;
      loc_isContainment := (not loc_isReference) and pt.getBoolean(s_IsComplex);
    end;
    loc_isNullable := (p.getInteger(s_PropertyMinOccurs) = 0);
    if ( ptSDO = nil ) then
      ptSDO := TranslateType(pt);
    flgs := [];
    if loc_isArray then
      Include(flgs, pfIsMany);
    if loc_isReadOnly then
      Include(flgs, pfIsReadOnly);
    if loc_isContainment then
      Include(flgs, pfIsContainment);
    if loc_isNullable then
      Exclude(flgs, pfIsNotNullable);
    if p.getBoolean(s_IsAttribute) then
      Include(flgs, pfIsAttribute);
    FDataFactory.addProperty(
      new_type,
      p.getString(s_Name),
      ptSDO,
      flgs
    );
  end;

  function IsOpenType() : Boolean;
  var
    locStrBuffer : string;
  begin
    locStrBuffer := FindTag(AElement,Format('%s#%s',[s_xsd_namespace,s_any]));
    Result := not IsStrEmpty(locStrBuffer);
    if not Result then begin
      locStrBuffer := FindTag(AElement,Format('%s#%s',[s_xsd_namespace,s_anyAttribute]));
      Result := not IsStrEmpty(locStrBuffer);
    end;
  end;

  procedure TranslateBaseType();
  var
    base : ISDODataObject;
    sdoBase : ISDOType;
  begin
    base := AElement.getDataObject(s_BaseType);
    if (base = nil) then
      Exit;
    sdoBase := TranslateType(base);
    if (sdoBase <> nil) then
      FDataFactory.setBaseType(new_type,sdoBase);
  end;

var
  loc_uri, loc_name : string;
  memberList : ISDODataObjectList;
  i : PtrInt;
  typFlags : TTypeFlags;
begin
  loc_uri := GetTypeNameSpace(AElement);
  loc_name := AElement.getString(s_Name);
  new_type := FindType(loc_uri,loc_name);
  if ( new_type = nil ) then begin
    typFlags := [];
    if IsOpenType() then
      Include(typFlags,tfIsOpen);
    FDataFactory.AddType(loc_uri,loc_name,typFlags);
    new_type := FDataFactory.getType(loc_uri,loc_name);
    TranslateBaseType();
    memberList := AElement.getList(s_Property);
    for i := 0 to memberList.size() - 1 do
      ProcessProperty(memberList.getDataObject(i));
  end;
  Result := new_type;
end;

function TPasTreeSdoConverter.TranslateEnum(AElement: ISDODataObject): ISDOType;
var
  new_type : ISDOType;
  new_typeX : ISDOTypeEx;
  loc_uri, loc_name : string;
begin
  loc_uri := GetTypeNameSpace(AElement);
  loc_name := AElement.getString(s_Name);
  new_type := FindType(loc_uri,loc_name);
  if ( new_type = nil ) then begin
    FDataFactory.AddType(loc_uri,loc_name,[tfIsDataType]);
    new_type := FDataFactory.getType(loc_uri,loc_name);
    new_typeX := new_type as ISDOTypeEx;
    new_typeX.setBaseType(FDataFactory.getType(sdo_namespace,'string'));
  end;
  Result := new_type;
end;

procedure TPasTreeSdoConverter.TranslateModule(AModule: ISDODataObject);
var
  i : PtrInt;
  ls : ISDODataObjectList;
begin
  ls := AModule.getList(s_Type);
  for i := 0 to ls.size() - 1 do
    TranslateType(ls.getDataObject(i));
  ls := AModule.getList(s_Variable);
  for i := 0 to ls.size() - 1 do
    TranslateVariable(ls.getDataObject(i));
end;

function TPasTreeSdoConverter.TranslateType(AType: ISDODataObject): ISDOType;
var
  locUri, locName : string;
  locType : ISDODataObject;
begin
  Result := nil;
  locType := AType;
  if locType.getBoolean(s_Unresolved) then begin
    locType := Find(FTree,GetTypeNameSpace(AType),AType.getString(s_Name));
    if (locType = nil) then
      locType := AType;
  end;
  locUri := GetTypeNameSpace(locType);
  locName := locType.getString(s_Name);
  if ( Result = nil ) then
    Result := FindType(locUri,locName);
  if ( Result = nil ) then begin
    if locType.getBoolean(s_IsComplex) then
      Result := TranslateClass(locType)
    else if (locType.getList(s_EnumValue).size() > 0) then
      Result := TranslateEnum(locType)
    else if (locType.getByte(s_ElementKind) = ELEMENT_KIND_VARIABLE) then
      Result := TranslateVariable(locType);
  end;
  if ( Result = nil ) then
    raise ESDOException.CreateFmt('Unable to translate this type : Name = "%s"; Uri = "%s".',[locName,locUri]);
end;

{ TSdoPasConverter }

constructor TSdoPasConverter.Create(
  ATree: ISDODataObject;
  ADataFactory: ISDODataFactory;
  ATypeList: ISDOTypeList
);
begin
  FTree := ATree;
  FDataFactory := ADataFactory;
  FTypeList := ATypeList;
end;

procedure TSdoPasConverter.Execute();
var
  c, i : PtrInt;
begin
  c := FTypeList.getCount();
  for i := 0 to Pred(c) do begin
    TranslateType(FTypeList.getItem(i));
  end;
end;

function TSdoPasConverter.FindOrCreateModule(const ANamespace: string): ISDODataObject;
var
  locName : string;
begin
  Result := FindModule(FTree,ANamespace);
  if (Result = nil) then begin
    locName := ExtractIdentifier(ANamespace);
    Result := FTree.createDataObject(s_Module);
    Result.setString(s_Name,locName);
    Result.setString(s_NameSpace,ANamespace);
    FTree.getList(s_Module).append(Result);
    FTree.setDataObject(s_CurrentModule,Result);
  end;
end;

function TSdoPasConverter.FindType(const AUri, AName: string): ISDODataObject;
var
  locElement : ISDODataObject;
begin
  locElement := Find(FTree,AUri,AName);
  if Assigned(locElement) and (locElement.getByte(s_ElementKind) = ELEMENT_KIND_TYPE) then
    Result := locElement
  else
    Result := nil;
end;

function TSdoPasConverter.TranslateAlias(const AType: ISDOType): ISDODataObject;
var
  res : ISDODataObject;
  locModule : ISDODataObject;
begin
  locModule := FindOrCreateModule(AType.getURI());
  res := locModule.createDataObject(s_Variable);
    res.setByte(s_ElementKind,ELEMENT_KIND_VARIABLE);
    res.setString(s_Name,AType.getName());
    res.setDataObject(s_DataType,TranslateType(AType.getBaseType()));
  locModule.getList(s_Variable).append(res);
  Result := res;
end;

function TSdoPasConverter.TranslateChangeSummary(const AType: ISDOType): ISDODataObject;
var
  res : ISDODataObject;
  locModule : ISDODataObject;
begin
  locModule := FindOrCreateModule(AType.getURI());
  res := locModule.createDataObject(s_Type);
    res.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
    res.setString(s_Name,AType.getName());
  locModule.getList(s_Type).append(res);
  Result := res;
end;

function TSdoPasConverter.TranslateObject(const AType: ISDOType): ISDODataObject;
var
  res : ISDODataObject;
  locModule : ISDODataObject;

  procedure TranslateProps();
  var
    propList : ISDOPropertyList;

    procedure TranslateProperty(const AProperty : ISDOProperty);
    var
      propType : ISDODataObject;
      prop : ISDODataObject;
      propTypeSDO : ISDOType;
      isObjRefProp : Boolean;
    begin
      if not AProperty.getContainingType().equals(AType) then
        Exit;
      propTypeSDO := AProperty.getType();
      isObjRefProp := propTypeSDO.isDataObjectType() and AProperty.isReference();
      if isObjRefProp then
        propType := Find(FTree,s_xs,s_anyURI)
      else
        propType := TranslateType(propTypeSDO);
      prop := res.createDataObject(s_Property);
        prop.setString(s_Name,AProperty.getName());
        prop.setDataObject(s_DataType,propType);
        if AProperty.getType().isChangeSummaryType() then
          prop.setInteger(s_PropertyMinOccurs,1);
        if AProperty.isMany() then
          prop.setInteger(s_PropertyMaxOccurs,MaxInt);
        prop.setBoolean(
          s_IsAttribute,
          (not( propTypeSDO.isDataObjectType() or
                isObjRefProp or
                AProperty.isMany() or
                AProperty.getType().isChangeSummaryType()
              )
          ) and
          AProperty.isAttribute()
        );
      res.getList(s_Property).append(prop);
      if isObjRefProp or (propTypeSDO.getTypeEnum() in SDO_SPECIAL_TYPES) then
        SetTagValue(
          prop,
          Format('%s#%s',[sdo_namespace,sdo_consts.s_propertyType]),
          Format('%s#%s',[propTypeSDO.getURI(),propTypeSDO.getName()])
        );
    end;

  var
    k , propCount : PtrUInt;
  begin
    propList := AType.getProperties();
    propCount := propList.getCount();
    if ( propCount > 0 ) then begin
      for k := 0 to Pred(propCount) do begin
        TranslateProperty(propList.getItem(k));
      end;
    end;
  end;

  procedure MarkAsOpenType();
  begin
    SetTagValue(
      res,
      s_xsd_namespace + '#' + s_any,
      s_maxOccurs + '=' + s_unbounded +
        ';' + s_processContents + '=lax'
    );
    SetTagValue(
      res,
      s_xsd_namespace + '#' + s_anyAttribute,
      s_processContents + '=lax'
    );
  end;

  procedure TranslateBaseType();
  var
    base : ISDOType;
    pasBase : ISDODataObject;
  begin
    base := AType.getBaseType();
    if (base = nil) then
      Exit;
    pasBase := TranslateType(base);
    if (pasBase <> nil) then
      res.setDataObject(s_BaseType,pasBase);
  end;

begin
  locModule := FindOrCreateModule(AType.getURI());
  res := locModule.createDataObject(s_Type);
    res.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
    res.setBoolean(s_IsComplex,True);
    res.setString(s_Name,AType.getName());
  locModule.getList(s_Type).append(res);
  TranslateBaseType();
  TranslateProps();
  if AType.isOpenType() then
    MarkAsOpenType();
  Result := res;
end;

function TSdoPasConverter.TranslateType(const AType : ISDOType) : ISDODataObject;
var
  elt : ISDODataObject;
  res : ISDODataObject;
  c, i : PtrInt;
  locURI, locName : TSDOString;
begin
  locURI := AType.getURI();
  locName := AType.getName();
  res := FindType(locURI,locName);
  if ( res = nil ) then begin
    c := AType.getAliasCount();
    for i := 0 to Pred(c) do begin
      res := FindType(locURI,AType.getAlias(i));
      if Assigned(res) then
        Break;
    end;
  end;
  if ( res = nil ) then begin
    if AType.isDataType() then begin
      if AType.isChangeSummaryType() then begin
        res := TranslateChangeSummary(AType);
      end else if ( AType.getBaseType() = nil ) then begin
        elt := Find(FTree,s_xs,locName);
        if not (Assigned(elt) and (elt.getByte(s_ElementKind) = ELEMENT_KIND_TYPE)) then begin
          c := AType.getAliasCount();
          if ( c > 0 ) then begin
            for i := 0 to Pred(c) do begin
              elt := Find(FTree,s_xs,AType.getAlias(i));
              if Assigned(elt) and (elt.getByte(s_ElementKind) = ELEMENT_KIND_TYPE) then
                Break;
            end;
          end;
        end;
        if Assigned(elt) and (elt.getByte(s_ElementKind) = ELEMENT_KIND_TYPE) then
          res := elt
        else
          res := Find(FTree,s_xs,'string');
      end else begin
        res := TranslateAlias(AType);
      end;
    end else if AType.isChangeSummaryType() then begin
      res := TranslateChangeSummary(AType);
    end else begin
      res := TranslateObject(AType);
    end;
  end;
  Result := res;
end;

end.
