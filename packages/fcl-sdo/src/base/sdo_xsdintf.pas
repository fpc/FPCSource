{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements basic XSD - SDO type mapping

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_xsdintf;

interface
uses
  SysUtils,
  sdo;

const
{ Type tree :
    s_TypeTreeType
      s_Module (s_ModuleType)
        s_Type (s_SchemaTypeType)
        s_Variable (s_VariableType)
      s_Unresolved (s_SchemaTypeType)
      s_UnresolvedLink (s_UnresolvedLink)


  Type Tree properties
    s_NameValueType
      Name (String)
      Value (String)

    s_ElementType
      Name (String)
      NameSpace (String, if in module use the module name instead of this!)
      ElementKind (Byte, ELEMENT_KIND_TYPE, ELEMENT_KIND_VARIABLE, ...)
      Tag (array, s_NameValueType)

    s_TypeTreeType (inherits s_ElementType)
      Module (array, s_ModuleType)
      CurrentModule (s_ModuleType)
      Unresolved (s_SchemaTypeType)

    s_Module_Type (inherits s_ElementType)
      Native (Boolean)
      Type (array, s_SchemaTypeType)
      Variable (array, s_VariableType)

    s_SchemaTypeType (inherits s_ElementType)
      IsComplex (Boolean)
      BaseType (s_SchemaTypeType)
      Unresolved (Boolean)
      Embedded (Boolean)
      Property (array, s_PropertyType)
      EnumValue(array, String)

    s_VariableType (inherits s_ElementType)
      DataType (s_SchemaTypeType)

    s_PropertyType (inherits s_ElementType)
      DataType (s_SchemaTypeType)
      IsAttribute (Boolean)
      DefaultValue (String)
      MinOccurs (Integer)
      MaxOccurs (Integer)

    s_UnresolvedLinkType
      Element (s_SchemaTypeType)
      Target (s_SchemaTypeType)
      LinkKind (String)
      Resolved (Boolean)
}
  s_XsdParserNS       = 'xsd:parser:ns';
  s_SchemaTypeType    = 'SchemaTypeType';

  s_BaseType          = 'BaseType';
  s_CurrentModule     = 'CurrentModule';
  s_DataType          = 'DataType';
  s_DefaultValue      = 'DefaultValue';
  s_Element           = 'Element';
  s_ElementKind       = 'ElementKind';
  s_ElementType       = 'ElementType';
  s_Embedded          = 'Embedded';
  s_EnumValue         = 'EnumValue';
  s_IsAttribute       = 'IsAttribute';
  s_IsComplex         = 'IsComplex';
  s_LinkKind          = 'LinkKind';
  s_Module            = 'Module';
  s_ModuleType        = 'ModuleType';
  s_Name              = 'Name';
  s_NameSpace         = 'NameSpace';
  s_NameValueType     = 'NameValueType';
  s_Native            = 'Native';
  s_Property          = 'Property';
  s_PropertyMaxOccurs = 'MaxOccurs';
  s_PropertyMinOccurs = 'MinOccurs';
  s_PropertyType      = 'PropertyType';
  s_Resolved          = 'Resolved';
  s_Tag               = 'Tag';
  s_Target            = 'Target';
  s_Type              = 'Type';
  s_TypeTreeType      = 'TypeTreeType';
  s_Unresolved        = 'Unresolved';
  s_UnresolvedLink    = 'UnresolvedLink';
  s_UnresolvedLinkType= 'UnresolvedLinkType';
  s_UnresolvedType    = 'UnresolvedType';
  s_Value             = 'Value';
  s_Variable          = 'Variable';
  s_VariableType      = 'VariableType';

  ELEMENT_KIND_TYPE       = TSDOByte(1);
  ELEMENT_KIND_VARIABLE   = TSDOByte(2);
  ELEMENT_KIND_PROPERTY   = TSDOByte(4);

  LINK_KIND_BASE_TYPE = 'BASE_TYPE';
  LINK_KIND_PROP_TYPE = 'PROPERTY_TYPE';

  XSD_NAME_SPACE = 'http://www.w3.org/2001/XMLSchema';
  XSD_TYPES : array[0..26] of string = (
    'anyType', 'anyURI',
    'base64Binary', 'boolean', 'byte',
    'date', 'dateTime', 'decimal', 'double', 'duration',
    'float',
    'hexBinary',
    'ID', 'int',
    'language', 'long',
    'nonNegativeInteger',
    'positiveInteger',
    'schema', 'short', 'string',
    'time', 'token',
    'unsignedByte', 'unsignedInt', 'unsignedLong', 'unsignedShort'
  );

  procedure AddTypeTree(const AFactory : ISDODataFactory);
  function FindInModule(
    const AModule : ISDODataObject;
    const AName   : string
  ) : ISDODataObject;
  function FindVariableInModule(
    const AModule : ISDODataObject;
    const AName   : string
  ) : ISDODataObject;
  function Find(
    const ATypeTree : ISDODataObject;
    const AName     : string
  ) : ISDODataObject; overload;
  function Find(
    const ATypeTree : ISDODataObject;
    const ANameSpace,
          AName     : string
  ) : ISDODataObject; overload;
  function FindVariable(
    const ATypeTree : ISDODataObject;
    const AName     : string
  ) : ISDODataObject;
  function FindModule(
    const ATypeTree  : ISDODataObject;
    const AName : string
  ) : ISDODataObject;

  procedure SetTagValue(
    const AObject : ISDODataObject;
    const AName,
          AValue  : string
  );

  procedure AddXsdTypes(ATypeTree  : ISDODataObject);

  function GetUltimeType(AType : ISDODataObject) : ISDODataObject;
  function FindTag(
          AObject  : ISDODataObject;
    const ATagName : TSDOString
  ) : TSDOString; overload;
  function FindTag(
          AObject  : ISDODataObject;
    const ATagName : TSDOString;
    var   AResult  : TSDOString
  ) : Boolean; overload;

  function GetVariableNameSpace(AVariable : ISDODataObject) : string;
  function GetTypeNameSpace(AType : ISDODataObject) : string;
  function IsNativeType(AType : ISDODataObject) : Boolean;


implementation

procedure AddTypeTree(const AFactory : ISDODataFactory);
var
  f, m, e, st, nv, vt, pt, ul : ISDOType;
begin
  f := AFactory.getTypes().find(s_XsdParserNS,s_TypeTreeType);
  if (f <> nil) then
    Exit;

  AFactory.AddType(s_XsdParserNS,s_ModuleType,[]);
  AFactory.AddType(s_XsdParserNS,s_TypeTreeType,[]);
  AFactory.AddType(s_XsdParserNS,s_ElementType,[]);
  AFactory.AddType(s_XsdParserNS,s_SchemaTypeType,[]);
  AFactory.AddType(s_XsdParserNS,s_VariableType,[]);
  AFactory.AddType(s_XsdParserNS,s_PropertyType,[]);
  AFactory.AddType(s_XsdParserNS,s_UnresolvedLinkType,[]);
  AFactory.AddType(s_XsdParserNS,s_NameValueType,[]);

  f := AFactory.getType(s_XsdParserNS,s_TypeTreeType);
  m := AFactory.getType(s_XsdParserNS,s_ModuleType);
  e := AFactory.getType(s_XsdParserNS,s_ElementType);
  st := AFactory.getType(s_XsdParserNS,s_SchemaTypeType);
  nv := AFactory.getType(s_XsdParserNS,s_NameValueType);
  vt := AFactory.getType(s_XsdParserNS,s_VariableType);
  pt := AFactory.getType(s_XsdParserNS,s_PropertyType);
  ul := AFactory.getType(s_XsdParserNS,s_UnresolvedLinkType);

  AFactory.addProperty(f,s_Module,m,[pfIsMany,pfIsContainment,pfIsNotNullable]);
  AFactory.addProperty(f,s_CurrentModule,m,[]);
  AFactory.addProperty(f,s_Unresolved,st,[pfIsMany,pfIsContainment,pfIsNotNullable]);
  AFactory.addProperty(f,s_UnresolvedLink,ul,[pfIsMany,pfIsContainment,pfIsNotNullable]);

  AFactory.setBaseType(m,e);
  AFactory.addProperty(m,s_Native,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
  AFactory.addProperty(m,s_Type,st,[pfIsMany,pfIsContainment,pfIsNotNullable]);
  AFactory.addProperty(m,s_Variable,vt,[pfIsMany,pfIsContainment,pfIsNotNullable]);

  AFactory.addProperty(e,s_Name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  AFactory.addProperty(e,s_NameSpace,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  AFactory.addProperty(e,s_ElementKind,sdo_namespace,SDOTypeDefaultTypeNames[ByteType],[pfIsReadOnly,pfIsNotNullable]);
  AFactory.addProperty(e,s_Tag,s_XsdParserNS,s_NameValueType,[pfIsMany,pfIsContainment]);

  AFactory.setBaseType(st,e);
  AFactory.addProperty(st,s_IsComplex,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
  AFactory.addProperty(st,s_BaseType,st,[]);
  AFactory.addProperty(st,s_Unresolved,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
  AFactory.addProperty(st,s_Embedded,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
  AFactory.addProperty(st,s_Property,pt,[pfIsMany,pfIsContainment,pfIsNotNullable]);
  AFactory.addProperty(st,s_EnumValue,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[pfIsMany]);
  //st.getProperty(s_ElementKind).setDefault(ELEMENT_KIND_TYPE);

  AFactory.setBaseType(vt,e);
  AFactory.addProperty(vt,s_DataType,st,[]);
  //vt.getProperty(s_ElementKind).setDefault(ELEMENT_KIND_VARIABLE);

  AFactory.setBaseType(pt,e);
  //AFactory.addProperty(pt,s_DataType,st,[]);
  AFactory.addProperty(pt,s_DataType,sdo_namespace,SDOTypeDefaultTypeNames[ObjectType],[]);
  AFactory.addProperty(pt,s_IsAttribute,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);
  AFactory.addProperty(pt,s_PropertyMinOccurs,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
  AFactory.addProperty(pt,s_PropertyMaxOccurs,sdo_namespace,SDOTypeDefaultTypeNames[IntegerType],[]);
  AFactory.addProperty(pt,s_DefaultValue,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  //vt.getProperty(s_ElementKind).setDefault(ELEMENT_KIND_PROPERTY);

  AFactory.addProperty(nv,s_Name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  AFactory.addProperty(nv,s_Value,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);


  AFactory.addProperty(ul,s_Element,st,[]);
  AFactory.addProperty(ul,s_Target,st,[]);
  AFactory.addProperty(ul,s_LinkKind,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  AFactory.addProperty(ul,s_Name,sdo_namespace,SDOTypeDefaultTypeNames[StringType],[]);
  AFactory.addProperty(ul,s_Resolved,sdo_namespace,SDOTypeDefaultTypeNames[BooleanType],[]);

end;

function InheritsFrom(const AChild, AParent : ISDOType) : Boolean;
begin
  Result := False;
  if (AChild = nil) or (AParent = nil)
  then
    Exit;

  Result := (AChild <> nil) and
            (AParent <> nil) and
            (AChild.equals(AParent) or InheritsFrom(AChild.getBaseType(),AParent));
end;

function FindInModule(
  const AModule : ISDODataObject;
  const AName   : string
) : ISDODataObject;
begin
  Result := AModule.getDataObject(Format('%s[%s=%s]',[s_Type,s_Name,QuotedStr(AName)]));
  if (Result = nil) then
    Result := AModule.getDataObject(Format('%s[%s=%s]',[s_Variable,s_Name,QuotedStr(AName)]));
end;

function FindVariableInModule(
  const AModule : ISDODataObject;
  const AName   : string
) : ISDODataObject;
begin
  Result := AModule.getDataObject(Format('%s[%s=%s]',[s_Variable,s_Name,QuotedStr(AName)]));
end;

function Find(
  const ATypeTree : ISDODataObject;
  const AName     : string
) : ISDODataObject;
var
  locCurrentModule, locModule : ISDODataObject;
  locModules : ISDODataObjectList;
  i : Integer;
  locRes : ISDODataObject;
begin
  locCurrentModule := ATypeTree.getDataObject(s_CurrentModule);
  if (locCurrentModule <> nil) then
    Result := FindInModule(locCurrentModule,AName);
  if (Result = nil) then begin
    locModules := ATypeTree.getList(s_Module);
    if (locModules.size() > 0) then begin
      for i := 0 to locModules.size() - 1 do begin
        locModule := locModules.getDataObject(i);
        if (locModule <> locCurrentModule) then begin
          locRes := FindInModule(locModule,AName);
          if (locRes <> nil) then begin
            Result := locRes;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function FindVariable(
  const ATypeTree : ISDODataObject;
  const AName     : string
) : ISDODataObject;
var
  locCurrentModule, locModule : ISDODataObject;
  locModules : ISDODataObjectList;
  i : Integer;
  locRes : ISDODataObject;
begin
  locCurrentModule := ATypeTree.getDataObject(s_CurrentModule);
  if (locCurrentModule <> nil) then
    Result := FindVariableInModule(locCurrentModule,AName);
  if (Result = nil) then begin
    locModules := ATypeTree.getList(s_Module);
    if (locModules.size() > 0) then begin
      for i := 0 to locModules.size() - 1 do begin
        locModule := locModules.getDataObject(i);
        if (locModule <> locCurrentModule) then begin
          locRes := FindVariableInModule(locModule,AName);
          if (locRes <> nil) then begin
            Result := locRes;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function Find(
  const ATypeTree : ISDODataObject;
  const ANameSpace,
        AName     : string
) : ISDODataObject;
var
  locModule : ISDODataObject;
begin
  locModule := ATypeTree.getDataObject(Format('%s[%s=%s]',[s_Module,s_NameSpace,QuotedStr(ANameSpace)]));
  if (locModule <> nil) then
    Result := FindInModule(locModule,AName);
end;

function FindModule(
  const ATypeTree  : ISDODataObject;
  const AName : string
) : ISDODataObject;
begin
  Result := ATypeTree.getDataObject(Format('%s[%s=%s]',[s_Module,s_Name,QuotedStr(AName)]));
  if (Result = nil) then
    Result := ATypeTree.getDataObject(Format('%s[%s=%s]',[s_Module,s_NameSpace,QuotedStr(AName)]));
end;

procedure SetTagValue(
  const AObject : ISDODataObject;
  const AName,
        AValue  : string
);
var
  locTag : ISDODataObject;
begin
  locTag := AObject.getDataObject(Format('%s[%s=%s]',[s_Tag,s_Name,QuotedStr(AName)]));
  if (locTag = nil) then begin
    locTag := AObject.createDataObject(s_Tag);
    locTag.setString(s_Name,AName);
    AObject.getList(s_Tag).append(locTag);
  end;
  locTag.setString(s_Value,AValue);
end;

procedure AddXsdTypes(ATypeTree  : ISDODataObject);
var
  locModule, locType : ISDODataObject;
  locTypeList : ISDODataObjectList;
  i : Integer;
begin
  locModule := FindModule(ATypeTree,XSD_NAME_SPACE);
  if (locModule <> nil) then
    exit;

  locModule := ATypeTree.createDataObject(s_Module);
  locModule.setBoolean(s_Native,True);
  locModule.setString(s_NameSpace,XSD_NAME_SPACE);
  locModule.setString(s_Name,XSD_NAME_SPACE);
  ATypeTree.getList(s_Module).append(locModule);

  locTypeList := locModule.getList(s_Type);
  for i := Low(XSD_TYPES) to High(XSD_TYPES) do begin
    locType := locModule.createDataObject(s_Type);
    locType.setByte(s_ElementKind,ELEMENT_KIND_TYPE);
    locType.setString(s_Name,XSD_TYPES[i]);
    locTypeList.append(locType);
  end;
end;

function GetUltimeType(AType : ISDODataObject) : ISDODataObject;
var
  p, q : ISDODataObject;
begin
  if (AType <> nil) then begin
    p := AType;
    if (p.getByte(s_ElementKind) = ELEMENT_KIND_TYPE) then begin
      Result := p;
      Exit;
    end;
    if (p.getByte(s_ElementKind) = ELEMENT_KIND_VARIABLE) then begin
      q := p.getDataObject(s_DataType);
      if (q <> nil) then begin
        p := q;
        q := GetUltimeType(q);
        if (q <> nil) then
          p := q;
      end;
    end;
    Result := p;
  end;
end;

function FindTag(
        AObject  : ISDODataObject;
  const ATagName : TSDOString;
  var   AResult  : TSDOString
) : Boolean;
var
  tagObj : ISDODataObject;
begin
  tagObj := AObject.getDataObject(Format('%s[%s=%s]',[s_Tag,s_Name,QuotedStr(ATagName)]));
  Result := (tagObj <> nil);
  if Result then
    AResult := tagObj.getString(s_Value);
end;

function FindTag(
        AObject  : ISDODataObject;
  const ATagName : TSDOString
) : TSDOString;
begin
  if not FindTag(AObject,ATagName,Result) then
    Result := '';
end;

function GetTypeNameSpace(AType : ISDODataObject) : string;

  function GetParentNS() : string;
  var
    locParent : ISDODataObject;
  begin
    locParent := AType.getContainer();
    if (locParent <> nil) then
      Result := locParent.getString(s_namespace);
  end;

var
  locRes : string;
begin
  locRes := AType.getString(s_namespace);
  if (Trim(locRes) = '') then
    locRes := GetParentNS();
  Result := Trim(locRes);
end;

function GetVariableNameSpace(AVariable : ISDODataObject) : string;
begin
  Result := GetTypeNameSpace(AVariable)
end;

function IsNativeType(AType : ISDODataObject) : Boolean;
var
  locParent : ISDODataObject;
begin
  Result := False;
  if (AType <> nil) then begin
    locParent := AType.getContainer();
    if AType.getBoolean(s_Unresolved) then
      locParent := FindModule(locParent,AType.getString(s_NameSpace));
    if (locParent <> nil) then
      Result := locParent.getBoolean(s_Native);
  end
end;

end.
