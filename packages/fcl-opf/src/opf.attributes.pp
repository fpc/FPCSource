{
    This file is part of the Free Pascal component  library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Attribute definitions for attribute-based persister

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Attributes;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo,
{$ELSE}
  Classes, SysUtils, typinfo,
{$ENDIF}
  Opf.Fields;

Type
  TOPPersistKind = (pkProperties, pkFields, pkBoth);

  { OPTableName - override table name for a class }

  OPTableName = class(TCustomAttribute)
  private
    FName: String;
  public
    { Sets the name of the database table for the class. }
    constructor Create(const aName: String);
    { The name of the database table. }
    property Name: String read FName;
  end;

  { OPVisibility - set which visibility sections to scan }

  OPVisibility = class(TCustomAttribute)
  private
    FVisibilities: TVisibilityClasses;
  public
    { Sets which visibility sections (public, published, ...) are looked at for fields to store. }
    constructor Create(aVisibilities: TVisibilityClasses);
    { The visibility sections that are looked at. }
    property Visibilities: TVisibilityClasses read FVisibilities;
  end;

  { OPPersistKind - persist properties, fields, or both }

  OPPersistKind = class(TCustomAttribute)
  private
    FKind: TOPPersistKind;
  public
    { Sets whether properties, fields or both are stored. }
    constructor Create(aKind: TOPPersistKind);
    { Whether properties, fields or both are stored. }
    property Kind: TOPPersistKind read FKind;
  end;

  { OPFieldName - override column name; also acts as explicit include }

  OPFieldName = class(TCustomAttribute)
  private
    FName: String;
  public
    { Sets the name of the column in the database table. }
    constructor Create(const aName: String);
    { The name of the column in the database table. }
    property Name: String read FName;
  end;

  { OPSkip - exclude from persistence }

  OPSkip = class(TCustomAttribute)
  public
    { Marks a property or field as not stored. }
    constructor Create;
  end;

  { OPFieldOptions - set field options }

  OPFieldOptions = class(TCustomAttribute)
  private
    FOptions: TOPFieldOptions;
  public
    { Sets the options of a stored field. }
    constructor Create(aOptions: TOPFieldOptions);
    { The options of the stored field. }
    property Options: TOPFieldOptions read FOptions;
  end;

  { OPPrimaryKey - declare primary key fields and optional constraint name }

  OPPrimaryKey = class(TCustomAttribute)
  private
    FFields: String;
    FConstraintName: String;
  public
    { Sets the primary key fields, without a constraint name. }
    constructor Create(const aFields: String);
    { Sets the primary key fields and the name of the primary key constraint. }
    constructor Create(const aFields, aConstraintName: String);
    { The fields that make up the primary key. }
    property Fields: String read FFields;
    { The name of the primary key constraint, empty if not set. }
    property ConstraintName: String read FConstraintName;
  end;

  { OPIndex - declare database index }

  OPIndex = class(TCustomAttribute)
  private
    FFields: String;
    FIndexName: String;
  public
    { Declares an index with the given name on the given fields. }
    constructor Create(const aFields, aIndexName: String);
    { The fields in the index. }
    property Fields: String read FFields;
    { The name of the index. }
    property IndexName: String read FIndexName;
  end;

  { OPRelation - declare foreign key relationship.
    Uses string class name because FPC attributes require constant expressions. }

  OPRelation = class(TCustomAttribute)
  private
    FParentClassName: String;
    FFKName: String;
    FDBCascade: Boolean;
  public
    { Links the field to the parent class, with no foreign key name and no cascading delete. }
    constructor Create(const aParentClassName: String);
    { Links the field to the parent class with the given foreign key name, no cascading delete. }
    constructor Create(const aParentClassName, aFKName: String);
    { Links the field to the parent class with a foreign key name, and sets the cascading delete. }
    constructor Create(const aParentClassName, aFKName: String; aDBCascade: Boolean);
    { The name of the parent class, optionally prefixed with its unit name. }
    property ParentClassName: String read FParentClassName;
    { The name of the foreign key constraint in the database; empty for a generated name. }
    property FKName: String read FFKName;
    { True if the database deletes child records when the parent record is deleted. }
    property DBCascade: Boolean read FDBCascade;
  end;

  { OPSequence - set sequence name for value generation }

  OPSequence = class(TCustomAttribute)
  private
    FSequenceName: String;
  public
    { Sets the name of the database sequence that generates values for the field. }
    constructor Create(const aSequenceName: String);
    { The name of the database sequence. }
    property SequenceName: String read FSequenceName;
  end;

  { OPSize - set field size constraints }

  OPSize = class(TCustomAttribute)
  private
    FMaxSize: Integer;
    FMinSize: Integer;
  public
    { Sets the maximum size of the field; the minimum size is 0. }
    constructor Create(aMaxSize: Integer);
    { Sets both the maximum and the minimum size of the field. }
    constructor Create(aMaxSize, aMinSize: Integer);
    { The maximum size of the field. }
    property MaxSize: Integer read FMaxSize;
    { The minimum size of the field. }
    property MinSize: Integer read FMinSize;
  end;

  { OPUnique - mark field as UNIQUE }

  OPUnique = class(TCustomAttribute)
  public
    { Marks the field as having unique values in the database table. }
    constructor Create;
  end;

  { OPDefault - set default value for DDL generation }

  OPDefault = class(TCustomAttribute)
  private
    FValue: String;
  public
    { Sets the default value of the column when the database table is created. }
    constructor Create(const aValue: String);
    { The default value, as text. }
    property Value: String read FValue;
  end;

{ Attribute reading helpers }

function FindClassAttribute(aClass: TClass; aAttrClass: TClass): TCustomAttribute;
function HasClassAttribute(aClass: TClass; aAttrClass: TClass): Boolean;
function FindPropAttribute(aPropInfo: PPropInfo; aAttrClass: TClass): TCustomAttribute;
function HasPropAttribute(aPropInfo: PPropInfo; aAttrClass: TClass): Boolean;
function FindFieldAttribute(aFieldEntry: PExtendedVmtFieldEntry; aAttrClass: TClass): TCustomAttribute;
function HasFieldAttribute(aFieldEntry: PExtendedVmtFieldEntry; aAttrClass: TClass): Boolean;

{ Utility }
function FieldToColumnName(const aFieldName: String): String;

{ Attribute aIndex of an attribute table. RTTI creates a new instance on every
  call; this one is created once, owned by this unit: do not free it. }
function OPFCachedAttribute(aTable: PAttributeTable; aIndex: Integer): TCustomAttribute;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Contnrs;
{$ELSE}
  contnrs;
{$ENDIF}

{ RTTI creates a new attribute instance on every GetAttribute call. Each
  attribute is created once and kept here; they are freed at the end. }

var
  AttributeCache: TFPHashObjectList;
  AttributeLock: TRTLCriticalSection;

function OPFCachedAttribute(aTable: PAttributeTable; aIndex: Integer): TCustomAttribute;
var
  Key: ShortString;
begin
  Key:=HexStr(PtrUInt(aTable), SizeOf(Pointer)*2)+':'+IntToStr(aIndex);
  EnterCriticalSection(AttributeLock);
  try
    Result:=TCustomAttribute(AttributeCache.Find(Key));
    if Result=nil then
    begin
      Result:=GetAttribute(aTable, aIndex);
      if Result<>nil then
        AttributeCache.Add(Key, Result);
    end;
  finally
    LeaveCriticalSection(AttributeLock);
  end;
end;

{ OPTableName }

constructor OPTableName.Create(const aName: String);
begin
  inherited Create;
  FName:=aName;
end;

{ OPVisibility }

constructor OPVisibility.Create(aVisibilities: TVisibilityClasses);
begin
  inherited Create;
  FVisibilities:=aVisibilities;
end;

{ OPPersistKind }

constructor OPPersistKind.Create(aKind: TOPPersistKind);
begin
  inherited Create;
  FKind:=aKind;
end;

{ OPFieldName }

constructor OPFieldName.Create(const aName: String);
begin
  inherited Create;
  FName:=aName;
end;

{ OPSkip }

constructor OPSkip.Create;
begin
  inherited Create;
end;

{ OPFieldOptions }

constructor OPFieldOptions.Create(aOptions: TOPFieldOptions);
begin
  inherited Create;
  FOptions:=aOptions;
end;

{ OPPrimaryKey }

constructor OPPrimaryKey.Create(const aFields: String);
begin
  inherited Create;
  FFields:=aFields;
  FConstraintName:='';
end;

constructor OPPrimaryKey.Create(const aFields, aConstraintName: String);
begin
  inherited Create;
  FFields:=aFields;
  FConstraintName:=aConstraintName;
end;

{ OPIndex }

constructor OPIndex.Create(const aFields, aIndexName: String);
begin
  inherited Create;
  FFields:=aFields;
  FIndexName:=aIndexName;
end;

{ OPRelation }

constructor OPRelation.Create(const aParentClassName: String);
begin
  inherited Create;
  FParentClassName:=aParentClassName;
  FFKName:='';
  FDBCascade:=False;
end;

constructor OPRelation.Create(const aParentClassName, aFKName: String);
begin
  inherited Create;
  FParentClassName:=aParentClassName;
  FFKName:=aFKName;
  FDBCascade:=False;
end;

constructor OPRelation.Create(const aParentClassName, aFKName: String; aDBCascade: Boolean);
begin
  inherited Create;
  FParentClassName:=aParentClassName;
  FFKName:=aFKName;
  FDBCascade:=aDBCascade;
end;

{ OPSequence }

constructor OPSequence.Create(const aSequenceName: String);
begin
  inherited Create;
  FSequenceName:=aSequenceName;
end;

{ OPSize }

constructor OPSize.Create(aMaxSize: Integer);
begin
  inherited Create;
  FMaxSize:=aMaxSize;
  FMinSize:=0;
end;

constructor OPSize.Create(aMaxSize, aMinSize: Integer);
begin
  inherited Create;
  FMaxSize:=aMaxSize;
  FMinSize:=aMinSize;
end;

{ OPUnique }

constructor OPUnique.Create;
begin
  inherited Create;
end;

{ OPDefault }

constructor OPDefault.Create(const aValue: String);
begin
  inherited Create;
  FValue:=aValue;
end;

{ Attribute reading helpers }

function FindClassAttribute(aClass: TClass; aAttrClass: TClass): TCustomAttribute;
var
  AT: PAttributeTable;
  I: Integer;
  Attr: TCustomAttribute;
begin
  Result:=nil;
  AT:=GetAttributeTable(aClass.ClassInfo);
  if AT=nil then Exit;
  for I:=0 to AT^.AttributeCount-1 do
  begin
    Attr:=OPFCachedAttribute(AT, I);
    if Attr is aAttrClass then
      Exit(Attr);
  end;
end;

function HasClassAttribute(aClass: TClass; aAttrClass: TClass): Boolean;
begin
  Result:=FindClassAttribute(aClass, aAttrClass) <> nil;
end;

function FindPropAttribute(aPropInfo: PPropInfo; aAttrClass: TClass): TCustomAttribute;
var
  AT: PAttributeTable;
  I: Integer;
  Attr: TCustomAttribute;
begin
  Result:=nil;
  AT:=aPropInfo^.AttributeTable;
  if AT=nil then Exit;
  for I:=0 to AT^.AttributeCount-1 do
  begin
    Attr:=OPFCachedAttribute(AT, I);
    if Attr is aAttrClass then
      Exit(Attr);
  end;
end;

function HasPropAttribute(aPropInfo: PPropInfo; aAttrClass: TClass): Boolean;
begin
  Result:=FindPropAttribute(aPropInfo, aAttrClass) <> nil;
end;

function FindFieldAttribute(aFieldEntry: PExtendedVmtFieldEntry; aAttrClass: TClass): TCustomAttribute;
var
  AT: PAttributeTable;
  I: Integer;
  Attr: TCustomAttribute;
begin
  Result:=nil;
  AT:=aFieldEntry^.AttributeTable;
  if AT=nil then Exit;
  for I:=0 to AT^.AttributeCount-1 do
  begin
    Attr:=OPFCachedAttribute(AT, I);
    if Attr is aAttrClass then
      Exit(Attr);
  end;
end;

function HasFieldAttribute(aFieldEntry: PExtendedVmtFieldEntry; aAttrClass: TClass): Boolean;
begin
  Result:=FindFieldAttribute(aFieldEntry, aAttrClass) <> nil;
end;

function FieldToColumnName(const aFieldName: String): String;
begin
  if (Length(aFieldName) > 1) and (aFieldName[1] = 'F') and (aFieldName[2] in ['A'..'Z']) then
    Result:=Copy(aFieldName, 2, MaxInt)
  else
    Result:=aFieldName;
end;

initialization
  InitCriticalSection(AttributeLock);
  AttributeCache:=TFPHashObjectList.Create(True);

finalization
  FreeAndNil(AttributeCache);
  DoneCriticalSection(AttributeLock);

end.
