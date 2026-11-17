{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Effective value and null state of mapped fields

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.FieldValue;

{ The persisters and the change trackers must agree on what is stored for a
  field: a field that is written as NULL must also be seen as NULL by the
  dirty tracking, otherwise setting a field to NULL is not detected.
  This unit is the single place where that is decided. }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants, Data.Db,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, DB,
{$ENDIF}
  Opf.Fields;

type
  { Reads a field that is not a published property (Level 2 TNullable<T>
    record fields). Returns False when the field cannot be read.
    A field without value is returned as Null. }
  TOPFNullableMemberReader = function(aInstance: TObject; aField: TOPFieldMap; out aValue: Variant): Boolean;

var
  { Set by Opf.Persister.ExtRTTI (FPC 3.3.1+) }
  OPFNullableMemberReader: TOPFNullableMemberReader = Nil;

{ True if the field map entry is a Level 2 nullable record field }
function OPFIsNullableMember(aField: TOPFieldMap): Boolean;

{ Null state of a published property.
  aNullable: the field is nullable (foNullable).
  Level 1: an object implementing IOPFNullable decides through IsNull.
  Level 0: a property with a dynamic stored specifier is null when not stored. }
function OPFIsPropNull(aInstance: TObject; const aPropName: UTF8String;
  aPropInfo: PPropInfo; aNullable: Boolean): Boolean;

{ Null state of a mapped field: what the persister writes as SQL NULL }
function OPFIsFieldNull(aInstance: TObject; aField: TOPFieldMap): Boolean;

{ Set the null state of a published property.
  Level 1 through IOPFNullable, Level 0 only for a field-based stored specifier.
  Returns False when the null state cannot be set. }
function OPFSetPropNull(aInstance: TObject; const aPropName: UTF8String;
  aPropInfo: PPropInfo; aIsNull: Boolean): Boolean;

{ The value of a mapped field as stored in the database would see it:
  Null when the field is null, otherwise the property value
  (class properties as text: TStrings.Text or JSON).
  Returns False when the field cannot be read; aValue is then Unassigned. }
function OPFReadFieldValue(aInstance: TObject; aField: TOPFieldMap; out aValue: Variant): Boolean;

{ True when both values are equal for change detection.
  Empty and Null only equal themselves, a different variant type counts as
  a change, and the comparison never raises. }
function OPFSameVariant(const aLeft, aRight: Variant): Boolean;

{ Set a parameter from an array of const element.
  Strings are stored as UTF-8, QWord values above High(Int64) raise EOPF. }
procedure OPFAssignVarRec(const aVarRec: TVarRec; aParam: TParam);

{ Set a parameter from a variant, using the variant type for the parameter type }
procedure OPFAssignVariant(const aValue: Variant; aParam: TParam);

{ Set a string parameter to UTF-8 text. The parameter becomes ftString with
  UTF-8 content: sqldb converts it to the connection character set
  (TSQLConnection.GetAsString), without a detour through the system code page.
  Do not use ftWideString: up to FPC 3.2.2 (and trunk before the fix),
  TSQLConnection.GetAsSQLText does not quote ftWideString values, so drivers
  that substitute parameters into the SQL text (MySQL, MSSQL, unprepared
  PostgreSQL statements) would paste the value unquoted (SQL injection), and
  Oracle rejects ftWideString parameters. }
procedure OPFSetUTF8Param(aParam: TParam; const aValue: UTF8String);

{ Text of a string field as UTF-8, independent of the system code page }
function OPFGetUTF8Field(aField: TField): UTF8String;

{ True for an unsigned 32-bit integer field (Cardinal, LongWord, DWord) }
function OPFIsUnsigned32(aField: TOPFieldMap): Boolean;

{ True for an integer field that needs a 64-bit column:
  Int64, QWord and unsigned 32-bit integers }
function OPFIsWideOrdinal(aField: TOPFieldMap): Boolean;

{ An unsigned 64-bit value as stored in a signed 64-bit column.
  aValue is the raw ordinal value (GetOrdProp); negative means above High(Int64). }
function OPFCheckQWord(aValue: Int64): Int64;

{ A character property as text (UTF-8), #0 as empty string.
  An AnsiChar is converted from the system code page; when that is UTF-8 (or
  another multi-byte code page), a byte above 127 is taken as Latin-1. }
function OPFCharToText(aKind: TTypeKind; aOrd: Int64): UTF8String;

{ The ordinal value of a character property from text, empty string as #0.
  The inverse of OPFCharToText; a character an AnsiChar cannot hold gives '?'. }
function OPFTextToChar(aKind: TTypeKind; aField: TField): Int64;

{ A tkFloat value as the change trackers compare it: Currency as Currency,
  Comp as Int64 and Extended (where it is larger than Double) in an exact
  text form, so a change in the last digits is not lost in a Double. }
function OPFFloatVariant(aField: TOPFieldMap; const aValue: Extended): Variant;

implementation

uses
  Opf.Nullable, Opf.Json, Opf.Types, Opf.Consts;

procedure OPFSetUTF8Param(aParam: TParam; const aValue: UTF8String);
begin
  aParam.AsUTF8String:=aValue;
end;

function OPFGetUTF8Field(aField: TField): UTF8String;
begin
  Result:=aField.AsUTF8String;
end;

function OPFIsUnsigned32(aField: TOPFieldMap): Boolean;
begin
  Result:=(aField.PropertyKind=tkInteger)
          and Assigned(aField.PropertyTypeInfo)
          and (GetTypeData(aField.PropertyTypeInfo)^.OrdType=otULong);
end;

function OPFIsWideOrdinal(aField: TOPFieldMap): Boolean;
begin
  Result:=(aField.PropertyKind in [tkInt64, tkQWord]) or OPFIsUnsigned32(aField);
end;

function OPFCheckQWord(aValue: Int64): Int64;
begin
  if aValue<0 then
    raise EOPF.CreateFmt(SErrQWordOutOfRange, [QWord(aValue)]);
  Result:=aValue;
end;

{ True when an AnsiChar is a character of the system code page: false for
  UTF-8, where a single byte above 127 is no character, and for ASCII. }

function AnsiCharIsSystemCP: Boolean;
begin
  Result:=(DefaultSystemCodePage<>CP_UTF8) and (DefaultSystemCodePage<>CP_NONE)
          and (DefaultSystemCodePage<>CP_ASCII);
end;

function OPFCharToText(aKind: TTypeKind; aOrd: Int64): UTF8String;
var
  S: RawByteString;
begin
  if aOrd=0 then
    Result:=''
  else if aKind<>tkChar then
    Result:=UTF8Encode(UnicodeString(WideChar(aOrd)))
  else if aOrd<128 then
    Result:=AnsiChar(aOrd)
  else if AnsiCharIsSystemCP then
  begin
    S:=AnsiChar(aOrd);
    SetCodePage(S, DefaultSystemCodePage, False);
    Result:=UTF8Encode(UnicodeString(S));
  end
  else
    // Latin-1: the byte is the code point
    Result:=UTF8Encode(UnicodeString(WideChar(aOrd)));
end;

function OPFTextToChar(aKind: TTypeKind; aField: TField): Int64;
var
  S: RawByteString;
  U: UnicodeString;
begin
  U:=UTF8Decode(OPFGetUTF8Field(aField));
  if U='' then
    Exit(0);
  Result:=Ord(U[1]);
  if (aKind<>tkChar) or (Result<128) then
    Exit;
  if AnsiCharIsSystemCP then
  begin
    // To the system code page
    S:=AnsiString(UnicodeString(U[1]));
    if Length(S)=1 then
      Result:=Ord(S[1])
    else
      Result:=Ord('?');
  end
  else if Result>255 then
    Result:=Ord('?');
end;

function OPFFloatVariant(aField: TOPFieldMap; const aValue: Extended): Variant;
{$IFDEF FPC_HAS_TYPE_EXTENDED}
var
  E: Extended;
  I: Integer;
  S: AnsiString;
{$ENDIF}
begin
  case OPFFloatKind(aField) of
    fkCurrency:
      Result:=Currency(aValue);
    fkComp:
      Result:=Int64(Trunc(aValue));
{$IFDEF FPC_HAS_TYPE_EXTENDED}
    fkExtended:
      begin
        // The exact bits: no decimal text of an Extended is guaranteed exact
        E:=aValue;
        S:='';
        for I:=0 to SizeOf(E)-1 do
          S:=S+HexStr(PByte(@E)[I], 2);
        Result:=S;
      end;
{$ENDIF}
  else
    Result:=Double(aValue);
  end;
end;

procedure OPFAssignVariant(const aValue: Variant; aParam: TParam);
begin
  case VarType(aValue) and varTypeMask of
    varEmpty, varNull:
      aParam.Clear;
    varSmallint, varShortInt, varByte, varWord, varInteger:
      aParam.AsInteger:=aValue;
    varLongWord, varInt64:
      aParam.AsLargeInt:=aValue;
    varQWord:
      begin
      if QWord(aValue)>QWord(High(Int64)) then
        raise EOPF.CreateFmt(SErrQWordOutOfRange, [QWord(aValue)]);
      aParam.AsLargeInt:=Int64(QWord(aValue));
      end;
    varSingle, varDouble:
      aParam.AsFloat:=aValue;
    varCurrency:
      aParam.AsBCD:=aValue;
    varDate:
      aParam.AsDateTime:=aValue;
    varBoolean:
      aParam.AsBoolean:=aValue;
    varOleStr, varUString:
      OPFSetUTF8Param(aParam, UTF8Encode(UnicodeString(aValue)));
    varString:
      aParam.AsString:=AnsiString(aValue);
  else
    raise EOPF.CreateFmt(SErrUnsupportedVariantParam, [VarType(aValue)]);
  end;
end;

procedure OPFAssignVarRec(const aVarRec: TVarRec; aParam: TParam);
begin
  case aVarRec.VType of
    vtInteger:
      aParam.AsInteger:=aVarRec.VInteger;
    vtInt64:
      aParam.AsLargeInt:=aVarRec.VInt64^;
    vtQWord:
      begin
      if aVarRec.VQWord^>QWord(High(Int64)) then
        raise EOPF.CreateFmt(SErrQWordOutOfRange, [aVarRec.VQWord^]);
      aParam.AsLargeInt:=Int64(aVarRec.VQWord^);
      end;
    vtBoolean:
      aParam.AsBoolean:=aVarRec.VBoolean;
    vtExtended:
      aParam.AsFloat:=aVarRec.VExtended^;
    vtCurrency:
      aParam.AsBCD:=aVarRec.VCurrency^;
    vtString:
      aParam.AsString:=aVarRec.VString^;
    vtAnsiString:
      aParam.AsString:=AnsiString(aVarRec.VAnsiString);
    vtUnicodeString:
      OPFSetUTF8Param(aParam, UTF8Encode(UnicodeString(aVarRec.VUnicodeString)));
    vtWideString:
      OPFSetUTF8Param(aParam, UTF8Encode(WideString(aVarRec.VWideString)));
    vtPChar:
      aParam.AsString:=AnsiString(aVarRec.VPChar);
    vtPWideChar:
      OPFSetUTF8Param(aParam, UTF8Encode(UnicodeString(aVarRec.VPWideChar)));
    vtChar:
      aParam.AsString:=aVarRec.VChar;
    vtWideChar:
      OPFSetUTF8Param(aParam, UTF8Encode(UnicodeString(aVarRec.VWideChar)));
    vtVariant:
      OPFAssignVariant(aVarRec.VVariant^, aParam);
  else
    raise EOPF.CreateFmt(SErrUnsupportedVarRecParam, [aVarRec.VType]);
  end;
end;

function OPFIsNullableMember(aField: TOPFieldMap): Boolean;
begin
  Result:=aField.MemberKind=omkNullableRecord;
end;

function OPFIsPropNull(aInstance: TObject; const aPropName: UTF8String;
  aPropInfo: PPropInfo; aNullable: Boolean): Boolean;
var
  NullIntf: IOPFNullable;
begin
  Result:=False;
  if not aNullable or (aInstance=Nil) then
    Exit;
  // Level 1 takes precedence: the interface decides for all properties
  if aInstance.GetInterface(SGUIDIOPFNullable, NullIntf) then
    Exit(NullIntf.IsNull(aPropName));
  // Level 0: dynamic stored specifier
  if aPropInfo<>Nil then
    Result:=not IsStoredProp(aInstance, aPropInfo);
end;

function OPFIsFieldNull(aInstance: TObject; aField: TOPFieldMap): Boolean;
var
  V: Variant;
begin
  if OPFIsNullableMember(aField) then
    Result:=Assigned(OPFNullableMemberReader)
            and OPFNullableMemberReader(aInstance, aField, V)
            and VarIsNull(V)
  else if aField.MemberKind=omkField then
    // A plain class field has no null state
    Result:=False
  else
    Result:=OPFIsPropNull(aInstance, aField.PropertyName, aField.PropInfo,
                          foNullable in aField.Options);
end;

function OPFSetPropNull(aInstance: TObject; const aPropName: UTF8String;
  aPropInfo: PPropInfo; aIsNull: Boolean): Boolean;
var
  NullIntf: IOPFNullable;
begin
  Result:=True;
  if aInstance.GetInterface(SGUIDIOPFNullable, NullIntf) then
  begin
    if aIsNull then
      NullIntf.SetNull(aPropName)
    else
      NullIntf.ClearNull(aPropName);
  end
  // Level 0: only a field-based stored specifier can be written
  else if (aPropInfo<>Nil) and (((aPropInfo^.PropProcs shr 4) and 3) = 0) then
    PBoolean(Pointer(aInstance) + PtrUInt(aPropInfo^.StoredProc))^ := not aIsNull
  else
    Result:=False;
end;

function OPFReadFieldValue(aInstance: TObject; aField: TOPFieldMap; out aValue: Variant): Boolean;
var
  Obj: TObject;
begin
  aValue:=Unassigned;
  if aField.MemberKind<>omkProperty then
    Exit(Assigned(OPFNullableMemberReader) and OPFNullableMemberReader(aInstance, aField, aValue));
  // Fields that are not published properties (manually added maps, fields
  // mapped by attributes) cannot be read generically.
  if GetPropInfo(aInstance, aField.PropertyName)=Nil then
    Exit(False);
  Result:=True;
  if OPFIsFieldNull(aInstance, aField) then
  begin
    aValue:=Null;
    Exit;
  end;
  case aField.PropertyKind of
    tkSString, tkAString, tkLString:
      aValue:=GetStrProp(aInstance, aField.PropertyName);
    tkWString, tkUString:
      aValue:=GetUnicodeStrProp(aInstance, aField.PropertyName);
    tkInteger, tkBool, tkEnumeration, tkSet, tkChar, tkWChar, tkUChar:
      aValue:=GetOrdProp(aInstance, aField.PropertyName);
    tkQWord:
      aValue:=QWord(GetOrdProp(aInstance, aField.PropertyName));
    tkInt64:
      aValue:=GetInt64Prop(aInstance, aField.PropertyName);
    tkFloat:
      if OPFFloatKind(aField)=fkCurrency then
        // Exact, also where Extended is Double
        aValue:=OPFGetCurrencyProp(aInstance, GetPropInfo(aInstance, aField.PropertyName))
      else
        aValue:=OPFFloatVariant(aField, GetFloatProp(aInstance, aField.PropertyName));
    tkVariant:
      aValue:=GetVariantProp(aInstance, aField.PropertyName);
    tkClass:
      begin
        Obj:=GetObjectProp(aInstance, aField.PropertyName);
        if Obj = Nil then
          aValue:=Null
        else if Obj is TStrings then
          aValue:=TStrings(Obj).Text
        else
          aValue:=OPSerializer.Serialize(Obj);
      end;
  else
    // Not persisted: never reported as changed
    aValue:=Unassigned;
  end;
end;

function SameVariantArray(const aLeft, aRight: Variant): Boolean;
var
  I, L, H: Integer;
begin
  Result:=False;
  if not (VarIsArray(aLeft) and VarIsArray(aRight)) then
    Exit;
  // Only one-dimensional arrays are compared element by element
  if (VarArrayDimCount(aLeft)<>1) or (VarArrayDimCount(aRight)<>1) then
    Exit;
  L:=VarArrayLowBound(aLeft, 1);
  H:=VarArrayHighBound(aLeft, 1);
  if (L<>VarArrayLowBound(aRight, 1)) or (H<>VarArrayHighBound(aRight, 1)) then
    Exit;
  for I:=L to H do
    if not OPFSameVariant(aLeft[I], aRight[I]) then
      Exit;
  Result:=True;
end;

function OPFSameVariant(const aLeft, aRight: Variant): Boolean;
begin
  if VarIsEmpty(aLeft) or VarIsEmpty(aRight) then
    Exit(VarIsEmpty(aLeft) and VarIsEmpty(aRight));
  if VarIsNull(aLeft) or VarIsNull(aRight) then
    Exit(VarIsNull(aLeft) and VarIsNull(aRight));
  if VarIsArray(aLeft) or VarIsArray(aRight) then
    Exit(SameVariantArray(aLeft, aRight));
  // 'abc' -> 5 must not raise, '1' -> 1 is a change of the stored value
  if VarType(aLeft)<>VarType(aRight) then
    Exit(False);
  // Strings are compared directly: VarSameValue needs a widestring manager
  // for OLE strings, and comparison here must be exact anyway.
  case VarType(aLeft) of
    varOleStr:
      Exit(WideString(TVarData(aLeft).vOleStr)=WideString(TVarData(aRight).vOleStr));
    varUString:
      Exit(UnicodeString(TVarData(aLeft).vUString)=UnicodeString(TVarData(aRight).vUString));
    varString:
      Exit(RawByteString(TVarData(aLeft).vString)=RawByteString(TVarData(aRight).vString));
  end;
  try
    Result:=VarSameValue(aLeft, aRight);
  except
    Result:=False;
  end;
end;

end.
