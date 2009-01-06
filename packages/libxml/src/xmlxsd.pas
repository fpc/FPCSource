{
  A set of helper functions for libxml2 for FreePascal
  Copyright (C) 2008 by Ivo Steinmann
}

unit xmlxsd;

{$mode objfpc}
{$H+}

interface

uses
  libxml2,
  DateUtils,
  SysUtils;

{ Format functions }
function xsdFormatBoolean(Value: Boolean): String;
function xsdFormatDate(Year, Month, Day: Longword): String;
function xsdFormatDate(Value: TDateTime): String;
function xsdFormatTime(Hour, Minute, Second: Longword): String;
function xsdFormatTime(Value: TDateTime): String;
function xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second: Longword): String;
function xsdFormatDateTime(Value: TDateTime): String;
function xsdFormatDecimal(Value: Extended; Precision: Integer = 4; Digits: Integer = 1): String;
function xsdFormatDouble(Value: Double): String;
function xsdFormatFloat(Value: Single): String;
function xsdFormatByte(Value: Shortint): String;
function xsdFormatShort(Value: Smallint): String;
function xsdFormatInt(Value: Longint): String;
function xsdFormatLong(Value: Int64): String;
function xsdFormatUnsignedByte(Value: Byte): String;
function xsdFormatUnsignedShort(Value: Word): String;
function xsdFormatUnsignedInt(Value: Longword): String;
function xsdFormatUnsignedLong(Value: QWord): String;

{ Parse functions }
function xsdParseBoolean(Value: String): Boolean;
function xsdParseDate(Value: String; var Year, Month, Day: Longword): Boolean;
function xsdParseDate(Value: String): TDateTime;
function xsdParseTime(Value: String; var Hour, Minute, Second: Longword): Boolean;
function xsdParseTime(Value: String): TDateTime;
function xsdParseDateTime(Value: String; var Year, Month, Day, Hour, Minute, Second: Longword): Boolean;
function xsdParseDateTime(Value: String): TDateTime;
function xsdParseDecimal(Value: String): Extended;
function xsdParseDouble(Value: String): Double;
function xsdParseFloat(Value: String): Single;
function xsdParseByte(Value: String): Shortint;
function xsdParseShort(Value: String): Smallint;
function xsdParseInt(Value: String): Longint;
function xsdParseLong(Value: String): Int64;
function xsdParseUnsignedByte(Value: String): Byte;
function xsdParseUnsignedShort(Value: String): Word;
function xsdParseUnsignedInt(Value: String): Longword;
function xsdParseUnsignedLong(Value: String): QWord;

{ Node creation functions }
function xsdNewChildString(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlNodePtr;
function xsdNewChildBoolean(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean): xmlNodePtr;
function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword): xmlNodePtr;
function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second: Longword): xmlNodePtr;
function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second: Longword): xmlNodePtr;
function xsdNewChildDecimal(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Extended; Precision: Integer = 4; Digits: Integer = 1): xmlNodePtr;
function xsdNewChildDouble(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Double): xmlNodePtr;
function xsdNewChildFloat(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Single): xmlNodePtr;
function xsdNewChildByte(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Shortint): xmlNodePtr;
function xsdNewChildShort(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Smallint): xmlNodePtr;
function xsdNewChildInt(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longint): xmlNodePtr;
function xsdNewChildLong(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Int64): xmlNodePtr;
function xsdNewChildUnsignedByte(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Byte): xmlNodePtr;
function xsdNewChildUnsignedShort(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Word): xmlNodePtr;
function xsdNewChildUnsignedInt(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longword): xmlNodePtr;
function xsdNewChildUnsignedLong(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: QWord): xmlNodePtr;

{ Property creation functions }
function xsdNewPropString(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlAttrPtr;
function xsdNewPropBoolean(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean): xmlAttrPtr;
function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword): xmlAttrPtr;
function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second: Longword): xmlAttrPtr;
function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second: Longword): xmlAttrPtr;
function xsdNewPropDecimal(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Extended; Precision: Integer = 4; Digits: Integer = 1): xmlAttrPtr;
function xsdNewPropDouble(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Double): xmlAttrPtr;
function xsdNewPropFloat(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Single): xmlAttrPtr;
function xsdNewPropByte(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Shortint): xmlAttrPtr;
function xsdNewPropShort(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Smallint): xmlAttrPtr;
function xsdNewPropInt(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longint): xmlAttrPtr;
function xsdNewPropLong(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Int64): xmlAttrPtr;
function xsdNewPropUnsignedByte(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Byte): xmlAttrPtr;
function xsdNewPropUnsignedShort(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Word): xmlAttrPtr;
function xsdNewPropUnsignedInt(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longword): xmlAttrPtr;
function xsdNewPropUnsignedLong(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: QWord): xmlAttrPtr;

{ Query functions }
const
  NS_IGNORE  : xmlCharPtr = nil;
  NS_EXCLUDE : xmlCharPtr = pointer(-1);

{ Node query functions }
function xsdHasChild(node: xmlNodePtr; name: xmlCharPtr; index: Integer = 0): xmlNodePtr;
function xsdHasNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; index: Integer = 0): xmlNodePtr;
function xsdGetChild(node: xmlNodePtr; name: xmlCharPtr; index: Integer = 0): xmlCharPtr;
function xsdGetNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; index: Integer = 0): xmlCharPtr;
function xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: String; index: Integer = 0): String;
function xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Boolean; index: Integer = 0): Boolean;
function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer = 0): TDateTime;
function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer = 0): TDateTime;
function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer = 0): TDateTime;
function xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Extended; index: Integer = 0): Extended;
function xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Double; index: Integer = 0): Double;
function xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Single; index: Integer = 0): Single;
function xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Shortint; index: Integer = 0): Shortint;
function xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Smallint; index: Integer = 0): Smallint;
function xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longint; index: Integer = 0): Longint;
function xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Int64; index: Integer = 0): Int64;
function xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Byte; index: Integer = 0): Byte;
function xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Word; index: Integer = 0): Word;
function xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longword; index: Integer = 0): Longword;
function xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: QWord; index: Integer = 0): QWord;
function xsdGetChildEnumString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Values: array of string; index: Integer = 0): Integer;

{ Property query functions }
function xsdHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr;
function xsdHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
function xsdGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr;
function xsdGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
function xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: String): String;
function xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Boolean): Boolean;
function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
function xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Extended): Extended;
function xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Double): Double;
function xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Single): Single;
function xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Shortint): Shortint;
function xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Smallint): Smallint;
function xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longint): Longint;
function xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Int64): Int64;
function xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Byte): Byte;
function xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Word): Word;
function xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longword): Longword;
function xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: QWord): QWord;
function xsdGetPropEnumString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Values: array of string): Integer;

function xsdRemoveBlanks(content: xmlCharPtr; out cleaned: string): boolean;

implementation

function xsdFormatBoolean(Value: Boolean): String;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function xsdFormatDate(Year, Month, Day: Longword): String;
begin
  Result := Format('%4.4d-%2.2d-%2.2dZ', [Year, Month, Day]);
end;

function xsdFormatDate(Value: TDateTime): String;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := xsdFormatDate(Year, Month, Day);
end;

function xsdFormatTime(Hour, Minute, Second: Longword): String;
begin
  Result := Format('%2.2d:%2.2d:%2.2dZ', [Hour, Minute, Second]);
end;

function xsdFormatTime(Value: TDateTime): String;
var
  Hour, Minute, Second, Millisecond: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, Millisecond);
  Result := xsdFormatTime(Hour, Minute, Second);
end;

function xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second: Longword): String;
begin
  Result := Format('%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2dZ', [Year, Month, Day, Hour, Minute, Second]);
end;

function xsdFormatDateTime(Value: TDateTime): String;
var
  Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, Millisecond);
  Result := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second);
end;

function xsdFormatDecimal(Value: Extended; Precision: Integer; Digits: Integer): String;
begin
  Result := FloatToStrF(Value, ffFixed, Precision, Digits);
end;

function xsdFormatDouble(Value: Double): String;
begin
  Result := FloatToStr(Value);
end;

function xsdFormatFloat(Value: Single): String;
begin
  Result := FloatToStr(Value);
end;

function xsdFormatByte(Value: Shortint): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatShort(Value: Smallint): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatInt(Value: Integer): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatLong(Value: Int64): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedByte(Value: Byte): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedShort(Value: Word): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedInt(Value: Longword): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedLong(Value: QWord): String;
begin
  Result := IntToStr(Value);
end;

function xsdParseBoolean(Value: String): Boolean;
begin
  Result := StrToBool(Value);
end;

function xsdParseDate(Value: String; var Year, Month, Day: Longword): Boolean;
// xsd:date = [-]CCYY-MM-DD[Z|(+|-)hh:mm]
var
  S: String;
  I: Integer;
begin
{ ignore leading - }
  if (Length(Value) > 0) and (Value[1]='-') then
    S := Copy(Value, 2, MaxInt)
  else
    S := Value;

{ parse year }
  I := Pos('-', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Integer(Year)) then
    Exit(False);

{ parse month }
  S := Copy(S, I+1, MaxInt);
  I := Pos('-', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Integer(Month)) then
    Exit(False);

{ parse day }
  S := Copy(S, I+1, MaxInt);
  Result := TryStrToInt(S, Integer(Day));
end;

function xsdParseDate(Value: String): TDateTime;
var
  Year, Month, Day: Longword;
begin
  if xsdParseDate(Value, Year, Month, Day) then
    Result := EncodeDate(Year, Month, Day)
  else
    Result := 0;
end;

function xsdParseTime(Value: String; var Hour, Minute, Second: Longword): Boolean;
// xsd:time = hh:mm:ss[Z|(+|-)hh:mm]
var
  S: String;
  I: Integer;
begin
  S := Value;

{ parse hour }
  I := Pos(':', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Integer(Hour)) or (Hour > 23) then
    Exit(False);

{ parse minute }
  S := Copy(S, I+1, MaxInt);
  I := Pos(':', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Integer(Minute)) or (Minute > 59) then
    Exit(False);

{ parse second }
  S := Copy(S, I+1, MaxInt);
  Result := TryStrToInt(S, Integer(Second)) and (Second < 60);
end;

function xsdParseTime(Value: String): TDateTime;
var
  Hour, Minute, Second: Longword;
begin
  if xsdParseDate(Value, Hour, Minute, Second) then
    Result := EncodeTime(Hour, Minute, Second, 0)
  else
    Result := 0;
end;

function xsdParseDateTime(Value: String; var Year, Month, Day, Hour, Minute, Second: Longword): Boolean;
// xsd:dateTime = [-]CCYY-MM-DDThh:mm:ss[Z|(+|-)hh:mm]
var
  S: String;
  I: Integer;
begin
  S := Value;

{ ignore Z }
  I := Pos('Z', S);
  if I > 0 then
    S := Copy(S, 1, I-1);

{ parse date and time }
  I := Pos('T', S);
  Result := (I > 0) and
    xsdParseDate(Copy(S, 1, I-1), Year, Month, Day) and
    xsdParseTime(Copy(S, I+1, MaxInt), Hour, Minute, Second);
end;

function xsdParseDateTime(Value: String): TDateTime;
var
  Year, Month, Day: Longword;
  Hour, Minute, Second: Longword;
begin
  if xsdParseDateTime(Value, Year, Month, Day, Hour, Minute, Second) then
    Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0)
  else
    Result := 0;
end;

function xsdParseDecimal(Value: String): Extended;
begin
  Result := StrToFloat(Value);
end;

function xsdParseDouble(Value: String): Double;
begin
  Result := StrToFloat(Value);
end;

function xsdParseFloat(Value: String): Single;
begin
  Result := StrToFloat(Value);
end;

function xsdParseByte(Value: String): Shortint;
begin
  Result := StrToInt(Value);
end;

function xsdParseShort(Value: String): Smallint;
begin
  Result := StrToInt(Value);
end;

function xsdParseInt(Value: String): Longint;
begin
  Result := StrToInt(Value);
end;

function xsdParseLong(Value: String): Int64;
begin
  Result := StrToInt64(Value);
end;

function xsdParseUnsignedByte(Value: String): Byte;
begin
  Result := StrToInt(Value);
end;

function xsdParseUnsignedShort(Value: String): Word;
begin
  Result := StrToInt(Value);
end;

function xsdParseUnsignedInt(Value: String): Longword;
begin
  Result := StrToInt(Value);
end;

function xsdParseUnsignedLong(Value: String): QWord;
begin
  Result := StrToInt64(Value);
end;

function xsdNewChildString(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlNodePtr;
begin
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Value));
end;

function xsdNewChildBoolean(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatBoolean(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second: Longword): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Hour, Minute, Second);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Year, Month, Day);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second: Longword): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDecimal(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Extended; Precision: Integer; Digits: Integer): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDecimal(Value, Precision, Digits);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDouble(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Double): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDouble(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildFloat(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Single): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatFloat(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildByte(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Shortint): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatByte(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildShort(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Smallint): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatShort(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildInt(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longint): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatInt(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildLong(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Int64): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatLong(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildUnsignedByte(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Byte): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedByte(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildUnsignedShort(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Word): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedShort(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildUnsignedInt(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longword): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedInt(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildUnsignedLong(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: QWord): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedLong(Value);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropString(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlAttrPtr;
begin
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Value));
end;

function xsdNewPropBoolean(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatBoolean(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second: Longword): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Hour, Minute, Second);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Year, Month, Day);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second: Longword): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDecimal(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Extended; Precision: Integer; Digits: Integer): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDecimal(Value, Precision, Digits);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDouble(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Double): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDouble(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropFloat(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Single): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatFloat(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropByte(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Shortint): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatByte(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropShort(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Smallint): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatShort(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropInt(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longint): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatInt(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropLong(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Int64): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatLong(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropUnsignedByte(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Byte): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedByte(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropUnsignedShort(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Word): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedShort(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropUnsignedInt(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Longword): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedInt(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropUnsignedLong(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: QWord): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatUnsignedLong(Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdHasChild(node: xmlNodePtr; name: xmlCharPtr; index: integer): xmlNodePtr;
begin
  Result := xsdHasNsChild(node, name, nil, index);
end;

function xsdHasNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; index: integer): xmlNodePtr;
begin
  if Assigned(node) and (Index >= 0) then
  begin
    node := node^.children;
    while Assigned(node) do
    begin
      if (xmlStrEqual(name, node^.name) <> 0) and ((nameSpace = NS_IGNORE) or
        ((nameSpace = NS_EXCLUDE) and (node^.ns = nil)) or
        ((nameSpace <> NS_EXCLUDE) and (nameSpace <> NS_IGNORE) and (node^.ns <> nil) and (xmlStrEqual(nameSpace, node^.ns^.prefix) <> 0))) then
      begin
        if index = 0 then
        begin
          result := node;
          Exit;
        end;
        Dec(Index);
      end;
      node := node^.next;
    end;
  end;

  Result := nil;
end;

function xsdGetChild(node: xmlNodePtr; name: xmlCharPtr; index: Integer): xmlCharPtr;
begin
  Result := xsdGetNsChild(node, name, nil, index);
end;

function xsdGetNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; index: Integer): xmlCharPtr;
var
  child: xmlNodePtr;
begin
  child := xsdHasNsChild(node, name, nameSpace, index);
  if Assigned(child) then
    result := xmlNodeGetContent(child)
  else
    result := nil;
end;

function xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: String; index: Integer): String;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := pchar(value)
  else
    result := defaultValue;
end;

function xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Boolean; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseBoolean(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDate(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseTime(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime; index: Integer): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Extended; index: Integer): Extended;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDecimal(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Double; index: Integer): Double;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDouble(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Single; index: Integer): Single;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseFloat(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Shortint; index: Integer): Shortint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseByte(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Smallint; index: Integer): Smallint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseShort(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longint; index: Integer): Longint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseInt(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Int64; index: Integer): Int64;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseLong(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Byte; index: Integer): Byte;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedByte(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Word; index: Integer): Word;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedShort(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longword; index: Integer): Longword;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedInt(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: QWord; index: Integer): QWord;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedLong(pchar(value))
  else
    result := defaultValue;
end;

function IndexOfString(const S: String; const List: array of String): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(List) - 1 do
    if List[I] = S then
      Exit(I);
  Result := -1;
end;

function xsdGetChildEnumString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Values: array of string; index: Integer = 0): Integer;
begin
  Result := IndexOfString(xsdGetChildString(node, name, nameSpace, Values[0], index), Values);
end;

function xsdHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr;
begin
  result := xsdHasNsProp(node, name, nil);
end;

function xsdHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
begin
  if nameSpace = NS_EXCLUDE then
  begin
    result := xmlHasProp(node, name);
    if Assigned(result) and (result^.ns <> nil) then
      result := nil;
  end else
    result := xmlHasNsProp(node, name, nameSpace);
end;

function xsdGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr;
begin
  result := xmlGetProp(node, name);
end;

function xsdGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
begin
  if nameSpace = NS_EXCLUDE then
    result := xmlGetNoNsProp(node, name)
  else
    result := xmlGetNsProp(node, name, nameSpace);
end;

function xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: String): String;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := pchar(value)
  else
    result := defaultValue;
end;

function xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Boolean): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseBoolean(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDate(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseTime(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: TDateTime): TDateTime;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Extended): Extended;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDecimal(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Double): Double;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDouble(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Single): Single;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseFloat(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Shortint): Shortint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseByte(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Smallint): Smallint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseShort(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longint): Longint;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseInt(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Int64): Int64;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseLong(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Byte): Byte;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedByte(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Word): Word;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedShort(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: Longword): Longword;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedInt(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; defaultValue: QWord): QWord;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedLong(pchar(value))
  else
    result := defaultValue;
end;

function xsdGetPropEnumString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Values: array of string): Integer;
begin
  result := IndexOfString(xsdGetPropString(node, name, nameSpace, Values[0]), Values);
end;

function xsdRemoveBlanks(content: xmlCharPtr; out cleaned: string): boolean;
var
  Space: Boolean;
  len: Integer;
begin
  cleaned := '';

  if Assigned(content) then
  begin
    Space := True;

    while content^ <> 0 do
    begin
      if xmlIsBlank(content^) then
      begin
        if not Space then
          cleaned := cleaned + ' ';
        Space := True;
      end else begin
        cleaned := cleaned + chr(content^);
        Space := False;
      end;

      Inc(content);
    end;
  end;

  len := Length(cleaned);
  if len > 0 then
  begin
    if cleaned[len] = ' ' then
      SetLength(cleaned, len-1);
    Result := True;
  end else
    Result := False;
end;

end.
