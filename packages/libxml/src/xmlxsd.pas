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
function xsdFormatBoolean(Value: Boolean; UseWords: Boolean = False): String;
function xsdFormatDate(Year: Longint; Month, Day: Longword): String;
function xsdFormatDate(Value: TDateTime): String;
function xsdFormatTime(Hour, Minute, Second: Longword): String;
function xsdFormatTime(Value: TDateTime): String;
function xsdFormatDateTime(Year: Longint; Month, Day, Hour, Minute, Second: Longword): String;
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
function xsdParseBoolean(Value: String; var P: Boolean): Boolean;
function xsdParseDate(Value: String; var Year: Longint; var Month, Day: Longword): Boolean;
function xsdParseDate(Value: String; var P: TDateTime): Boolean;
function xsdParseTime(Value: String; var Hour, Minute, Second: Longword): Boolean;
function xsdParseTime(Value: String; var P: TDateTime): Boolean;
function xsdParseDateTime(Value: String; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword): Boolean;
function xsdParseDateTime(Value: String; var P: TDateTime): Boolean;
function xsdParseDecimal(Value: String; var P: Extended): Boolean;
function xsdParseDouble(Value: String; var P: Double): Boolean;
function xsdParseFloat(Value: String; var P: Single): Boolean;
function xsdParseByte(Value: String; var P: Shortint): Boolean;
function xsdParseShort(Value: String; var P: Smallint): Boolean;
function xsdParseInt(Value: String; var P: Longint): Boolean;
function xsdParseLong(Value: String; var P: Int64): Boolean;
function xsdParseUnsignedByte(Value: String; var P: Byte): Boolean;
function xsdParseUnsignedShort(Value: String; var P: Word): Boolean;
function xsdParseUnsignedInt(Value: String; var P: Longword): Boolean;
function xsdParseUnsignedLong(Value: String; var P: QWord): Boolean;

{ Node creation functions }
function xsdNewChildString(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlNodePtr;
function xsdNewChildBoolean(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean; UseWords: Boolean = False): xmlNodePtr;
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
function xsdNewPropBoolean(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean; UseWords: Boolean = False): xmlAttrPtr;
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
function xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: String; index: Integer = 0): Boolean;
function xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Boolean; index: Integer = 0): Boolean;
function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day: Longword; index: Integer = 0): Boolean;
function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer = 0): Boolean;
function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second: Longword; index: Integer = 0): Boolean;
function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer = 0): Boolean;
function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword; index: Integer = 0): Boolean;
function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer = 0): Boolean;
function xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Extended; index: Integer = 0): Boolean;
function xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Double; index: Integer = 0): Boolean;
function xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Single; index: Integer = 0): Boolean;
function xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Shortint; index: Integer = 0): Boolean;
function xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Smallint; index: Integer = 0): Boolean;
function xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longint; index: Integer = 0): Boolean;
function xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Int64; index: Integer = 0): Boolean;
function xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Byte; index: Integer = 0): Boolean;
function xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Word; index: Integer = 0): Boolean;
function xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longword; index: Integer = 0): Boolean;
function xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: QWord; index: Integer = 0): Boolean;

{ Property query functions }
function xsdHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr;
function xsdHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
function xsdGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr;
function xsdGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
function xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: String): Boolean;
function xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Boolean): Boolean;
function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day: Longword): Boolean;
function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second: Longword): Boolean;
function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword): Boolean;
function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
function xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Extended): Boolean;
function xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Double): Boolean;
function xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Single): Boolean;
function xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Shortint): Boolean;
function xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Smallint): Boolean;
function xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longint): Boolean;
function xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Int64): Boolean;
function xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Byte): Boolean;
function xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Word): Boolean;
function xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longword): Boolean;
function xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: QWord): Boolean;

function xsdRemoveBlanks(content: xmlCharPtr; out cleaned: string): boolean;

implementation

function xsdFormatBoolean(Value: Boolean; UseWords: Boolean): String;
begin
  if UseWords then
    if Value then
      Result := 'true'
    else
      Result := 'false'
  else
    if Value then
      Result := '1'
    else
      Result := '0';
end;

function xsdFormatDate(Year: Longint; Month, Day: Longword): String;
begin
  Result := Format('%4.4d-%2.2u-%2.2uZ', [Year, Month, Day]);
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
  Result := Format('%2.2u:%2.2u:%2.2uZ', [Hour, Minute, Second]);
end;

function xsdFormatTime(Value: TDateTime): String;
var
  Hour, Minute, Second, Millisecond: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, Millisecond);
  Result := xsdFormatTime(Hour, Minute, Second);
end;

function xsdFormatDateTime(Year: Longint; Month, Day, Hour, Minute, Second: Longword): String;
begin
  Result := Format('%4.4d-%2.2u-%2.2uT%2.2u:%2.2u:%2.2uZ', [Year, Month, Day, Hour, Minute, Second]);
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

function xsdParseBoolean(Value: String; var P: Boolean): Boolean;
begin
  Result := TryStrToBool(Value, P);
end;

function xsdParseDate(Value: String; var Year: Longint; var Month, Day: Longword): Boolean;
// xsd:date = [-]CCYY-MM-DD[Z|(+|-)hh:mm]
var
  S: String;
  I: Integer;
  N: Boolean;
begin
{ negative year }
  N := (Length(Value) > 0) and (Value[1]='-');
  if N then
    S := Copy(Value, 2, MaxInt)
  else
    S := Value;

{ parse year }
  I := Pos('-', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Year) then
    Exit(False);
  if N then
    Year := -Year;

{ parse month }
  S := Copy(S, I+1, MaxInt);
  I := Pos('-', S);
  if (I = 0) or not TryStrToInt(Copy(S, 1, I-1), Integer(Month)) then
    Exit(False);

{ parse day }
  S := Copy(S, I+1, MaxInt);
  Result := TryStrToInt(S, Integer(Day));
end;

function xsdParseDate(Value: String; var P: TDateTime): Boolean;
var
  Year: Longint;
  Month, Day: Longword;
begin
  if xsdParseDate(Value, Year, Month, Day) and (Year > 0) then
    Result := TryEncodeDate(Year, Month, Day, P)
  else
    Result := False;
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

function xsdParseTime(Value: String; var P: TDateTime): Boolean;
var
  Hour, Minute, Second: Longword;
begin
  if xsdParseTime(Value, Hour, Minute, Second) then
    Result := TryEncodeTime(Hour, Minute, Second, 0, P)
  else
    Result := False;
end;

function xsdParseDateTime(Value: String; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword): Boolean;
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

function xsdParseDateTime(Value: String; var P: TDateTime): Boolean;
var
  Year: Longint;
  Month, Day: Longword;
  Hour, Minute, Second: Longword;
begin
  if xsdParseDateTime(Value, Year, Month, Day, Hour, Minute, Second) and (Year > 0) then
    Result := TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0, P)
  else
    Result := False;
end;

function xsdParseDecimal(Value: String; var P: Extended): Boolean;
begin
  Result := TryStrToFloat(Value, P);
end;

function xsdParseDouble(Value: String; var P: Double): Boolean;
begin
  Result := TryStrToFloat(Value, P);
end;

function xsdParseFloat(Value: String; var P: Single): Boolean;
begin
  Result := TryStrToFloat(Value, P);
end;

function xsdParseByte(Value: String; var P: Shortint): Boolean;
var
  Tmp: Longint;
begin
  Result := TryStrToInt(Value, Tmp);
  P := Tmp;
end;

function xsdParseShort(Value: String; var P: Smallint): Boolean;
var
  Tmp: Longint;
begin
  Result := TryStrToInt(Value, Tmp);
  P := Tmp;
end;

function xsdParseInt(Value: String; var P: Longint): Boolean;
begin
  Result := TryStrToInt(Value, P);
end;

function xsdParseLong(Value: String; var P: Int64): Boolean;
begin
  Result := TryStrToInt64(Value, P);
end;

function xsdParseUnsignedByte(Value: String; var P: Byte): Boolean;
var
  Tmp: Longint;
begin
  Result := TryStrToInt(Value, Tmp);
  P := Tmp;
end;

function xsdParseUnsignedShort(Value: String; var P: Word): Boolean;
var
  Tmp: Longint;
begin
  Result := TryStrToInt(Value, Tmp);
  P := Tmp;
end;

function xsdParseUnsignedInt(Value: String; var P: Longword): Boolean;
begin
  Result := TryStrToInt(Value, Longint(P));
end;

function xsdParseUnsignedLong(Value: String; var P: QWord): Boolean;
begin
  Result := TryStrToQWord(Value, P);
end;

function xsdNewChildString(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlNodePtr;
begin
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Value));
end;

function xsdNewChildBoolean(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean; UseWords: Boolean): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatBoolean(Value, UseWords);
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

function xsdNewPropBoolean(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean; UseWords: Boolean): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatBoolean(Value, UseWords);
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

function xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: String; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
  begin
    P := pchar(value);
    result := true;
  end else
    result := false;
end;

function xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Boolean; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseBoolean(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day: Longword; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDate(pchar(value), Year, Month, Day)
  else
    result := false;
end;

function xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDate(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second: Longword; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseTime(pchar(value), Hour, Minute, Second)
  else
    result := false;
end;

function xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseTime(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value), Year, Month, Day, Hour, Minute, Second)
  else
    result := false;
end;

function xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Extended; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDecimal(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Double; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseDouble(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Single; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseFloat(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Shortint; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseByte(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Smallint; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseShort(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longint; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseInt(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Int64; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseLong(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Byte; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedByte(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Word; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedShort(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longword; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedInt(pchar(value), P)
  else
    result := false;
end;

function xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: QWord; index: Integer): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsChild(node, name, nameSpace, index);
  if assigned(value) then
    result := xsdParseUnsignedLong(pchar(value), P)
  else
    result := false;
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

function xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: String): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
  begin
    P := pchar(value);
    Result := true;
  end else
    result := false;
end;

function xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Boolean): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseBoolean(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day: Longword): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDate(pchar(value), Year, Month, Day)
  else
    result := false;
end;

function xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDate(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second: Longword): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseTime(pchar(value), Hour, Minute, Second)
  else
    result := false;
end;

function xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseTime(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year: Longint; var Month, Day, Hour, Minute, Second: Longword): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value), Year, Month, Day, Hour, Minute, Second)
  else
    result := false;
end;

function xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: TDateTime): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDateTime(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Extended): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDecimal(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Double): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseDouble(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Single): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseFloat(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Shortint): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseByte(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Smallint): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseShort(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longint): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseInt(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Int64): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseLong(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Byte): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedByte(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Word): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedShort(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: Longword): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedInt(pchar(value), P)
  else
    result := false;
end;

function xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var P: QWord): Boolean;
var
  value: xmlCharPtr;
begin
  value := xsdGetNsProp(node, name, nameSpace);
  if assigned(value) then
    result := xsdParseUnsignedLong(pchar(value), P)
  else
    result := false;
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
