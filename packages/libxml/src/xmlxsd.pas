{
  A set of helper functions for libxml2 for FreePascal
  Copyright (C) 2008 by Ivo Steinmann
}

unit xmlxsd;

{$mode objfpc}
{$H+}

interface

uses
  xml2,
  DateUtils,
  SysUtils;

resourcestring
  ChildNotFound = 'child %s not found';
  PropNotFound  = 'attribute %s not found';

type
  XSDException = class(Exception)
  public
    constructor CreateNode(const Msg: String; name, nameSpace: xmlCharPtr);
  end;

  TTimezoneType = (
    tzUnknown,
    tzUTC,
    tzUser
  );

  PTimezone = ^TTimezone;
  TTimezone = record
    Kind   : TTimezoneType;
    Hour   : Longint;  // +/- [00..23]
    Minute : Longword; // [00..59]
    Convert: Boolean;  // you have to initialize this field allways!!!
  end;

const
  TIMEZONE_UTC: TTimezone = (Kind:tzUTC;Hour:0;Minute:0;Convert:False);
  TIMEZONE_UNKNOWN: TTimezone = (Kind:tzUnknown;Hour:0;Minute:0;Convert:False);
  CONVERT_TO_TIMEZONE_UTC: TTimezone = (Kind:tzUTC;Hour:0;Minute:0;Convert:True);

{ Format functions }
function xsdFormatBoolean(Value: Boolean; UseWords: Boolean = False): String;
function xsdFormatDate(Year, Month, Day: Longword; BC: Boolean; Timezone: PTimezone = nil): String;
function xsdFormatDate(Value: TDateTime; Timezone: PTimezone = nil): String;
function xsdFormatTime(Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): String;
function xsdFormatTime(Value: TDateTime; Timezone: PTimezone = nil): String;
function xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean; Timezone: PTimezone = nil): String;
function xsdFormatDateTime(Value: TDateTime; Timezone: PTimezone): String;
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

{ DateTime functions }
procedure xsdTimeConvertTo(var Hour, Minute, Second, Milliseconds: Longword; var Source: TTimezone; const Target: TTimezone);
procedure xsdDateConvertTo(var Year, Month, Day: Longword; var Source: TTimezone; const Target: TTimezone);
procedure xsdDateTimeConvertTo(var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; var Source: TTimezone; const Target: TTimezone);

{ Parse functions }
function xsdParseBoolean(Chars: xmlCharPtr; var Value: Boolean): Boolean;
function xsdParseDate(Chars: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdParseDate(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdParseTime(Chars: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): Boolean;
function xsdParseTime(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdParseDateTime(Chars: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdParseDateTime(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdParseDecimal(Chars: xmlCharPtr; var Value: Extended): Boolean;
function xsdParseDouble(Chars: xmlCharPtr; var Value: Double): Boolean;
function xsdParseFloat(Chars: xmlCharPtr; var Value: Single): Boolean;
function xsdParseInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
function xsdParseNonNegativeInteger(Chars: xmlCharPtr; var Value: QWord): Boolean;
function xsdParseNonPositiveInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
function xsdParseNegativeInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
function xsdParsePositiveInteger(Chars: xmlCharPtr; var Value: QWord): Boolean;
function xsdParseByte(Chars: xmlCharPtr; var Value: Shortint): Boolean;
function xsdParseShort(Chars: xmlCharPtr; var Value: Smallint): Boolean;
function xsdParseInt(Chars: xmlCharPtr; var Value: Longint): Boolean;
function xsdParseLong(Chars: xmlCharPtr; var Value: Int64): Boolean;
function xsdParseUnsignedByte(Chars: xmlCharPtr; var Value: Byte): Boolean;
function xsdParseUnsignedShort(Chars: xmlCharPtr; var Value: Word): Boolean;
function xsdParseUnsignedInt(Chars: xmlCharPtr; var Value: Longword): Boolean;
function xsdParseUnsignedLong(Chars: xmlCharPtr; var Value: QWord): Boolean;

{ Node creation functions }
function xsdNewChildString(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: String): xmlNodePtr;
function xsdNewChildBoolean(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Value: Boolean; UseWords: Boolean = False): xmlNodePtr;
function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword; BC: Boolean = False; Timezone: PTimezone = nil): xmlNodePtr;
function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Date: TDateTime; Timezone: PTimezone = nil): xmlNodePtr;
function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): xmlNodePtr;
function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Time: TDateTime; Timezone: PTimezone = nil): xmlNodePtr;
function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean = False; Timezone: PTimezone = nil): xmlNodePtr;
function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; DateTime: TDateTime; Timezone: PTimezone = nil): xmlNodePtr;
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
function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword; BC: Boolean = False; Timezone: PTimezone = nil): xmlAttrPtr;
function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Date: TDateTime; Timezone: PTimezone = nil): xmlAttrPtr;
function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): xmlAttrPtr;
function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Time: TDateTime; Timezone: PTimezone = nil): xmlAttrPtr;
function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean = False; Timezone: PTimezone = nil): xmlAttrPtr;
function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; DateTime: TDateTime; Timezone: PTimezone = nil): xmlAttrPtr;
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
function xsdTestNode(node: xmlNodePtr; name, nameSpace: xmlCharPtr): Boolean;
function xsdHasChild(node: xmlNodePtr; name: xmlCharPtr; Index: Integer = 0): xmlNodePtr;
function xsdHasNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlNodePtr;

//function xsdTryGetChild(node: xmlNodePtr; name: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
//function xsdTryGetNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
function xsdTryGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String; Index: Integer = 0): Boolean;
function xsdTryGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean; Index: Integer = 0): Boolean;
function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended; Index: Integer = 0): Boolean;
function xsdTryGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double; Index: Integer = 0): Boolean;
function xsdTryGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single; Index: Integer = 0): Boolean;
function xsdTryGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint; Index: Integer = 0): Boolean;
function xsdTryGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint; Index: Integer = 0): Boolean;
function xsdTryGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint; Index: Integer = 0): Boolean;
function xsdTryGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord; Index: Integer = 0): Boolean;

function xsdGetChild(node: xmlNodePtr; name: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
function xsdGetNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
procedure xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String; Index: Integer = 0);
procedure xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean; Index: Integer = 0);
procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0);
procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0);
procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended; Index: Integer = 0);
procedure xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double; Index: Integer = 0);
procedure xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single; Index: Integer = 0);
procedure xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint; Index: Integer = 0);
procedure xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint; Index: Integer = 0);
procedure xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint; Index: Integer = 0);
procedure xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64; Index: Integer = 0);
procedure xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte; Index: Integer = 0);
procedure xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word; Index: Integer = 0);
procedure xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword; Index: Integer = 0);
procedure xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord; Index: Integer = 0);

{ Property query functions }
function xsdTestProp(attr: xmlAttrPtr; name, nameSpace: xmlCharPtr): Boolean;
function xsdHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr;
function xsdHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
function xsdGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr;
function xsdGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
function xsdTryGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String): Boolean;
function xsdTryGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean): Boolean;
function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended): Boolean;
function xsdTryGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double): Boolean;
function xsdTryGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single): Boolean;
function xsdTryGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint): Boolean;
function xsdTryGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint): Boolean;
function xsdTryGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint): Boolean;
function xsdTryGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64): Boolean;
function xsdTryGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte): Boolean;
function xsdTryGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word): Boolean;
function xsdTryGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword): Boolean;
function xsdTryGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord): Boolean;

procedure xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String);
procedure xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean);
procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil);
procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended);
procedure xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double);
procedure xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single);
procedure xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint);
procedure xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint);
procedure xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint);
procedure xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64);
procedure xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte);
procedure xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word);
procedure xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword);
procedure xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord);

function xsdRemoveBlanks(content: xmlCharPtr; var cleaned: string): boolean;

implementation

constructor XSDException.CreateNode(const Msg: String; name, nameSpace: xmlCharPtr);
var
  S: String;
begin
  S := PChar(name);
  if Assigned(nameSpace) then
    S := PChar(nameSpace)+':'+S;
  inherited CreateFmt(Msg, [S]);
end;

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

function xsdFormatDate(Year, Month, Day: Longword; BC: Boolean; Timezone: PTimezone): String;
begin
  Result := Format('%4.4d-%2.2u-%2.2u', [Year, Month, Day]);
  if BC then
    Result := '-' + Result;

  if Assigned(Timezone) then
    case Timezone^.Kind of
      tzUTC:
        Result := Result + 'Z';
      tzUser:
        begin
          if Timezone^.Hour >= 0 then
            Result := Result + '+';
          Result := Result + Format('%2.2d:%2.2u', [Timezone^.Hour, Timezone^.Minute]);
        end;
    end;
end;

function xsdFormatDate(Value: TDateTime; Timezone: PTimezone): String;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := xsdFormatDate(Year, Month, Day, False, Timezone);
end;

function xsdFormatTime(Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): String;
begin
  Result := Format('%2.2u:%2.2u:%2.2u', [Hour, Minute, Second]);
  if Milliseconds > 0 then
    Result := Result + '.' + IntToStr(Milliseconds);

  if Assigned(Timezone) then
    case Timezone^.Kind of
      tzUTC:
        Result := Result + 'Z';
      tzUser:
        begin
          if Timezone^.Hour >= 0 then
            Result := Result + '+';
          Result := Result + Format('%2.2d:%2.2u', [Timezone^.Hour, Timezone^.Minute]);
        end;
    end;
end;

function xsdFormatTime(Value: TDateTime; Timezone: PTimezone): String;
var
  Hour, Minute, Second, Milliseconds: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, Milliseconds);
  Result := xsdFormatTime(Hour, Minute, Second, Milliseconds, Timezone);
end;

function xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean; Timezone: PTimezone): String;
begin
  Result := xsdFormatDate(Year, Month, Day, BC, nil) + 'T' + xsdFormatTime(Hour, Minute, Second, Milliseconds, Timezone);
end;

function xsdFormatDateTime(Value: TDateTime; Timezone: PTimezone): String;
var
  Year, Month, Day, Hour, Minute, Second, Milliseconds: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, Milliseconds);
  Result := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds, False, Timezone);
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

procedure xsdTimeConvertTo(var Hour, Minute, Second, Milliseconds: Longword; var Source: TTimezone; const Target: TTimezone);
begin
  {$warning not implemented}
end;

procedure xsdDateConvertTo(var Year, Month, Day: Longword; var Source: TTimezone; const Target: TTimezone);
begin
  {$warning not implemented}
end;

procedure xsdDateTimeConvertTo(var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; var Source: TTimezone; const Target: TTimezone);
begin
  {$warning not implemented}
end;

function xsdParseBoolean(Chars: xmlCharPtr; var Value: Boolean): Boolean;
begin
  Result := TryStrToBool(PChar(Chars), Value);
end;

function __parseTimezone(var P: PChar; var T: TTimezone): Boolean;
var
  I: Integer;
  N: Boolean;
begin
  { allow 'Z' }
  if P^ = 'Z' then
  begin
    T.Kind := tzUTC;
    T.Hour := 0;
    T.Minute := 0;
    Inc(P);
  end else

    { allow '+' or '-' }
    if P^ in ['+','-'] then
    begin
      N := P^ = '-';
      Inc(P);

    { expect 00..13 }
      T.Hour := 0; I := 2;
      while (P^ in ['0'..'9']) and (I > 0) do
      begin
        T.Hour := 10*T.Hour + Ord(P^) - Ord('0');
        Inc(P); Dec(I);
      end;
      if T.Hour > 13 then
        Exit(False);
      if N then
        T.Hour := -T.Hour;

    { expect ':' }
      if P^ <> ':' then
        Exit(False);
      Inc(P);

    { expect 00..59 }
      T.Minute := 0; I := 2;
      while (P^ in ['0'..'9']) and (I > 0) do
      begin
        T.Minute := 10*T.Minute + Ord(P^) - Ord('0');
        Inc(P); Dec(I);
      end;
      if T.Minute > 59 then
        Exit(False);
    end else

      { unknown }
      begin
        T.Kind := tzUnknown;
        T.Hour := 0;
        T.Minute := 0;
      end;

  Result := True;
end;

function __parseDate(var P: PChar; var Year, Month, Day: Longword; BC: PBoolean): Boolean;
var
  I: Integer;
begin
{ allow '-' }
  if P^ = '-' then
  begin
    if Assigned(BC) then
      BC^ := True
    else
      Exit(False);
    Inc(P);
  end else
    if Assigned(BC) then
      BC^ := False;

{ expect Integer }
  Year := 0;
  while P^ in ['0'..'9'] do
  begin
    Year := 10*Year + Ord(P^) - Ord('0');
    Inc(P);
  end;

{ expect '-' }
  if P^ <> '-' then
    Exit(False);
  Inc(P);

{ expect 01..12 }
  Month := 0; I := 2;
  while (P^ in ['0'..'9']) and (I > 0) do
  begin
    Month := 10*Month + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if (Month < 1) or (Month > 12) then
    Exit(False);

{ expect '-' }
  if P^ <> '-' then
    Exit(False);
  Inc(P);

{ expect 01..31 }
  Day := 0; I := 2;
  while (P^ in ['0'..'9']) and (I > 0) do
  begin
    Day := 10*Day + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if (Day < 1) or (Day > 31) then
    Exit(False);

  Result := True;
end;

function __parseTime(P: PChar; var Hour, Minute, Second, Milliseconds: Longword): Boolean;
var
  I: Integer;
begin
{ expect 00..24 }
  Hour := 0; I := 2;
  while (P^ in ['0'..'9']) and (I > 0) do
  begin
    Hour := 10*Hour + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if Hour > 24 then
    Exit(False);

{ expect ':' }
  if P^ <> ':' then
    Exit(False);
  Inc(P);

{ expect 00..59 }
  Minute := 0; I := 2;
  while (P^ in ['0'..'9']) and (I > 0) do
  begin
    Minute := 10*Minute + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if (Minute > 59) or ((Hour = 24) and (Minute > 0)) then
    Exit(False);

{ expect ':' }
  if P^ <> ':' then
    Exit(False);
  Inc(P);

{ expect 00..59 }
  Second := 0; I := 2;
  while (P^ in ['0'..'9']) and (I > 0) do
  begin
    Second := 10*Second + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if (Second > 59) or ((Hour = 24) and (Second > 0)) then
    Exit(False);

{ allow '.' }
  if P^ = '.' then
  begin
    Inc(P);

    { expect Integer }
    Milliseconds := 0;
    while P^ in ['0'..'9'] do
    begin
      Milliseconds := 10*Milliseconds + Ord(P^) - Ord('0');
      Inc(P);
    end;
    if (Hour = 24) and (Milliseconds > 0) then
      Exit(False);
  end else
    Milliseconds := 0;

  Result := True;
end;

function xsdParseDate(Chars: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
var
  P: PChar;
  T: TTimezone;
begin
  P := PChar(Chars);
  Result :=
    Assigned(P) and
    __parseDate(P, Year, Month, Day, BC) and
    __parseTimezone(P, T) and
    (P^ = #0);

{ assig Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdDateConvertTo(Year, Month, Day, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdParseDate(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Year, Month, Day: Longword;
begin
  if xsdParseDate(Chars, Year, Month, Day, Timezone, nil) then
    Result := TryEncodeDate(Year, Month, Day, Value)
  else
    Result := False;
end;

function xsdParseTime(Chars: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): Boolean;
var
  P: PChar;
  T: TTimezone;
begin
  P := PChar(Chars);
  Result :=
    Assigned(P) and
    __parseTime(P, Hour, Minute, Second, Milliseconds) and
    __parseTimezone(P, T) and
    (P^ = #0);

{ assig Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdTimeConvertTo(Hour, Minute, Second, Milliseconds, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdParseTime(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Hour, Minute, Second, Milliseconds: Longword;
begin
  if xsdParseTime(Chars, Hour, Minute, Second, Milliseconds, Timezone) then
    Result := TryEncodeTime(Hour, Minute, Second, Milliseconds, Value)
  else
    Result := False;
end;

function xsdParseDateTime(Chars: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;

    function __parseT(var P: PChar): Boolean;
    begin
      Result := P^ = 'T';
      if Result then Inc(P);
    end;

var
  P: PChar;
  T: TTimezone;
begin
  P := PChar(Chars);
  Result :=
    Assigned(P) and
    __parseDate(P, Year, Month, Day, BC) and
    __parseT(P) and
    __parseTime(P, Hour, Minute, Second, Milliseconds) and
    __parseTimezone(P, T) and
    (P^ = #0);

{ assig Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdDateTimeConvertTo(Year, Month, Day, Hour, Minute, Second, Milliseconds, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdParseDateTime(Chars: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Year, Month, Day: Longword;
  Hour, Minute, Second, Milliseconds: Longword;
begin
  if xsdParseDateTime(Chars, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone) then
    Result := TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds, Value)
  else
    Result := False;
end;

function xsdParseDecimal(Chars: xmlCharPtr; var Value: Extended): Boolean;
begin
  Result := TryStrToFloat(PChar(Chars), Value);
end;

function xsdParseDouble(Chars: xmlCharPtr; var Value: Double): Boolean;
begin
  Result := TryStrToFloat(PChar(Chars), Value);
end;

function xsdParseFloat(Chars: xmlCharPtr; var Value: Single): Boolean;
begin
  Result := TryStrToFloat(PChar(Chars), Value);
end;

function xsdParseInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
var
  P: PChar;
  N: Boolean;
begin
  P := PChar(Chars);
  if not Assigned(P) then
    Exit(False);

{ allow '-' }
  N := P^ = '-';
  if N then
    Inc(P);

{ read Integer }
  Value := 0;
  while P^ in ['0'..'9'] do
  begin
    Value := 10*Value + Ord(P^) - Ord('0');
    Inc(P);
  end;
  if N then
    Value := -Value;

{ expect #0 }
  Result := P^ = #0;
end;

function xsdParseNonNegativeInteger(Chars: xmlCharPtr; var Value: QWord): Boolean;
var
  P: PChar;
begin
  P := PChar(Chars);
  if not Assigned(P) then
    Exit(False);

{ read Integer }
  Value := 0;
  while P^ in ['0'..'9'] do
  begin
    Value := 10*Value + Ord(P^) - Ord('0');
    Inc(P);
  end;

{ expect #0 }
  Result := P^ = #0;
end;

function xsdParseNonPositiveInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
begin
  Result := xsdParseInteger(Chars, Value) and (Value <= 0);
end;

function xsdParseNegativeInteger(Chars: xmlCharPtr; var Value: Int64): Boolean;
begin
  Result := xsdParseInteger(Chars, Value) and (Value <= -1);
end;

function xsdParsePositiveInteger(Chars: xmlCharPtr; var Value: QWord): Boolean;
begin
  Result := xsdParseNonNegativeInteger(Chars, Value) and (Value >= 1);
end;

function xsdParseByte(Chars: xmlCharPtr; var Value: Shortint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdParseInteger(Chars, Tmp) and (Tmp <= 128) and (Tmp >= -127);
  Value := Tmp;
end;

function xsdParseShort(Chars: xmlCharPtr; var Value: Smallint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdParseInteger(Chars, Tmp) and (Tmp <= 32767) and (Tmp >= -32768);
  Value := Tmp;
end;

function xsdParseInt(Chars: xmlCharPtr; var Value: Longint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdParseInteger(Chars, Tmp) and (Tmp <= 2147483647) and (Tmp >= -2147483648);
  Value := Tmp;
end;

function xsdParseLong(Chars: xmlCharPtr; var Value: Int64): Boolean;
begin
  Result := xsdParseInteger(Chars, Value);
end;

function xsdParseUnsignedByte(Chars: xmlCharPtr; var Value: Byte): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdParseNonNegativeInteger(Chars, Tmp) and (Tmp <= 255);
  Value := Tmp;
end;

function xsdParseUnsignedShort(Chars: xmlCharPtr; var Value: Word): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdParseNonNegativeInteger(Chars, Tmp) and (Tmp <= 65535);
  Value := Tmp;
end;

function xsdParseUnsignedInt(Chars: xmlCharPtr; var Value: Longword): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdParseNonNegativeInteger(Chars, Tmp) and (Tmp <= 4294967295);
  Value := Tmp;
end;

function xsdParseUnsignedLong(Chars: xmlCharPtr; var Value: QWord): Boolean;
begin
  Result := xsdParseNonNegativeInteger(Chars, Value)
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

function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Hour, Minute, Second, Milliseconds, Timezone);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Time: TDateTime; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Time, Timezone);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword; BC: Boolean; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Year, Month, Day, BC, Timezone);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDate(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Date: TDateTime; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Date, Timezone);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds, BC, Timezone);
  Result := xmlNewChild(parent, ns, name, BAD_CAST(Tmp));
end;

function xsdNewChildDateTime(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; DateTime: TDateTime; Timezone: PTimezone): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(DateTime, Timezone);
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

function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Hour, Minute, Second, Milliseconds, Timezone);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Time: TDateTime; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatTime(Time, Timezone);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day: Longword; BC: Boolean; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Year, Month, Day, BC, Timezone);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDate(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Date: TDateTime; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDate(Date, Timezone);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; BC: Boolean; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds, BC, Timezone);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdNewPropDateTime(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; DateTime: TDateTime; Timezone: PTimezone): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatDateTime(DateTime, Timezone);
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

function xsdTestNode(node: xmlNodePtr; name, nameSpace: xmlCharPtr): Boolean;
begin
  Result := (xmlStrEqual(name, node^.name) <> 0) and ((nameSpace = NS_IGNORE) or
    ((nameSpace = NS_EXCLUDE) and (node^.ns = nil)) or
    ((nameSpace <> NS_EXCLUDE) and (nameSpace <> NS_IGNORE) and (node^.ns <> nil) and (xmlStrEqual(nameSpace, node^.ns^.href) <> 0)));
end;

function xsdHasChild(node: xmlNodePtr; name: xmlCharPtr; Index: integer): xmlNodePtr;
begin
  Result := xsdHasNsChild(node, name, NS_IGNORE, Index);
end;

function xsdHasNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: integer): xmlNodePtr;
begin
  if Assigned(node) and (Index >= 0) then
  begin
    Result := node^.children;
    while Assigned(Result) do
    begin
      if xsdTestNode(Result, name, nameSpace) then
      begin
        if Index = 0 then
          Exit;
        Dec(Index);
      end;
      Result := Result^.next;
    end;
  end else
    Result := nil;
end;

function xsdGetChild(node: xmlNodePtr; name: xmlCharPtr; Index: Integer): xmlCharPtr;
begin
  Result := xsdGetNsChild(node, name, NS_IGNORE, Index);
end;

function xsdGetNsChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer): xmlCharPtr;
var
  child: xmlNodePtr;
begin
  child := xsdHasNsChild(node, name, nameSpace, Index);
  if Assigned(child) then
    result := xmlNodeGetContent(child)
  else
    result := nil;
end;

function xsdTryGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
  begin
    Value := pchar(chars);
    result := true;
  end else
    result := false;
end;

function xsdTryGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseBoolean(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDate(chars, Year, Month, Day, Timezone, BC)
  else
    result := false;
end;

function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDate(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseTime(chars, Hour, Minute, Second, Milliseconds, Timezone)
  else
    result := false;
end;

function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseTime(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDateTime(chars, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC)
  else
    result := false;
end;

function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDateTime(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDecimal(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseDouble(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseFloat(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseByte(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseShort(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseInt(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseLong(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseUnsignedByte(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseUnsignedShort(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseUnsignedInt(chars, Value)
  else
    result := false;
end;

function xsdTryGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord; Index: Integer): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsChild(node, name, nameSpace, Index);
  if assigned(chars) then
    result := xsdParseUnsignedLong(chars, Value)
  else
    result := false;
end;

procedure xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String; Index: Integer);
begin
  if not xsdTryGetChildString(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean; Index: Integer);
begin
  if not xsdTryGetChildBoolean(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer);
begin
  if not xsdTryGetChildDate(node, name, nameSpace, Year, Month, Day, Timezone, BC, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  if not xsdTryGetChildDate(node, name, nameSpace, Value, Timezone, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; Index: Integer);
begin
  if not xsdTryGetChildTime(node, name, nameSpace, Hour, Minute, Second, Milliseconds, Timezone, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  if not xsdTryGetChildTime(node, name, nameSpace, Value, Timezone, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer);
begin
  if not xsdTryGetChildDateTime(node, name, nameSpace, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  if not xsdTryGetChildDateTime(node, name, nameSpace, Value, Timezone, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended; Index: Integer);
begin
  if not xsdTryGetChildDecimal(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double; Index: Integer);
begin
  if not xsdTryGetChildDouble(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single; Index: Integer);
begin
  if not xsdTryGetChildFloat(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint; Index: Integer);
begin
  if not xsdTryGetChildByte(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint; Index: Integer);
begin
  if not xsdTryGetChildShort(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint; Index: Integer);
begin
  if not xsdTryGetChildInt(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64; Index: Integer);
begin
  if not xsdTryGetChildLong(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte; Index: Integer);
begin
  if not xsdTryGetChildUnsignedByte(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word; Index: Integer);
begin
  if not xsdTryGetChildUnsignedShort(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword; Index: Integer);
begin
  if not xsdTryGetChildUnsignedInt(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

procedure xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord; Index: Integer);
begin
  if not xsdTryGetChildUnsignedLong(node, name, nameSpace, Value, Index) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

function xsdTestProp(attr: xmlAttrPtr; name, nameSpace: xmlCharPtr): Boolean;
begin
  Result := (xmlStrEqual(name, attr^.name) <> 0) and ((nameSpace = NS_IGNORE) or
    ((nameSpace = NS_EXCLUDE) and (attr^.ns = nil)) or
    ((nameSpace <> NS_EXCLUDE) and (nameSpace <> NS_IGNORE) and (attr^.ns <> nil) and (xmlStrEqual(nameSpace, attr^.ns^.href) <> 0)));
end;

function xsdHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr;
begin
  result := xsdHasNsProp(node, name, NS_IGNORE);
end;

function xsdHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
begin
  if Assigned(node) then
  begin
    Result := node^.properties;
    while Assigned(Result) do
    begin
      if xsdTestProp(Result, name, nameSpace) then
        Exit;
      Result := Result^.next;
    end;
  end else
    Result := nil;

  {if nameSpace = NS_EXCLUDE then
  begin
    result := xmlHasProp(node, name);
    if Assigned(result) and (result^.ns <> nil) then
      result := nil;
  end else
    result := xmlHasNsProp(node, name, nameSpace);}
end;

function xsdGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr;
begin
  result := xsdGetNsProp(node, name, NS_IGNORE);
end;

function xsdGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
var
  prop: xmlAttrPtr;
begin
  prop := xsdHasNsProp(node, name, nameSpace);
  if Assigned(prop) then
    result := xmlNodeGetContent(prop^.children)
  else
    result := nil;
  {if nameSpace = NS_EXCLUDE then
    result := xmlGetNoNsProp(node, name)
  else
    result := xmlGetNsProp(node, name, nameSpace);}
end;

function xsdTryGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
  begin
    Value := pchar(chars);
    Result := true;
  end else
    result := false;
end;

function xsdTryGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseBoolean(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDate(chars, Year, Month, Day, Timezone, BC)
  else
    result := false;
end;

function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDate(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseTime(chars, Hour, Minute, Second, Milliseconds, Timezone)
  else
    result := false;
end;

function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseTime(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDateTime(chars, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC)
  else
    result := false;
end;

function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDateTime(chars, Value, Timezone)
  else
    result := false;
end;

function xsdTryGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDecimal(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseDouble(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseFloat(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseByte(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseShort(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseInt(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseLong(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseUnsignedByte(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseUnsignedShort(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseUnsignedInt(chars, Value)
  else
    result := false;
end;

function xsdTryGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord): Boolean;
var
  chars: xmlCharPtr;
begin
  chars := xsdGetNsProp(node, name, nameSpace);
  if assigned(chars) then
    result := xsdParseUnsignedLong(chars, Value)
  else
    result := false;
end;

procedure xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: String);
begin
  if not xsdTryGetPropString(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Boolean);
begin
  if not xsdTryGetPropBoolean(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  if not xsdTryGetPropDate(node, name, nameSpace, Year, Month, Day, Timezone, BC) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryGetPropDate(node, name, nameSpace, Value, Timezone) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone);
begin
  if not xsdTryGetPropTime(node, name, nameSpace, Hour, Minute, Second, Milliseconds, Timezone) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryGetPropTime(node, name, nameSpace, Value, Timezone) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  if not xsdTryGetPropDateTime(node, name, nameSpace, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryGetPropDateTime(node, name, nameSpace, Value, Timezone) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Extended);
begin
  if not xsdTryGetPropDecimal(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Double);
begin
  if not xsdTryGetPropDouble(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Single);
begin
  if not xsdTryGetPropFloat(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Shortint);
begin
  if not xsdTryGetPropByte(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Smallint);
begin
  if not xsdTryGetPropShort(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longint);
begin
  if not xsdTryGetPropInt(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Int64);
begin
  if not xsdTryGetPropLong(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Byte);
begin
  if not xsdTryGetPropUnsignedByte(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Word);
begin
  if not xsdTryGetPropUnsignedShort(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: Longword);
begin
  if not xsdTryGetPropUnsignedInt(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

procedure xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; var Value: QWord);
begin
  if not xsdTryGetPropUnsignedLong(node, name, nameSpace, Value) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

function xsdRemoveBlanks(content: xmlCharPtr; var cleaned: string): boolean;
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
