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
  Math,
  DateUtils,
  SysUtils;

resourcestring
  ParserError = 'parsing "%s" failed';
  ChildNotFound = 'child %s not found';
  PropNotFound  = 'attribute %s not found';

const
  IGNORE_LAST = Pointer(-1);

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
function xsdFormatEnum(enum: array of String; Value: Integer): String;

{ DateTime functions }
procedure xsdTimeConvertTo(var Hour, Minute, Second, Milliseconds: Longword; const Current, Target: TTimezone);
procedure xsdDateConvertTo(var Year, Month, Day: Longword; const Current, Target: TTimezone);
procedure xsdDateTimeConvertTo(var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; const Current, Target: TTimezone);

{ Parse functions }
function xsdTryParseString(Chars, Last: xmlCharPtr; out Value: String): Boolean;
function xsdTryParseBoolean(Chars, Last: xmlCharPtr; out Value: Boolean): Boolean;
function xsdTryParseDate(Chars, Last: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryParseDate(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryParseTime(Chars, Last: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): Boolean;
function xsdTryParseTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryParseDateTime(Chars, Last: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryParseDateTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryParseDecimal(Chars, Last: xmlCharPtr; out Value: Extended): Boolean;
function xsdTryParseDouble(Chars, Last: xmlCharPtr; out Value: Double): Boolean;
function xsdTryParseFloat(Chars, Last: xmlCharPtr; out Value: Single): Boolean;
function xsdTryParseInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryParseNonNegativeInteger(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
function xsdTryParseNonPositiveInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryParseNegativeInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryParsePositiveInteger(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
function xsdTryParseByte(Chars, Last: xmlCharPtr; out Value: Shortint): Boolean;
function xsdTryParseShort(Chars, Last: xmlCharPtr; out Value: Smallint): Boolean;
function xsdTryParseInt(Chars, Last: xmlCharPtr; out Value: Longint): Boolean;
function xsdTryParseLong(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryParseUnsignedByte(Chars, Last: xmlCharPtr; out Value: Byte): Boolean;
function xsdTryParseUnsignedShort(Chars, Last: xmlCharPtr; out Value: Word): Boolean;
function xsdTryParseUnsignedInt(Chars, Last: xmlCharPtr; out Value: Longword): Boolean;
function xsdTryParseUnsignedLong(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
function xsdTryParseEnum(Chars, Last: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;

procedure xsdParseString(Chars, Last: xmlCharPtr; out Value: String);
procedure xsdParseBoolean(Chars, Last: xmlCharPtr; out Value: Boolean);
procedure xsdParseDate(Chars, Last: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdParseDate(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdParseTime(Chars, Last: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil);
procedure xsdParseTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdParseDateTime(Chars, Last: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdParseDateTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdParseDecimal(Chars, Last: xmlCharPtr; out Value: Extended);
procedure xsdParseDouble(Chars, Last: xmlCharPtr; out Value: Double);
procedure xsdParseFloat(Chars, Last: xmlCharPtr; out Value: Single);
procedure xsdParseInteger(Chars, Last: xmlCharPtr; out Value: Int64);
procedure xsdParseNonNegativeInteger(Chars, Last: xmlCharPtr; out Value: QWord);
procedure xsdParseNonPositiveInteger(Chars, Last: xmlCharPtr; out Value: Int64);
procedure xsdParseNegativeInteger(Chars, Last: xmlCharPtr; out Value: Int64);
procedure xsdParsePositiveInteger(Chars, Last: xmlCharPtr; out Value: QWord);
procedure xsdParseByte(Chars, Last: xmlCharPtr; out Value: Shortint);
procedure xsdParseShort(Chars, Last: xmlCharPtr; out Value: Smallint);
procedure xsdParseInt(Chars, Last: xmlCharPtr; out Value: Longint);
procedure xsdParseLong(Chars, Last: xmlCharPtr; out Value: Int64);
procedure xsdParseUnsignedByte(Chars, Last: xmlCharPtr; out Value: Byte);
procedure xsdParseUnsignedShort(Chars, Last: xmlCharPtr; out Value: Word);
procedure xsdParseUnsignedInt(Chars, Last: xmlCharPtr; out Value: Longword);
procedure xsdParseUnsignedLong(Chars, Last: xmlCharPtr; out Value: QWord);
procedure xsdParseEnum(Chars, Last: xmlCharPtr; enum: array of String; out Value: Integer);

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
function xsdNewChildEnum(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; enum: array of string; Value: Integer): xmlNodePtr;

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
function xsdNewPropEnum(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; enum: array of string; Value: Integer): xmlAttrPtr;

{ Query functions }
const
  NS_IGNORE  : xmlCharPtr = nil;
  NS_EXCLUDE : xmlCharPtr = pointer(-1);

{ Node query functions }
function xsdTestNodeNs(node: xmlNodePtr; nameSpace: xmlCharPtr): Boolean;
function xsdTestNode(node: xmlNodePtr; name, nameSpace: xmlCharPtr): Boolean;

function xsdTryGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlNodePtr;
function xsdTryGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out child: xmlNodePtr; Index: Integer = 0): Boolean;
function xsdTryGetChildChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
function xsdTryGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String; Index: Integer = 0): Boolean;
function xsdTryGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean; Index: Integer = 0): Boolean;
function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0): Boolean;
function xsdTryGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended; Index: Integer = 0): Boolean;
function xsdTryGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double; Index: Integer = 0): Boolean;
function xsdTryGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single; Index: Integer = 0): Boolean;
function xsdTryGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint; Index: Integer = 0): Boolean;
function xsdTryGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint; Index: Integer = 0): Boolean;
function xsdTryGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint; Index: Integer = 0): Boolean;
function xsdTryGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword; Index: Integer = 0): Boolean;
function xsdTryGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord; Index: Integer = 0): Boolean;
function xsdTryGetChildEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer; Index: Integer = 0): Boolean;

function xsdGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlNodePtr;
function xsdGetChildChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer = 0): xmlCharPtr;
procedure xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String; Index: Integer = 0);
procedure xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean; Index: Integer = 0);
procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0);
procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil; Index: Integer = 0);
procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil; Index: Integer = 0);
procedure xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended; Index: Integer = 0);
procedure xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double; Index: Integer = 0);
procedure xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single; Index: Integer = 0);
procedure xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint; Index: Integer = 0);
procedure xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint; Index: Integer = 0);
procedure xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint; Index: Integer = 0);
procedure xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64; Index: Integer = 0);
procedure xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte; Index: Integer = 0);
procedure xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word; Index: Integer = 0);
procedure xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword; Index: Integer = 0);
procedure xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord; Index: Integer = 0);
procedure xsdGetChildEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer; Index: Integer = 0);

{ Node parsing functions }
function xsdTryNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlNodePtr;
function xsdTryNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out last: xmlNodePtr): Boolean;
function xsdTryNextChars(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
function xsdTryNextString(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String): Boolean;
function xsdTryNextBoolean(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean): Boolean;
function xsdTryNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): Boolean;
function xsdTryNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryNextDecimal(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended): Boolean;
function xsdTryNextDouble(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double): Boolean;
function xsdTryNextFloat(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single): Boolean;
function xsdTryNextByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint): Boolean;
function xsdTryNextShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint): Boolean;
function xsdTryNextInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint): Boolean;
function xsdTryNextLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryNextUnsignedByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte): Boolean;
function xsdTryNextUnsignedShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word): Boolean;
function xsdTryNextUnsignedInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword): Boolean;
function xsdTryNextUnsignedLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord): Boolean;
function xsdTryNextEnum(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;

function xsdNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlNodePtr;
function xsdNextChars(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
procedure xsdNextString(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String);
procedure xsdNextBoolean(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean);
procedure xsdNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil);
procedure xsdNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdNextDecimal(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended);
procedure xsdNextDouble(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double);
procedure xsdNextFloat(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single);
procedure xsdNextByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint);
procedure xsdNextShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint);
procedure xsdNextInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint);
procedure xsdNextLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64);
procedure xsdNextUnsignedByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte);
procedure xsdNextUnsignedShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word);
procedure xsdNextUnsignedInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword);
procedure xsdNextUnsignedLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord);
procedure xsdNextEnum(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer);

{ Property query functions }
function xsdTestPropNs(attr: xmlAttrPtr; nameSpace: xmlCharPtr): Boolean;
function xsdTestProp(attr: xmlAttrPtr; name, nameSpace: xmlCharPtr): Boolean;

function xsdTryGetProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
function xsdTryGetPropChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
function xsdTryGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String): Boolean;
function xsdTryGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean): Boolean;
function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil): Boolean;
function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil): Boolean;
function xsdTryGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended): Boolean;
function xsdTryGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double): Boolean;
function xsdTryGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single): Boolean;
function xsdTryGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint): Boolean;
function xsdTryGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint): Boolean;
function xsdTryGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint): Boolean;
function xsdTryGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64): Boolean;
function xsdTryGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte): Boolean;
function xsdTryGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word): Boolean;
function xsdTryGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword): Boolean;
function xsdTryGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord): Boolean;
function xsdTryGetPropEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;

function xsdGetProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
function xsdGetPropChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
procedure xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String);
procedure xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean);
procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil);
procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone = nil; BC: PBoolean = nil);
procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone = nil);
procedure xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended);
procedure xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double);
procedure xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single);
procedure xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint);
procedure xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint);
procedure xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint);
procedure xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64);
procedure xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte);
procedure xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word);
procedure xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword);
procedure xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord);
procedure xsdGetPropEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer);

function xsdRemoveBlanks(content: xmlCharPtr; out cleaned: string): boolean;

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
            Result := Result + '+'
          else
            Result := Result + '-';
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
            Result := Result + '+'
          else
            Result := Result + '-';
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

function xsdFormatEnum(enum: array of String; Value: Integer): String;
begin
  Result := enum[Value];
end;

procedure xsdTimeConvertTo(var Hour, Minute, Second, Milliseconds: Longword; const Current, Target: TTimezone);
begin
  {$warning not implemented}
end;

procedure xsdDateConvertTo(var Year, Month, Day: Longword; const Current, Target: TTimezone);
begin
  {$warning not implemented}
end;

procedure xsdDateTimeConvertTo(var Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; const Current, Target: TTimezone);
begin
  {$warning not implemented}
end;

function __parseNonNegativeInteger(var P: PChar; const L: PChar; out Value: QWord): Boolean;
begin
  { expect integer }
  Value := 0;
  while (P <= L) and (P^ in ['0'..'9']) do
  begin
    Value := 10*Value + Ord(P^) - Ord('0');
    Inc(P);
  end;

  Result := True;
end;

function __parseInteger(var P: PChar; const L: PChar; out Value: Int64): Boolean;
var
  N: Boolean;
begin
  { allow '-' }
  N := (P <= L) and (P^ = '-');
  if N then
    Inc(P);

  { expect integer }
  Value := 0;
  while (P <= L) and (P^ in ['0'..'9']) do
  begin
    Value := 10*Value + Ord(P^) - Ord('0');
    Inc(P);
  end;
  if N then
    Value := -Value;

  Result := True;
end;

function __parseFloat(var P: PChar; const L: PChar; out Value: Extended): Boolean;
var
  N: Boolean;
  Exp: Int64;
  Int: QWord;
begin
  { allow 'Nan' }
  if (P+2 <= L) and ((P^ = 'N') or (P^ = 'n')) then
  begin
    Inc(P);
    if (P^ <> 'A') and (P^ <> 'a') then Exit(False);
    Inc(P);
    if (P^ <> 'N') and (P^ <> 'n') then Exit(False);
    Inc(P);
    Value := Nan;
    Result := True;
    Exit;
  end;

  { allow '-' }
  N := (P <= L) and (P^ = '-');
  if N then
    Inc(P);

  { allow 'Inf' }
  if (P+2 <= L) and ((P^ = 'I') or (P^ = 'i')) then
  begin
    Inc(P);
    if (P^ <> 'N') and (P^ <> 'n') then Exit(False);
    Inc(P);
    if (P^ <> 'F') and (P^ <> 'f') then Exit(False);
    Inc(P);
    if N then
      Value := NegInfinity
    else
      Value := Infinity;
    Result := True;
    Exit;
  end;

  { expect integer }
  Int := 0;
  while (P <= L) and (P^ in ['0'..'9']) do
  begin
    Int := 10*Int + Ord(P^) - Ord('0');
    Inc(P);
  end;
  Value := Int;
  if N then
    Value := -Value;

  { allow '.' }
  if (P <= L) and (P^ = '.') then
  begin
    Inc(P);

    { expect integer }
    Exp := 1;
    Int := 0;
    while (P <= L) and (P^ in ['0'..'9']) do
    begin
      Int := 10*Int + Ord(P^) - Ord('0');
      Exp := 10*Exp;
      Inc(P);
    end;
    Value := Value + Int / Exp;
  end;

  { allow 'E' or 'e' }
  if (P <= L) and ((P^ = 'E') or (P^ = 'e')) then
  begin
    Inc(P);

    { expect integer }
    if not __parseInteger(P, L, Exp) then
      Exit(False);

    while Exp > 0 do
    begin
      Value := Value * 10;
      Dec(Exp);
    end;

    while Exp < 0 do
    begin
      Value := Value * 0.1;
      Inc(Exp);
    end;
  end;

  Result := True;
end;

function __parseTimezone(var P: PChar; const L: PChar; out T: TTimezone): Boolean;
var
  I: Integer;
  N: Boolean;
begin
  { allow 'Z' }
  if (P <= L) and (P^ = 'Z') then
  begin
    T.Kind := tzUTC;
    T.Hour := 0;
    T.Minute := 0;
    Inc(P);
  end else

    { allow '+' or '-' }
    if (P <= L) and (P^ in ['+','-']) then
    begin
      N := P^ = '-';
      Inc(P);

      { expect 00..13 }
      T.Hour := 0; I := 2;
      while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
      begin
        T.Hour := 10*T.Hour + Ord(P^) - Ord('0');
        Dec(I); Inc(P);
      end;
      if T.Hour > 13 then
        Exit(False);
      if N then
        T.Hour := -T.Hour;

      { expect ':' }
      if (P > L) or (P^ <> ':') then
        Exit(False);
      Inc(P);

      { expect 00..59 }
      T.Minute := 0; I := 2;
      while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
      begin
        T.Minute := 10*T.Minute + Ord(P^) - Ord('0');
        Dec(I); Inc(P);
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

function __parseDate(var P: PChar; const L: PChar; out Year, Month, Day: Longword; BC: PBoolean): Boolean;
var
  I: Integer;
begin
  { allow '-' }
  if (P <= L) and (P^ = '-') then
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
  while (P <= L) and (P^ in ['0'..'9']) do
  begin
    Year := 10*Year + Ord(P^) - Ord('0');
    Inc(P);
  end;

  { expect '-' }
  if (P > L) or (P^ <> '-') then
    Exit(False);
  Inc(P);

  { expect 01..12 }
  Month := 0; I := 2;
  while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
  begin
    Month := 10*Month + Ord(P^) - Ord('0');
    Dec(I); Inc(P);
  end;
  if (Month < 1) or (Month > 12) then
    Exit(False);

  { expect '-' }
  if (P > L) or (P^ <> '-') then
    Exit(False);
  Inc(P);

  { expect 01..31 }
  Day := 0; I := 2;
  while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
  begin
    Day := 10*Day + Ord(P^) - Ord('0');
    Dec(I); Inc(P);
  end;
  if (Day < 1) or (Day > 31) then
    Exit(False);

  Result := True;
end;

function __parseTime(var P: PChar; const L: PChar; out Hour, Minute, Second, Milliseconds: Longword): Boolean;
var
  I: Integer;
begin
  { expect 00..24 }
  Hour := 0; I := 2;
  while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
  begin
    Hour := 10*Hour + Ord(P^) - Ord('0');
    Inc(P); Dec(I);
  end;
  if Hour > 24 then
    Exit(False);

  { expect ':' }
  if (P > L) or (P^ <> ':') then
    Exit(False);
  Inc(P);

  { expect 00..59 }
  Minute := 0; I := 2;
  while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
  begin
    Minute := 10*Minute + Ord(P^) - Ord('0');
    Dec(I); Inc(P);
  end;
  if (Minute > 59) or ((Hour = 24) and (Minute > 0)) then
    Exit(False);

  { expect ':' }
  if (P > L) or (P^ <> ':') then
    Exit(False);
  Inc(P);

  { expect 00..59 }
  Second := 0; I := 2;
  while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
  begin
    Second := 10*Second + Ord(P^) - Ord('0');
    Dec(I); Inc(P);
  end;
  if (Second > 59) or ((Hour = 24) and (Second > 0)) then
    Exit(False);

  { allow '.' }
  if (P <= L) and (P^ = '.') then
  begin
    Inc(P);

    { expect integer }
    Milliseconds := 0; I := 4;
    while (P <= L) and (P^ in ['0'..'9']) and (I > 0) do
    begin
      Milliseconds := 10*Milliseconds + Ord(P^) - Ord('0');
      Dec(I); Inc(P);
    end;
    if (Milliseconds > 999) or ((Hour = 24) and (Milliseconds > 0)) then
      Exit(False);
  end else
    Milliseconds := 0;

  Result := True;
end;

function xsdTryParseString(Chars, Last: xmlCharPtr; out Value: String): Boolean;
var
  Len: Integer;
begin
  if Assigned(Chars) then
    if Assigned(Last) then
    begin
      Len := Last-Chars+1;
      if Len > 0 then
      begin
        SetLength(Value, Len);
        Move(Chars^, Value[1], Len);
        Result := True;
      end else
        Result := False;
    end else begin
      Value := PChar(Chars);
      Result := True;
    end
  else
    Result := False;
end;

function __strpas(Chars, Last: xmlCharPtr): String;
begin
  if not xsdTryParseString(Chars, Last, Result) then
    Result := '';
end;

function xsdTryParseBoolean(Chars, Last: xmlCharPtr; out Value: Boolean): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
  Num: QWord;
  Len: Integer;
begin
  if not Assigned(Last) then
  begin
    P := PChar(Chars);
    Len := 0;
    while (Len < 7) and (P^ <> #0) do
    begin
      Inc(Len);
      Inc(P);
    end;
  end else
    Len := Last-Chars+1;

  case Len of
    1: Num := PByte(Chars)^;
    4: Num := PLongword(Chars)^;
    5: Num := PLongword(Chars)^ or (QWord(Chars[4]) shl 32);
    else Exit(False);
  end;

  //writeln(Len, ', ', IntToHex(Num,16));

  case Num of
    $30,
    $65736C6166,$65736C6146,$65736C4166,$65736C4146,$65734C6166,$65734C6146,$65734C4166,$65734C4146,
    $65536C6166,$65536C6146,$65536C4166,$65536C4146,$65534C6166,$65534C6146,$65534C4166,$65534C4146,
    $45736C6166,$45736C6146,$45736C4166,$45736C4146,$45734C6166,$45734C6146,$45734C4166,$45734C4146,
    $45536C6166,$45536C6146,$45536C4166,$45536C4146,$45534C6166,$45534C6146,$45534C4166,$45534C4146:
      Value := False;
    $31,
    $65757274,$65757254,$65755274,$65755254,$65557274,$65557254,$65555274,$65555254,
    $45757274,$45757254,$45755274,$45755254,$45557274,$45557254,$45555274,$45555254:
      Value := True;
    else Exit(False);
  end;

  Result := True;
end;

function xsdTryParseDate(Chars, Last: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
  T: TTimezone;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and
      __parseDate(P, L, Year, Month, Day, BC) and
      __parseTimezone(P, L, T) and (P = L+1)
  else
    Result := Assigned(P) and
      __parseDate(P, IGNORE_LAST, Year, Month, Day, BC) and
      __parseTimezone(P, IGNORE_LAST, T) and (P^ = #0);

  { assign Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdDateConvertTo(Year, Month, Day, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdTryParseDate(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Year, Month, Day: Longword;
begin
  if xsdTryParseDate(Chars, Last, Year, Month, Day, Timezone, nil) then
    Result := TryEncodeDate(Year, Month, Day, Value)
  else
    Result := False;
end;

function xsdTryParseTime(Chars, Last: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
  T: TTimezone;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and
      __parseTime(P, L, Hour, Minute, Second, Milliseconds) and
      __parseTimezone(P, L, T) and (P = L+1)
  else
    Result := Assigned(P) and
      __parseTime(P, IGNORE_LAST, Hour, Minute, Second, Milliseconds) and
      __parseTimezone(P, IGNORE_LAST, T) and (P^ = #0);

  { assign Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdTimeConvertTo(Hour, Minute, Second, Milliseconds, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdTryParseTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Hour, Minute, Second, Milliseconds: Longword;
begin
  if xsdTryParseTime(Chars, Last, Hour, Minute, Second, Milliseconds, Timezone) then
    Result := TryEncodeTime(Hour, Minute, Second, Milliseconds, Value)
  else
    Result := False;
end;

function xsdTryParseDateTime(Chars, Last: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;

    function __parseT(var P: PChar; const L: PChar): Boolean;
    begin
      Result := (P <= L) and (P^ = 'T');
      if Result then Inc(P);
    end;

var
  P: PChar;
  L: PChar absolute Last;
  T: TTimezone;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and
      __parseDate(P, L, Year, Month, Day, BC) and
      __parseT(P, L) and
      __parseTime(P, L, Hour, Minute, Second, Milliseconds) and
      __parseTimezone(P, L, T) and (P = L+1)
  else
    Result := Assigned(P) and
      __parseDate(P, IGNORE_LAST, Year, Month, Day, BC) and
      __parseT(P, IGNORE_LAST) and
      __parseTime(P, IGNORE_LAST, Hour, Minute, Second, Milliseconds) and
      __parseTimezone(P, IGNORE_LAST, T) and (P^ = #0);

  { assign Timezone if requested }
  if Result and Assigned(Timezone) then
  begin
    if Timezone^.Convert then
      xsdDateTimeConvertTo(Year, Month, Day, Hour, Minute, Second, Milliseconds, T, Timezone^)
    else
      Timezone^ := T;
  end;
end;

function xsdTryParseDateTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
var
  Year, Month, Day: Longword;
  Hour, Minute, Second, Milliseconds: Longword;
begin
  if xsdTryParseDateTime(Chars, Last, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone) then
    Result := TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, Milliseconds, Value)
  else
    Result := False;
end;

function xsdTryParseDecimal(Chars, Last: xmlCharPtr; out Value: Extended): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and __parseFloat(P, L, Value) and (P = L+1)
  else
    Result := Assigned(P) and __parseFloat(P, IGNORE_LAST, Value) and (P^ = #0);
end;

function xsdTryParseDouble(Chars, Last: xmlCharPtr; out Value: Double): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
  Tmp: Extended;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and __parseFloat(P, L, Tmp) and (P = L+1)
  else
    Result := Assigned(P) and __parseFloat(P, IGNORE_LAST, Tmp) and (P^ = #0);
  Value := Tmp;
end;

function xsdTryParseFloat(Chars, Last: xmlCharPtr; out Value: Single): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
  Tmp: Extended;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and __parseFloat(P, L, Tmp) and (P = L+1)
  else
    Result := Assigned(P) and __parseFloat(P, IGNORE_LAST, Tmp) and (P^ = #0);
  Value := Tmp;
end;

function xsdTryParseInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and __parseInteger(P, L, Value) and (P = L+1)
  else
    Result := Assigned(P) and __parseInteger(P, IGNORE_LAST, Value) and (P^ = #0);
end;

function xsdTryParseNonNegativeInteger(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
var
  P: PChar;
  L: PChar absolute Last;
begin
  P := PChar(Chars);
  if Assigned(Last) then
    Result := Assigned(P) and __parseNonNegativeInteger(P, L, Value) and (P = L+1)
  else
    Result := Assigned(P) and __parseNonNegativeInteger(P, IGNORE_LAST, Value) and (P^ = #0);
end;

function xsdTryParseNonPositiveInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
begin
  Result := xsdTryParseInteger(Chars, Last, Value) and (Value <= 0);
end;

function xsdTryParseNegativeInteger(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
begin
  Result := xsdTryParseInteger(Chars, Last, Value) and (Value <= -1);
end;

function xsdTryParsePositiveInteger(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
begin
  Result := xsdTryParseNonNegativeInteger(Chars, Last, Value) and (Value >= 1);
end;

function xsdTryParseByte(Chars, Last: xmlCharPtr; out Value: Shortint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdTryParseInteger(Chars, Last, Tmp) and (Tmp <= 128) and (Tmp >= -127);
  Value := Tmp;
end;

function xsdTryParseShort(Chars, Last: xmlCharPtr; out Value: Smallint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdTryParseInteger(Chars, Last, Tmp) and (Tmp <= 32767) and (Tmp >= -32768);
  Value := Tmp;
end;

function xsdTryParseInt(Chars, Last: xmlCharPtr; out Value: Longint): Boolean;
var
  Tmp: Int64;
begin
  Result := xsdTryParseInteger(Chars, Last, Tmp) and (Tmp <= 2147483647) and (Tmp >= -2147483648);
  Value := Tmp;
end;

function xsdTryParseLong(Chars, Last: xmlCharPtr; out Value: Int64): Boolean;
begin
  Result := xsdTryParseInteger(Chars, Last, Value);
end;

function xsdTryParseUnsignedByte(Chars, Last: xmlCharPtr; out Value: Byte): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdTryParseNonNegativeInteger(Chars, Last, Tmp) and (Tmp <= 255);
  Value := Tmp;
end;

function xsdTryParseUnsignedShort(Chars, Last: xmlCharPtr; out Value: Word): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdTryParseNonNegativeInteger(Chars, Last, Tmp) and (Tmp <= 65535);
  Value := Tmp;
end;

function xsdTryParseUnsignedInt(Chars, Last: xmlCharPtr; out Value: Longword): Boolean;
var
  Tmp: QWord;
begin
  Result := xsdTryParseNonNegativeInteger(Chars, Last, Tmp) and (Tmp <= 4294967295);
  Value := Tmp;
end;

function xsdTryParseUnsignedLong(Chars, Last: xmlCharPtr; out Value: QWord): Boolean;
begin
  Result := xsdTryParseNonNegativeInteger(Chars, Last, Value)
end;

function xsdTryParseEnum(Chars, Last: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;
var
  Temp: String;
  I: Integer;
begin
  Temp := '';
  Result := xsdTryParseString(Chars, Last, Temp);
  if Result then
  begin
    for I := 0 to High(enum) do
      if Temp = enum[I] then
      begin
        Value := I;
        Exit(True);
      end;
    Result := False;
  end;
end;

procedure xsdParseString(Chars, Last: xmlCharPtr; out Value: String);
begin
  if not xsdTryParseString(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseBoolean(Chars, Last: xmlCharPtr; out Value: Boolean);
begin
  if not xsdTryParseBoolean(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDate(Chars, Last: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  if not xsdTryParseDate(Chars, Last, Year, Month, Day, Timezone, BC) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDate(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryParseDate(Chars, Last, Value, Timezone) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseTime(Chars, Last: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone);
begin
  if not xsdTryParseTime(Chars, Last, Hour, Minute, Second, Milliseconds, Timezone) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryParseTime(Chars, Last, Value, Timezone) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDateTime(Chars, Last: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  if not xsdTryParseDateTime(Chars, Last, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDateTime(Chars, Last: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  if not xsdTryParseDateTime(Chars, Last, Value, Timezone) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDecimal(Chars, Last: xmlCharPtr; out Value: Extended);
begin
  if not xsdTryParseDecimal(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseDouble(Chars, Last: xmlCharPtr; out Value: Double);
begin
  if not xsdTryParseDouble(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseFloat(Chars, Last: xmlCharPtr; out Value: Single);
begin
  if not xsdTryParseFloat(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseInteger(Chars, Last: xmlCharPtr; out Value: Int64);
begin
  if not xsdTryParseInteger(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseNonNegativeInteger(Chars, Last: xmlCharPtr; out Value: QWord);
begin
  if not xsdTryParseNonNegativeInteger(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseNonPositiveInteger(Chars, Last: xmlCharPtr; out Value: Int64);
begin
  if not xsdTryParseNonPositiveInteger(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseNegativeInteger(Chars, Last: xmlCharPtr; out Value: Int64);
begin
  if not xsdTryParseNegativeInteger(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParsePositiveInteger(Chars, Last: xmlCharPtr; out Value: QWord);
begin
  if not xsdTryParsePositiveInteger(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseByte(Chars, Last: xmlCharPtr; out Value: Shortint);
begin
  if not xsdTryParseByte(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseShort(Chars, Last: xmlCharPtr; out Value: Smallint);
begin
  if not xsdTryParseShort(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseInt(Chars, Last: xmlCharPtr; out Value: Longint);
begin
  if not xsdTryParseInt(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseLong(Chars, Last: xmlCharPtr; out Value: Int64);
begin
  if not xsdTryParseLong(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseUnsignedByte(Chars, Last: xmlCharPtr; out Value: Byte);
begin
  if not xsdTryParseUnsignedByte(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseUnsignedShort(Chars, Last: xmlCharPtr; out Value: Word);
begin
  if not xsdTryParseUnsignedShort(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseUnsignedInt(Chars, Last: xmlCharPtr; out Value: Longword);
begin
  if not xsdTryParseUnsignedInt(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseUnsignedLong(Chars, Last: xmlCharPtr; out Value: QWord);
begin
  if not xsdTryParseUnsignedLong(Chars, Last, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
end;

procedure xsdParseEnum(Chars, Last: xmlCharPtr; enum: array of String; out Value: Integer);
begin
  if not xsdTryParseEnum(Chars, Last, enum, Value) then
    raise XSDException.CreateFmt(ParserError, [__strpas(Chars,Last)]);
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

function xsdNewChildEnum(parent: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; enum: array of string; Value: Integer): xmlNodePtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatEnum(enum, Value);
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

function xsdNewPropEnum(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr; enum: array of string; Value: Integer): xmlAttrPtr;
var
  Tmp: String;
begin
  Tmp := xsdFormatEnum(enum, Value);
  Result := xmlNewNsProp(node, ns, name, BAD_CAST(Tmp));
end;

function xsdTestNodeNs(node: xmlNodePtr; nameSpace: xmlCharPtr): Boolean;
begin
  Result :=
     (nameSpace = NS_IGNORE) or
    ((nameSpace = NS_EXCLUDE) and (node^.ns = nil)) or
    ((nameSpace <> NS_EXCLUDE) and (nameSpace <> NS_IGNORE) and (node^.ns <> nil) and (xmlStrEqual(nameSpace, node^.ns^.href) <> 0));
end;

function xsdTestNode(node: xmlNodePtr; name, nameSpace: xmlCharPtr): Boolean;
begin
  Result := (node <> nil) and (xmlStrEqual(name, node^.name) <> 0) and xsdTestNodeNs(node, nameSpace);
end;

function xsdTryGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: integer): xmlNodePtr;
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

function xsdTryGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out child: xmlNodePtr; Index: Integer): Boolean;
begin
  child := xsdTryGetChild(node, name, nameSpace, Index);
  Result := Assigned(child);
end;

function xsdTryGetChildChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer): xmlCharPtr;
begin
  Result := xmlNodeGetContent(xsdTryGetChild(node, name, nameSpace, Index));
end;

function xsdTryGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String; Index: Integer): Boolean;
begin
  Result := xsdTryParseString(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean; Index: Integer): Boolean;
begin
  Result := xsdTryParseBoolean(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer): Boolean;
begin
  Result := xsdTryParseDate(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Year, Month, Day, Timezone, BC);
end;

function xsdTryGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
begin
  Result := xsdTryParseDate(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; Index: Integer): Boolean;
begin
  Result := xsdTryParseTime(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

function xsdTryGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
begin
  Result := xsdTryParseTime(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

function xsdTryGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

function xsdTryGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended; Index: Integer): Boolean;
begin
  Result := xsdTryParseDecimal(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double; Index: Integer): Boolean;
begin
  Result := xsdTryParseDouble(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single; Index: Integer): Boolean;
begin
  Result := xsdTryParseFloat(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint; Index: Integer): Boolean;
begin
  Result := xsdTryParseByte(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint; Index: Integer): Boolean;
begin
  Result := xsdTryParseShort(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint; Index: Integer): Boolean;
begin
  Result := xsdTryParseInt(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64; Index: Integer): Boolean;
begin
  Result := xsdTryParseLong(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte; Index: Integer): Boolean;
begin
  Result := xsdTryParseUnsignedByte(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word; Index: Integer): Boolean;
begin
  Result := xsdTryParseUnsignedShort(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword; Index: Integer): Boolean;
begin
  Result := xsdTryParseUnsignedInt(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord; Index: Integer): Boolean;
begin
  Result := xsdTryParseUnsignedLong(xsdTryGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

function xsdTryGetChildEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer; Index: Integer): Boolean;
begin
  Result := xsdTryParseEnum(xsdTryGetChildChars(node, name, nameSpace, Index), nil, enum, Value);
end;

function xsdGetChild(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer): xmlNodePtr;
begin
  Result := xsdTryGetChild(node, name, nameSpace, Index);
  if not Assigned(Result) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

function xsdGetChildChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr; Index: Integer): xmlCharPtr;
begin
  Result := xmlNodeGetContent(xsdGetChild(node, name, nameSpace, Index));
end;

procedure xsdGetChildString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String; Index: Integer);
begin
  xsdParseString(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean; Index: Integer);
begin
  xsdParseBoolean(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer);
begin
  xsdParseDate(xsdGetChildChars(node, name, nameSpace, Index), nil, Year, Month, Day, Timezone, BC);
end;

procedure xsdGetChildDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  xsdParseDate(xsdGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; Index: Integer);
begin
  xsdParseTime(xsdGetChildChars(node, name, nameSpace, Index), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

procedure xsdGetChildTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  xsdParseTime(xsdGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean; Index: Integer);
begin
  xsdParseDateTime(xsdGetChildChars(node, name, nameSpace, Index), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

procedure xsdGetChildDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone; Index: Integer);
begin
  xsdParseDateTime(xsdGetChildChars(node, name, nameSpace, Index), nil, Value, Timezone);
end;

procedure xsdGetChildDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended; Index: Integer);
begin
  xsdParseDecimal(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double; Index: Integer);
begin
  xsdParseDouble(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single; Index: Integer);
begin
  xsdParseFloat(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint; Index: Integer);
begin
  xsdParseByte(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint; Index: Integer);
begin
  xsdParseShort(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint; Index: Integer);
begin
  xsdParseInt(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64; Index: Integer);
begin
  xsdParseLong(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte; Index: Integer);
begin
  xsdParseUnsignedByte(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word; Index: Integer);
begin
  xsdParseUnsignedShort(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword; Index: Integer);
begin
  xsdParseUnsignedInt(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord; Index: Integer);
begin
  xsdParseUnsignedLong(xsdGetChildChars(node, name, nameSpace, Index), nil, Value);
end;

procedure xsdGetChildEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer; Index: Integer);
begin
  xsdParseEnum(xsdGetChildChars(node, name, nameSpace, Index), nil, enum, Value);
end;

function xsdTryNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlNodePtr;
begin
  while Assigned(node) and (node^._type = XML_TEXT_NODE) do
    node := node^.next;

  if xsdTestNode(node, name, nameSpace) then
  begin
    Result := node;
    node := node^.next;
  end else
    Result := nil;
end;

function xsdTryNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out last: xmlNodePtr): Boolean;
begin
  last := xsdTryNext(node, name, nameSpace);
  Result := Assigned(last);
end;

function xsdTryNextChars(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
begin
  Result := xmlNodeGetContent(xsdTryNext(node, name, nameSpace));
end;

function xsdTryNextString(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String): Boolean;
begin
  Result := xsdTryParseString(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextBoolean(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean): Boolean;
begin
  Result := xsdTryParseBoolean(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
begin
  Result := xsdTryParseDate(xsdTryNextChars(node, name, nameSpace), nil, Year, Month, Day, Timezone, BC);
end;

function xsdTryNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseDate(xsdTryNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseTime(xsdTryNextChars(node, name, nameSpace), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

function xsdTryNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseTime(xsdTryNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryNextChars(node, name, nameSpace), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

function xsdTryNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryNextDecimal(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended): Boolean;
begin
  Result := xsdTryParseDecimal(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextDouble(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double): Boolean;
begin
  Result := xsdTryParseDouble(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextFloat(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single): Boolean;
begin
  Result := xsdTryParseFloat(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint): Boolean;
begin
  Result := xsdTryParseByte(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint): Boolean;
begin
  Result := xsdTryParseShort(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint): Boolean;
begin
  Result := xsdTryParseInt(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64): Boolean;
begin
  Result := xsdTryParseLong(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextUnsignedByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte): Boolean;
begin
  Result := xsdTryParseUnsignedByte(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextUnsignedShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word): Boolean;
begin
  Result := xsdTryParseUnsignedShort(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextUnsignedInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword): Boolean;
begin
  Result := xsdTryParseUnsignedInt(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextUnsignedLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord): Boolean;
begin
  Result := xsdTryParseUnsignedLong(xsdTryNextChars(node, name, nameSpace), nil, Value);
end;

function xsdTryNextEnum(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;
begin
  Result := xsdTryParseEnum(xsdTryNextChars(node, name, nameSpace), nil, enum, Value);
end;

function xsdNext(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlNodePtr;
begin
  Result := xsdTryNext(node, name, nameSpace);
  if not Assigned(Result) then
    raise XSDException.CreateNode(ChildNotFound, name, nameSpace);
end;

function xsdNextChars(var node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
begin
  Result := xmlNodeGetContent(xsdNext(node, name, nameSpace));
end;

procedure xsdNextString(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String);
begin
  xsdParseString(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextBoolean(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean);
begin
  xsdParseBoolean(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  xsdParseDate(xsdNextChars(node, name, nameSpace), nil, Year, Month, Day, Timezone, BC);
end;

procedure xsdNextDate(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseDate(xsdNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone);
begin
  xsdParseTime(xsdNextChars(node, name, nameSpace), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

procedure xsdNextTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseTime(xsdNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  xsdParseDateTime(xsdNextChars(node, name, nameSpace), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

procedure xsdNextDateTime(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseDateTime(xsdNextChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdNextDecimal(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended);
begin
  xsdParseDecimal(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextDouble(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double);
begin
  xsdParseDouble(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextFloat(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single);
begin
  xsdParseFloat(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint);
begin
  xsdParseByte(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint);
begin
  xsdParseShort(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint);
begin
  xsdParseInt(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64);
begin
  xsdParseLong(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextUnsignedByte(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte);
begin
  xsdParseUnsignedByte(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextUnsignedShort(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word);
begin
  xsdParseUnsignedShort(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextUnsignedInt(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword);
begin
  xsdParseUnsignedInt(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextUnsignedLong(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord);
begin
  xsdParseUnsignedLong(xsdNextChars(node, name, nameSpace), nil, Value);
end;

procedure xsdNextEnum(var node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer);
begin
  xsdParseEnum(xsdNextChars(node, name, nameSpace), nil, enum, Value);
end;

function xsdTestPropNs(attr: xmlAttrPtr; nameSpace: xmlCharPtr): Boolean;
begin
  Result :=
     (nameSpace = NS_IGNORE) or
    ((nameSpace = NS_EXCLUDE) and (attr^.ns = nil)) or
    ((nameSpace <> NS_EXCLUDE) and (nameSpace <> NS_IGNORE) and (attr^.ns <> nil) and (xmlStrEqual(nameSpace, attr^.ns^.href) <> 0))
end;

function xsdTestProp(attr: xmlAttrPtr; name, nameSpace: xmlCharPtr): Boolean;
begin
  Result := (attr <> nil) and (xmlStrEqual(name, attr^.name) <> 0) and xsdTestPropNs(attr, nameSpace);
end;

function xsdTryGetProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
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
end;

function xsdTryGetPropChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
var
  Prop: xmlAttrPtr;
begin
  Prop := xsdTryGetProp(node, name, nameSpace);
  if Assigned(Prop) then
    Result := xmlNodeGetContent(Prop^.children)
  else
    Result := nil;
end;

function xsdTryGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String): Boolean;
begin
  Result := xsdTryParseString(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean): Boolean;
begin
   Result := xsdTryParseBoolean(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
begin
  Result := xsdTryParseDate(xsdTryGetPropChars(node, name, nameSpace), nil, Year, Month, Day, Timezone, BC);
end;

function xsdTryGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseDate(xsdTryGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseTime(xsdTryGetPropChars(node, name, nameSpace), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

function xsdTryGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseTime(xsdTryGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryGetPropChars(node, name, nameSpace), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

function xsdTryGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone): Boolean;
begin
  Result := xsdTryParseDateTime(xsdTryGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

function xsdTryGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended): Boolean;
begin
  Result := xsdTryParseDecimal(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double): Boolean;
begin
  Result := xsdTryParseDouble(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single): Boolean;
begin
  Result := xsdTryParseFloat(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint): Boolean;
begin
  Result := xsdTryParseByte(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint): Boolean;
begin
  Result := xsdTryParseShort(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint): Boolean;
begin
  Result := xsdTryParseInt(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64): Boolean;
begin
  Result := xsdTryParseLong(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte): Boolean;
begin
  Result := xsdTryParseUnsignedByte(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word): Boolean;
begin
  Result := xsdTryParseUnsignedShort(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword): Boolean;
begin
  Result := xsdTryParseUnsignedInt(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord): Boolean;
begin
  Result := xsdTryParseUnsignedLong(xsdTryGetPropChars(node, name, nameSpace), nil, Value);
end;

function xsdTryGetPropEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer): Boolean;
begin
  Result := xsdTryParseEnum(xsdTryGetPropChars(node, name, nameSpace), nil, enum, Value);
end;

function xsdGetProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr;
begin
  Result := xsdTryGetProp(node, name, nameSpace);
  if not Assigned(Result) then
    raise XSDException.CreateNode(PropNotFound, name, nameSpace);
end;

function xsdGetPropChars(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr;
begin
  Result := xmlNodeGetContent(xsdGetProp(node, name, nameSpace)^.children);
end;

procedure xsdGetPropString(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: String);
begin
  xsdParseString(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropBoolean(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Boolean);
begin
   xsdParseBoolean(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  xsdParseDate(xsdGetPropChars(node, name, nameSpace), nil, Year, Month, Day, Timezone, BC);
end;

procedure xsdGetPropDate(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseDate(xsdGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone);
begin
  xsdParseTime(xsdGetPropChars(node, name, nameSpace), nil, Hour, Minute, Second, Milliseconds, Timezone);
end;

procedure xsdGetPropTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseTime(xsdGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Year, Month, Day, Hour, Minute, Second, Milliseconds: Longword; Timezone: PTimezone; BC: PBoolean);
begin
  xsdParseDateTime(xsdGetPropChars(node, name, nameSpace), nil, Year, Month, Day, Hour, Minute, Second, Milliseconds, Timezone, BC);
end;

procedure xsdGetPropDateTime(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: TDateTime; Timezone: PTimezone);
begin
  xsdParseDateTime(xsdGetPropChars(node, name, nameSpace), nil, Value, Timezone);
end;

procedure xsdGetPropDecimal(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Extended);
begin
  xsdParseDecimal(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropDouble(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Double);
begin
  xsdParseDouble(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropFloat(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Single);
begin
  xsdParseFloat(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Shortint);
begin
  xsdParseByte(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Smallint);
begin
  xsdParseShort(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longint);
begin
  xsdParseInt(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Int64);
begin
  xsdParseLong(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropUnsignedByte(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Byte);
begin
  xsdParseUnsignedByte(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropUnsignedShort(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Word);
begin
  xsdParseUnsignedShort(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropUnsignedInt(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: Longword);
begin
  xsdParseUnsignedInt(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropUnsignedLong(node: xmlNodePtr; name, nameSpace: xmlCharPtr; out Value: QWord);
begin
  xsdParseUnsignedLong(xsdGetPropChars(node, name, nameSpace), nil, Value);
end;

procedure xsdGetPropEnum(node: xmlNodePtr; name, nameSpace: xmlCharPtr; enum: array of String; out Value: Integer);
begin
  xsdParseEnum(xsdGetPropChars(node, name, nameSpace), nil, enum, Value);
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
