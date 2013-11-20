{$INCLUDE sdo_global.inc}
unit test_convert_helper;

interface
uses
  SysUtils, Classes
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo, sdo_types;

type

  TSDOConvertHelper_Test = class(TWstBaseTest)
  protected
    class function getObject() : TSDOConvertHelperClass;
  published
    procedure BoolToByte();
    procedure BoolToInteger();
    procedure BoolToLong();
    procedure BoolToShort();
    procedure BoolToString();

    procedure ByteToBool();
    procedure ByteToChar();
    procedure ByteToString();

    procedure BytesToString();

    procedure CharToBool();
    procedure CharToByte();
    procedure CharToInteger();
    procedure CharToLong();
    procedure CharToShort();

    procedure IntegerToBool();
    procedure IntegerToChar();
    procedure IntegerToString();

    procedure LongToBool();
    procedure LongToChar();
    procedure LongToString();

    procedure ShortToBool();
    procedure ShortToChar();
    procedure ShortToString();

    procedure StringToBool();
    procedure StringToByte();
    procedure StringToBytes();
    procedure StringToDate();
    procedure StringToInteger();
    procedure StringToLong();
    procedure StringToShort();

    procedure FloatToString();
    procedure FloatToBool();

    procedure DateToString();
  end;

implementation

{ TSDOConvertHelper_Test }

procedure TSDOConvertHelper_Test.BoolToByte();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(1, locObj.BoolToByte(True), 'true');
  CheckEquals(0, locObj.BoolToByte(False), 'false');
end;

procedure TSDOConvertHelper_Test.BoolToInteger();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(1, locObj.BoolToInteger(True), 'true');
  CheckEquals(0, locObj.BoolToInteger(False), 'false');
end;

procedure TSDOConvertHelper_Test.BoolToLong();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(1, locObj.BoolToLong(True), 'true');
  CheckEquals(0, locObj.BoolToLong(False), 'false');
end;

procedure TSDOConvertHelper_Test.BoolToShort();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(1, locObj.BoolToShort(True), 'true');
  CheckEquals(0, locObj.BoolToShort(False), 'false');
end;

procedure TSDOConvertHelper_Test.BoolToString();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals('1', locObj.BoolToString(True), 'true');
  CheckEquals('0', locObj.BoolToString(False), 'false');
end;

procedure TSDOConvertHelper_Test.ByteToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.ByteToBool(123));
  CheckEquals(False, locObj.ByteToBool(0));
  CheckEquals(True, locObj.ByteToBool(1));
  CheckEquals(True, locObj.ByteToBool(2));
  CheckEquals(True, locObj.ByteToBool(34));
end;

procedure TSDOConvertHelper_Test.ByteToChar();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(TSDOChar(#123), locObj.ByteToChar(123));
  CheckEquals(TSDOChar(#0), locObj.ByteToChar(0));
  CheckEquals(TSDOChar(#1), locObj.ByteToChar(1));
  CheckEquals(TSDOChar(#56), locObj.ByteToChar(56));
end;

procedure TSDOConvertHelper_Test.ByteToString();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals('123', locObj.ByteToString(123));
  CheckEquals('0', locObj.ByteToString(0));
  CheckEquals('1', locObj.ByteToString(1));
  CheckEquals('56', locObj.ByteToString(56));
end;

procedure TSDOConvertHelper_Test.DateToString();
const
  sDATE_1 = '1976-10-12T23:34:56Z';
  sDATE_2 = '0987-06-12T20:34:56Z';
var
  d : TSDODate;
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(1976,10,12) + EncodeTime(23,34,56,0);
  CheckEquals(sDATE_1, locObj.DateToString(d));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(987,06,12) - EncodeTime(20,34,56,0);
  CheckEquals(sDATE_2, locObj.DateToString(d));

  FillChar(d,SizeOf(d),#0);
  d.Date := EncodeDate(2002,10,10) + EncodeTime(12,0,0,0);
  d.HourOffset := 5;
  CheckEquals('2002-10-10T07:00:00Z', locObj.DateToString(d));
end;

procedure TSDOConvertHelper_Test.FloatToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.FloatToBool(1));
  CheckEquals(False, locObj.FloatToBool(0));

  CheckEquals(True, locObj.FloatToBool(123));
  CheckEquals(True, locObj.FloatToBool(-43));
  CheckEquals(True, locObj.FloatToBool(123456789));
end;

procedure TSDOConvertHelper_Test.FloatToString;
begin

end;

class function TSDOConvertHelper_Test.getObject() : TSDOConvertHelperClass;
begin
  Result := TSDOConvertHelper;
end;

procedure TSDOConvertHelper_Test.CharToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.CharToBool('8'));
  CheckEquals(False, locObj.CharToBool(#0));
  CheckEquals(True, locObj.CharToBool(#1));
  CheckEquals(True, locObj.CharToBool('T'));
end;

procedure TSDOConvertHelper_Test.CharToByte();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(Ord(TSDOChar('8')), locObj.CharToByte('8'));
  CheckEquals(0, locObj.CharToByte(#0));
  CheckEquals(1, locObj.CharToByte(#1));
  CheckEquals(Ord(TSDOChar('T')), locObj.CharToByte('T'));
end;

procedure TSDOConvertHelper_Test.CharToInteger();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(Ord(TSDOChar('8')), locObj.CharToInteger('8'));
  CheckEquals(0, locObj.CharToInteger(#0));
  CheckEquals(1, locObj.CharToInteger(#1));
  CheckEquals(Ord(TSDOChar('T')), locObj.CharToInteger('T'));
end;

procedure TSDOConvertHelper_Test.CharToLong();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(Ord(TSDOChar('8')), locObj.CharToLong('8'));
  CheckEquals(0, locObj.CharToLong(#0));
  CheckEquals(1, locObj.CharToLong(#1));
  CheckEquals(Ord(TSDOChar('T')), locObj.CharToLong('T'));
end;

procedure TSDOConvertHelper_Test.CharToShort();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(Ord(TSDOChar('8')), locObj.CharToShort('8'));
  CheckEquals(0, locObj.CharToShort(#0));
  CheckEquals(1, locObj.CharToShort(#1));
  CheckEquals(Ord(TSDOChar('T')), locObj.CharToShort('T'));
end;

procedure TSDOConvertHelper_Test.IntegerToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.IntegerToBool(-789));
  CheckEquals(False, locObj.IntegerToBool(0));
  CheckEquals(True, locObj.IntegerToBool(1));
  CheckEquals(True, locObj.IntegerToBool(-1));
  CheckEquals(True, locObj.IntegerToBool(123));
end;

procedure TSDOConvertHelper_Test.IntegerToChar();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(TSDOChar(0), locObj.IntegerToChar(0));
  CheckEquals(TSDOChar(12), locObj.IntegerToChar(12));
  CheckEquals(TSDOChar(123), locObj.IntegerToChar(123));
  CheckEquals(TSDOChar(TSDOInteger(-52)), locObj.IntegerToChar(-52));
  CheckEquals(TSDOChar(12345), locObj.IntegerToChar(12345));
end;

procedure TSDOConvertHelper_Test.IntegerToString();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals('-123456', locObj.IntegerToString(-123456));
  CheckEquals('0', locObj.IntegerToString(0));
  CheckEquals('1', locObj.IntegerToString(1));
  CheckEquals('123456', locObj.IntegerToString(123456));
end;

procedure TSDOConvertHelper_Test.LongToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.LongToBool(-789));
  CheckEquals(False, locObj.LongToBool(0));
  CheckEquals(True, locObj.LongToBool(1));
  CheckEquals(True, locObj.LongToBool(-1));
  CheckEquals(True, locObj.LongToBool(123));
end;

procedure TSDOConvertHelper_Test.LongToChar();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(TSDOChar(0), locObj.LongToChar(0));
  CheckEquals(TSDOChar(12), locObj.LongToChar(12));
  CheckEquals(TSDOChar(123), locObj.LongToChar(123));
  CheckEquals(TSDOChar(TSDOLong(-52)), locObj.LongToChar(-52));
  CheckEquals(TSDOChar(TSDOLong(123456)), locObj.LongToChar(123456));
end;

procedure TSDOConvertHelper_Test.LongToString();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals('-123456', locObj.LongToString(-123456));
  CheckEquals('0', locObj.LongToString(0));
  CheckEquals('1', locObj.LongToString(1));
  CheckEquals('123456', locObj.LongToString(123456));
end;

procedure TSDOConvertHelper_Test.ShortToBool();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(True, locObj.ShortToBool(-789));
  CheckEquals(False, locObj.ShortToBool(0));
  CheckEquals(True, locObj.ShortToBool(1));
  CheckEquals(True, locObj.ShortToBool(-1));
  CheckEquals(True, locObj.ShortToBool(123));
end;

procedure TSDOConvertHelper_Test.ShortToChar();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals(TSDOChar(0), locObj.ShortToChar(0));
  CheckEquals(TSDOChar(12), locObj.ShortToChar(12));
  CheckEquals(TSDOChar(123), locObj.ShortToChar(123));
  CheckEquals(TSDOChar(TSDOShort(-52)), locObj.ShortToChar(-52));
  CheckEquals(TSDOChar(TSDOShort(12345)), locObj.ShortToChar(12345));
end;

procedure TSDOConvertHelper_Test.ShortToString();
var
  locObj : TSDOConvertHelperClass;
begin
  locObj := getObject();
  CheckEquals('-1234', locObj.ShortToString(-1234));
  CheckEquals('0', locObj.ShortToString(0));
  CheckEquals('1', locObj.ShortToString(1));
  CheckEquals('1234', locObj.ShortToString(1234));
end;

procedure TSDOConvertHelper_Test.StringToBool();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToBool(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

begin
  locObj := getObject();
  CheckEquals(False, locObj.StringToBool('0'));
  CheckEquals(False, locObj.StringToBool('false'));
  CheckEquals(False, locObj.StringToBool('FaLsE'));
  CheckEquals(True, locObj.StringToBool('1'));
  CheckEquals(True, locObj.StringToBool('True'));
  check_invalid('');
  check_invalid('xxxxx');
  check_invalid('falsexxx');
  check_invalid('truessss');
end;

procedure TSDOConvertHelper_Test.StringToByte();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToByte(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

begin
  locObj := getObject();
  CheckEquals(123, locObj.StringToByte('123'));
  CheckEquals(0, locObj.StringToByte('0'));
  CheckEquals(1, locObj.StringToByte('1'));
  CheckEquals(45, locObj.StringToByte('45'));
  check_invalid('');
  check_invalid('xxxxx');
  check_invalid('1235dd');
  check_invalid('a2522');
end;

procedure TSDOConvertHelper_Test.StringToDate();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToDate(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

var
  s : string;
  d : TSDODate;
  y,m,dy : Word;
  hh,mn,ss, ssss : Word;
begin
  locObj := getObject();
  //'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?
  s := '1976-10-12T23:34:56Z';
  d := locObj.StringToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(0,d.HourOffset,'HourOffset');
    CheckEquals(0,d.MinuteOffset,'MinuteOffset');

  s := '1976-10-12T23:34:56+12:34';
  d := locObj.StringToDate(s);
    DecodeDate(d.Date,y,m,dy);
      CheckEquals(y,1976,'Year');
      CheckEquals(m,10,'Month');
      CheckEquals(dy,12,'Day');
    DecodeTime(d.Date,hh,mn,ss,ssss);
      CheckEquals(hh,23,'Hour');
      CheckEquals(mn,34,'Minute');
      CheckEquals(ss,56,'Second');
    CheckEquals(12,d.HourOffset,'HourOffset');
    CheckEquals(34,d.MinuteOffset,'MinuteOffset');

  check_invalid('azerty');
  check_invalid('123');  
end;

procedure TSDOConvertHelper_Test.StringToInteger();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToInteger(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

begin
  locObj := getObject();
  CheckEquals(-123456, locObj.StringToInteger('-123456'));
  CheckEquals(0, locObj.StringToInteger('0'));
  CheckEquals(1, locObj.StringToInteger('1'));
  CheckEquals(123456, locObj.StringToInteger('123456'));
  check_invalid('');
  check_invalid('xxxxx');
  check_invalid('1235dd');
  check_invalid('a2522');
end;

procedure TSDOConvertHelper_Test.StringToLong();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToLong(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

begin
  locObj := getObject();
  CheckEquals(-123456, locObj.StringToLong('-123456'));
  CheckEquals(0, locObj.StringToLong('0'));
  CheckEquals(1, locObj.StringToLong('1'));
  Check(Int64(High(LongWord)) + 10 = locObj.StringToLong(IntToStr(Int64(High(LongWord)) + 10)));
  check_invalid('');
  check_invalid('xxxxx');
  check_invalid('1235dd');
  check_invalid('a2522');
end;

procedure TSDOConvertHelper_Test.StringToShort();
var
  locObj : TSDOConvertHelperClass;

  procedure check_invalid(const AString : TSDOString);
  var
    Ok : boolean;
  begin
    ok := False;
    try
      locObj.StringToShort(AString);
    except
      on e : ESDOInvalidConversionException do
        ok := True;
    end;
    Check(ok,Format('This conversion should fail : "%s".',[AString]));
  end;

begin
  locObj := getObject();
  CheckEquals(-1234, locObj.StringToShort('-1234'));
  CheckEquals(0, locObj.StringToShort('0'));
  CheckEquals(1, locObj.StringToShort('1'));
  CheckEquals(1234, locObj.StringToShort('1234'));
  check_invalid('');
  check_invalid('xxxxx');
  check_invalid('1235dd');
  check_invalid('a2522');
end;

procedure TSDOConvertHelper_Test.BytesToString();
const
  LEN = 1000;
var
  locObj : TSDOConvertHelperClass;
  buff : TSDOBytes;
  strBuff : TSDOString;
  ansiStrBuff : AnsiString;
  i : Integer;
begin
  locObj := getObject();

  buff := nil;
  strBuff := locObj.BytesToString(buff);
  CheckEquals('', strBuff);

  SetLength(buff,LEN);
  for i := 0 to Pred(LEN) do
    buff[i] := ( i mod High(TSDOByte) );

  SetLength(ansiStrBuff,LEN * 2);
  BinToHex(PAnsiChar(@buff[Low(buff)]),PAnsiChar(@ansiStrBuff[1]),LEN);
  strBuff := ansiStrBuff;
  CheckEquals(strBuff,locObj.BytesToString(buff));
end;

procedure TSDOConvertHelper_Test.StringToBytes();
const
  LEN = 1000;
var
  locObj : TSDOConvertHelperClass;
  buff, calcBuff : TSDOBytes;
  strBuff : TSDOString;
  ansiStrBuff : AnsiString;
  i : Integer;
begin
  locObj := getObject();
  SetLength(buff,LEN);
  for i := 0 to Pred(LEN) do
    buff[i] := ( i mod High(TSDOByte) );
  SetLength(ansiStrBuff,LEN * 2);
  BinToHex(PAnsiChar(buff),PAnsiChar(ansiStrBuff),LEN);
  strBuff := ansiStrBuff;

  CheckEquals(PtrUInt(nil), PtrUInt(locObj.StringToBytes('')));
  calcBuff := locObj.StringToBytes(strBuff);
  Check(CompareMem(Pointer(buff),Pointer(calcBuff),LEN));
end;

initialization
  RegisterTest('Helpers',TSDOConvertHelper_Test.Suite);

end.
