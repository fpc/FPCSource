{$INCLUDE sdo_global.inc}
unit test_suite_utils;

interface
uses
  SysUtils, Classes,
{$IFDEF FPC}
  fpcunit,
{$ENDIF}
{$IFNDEF FPC}
  TestFrameWork,
{$ENDIF}
  sdo, sdo_types;

const
  TestFilesPath = '..' + PathDelim + '..' + PathDelim + 'files' + PathDelim;
  XsdTestFilesPath = '..' + PathDelim + '..' + PathDelim + 'files' + PathDelim + 'xsd' + PathDelim;

type

  { TWstBaseTest }

  TWstBaseTest = class(TTestCase)
  protected
    procedure CheckEquals(expected, actual: TSDOBytes; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure CheckEqualsCurrency(expected, actual: Currency; const delta: Currency; const msg: string = ''); overload;
    procedure CheckEqualsCurrency(expected, actual: Currency; const msg: string = ''); overload;
{$IFDEF FPC}
    procedure CheckEquals(expected, actual: Int64; msg: string = ''; const AStrict : Boolean = True); overload;
    procedure CheckEquals(expected, actual: QWord; msg: string = ''; const AStrict : Boolean = True); overload;
{$ENDIF FPC}
  end;

//{$IFDEF FPC}
//  {$IF not Defined(RandomRange)}
    //function RandomRange(const AFrom, ATo : Integer) : Integer ;overload;
//  {$IFEND}
//{$ENDIF}
   function RandomRange(const AFrom, ATo : Int64) : Int64 ;overload;

   function RandomString(const AMaxLen : PtrInt) : TSDOString;
   function RandomDate() : TSDODateTime ;
   function RandomBytes(const AMaxLen : Integer) : TSDOBytes;
   function BytesToString(const AValue: TSDOBytes): TSDOString;overload;
   function BytesToString(const AValue: array of Byte): TSDOString;overload;
   function RandomDouble(const AFrom, ATo: Integer): TSDODouble;
   function RandomFloat(const AFrom, ATo: Integer): TSDOFloat;


   function sdoExpandLocalFileName(const AFileName : string) : string;

implementation
uses
  Math;

//{$IFDEF FPC}
 // {$IF not Defined(RandomRange)}
    {function RandomRange(const AFrom, ATo : Integer) : Integer ;
    var
      a : Integer;
    begin
      if ( AFrom <= ATo ) then
        a := AFrom
      else
        a := ATo;
      Result := a + Random(Abs(ATo - AFrom));
    end; }
//  {$IFEND}
//{$ENDIF}

function RandomRange(const AFrom, ATo : Int64) : Int64 ;
var
  a : Int64;
begin
  if ( AFrom <= ATo ) then
    a := AFrom
  else
    a := ATo;
  Result := a + Random(Abs(ATo - AFrom));
end;

function RandomString(const AMaxLen : PtrInt) : TSDOString;
var
  i, l, j : PtrInt;
begin
  l := RandomRange(0,AMaxLen);
  j := RandomRange(1, 3);
  SetLength(Result,l);
  for i := 1 to l do begin
    case j of
      1 : Result[i] := Char(RandomRange(Ord('a'),Ord('z')));
      2 : Result[i] := Char(RandomRange(Ord('0'),Ord('9')));
      3 : Result[i] := Char(RandomRange(Ord('A'),Ord('Z')));
    end;
    j := ( j + 1 );
    if ( j > 3 ) then
      j := 1;
  end;
end;

function BytesToString(const AValue: TSDOBytes): TSDOString;
var
  locRes : AnsiString;
begin
  if (  Length(AValue) > 0 ) then begin
    SetLength(locRes, ( 2 * Length(AValue) ) );
    BinToHex(PAnsiChar(@(AValue[0])),PAnsiChar(@(locRes[1])),Length(AValue));
    Result := locRes;
  end else begin
    Result := '';
  end;
end;

function BytesToString(const AValue: array of Byte): TSDOString;
var
  locRes : AnsiString;
begin
  if (  Length(AValue) > 0 ) then begin
    SetLength(locRes, ( 2 * Length(AValue) ) );
    BinToHex(PAnsiChar(@(AValue[0])),PAnsiChar(@(locRes[1])),Length(AValue));
    Result := locRes;
  end else begin
    Result := '';
  end;
end;

function RandomDate() : TSDODateTime ;
begin
  Result.Date := RandomRange(0,50000);
  Result.HourOffset := RandomRange(-12,12);
  Result.MinuteOffset := RandomRange(-59,59)
end;

function sdoExpandLocalFileName(const AFileName : string) : string;
begin
  Result := ExtractFilePath(ParamStr(0)) + AFileName;
end;

function RandomBytes(const AMaxLen : Integer) : TSDOBytes;
var
  i : Integer;
begin
  if ( AMaxLen > 0 ) then begin
    SetLength(Result,AMaxLen);
    for i:= 0 to Pred(AMaxLen) do
      Result[i] := RandomRange(Low(Byte),High(Byte));
  end else begin
    Result := nil;
  end;
end;

function RandomDouble(const AFrom, ATo: Integer): TSDODouble;
begin
  Result := RandomRange(AFrom,ATo)
end;

function RandomFloat(const AFrom, ATo: Integer): TSDOFloat;
begin
  Result := RandomRange(AFrom,ATo)
end;

{ TWstBaseTest }

{$IFDEF FPC}
procedure TWstBaseTest.CheckEquals(expected, actual: Int64; msg: string;
  const AStrict: Boolean);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF WST_DELPHI});
end;

procedure TWstBaseTest.CheckEquals(expected, actual: QWord; msg: string;
  const AStrict: Boolean);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF WST_DELPHI});
end;
{$ENDIF FPC}

procedure TWstBaseTest.CheckEquals(expected, actual: TSDOBytes;
  msg: string; const AStrict: Boolean
);
begin
  if ( expected = nil ) then begin
    Check(actual = nil, msg);
  end else begin
    CheckEquals(Length(expected),Length(actual),msg);
    if ( Length(expected) > 0 ) then
      Check(CompareMem(Pointer(expected), Pointer(actual),Length(expected)),msg);
  end;
end;

procedure TWstBaseTest.CheckEqualsCurrency(expected, actual: Currency; const delta: Currency; const msg: string);
begin
  if ( delta = 0 ) then begin
    if (expected <> actual) then
      FailNotEquals(CurrToStr(expected), CurrToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF});
  end else begin
    if ( Abs(expected-actual) > delta ) then
      FailNotEquals(CurrToStr(expected), CurrToStr(actual), msg{$IFDEF WST_DELPHI}, CallerAddr{$ENDIF});
  end;
end;

procedure TWstBaseTest.CheckEqualsCurrency(expected, actual: Currency;
  const msg: string);
begin
  CheckEqualsCurrency(expected,actual,0,msg);
end;

end.
