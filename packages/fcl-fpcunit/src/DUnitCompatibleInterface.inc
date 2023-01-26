{%MainUnit fpcunit.pp}

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    DUnit compatibility

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF read_interface}

    class procedure Check(pValue: boolean; const pMessage: string = '');
    class procedure CheckEquals(expected, actual: extended; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: extended; delta: extended; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: AnsiString; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: unicodestring;const  msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: integer; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: boolean; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: TClass; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: int64; const msg: string = ''); overload;
    class procedure CheckEquals(expected, actual: QWord; const msg: string = ''); overload;
    class procedure CheckNotEquals(expected, actual: Ansistring; const msg: string = ''); overload;
    class procedure CheckNotEquals(expected, actual: unicodestring; const msg: string = ''); overload;
    class procedure CheckNotEquals(expected, actual: integer; const msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected, actual: boolean; const msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0;    const msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected, actual: int64; const msg: string = ''); overload; virtual;
    class procedure CheckNotEquals(expected, actual: QWord; const msg: string = ''); overload; virtual;
    class procedure CheckNull(obj: IUnknown; const msg: string = ''); overload;
    class procedure CheckNull(obj: TObject; const msg: string = ''); overload;
    class procedure CheckNotNull(obj: TObject; const msg: string = ''); overload;
    class procedure CheckNotNull(obj: IUnknown; const msg: string = ''); overload; virtual;
    class procedure CheckIs(obj :TObject; pClass: TClass; const msg: string = ''); overload;
    class procedure CheckSame(expected, actual: TObject; const msg: string = ''); overload;
    class procedure CheckTrue(condition: Boolean; const msg: string = '');
    class procedure CheckFalse(condition: Boolean; const msg: string = '');
    class procedure CheckException(AMethod: TRunMethod; AExceptionClass: ExceptClass; const msg: string = '');
    class function  EqualsErrorMessage(const expected, actual: string; const ErrorMsg: string): string;
    class function  NotEqualsErrorMessage(const expected, actual: string; const ErrorMsg: string): string;

    class function Suite: TTest;


    {
    *** TODO  ***
    procedure CheckEqualsBin(expected, actual: longword; const msg: string = ''; digits: integer=32); virtual;
    procedure CheckEqualsHex(expected, actual: longword; const msg: string = ''; digits: integer=8); virtual;
    procedure CheckNotEqualsBin(expected, actual: longword; const msg: string = ''; digits: integer=32); virtual;
    procedure CheckNotEqualsHex(expected, actual: longword; const msg: string = ''; digits: integer=8); virtual;
    procedure CheckNotNull(obj :IUnknown; const msg :string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; const msg: string = ''); overload; virtual;
    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string = '');
    procedure CheckInherits(expected, actual: TClass; const msg: string = ''); overload; virtual;
    }

{$ENDIF read_interface}


{$IFDEF read_implementation}

class procedure TAssert.Check(pValue: boolean; const pMessage: string);
begin
  AssertTrue(pMessage, pValue);
end;

class procedure TAssert.CheckEquals(expected, actual: extended; const msg: string);
begin
  CheckEquals(expected, actual, 0, msg);
end;

class procedure TAssert.CheckEquals(expected, actual: extended;
  delta: extended; const msg: string);
begin
  AssertEquals(msg, expected, actual, delta);
end;

class procedure TAssert.CheckEquals(expected, actual: Ansistring; const msg: string);

begin
  AssertEquals(msg, expected, actual);
end;
class procedure TAssert.CheckEquals(expected, actual: unicodestring; const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: integer; const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: boolean; const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: TClass; const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: QWord;
  const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckEquals(expected, actual: int64;
  const msg: string);
begin
  AssertEquals(msg, expected, actual);
end;

class procedure TAssert.CheckNotEquals(expected, actual: Ansistring; const msg: string);
begin
  if AnsiCompareStr(Expected, Actual) = 0 then
    Fail(ComparisonMsg(msg, Expected, Actual, false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected, actual: unicodestring; const msg: string ='');

begin
  if (Expected=Actual) then
    Fail(ComparisonMsg(msg, Expected, Actual, false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected, actual: integer; const msg: string);
begin
  if (expected = actual) then
    Fail(ComparisonMsg(msg, IntToStr(expected), IntToStr(actual), false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected, actual: boolean; const msg: string);
begin
  if (expected = actual) then
    Fail(ComparisonMsg(msg, BoolToStr(expected), BoolToStr(actual), false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected: extended; actual: extended;
  delta: extended; const msg: string);
begin
  if (abs(expected-actual) <= delta) then
    FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, nil)
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected, actual: QWord;
  const msg: string);
begin
  if (expected = actual) then
    Fail(ComparisonMsg(msg, IntToStr(expected), IntToStr(actual), false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNotEquals(expected, actual: int64;
  const msg: string);
begin
  if (expected = actual) then
    Fail(ComparisonMsg(msg, IntToStr(expected), IntToStr(actual), false))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckNull(obj: IUnknown; const msg: string);
begin
  AssertNullIntf(msg, obj);
end;

class procedure TAssert.CheckNull(obj: TObject; const msg: string);
begin
  AssertNull(msg, obj);
end;

class procedure TAssert.CheckNotNull(obj: TObject; const msg: string);
begin
  AssertNotNull(msg, obj);
end;

class procedure TAssert.CheckNotNull(obj: IUnknown; const msg: string);
begin
  AssertNotNullIntf(msg, obj);
end;

class procedure TAssert.CheckIs(obj: TObject; pClass: TClass; const msg: string);
begin
  Assert(pClass <> nil);
  if obj = nil then
    Fail(ComparisonMsg(msg, pClass.ClassName, 'nil'))
  else if not obj.ClassType.InheritsFrom(pClass) then
    Fail(ComparisonMsg(msg, pClass.ClassName, obj.ClassName))
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckSame(expected, actual: TObject; const msg: string);
begin
   AssertSame(msg, expected, actual);
end;

class procedure TAssert.CheckTrue(condition: Boolean; const msg: string);
begin
  if (not condition) then
    FailNotEquals(BoolToStr(true, true), BoolToStr(false, true), msg, nil)
  else
    Inc(AssertCount);
end;

class procedure TAssert.CheckFalse(condition: Boolean; const msg: string);
begin
  if (condition) then
    FailNotEquals(BoolToStr(false, true), BoolToStr(true, true), msg, nil)
  else
    Inc(AssertCount);
end;


class procedure TAssert.CheckException(AMethod: TRunMethod; AExceptionClass: ExceptClass; const msg: string = '');
begin
  AssertException(msg, AExceptionClass, AMethod);
end;

class function TAssert.EqualsErrorMessage(const expected, actual: string;
    const ErrorMsg: string): string;
begin
  if (ErrorMsg <> '') then
    Result := Format(sExpectedButWasAndMessageFmt, [ErrorMsg + ', ', expected, actual])
  else
    Result := Format(sExpectedButWasFmt, [expected, actual])
end;

class function TAssert.NotEqualsErrorMessage(const expected, actual: string;
    const ErrorMsg: string): string;
begin
  if (ErrorMsg <> '') then
    Result := Format(sExpectedButWasAndMessageFmt, [ErrorMsg, expected, actual])
  else
    Result := Format(sExpectedButWasFmt, [expected, actual]);
end;

class function TAssert.Suite: TTest;
begin
  result := TTestSuite.Create(self);
end;

{$ENDIF read_implementation}

