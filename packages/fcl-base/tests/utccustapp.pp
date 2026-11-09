unit utcCustApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, custapp;

procedure RegisterTests;

implementation

type

  { TTestApplication }

  TTestApplication = class(TCustomApplication)
  private
    FArgs: TStringArray;
  protected
    function GetParams(Index: Integer): String; override;
    function GetParamCount: Integer; override;
  public
    // Create an application with aArgs as command-line parameters 1..n
    constructor CreateArgs(const aArgs: array of String; aShortCase, aLongCase: Boolean);
  end;


constructor TTestApplication.CreateArgs(const aArgs: array of String; aShortCase, aLongCase: Boolean);

var
  i: Integer;

begin
  inherited Create(nil);
  SetLength(FArgs,Length(aArgs));
  for i:=0 to Length(aArgs)-1 do
    FArgs[i]:=aArgs[i];
  CaseSensitiveShortOptions:=aShortCase;
  CaseSensitiveLongOptions:=aLongCase;
end;


function TTestApplication.GetParams(Index: Integer): String;

begin
  if Index=0 then
    Result:='prog'
  else
    Result:=FArgs[Index-1];
end;


function TTestApplication.GetParamCount: Integer;

begin
  Result:=Length(FArgs);
end;


// Index of option aOption for aArgs, with Longopt in aLong
function FindIn(const aArgs: array of String; aShortCase, aLongCase: Boolean;
  const aOption: String; out aLong: Boolean): Integer;

var
  lApp: TTestApplication;

begin
  lApp:=TTestApplication.CreateArgs(aArgs,aShortCase,aLongCase);
  try
    aLong:=True;
    Result:=lApp.FindOptionIndex(aOption,aLong);
  finally
    lApp.Free;
  end;
end;


function TCustApp_ShortBeforeLongMixedCase: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('-x found in "-x --foo" with case-sensitive short options',1,FindIn(['-x','--foo'],True,False,'x',lLong));
  AssertFalse('-x is a short option',lLong);
end;


function TCustApp_LongBeforeShortMixedCase: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('-x found in "--foo -x" with case-sensitive short options',2,FindIn(['--foo','-x'],True,False,'x',lLong));
  AssertFalse('-x is a short option',lLong);
end;


function TCustApp_ShortCaseSensitive: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('-X does not match x with case-sensitive short options',-1,FindIn(['-X','--foo'],True,False,'x',lLong));
  AssertEquals('-X matches x with case-insensitive short options',1,FindIn(['-X','--foo'],False,True,'x',lLong));
end;


function TCustApp_LongCaseInsensitive: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('--FOO matches foo with case-insensitive long options',2,FindIn(['-x','--FOO'],True,False,'foo',lLong));
  AssertTrue('--FOO is a long option',lLong);
  AssertEquals('--FOO does not match foo with case-sensitive long options',-1,FindIn(['-X','--FOO'],False,True,'foo',lLong));
end;


function TCustApp_LongCaseSensitiveAfterShort: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('--FOO does not match foo with case-sensitive long options, after a case-insensitive short option',-1,
    FindIn(['--FOO','-x'],False,True,'foo',lLong));
  AssertEquals('--Foo does not match foo with case-sensitive long options, after a short option',-1,
    FindIn(['--Foo','-x'],False,True,'foo',lLong));
  AssertEquals('--foo matches foo with case-sensitive long options, after a short option',1,
    FindIn(['--foo','-X'],False,True,'foo',lLong));
end;


function TCustApp_LongWithValue: TTestString;

var
  lLong: Boolean;
  lApp: TTestApplication;

begin
  Result:='';
  AssertEquals('--name=value matches name',2,FindIn(['-x','--name=value'],True,True,'name',lLong));
  AssertTrue('--name=value is a long option',lLong);
  lApp:=TTestApplication.CreateArgs(['-x','--name=value'],True,True);
  try
    AssertEquals('Value of --name=value','value',lApp.GetOptionValue('n','name'));
  finally
    lApp.Free;
  end;
end;


function TCustApp_LastMatchWins: TTestString;

var
  lLong: Boolean;
  lApp: TTestApplication;

begin
  Result:='';
  AssertEquals('The last occurrence of an option is found',3,FindIn(['-x','--foo','-x'],True,True,'x',lLong));
  lApp:=TTestApplication.CreateArgs(['-x','--foo','-x'],True,True);
  try
    AssertEquals('StartAt searches from that parameter down',1,lApp.FindOptionIndex('x',lLong,2));
  finally
    lApp.Free;
  end;
end;


function TCustApp_LongoptWhenNotFound: TTestString;

var
  lLong: Boolean;

begin
  Result:='';
  AssertEquals('No match',-1,FindIn(['--foo','--bar'],True,True,'baz',lLong));
  AssertFalse('Longopt is False when nothing matches',lLong);
  AssertEquals('No match without parameters',-1,FindIn([],True,True,'baz',lLong));
  AssertFalse('Longopt is False without parameters',lLong);
end;


procedure RegisterTests;

begin
  AddSuite('TCustomApplicationTests');
  AddTest('ShortBeforeLongMixedCase',@TCustApp_ShortBeforeLongMixedCase,'TCustomApplicationTests');
  AddTest('LongBeforeShortMixedCase',@TCustApp_LongBeforeShortMixedCase,'TCustomApplicationTests');
  AddTest('ShortCaseSensitive',@TCustApp_ShortCaseSensitive,'TCustomApplicationTests');
  AddTest('LongCaseInsensitive',@TCustApp_LongCaseInsensitive,'TCustomApplicationTests');
  AddTest('LongCaseSensitiveAfterShort',@TCustApp_LongCaseSensitiveAfterShort,'TCustomApplicationTests');
  AddTest('LongWithValue',@TCustApp_LongWithValue,'TCustomApplicationTests');
  AddTest('LastMatchWins',@TCustApp_LastMatchWins,'TCustomApplicationTests');
  AddTest('LongoptWhenNotFound',@TCustApp_LongoptWhenNotFound,'TCustomApplicationTests');
end;

end.
