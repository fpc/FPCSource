{ Tests System.UpCase/LowerCase and SysUtils.UpperCase/LowerCase for AnsiString: results, sharing and code page }
program tupcaseansi;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TCaseFunc = function(const aSource: AnsiString): AnsiString;

var
  gFailed: Boolean = False;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    gFailed:=True;
    end;
end;


// Converts aSource character by character with System.UpCase(AnsiChar)
function RefUpper(const aSource: AnsiString): AnsiString;

var
  i: Integer;

begin
  Result:='';
  for i:=1 to Length(aSource) do
    Result:=Result+UpCase(aSource[i]);
end;


// Converts aSource character by character with System.LowerCase(AnsiChar)
function RefLower(const aSource: AnsiString): AnsiString;

var
  i: Integer;

begin
  Result:='';
  for i:=1 to Length(aSource) do
    Result:=Result+LowerCase(aSource[i]);
end;


// Calls System.UpCase
function SysUpper(const aSource: AnsiString): AnsiString;

begin
  Result:=System.UpCase(aSource);
end;


// Calls System.LowerCase
function SysLower(const aSource: AnsiString): AnsiString;

begin
  Result:=System.LowerCase(aSource);
end;


// Calls SysUtils.UpperCase
function SUUpper(const aSource: AnsiString): AnsiString;

begin
  Result:=SysUtils.UpperCase(aSource);
end;


// Calls SysUtils.LowerCase
function SULower(const aSource: AnsiString): AnsiString;

begin
  Result:=SysUtils.LowerCase(aSource);
end;


// Checks aFunc against aRef for one input
procedure CheckOne(const aName: string; aFunc, aRef: TCaseFunc; const aSource, aExpected: AnsiString);

var
  lSource, lCopy, lResult: AnsiString;

begin
  lSource:=aSource;
  UniqueString(lSource);
  lCopy:=lSource;
  UniqueString(lCopy);
  lResult:=aFunc(lSource);
  Check(lResult=aExpected,aName+'('''+aSource+''') = '''+lResult+''', expected '''+aExpected+'''');
  Check(lResult=aRef(lSource),aName+'('''+aSource+''') matches the character conversion');
  Check(lSource=lCopy,aName+'('''+aSource+''') leaves the source unchanged');
  if lResult=lSource then
    Check(Pointer(lResult)=Pointer(lSource),aName+'('''+aSource+''') shares the source when nothing changes')
  else
    Check(Pointer(lResult)<>Pointer(lSource),aName+'('''+aSource+''') returns a new string');
end;


// Runs the shared checks for one upper/lower pair
procedure CheckPair(const aName: string; aUpper, aLower: TCaseFunc);

var
  lAll, lS, lR: AnsiString;
  i: Integer;

begin
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'','');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'a','A');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'A','A');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'hello world','HELLO WORLD');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'HELLO WORLD','HELLO WORLD');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'Hello World 123','HELLO WORLD 123');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'aBCDEF','ABCDEF');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'ABCDEf','ABCDEF');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'@[`{','@[`{');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,'z'#0'a','Z'#0'A');
  CheckOne(aName+'.Upper',aUpper,@RefUpper,#200'x'#233,#200'X'#233);
  CheckOne(aName+'.Lower',aLower,@RefLower,'','');
  CheckOne(aName+'.Lower',aLower,@RefLower,'A','a');
  CheckOne(aName+'.Lower',aLower,@RefLower,'a','a');
  CheckOne(aName+'.Lower',aLower,@RefLower,'HELLO WORLD','hello world');
  CheckOne(aName+'.Lower',aLower,@RefLower,'hello world','hello world');
  CheckOne(aName+'.Lower',aLower,@RefLower,'Hello World 123','hello world 123');
  CheckOne(aName+'.Lower',aLower,@RefLower,'Abcdef','abcdef');
  CheckOne(aName+'.Lower',aLower,@RefLower,'abcdeF','abcdef');
  CheckOne(aName+'.Lower',aLower,@RefLower,'@[`{','@[`{');
  CheckOne(aName+'.Lower',aLower,@RefLower,'Z'#0'A','z'#0'a');
  CheckOne(aName+'.Lower',aLower,@RefLower,#200'X'#233,#200'x'#233);

  SetLength(lAll,256);
  for i:=0 to 255 do
    lAll[i+1]:=AnsiChar(i);
  Check(aUpper(lAll)=RefUpper(lAll),aName+'.Upper of all 256 characters');
  Check(aLower(lAll)=RefLower(lAll),aName+'.Lower of all 256 characters');

  lS:=StringOfChar('a',100000);
  Check(aUpper(lS)=StringOfChar('A',100000),aName+'.Upper of a long string');
  lS[100000]:='b';
  lR:=aLower(aUpper(lS));
  Check(lR=lS,aName+'.Lower of a long string');

  lS:='mixed Case';
  lS:=aUpper(lS);
  Check(lS='MIXED CASE',aName+'.Upper assigned to its own argument: '+lS);
  lS:=aLower(lS);
  Check(lS='mixed case',aName+'.Lower assigned to its own argument: '+lS);
  Check(aUpper('literal')='LITERAL',aName+'.Upper of a constant');
  Check(aUpper('LITERAL')='LITERAL',aName+'.Upper of an unchanged constant');

  lS:='abc'#233;
  SetCodePage(RawByteString(lS),1252,False);
  lR:=aUpper(lS);
  Check(StringCodePage(lR)=1252,aName+'.Upper keeps the code page of the source, got '+IntToStr(StringCodePage(lR)));
  Check(lR='ABC'#233,aName+'.Upper does not convert between code pages');
  lR:=aLower(lS);
  Check(StringCodePage(lR)=1252,aName+'.Lower of an unchanged string keeps the code page, got '+IntToStr(StringCodePage(lR)));
  lS:='abc';
  SetCodePage(RawByteString(lS),CP_UTF8,False);
  Check(StringCodePage(aUpper(lS))=CP_UTF8,aName+'.Upper keeps code page UTF-8');
end;


begin
  CheckPair('System',@SysUpper,@SysLower);
  CheckPair('SysUtils',@SUUpper,@SULower);
  if gFailed then
    Halt(1);
  Writeln('ok');
end.
