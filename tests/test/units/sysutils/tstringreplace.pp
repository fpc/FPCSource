{ Tests StringReplace, UnicodeStringReplace and WideStringReplace, compared to a reference implementation }
program tstringreplace;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cwstring,{$endif} SysUtils;

type
{$if sizeof(SizeInt)<sizeof(Integer)}
  TReplaceCount = Integer;
{$else}
  TReplaceCount = SizeInt;
{$endif}

var
  gFailed: Integer = 0;
  gSeed: Cardinal = 12345;


// Reports aMessage as a failure when aCondition is false
procedure Check(aCondition: Boolean; const aMessage: string);

begin
  if not aCondition then
    begin
    Writeln('failed: ',aMessage);
    Inc(gFailed);
    end;
end;


// Returns aSource with aOld replaced by aNew, built with Copy and Pos
function RefReplace(const aSource, aOld, aNew: UnicodeString; aFlags: TReplaceFlags; out aCount: SizeInt): UnicodeString;

var
  lSrch, lOld: UnicodeString;
  lStart, lPos: SizeInt;

begin
  Result:='';
  aCount:=0;
  if aOld='' then
    Exit(aSource);
  if rfIgnoreCase in aFlags then
    begin
    lSrch:=UpperCase(aSource);
    lOld:=UpperCase(aOld);
    end
  else
    begin
    lSrch:=aSource;
    lOld:=aOld;
    end;
  lStart:=1;
  repeat
    lPos:=Pos(lOld,lSrch,lStart);
    if lPos=0 then
      Break;
    Result:=Result+Copy(aSource,lStart,lPos-lStart)+aNew;
    lStart:=lPos+Length(lOld);
    Inc(aCount);
  until not (rfReplaceAll in aFlags);
  Result:=Result+Copy(aSource,lStart,Length(aSource));
end;


// Returns True when all characters of aSource are below #128
function IsAscii(const aSource: UnicodeString): Boolean;

var
  i: SizeInt;

begin
  Result:=True;
  for i:=1 to Length(aSource) do
    if Ord(aSource[i])>127 then
      Exit(False);
end;


// Returns a printable description of a replace call
function Describe(const aSource, aOld, aNew: UnicodeString; aFlags: TReplaceFlags): string;

begin
  if Length(aSource)>40 then
    Result:='(length '+IntToStr(Length(aSource))+')'
  else
    Result:=''''+string(aSource)+'''';
  Result:=Result+', '''+string(aOld)+''' -> '''+string(aNew)+'''';
  if rfReplaceAll in aFlags then
    Result:=Result+' [all]';
  if rfIgnoreCase in aFlags then
    Result:=Result+' [ignorecase]';
end;


// Checks the three StringReplace variants against aExpected and aExpectedCount
procedure CheckReplace(const aSource, aOld, aNew: UnicodeString; aFlags: TReplaceFlags; const aExpected: UnicodeString; aExpectedCount: SizeInt);

var
  lDesc: string;
  lAS, lASaved, lAResult: AnsiString;
  lUS, lUSaved, lUResult: UnicodeString;
  lWS, lWSaved, lWResult: WideString;
  lCount: TReplaceCount;
  lIntCount: Integer;

begin
  lDesc:=Describe(aSource,aOld,aNew,aFlags);
  if IsAscii(aSource) and IsAscii(aOld) and IsAscii(aNew) then
    begin
    lAS:=AnsiString(aSource);
    lASaved:=Copy(lAS,1,Length(lAS));
    lAResult:=StringReplace(lAS,AnsiString(aOld),AnsiString(aNew),aFlags,lCount);
    Check(lAResult=AnsiString(aExpected),'AnsiString result of '+lDesc+': '''+lAResult+'''');
    Check(lCount=aExpectedCount,'AnsiString count of '+lDesc+': '+IntToStr(lCount));
    Check(lAS=lASaved,'AnsiString source unchanged by '+lDesc);
    lAResult:=StringReplace(lAS,AnsiString(aOld),AnsiString(aNew),aFlags,lIntCount);
    Check((lAResult=AnsiString(aExpected)) and (lIntCount=aExpectedCount),'AnsiString Integer count overload of '+lDesc);
    Check(StringReplace(lAS,AnsiString(aOld),AnsiString(aNew),aFlags)=AnsiString(aExpected),'AnsiString overload without count of '+lDesc);
    end;
  lUS:=aSource;
  lUSaved:=Copy(lUS,1,Length(lUS));
  lUResult:=UnicodeStringReplace(lUS,aOld,aNew,aFlags,lCount);
  Check(lUResult=aExpected,'UnicodeString result of '+lDesc+': '''+string(lUResult)+'''');
  Check(lCount=aExpectedCount,'UnicodeString count of '+lDesc+': '+IntToStr(lCount));
  Check(lUS=lUSaved,'UnicodeString source unchanged by '+lDesc);
  lWS:=aSource;
  lWSaved:=Copy(lWS,1,Length(lWS));
  lWResult:=WideStringReplace(lWS,WideString(aOld),WideString(aNew),aFlags,lCount);
  Check(lWResult=WideString(aExpected),'WideString result of '+lDesc+': '''+string(lWResult)+'''');
  Check(lCount=aExpectedCount,'WideString count of '+lDesc+': '+IntToStr(lCount));
  Check(lWS=lWSaved,'WideString source unchanged by '+lDesc);
end;


// Checks the three StringReplace variants against the reference implementation
procedure CheckAgainstRef(const aSource, aOld, aNew: UnicodeString; aFlags: TReplaceFlags);

var
  lExpected: UnicodeString;
  lExpectedCount: SizeInt;

begin
  lExpected:=RefReplace(aSource,aOld,aNew,aFlags,lExpectedCount);
  CheckReplace(aSource,aOld,aNew,aFlags,lExpected,lExpectedCount);
end;


{$push}{$Q-}{$R-}
// Returns a pseudo-random number in 0..aRange-1
function NextRandom(aRange: Cardinal): Cardinal;

begin
  gSeed:=gSeed*1103515245+12345;
  Result:=(gSeed shr 16) mod aRange;
end;
{$pop}


// Returns a pseudo-random string of aLength characters taken from aAlphabet
function RandomString(aLength: Integer; const aAlphabet: UnicodeString): UnicodeString;

var
  i: Integer;

begin
  SetLength(Result,aLength);
  for i:=1 to aLength do
    Result[i]:=aAlphabet[1+NextRandom(Length(aAlphabet))];
end;


procedure TestSimple;

begin
  CheckReplace('This works','works','only works',[rfReplaceAll],'This only works',1);
  CheckReplace('ababab','a','z',[rfReplaceAll],'zbzbzb',3);
  CheckReplace('abcabcabc','b','XY',[rfReplaceAll],'aXYcaXYcaXYc',3);
  CheckReplace('abcabcabc','bc','',[rfReplaceAll],'aaa',3);
  CheckReplace('abcabc','b','XY',[],'aXYcabc',1);
  CheckReplace('abcabc','b','X',[],'aXcabc',1);
  CheckReplace('abcabc','bc','',[],'aabc',1);
  CheckReplace('Hello World hello','HELLO','Bye',[rfReplaceAll,rfIgnoreCase],'Bye World Bye',2);
  CheckReplace('Hello World hello','HELLO','Bye',[rfIgnoreCase],'Bye World hello',1);
end;


procedure TestCorners;

begin
  CheckReplace('','a','b',[rfReplaceAll],'',0);
  CheckReplace('','a','',[rfReplaceAll],'',0);
  CheckReplace('','','x',[rfReplaceAll],'',0);
  CheckReplace('abc','','x',[rfReplaceAll],'abc',0);
  CheckReplace('abc','','',[],'abc',0);
  CheckReplace('ab','abc','x',[rfReplaceAll],'ab',0);
  CheckReplace('abc','x','yy',[rfReplaceAll],'abc',0);
  CheckReplace('abc','x','y',[rfReplaceAll],'abc',0);
  CheckReplace('abc','x','',[],'abc',0);
  CheckReplace('ABC','b','x',[rfReplaceAll],'ABC',0);
  CheckReplace('abc','abc','x',[rfReplaceAll],'x',1);
  CheckReplace('abc','abc','',[rfReplaceAll],'',1);
  CheckReplace('abc','abc','xyz',[rfReplaceAll],'xyz',1);
  CheckReplace('abc','abc','wxyz',[],'wxyz',1);
  CheckReplace('abcd','ab','X',[rfReplaceAll],'Xcd',1);
  CheckReplace('abcd','cd','XYZ',[rfReplaceAll],'abXYZ',1);
  CheckReplace('abcd','cd','',[],'ab',1);
  CheckReplace('aaaa','a','',[rfReplaceAll],'',4);
  CheckReplace('aaaa','a','',[],'aaa',1);
  CheckReplace('ababab','ab','x',[rfReplaceAll],'xxx',3);
  CheckReplace('aaa','aa','b',[rfReplaceAll],'ba',1);
  CheckReplace('aaaa','aa','b',[rfReplaceAll],'bb',2);
  CheckReplace('aaaaa','aa','bbb',[rfReplaceAll],'bbbbbba',2);
  CheckReplace('aaa','a','aa',[rfReplaceAll],'aaaaaa',3);
  CheckReplace('aaa','a','a',[rfReplaceAll],'aaa',3);
  CheckReplace('xax','a','xax',[rfReplaceAll],'xxaxx',1);
  CheckReplace('aBcAbC','b','-',[rfReplaceAll,rfIgnoreCase],'a-cA-C',2);
  CheckReplace('aBcAbC','B','x',[rfReplaceAll,rfIgnoreCase],'axcAxC',2);
  CheckReplace('aBcAbC','BC','',[rfReplaceAll,rfIgnoreCase],'aA',2);
  CheckReplace('a'#0'b'#0,#0,'<0>',[rfReplaceAll],'a<0>b<0>',2);
  CheckReplace('a'#0'b'#0,#0'b',#0,[rfReplaceAll],'a'#0#0,1);
  CheckReplace(#$E4#$F6#$FC#$F6,#$F6,'oe',[rfReplaceAll],#$E4'oe'#$FC'oe',2);
  CheckReplace(#$E4#$F6#$FC#$F6,#$F6,#$20AC,[],#$E4#$20AC#$FC#$F6,1);
  CheckReplace(#$D834#$DD1E'a'#$D834#$DD1E,#$D834#$DD1E,'G',[rfReplaceAll],'GaG',2);
  CheckReplace('GaG','G',#$D834#$DD1E,[rfReplaceAll],#$D834#$DD1E'a'#$D834#$DD1E,2);
end;


procedure TestMatchCounts;

var
  lCount: Integer;
  lS: UnicodeString;

begin
  for lCount:=0 to 1100 do
    begin
    lS:=StringOfChar(WideChar('a'),lCount);
    CheckReplace(lS,'a','bb',[rfReplaceAll],StringOfChar(WideChar('b'),2*lCount),lCount);
    CheckReplace(lS,'a','',[rfReplaceAll],'',lCount);
    CheckReplace(lS,'a','b',[rfReplaceAll],StringOfChar(WideChar('b'),lCount),lCount);
    CheckAgainstRef(lS+'x','a','yz',[]);
    end;
  for lCount:=0 to 400 do
    begin
    CheckAgainstRef(RandomString(lCount,'xab'),'ab','Q',[rfReplaceAll]);
    CheckAgainstRef(RandomString(lCount,'xab'),'ab','QRS',[rfReplaceAll]);
    end;
end;


procedure TestLarge;

const
  CLength = 300000;

var
  lS: UnicodeString;

begin
  lS:=StringOfChar(WideChar('a'),CLength);
  CheckAgainstRef(lS,'a','bc',[rfReplaceAll]);
  CheckAgainstRef(lS,'aa','',[rfReplaceAll]);
  CheckAgainstRef(lS,'a','bc',[]);
  CheckAgainstRef(lS+'z','z','',[rfReplaceAll]);
  lS:=RandomString(CLength,'ab');
  CheckAgainstRef(lS,'ab','x',[rfReplaceAll]);
  CheckAgainstRef(lS,'b','xyz',[rfReplaceAll]);
  CheckAgainstRef(lS,'abba','',[rfReplaceAll]);
end;


procedure TestRandom;

var
  i: Integer;
  lS, lOld, lNew: UnicodeString;

begin
  for i:=1 to 3000 do
    begin
    lS:=RandomString(NextRandom(200),'ab');
    lOld:=RandomString(1+NextRandom(3),'ab');
    lNew:=RandomString(NextRandom(5),'xy');
    CheckAgainstRef(lS,lOld,lNew,[rfReplaceAll]);
    CheckAgainstRef(lS,lOld,lNew,[]);
    lS:=RandomString(NextRandom(200),'aAbB');
    lOld:=RandomString(1+NextRandom(3),'aAbB');
    CheckAgainstRef(lS,lOld,lNew,[rfReplaceAll,rfIgnoreCase]);
    CheckAgainstRef(lS,lOld,lNew,[rfIgnoreCase]);
    end;
end;


begin
  TestSimple;
  TestCorners;
  TestMatchCounts;
  TestLarge;
  TestRandom;
  if gFailed>0 then
    begin
    Writeln(gFailed,' checks failed');
    Halt(1);
    end;
  Writeln('ok');
end.
