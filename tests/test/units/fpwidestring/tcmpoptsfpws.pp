{ TCompareOption values with the fpwidestring manager and the DUCET collation, issue #36527 }
program tcmpoptsfpws;

{$mode objfpc}{$h+}

uses
  SysUtils, unicodeducet, fpwidestring;

var
  gFailures: Integer;


// Formats aOptions for messages
function OptionsText(aOptions: TCompareOptions): string;

var
  lOption: TCompareOption;

begin
  Result:='';
  for lOption in aOptions do
    begin
    if Result<>'' then
      Result:=Result+',';
    WriteStr(Result,Result,lOption);
    end;
  Result:='['+Result+']';
end;


// Returns -1, 0 or 1 for the sign of aValue
function Sign(aValue: PtrInt): Integer;

begin
  if aValue<0 then
    Result:=-1
  else if aValue>0 then
    Result:=1
  else
    Result:=0;
end;


{ Fails unless comparing aS1 with aS2 using aOptions gives the sign aExpected (-1, 0, 1),
  or any non-zero sign when aExpected is 2, in both the UnicodeString and WideString procs. }
procedure Check(aCode: Integer; const aS1, aS2: UnicodeString; aOptions: TCompareOptions; aExpected: Integer);

var
  lUnicode, lWide: Integer;
  lW1, lW2: WideString;

begin
  lW1:=aS1;
  lW2:=aS2;
  lUnicode:=Sign(widestringmanager.CompareUnicodeStringProc(aS1,aS2,aOptions));
  lWide:=Sign(widestringmanager.CompareWideStringProc(lW1,lW2,aOptions));
  if ((aExpected=2) and (lUnicode=0)) or ((aExpected<>2) and (lUnicode<>aExpected)) or (lWide<>lUnicode) then
    begin
    Inc(gFailures);
    writeln('Failed ',aCode,': compare("',UTF8Encode(aS1),'","',UTF8Encode(aS2),'",',OptionsText(aOptions),
      ') gives ',lUnicode,' (WideString ',lWide,'), expected ',aExpected);
    end;
end;


const
  cResumeAccented = UnicodeString('r'#$00E9'sum'#$00E9);
  cResumeDecomposed = UnicodeString('re'#$0301'sume'#$0301);
  cResumeUpper = UnicodeString('R'#$00C9'SUM'#$00C9);
  cHiragana = UnicodeString(#$304B#$306A);
  cKatakana = UnicodeString(#$30AB#$30CA);
  cHalfKatakana = UnicodeString(#$FF76#$FF85);
  cFullABC = UnicodeString(#$FF21#$FF22#$FF23);
  cEuro = UnicodeString('a'#$20AC'b');
  cEmoji = UnicodeString('a'#$D83D#$DE00'b');
  cMathBoldA = UnicodeString(#$D835#$DC00);

begin
  gFailures:=0;
  if GetActiveCollation=nil then
    begin
    writeln('No active collation');
    halt(1);
    end;
  if widestringmanager.CompareUnicodeStringProc('a.b!c','abc',[])<>0 then
    begin
    writeln('fpwidestring with the DUCET collation is not the active widestring manager');
    halt(2);
    end;

  Check(1,cResumeAccented,'resume',[],2);
  Check(2,cResumeAccented,'resume',[coIgnoreNonSpace],0);
  Check(3,cResumeAccented,'resume',[coLingIgnoreDiacritic],0);
  Check(4,cResumeDecomposed,'resume',[coIgnoreNonSpace],0);
  Check(5,cResumeDecomposed,cResumeAccented,[coIgnoreNonSpace],0);
  Check(6,cResumeUpper,'resume',[coIgnoreCase,coIgnoreNonSpace],0);
  Check(7,cResumeAccented,'resumf',[coIgnoreNonSpace],-1);
  Check(8,UnicodeString(#$00F8),'o',[coIgnoreNonSpace],0);
  Check(9,cResumeUpper,'resume',[coIgnoreNonSpace],2);

  Check(10,cHiragana,cKatakana,[],2);
  Check(11,cHiragana,cKatakana,[coIgnoreKanaType],0);

  Check(20,cFullABC,'ABC',[],2);
  Check(21,cFullABC,'ABC',[coIgnoreWidth],0);
  Check(22,cFullABC,'abc',[coIgnoreWidth,coIgnoreCase],0);
  Check(23,cHalfKatakana,cKatakana,[],2);
  Check(24,cHalfKatakana,cKatakana,[coIgnoreWidth],0);
  Check(25,cHalfKatakana,cHiragana,[coIgnoreWidth,coIgnoreKanaType],0);

  Check(30,'a.b!c','abc',[coIgnoreSymbols],0);
  Check(31,'a-b','ab',[coIgnoreSymbols],0);
  Check(32,'ab.','ac',[coIgnoreSymbols],-1);
  Check(33,cEuro,'ab',[],2);
  Check(34,cEuro,'ab',[coIgnoreSymbols],0);
  Check(35,cEmoji,'ab',[coIgnoreSymbols],0);
  Check(36,'a b','ab',[coIgnoreSymbols],0);
  Check(37,cMathBoldA,cMathBoldA,[coIgnoreSymbols],0);
  Check(38,cMathBoldA,'',[coIgnoreSymbols],1);

  Check(40,'file2','file10',[],1);
  Check(41,'file2','file10',[coDigitAsNumbers],-1);
  Check(42,'file10','file10',[coDigitAsNumbers],0);
  Check(43,'x9','x10',[coDigitAsNumbers],-1);
  Check(44,'a2b10','a2b9',[coDigitAsNumbers],1);
  Check(45,'file10a','file10b',[coDigitAsNumbers],-1);
  Check(46,'FILE2','file10',[coDigitAsNumbers,coIgnoreCase],-1);

  Check(50,'abc','ABC',[coIgnoreCase,coLingCasing],0);
  Check(51,'abc','abd',[coLingCasing],-1);

  Check(60,'co-op','coat',[],1);
  Check(61,'co-op','coat',[coStringSort],-1);

  Check(70,'abc','abd',[Low(TCompareOption)..High(TCompareOption)],-1);
  Check(71,'','',[Low(TCompareOption)..High(TCompareOption)],0);
  Check(72,'abc','ABC',[],2);
  Check(73,'abc','ABC',[coIgnoreCase],0);

  if gFailures>0 then
    begin
    writeln(gFailures,' failures');
    halt(1);
    end;
  writeln('ok');
end.
