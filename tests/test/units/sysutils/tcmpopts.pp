{ %TARGET=win32,win64,linux,freebsd,netbsd,openbsd,dragonfly,solaris,haiku }
{ TCompareOption values in the CompareUnicodeString and CompareWideString procs, issue #36527 }
program tcmpopts;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cwstring,
{$endif}
  SysUtils;

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

begin
  gFailures:=0;

  Check(1,cResumeAccented,'resume',[],2);
  Check(2,cResumeAccented,'resume',[coIgnoreNonSpace],0);
  Check(3,cResumeAccented,'resume',[coLingIgnoreDiacritic],0);
  Check(4,cResumeDecomposed,'resume',[coIgnoreNonSpace],0);
  Check(5,cResumeDecomposed,cResumeAccented,[coIgnoreNonSpace],0);
  Check(6,cResumeUpper,'resume',[coIgnoreCase,coIgnoreNonSpace],0);
  Check(7,cResumeAccented,'resumf',[coIgnoreNonSpace],-1);
  Check(8,UnicodeString(#$00F8),'o',[coIgnoreNonSpace],0);

  Check(10,cHiragana,cKatakana,[],2);
  Check(11,cHiragana,cKatakana,[coIgnoreKanaType],0);

  Check(20,cFullABC,'ABC',[],2);
  Check(21,cFullABC,'ABC',[coIgnoreWidth],0);
  Check(22,cFullABC,'abc',[coIgnoreWidth,coIgnoreCase],0);
  Check(23,cHalfKatakana,cKatakana,[],2);
  Check(24,cHalfKatakana,cKatakana,[coIgnoreWidth],0);
  Check(25,cHalfKatakana,cHiragana,[coIgnoreWidth,coIgnoreKanaType],0);

  Check(30,'a.b!c','abc',[],2);
  Check(31,'a.b!c','abc',[coIgnoreSymbols],0);
  Check(32,'a-b','ab',[coIgnoreSymbols],0);
  Check(33,'ab.','ac',[coIgnoreSymbols],-1);

  Check(40,'file2','file10',[],1);
  Check(41,'file2','file10',[coDigitAsNumbers],-1);
  Check(42,'file10','file10',[coDigitAsNumbers],0);
  Check(43,'x9','x10',[coDigitAsNumbers],-1);
  Check(44,'a2b10','a2b9',[coDigitAsNumbers],1);
  Check(45,'file10a','file10b',[coDigitAsNumbers],-1);
  Check(46,'FILE2','file10',[coDigitAsNumbers,coIgnoreCase],-1);

  Check(50,'abc','ABC',[coIgnoreCase,coLingCasing],0);
  Check(51,'abc','abd',[coLingCasing],-1);

{$ifdef windows}
  Check(60,'co-op','coat',[],1);
  Check(61,'co-op','coat',[coStringSort],-1);
{$endif windows}

  Check(70,'abc','abd',[Low(TCompareOption)..High(TCompareOption)],-1);
  Check(71,'','',[Low(TCompareOption)..High(TCompareOption)],0);

  if gFailures>0 then
    begin
    writeln(gFailures,' failures');
    halt(1);
    end;
  writeln('ok');
end.
