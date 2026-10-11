{ StringReplace with every TStringReplaceAlgorithm, for 1 and 2 byte Char }
program tstrreplalg;

{$mode objfpc}{$h+}

uses
  {$ifdef unix}cwstring,{$endif} SysUtils, StrUtils;

const
  cAlgorithmNames: array[TStringReplaceAlgorithm] of string = ('sraDefault','sraManySmall','sraBoyerMoore');

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


// Checks StringReplace of aOld by aNew in aSource with algorithm aAlgorithm
procedure CheckReplace(aAlgorithm: TStringReplaceAlgorithm; const aSource, aOld, aNew: string; aFlags: TReplaceFlags;
  const aExpected: string; aExpectedCount: Integer; const aMessage: string);

var
  lResult: string;
  lCount: Integer;

begin
  lResult:=StringReplace(aSource,aOld,aNew,aFlags,lCount,aAlgorithm);
  Check(lResult=aExpected,cAlgorithmNames[aAlgorithm]+' result: '+aMessage);
  Check(lCount=aExpectedCount,cAlgorithmNames[aAlgorithm]+' count: '+aMessage);
end;


// Checks that aMatches equals aExpected
procedure CheckMatches(const aMatches: SizeIntArray; const aExpected: array of SizeInt; const aMessage: string);

var
  lIdx: Integer;
  lOK: Boolean;

begin
  lOK:=Length(aMatches)=Length(aExpected);
  if lOK then
    for lIdx:=0 to High(aExpected) do
      lOK:=lOK and (aMatches[lIdx]=aExpected[lIdx]);
  Check(lOK,aMessage);
end;


var
  lAlgorithm: TStringReplaceAlgorithm;
  lMatches: SizeIntArray;
{$if sizeof(Char)=2}
  lS, lWide, lWideA: string;
{$endif}

begin
  for lAlgorithm in TStringReplaceAlgorithm do
    begin
    CheckReplace(lAlgorithm,'abcabcabc','b','XY',[rfReplaceAll],'aXYcaXYcaXYc',3,'longer replacement');
    CheckReplace(lAlgorithm,'abcabcabc','bc','',[rfReplaceAll],'aaa',3,'removal');
    CheckReplace(lAlgorithm,'abcabcabc','bc','Z',[],'aZabcabc',1,'first occurrence');
    CheckReplace(lAlgorithm,'abcABCabc','BC','-',[rfReplaceAll,rfIgnoreCase],'a-A-a-',3,'ignore case');
    CheckReplace(lAlgorithm,'hello hehehe','hehe','X',[rfReplaceAll],'hello Xhe',1,'overlapping pattern');
    CheckReplace(lAlgorithm,'aaaaa','aa','X',[rfReplaceAll],'XXa',2,'repeated pattern character');
    CheckReplace(lAlgorithm,'abc','x','y',[rfReplaceAll],'abc',0,'no match');
    CheckReplace(lAlgorithm,'xyz','xyz','',[rfReplaceAll],'',1,'whole string');
    end;

  Check(FindMatchesBoyerMooreCaseSensitive('abcabcabc','bc',lMatches,True),'FindMatchesBoyerMooreCaseSensitive finds matches');
  CheckMatches(lMatches,[2,5,8],'FindMatchesBoyerMooreCaseSensitive positions');
  Check(FindMatchesBoyerMooreCaseInSensitive('aBCabcAbC','bc',lMatches,True),'FindMatchesBoyerMooreCaseInSensitive finds matches');
  CheckMatches(lMatches,[2,5,8],'FindMatchesBoyerMooreCaseInSensitive positions');

{$if sizeof(Char)=2}
  lS:=WideChar($0161);
  lWide:='a'+lS+'b'+lS+'a';
  lWideA:=lS+'a';
  for lAlgorithm in TStringReplaceAlgorithm do
    begin
    CheckReplace(lAlgorithm,lWide,lS,'-',[rfReplaceAll],'a-b-a',2,'non-ASCII character');
    CheckReplace(lAlgorithm,lWide,lWideA,'Q',[rfReplaceAll],'a'+lS+'bQ',1,'characters with the same low byte');
    CheckReplace(lAlgorithm,lWide,'A','Q',[rfReplaceAll,rfIgnoreCase],'Q'+lS+'b'+lS+'Q',2,'ignore case with non-ASCII characters');
    end;
  Check(FindMatchesBoyerMooreCaseSensitive(lWide,lWideA,lMatches,True),'FindMatchesBoyerMooreCaseSensitive finds a non-ASCII pattern');
  CheckMatches(lMatches,[4],'FindMatchesBoyerMooreCaseSensitive non-ASCII position');
{$endif}

  if gFailed then
    Halt(1);
  Writeln('ok');
end.
