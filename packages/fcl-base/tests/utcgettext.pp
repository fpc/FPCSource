{ Tests of language ID detection and .mo file selection in GetText }
unit utcGetText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, GetText, punit;

// Registers the GetText suite
procedure RegisterTests;

implementation

resourcestring
  rsHello = 'Hello';
  rsBye = 'Goodbye';
  rsThanks = 'Thanks';

var
  ErrorCount: Integer;


{ Counts translation errors without raising them }
procedure CountError(const aFileName, aUnitName: String; aError: Exception; out ReRaise: Boolean);

begin
  Inc(ErrorCount);
  ReRaise:=False;
end;


{ Writes a .mo file with the translations aTrans of aOrig }
procedure WriteMOFile(const aFileName: String; const aOrig, aTrans: array of AnsiString);

const
  CHashSize = 11;

var
  lStream: TFileStream;
  lHeader: TMOFileHeader;
  lHash: array[0..CHashSize-1] of LongWord;
  lInfo: TMOStringInfo;
  lCount, i: Integer;
  lOffset, lValue, lIndex, lIncr: LongWord;

  // Writes the table entries for aStrings, with string data starting at lOffset
  procedure WriteTable(const aStrings: array of AnsiString);

  var
    j: Integer;

  begin
    for j:=0 to lCount-1 do
      begin
      lInfo.length:=Length(aStrings[j]);
      lInfo.offset:=lOffset;
      lStream.WriteBuffer(lInfo,SizeOf(lInfo));
      Inc(lOffset,Length(aStrings[j])+1);
      end;
  end;

  // Writes aStrings, each followed by a #0
  procedure WriteStrings(const aStrings: array of AnsiString);

  var
    j: Integer;
    lZero: Byte;

  begin
    lZero:=0;
    for j:=0 to lCount-1 do
      begin
      if aStrings[j]<>'' then
        lStream.WriteBuffer(aStrings[j][1],Length(aStrings[j]));
      lStream.WriteBuffer(lZero,1);
      end;
  end;

begin
  lCount:=Length(aOrig);
  FillChar(lHash,SizeOf(lHash),0);
  for i:=0 to lCount-1 do
    begin
    lValue:=Hash(aOrig[i]);
    lIndex:=lValue mod CHashSize;
    lIncr:=1+(lValue mod (CHashSize-2));
    while lHash[lIndex]<>0 do
      if lIndex>=CHashSize-lIncr then
        Dec(lIndex,CHashSize-lIncr)
      else
        Inc(lIndex,lIncr);
    lHash[lIndex]:=i+1;
    end;
  lHeader.magic:=MOFileHeaderMagic;
  lHeader.revision:=0;
  lHeader.nstrings:=lCount;
  lHeader.OrigTabOffset:=SizeOf(lHeader);
  lHeader.TransTabOffset:=lHeader.OrigTabOffset+LongWord(lCount)*SizeOf(TMOStringInfo);
  lHeader.HashTabSize:=CHashSize;
  lHeader.HashTabOffset:=lHeader.TransTabOffset+LongWord(lCount)*SizeOf(TMOStringInfo);
  lStream:=TFileStream.Create(aFileName,fmCreate);
  try
    lStream.WriteBuffer(lHeader,SizeOf(lHeader));
    lOffset:=lHeader.HashTabOffset+SizeOf(lHash);
    WriteTable(aOrig);
    WriteTable(aTrans);
    lStream.WriteBuffer(lHash,SizeOf(lHash));
    WriteStrings(aOrig);
    WriteStrings(aTrans);
  finally
    lStream.Free;
  end;
end;


{ Returns aList as a comma-separated string }
function ListText(const aList: TStringDynArray): String;

var
  i: Integer;

begin
  Result:='';
  for i:=0 to Length(aList)-1 do
    begin
    if i>0 then
      Result:=Result+',';
    Result:=Result+aList[i];
    end;
end;


{ Returns the ID list for LANGUAGE aLanguage and LANG aLang as a comma-separated string }
function LangList(const aLanguage, aLang: String): String;

begin
  Result:=ListText(GetLanguageIDList(aLanguage,'','',aLang));
end;


var
  TempDir: String;


{ Creates the .mo files test.de.mo, test.de_AT.mo and the invalid test.xx.mo in TempDir }
function SetupFiles: TTestString;

var
  lFile: TStringList;

begin
  Result:='';
  TempDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'utcgettext'+IntToStr(GetProcessID)+PathDelim;
  ForceDirectories(TempDir);
  WriteMOFile(TempDir+'test.de.mo',['Hello','Goodbye'],['Hallo','Tschuess']);
  WriteMOFile(TempDir+'test.de_AT.mo',['Hello'],['Servus']);
  WriteMOFile(TempDir+'test.pt.mo',['Thanks'],['Obrigado']);
  lFile:=TStringList.Create;
  try
    lFile.Add('not a mo file');
    lFile.SaveToFile(TempDir+'test.xx.mo');
  finally
    lFile.Free;
  end;
end;


{ Removes the files created by SetupFiles }
function TearDownFiles: TTestString;

begin
  Result:='';
  DeleteFile(TempDir+'test.de.mo');
  DeleteFile(TempDir+'test.de_AT.mo');
  DeleteFile(TempDir+'test.pt.mo');
  DeleteFile(TempDir+'test.xx.mo');
  RemoveDir(TempDir);
end;


{ Restores the resource strings and the error counter }
function ResetTranslations: TTestString;

begin
  Result:='';
  ResetResourceTables;
  ErrorCount:=0;
  OnTranslationError:=@CountError;
end;


function TestNormalize: TTestString;

begin
  Result:='';
  AssertEquals('Codeset is removed','es_ES',NormalizeLanguageID('es_ES.UTF-8'));
  AssertEquals('Modifier is removed','sr_RS',NormalizeLanguageID('sr_RS@latin'));
  AssertEquals('Codeset and modifier are removed','de_DE',NormalizeLanguageID('de_DE.ISO-8859-15@euro'));
  AssertEquals('Three-letter language is kept','ast_ES',NormalizeLanguageID('ast_ES.UTF-8'));
  AssertEquals('Language without country is kept','de',NormalizeLanguageID('de'));
  AssertEquals('C means no translation','',NormalizeLanguageID('C'));
  AssertEquals('C with codeset means no translation','',NormalizeLanguageID('C.UTF-8'));
  AssertEquals('POSIX means no translation','',NormalizeLanguageID('POSIX'));
  AssertEquals('Empty locale','',NormalizeLanguageID(''));
end;


function TestLanguagePart: TTestString;

begin
  Result:='';
  AssertEquals('Language part of a two-letter ID','de',LanguageIDLanguage('de_AT'));
  AssertEquals('Language part of a three-letter ID','ast',LanguageIDLanguage('ast_ES'));
  AssertEquals('ID without country is its own language part','de',LanguageIDLanguage('de'));
end;


function TestListFromLang: TTestString;

begin
  Result:='';
  AssertEquals('LANG with codeset','es_ES,es',LangList('','es_ES.UTF-8'));
  AssertEquals('LANG with modifier','sr_RS,sr',LangList('','sr_RS@latin'));
  AssertEquals('LANG with three-letter language','ast_ES,ast',LangList('','ast_ES.UTF-8'));
  AssertEquals('LANG without country','de',LangList('','de'));
end;


function TestListNoTranslation: TTestString;

begin
  Result:='';
  AssertEquals('LANG=C gives no IDs','',LangList('','C'));
  AssertEquals('LANG=POSIX gives no IDs','',LangList('','POSIX'));
  AssertEquals('LANG=C.UTF-8 gives no IDs','',LangList('','C.UTF-8'));
  AssertEquals('No locale gives no IDs','',LangList('',''));
  AssertEquals('LANGUAGE is ignored when the locale is C','',LangList('pt_BR:de','C'));
  AssertEquals('LANGUAGE is ignored without a locale','',LangList('pt_BR:de',''));
end;


function TestListFromLanguage: TTestString;

begin
  Result:='';
  AssertEquals('LANGUAGE entries in order, each followed by its language','pt_BR,pt,de',LangList('pt_BR:de','en_US.UTF-8'));
  AssertEquals('Duplicate entries are removed','pt_BR,pt,de',LangList('pt_BR:pt:de','en_US.UTF-8'));
  AssertEquals('Empty and C entries are skipped','de,fr',LangList('de::C:fr','en_US.UTF-8'));
  AssertEquals('LANGUAGE entries are normalized','sr_RS,sr,de_AT,de',LangList('sr_RS@latin:de_AT.UTF-8','en_US.UTF-8'));
end;


function TestListPrecedence: TTestString;

begin
  Result:='';
  AssertEquals('LC_ALL before LC_MESSAGES and LANG','fr_FR,fr',ListText(GetLanguageIDList('','fr_FR.UTF-8','de_DE','es_ES')));
  AssertEquals('LC_MESSAGES before LANG','de_DE,de',ListText(GetLanguageIDList('','','de_DE','es_ES')));
  AssertEquals('LC_ALL=C disables LANG','',ListText(GetLanguageIDList('','C','','es_ES')));
end;


function TestGetLanguageIDs: TTestString;

var
  lList: TStringDynArray;
  lLang, lFallbackLang: AnsiString;

begin
  Result:='';
  lList:=GetLanguageIDList;
  GetLanguageIDs(lLang,lFallbackLang);
  if Length(lList)=0 then
    begin
    AssertEquals('No Lang without language IDs','',lLang);
    AssertEquals('No FallbackLang without language IDs','',lFallbackLang);
    end
  else
    begin
    AssertEquals('Lang is the most preferred ID',lList[0],lLang);
    AssertEquals('FallbackLang is the language part of Lang',LanguageIDLanguage(lList[0]),lFallbackLang);
    AssertFalse('Lang has no codeset',Pos('.',lLang)>0);
    AssertFalse('Lang has no modifier',Pos('@',lLang)>0);
    end;
end;


function TestUnitCountryFile: TTestString;

begin
  Result:=ResetTranslations;
  TranslateUnitResourceStrings('utcGetText',TempDir+'test.%s.mo',['de_AT','de']);
  AssertEquals('Country file overrides the language file','Servus',rsHello);
  AssertEquals('Language file fills strings missing in the country file','Tschuess',rsBye);
  AssertEquals('Untranslated string keeps its value','Thanks',rsThanks);
  AssertEquals('No translation errors',0,ErrorCount);
  ResetTranslations;
end;


function TestUnitLanguageFileOnly: TTestString;

begin
  Result:=ResetTranslations;
  TranslateUnitResourceStrings('utcGetText',TempDir+'test.%s.mo',['de_CH','de']);
  AssertEquals('Language file is used when the country file is missing','Hallo',rsHello);
  AssertEquals('No translation error for the missing country file',0,ErrorCount);
  ResetTranslations;
end;


function TestUnitPreferenceOrder: TTestString;

begin
  Result:=ResetTranslations;
  TranslateUnitResourceStrings('utcGetText',TempDir+'test.%s.mo',['pt_BR','pt','de']);
  AssertEquals('Most preferred language translates its strings','Obrigado',rsThanks);
  AssertEquals('Less preferred language fills the remaining strings','Hallo',rsHello);
  ResetTranslations;
end;


function TestUnitOtherUnit: TTestString;

begin
  Result:=ResetTranslations;
  TranslateUnitResourceStrings('NoSuchUnit',TempDir+'test.%s.mo',['de']);
  AssertEquals('Strings of other units are not translated','Hello',rsHello);
  ResetTranslations;
end;


function TestAllUnits: TTestString;

begin
  Result:=ResetTranslations;
  TranslateResourceStrings(TempDir+'test.%s.mo',['de_AT','de']);
  AssertEquals('Country file overrides the language file','Servus',rsHello);
  AssertEquals('Language file fills strings missing in the country file','Tschuess',rsBye);
  AssertEquals('No translation errors',0,ErrorCount);
  ResetTranslations;
end;


function TestNoLanguages: TTestString;

begin
  Result:=ResetTranslations;
  TranslateResourceStrings(TempDir+'test.%s.mo',[]);
  AssertEquals('Empty ID list translates nothing','Hello',rsHello);
  AssertEquals('Empty ID list gives no errors',0,ErrorCount);
  ResetTranslations;
end;


function TestInvalidFile: TTestString;

begin
  Result:=ResetTranslations;
  TranslateUnitResourceStrings('utcGetText',TempDir+'test.%s.mo',['xx','de']);
  AssertEquals('Invalid file is reported once',1,ErrorCount);
  AssertEquals('Other files are still applied','Hallo',rsHello);
  ResetTranslations;
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('GetText');
  AddTest('Normalize', @TestNormalize, lSuite);
  AddTest('LanguagePart', @TestLanguagePart, lSuite);
  AddTest('ListFromLang', @TestListFromLang, lSuite);
  AddTest('ListNoTranslation', @TestListNoTranslation, lSuite);
  AddTest('ListFromLanguage', @TestListFromLanguage, lSuite);
  AddTest('ListPrecedence', @TestListPrecedence, lSuite);
  AddTest('GetLanguageIDs', @TestGetLanguageIDs, lSuite);
  lSuite:=AddSuite('GetTextFiles', @SetupFiles, @TearDownFiles);
  AddTest('UnitCountryFile', @TestUnitCountryFile, lSuite);
  AddTest('UnitLanguageFileOnly', @TestUnitLanguageFileOnly, lSuite);
  AddTest('UnitPreferenceOrder', @TestUnitPreferenceOrder, lSuite);
  AddTest('UnitOtherUnit', @TestUnitOtherUnit, lSuite);
  AddTest('AllUnits', @TestAllUnits, lSuite);
  AddTest('NoLanguages', @TestNoLanguages, lSuite);
  AddTest('InvalidFile', @TestInvalidFile, lSuite);
end;


end.
