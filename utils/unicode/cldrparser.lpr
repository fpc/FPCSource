{   Unicode CLDR's collation parser.
    Copyright (c) 2013 by Inoussa OUEDRAOGO

    It creates units from CLDR's collation files.

    The source code is distributed under the Library GNU
    General Public License with the following modification:

        - object files and libraries linked into an application may be
          distributed without source code.

    If you didn't receive a copy of the file COPYING, contact:
          Free Software Foundation
          675 Mass Ave
          Cambridge, MA  02139
          USA

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program cldrparser;

{$mode objfpc}{$H+}
{ $define WINCE_TEST}

uses
  SysUtils, classes, getopts,{$ifdef WINCE}StreamIO,{$endif}
  cldrhelper, helper, cldrtest, cldrxml, unicodeset;

const
  SUsageText =
    'This program creates pascal units from CLDR''s collation files for usage ' + sLineBreak +
    'with the FreePascal Native Unicode Manager.' + sLineBreak + sLineBreak +
    'Usage : cldrparser <collationFileName> [<typeName>] [-d<dataDir>] [-o<outputDir>] [-t]' + sLineBreak + sLineBreak +
    '  where :' + sLineBreak +
    ' ' + sLineBreak +
    '   - collationFileName : specify the target file.' + sLineBreak +
    '   - typeName : optional, specify the collation'' type-name to be parse;' + sLineBreak +
    '     If this argument is not supplied, a default type-name' + sLineBreak +
    '     is chosen from the type-name list in the following order  : ' + sLineBreak +
    '          * the "default" specified type-name indicated by the collation' + sLineBreak +
    '          * the type named "standard" ' + sLineBreak +
    '          * the type named "search" ' + sLineBreak +
    '          * the first type.' + sLineBreak +
    '   - dataDir : specify the directory that contains the collation files.' + sLineBreak +
    '               The default value is the program''s directory.' + sLineBreak +
    '   - outputDir : specify the directory where the generated files will be stored.' + sLineBreak +
    '                 The default value is the program''s directory.' + sLineBreak +
    '   - t : to execute parser the test suite. The program will execute only the test suite and exit.' + sLineBreak +
    ' ' + sLineBreak +
    '  The program expects some files to be present in the <dataDir> folder : ' + sLineBreak +
    '     - UCA_Rules_SHORT.xml found in the CollationAuxiliary.zip available on unicode.org' + sLineBreak +
    '     - allkeys.txt this is the file allkeys_CLDR.txt contained in CollationAuxiliary.zip renamed to allkeys.txt' + sLineBreak +
    '  The CollationAuxiliary.zip archive is provided by unicode in the "unicode collation algorithm data files" section.';


function ParseOptions(
  var ADataDir,
      AOuputDir,
      ACollationFileName,
      ACollationTypeName : string;
  var AExecTestSuite     : Boolean
) : Boolean;
var
  c : Char;
  idx, k : Integer;
  s : string;
begin
{$ifdef WINCE_TEST}
  ADataDir := ExtractFilePath(ParamStr(0))+'data';
  AOuputDir := ADataDir;
  ACollationFileName := 'sv.xml';
  exit(True);
{$endif WINCE_TEST}
  if (ParamCount() = 0) then
    exit(False);
  Result := True;
  AExecTestSuite := False;
  repeat
    c := GetOpt('d:o:ht');
    case c of
      'd' : ADataDir := ExpandFileName(Trim(OptArg));
      'o' : AOuputDir := ExpandFileName(Trim(OptArg));
      'h', '?' :
        begin
          WriteLn(SUsageText);
          Result := False;
        end;
      't' : AExecTestSuite := True;
    end;
  until (c = EndOfOptions);
  idx := 0;
  for k := 1 to ParamCount() do begin
    s := Trim(ParamStr(k));
    if (s <> '') and (s[1] <> '-') then begin
      if (idx = 0) then
        ACollationFileName := s
      else if (idx = 1) then
        ACollationTypeName := s;
      Inc(idx);
      if (idx >= 2) then
        Break;
    end;
  end;
end;

var
  orderedChars : TOrderedCharacters;
  ucaBook : TUCA_DataBook;
  stream, streamNE, streamOE, binaryStreamNE, binaryStreamOE : TMemoryStream;
  s, collationFileName, collationTypeName : string;
  i , c: Integer;
  collation : TCldrCollation;
  dataPath, outputPath : string;
  collationItem : TCldrCollationItem;
  testSuiteFlag : Boolean;
{$ifdef WINCE}
  fs : TFileStream;
{$endif WINCE}
begin
{$ifdef WINCE}
  s := ExtractFilePath(ParamStr(0))+'cldr-log.txt';
  DeleteFile(s);
  fs := TFileStream.Create(s,fmCreate);
  AssignStream(Output,fs);
  Rewrite(Output);
  s := ExtractFilePath(ParamStr(0))+'cldr-err.txt';
  DeleteFile(s);
  fs := TFileStream.Create(s,fmCreate);
  AssignStream(ErrOutput,fs);
  Rewrite(ErrOutput);
{$endif WINCE}
{$ifdef WINCE_TEST}
  testSuiteFlag := True;
  try
    exec_tests();
  except
    on e : Exception do begin
      WriteLn('Exception : '+e.Message);
      raise;
    end;
  end;
  exit;
{$endif WINCE_TEST}
  dataPath := '';
  outputPath := '';
  collationFileName := '';
  collationTypeName := '';
  testSuiteFlag := False;
  if not ParseOptions(dataPath,outputPath,collationFileName,collationTypeName,testSuiteFlag) then
    Halt(1);
  if testSuiteFlag then begin
    exec_tests();
    Halt;
  end;
  if (dataPath <> '') and not(DirectoryExists(dataPath)) then begin
    WriteLn('This directory does not exist : ',dataPath);
    Halt(1);
  end;
  if (dataPath = '') then
    dataPath := ExtractFilePath(ParamStr(0))
  else
    dataPath := IncludeTrailingPathDelimiter(dataPath);
  if (outputPath = '') then
    outputPath := dataPath
  else
    outputPath := IncludeTrailingPathDelimiter(outputPath);
{$ifndef WINCE_TEST}
  if (ParamCount() = 0) then begin
    WriteLn(SUsageText);
    Halt(1);
  end;
{$endif WINCE_TEST}
  if not(
       FileExists(dataPath+'UCA_Rules_SHORT.xml') and
       FileExists(dataPath+'allkeys.txt')
     )
  then begin
    WriteLn(Format('File not found : %s or %s.',[dataPath+'UCA_Rules_SHORT.xml',dataPath+'allkeys.txt']));
    Halt(1);
  end;

  collationFileName := dataPath + collationFileName;
  if not FileExists(collationFileName) then begin
    WriteLn('File not found : "',collationFileName,'"');
    Halt(1);
  end;

  WriteLn(sLineBreak,'Collation Parsing ',QuotedStr(collationFileName),'  ...');
  stream := nil;
  streamNE := nil;
  streamOE := nil;
  binaryStreamNE := nil;
  binaryStreamOE := nil;
  collation := TCldrCollation.Create();
  try
    ParseCollationDocument(collationFileName,collation,TCldrParserMode.HeaderParsing);
    WriteLn(Format('  Collation Count = %d',[collation.ItemCount]));
    if (collation.ItemCount = 0) then begin
      WriteLn('No collation in this file.');
    end else begin
      for i := 0 to collation.ItemCount - 1 do
        WriteLn(Format('  Item[%d] = (Type = %s)',[i, collation.Items[i].TypeName]));
      collationItem := collation.Find(collationTypeName);
      if (collationItem = nil) then begin
        collationTypeName := FindCollationDefaultItemName(collation);
        collationItem := collation.Find(collationTypeName);
      end;
      WriteLn(Format('Parsing Collation Item "%s" ...',[collationTypeName]));
      ParseCollationDocument(collationFileName,collationItem,collationTypeName);

      s := dataPath + 'UCA_Rules_SHORT.xml';
      WriteLn;
      WriteLn('Parsing ',QuotedStr(s),'  ...');
      FillByte(orderedChars,SizeOf(orderedChars),0);
      orderedChars.Clear();
      ParseInitialDocument(@orderedChars,s);
      WriteLn('File parsed, ',orderedChars.ActualLength,' characters.');

      WriteLn('Loading CLDR root''s key table ...');
      stream := TMemoryStream.Create();
      s := dataPath + 'allkeys.txt';
      stream.LoadFromFile(s);
      ParseUCAFile(stream,ucaBook);
      c := FillInitialPositions(@orderedChars.Data[0],orderedChars.ActualLength,ucaBook.Lines);
      if (c > 0) then
        WriteLn('  Missed Initial Positions = ',c);
      WriteLn('   Loaded.');

      WriteLn('Start generation ...');
      stream.Clear();
      streamNE := TMemoryStream.Create();
      streamOE := TMemoryStream.Create();
      binaryStreamNE := TMemoryStream.Create();
      binaryStreamOE := TMemoryStream.Create();
      s := COLLATION_FILE_PREFIX + ChangeFileExt(LowerCase(ExtractFileName(collationFileName)),'.pas');
      GenerateCdlrCollation(
        collation,collationTypeName,s,stream,streamNE,streamOE,
        binaryStreamNE,binaryStreamOE,
        orderedChars,ucaBook.Lines
      );
      stream.SaveToFile(ExtractFilePath(collationFileName)+s);
      if (streamNE.Size > 0) then begin
        streamNE.SaveToFile(ExtractFilePath(collationFileName)+GenerateEndianIncludeFileName(s,ENDIAN_NATIVE));
        streamOE.SaveToFile(ExtractFilePath(collationFileName)+GenerateEndianIncludeFileName(s,ENDIAN_NON_NATIVE));
      end;
      if (binaryStreamNE.Size > 0) then begin
        binaryStreamNE.SaveToFile(
          ExtractFilePath(collationFileName) +
          ChangeFileExt(s,Format('_%s.bco',[ENDIAN_SUFFIX[ENDIAN_NATIVE]]))
        );
        binaryStreamOE.SaveToFile(
          ExtractFilePath(collationFileName) +
          ChangeFileExt(s,Format('_%s.bco',[ENDIAN_SUFFIX[ENDIAN_NON_NATIVE]]))
        );
      end;
    end;
  finally
    binaryStreamOE.Free();
    binaryStreamNE.Free();
    streamOE.Free();
    streamNE.Free();
    stream.Free();
    collation.Free();
  end;

  WriteLn(sLineBreak,'Finished.');
end.

