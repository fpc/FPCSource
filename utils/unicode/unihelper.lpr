{   Unicode tables parser.

    Copyright (c) 2012 by Inoussa OUEDRAOGO

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
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{ This program generates tables as include-files for use
  with the unicode related sources. It expects the following
  unicode.org's files to be present in the same folder :
    * HangulSyllableType.txt
    * PropList.txt
    * UnicodeData.txt
    * allkeys.txt
}

{$DEFINE UCA_TEST}
program unihelper;

{$mode objfpc}{$H+}
{$typedaddress on}

uses
  SysUtils, Classes,
  helper, uca_test;

const
  SUsage =
    'This program generates tables as include-files for use ' + sLineBreak +
    '  with the unicode related sources. It expects the following ' + sLineBreak +
    '  unicode.org''s files to be present in the same folder : ' + sLineBreak +
    '    * HangulSyllableType.txt ' + sLineBreak +
    '    * PropList.txt ' + sLineBreak +
    '    * UnicodeData.txt ' + sLineBreak +
    '    * allkeys.txt : Note that this file is the one provided for the CLDR root.' + sLineBreak +
    '' + sLineBreak +
    'Usage : unihelper [<dataDir> <outputDir>] ' + sLineBreak +
    '  where ' + sLineBreak +
    '    dataDir : the directory where are stored the unicode files. The default' + sLineBreak +
    '              value is the program''s directory.' + sLineBreak +
    '    outputDir : The directory where the generated files will be stored. The' + sLineBreak +
    '                default value is the program''s directory.'+sLineBreak;

function DumpCodePoint(ACodePoint : TCodePointRec) : string;
begin
  Result := '';
  if (ACodePoint.LineType = 0) then
    WriteStr(Result,IntToHex(ACodePoint.CodePoint,4))
  else
    WriteStr(Result,IntToHex(ACodePoint.StartCodePoint,4),'..',IntToHex(ACodePoint.EndCodePoint,4));
end;

var
  dataPath, outputPath : string;
  stream, binStreamNE, binStreamOE, tmpStream : TMemoryStream;
  binaryStreamNE, binaryStreamOE : TMemoryStream;
  hangulSyllables : TCodePointRecArray;
  ucaBook : TUCA_DataBook;
  ucaPropBook : PUCA_PropBook;
  propList : TPropListLineRecArray;
  whiteSpaceCodePoints : TCodePointRecArray;
  props : TPropRecArray;
  numericTable : TNumericValueArray;
  decomposition : TDecompositionArray;
  decompositionBook : TDecompositionBook;
  data : TDataLineRecArray;
  //----------------
  lvl3table1 : T3lvlBmp1Table;
  lvl3table2 : T3lvlBmp2Table;
  lvl3table3 : T3lvlBmp3Table;
  //----------------
  s : ansistring;
  i, k, h : Integer;
  p : PDataLineRec;
  r : TDataLineRecArray;
  olvl3table1 : T3lvlOBmp1Table;
  olvl3table2 : T3lvlOBmp2Table;
  olvl3table3 : T3lvlOBmp3Table;
  //----------------
  hs, ls : Word;
  ucaFirstTable   : TucaBmpFirstTable;
  ucaSecondTable  : TucaBmpSecondTable;
  ucaoFirstTable   : TucaoBmpFirstTable;
  ucaoSecondTable  : TucaOBmpSecondTable;
  WL : Integer;
  serializedHeader : TSerializedCollationHeader;
begin
  WriteLn(SUsage+sLineBreak);
  if (ParamCount > 0) then
    dataPath := IncludeTrailingPathDelimiter(ParamStr(1))
  else
    dataPath := ExtractFilePath(ParamStr(0));
  if (ParamCount > 1) then
    outputPath := IncludeTrailingPathDelimiter(ParamStr(2))
  else
    outputPath := dataPath;
  if not DirectoryExists(outputPath) then begin
    WriteLn('Directory not found : ',outputPath);
    if ForceDirectories(outputPath) then begin
      WriteLn('  directory created successfully');
    end else begin
      WriteLn('  fail to create directory.');
      Halt(1);
    end;
  end;
  if not(
       FileExists(dataPath + 'HangulSyllableType.txt') and
       FileExists(dataPath + 'PropList.txt') and
       FileExists(dataPath + 'UnicodeData.txt') and
       FileExists(dataPath + 'allkeys.txt')
     )
  then begin
    WriteLn('File(s) not found : HangulSyllableType.txt or PropList.txt or UnicodeData.txt or allkeys.txt .');
    Halt(1);
  end;

  binaryStreamNE := nil;
  binaryStreamOE := nil;
  binStreamOE := nil;
  binStreamNE := nil;
  tmpStream := nil;
  stream := TMemoryStream.Create();
  try
    binStreamNE := TMemoryStream.Create();
    binStreamOE := TMemoryStream.Create();
    tmpStream := TMemoryStream.Create();
    WriteLn('Load file HangulSyllableType.txt ...', DateTimeToStr(Now));
    stream.LoadFromFile(dataPath + 'HangulSyllableType.txt');
    stream.Position := 0;
    hangulSyllables := nil;
    ParseHangulSyllableTypes(stream,hangulSyllables);
    stream.Clear();

    WriteLn('Load file PropList.txt ...', DateTimeToStr(Now));
    stream.LoadFromFile(dataPath + 'PropList.txt');
    stream.Position := 0;
    propList := nil;
    ParseProps(stream,propList);
    stream.Clear();
    whiteSpaceCodePoints := FindCodePointsByProperty('White_Space',propList);
    writeln('  PropList Length = ',Length(propList));
    writeln('  White_Space Length = ',Length(whiteSpaceCodePoints));
    for i := Low(whiteSpaceCodePoints) to High(whiteSpaceCodePoints) do
      WriteLn('      ',DumpCodePoint(whiteSpaceCodePoints[i]):12,' , IsWhiteSpace = ',IsWhiteSpace(whiteSpaceCodePoints[i].CodePoint,whiteSpaceCodePoints));

    WriteLn('Load file UnicodeData.txt ...', DateTimeToStr(Now));
    stream.LoadFromFile(dataPath + 'UnicodeData.txt');
    stream.Position := 0;
    WriteLn('Parse file ...', DateTimeToStr(Now));
    data := nil;
    props := nil;
    Parse_UnicodeData(stream,props,numericTable,data,decomposition,hangulSyllables,whiteSpaceCodePoints);
    WriteLn('Decomposition building ...');
    MakeDecomposition(decomposition,decompositionBook);

    WriteLn('Load file UCA allkeys.txt ...', DateTimeToStr(Now));
    stream.LoadFromFile(dataPath + 'allkeys.txt');
    stream.Position := 0;
    ParseUCAFile(stream,ucaBook);
 { $IFDEF UCA_TEST}
    k := 0;  WL := 0; ;
    for i := 0 to Length(ucaBook.Lines) - 1 do begin
      h := GetPropID(ucaBook.Lines[i].CodePoints[0],data);
      if (h <> -1) and
         ({props[h].HangulSyllable or} (props[h].DecompositionID <> -1))
      then begin
        Inc(k);
        ucaBook.Lines[i].Stored := False;
      end else begin
        ucaBook.Lines[i].Stored := True;
        if Length(ucaBook.Lines[i].Weights) > WL then
          WL := Length(ucaBook.Lines[i].Weights);
      end;
    end;
    WriteLn(
      'UCA, Version = ',ucaBook.Version,'; entries count = ',Length(ucaBook.Lines),' ; Hangul # = ',k,
      'Max Weights Length = ',WL
    );
{ $ENDIF UCA_TEST}
    WriteLn('Construct UCA Property Book ...');
    ucaPropBook := nil;
    MakeUCA_Props(@ucaBook,ucaPropBook);
{$IFDEF UCA_TEST}
    uca_CheckProp_1(ucaBook,ucaPropBook);
    uca_CheckProp_x(ucaBook,ucaPropBook);
{$ENDIF UCA_TEST}
    WriteLn('Construct UCA BMP tables ...');
    MakeUCA_BmpTables(ucaFirstTable,ucaSecondTable,ucaPropBook);
    WriteLn('  UCA BMP Second Table Length = ',Length(ucaSecondTable));
{$IFDEF UCA_TEST}
    uca_CheckProp_1y(ucaBook,ucaPropBook,@ucaFirstTable,@ucaSecondTable);
{$ENDIF UCA_TEST}

    WriteLn('Construct UCA OBMP tables ...');
    MakeUCA_OBmpTables(ucaoFirstTable,ucaoSecondTable,ucaPropBook);
    WriteLn('  UCA OBMP Second Table Length = ',Length(ucaoSecondTable));
{$IFDEF UCA_TEST}
    uca_CheckProp_2y(ucaBook,ucaPropBook,@ucaoFirstTable,@ucaoSecondTable);
{$ENDIF UCA_TEST}
    binaryStreamNE := TMemoryStream.Create();
    binaryStreamOE := TMemoryStream.Create();
    WriteLn('Generate UCA Props tables ...');
    binStreamNE.Clear();
    binStreamOE.Clear();
    GenerateLicenceText(binStreamNE);
      GenerateLicenceText(binStreamOE);
    GenerateUCA_PropTable(binStreamNE,ucaPropBook,ENDIAN_NATIVE);
      GenerateUCA_PropTable(binStreamOE,ucaPropBook,ENDIAN_NON_NATIVE);
    WriteLn('Generate UCA BMP tables ...');
    stream.Clear();
    GenerateLicenceText(stream);
    GenerateUCA_Head(stream,@ucaBook,ucaPropBook);
    GenerateUCA_BmpTables(stream,binStreamNE,binStreamOE,ucaFirstTable,ucaSecondTable);
    WriteLn('Generate UCA OBMP tables ...');
    GenerateUCA_OBmpTables(stream,binStreamNE,binStreamOE,ucaoFirstTable,ucaoSecondTable);
    stream.SaveToFile(outputPath + 'ucadata.inc');
    s := outputPath + 'ucadata.inc';
    binStreamNE.SaveToFile(GenerateEndianIncludeFileName(s,ENDIAN_NATIVE));
      binStreamOE.SaveToFile(GenerateEndianIncludeFileName(s,ENDIAN_NON_NATIVE));
    binStreamNE.Clear();
    binStreamOE.Clear();
// Binary DUCET
    FillChar(serializedHeader,SizeOf(TSerializedCollationHeader),0);
    serializedHeader.Version := ucaBook.Version;
    serializedHeader.CollationName := 'DUCET';//'Default Unicode Collation Element Table (DUCET)';
    serializedHeader.VariableWeight := Ord(ucaBook.VariableWeight);
    SetBit(serializedHeader.Backwards,0,ucaBook.Backwards[0]);
    SetBit(serializedHeader.Backwards,1,ucaBook.Backwards[1]);
    SetBit(serializedHeader.Backwards,2,ucaBook.Backwards[2]);
    SetBit(serializedHeader.Backwards,3,ucaBook.Backwards[3]);
    serializedHeader.BMP_Table1Length := Length(ucaFirstTable);
    serializedHeader.BMP_Table2Length := Length(TucaBmpSecondTableItem) *
                                         (Length(ucaSecondTable) * SizeOf(UInt24));
    serializedHeader.OBMP_Table1Length := Length(ucaoFirstTable) * SizeOf(Word);
    serializedHeader.OBMP_Table2Length := Length(TucaOBmpSecondTableItem) *
                                         (Length(ucaoSecondTable) * SizeOf(UInt24));
    serializedHeader.PropCount := ucaPropBook^.ItemSize;
    serializedHeader.VariableLowLimit := ucaPropBook^.VariableLowLimit;
    serializedHeader.VariableHighLimit := ucaPropBook^.VariableHighLimit;
    binaryStreamNE.Write(serializedHeader,SizeOf(serializedHeader));
    ReverseRecordBytes(serializedHeader);
    binaryStreamOE.Write(serializedHeader,SizeOf(serializedHeader));
      GenerateBinaryUCA_BmpTables(binaryStreamNE,binaryStreamOE,ucaFirstTable,ucaSecondTable);
      GenerateBinaryUCA_OBmpTables(binaryStreamNE,binaryStreamOE,ucaoFirstTable,ucaoSecondTable);
      GenerateBinaryUCA_PropTable(binaryStreamNE,binaryStreamOE,ucaPropBook);
    binaryStreamNE.SaveToFile(
      outputPath + Format('collation_ducet_%s.bco',[ENDIAN_SUFFIX[ENDIAN_NATIVE]])
    );
    binaryStreamOE.SaveToFile(
      outputPath + Format('collation_ducet_%s.bco',[ENDIAN_SUFFIX[ENDIAN_NON_NATIVE]])
    );
// Binary DUCET - END


    stream.Clear();
    GenerateLicenceText(stream);
    WriteLn('File parsed ...', DateTimeToStr(Now));
    WriteLn('  Props Len = ',Length(props));
    WriteLn('  Data Len = ',Length(data));

    {WriteLn('BMP Tables building ...', DateTimeToStr(Now));
    MakeBmpTables(firstTable,secondTable,props,data);
    WriteLn('   First Table length = ',Length(firstTable));
    WriteLn('   Second Table length = ',Length(secondTable));}

    WriteLn('BMP Tables building ...', DateTimeToStr(Now));
    MakeBmpTables3Levels(lvl3table1,lvl3table2,lvl3table3,data);
    WriteLn(' 3 Levels Tables :');
    WriteLn('       Len 1 = ',Length(lvl3table1));
    WriteLn('       Len 2 = ',Length(lvl3table2));
    WriteLn('       Len 3 = ',Length(lvl3table3));
    for i := 0 to 255 do begin
      for k := 0 to 15 do begin
        for h := 0 to 15 do begin
          if lvl3table3[lvl3table2[lvl3table1[i]][k]][h] <>
             GetPropID(256*i + 16*k +h,data)
          then begin
            writeln('3 levels errors, i=',i,'; k=',k,'; h=',h);
          end;
        end;
      end;
    end;

    binStreamNE.Clear();
    binStreamOE.Clear();
    WriteLn('Source generation ...', DateTimeToStr(Now));
    WriteLn('BMP Tables sources ...', DateTimeToStr(Now));
      Generate3lvlBmpTables(stream,lvl3table1,lvl3table2,lvl3table3);
    WriteLn('Properties Table sources ...', DateTimeToStr(Now));
      tmpStream.Clear();
      GenerateNumericTable(tmpStream,numericTable,True);
      tmpStream.SaveToFile(outputPath + 'unicodenumtable.pas');
      tmpStream.Clear();
      GeneratePropTable(binStreamNE,props,ENDIAN_NATIVE);
      GeneratePropTable(binStreamOE,props,ENDIAN_NON_NATIVE);
//-------------------------------------------

   r := Compress(data);

//-------------------
    WriteLn('OBMP Tables building ...', DateTimeToStr(Now));
    MakeOBmpTables3Levels(olvl3table1,olvl3table2,olvl3table3,r);
    WriteLn(' 3 Levels Tables :');
    WriteLn('       Len 1 = ',Length(olvl3table1));
    WriteLn('       Len 2 = ',Length(olvl3table2));
    WriteLn('       Len 3 = ',Length(olvl3table3));
    for i := 0 to 1023 do begin
      for k := 0 to 31 do begin
        for h := 0 to 31 do begin
          if olvl3table3[olvl3table2[olvl3table1[i]][k]][h] <>
             GetPropID(ToUCS4(HIGH_SURROGATE_BEGIN + i,LOW_SURROGATE_BEGIN + (k*32) + h),data)
          then begin
            writeln('3, OBMP levels errors, i=',i,'; k=',k,'; h=',h);
          end;
        end;
      end;
    end;
    WriteLn('OBMP Tables sources ...', DateTimeToStr(Now));
    Generate3lvlOBmpTables(stream,olvl3table1,olvl3table2,olvl3table3);

  //---------------------
    WriteLn('Decomposition  Table sources ...', DateTimeToStr(Now));
    GenerateDecompositionBookTable(binStreamNE,decompositionBook,ENDIAN_NATIVE);
    GenerateDecompositionBookTable(binStreamOE,decompositionBook,ENDIAN_NON_NATIVE);
    stream.SaveToFile(outputPath + 'unicodedata.inc');
    binStreamNE.SaveToFile(outputPath + 'unicodedata_'+ENDIAN_SUFFIX[ENDIAN_NATIVE]+'.inc');
    binStreamOE.SaveToFile(outputPath + 'unicodedata_'+ENDIAN_SUFFIX[ENDIAN_NON_NATIVE]+'.inc');
    binStreamNE.Clear();
    binStreamOE.Clear();


    h := -1;
    for i := Low(data) to High(data) do
      if (data[i].CodePoint > $FFFF) then begin
        h := i;
        Break;
      end;
    stream.Clear();
    for i := h to High(data) do begin
      p := @data[i];
      if (p^.LineType = 0) then begin
        FromUCS4(p^.CodePoint,hs,ls);
        //k := GetProp(hs,ls,props,ofirstTable,osecondTable)^.PropID;
        k := GetProp(
               (hs-HIGH_SURROGATE_BEGIN),(ls-LOW_SURROGATE_BEGIN),
               props,olvl3table1,olvl3table2,olvl3table3
             )^.PropID;
        if (p^.PropID <> k) then begin
          s := Format('#%d-%d  #%d',[p^.CodePoint,p^.PropID,k]) + sLineBreak;
          stream.Write(s[1],Length(s));
        end;
      end else begin
        for h := p^.StartCodePoint to p^.EndCodePoint do begin
          FromUCS4(h,hs,ls);
          //k := GetProp(hs,ls,props,ofirstTable,osecondTable)^.PropID;
          k := GetProp(
                 (hs-HIGH_SURROGATE_BEGIN),(ls-LOW_SURROGATE_BEGIN),
                 props,olvl3table1,olvl3table2,olvl3table3
               )^.PropID;
          if (p^.PropID <> k) then begin
            s := Format('##%d;%d-%d  #%d',[p^.StartCodePoint,p^.EndCodePoint,p^.PropID,k]) + sLineBreak;
            stream.Write(s[1],Length(s));
            Break
          end;
        end;
      end;
    end;
    stream.SaveToFile(outputPath + 'diff-obmp.txt');

    stream.Clear();
    for i := Low(data) to High(data) do begin
      p := @data[i];
      if (p^.LineType = 0) then begin
        k := GetPropID(p^.CodePoint,r);
        if (p^.PropID <> k) then begin
          s := Format('#%d-%d  #%d',[p^.CodePoint,p^.PropID,k]) + sLineBreak;
          stream.Write(s[1],Length(s));
        end;
      end else begin
        for h := p^.StartCodePoint to p^.EndCodePoint do begin
          k := GetPropID(h,r);
          if (p^.PropID <> k) then begin
            s := Format('##%d;%d-%d  #%d',[p^.StartCodePoint,p^.EndCodePoint,p^.PropID,k]) + sLineBreak;
            stream.Write(s[1],Length(s));
            Break
          end;
        end;
      end;
    end;
    stream.SaveToFile(outputPath + 'diff.txt');
    stream.Clear();
    for i := Low(r) to High(r) do begin
      p := @r[i];
      if (p^.LineType = 0) then begin
        k := GetPropID(p^.CodePoint,data);
        if (p^.PropID <> k) then begin
          s := Format('#%d-%d  #%d',[p^.CodePoint,p^.PropID,k]) + sLineBreak;
          stream.Write(s[1],Length(s));
        end;
      end else begin
        for h := p^.StartCodePoint to p^.EndCodePoint do begin
          k := GetPropID(h,r);
          if (p^.PropID <> k) then begin
            s := Format('##%d;%d-%d  #%d',[p^.StartCodePoint,p^.EndCodePoint,p^.PropID,k]) + sLineBreak;
            stream.Write(s[1],Length(s));
            Break
          end;
        end;
      end;
    end;
    stream.SaveToFile(outputPath + 'diff2.txt');
  finally
    binaryStreamOE.Free();
    binaryStreamNE.Free();
    tmpStream.Free();
    binStreamOE.Free();
    binStreamNE.Free();
    stream.Free();
  end;
end.

