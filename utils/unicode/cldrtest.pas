{   CLDR collation Algorithm test routines.

    Copyright (c) 2013 by Inoussa OUEDRAOGO

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

unit cldrtest;

{$mode objfpc}{$H+}
{$typedaddress on}
{$warn 4056 off}  //Conversion between ordinals and pointers is not portable

interface

uses
  Classes, SysUtils,
  helper, cldrhelper, unicodedata;

  function ToAnsiChars(const AValue : array of TUnicodeCodePoint) : string;
  function DumpSequenceAnsi(const ASequence : TOrderedCharacters) : string;
  function DumpWeigth(const AItem : TUCA_WeightRec) : string;overload;
  function DumpWeigth(const AItems : array of TUCA_WeightRec) : string;overload;
  function DumpLine(ALine : TUCA_LineRec) : string;
  function DumpLines(ALines : TUCA_LineRecArray) : string;
  function CodePointToArray(const ACodePoint : TUnicodeCodePoint) : TUnicodeCodePointArray;overload;
  function CodePointToArray(const ACodePoints : array of TUnicodeCodePoint) : TUnicodeCodePointArray;overload;
  function ToWeight(const APrimary, ASecondary, ATertiary : Cardinal) : TUCA_WeightRecArray;overload;
  function ToWeight(const AWeigths : array of Cardinal) : TUCA_WeightRecArray;overload;

  procedure exec_tests();

  procedure test1();
  procedure test2();
  procedure test3();
  procedure test4();
  procedure test5();
  procedure test6();
  procedure test7();
  procedure test8();
  procedure test9();
  procedure test10();
  procedure test11();
  procedure test12();
  procedure test13();
  procedure test14();
  procedure test15();

implementation

procedure exec_tests();
begin
  WriteLn('***************************** TEST 1 ******************');
  test1();
  WriteLn('***************************** TEST 2 ******************');
  test2();
  WriteLn('***************************** TEST 3 ******************');
  test3();
  WriteLn('***************************** TEST 4 ******************');
  test4();
  WriteLn('***************************** TEST 5 ******************');
  test5();
  WriteLn('***************************** TEST 6 ******************');
  test6();
  WriteLn('***************************** TEST 7 ******************');
  test7();
  WriteLn('***************************** TEST 8 ******************');
  test8();
  WriteLn('***************************** TEST 9 ******************');
  test9();
  WriteLn('***************************** TEST 10 ******************');
  test10();
  WriteLn('***************************** TEST 11 ******************');
  test11();
  WriteLn('***************************** TEST 12 ******************');
  test12();
  WriteLn('***************************** TEST 13 ******************');
  test13();
  WriteLn('***************************** TEST 14 ******************');
  test14();
  WriteLn('***************************** TEST 15 ******************');
  test15();
end;

function ToAnsiChars(const AValue : array of TUnicodeCodePoint) : string;
var
  i : Integer;
  s : string;
begin
  Result := '';
  for i := Low(AValue) to High(AValue) do begin
    if (AValue[i] <= 127) then
      Result := Result + AnsiChar(AValue[i])
    else
      begin
        s := Format('%x',[AValue[i]]);
        if (Length(s) < 4) then
          s := StringOfChar('0',4-Length(s)) + s;
        Result := Result + '$' + s;
      end;
  end;
end;

function DumpSequenceAnsi(const ASequence : TOrderedCharacters) : string;
var
  i : Integer;
  s : string;
  p : PReorderUnit;
begin
  s := '';
  if (ASequence.ActualLength < 1) then
    exit;
  p := @ASequence.Data[0];
  i := 0;
  while (i < ASequence.ActualLength) do begin
    if (p^.WeigthKind <> TReorderWeigthKind.Deletion) then
      Break;
    WriteStr(s,s,  ' ',ToAnsiChars(p^.Characters),'- ');
    Inc(p);
    Inc(i);
  end;

  if (i < ASequence.ActualLength) then begin
    s := s + '   ' + ToAnsiChars(p^.Characters) + ' ';
    Inc(i);
    Inc(p);
    for i := i to ASequence.ActualLength - 1 do begin
      //WriteStr(s,s,AnsiChar(p^.Characters[0]),' <',(1+Ord(p^.WeigthKind)),' ');
      WriteStr(s,s,'<',(1+Ord(p^.WeigthKind)),' ',ToAnsiChars(p^.Characters),' ');
      Inc(p);
    end;
  end;
  Result := s;
end;

function DumpWeigth(const AItem : TUCA_WeightRec) : string;overload;
var
  r : string;
begin
  r := '[';
  if AItem.Variable then
    r := r + '*'
  else
    r := r + '.';
  r := r + Format('%x.%x.%x',[AItem.Weights[0],AItem.Weights[1],AItem.Weights[2]]);
  r := r + ']';
  Result := r;
end;

function DumpWeigth(const AItems : array of TUCA_WeightRec) : string;
var
  r : string;
  i : Integer;
begin
  r := '';
  for i := 0 to Length(AItems) - 1 do
    r := r + ' ' +DumpWeigth(AItems[i]);
  Result := Trim(r);
end;

function DumpKey(const AItem : TUCASortKey) : string;
var
  i : Integer;
  r : string;
begin
  r := '';
  for i := Low(AItem) to High(AItem) do
    r := Trim(r) + ' ' + Format('%4x',[AItem[i]]);
  Result := r;
end;

function DumpLine(ALine : TUCA_LineRec) : string;
var
  i : Integer;
  r : string;
  ctxItem : TUCA_LineContextItemRec;
begin
  r := '';
  if ALine.HasContext() then begin
    r := r + '*';
    for i := 0 to Length(ALine.Context.Data) - 1 do begin
      ctxItem := ALine.Context.Data[i];
      r := r + sLineBreak +
           '        ' + ToAnsiChars(ctxItem.CodePoints) + ' => ' + DumpWeigth(ctxItem.Weights);
    end;
    r := r + sLineBreak + '    ';
  end;
  if (Length(ALine.Weights) = 0) then begin
    r := r + '[]';
  end else begin
    for i := Low(ALine.Weights) to High(ALine.Weights) do
      r := r + DumpWeigth(ALine.Weights[i]);
  end;
  Result := Format('%s %s',[ToAnsiChars(ALine.CodePoints),r]);
end;

function DumpLines(ALines : TUCA_LineRecArray) : string;
var
  i : Integer;
  r : string;
begin
  r := '';
  for i := Low(ALines) to High(ALines) do
    r := r + '    ' +  DumpLine(ALines[i]) + sLineBreak;
  Result := r;
end;

function CodePointToArray(const ACodePoint : TUnicodeCodePoint) : TUnicodeCodePointArray;overload;
begin
  SetLength(Result,1);
  Result[0] := ACodePoint;
end;

function CodePointToArray(const ACodePoints : array of TUnicodeCodePoint) : TUnicodeCodePointArray;overload;
var
  i : Integer;
begin
  SetLength(Result,Length(ACodePoints));
  for i := 0 to length(ACodePoints) - 1 do
    Result[i] := ACodePoints[i];
end;

function ToWeight(const APrimary, ASecondary, ATertiary : Cardinal) : TUCA_WeightRecArray;overload;
begin
  SetLength(Result,1);
  Result[0].Weights[0] := APrimary;
  Result[0].Weights[1] := ASecondary;
  Result[0].Weights[2] := ATertiary;
  Result[0].Weights[3] := 0;
end;

function ToWeight(const AWeigths : array of Cardinal) : TUCA_WeightRecArray;overload;
var
  i, k, c : Integer;
begin
  c := Length(AWeigths);
  SetLength(Result,(c div 3));
  k := 0;
  for i := 0 to (c div 3) - 1 do begin
    Result[i].Weights[0] := AWeigths[k+0];
    Result[i].Weights[1] := AWeigths[k+1];
    Result[i].Weights[2] := AWeigths[k+2];
    Result[i].Weights[3] := 0;
    k := k + 3;
  end;
end;

procedure constructPropBook(
  var ABook : unicodedata.TUCA_DataBook;
  const AFirstTable  : TucaBmpFirstTable;
  const ASecondTable  : TucaBmpSecondTable;
  const AOFirstTable   : TucaOBmpFirstTable;
  const AOSecondTable  : TucaOBmpSecondTable;
  const AInitDataBook : helper.TUCA_DataBook;
  const AInitPropBook : helper.PUCA_PropBook
);
var
  c, i, k, ci : Integer;
begin
  c := Length(AFirstTable);
  if (c > 0) then begin
    ABook.BMP_Table1 := AllocMem(c);
    Move(AFirstTable[0],ABook.BMP_Table1^,c);
  end;
  c := Length(ASecondTable);
  if (c > 0) then begin
    ABook.BMP_Table2 := AllocMem(c*SizeOf(UInt24)*256);
    for i := 0 to c - 1 do begin
      for k := 0 to 255 do
        ABook.BMP_Table2[(i*256)+k] := ASecondTable[i][k];
    end;
  end;

  c := Length(AOFirstTable);
  if (c > 0) then begin
    ABook.OBMP_Table1 := AllocMem(c*SizeOf(Word));
    Move(AOFirstTable[0],ABook.OBMP_Table1^,(c*SizeOf(Word)));
  end;
  c := Length(AOSecondTable);
  if (c > 0) then begin
    ci := Length(AOSecondTable[0]);
    ABook.OBMP_Table2 := AllocMem(c*SizeOf(UInt24)*ci);
    for i := 0 to c - 1 do begin
      for k := 0 to ci - 1 do
        ABook.OBMP_Table2[(i*ci)+k] := AOSecondTable[i][k];
    end;
  end;

  ABook.Version := AInitDataBook.Version;
  ABook.VariableWeight := unicodedata.TUCA_VariableKind(Ord(AInitDataBook.VariableWeight));
  ABook.Backwards := AInitDataBook.Backwards;
  ABook.PropCount := AInitPropBook^.ItemSize;
  ABook.Props := Pointer(AInitPropBook^.Items);
  ABook.VariableLowLimit := AInitPropBook^.VariableLowLimit;
  ABook.VariableHighLimit := AInitPropBook^.VariableHighLimit;
end;

procedure ConstructUnicodeBook(
  const AWeitghs       : TUCA_LineRecArray;
  const AVersion       : string;
  const ACollationName : string;
  const ABase          : unicodedata.PUCA_DataBook;
  var   AUnicodeBook   : unicodedata.TUCA_DataBook
);
var
  dataBook : helper.TUCA_DataBook;
  propBook : helper.PUCA_PropBook;
  firstTable   : TucaBmpFirstTable;
  secondTable  : TucaBmpSecondTable;
  ofirstTable   : TucaOBmpFirstTable;
  osecondTable  : TucaOBmpSecondTable;
  i : Integer;
begin
  FillByte(dataBook,SizeOf(dataBook),0);
  dataBook.Version := AVersion;
  SetLength(dataBook.Lines,Length(AWeitghs));
  for i := 0 to Length(AWeitghs)-1 do begin
    dataBook.Lines[i] := AWeitghs[i];
    dataBook.Lines[i].Stored := True;
  end;
  MakeUCA_Props(@dataBook,propBook);
  MakeUCA_BmpTables(firstTable,secondTable,propBook);
  MakeUCA_OBmpTables(ofirstTable,osecondTable,propBook);
  FillByte(AUnicodeBook,SizeOf(AUnicodeBook),0);
  constructPropBook(
    AUnicodeBook,firstTable,secondTable,ofirstTable,osecondTable,
    dataBook,propBook
  );
  AUnicodeBook.CollationName := ACollationName;
  AUnicodeBook.Base := ABase;
end;

procedure CheckEqual(A,B : UnicodeString; ACollation : unicodedata.PUCA_DataBook);
var
  keyA, keyB : TUCASortKey;
  s : string;
begin
  keyA := ComputeSortKey(A,ACollation);
  keyB := ComputeSortKey(B,ACollation);
  if (CompareSortKey(keyA,keyB) <> 0) then begin
    s := Format('  KeyA=%s%s  KeyB=%s',[DumpKey(keyA),sLineBreak,DumpKey(keyB)]);
    s := Format('"%s" <>= "%s" %s%s',[A,B,sLineBreak,s]);
    raise Exception.Create(s);
  end;
end;

procedure CheckNotEqual(A,B : UnicodeString; ACollation : unicodedata.PUCA_DataBook);
var
  keyA, keyB : TUCASortKey;
  s : string;
begin
  keyA := ComputeSortKey(A,ACollation);
  keyB := ComputeSortKey(B,ACollation);
  if (CompareSortKey(keyA,keyB) = 0) then begin
    s := Format('  KeyA=%s%s  KeyB=%s',[DumpKey(keyA),sLineBreak,DumpKey(keyB)]);
    s := Format('"%s" = "%s" %s%s',[A,B,sLineBreak,s]);
    raise Exception.Create(s);
  end;
end;

procedure CheckInf(A,B : UnicodeString; ACollation : unicodedata.PUCA_DataBook);
var
  keyA, keyB : TUCASortKey;
begin
  keyA := ComputeSortKey(A,ACollation);
  keyB := ComputeSortKey(B,ACollation);
  if (CompareSortKey(keyA,keyB) >= 0) then
    raise Exception.CreateFmt('"%s" >= "%s" !',[A,B]);
end;

procedure CheckInf(AStrings : array of UnicodeString; ACollation : unicodedata.PUCA_DataBook);
var
  c, i : Integer;
  keyA, keyB : TUCASortKey;
  s : string;
begin
  c := Length(AStrings);
  if (c < 2) then
    exit;
  keyA := ComputeSortKey(AStrings[0],ACollation);
  for i := 1 to c - 1 do begin
    keyB := ComputeSortKey(AStrings[i],ACollation);
    if (CompareSortKey(keyA,keyB) >= 0) then begin
      s := Format('  KeyA=%s%s  KeyB=%s',[DumpKey(keyA),sLineBreak,DumpKey(keyB)]);
      s := Format('"%s" >= "%s" %s%s',[AStrings[i-1],AStrings[i],sLineBreak,s]);
      raise Exception.Create(s);
    end;
    keyA := keyB;
  end;
end;

procedure test1_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('f'));
    p^.Weights := ToWeight($1684,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('g'));
    p^.Weights := ToWeight($1691,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('h'));
    p^.Weights := ToWeight($16B4,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('i'));
    p^.Weights := ToWeight($16CD,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('j'));
    p^.Weights := ToWeight($16E6,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('l'));
    p^.Weights := ToWeight($1711,$0020,$0002);
end;

procedure test1();
var
  sequence : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  i : Integer;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test1_prepareWeigth(wfirst);
  sequence := TOrderedCharacters.Create();
  sequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  sequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  sequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,3));
  sequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,4));
  sequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,5));
  sequence.Append(TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,6));
  sequence.Append(TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,7));
  sequence.Append(TReorderUnit.From(Ord('h'),TReorderWeigthKind.Primary,8));
  sequence.Append(TReorderUnit.From(Ord('i'),TReorderWeigthKind.Primary,9));
  sequence.Append(TReorderUnit.From(Ord('j'),TReorderWeigthKind.Primary,10));
  sequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  sequence.Append(TReorderUnit.From(Ord('l'),TReorderWeigthKind.Primary,12));
  for i := 0 to sequence.ActualLength - 1 do
    sequence.Data[i].Changed := False;
  WriteLn('Initial = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wfirst,'test1','first',nil,unicodeBook1);
  CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook1);

  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','g'{*},'b','c','d','e','f','h','i','j','k','l'],@unicodeBook2);


  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('h'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','h'{*},'k'{*},'g'{*},'b','c','d','e','f','i','j','l'],@unicodeBook2);

  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('h');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence));
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','h'{*},'g'{*},'k'{*},'b','c','d','e','f','i','j','l'],@unicodeBook2);
end;

procedure test2_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,11);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('a'),Ord(')')]);
    p^.Weights := ToWeight($15EF,$0020,$0006); //15EF.0020.0006.24D0
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('A'));
    p^.Weights := ToWeight($15EF,$0020,$0008); //15EF.0020.0008.0041
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('A'),Ord(')')]);
    p^.Weights := ToWeight($15EF,$0020,$000C);  //15EF.0020.000C
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('`')]);
    p^.Weights := ToWeight([$15EF,$0020,$0002,  $0000,$0035,$0002]); //[.15EF.0020.0002.0061][.0000.0035.0002.0300]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('A'),Ord('`')]);
    p^.Weights := ToWeight([$15EF,$0020,$0008,  $0000,$0035,$0002]);  //[.15EF.0020.0008.0041][.0000.0035.0002.0300]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('e')]);
    p^.Weights := ToWeight([$15F0,$0020,$0002]);  //[.15EF.0020.0004.00E6][.0000.0139.0004.00E6][.164C.0020.0004.00E6]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord(UpCase('a')),Ord(UpCase('e'))]);
    p^.Weights := ToWeight([$15F0,$0020,$0006]);//[.15EF.0020.000A.00C6][.0000.0139.0004.00C6][.164C.0020.000A.00C6]
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('b'),Ord(')')]);
    p^.Weights := ToWeight($1605,$0020,$0006);  //.1605.0020.0006.24D1
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('B'));
    p^.Weights := ToWeight($1605,$0020,$0008); //1605.0020.0008.0042
end;

procedure test2();
var
  sequenceClean, sequence : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  i : Integer;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test2_prepareWeigth(wfirst);
  sequenceClean := TOrderedCharacters.Create();
  sequenceClean.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  sequenceClean.Append(TReorderUnit.From([Ord('('),Ord('a'),Ord(')')],TReorderWeigthKind.Tertiary,2));
  sequenceClean.Append(TReorderUnit.From(Ord('A'),TReorderWeigthKind.Tertiary,3));
  sequenceClean.Append(TReorderUnit.From([Ord('('),Ord('A'),Ord(')')],TReorderWeigthKind.Tertiary,4));

  //sequenceClean.Append(TReorderUnit.From(Ord('à'),TReorderWeigthKind.Secondary,0));
  sequenceClean.Append(TReorderUnit.From([Ord('a'),Ord('`')],TReorderWeigthKind.Secondary,5));
  //sequenceClean.Append(TReorderUnit.From(Ord(UpCase('à')),TReorderWeigthKind.Tertiary,0));
  sequenceClean.Append(TReorderUnit.From([Ord('A'),Ord('`')],TReorderWeigthKind.Tertiary,6));

  sequenceClean.Append(TReorderUnit.From([Ord('a'),Ord('e')],TReorderWeigthKind.Primary,7));
  sequenceClean.Append(TReorderUnit.From([Ord(UpCase('a')),Ord(UpCase('e'))],TReorderWeigthKind.Tertiary,8));

  sequenceClean.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,9));
  sequenceClean.Append(TReorderUnit.From([Ord('('),Ord('b'),Ord(')')],TReorderWeigthKind.Tertiary,10));
  sequenceClean.Append(TReorderUnit.From(Ord('B'),TReorderWeigthKind.Tertiary,11));
  for i := 0 to sequenceClean.ActualLength - 1 do
    sequenceClean.Data[i].Changed := False;

  WriteLn('Initial = ',sLineBreak,'  ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wfirst,'test1','first',nil,unicodeBook1);
  CheckInf(['a','(a)','A','(A)',  'a`','A`',  'ae','AE',  'b','(b)','B'],@unicodeBook1);

  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','(a)','A','(A)',  'a`','A`', 'g'{*},  'ae','AE',  'b','(b)','B'],@unicodeBook2);
  CheckInf(['gg','ae'],@unicodeBook2);
  CheckInf(['gb','ae'],@unicodeBook2);
  //CheckInf(['aeae','AE'],@unicodeBook2);

  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','(a)','A','(A)', 'g'{*},  'a`','A`',  'ae','AE',  'b','(b)','B'],@unicodeBook2);
  CheckInf(['(A)a','ga'],@unicodeBook2);
  CheckInf(['g','ae'],@unicodeBook2);

  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a', 'g'{*},'(a)','A','(A)',  'a`','A`',  'ae','AE',  'b','(b)','B'],@unicodeBook2);
  CheckInf(['aa','ga'],@unicodeBook2);
  CheckInf(['ga','(a)a'],@unicodeBook2);

  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('a');
  statement.Reset[1] := Ord('`');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #4 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
  CheckInf(['a','(a)','A','(A)',  'a`', 'g'{*},'A`',  'ae','AE',  'b','(b)','B'],@unicodeBook2);
  CheckInf(['a`a','ga'],@unicodeBook2);
  CheckInf(['ga','ae'],@unicodeBook2);
end;

//------------------------------------------------------

procedure test3_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('f'));
    p^.Weights := ToWeight($1684,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('g'));
    p^.Weights := ToWeight($1691,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('h'));
    p^.Weights := ToWeight($16B4,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('i'));
    p^.Weights := ToWeight($16CD,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('j'));
    p^.Weights := ToWeight($16E6,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('l'));
    p^.Weights := ToWeight($1711,$0020,$0002);
end;

procedure PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From(Ord('h'),TReorderWeigthKind.Primary,8));
  ASequence.Append(TReorderUnit.From(Ord('i'),TReorderWeigthKind.Primary,9));
  ASequence.Append(TReorderUnit.From(Ord('j'),TReorderWeigthKind.Primary,10));
  ASequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  ASequence.Append(TReorderUnit.From(Ord('l'),TReorderWeigthKind.Primary,12));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test3();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  i : Integer;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
  keyA, keyB : TUCASortKey;
  us : UnicodeString;
begin //'a','b','c','d','e','f','g','h','i','j','k','l'
  statement.Clear();
  test3_prepareWeigth(wfirst);
  PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test3','first',nil,unicodeBook1);

  us := 'a';
  keyA := ComputeSortKey(us,@unicodeBook1);
  for i := Ord('b') to Ord('l') do begin
    us := unicodeChar(i);
    keyB := ComputeSortKey(us,@unicodeBook1);
    if (CompareSortKey(keyA,keyB) >= 0) then
      raise Exception.CreateFmt('"%s" >= "%s" !',[AnsiChar(i-1),AnsiChar(i)]);
    keyA := keyB;
  end;

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('b');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test3','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','g'{*}, 'c','d','e','f','h','i','j','k','l'],@unicodeBook2);
    CheckInf(['bb','g'{*}],@unicodeBook2);
    CheckInf(['bc','g'{*}],@unicodeBook2);
    CheckInf(['bc','gg'{*}],@unicodeBook2);
    CheckInf(['bg','bc'{*}],@unicodeBook2);
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('c'),Ord('h')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test3','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','ch'{*},'d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckInf(['ca','ch'{*}],@unicodeBook2);
    CheckInf(['cc','ch'{*}],@unicodeBook2);
    CheckInf(['cd','ch'{*}],@unicodeBook2);
    CheckInf(['ce','ch'{*}],@unicodeBook2);
    CheckInf(['cf','ch'{*}],@unicodeBook2);
    CheckInf(['ci','ch'{*}],@unicodeBook2);
    CheckInf(['cj','ch'{*}],@unicodeBook2);
    CheckInf(['ck','ch'{*}],@unicodeBook2);
    CheckInf(['cl','ch'{*}],@unicodeBook2);
    CheckInf(['ac','ach'{*}],@unicodeBook2);
    CheckInf(['aci','achat'{*}],@unicodeBook2);
    WriteLn('    -- test 2 - ok');

  // --- test 3
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test3','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','k'{*},'d','e','f','g','h','i','j','l'],@unicodeBook2);
    CheckInf(['a','b','c'{*},'d','e','f','g','h','i','j','l'],@unicodeBook2);
    CheckEqual('c','k',@unicodeBook2);
    CheckEqual('cc','kk',@unicodeBook2);
    CheckEqual('ck','kc',@unicodeBook2);
    CheckEqual('kc','kk',@unicodeBook2);
    CheckEqual('cckkc','kckcc',@unicodeBook2);

    CheckInf(['acb','akc'{*}],@unicodeBook2);
    WriteLn('    -- test 3 - ok');

  // --- test 4
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('c')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('c'),Ord('h')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test3','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','c'{*},'ch'{*},'b','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckInf(['ca','ch'{*}],@unicodeBook2);
    CheckInf(['cc','ch'{*}],@unicodeBook2);
    CheckInf(['cd','ch'{*}],@unicodeBook2);
    CheckInf(['ce','ch'{*}],@unicodeBook2);
    CheckInf(['cf','ch'{*}],@unicodeBook2);
    CheckInf(['ci','ch'{*}],@unicodeBook2);
    CheckInf(['cj','ch'{*}],@unicodeBook2);
    CheckInf(['ck','ch'{*}],@unicodeBook2);
    CheckInf(['cl','ch'{*}],@unicodeBook2);
    CheckInf(['ac','ach'{*}],@unicodeBook2);
    CheckInf(['aci','achat'{*}],@unicodeBook2);
    WriteLn('    -- test 4 - ok');
end;


//------------------------------------------------------

procedure test4_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c')); {*}
    p^.Weights := ToWeight($1606,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('f'));
    p^.Weights := ToWeight($1684,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('g'));
    p^.Weights := ToWeight($1691,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('h'));
    p^.Weights := ToWeight($16B4,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('i'));
    p^.Weights := ToWeight($16CD,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('j'));
    p^.Weights := ToWeight($16E6,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('l'));
    p^.Weights := ToWeight($1711,$0020,$0002);
end;

procedure test4();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  i : Integer;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
  keyA, keyB : TUCASortKey;
  us : UnicodeString;
begin
  statement.Clear();
  test4_prepareWeigth(wfirst);
  PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test4','first',nil,unicodeBook1);

  us := 'a';
  keyA := ComputeSortKey(us,@unicodeBook1);
  for i := Ord('b') to Ord('l') do begin
    us := unicodeChar(i);
    keyB := ComputeSortKey(us,@unicodeBook1);
    if (CompareSortKey(keyA,keyB) >= 0) then
      raise Exception.CreateFmt('"%s" >= "%s" !',[AnsiChar(i-1),AnsiChar(i)]);
    keyA := keyB;
  end;

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('b');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test4','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a','b','g'{*}, 'c','d','e','f','h','i','j','k','l'],@unicodeBook2);
    CheckInf(['g'{*}, 'c'],@unicodeBook2);
    CheckInf(['gg'{*}, 'c'],@unicodeBook2);
    CheckInf(['gg'{*}, 'cc'],@unicodeBook2);
    CheckInf(['g'{*}, 'ca'],@unicodeBook2);
    CheckInf(['gg'{*}, 'ca'],@unicodeBook2);
    CheckInf(['bb','g'{*}],@unicodeBook2);
    CheckInf(['bc','g'{*}],@unicodeBook2);
    CheckInf(['bc','gg'{*}],@unicodeBook2);
    CheckInf(['bg','bc'{*}],@unicodeBook2);
    WriteLn('    -- test 1 - ok',sLineBreak);

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('b');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test4','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a','b','g'{*},'k'{*}, 'c','d','e','f','h','i','j','l'],@unicodeBook2);
    CheckInf(['g'{*}, 'c'],@unicodeBook2);
    CheckInf(['k'{*}, 'c'],@unicodeBook2);
    CheckInf(['b','kk'{*}],@unicodeBook2);
    CheckInf(['bb','kk'{*}],@unicodeBook2);
    CheckInf(['b','kkk'{*}],@unicodeBook2);
    CheckInf(['gk','kk'{*}],@unicodeBook2);
    CheckInf(['gk','k'{*}],@unicodeBook2);
    CheckInf(['gk','kkk'{*}],@unicodeBook2);

    CheckInf(['gg'{*}, 'c'],@unicodeBook2);
    CheckInf(['gg'{*}, 'cc'],@unicodeBook2);
    CheckInf(['g'{*}, 'ca'],@unicodeBook2);
    CheckInf(['gg'{*}, 'ca'],@unicodeBook2);
    CheckInf(['bb','g'{*}],@unicodeBook2);
    CheckInf(['bc','g'{*}],@unicodeBook2);
    CheckInf(['bc','gg'{*}],@unicodeBook2);
    CheckInf(['bg','bc'{*}],@unicodeBook2);
    WriteLn('    -- test 2 - ok');
end;

//-------------------------------------------------------------------------

procedure test5_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,6);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('d'),Ord('a')]);
    p^.Weights := ToWeight($1609,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
end;

procedure test5_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  ASequence.Append(TReorderUnit.From([Ord('a'),Ord('d'),Ord('a')],TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,6));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test5();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test5_prepareWeigth(wfirst);
  test5_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test5','first',nil,unicodeBook1);
  CheckInf(['a','b','ada','c','d','e'],@unicodeBook1);
  CheckInf(['ba','adaa'],@unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test5','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a','b','ada','d','e',  'c'{* deleted !}],@unicodeBook2);
    CheckInf(['ee','ca'],@unicodeBook2);
    WriteLn('    -- test 1 - ok',sLineBreak);

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d'),Ord('a')],TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test5','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a', 'adac'{* deleted !}, 'b','c','d','e'],@unicodeBook2);
    CheckInf(['a','ada'],@unicodeBook2);
    CheckInf(['ada','b'],@unicodeBook2);
    CheckInf(['ac','ada'],@unicodeBook2);
    CheckInf(['ac','adac'],@unicodeBook2);
    CheckInf(['abe','ada'],@unicodeBook2);
    CheckInf(['abe','adae'],@unicodeBook2);
    WriteLn('    -- test 2 - ok',sLineBreak);
end;

//-------------------------------------------------------------------------

procedure test6_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,7);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('d')]);
    p^.Weights := ToWeight($1609,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('d'),Ord('a')]);
    p^.Weights := ToWeight($1613,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
end;

procedure test6_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  ASequence.Append(TReorderUnit.From([Ord('a'),Ord('d')],TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From([Ord('a'),Ord('d'),Ord('a')],TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,7));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test6();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2, unicodeBook3 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test6_prepareWeigth(wfirst);
  test6_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test6','first',nil,unicodeBook1);
  CheckInf(['a','b','ad','ada','c','d','e'],@unicodeBook1);
  CheckInf(['ba','ad'],@unicodeBook1);
  CheckInf(['ba','adaa'],@unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d')],TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test6','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a', 'ad'{*},'ada', 'b','c','d','e'],@unicodeBook2);
    CheckInf(['ab','ad'],@unicodeBook2);
    CheckInf(['ab','adb'],@unicodeBook2);
    CheckInf(['ad','ba'],@unicodeBook2);
    CheckInf(['adaa','ba'],@unicodeBook2);
    WriteLn('    -- test 1 - ok',sLineBreak);

  // --- test 2
  //sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d'),Ord('a')],TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test6','second',@unicodeBook2,unicodeBook3);
  unicodeBook3.Base := @unicodeBook2;
    CheckInf(['a', 'ad'{*},'ada'{*}, 'b','c','d','e'],@unicodeBook3);
    CheckInf(['ab','ad'],@unicodeBook3);
    CheckInf(['ab','adb'],@unicodeBook3);
    CheckInf(['ab','ada'],@unicodeBook3);
    WriteLn('    -- test 2 - ok',sLineBreak);

  // --- test 3
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d'),Ord('a')],TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test6','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf(['a', 'b', 'ad', 'c','d','e'],@unicodeBook2);
    CheckInf(['ad','ada'],@unicodeBook2);
    WriteLn('    -- test 3 - ok',sLineBreak);
end;

//-------------------------------------------------------------------------

procedure test7_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,8);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray($030A);//030A  ; [.0000.0043.0002.030A] # COMBINING RING ABOVE
    p^.Weights := ToWeight($0000,$0043,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0327);//0327  ; [.0000.0056.0002.0327] # COMBINING CEDILLA
    p^.Weights := ToWeight($0000,$0056,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0061);//a
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0062);//b
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0063);//c
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0064);//d
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([$0061,$030A]);//a,030A;COMBINING RING ABOVE
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
end;

procedure test7_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From($030A,TReorderWeigthKind.Tertiary,1));
  ASequence.Append(TReorderUnit.From($0327,TReorderWeigthKind.Tertiary,2));
  ASequence.Append(TReorderUnit.From($0061,TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From($0062,TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From($0063,TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From($0064,TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From([$0061,$030A],TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test7();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin  // Permutation simple test
  statement.Clear();
  test7_prepareWeigth(wfirst);
  test7_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test7','first',nil,unicodeBook1);
  CheckInf([#$030A,#$0327,#$0061,#$0062,#$0063,#$0064, #$0061#$030A,'k'],@unicodeBook1);
  CheckInf([#$0064, #$0061#$030A#$0327#$0062,'k'],@unicodeBook1);// Permutation here $030A <=> #$0327
  CheckInf([#$0064, #$0061#$0327#$030A#$0062,'k'],@unicodeBook1);
  CheckEqual(#$0061#$030A#$0327, #$0061#$0327#$030A,@unicodeBook1);

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,0);
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([$0061,$030A],TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test7','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf([#$030A,#$0327,#$0061,#$0061#$030A ,#$0062,#$0063,#$0064,'k'],@unicodeBook2);
    CheckInf([#$0061, #$0061#$030A#$0327#$0062,#$0062,#$0064],@unicodeBook2);
    CheckInf([#$0061, #$0061#$030A#$0062#$0327,#$0062,#$0064],@unicodeBook2);
    WriteLn('    -- test 2 - ok',sLineBreak);
end;

//-------------------------------------------------------------------------

procedure test8_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('f'));
    p^.Weights := ToWeight($1684,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('g'));
    p^.Weights := ToWeight($1691,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('h'));
    p^.Weights := ToWeight($16B4,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('i'));
    p^.Weights := ToWeight($16CD,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('j'));
    p^.Weights := ToWeight($16E6,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('l'));
    p^.Weights := ToWeight($1711,$0020,$0002);
end;

procedure test8_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From(Ord('h'),TReorderWeigthKind.Primary,8));
  ASequence.Append(TReorderUnit.From(Ord('i'),TReorderWeigthKind.Primary,9));
  ASequence.Append(TReorderUnit.From(Ord('j'),TReorderWeigthKind.Primary,10));
  ASequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  ASequence.Append(TReorderUnit.From(Ord('l'),TReorderWeigthKind.Primary,12));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test8();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test8_prepareWeigth(wfirst);
  test8_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test8','first',nil,unicodeBook1);
  CheckInf('l','-'{* computed are greater},@unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('c')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test8','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckEqual('cc','c-'{*},@unicodeBook2);
    CheckEqual('ccc','c-c'{*},@unicodeBook2);
    CheckEqual('cca','c-a'{*},@unicodeBook2);
    CheckEqual('cce','c-e'{*},@unicodeBook2);
    CheckInf(['cc','c-c'{*}],@unicodeBook2);
    CheckInf(['bc','bc-c'{*}],@unicodeBook2);
    CheckInf('l','-'{* computed are greater},@unicodeBook2);
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('c')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test8','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckInf('cc','c-'{*},@unicodeBook2);
    CheckInf('ccl','c-'{*},@unicodeBook2);
    CheckInf('ccc','c-c'{*},@unicodeBook2);
    CheckInf('cca','c-a'{*},@unicodeBook2);
    CheckInf('cce','c-e'{*},@unicodeBook2);
    CheckInf(['cc','c-c'{*}],@unicodeBook2);
    CheckInf(['bc','bc-c'{*}],@unicodeBook2);
    CheckInf('l','-'{* computed are greater},@unicodeBook2);
    WriteLn('    -- test 2 - ok');

  // --- test 3
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('-'),Ord('+')],[Ord('c')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test8','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckEqual('cc','c-+'{*},@unicodeBook2);
    CheckEqual('ccc','c-+c'{*},@unicodeBook2);
    CheckEqual('cca','c-+a'{*},@unicodeBook2);
    CheckEqual('cce','c-+e'{*},@unicodeBook2);
    CheckInf(['cc','c-+c'{*}],@unicodeBook2);
    CheckInf(['bc','bc-+c'{*}],@unicodeBook2);
    CheckInf('l','-+'{* computed are greater},@unicodeBook2);
    WriteLn('    -- test 3 - ok');

  // --- test 4 : '-' has 3 contexts to force the context tree to have at least
  //                  a "Left" and a "Right"
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('c')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  statement.Reset[0] := Ord('f');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('f')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('a')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test8','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    // Check c
    CheckEqual('cc','c-'{*},@unicodeBook2);
    CheckEqual('ccc','c-c'{*},@unicodeBook2);
    CheckEqual('cca','c-a'{*},@unicodeBook2);
    CheckEqual('cce','c-e'{*},@unicodeBook2);
    CheckInf(['cc','c-c'{*}],@unicodeBook2);
    CheckInf(['bc','bc-c'{*}],@unicodeBook2);
    //check f
    CheckEqual('ff','f-'{*},@unicodeBook2);
    CheckEqual('fff','f-f'{*},@unicodeBook2);
    CheckEqual('ffa','f-a'{*},@unicodeBook2);
    CheckEqual('ffe','f-e'{*},@unicodeBook2);
    CheckInf(['ff','f-f'{*}],@unicodeBook2);
    CheckInf(['bf','bf-f'{*}],@unicodeBook2);
    //check c and f
    CheckEqual('ccf','c-f'{*},@unicodeBook2);
    CheckEqual('ccff','c-f-'{*},@unicodeBook2);
    CheckEqual('ccfff','c-f-f'{*},@unicodeBook2);
    CheckEqual('ffcc','f-c-'{*},@unicodeBook2);
    CheckEqual('ffccf','f-c-f'{*},@unicodeBook2);

    CheckInf('ffccf','g'{*},@unicodeBook2);
    CheckInf('a-','ab',@unicodeBook2);
    // check - alone
    CheckInf('l','-'{* computed are greater},@unicodeBook2);
    WriteLn('    -- test 4 - ok');

  // --- test 5 : Add a contraction to force the code path
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('-'),Ord('h')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('-'),[Ord('c')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test8','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','-h','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckEqual('cc','c-'{*},@unicodeBook2);
    CheckEqual('ccc','c-c'{*},@unicodeBook2);
    CheckEqual('cca','c-a'{*},@unicodeBook2);
    CheckEqual('cce','c-e'{*},@unicodeBook2);
    CheckInf(['cc','c-c'{*}],@unicodeBook2);
    CheckInf(['bc','bc-c'{*}],@unicodeBook2);
    CheckInf(['ab','-hb'{*}],@unicodeBook2);
    CheckInf(['-hb','ba'],@unicodeBook2);
    CheckInf('l','-'{* computed are greater},@unicodeBook2);
    WriteLn('    -- test 5 - ok');

  WriteLn('    -- test - ok',sLineBreak);
end;

//-------------------------------------------------------------------------

procedure test9_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,8);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray($030A);//030A  ; [.0000.0043.0002.030A] # COMBINING RING ABOVE
    p^.Weights := ToWeight($0000,$0043,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0327);//0327  ; [.0000.0056.0002.0327] # COMBINING CEDILLA
    p^.Weights := ToWeight($0000,$0056,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0061);//a
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0062);//b
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0063);//c
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray($0064);//d
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([$0061,$030A]);//a,030A;COMBINING RING ABOVE
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
end;

procedure test9_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From($030A,TReorderWeigthKind.Tertiary,1));
  ASequence.Append(TReorderUnit.From($0327,TReorderWeigthKind.Tertiary,2));
  ASequence.Append(TReorderUnit.From($0061,TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From($0062,TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From($0063,TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From($0064,TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From([$0061,$030A],TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test9();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin// Permutation with Context
  statement.Clear();
  test7_prepareWeigth(wfirst);
  test7_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test9','first',nil,unicodeBook1);
  CheckInf([#$030A,#$0327,#$0061,#$0062,#$0063,#$0064, #$0061#$030A,'k'],@unicodeBook1);
  CheckInf([#$0064, #$0061#$030A#$0327#$0062,'k'],@unicodeBook1);// Permutation here $030A <=> #$0327

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := $0062;
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('k'),$032D],[$0061],TReorderWeigthKind.Secondary,0); //032D;COMBINING CIRCUMFLEX ACCENT BELOW;Mn;220;NSM;;;;;N;NON-SPACING CIRCUMFLEX BELOW;;;;
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test9','second',@unicodeBook1,unicodeBook2);
  unicodeBook2.Base := @unicodeBook1;
    CheckInf([#$030A,#$0327,#$0061,#$0062,#$0063,#$0064,'k'],@unicodeBook2);
    CheckInf([#$0061'k'#$032D  ,#$0061#$0063],@unicodeBook2);
    CheckNotEqual(#$0061'k'#$0327#$032D, #$0061#$0327,@unicodeBook2);
    CheckInf([#$0061'k'#$0327#$032D  ,#$0061#$0063],@unicodeBook2);
    WriteLn('    -- test 2 - ok',sLineBreak);
end;

//------------------------------------------------------

procedure test10_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('a'),Ord(')')]);
    p^.Weights := ToWeight($15EF,$0020,$0006); //15EF.0020.0006.24D0
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('A'));
    p^.Weights := ToWeight($15EF,$0020,$0008); //15EF.0020.0008.0041
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('A'),Ord(')')]);
    p^.Weights := ToWeight($15EF,$0020,$000C);  //15EF.0020.000C
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('`')]);
    p^.Weights := ToWeight([$15EF,$0020,$0002,  $0000,$0035,$0002]); //[.15EF.0020.0002.0061][.0000.0035.0002.0300]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('A'),Ord('`')]);
    p^.Weights := ToWeight([$15EF,$0020,$0008,  $0000,$0035,$0002]);  //[.15EF.0020.0008.0041][.0000.0035.0002.0300]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('a'),Ord('e')]);
    p^.Weights := ToWeight([$15F0,$0020,$0002]);  //[.15EF.0020.0004.00E6][.0000.0139.0004.00E6][.164C.0020.0004.00E6]
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord(UpCase('a')),Ord(UpCase('e'))]);
    p^.Weights := ToWeight([$15F0,$0020,$0006]);//[.15EF.0020.000A.00C6][.0000.0139.0004.00C6][.164C.0020.000A.00C6]
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('('),Ord('b'),Ord(')')]);
    p^.Weights := ToWeight($1605,$0020,$0006);  //.1605.0020.0006.24D1
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('B'));
    p^.Weights := ToWeight($1605,$0020,$0008); //1605.0020.0008.0042
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
end;

procedure test10_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From([Ord('('),Ord('a'),Ord(')')],TReorderWeigthKind.Tertiary,2));
  ASequence.Append(TReorderUnit.From(Ord('A'),TReorderWeigthKind.Tertiary,3));
  ASequence.Append(TReorderUnit.From([Ord('('),Ord('A'),Ord(')')],TReorderWeigthKind.Tertiary,4));

  //ASequence.Append(TReorderUnit.From(Ord('à'),TReorderWeigthKind.Secondary,0));
  ASequence.Append(TReorderUnit.From([Ord('a'),Ord('`')],TReorderWeigthKind.Secondary,5));
  //ASequence.Append(TReorderUnit.From(Ord(UpCase('à')),TReorderWeigthKind.Tertiary,0));
  ASequence.Append(TReorderUnit.From([Ord('A'),Ord('`')],TReorderWeigthKind.Tertiary,6));

  ASequence.Append(TReorderUnit.From([Ord('a'),Ord('e')],TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From([Ord(UpCase('a')),Ord(UpCase('e'))],TReorderWeigthKind.Tertiary,8));

  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,9));
  ASequence.Append(TReorderUnit.From([Ord('('),Ord('b'),Ord(')')],TReorderWeigthKind.Tertiary,10));
  ASequence.Append(TReorderUnit.From(Ord('B'),TReorderWeigthKind.Tertiary,11));

  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,12));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test10();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test10_prepareWeigth(wfirst);
  test10_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);

  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test6','first',nil,unicodeBook1);
  CheckInf(['a','(a)','A','(A)',  'a`','A`',  'ae','AE',  'b','(b)','B', 'c'],@unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('a');
  statement.Reset[1] := Ord('e');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
    CheckInf(['a','(a)','A','(A)',  'a`','A`', 'g'{*},  'ae','AE',  'b','(b)','B', 'c'],@unicodeBook2);
    CheckInf(['gg','ae'],@unicodeBook2);
    CheckInf(['gb','ae'],@unicodeBook2);
    WriteLn('    -- test 1 - ok',sLineBreak);

  // --- test 2
  sequence := sequenceClean.Clone();
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('a');
  statement.Reset[1] := Ord('e');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test2','2',@unicodeBook1,unicodeBook2);
    CheckInf(['a','(a)','A','(A)',  'a`','A`', 'g'{*},  'ae','AE',  'b','(b)','B', 'c'],@unicodeBook2);
    CheckInf(['A`B','gg'],@unicodeBook2);
    CheckInf(['A`b','g'],@unicodeBook2);
    WriteLn('    -- test 2 - ok',sLineBreak);

  // --- test 3
  sequence := sequenceClean.Clone();
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('a');
  statement.Reset[1] := Ord('e');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test2','2',@unicodeBook1,unicodeBook2);
    CheckInf(['a','(a)','A','(A)',  'a`','A`', 'g'{*},  'ae','AE',  'b','(b)','B', 'c'],@unicodeBook2);
    CheckInf(['A`B','gg'],@unicodeBook2);
    CheckInf(['A`b','g'],@unicodeBook2);
    WriteLn('    -- test 3 - ok',sLineBreak);

  // --- test 4
  sequence := sequenceClean.Clone();
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('A');
  statement.Reset[1] := Ord('`');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #4.1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('A');
  statement.Reset[1] := Ord('`');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('G'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #4.2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test2','2',@unicodeBook1,unicodeBook2);
    CheckInf(['a','(a)','A','(A)', 'G','a`','g'{*},'A`',  'ae','AE',  'b','(b)','B', 'c'],@unicodeBook2);
    CheckInf(['gg','A`B'],@unicodeBook2);
    CheckInf(['g','A`b'],@unicodeBook2);
    CheckInf(['A','gg'],@unicodeBook2);
    CheckInf(['A','Ga'],@unicodeBook2);
    WriteLn('    -- test 4 - ok',sLineBreak);

  // --- test 5
  sequence := sequenceClean.Clone();
  statement.Clear();
  statement.Before := True;
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('B');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('Statement #5 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  ConstructUnicodeBook(wresult,'test1','1',@unicodeBook1,unicodeBook2);
    CheckInf(['a','(a)','A','(A)',  'a`','A`',  'ae','AE', 'g'{*},  'b','(b)','B', 'c'],@unicodeBook2);
    CheckInf(['gg','b'],@unicodeBook2);
    CheckInf(['ae','gb'],@unicodeBook2);
    WriteLn('    -- test 5 - ok',sLineBreak);
end;

//------------------------------------------------------

procedure test11_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,12);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('b'));
    p^.Weights := ToWeight($1605,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('c'));
    p^.Weights := ToWeight($161D,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('d'));
    p^.Weights := ToWeight($1631,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('e'));
    p^.Weights := ToWeight($164C,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('f'));
    p^.Weights := ToWeight($1684,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('g'));
    p^.Weights := ToWeight($1691,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('h'));
    p^.Weights := ToWeight($16B4,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('i'));
    p^.Weights := ToWeight($16CD,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('j'));
    p^.Weights := ToWeight($16E6,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('k'));
    p^.Weights := ToWeight($16FF,$0020,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray(Ord('l'));
    p^.Weights := ToWeight($1711,$0020,$0002);
end;

procedure test11_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  ASequence.Append(TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,3));
  ASequence.Append(TReorderUnit.From(Ord('d'),TReorderWeigthKind.Primary,4));
  ASequence.Append(TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,5));
  ASequence.Append(TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,6));
  ASequence.Append(TReorderUnit.From(Ord('g'),TReorderWeigthKind.Primary,7));
  ASequence.Append(TReorderUnit.From(Ord('h'),TReorderWeigthKind.Primary,8));
  ASequence.Append(TReorderUnit.From(Ord('i'),TReorderWeigthKind.Primary,9));
  ASequence.Append(TReorderUnit.From(Ord('j'),TReorderWeigthKind.Primary,10));
  ASequence.Append(TReorderUnit.From(Ord('k'),TReorderWeigthKind.Primary,11));
  ASequence.Append(TReorderUnit.From(Ord('l'),TReorderWeigthKind.Primary,12));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test11();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test11_prepareWeigth(wfirst);
  test11_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test11','first',nil,unicodeBook1);
  CheckInf(['a','b','c','d','e','f','g','h','i','j','k','l'],@unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('x'),TReorderWeigthKind.Tertiary,0);
  statement.Elements[0].SetExpansion(Ord('h'));
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test1','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','x'{*},'d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckInf('ch','x'{*},@unicodeBook2);
    CheckInf('cha','xa'{*},@unicodeBook2);
    CheckInf('chk','xka'{*},@unicodeBook2);
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('c');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('x'),TReorderWeigthKind.Tertiary,0);
    statement.Elements[0].SetExpansion(Ord('h'));
  statement.Elements[1] := TReorderUnit.From(Ord('X'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test2','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','x'{*},'X'{*},'d','e','f','g','h','i','j','k','l'],@unicodeBook2);
    CheckInf('ch','x'{*},@unicodeBook2);
    CheckInf('cha','xa'{*},@unicodeBook2);
    CheckInf('chk','xka'{*},@unicodeBook2);
    CheckInf('ch','X'{*},@unicodeBook2);
    CheckInf('cha','Xa'{*},@unicodeBook2);
    CheckInf('chk','Xka'{*},@unicodeBook2);
    WriteLn('    -- test 2 - ok');

end;

//------------------------------------------------------

procedure test12_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,1);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0120,$0002);
end;

procedure test12_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure Check(const ACondition : Boolean; const AMsg : string);overload;
begin
  if not ACondition then
    raise Exception.Create(AMsg);
end;

procedure Check(
  const ACondition : Boolean;
  const AFormatMsg : string;
  const AArgs      : array of const
);overload;
begin
  Check(ACondition,Format(AFormatMsg,AArgs));
end;

procedure Check(const ACondition : Boolean);overload;
begin
  Check(ACondition,'Check failed.')
end;

procedure CheckSimpleProps(
  AItem          : PUCA_PropItemRec;
  AHasCodePoint,
  AIsValid       : Boolean;
  AChildCount    : Byte;
  AContextual    : Boolean
);overload;
var
  p : PUCA_PropItemRec;
begin
  p := AItem;
  Check(p<>nil,'p = nil');
  Check(p^.HasCodePoint()=AHasCodePoint,'HasCodePoint');
  Check(p^.IsValid()=AIsValid,'IsValid');
  Check(p^.ChildCount=AChildCount,'ChildCount');
  Check(p^.Contextual=AContextual,'Contextual');
end;

procedure CheckSimpleProps(
  AItem          : PUCA_PropItemRec;
  AHasCodePoint,
  AIsValid       : Boolean;
  AChildCount    : Byte;
  AContextual,
  AIsDeleted     : Boolean
);overload;
begin
  CheckSimpleProps(AItem,AHasCodePoint,AIsValid,AChildCount,AContextual);
  Check(AItem^.IsDeleted=AIsDeleted,'IsDeleted');
end;

procedure CheckWeigths(AItem : PUCA_PropItemRec; const AWeigths : array of Word);overload;
var
  p : PUCA_PropItemRec;
  c, i : Integer;
  pb : PByte;
  pw : PWord;
begin
  p := AItem;
  c := Length(AWeigths);
  if ((c mod 3) > 0) then
    Check(False,'Invalid Weigth Array.');
  c := c div 3;
  Check(c=p^.WeightLength,'WeightLength');
  if (c = 0) then
    exit;
  pb := PByte(PtrUInt(p)+SizeOf(TUCA_PropItemRec));
  pw := @AWeigths[Low(AWeigths)];
//First Item
  Check(PWord(pb)^ = pw^, 'First Item[0]');
    pw := pw + 1;
    pb := pb + 2;
  if (pw^ > High(Byte)) then begin
    Check(PWord(pb)^ = pw^, 'First Item[1]');
    pb := pb + 2;
  end else begin
    Check(pb^ = pw^, 'First Item[1]');
    pb := pb + 1;
  end;
  pw := pw + 1;
  if (pw^ > High(Byte)) then begin
    Check(PWord(pb)^ = pw^, 'First Item[2]');
    pb := pb + 2;
  end else begin
    Check(pb^ = pw^, 'First Item[2]');
    pb := pb + 1;
  end;
  pw := pw + 1;
// Others
  for i := 1 to c-1 do begin
    Check(PWord(pb)^ = pw^, 'Item[0],i=%d',[i]);
      Inc(pw);
      pb := pb + 2;
    Check(PWord(pb)^ = pw^, 'Item[1],i=%d',[i]);
      Inc(pw);
      pb := pb + 2;
    Check(PWord(pb)^ = pw^, 'Item[2],i=%d',[i]);
      Inc(pw);
      pb := pb + 2;
  end;
end;

procedure CheckWeigths(
        AData      : PUCA_PropWeights;
  const ADataCount : Integer;
  const AWeigths   : array of Word
);overload;
var
  c: Integer;
begin
  c := Length(AWeigths);
  if ((c mod 3) > 0) then
    Check(False,'Invalid Weigth Array.');
  c := c div 3;
  Check(c=ADataCount,'WeightLength');
  if (c = 0) then
    exit;
  if not CompareMem(AData,@AWeigths[0],(ADataCount*3*SizeOf(Word))) then
    Check(False,'Weight');
end;

function CalcWeigthSize(const AWeigths : array of Word) : Integer;
var
  c : Integer;
begin
  c := Length(AWeigths);
  if ((c mod 3) > 0) then
    Check(False,'Invalid Weigth Array.');
  Result := c * SizeOf(Word);
  if (c>0) then begin
    if (AWeigths[1] <= High(Byte)) then
      Result := Result - 1;
    if (AWeigths[2] <= High(Byte)) then
      Result := Result - 1;
  end;
end;

procedure test12_check_1(const ABook : unicodedata.TUCA_DataBook);
var
  p, px : PUCA_PropItemRec;
  size, sizeTotal, t: Cardinal;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15F0,0,0, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15F0,0,0, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15F0,0,0, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15F0,0,0, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'x'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  px := p;
  CheckSimpleProps(p,False,False,1,False);
  CheckWeigths(p,[]);
  size := SizeOf(TUCA_PropItemRec);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;
// for 'y'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False);
  CheckWeigths(p,[$15F0,0,0, $15F2,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15F0,0,0, $15F2,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(y)');

  Check(px^.Size = (t+size),'size(x)');

  sizeTotal:= sizeTotal+size;
  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test12_check_2(const ABook : unicodedata.TUCA_DataBook);
var
  p, ph : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$121,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$121,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$122,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$122,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'h'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  ph := p;
  CheckSimpleProps(p,False,False,1,False);
  CheckWeigths(p,[]);
  size := SizeOf(TUCA_PropItemRec);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;
// for 'i'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False);
  CheckWeigths(p,[$15EF,$123,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15EF,$123,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(i)');
  sizeTotal:= sizeTotal+size;

  Check(ph^.Size = (t+size),'size(h)');

// for 'k'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$123,1, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$123,1, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(k)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test12_check_3(const ABook : unicodedata.TUCA_DataBook);
var
  p, pc : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$121,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$121,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  pc := p;
  CheckSimpleProps(p,False,True,1,False);
  CheckWeigths(p,[$15EF,$122,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$122,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;

// for 'i'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False);
  CheckWeigths(p,[$15EF,$123,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15EF,$123,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(i)');
  sizeTotal:= sizeTotal+size;

  Check(pc^.Size = (t+size),'size(c)');

// for 'k'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$123,1, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$123,1, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(k)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test12_check_4(const ABook : unicodedata.TUCA_DataBook);
var
  p, pc : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$121,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$121,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size');
  sizeTotal:= sizeTotal+size;

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  pc := p;
  CheckSimpleProps(p,False,True,2,False);
  CheckWeigths(p,[$15EF,$122,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$122,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;

// for 'i' as in 'ci'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False);
  CheckWeigths(p,[$15EF,$123,0, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15EF,$123,0, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(i)');
  t := t+size;
  sizeTotal:= sizeTotal+size;

// for 's' as in 'cs'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False);
  CheckWeigths(p,[$15EF,$123,1, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15EF,$123,1, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(s)');
  t := t+size;
  sizeTotal:= sizeTotal+size;

  Check(pc^.Size = t,'size(c)');

// for 'k'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False);
  CheckWeigths(p,[$15EF,$123,1, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec) + CalcWeigthSize([$15EF,$123,1, $15EF,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(k)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test12();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test12_prepareWeigth(wfirst);
  test12_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test','first',nil,unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,3);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,0);
  statement.Elements[2] := TReorderUnit.From([Ord('x'),Ord('y')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','xy'],@unicodeBook2);
    test12_check_1(unicodeBook2);
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,4);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Secondary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Secondary,0);
  statement.Elements[2] := TReorderUnit.From([Ord('h'),Ord('i')],TReorderWeigthKind.Secondary,0);
  statement.Elements[3] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','hi','k'],@unicodeBook2);
    test12_check_2(unicodeBook2);
    WriteLn('    -- test 2 - ok');

  // --- test 3
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,4);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Secondary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Secondary,0);
  statement.Elements[2] := TReorderUnit.From([Ord('c'),Ord('i')],TReorderWeigthKind.Secondary,0);
  statement.Elements[3] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','ci','k'],@unicodeBook2);
    test12_check_3(unicodeBook2);
    WriteLn('    -- test 3 - ok');

  // --- test 4
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,5);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Secondary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Secondary,0);
  statement.Elements[2] := TReorderUnit.From([Ord('c'),Ord('i')],TReorderWeigthKind.Secondary,0);
  statement.Elements[3] := TReorderUnit.From(Ord('k'),TReorderWeigthKind.Tertiary,0);
  statement.Elements[4] := TReorderUnit.From([Ord('c'),Ord('s')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #4 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','b','c','ci','k','cs'],@unicodeBook2);
    test12_check_4(unicodeBook2);
    WriteLn('    -- test 4 - ok');
end;

//------------------------------------------------------

procedure test13_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,2);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0120,$0002);
  Inc(p);
    p^.CodePoints := CodePointToArray([Ord('b')]);
    p^.Weights := ToWeight($15F0,$0120,$0002);
end;

procedure test13_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  ASequence.Append(TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,2));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test13_check_1(const ABook : unicodedata.TUCA_DataBook);
var
  p, pb : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  pb := p;
  CheckSimpleProps(p,False,True,1,False,True);
  CheckWeigths(p,[]);
  size := SizeOf(TUCA_PropItemRec);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;

// for 'u' as in 'bu'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False,False);
  CheckWeigths(p,[$15F0,0,0, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15F0,0,0, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(u)');
  t := t+size;
  sizeTotal:= sizeTotal+size;

  Check(pb^.Size = t,'size(c)');

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test13_check_2(const ABook : unicodedata.TUCA_DataBook);
var
  p, pb : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  pb := p;
  CheckSimpleProps(p,False,True,1,False,True);
  CheckWeigths(p,[]);
  size := SizeOf(TUCA_PropItemRec);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  t := size;
  sizeTotal:= sizeTotal+size;

// for 'u' as in 'bu'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,True,True,0,False,False);
  CheckWeigths(p,[$15F0,0,0, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          SizeOf(UInt24) +
          CalcWeigthSize([$15F0,0,0, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(u)');
  t := t+size;
  sizeTotal:= sizeTotal+size;

  Check(pb^.Size = t,'size(c)');

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15F0,0,0, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15F0,0,0, $15F1,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(c)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test13();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test12_prepareWeigth(wfirst);
  test12_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test','first',nil,unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From([Ord('b'),Ord('u')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','bu','b'{because b's weigth is now computed!}],@unicodeBook2);
    test13_check_1(unicodeBook2);
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Deletion,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,2);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From([Ord('b'),Ord('u')],TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    CheckInf(['a','bu','c','b'{because b's weigth is now computed!}],@unicodeBook2);
    test13_check_2(unicodeBook2);
    WriteLn('    -- test 2 - ok');
end;

//------------------------------------------------------

procedure test14_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,1);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0120,$0002);
end;

procedure test14_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

procedure test14_check_1(const ABook : unicodedata.TUCA_DataBook);
var
  p : PUCA_PropItemRec;
  size, sizeTotal: Integer;
  ctx : PUCA_PropItemContextTreeRec;
  ctxItem : PUCA_PropItemContextTreeNodeRec;
  pb : PByte;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  pb := PByte(p);
  size := 0;
  CheckSimpleProps(p,True,True,0,True,False);
  size := SizeOf(TUCA_PropItemRec)+SizeOf(UInt24){codepoint};
  CheckWeigths(p,[]);
  ctx := PUCA_PropItemContextTreeRec(PtrUInt(p)+SizeOf(TUCA_PropItemRec)+SizeOf(UInt24));
  Check(ctx^.Size>0,'ctx^.Size');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctx)+SizeOf(ctx^.Size));
  Check(ctxItem<>nil,'ctxItem');
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right=0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('a'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$3, $15EF,0,0]);

  size := SizeOf(TUCA_PropItemRec)+
          SizeOf(UInt24){codepoint}+
          SizeOf(TUCA_PropItemContextTreeRec.Size)+
          SizeOf(TUCA_PropItemContextTreeNodeRec) +
          (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
          (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));

  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test14_check_2(const ABook : unicodedata.TUCA_DataBook);
var
  p : PUCA_PropItemRec;
  size, sizeTotal : Integer;
  ctx : PUCA_PropItemContextTreeRec;
  ctxItem : PUCA_PropItemContextTreeNodeRec;
  pb : PByte;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  pb := PByte(p);
  size := 0;
  CheckSimpleProps(p,True,True,0,True,False);
  CheckWeigths(p,[]);
  ctx := PUCA_PropItemContextTreeRec(PtrUInt(p)+SizeOf(TUCA_PropItemRec)+SizeOf(UInt24));
  Check(ctx^.Size>0,'ctx^.Size');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctx)+SizeOf(ctx^.Size));
  Check(ctxItem<>nil,'ctxItem');
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right=0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('a'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$3, $15EF,0,0]);
  size := SizeOf(TUCA_PropItemRec)+
          SizeOf(UInt24){codepoint}+
          SizeOf(TUCA_PropItemContextTreeRec.Size)+
          SizeOf(TUCA_PropItemContextTreeNodeRec) +
          (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
          (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  sizeTotal:= sizeTotal+size;

// for 'c'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$3, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$3, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(u)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test14_check_3(const ABook : unicodedata.TUCA_DataBook);
var
  p : PUCA_PropItemRec;
  size, sizeTotal, t: Integer;
  ctx : PUCA_PropItemContextTreeRec;
  ctxItem : PUCA_PropItemContextTreeNodeRec;
  pb : PByte;
begin
  sizeTotal := 0;
// for 'b'
  p := ABook.Props;
  pb := PByte(p);
  size := 0;
  CheckSimpleProps(p,True,True,0,True,False);
  CheckWeigths(p,[]);
  ctx := PUCA_PropItemContextTreeRec(PtrUInt(p)+SizeOf(TUCA_PropItemRec)+SizeOf(UInt24));
  Check(ctx^.Size>0,'ctx^.Size');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctx)+SizeOf(ctx^.Size));
  Check(ctxItem<>nil,'ctxItem');
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right<>0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('a'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$3, $15EF,0,0]);

  t := SizeOf(TUCA_PropItemContextTreeNodeRec) +
       (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
       (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  Check(ctxItem^.Right = t,'ctxItem^.Right');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctxItem)+t);
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right=0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('f'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$4, $15F1,0,0]);

  size := SizeOf(TUCA_PropItemRec)+
          SizeOf(UInt24){codepoint}+
          SizeOf(TUCA_PropItemContextTreeRec.Size)+
          t+
          SizeOf(TUCA_PropItemContextTreeNodeRec) +
          (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
          (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  sizeTotal:= sizeTotal+size;

// for 'e'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$3, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$3, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(e)');
  sizeTotal:= sizeTotal+size;

// for 'f'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$3, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$3, $15F1,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(f)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test14_check_4(const ABook : unicodedata.TUCA_DataBook);
var
  p : PUCA_PropItemRec;
  size, sizeTotal, t, ctxSize: Integer;
  ctx : PUCA_PropItemContextTreeRec;
  ctxItem, ctxItemParent : PUCA_PropItemContextTreeNodeRec;
  pb : PByte;
begin
  sizeTotal := 0;
  ctxSize := 0;
// for 'b'
  p := ABook.Props;
  pb := PByte(p);
  size := 0;
  CheckSimpleProps(p,True,True,0,True,False);
  CheckWeigths(p,[]);
  ctx := PUCA_PropItemContextTreeRec(PtrUInt(p)+SizeOf(TUCA_PropItemRec)+SizeOf(UInt24));
  Check(ctx^.Size>0,'ctx^.Size');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctx)+SizeOf(ctx^.Size));
  ctxItemParent := ctxItem;
  Check(ctxItem<>nil,'ctxItem');
  Check(ctxItem^.Left<>0,'ctxItem^.Left');
  Check(ctxItem^.Right<>0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('f'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$4, $15F1,0,0]);
  t := SizeOf(TUCA_PropItemContextTreeNodeRec) +
       (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
       (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  ctxSize := ctxSize+t;

  Check(ctxItem^.Left = t,'ctxItem^.Left');
  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctxItem)+t);
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right=0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('a'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$120,$3, $15EF,0,0]);
  t := SizeOf(TUCA_PropItemContextTreeNodeRec) +
       (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
       (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  ctxSize := ctxSize+t;

  ctxItem := PUCA_PropItemContextTreeNodeRec(PtrUInt(ctxItemParent)+ctxSize);
  Check(ctxItem^.Left=0,'ctxItem^.Left');
  Check(ctxItem^.Right=0,'ctxItem^.Right');
  Check(ctxItem^.Data.CodePointCount=1,'ctxItem^.Data.CodePointCount');
  Check(ctxItem^.Data.WeightCount=2,'ctxItem^.Data.WeightCount');
  pb := PByte(PtrUInt(@ctxItem^.Data)+SizeOf(ctxItem^.Data));
  Check(Cardinal(PUInt24(pb)^)=Ord('h'),'Context CodePoint');
  pb := pb + (ctxItem^.Data.CodePointCount*SizeOf(UInt24));
  CheckWeigths(PUCA_PropWeights(pb),ctxItem^.Data.WeightCount,[$15EF,$121,$6, $15F1,0,0]);
  t := SizeOf(TUCA_PropItemContextTreeNodeRec) +
       (ctxItem^.Data.CodePointCount*SizeOf(UInt24))+
       (ctxItem^.Data.WeightCount*SizeOf(TUCA_PropWeights));
  ctxSize := ctxSize+t;

  ctxSize := ctxSize + SizeOf(TUCA_PropItemContextTreeRec.Size);
  Check(ctx^.Size = ctxSize,'ctx^.Size');
  size := SizeOf(TUCA_PropItemRec)+
          SizeOf(UInt24){codepoint}+
          ctxSize;
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  sizeTotal:= sizeTotal+size;

// for 'e'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$3, $15F0,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$3, $15F0,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(e)');
  sizeTotal:= sizeTotal+size;

// for 'f'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$3, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$3, $15F1,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(f)');
  sizeTotal:= sizeTotal+size;

// for 'g'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$120,$5, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$120,$5, $15F1,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(g)');
  sizeTotal:= sizeTotal+size;

// for 'h'
  p := PUCA_PropItemRec(PtrUInt(p)+size);
  CheckSimpleProps(p,False,True,0,False,False);
  CheckWeigths(p,[$15EF,$121,$5, $15F1,0,0]);
  size := SizeOf(TUCA_PropItemRec) +
          CalcWeigthSize([$15EF,$121,$5, $15F1,0,0]);
  Check(p^.GetSelfOnlySize() = size,'GetSelfOnlySize');
  Check(p^.Size = size,'size(h)');
  sizeTotal:= sizeTotal+size;

  Check(ABook.PropCount = sizeTotal,'size(total)');
end;

procedure test14();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test12_prepareWeigth(wfirst);
  test12_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test','first',nil,unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    test14_check_1(unicodeBook2);
    WriteLn('    -- test 1 - ok');
    WriteLn;

  // --- test 2
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    test14_check_2(unicodeBook2);
    WriteLn('    -- test 2 - ok');
    WriteLn;

  // --- test 3
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('f');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('f')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);

  WriteLn('    Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    test14_check_3(unicodeBook2);
    WriteLn('    -- test 3 - ok');
    WriteLn;

  // --- test 4
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,4);
  statement.Elements[0] := TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,0);
  statement.Elements[2] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  statement.Elements[3] := TReorderUnit.From(Ord('h'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('f');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('f')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('h');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('h')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);

  WriteLn('    Statement #4 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    test14_check_4(unicodeBook2);
    WriteLn('    -- test 4 - ok');
    WriteLn;
end;

//------------------------------------------------------

procedure test15_prepareWeigth(var AData : TUCA_LineRecArray);
var
  p : PUCA_LineRec;
begin
  SetLength(AData,1);
  p := @AData[Low(AData)];
    p^.CodePoints := CodePointToArray(Ord('a'));
    p^.Weights := ToWeight($15EF,$0120,$0002);
end;

procedure test15_PopulateSequence(var ASequence : TOrderedCharacters);
var
  i : Integer;
begin
  ASequence := TOrderedCharacters.Create();
  ASequence.Append(TReorderUnit.From(Ord('a'),TReorderWeigthKind.Primary,1));
  for i := 0 to ASequence.ActualLength - 1 do
    ASequence.Data[i].Changed := False;
end;

function ConvertEndianFromNative(
  const AData    : Pointer;
  const ADataLen : Integer
) : Boolean;
type
  PUCA_PropItemRec = helper.PUCA_PropItemRec;
var
  s : PUCA_PropItemRec;
  x, y : array of Byte;
  px, py : PUCA_PropItemRec;
begin
  if (ADataLen <= 0) then
    exit(True);
  s := PUCA_PropItemRec(AData);
  SetLength(x,ADataLen);
  px := PUCA_PropItemRec(@x[0]);
  ReverseFromNativeEndian(s,ADataLen,px);

  SetLength(y,ADataLen);
  py := PUCA_PropItemRec(@y[0]);
  ReverseToNativeEndian(px,ADataLen,py);
  Result := CompareMem(AData,@y[0],Length(x));
end;

procedure test15();
var
  sequence, sequenceClean : TOrderedCharacters;
  statement : TReorderSequence;
  wfirst, wresult : TUCA_LineRecArray;
  unicodeBook1, unicodeBook2 : unicodedata.TUCA_DataBook;
begin
  statement.Clear();
  test12_prepareWeigth(wfirst);
  test12_PopulateSequence(sequenceClean);

  WriteLn('  Initial = ',sLineBreak,'    ',DumpSequenceAnsi(sequenceClean),sLineBreak);
  WriteLn(DumpLines(wfirst),sLineBreak+sLineBreak);
  //Generate the original tables
  ConstructUnicodeBook(wfirst,'test','first',nil,unicodeBook1);

  // --- test 1
  sequence := sequenceClean.Clone();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #1 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 1 - ok');

  // --- test 2
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #2 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 2 - ok');
    WriteLn;

  // --- test 3
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('c'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #3 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 3 - ok');
    WriteLn;

  // --- test 4
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('f');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('f')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #4 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 4 - ok');
    WriteLn;

  // --- test 5
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,4);
  statement.Elements[0] := TReorderUnit.From(Ord('e'),TReorderWeigthKind.Primary,0);
  statement.Elements[1] := TReorderUnit.From(Ord('f'),TReorderWeigthKind.Primary,0);
  statement.Elements[2] := TReorderUnit.From(Ord('g'),TReorderWeigthKind.Tertiary,0);
  statement.Elements[3] := TReorderUnit.From(Ord('h'),TReorderWeigthKind.Secondary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('f');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('f')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('a')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('h');
  SetLength(statement.Elements,1);
  statement.Elements[0] := TReorderUnit.From(Ord('b'),[Ord('h')],TReorderWeigthKind.Tertiary,0);
  sequence.ApplyStatement(@statement);

  WriteLn('    Statement #5 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 5 - ok');
    WriteLn;

  // --- test 6
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,2);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d')],[Ord('a')],TReorderWeigthKind.Tertiary,0);
  statement.Elements[1] := TReorderUnit.From([Ord('a'),Ord('d')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #6 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 6 - ok');
    WriteLn;

  // --- test 7
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,3);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d')],[Ord('a')],TReorderWeigthKind.Tertiary,0);
  statement.Elements[1] := TReorderUnit.From([Ord('a'),Ord('d')],TReorderWeigthKind.Primary,0);
  statement.Elements[2] := TReorderUnit.From(Ord('e'),[Ord('a'),Ord('d')],TReorderWeigthKind.Identity,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #7 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 7 - ok');
    WriteLn;

  // --- test 8
  sequence := sequenceClean.Clone();
  statement.Clear();
  SetLength(statement.Reset,1);
  statement.Reset[0] := Ord('a');
  SetLength(statement.Elements,3);
  statement.Elements[0] := TReorderUnit.From([Ord('a'),Ord('d')],[Ord('a')],TReorderWeigthKind.Tertiary,0);
  statement.Elements[1] := TReorderUnit.From([Ord('a'),Ord('x')],TReorderWeigthKind.Primary,0);
  statement.Elements[2] := TReorderUnit.From([Ord('e'),Ord('a'),Ord('r')],[Ord('a'),Ord('d')],TReorderWeigthKind.Primary,0);
  sequence.ApplyStatement(@statement);
  WriteLn('    Statement #8 = ',sLineBreak,'  ',DumpSequenceAnsi(sequence),sLineBreak);
  wresult := nil;
  ComputeWeigths(@sequence.Data[0],sequence.ActualLength,wfirst,wresult);
  WriteLn(DumpLines(wresult),sLineBreak+sLineBreak);
  //Generate updatet tables
  ConstructUnicodeBook(wresult,'test','second',@unicodeBook1,unicodeBook2);
    Check(ConvertEndianFromNative(unicodeBook2.Props,unicodeBook2.PropCount),'Endian conversion failed.');
    WriteLn('    -- test 8 - ok');
    WriteLn;

end;

end.
