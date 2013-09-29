{   Unicode Collation Algorithm test routines for generated data.

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
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit uca_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  helper;

  procedure uca_CheckProp_1(
    ABook : TUCA_DataBook;
    APropBook : PUCA_PropBook
  );
  procedure uca_CheckProp_x(
    ABook : TUCA_DataBook;
    APropBook : PUCA_PropBook
  );

  procedure uca_CheckProp_1y(
    const ABook         : TUCA_DataBook;
    const APropBook     : PUCA_PropBook;
    const AFirstTable   : PucaBmpFirstTable;
    const ASecondTable  : PucaBmpSecondTable
  );
  procedure uca_CheckProp_2y(
    const ABook         : TUCA_DataBook;
    const APropBook     : PUCA_PropBook;
    const AFirstTable   : PucaOBmpFirstTable;
    const ASecondTable  : PucaOBmpSecondTable
  );

implementation

function IndexOf(const ACodePoint : Cardinal; APropBook : PUCA_PropBook): Integer;
var
  i : Integer;
begin
  for i := 0 to Length(APropBook^.Index) - 1 do begin
    if (ACodePoint = APropBook^.Index[i].CodePoint) then
      exit(i);
  end;
  Result := -1;
end;

function CompareWeigth(AExpect : PUCA_LineRec; AActual : PUCA_PropItemRec) : Boolean;
var
  i, k : Integer;
  p : PUCA_PropWeights;
  pw : array of TUCA_PropWeights;
begin
  Result := False;
  if (Length(AExpect^.Weights) <> AActual^.WeightLength) then
    exit;
  //p := PUCA_PropWeights(PtrUInt(AActual) + SizeOf(TUCA_PropItemRec));
  SetLength(pw,AActual^.WeightLength);
  p := @pw[0];
  AActual^.GetWeightArray(p);
  for i := 0 to Length(AExpect^.Weights) - 1 do begin
    //if (BoolToByte(AExpect^.Weights[i].Variable) <> p^.Variable) then
      //exit;
    for k := 0 to 3 - 1 do begin
      if (AExpect^.Weights[i].Weights[k] <> p^.Weights[k]) then
        exit;
    end;
    Inc(p);
  end;
  Result := True;
end;

procedure uca_CheckProp_1(
  ABook : TUCA_DataBook;
  APropBook : PUCA_PropBook
);
var
  i, c, k : Integer;
  line : PUCA_LineRec;
  uc : Cardinal;
  p : PUCA_PropItemRec;
begin
  WriteLn('uca_CheckProp_1 Start ... ');
  line := @ABook.Lines[0];
  c := Length(ABook.Lines);
  for i := 0 to c - 1 do begin
    if line^.Stored and (Length(line^.CodePoints) = 1) then begin
      uc := line^.CodePoints[0];
      k := IndexOf(uc,APropBook);
      if (k = -1) then begin
        WriteLn('Property not found for Code Point : ' + Format('%x',[uc]));
      end else begin
        p := PUCA_PropItemRec(PtrUInt(APropBook^.Items)+APropBook^.Index[k].Position);
        if not CompareWeigth(line,p) then
          WriteLn('CompareWeigth fail for Code Point : ' + Format('%x',[uc]));
      end;
    end;
    Inc(line);
  end;
  WriteLn('uca_CheckProp_1 End');
end;

function FindWord(
  const AWord : array of Cardinal;
  const APropBook : PUCA_PropItemRec
) : PUCA_PropItemRec;
var
  cc : Cardinal;
  p : PUCA_PropItemRec;
  i, k, kc : Integer;
  ok : Boolean;
begin
  Result := nil;
  p := APropBook;
  for i := 1 to Length(AWord) - 1 do begin
    ok := False;
    kc := p^.ChildCount - 1;
    p := PUCA_PropItemRec(PtrUInt(p) + p^.GetSelfOnlySize());
    for k := 0 to kc do begin
      if (AWord[i] = p^.CodePoint) then begin
        ok := True;
        Break;
      end;
      p := PUCA_PropItemRec(PtrUInt(p) + p^.Size);
    end;
    if not ok then
      exit;
  end;
  Result := p;
end;

function DumpCodePoints(const AValues : array of Cardinal) : string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Length(AValues) - 1 do
    Result := Format('%s %x',[Result,AValues[i]]);
  Result := Trim(Result);
end;

procedure uca_CheckProp_x(
  ABook : TUCA_DataBook;
  APropBook : PUCA_PropBook
);
var
  i, c, k : Integer;
  line : PUCA_LineRec;
  uc : Cardinal;
  p, q : PUCA_PropItemRec;
begin
  WriteLn('uca_CheckProp_x Start ... ');
  line := @ABook.Lines[0];
  c := Length(ABook.Lines);
  for i := 0 to c - 1 do begin
    if line^.Stored and (Length(line^.CodePoints) > 1) then begin
      //WriteLn('  Code Point sequence  : ' + DumpCodePoints(line^.CodePoints));
      uc := line^.CodePoints[0];
      k := IndexOf(uc,APropBook);
      if (k = -1) then begin
        WriteLn('    Property not found for Code Point : ' + Format('%x',[uc]));
      end else begin
        q := PUCA_PropItemRec(PtrUInt(APropBook^.Items)+APropBook^.Index[k].Position);
        p := FindWord(line^.CodePoints,q);
        if (p = nil) then
          WriteLn('    Data not found for Code Point sequence  : ' + DumpCodePoints(line^.CodePoints))
        else if not CompareWeigth(line,p) then
          WriteLn('    CompareWeigth fail for Code Point sequence  : ' + DumpCodePoints(line^.CodePoints));
      end;
    end;
    Inc(line);
  end;
  WriteLn('uca_CheckProp_x End');
end;


function GetPropPosition(
  const ABMPCodePoint : Word;
  const AFirstTable   : PucaBmpFirstTable;
  const ASecondTable  : PucaBmpSecondTable
) : Integer; inline;overload;
begin
  Result:=
      ASecondTable^[AFirstTable^[WordRec(ABMPCodePoint).Hi]][WordRec(ABMPCodePoint).Lo] - 1
end;

procedure uca_CheckProp_1y(
  const ABook         : TUCA_DataBook;
  const APropBook     : PUCA_PropBook;
  const AFirstTable   : PucaBmpFirstTable;
  const ASecondTable  : PucaBmpSecondTable
);
var
  i, c, k : Integer;
  line : PUCA_LineRec;
  uc : Cardinal;
  p : PUCA_PropItemRec;
  ucw : Word;
begin
  WriteLn('uca_CheckProp_1y Start (BMP) ... ');
  line := @ABook.Lines[0];
  c := Length(ABook.Lines);
  for i := 0 to c - 1 do begin
    if line^.Stored and (Length(line^.CodePoints) = 1) then begin
      uc := line^.CodePoints[0];
      if (uc <= High(Word)) then begin
        ucw := uc;
        k := GetPropPosition(ucw,AFirstTable,ASecondTable);
        if (k = -1) then begin
          WriteLn('Property not found for Code Point : ' + Format('%x',[uc]));
        end else begin
          p := PUCA_PropItemRec(PtrUInt(APropBook^.Items)+k);
          if not CompareWeigth(line,p) then
            WriteLn('CompareWeigth fail for Code Point : ' + Format('%x',[uc]));
        end;
      end;
    end;
    Inc(line);
  end;
  WriteLn('uca_CheckProp_1y End');
end;

procedure uca_CheckProp_2y(
  const ABook         : TUCA_DataBook;
  const APropBook     : PUCA_PropBook;
  const AFirstTable   : PucaOBmpFirstTable;
  const ASecondTable  : PucaOBmpSecondTable
);
var
  i, c, k : Integer;
  line : PUCA_LineRec;
  uc : Cardinal;
  p : PUCA_PropItemRec;
  uchs, ucls : Word;
begin
  WriteLn('uca_CheckProp_2y Start (>BMP) ... ');
  line := @ABook.Lines[0];
  c := Length(ABook.Lines);
  for i := 0 to c - 1 do begin
    if line^.Stored and (Length(line^.CodePoints) = 1) then begin
      uc := line^.CodePoints[0];
      if (uc > High(Word)) then begin
        FromUCS4(uc,uchs,ucls);
        k := GetPropPosition(uchs,ucls,AFirstTable,ASecondTable);
        if (k = -1) then begin
          WriteLn('Property not found for Code Point : ' + Format('%x',[uc]));
        end else begin
          p := PUCA_PropItemRec(PtrUInt(APropBook^.Items)+k);
          if not CompareWeigth(line,p) then
            WriteLn('CompareWeigth fail for Code Point : ' + Format('%x',[uc]));
        end;
      end;
    end;
    Inc(line);
  end;
  WriteLn('uca_CheckProp_2y End');
end;


end.

