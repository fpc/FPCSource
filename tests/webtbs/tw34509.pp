{ %TARGET = win64 }

program tw34509;

{$MODE DELPHI}

uses
  TypInfo,
  RTTI;

type
  TRec = record
    S: string;
    I: Integer;
  end;

function Test(P: TRec): TRec;
begin
  Result := P;
  WriteLn('P: ', P.S, ' - ', P.I);
end;

var
  V: TValue;
  R1, R2: TRec;
begin
  R1.S := 'abc';
  R1.I := 123;
  TValue.Make(@R1, TypeInfo(TRec), V);
  R2 := TRec(Rtti.Invoke(@Test, [V], ccReg, TypeInfo(TRec), True, False).GetReferenceToRawData^);
  WriteLn('R: ', R2.S, ' - ', R2.I);
  //ReadLn;
end.
