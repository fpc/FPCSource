program pi;

{$ifdef fpc}
{$mode objfpc}
{$endif fpc}

{$APPTYPE CONSOLE}

{$h+}

uses
  timer;

function ComputePi(NumDigits: Integer): string;
var
  A: array of LongInt;
  I, J, K, P, Q, X, Nines, Predigit: Integer;
  PiLength: Integer;
begin
  start;
  SetLength(A, 10*NumDigits div 3);
  SetLength(Result, NumDigits+1);
  PiLength := 1;
  for I := Low(A) to High(A) do
    A[I] := 2;
  Nines := 0;
  Predigit := 0;
  for J := 0 to NumDigits-1 do
  begin
    Q := 0;
    P := 2 * High(A) + 1;
    for I := High(A) downto Low(A) do
    begin
      X := 10*A[I] + Q*(I+1);
      A[I] := X mod P;
      Q := X div P;
      P := P - 2;
    end;
    A[Low(A)] := Q mod 10;
    Q := Q div 10;
    if Q = 9 then
      Inc(Nines)
    else if Q = 10 then
    begin
      Result[PiLength] := Chr(Predigit + 1 + Ord('0'));
      for K := 1 to Nines do
        Result[PiLength+K] := '0';
      PiLength := PiLength + Nines + 1;
      Predigit := 0;
      Nines := 0;
    end
    else
    begin
      Result[PiLength] := Chr(Predigit + Ord('0'));
      Predigit := Q;
      for K := 1 to Nines do
        Result[PiLength+K] := '9';
      PiLength := PiLength + Nines + 1;
      Nines := 0;
    end;
  end;
  Result[PiLength] := Chr(Predigit + Ord('0'));
  stop;
end;

var
  NumDigits: Integer;
  Code: Integer;
  F: TextFile;
  result : string;
begin
  if ParamCount = 0 then
    WriteLn('usage: pi #DIGITS [FILE]')
  else
  begin
    Val(ParamStr(1), NumDigits, Code);
    if Code <> 0 then
    begin
      WriteLn('Invalid # digits: ', ParamStr(1));
      Halt(1);
    end;

    if ParamCount > 1 then
    begin
      AssignFile(F, ParamStr(2));
      Rewrite(F);
      WriteLn(F, ComputePi(NumDigits));
      CloseFile(F);
    end
    else
      begin
        result:=ComputePi(NumDigits);
        WriteLn(result);
      end;
  end;
end.
