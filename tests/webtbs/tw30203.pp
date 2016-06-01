{ %NORUN }

program tw30203;

{$MODE DELPHI}
{$POINTERMATH ON}

procedure QuickSort<T>(var A: Array of T; const Index, Count: Integer);
var
  I, J: Integer;
  Temp, Pivot: T;
begin
  if Index < Count then
  begin
    Pivot := A[Random(Count - Index) + Index + 1];
    I := Index - 1;
    J := Count + 1;
    repeat
      repeat Inc(I) until A[I] >= Pivot;
      repeat Dec(J) until A[J] <= Pivot;
      Temp := A[I];
      A[I] := A[J];
      A[J] := Temp;
    until I >= J;
    A[J] := A[I];
    A[I] := Temp;
    QuickSort<T>(A, Index, I - 1);
    QuickSort<T>(A, I, Count);
  end;
end;

var
  arri: array of LongInt;
  arrs: array of String;
begin
  SetLength(arri, 4);
  arri[0] := 4;
  arri[1] := 2;
  arri[2] := 6;
  arri[3] := 1;
  SetLength(arrs, 4);
  arrs[0] := 'World';
  arrs[1] := 'Alpha';
  arrs[2] := 'Hello';
  arrs[3] := 'Foo';
  QuickSort<LongInt>(arri, Low(arri), High(arri));
  QuickSort<String>(arrs, Low(arrs), High(arrs));
end.
