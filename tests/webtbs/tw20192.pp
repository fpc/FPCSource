program SizeOfBug;
{$mode objfpc}{$H+}

type
  generic TGen<_T> = class(TObject)
    private
      FField: _T;
    public
      constructor Create(Val: _T);

      function Bug: LongInt;
  end;

{--- TGen.Create ---}
constructor TGen.Create(Val: _T);
begin
  inherited Create;
  FField := Val;
end;

{--- TGen.Bug ---}
function TGen.Bug : LongInt;
begin
  Result := 100000 div SizeOf(_T);  // *** DIVISION BY ZERO ***

  // THE FOLLOWING CODE IS OK !
  //
  // var
  //   S: Integer;
  // begin
  //   S := SizeOf(_T);
  //   Result := 100000 div S;
end;

type
  TGenInt = specialize TGen<Integer>;

var
  V: TGenInt;
begin
  V := V.Create(589);
  WriteLn('V.Bug = ', V.Bug);
  if V.Bug<>100000 div sizeof(Integer) then
    halt(1);
  V.Free;
  writeln('ok');
end.
