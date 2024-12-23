program project2;

{$mode objfpc}
{$inline on}

function Func1(S: PAnsiChar): SizeUInt;
begin
  Result := 0;
end;

function Func2(S: PAnsiChar): SizeUInt; inline;
begin
  Result := Func1(S);
  if S <> nil then S[Result] := #0;
end;

function Func3(S: PAnsiChar): SizeUInt; inline;
begin
  Result := Func1(S);
  if Assigned(S) then S[Result] := #0;
end;

begin
  Func2(nil); // <-- OK
  Func3(nil); // <-- Error: Incompatible types: got "PAnsiChar" expected "Pointer"
end.
