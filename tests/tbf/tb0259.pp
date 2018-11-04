{ %fail% }
{ %opt=-Sew -vw -O- }

{
  Test for correct emitting of warnings/hints for uninitialized variables of management types
  See also tbf/tb0258.pp
}

// This code must issue warnings "Function result variable of a managed type does not seem to be initialized".

{$mode objfpc}

type
  TLongArray = array of longint;

procedure fvar(var a: TLongArray);
begin
  setlength(a,100);
  a[2]:=1;
end;

function f: TLongArray;
begin
  // Warning for the dyn array Result, since initial contents of the Result is undefined.
  fvar(Result);
end;

begin
  f;
end.
