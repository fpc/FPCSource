{ %fail% }
{ %opt=-Sew -vw -O- }

{
  Test for correct emitting of warnings/hints for uninitialized variables of management types
  See also tbf/tb0258.pp
}

// This code must issue warnings "Function result variable of a managed type does not seem to be initialized".

{$mode objfpc}

procedure fvar(var a: ansistring);
begin
  setlength(a,100);
  a[2]:='a';
end;

function f: ansistring;
begin
  // Warning for the ansistring Result, since initial contents of the Result is undefined.
  fvar(Result);
end;

begin
  f;
end.
