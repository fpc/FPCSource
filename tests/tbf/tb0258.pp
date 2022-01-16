{ %fail% }
{ %opt=-Sew -vw -O- }

{
  Test for correct emitting of warnings/hints for uninitialized variables of management types
  See also tbs/tb0653.pp, tbf/tb0259.pp, tbf/tb0260.pp
}

// This code must issue warnings "Function result variable of a managed type does not seem to be initialized".

{$mode objfpc}

type
  TLongArray = array of longint;

function f: TLongArray;
begin
  // Warning for the dyn array Result, since contents of the Result after calling SetLength()
  // is expected to be zeroed, but instead it is undefined.
  setlength(Result,100);
  Result[2]:=1;
end;

begin
  f;
end.
