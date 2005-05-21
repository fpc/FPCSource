{ Old file: tbs0183.pp }
{ internal error 10 in secondnot                        OK 0.99.11 (PM) }

program Internal_Error_10;

type
  PBug = ^TBug;
  TBug = array[1..1] of boolean;

var
  Left : PBug;
  test : longint;

begin
  New(left);
  test := 1;

{ following shows internal error 10 only if the

    array index is a var on both sides
  ( if either is a constant then it compiles fine, error only occurs if the
    not is in the statement )
    bug only appears if the array is referred to using a pointer -
      if using TBug, and no pointers it compiles fine
      with PBug the error appears
    }

  Left^[test] := not Left^[test];
end.
