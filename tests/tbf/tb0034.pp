{ %FAIL }
{ Old file: tbf0168.pp }
{ set:=set+element is allowed (should be: set:=set+[element]) OK 0.99.9 (PFV) }

var bset: set of 0..31;
    b: byte;

Begin
  bset := bset + b;
End.
