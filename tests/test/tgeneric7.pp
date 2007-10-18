{ %result=201 }

{ checks proper saving of compiler state }
{$mode objfpc}

{$R-}
uses
  ugeneric7;

type
  tmytype = specialize tgeneric<byte>;
var
  s : tmytype;
begin
  s:=tmytype.create;
  s.test;
  s.free;
end.
