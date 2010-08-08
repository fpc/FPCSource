{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
   private
    fl: longint;
   public
    property l: longint read fl write fl;
  end;

var
  c: tc;
begin
  { should give an error stating that you cannot assign to left hand side }
  { (generated code also does not result in an assignment)                }
  cardinal(c.l):=cardinal(5);
end.


