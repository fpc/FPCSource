{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  tc = class
   private
    fo: tobject;
    function getfo(i: integer): tobject;
    procedure putfo(i: integer; o: tobject);
   public
    property o[i:integer]: tobject read getfo write putfo; default;
  end;

function tc.getfo(i:integer): tobject;
begin
  result:=fo;
end;

procedure tc.putfo(i: integer; o: tobject);
begin
  fo:=o;
end;

var
  c: tc;
begin
  c:=tc.create;
  { should give an error stating that you cannot assign to left hand side }
  { (generated code also does not result in an assignment)                }
  tc(c[5]):=tc(5);
  writeln(longint(c[5]));
end.

