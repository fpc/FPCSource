{ %version=1.1 }

{$MODE DELPHI}
type
aClass=class
 private
  aa:longint;
  procedure bb(index:integer;value:longint);
 public
  property cc:longint index 1 read aa write bb;
end;
procedure AClass.bb(index:integer;value:longint);
 begin
  aa:=value;
 end;
var
 C:aClass;
begin
 C:=aClass.Create;
 C.cc:=1;
 writeln(C.cc);
end.
