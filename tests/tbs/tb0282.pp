{ Old file: tbs0334.pp }
{  }

{$ifdef fpc}{$mode objfpc}{$endif}

type
  tvarrec=record
    vpointer : pointer;
  end;
var
  r : tvarrec;
  b : boolean;
function Next: TVarRec;
begin
  next:=r;
end;

begin
  r.vpointer:=@b;
  { The result of next is loaded and a value is assigned }
  with Next do
   boolean(VPointer^) := true;
  if not b then
   writeln('Error with assigning to function result');
end.
