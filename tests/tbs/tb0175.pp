{ Old file: tbs0209.pp }
{ problem with boolean expressions of different store sizes }

program bug0209;

{ problem with boolean expression mixing different boolean sizes }

var
  b : boolean;
  wb : wordbool;
  lb : longbool;
begin
  b:=true;
  wb:=true;
  lb:=true;
  if (not b) or (not wb) or (not lb) then
    begin
       Writeln('Error with boolean expressions of different sizes');
       Halt(1);
    end;
end.
