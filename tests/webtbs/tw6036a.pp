{$interfaces corba}
{$mode objfpc}
type
  imyinterface = interface
  ['MYINTERFACE']
  end;

var
  s : string;
begin
  s:=imyinterface;
end.
