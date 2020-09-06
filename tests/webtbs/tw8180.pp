{ %recompile }

{$mode objfpc}

uses uw8180;

type
  tcl=class(TInterfacedObject,XStr)
  end;

var
  x : tcl;
  p: pointer;
  i: iunknown;
begin
  x:=tcl.create;
  x._Addref;
  i:=x as iunknown;
  if (x as iunknown).queryinterface(xstr,p) <> S_OK then
    halt(1);
  if (x as iunknown).queryinterface(iinterface,p) <> S_OK then
    halt(2);
end.
