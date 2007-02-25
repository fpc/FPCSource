{ %recompile }

{$mode objfpc}

uses uw8180;

type
  tcl=class(TInterfacedObject,XStr)
  end;

var
  x : tcl;
begin
  x:=tcl.create;
  x._Addref;
end.
