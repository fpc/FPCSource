{ %version=1.1 }
{ %fail }
{ Interfaces only supported in v1.1 }
{ Should give the same error as /tbf/tb0125.pp }
{$ifdef fpc}
{$mode delphi}
{$endif}
type

  tinterface = interface
      procedure x;
  end;

  tderivedinterface = interface(tinterface)
      procedure x;
  end;

  procedure testintparam(var i  : tinterface);
   begin
   end;

var
 t1 : tderivedinterface;
begin
  testintparam(t1);
end.
