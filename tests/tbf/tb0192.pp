{ %norun }
{ %fail }
{$mode delphi}
unit tb0192;

interface

type
  tii = interface(iunknown) end;
  ti2 = interface(iunknown) end;

function a: tii; stdcall;

implementation

function a:ti2;
begin
end;

end.
