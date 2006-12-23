{ %norun }
{$mode delphi}
unit tb0517;

interface

type
  tii = interface(iunknown) end;
  ti2 = interface(iunknown) end;

function a(b: longint): tii; stdcall;

implementation

function a;
begin
end;

end.
