{ %norun }
{ %fail }
{$mode delphi}
unit tb0191;

interface

type
  tii = interface(iunknown) end;
  ti2 = interface(tii) end;

function a(b: longint): tii; stdcall;

implementation

function a:tii;
begin
end;

end.
