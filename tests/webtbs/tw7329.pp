{$MODE DELPHI}
unit tw7329;

interface

type
  IDirect3D9 = interface(IUnknown) end;
// IDirect3D9 = Integer; //todo: Uncomment this line and comment previous one to successfully compile with 2.0.4

function Direct3DCreate9(SDKVersion: LongWord): IDirect3D9; stdcall;

implementation

function Direct3DCreate9(SDKVersion: LongWord): IDirect3D9;
begin
end;

end.