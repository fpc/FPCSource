{ %FAIL }

{ virtual methods are forbidden in mode objfpc }
program thlp4;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    procedure TestVirtual; virtual;
  end;

procedure TObjectHelper.TestVirtual;
begin
end;

begin
end.
