{ %FAIL }

{ overriding methods is forbidden in mode objfpc }
program thlp5;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uhlp3;

type
  TObjectHelperSub = class helper(TObjectHelper) for TObject
    procedure TestOverride; override;
  end;

procedure TObjectHelperSub.TestOverride;
begin
end;

begin
end.
