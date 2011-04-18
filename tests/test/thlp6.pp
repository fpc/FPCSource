{ %FAIL }

{ overriding methods is forbidden in mode objfpc }
program thlp6;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uhlp3;

type
  TObjectHelperSub = class helper(TObjectHelper) for TObject
    procedure TestFinal; override; final;
  end;

procedure TObjectHelperSub.TestFinal;
begin
end;

begin
end.

