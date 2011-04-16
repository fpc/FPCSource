{ %NORUN }

{ test that virtual methods can be defined in mode Delphi }
program thlp3;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp3;

type
  TObjectHelperSub = class helper(TObjectHelper) for TObject
    procedure TestOverride; override;
    procedure TestFinal; override; final;
  end;

procedure TObjectHelperSub.TestOverride;
begin
end;

procedure TObjectHelperSub.TestFinal;
begin
end;

begin
end.
