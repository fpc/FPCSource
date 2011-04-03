unit uhlp3;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TObjectHelper = class helper for TObject
    procedure TestOverride; virtual;
    procedure TestFinal; virtual;
  end;

implementation

procedure TObjectHelper.TestOverride;
begin
end;

procedure TObjectHelper.TestFinal;
begin
end;

end.
