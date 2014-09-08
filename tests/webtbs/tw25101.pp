program tw25101.pp;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TTest = class(TObject)
  public
    procedure Test(); virtual; deprecated 'Do not use this method'; abstract;
  end;

begin
end.
