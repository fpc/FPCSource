{ %NORUN }

program tw29321;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TVector = array[0..2] of Single;
  
  TVectorHelper = type helper for TVector
  public
    procedure Add;
  end;

procedure TVectorHelper.Add;
begin
end;

var
  v: TVector;
begin
  v.Add;
end.
