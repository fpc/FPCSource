{ %NORUN }

program tw26749;

{$mode delphi}
{$modeswitch advancedrecords}

type

	{ TVector3 }

  TVector3<T> = record
    class function null : TVector3<T>; static;
  end;

  TLine<T> = array[0..1] of TVector3<T>;

{ TVector3<T> }

class function TVector3<T>.null : TVector3<T>;
begin

end;

begin
end.

