unit uw3340;
interface
{$MODE DELPHI}
{$INLINE ON}

type
  TTT = class
  public
    zz: Integer;
  public
    function Yes: Integer; inline;
  end;

implementation

procedure LocK; inline;
begin WriteLn('asdfasdf'); end;

function TTT.Yes: Integer;
begin
  Lock;
  Result:= zz;
end;

end.
