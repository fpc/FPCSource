{ %FAIL }
{$MODE objfpc}
{$R+}
type
  TMyRec = record
    x: Integer;
  end;
  TMyArray = array[Ptrint] of TMyRec;
  PMyArray = ^TMyArray;
var
  a: PMyArray;
  i: Integer;
begin
  GetMem(a, SizeOf(TMyRec));
  i := 0;
  a^[i].x := 1;
end.
