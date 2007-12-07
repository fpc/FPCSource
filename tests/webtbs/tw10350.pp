{ %norun }

program TestLocal;
var i: longint;
begin
{$R+}
{$ifopt R+}
{$define local_RangeCheck}
{$R-}
{$endif}
  i:= longword( -1);
{$ifdef local_RangeCheck}
{$R+}
{$endif}
end.
