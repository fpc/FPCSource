
{$PACKRECORDS 1}

type
Time = object
  h,m,s:byte;
end;

var OT:Time;
 l : longint;
begin
  l:=SizeOf(OT);
end.
