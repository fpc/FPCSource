
{$ifdef FPC}
{$PACKRECORDS 1}
{$endif FPC}

type
Time = object
  h,m,s:byte;
end;

var OT:Time;
 l : longint;
begin
  l:=SizeOf(OT);
  Writeln('Time object size is ',l);
  if l<>3 then halt(1);
end.
