{ Old file: tbs0142.pp }
{ sizeof(object) is not tp7 compatible when no constructor is used OK 0.99.9 (PM) }


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
