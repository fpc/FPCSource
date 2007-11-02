uses
  bench,sysutils;
var
  i : longint;
  ps : ansistring;
  sticks : int64;
begin
  if paramcount>0 then
    begin
      for i:=2 to paramcount do
        ps:=ps+' '+paramstr(i);
      sticks:=GetMicroSTicks;
      ExecuteProcess(paramstr(1),ps);
      writeln(stderr,(GetMicroSTicks-sticks)/1000:0:3,' ms');
    end;
end.
