const
  BufSize = 2048;

var
  f : file;
  res : longint;
  buf : array [0..BufSize-1] of byte;
  result : word;
begin
assign(f,paramstr(0));
{$I-}
reset(f,1);
res:=IOResult;
{$I+}
if res=0 then
  Writeln('It is possible to open the executable in Read/Write mode')
else
  begin
    filemode:=0;
    {$I-}
    reset(f,1);
    res:=IOResult;
    {$I+}
    if res=0 then
      Writeln('It is only possible to open the executable in Read mode')
    else
      Writeln('It is not possible to open the executable in Read mode');
  end;
if res=0 then
  begin
{$I-}
    blockread(f,buf,sizeof(buf),result);
    res:=IOResult;
{$I+}
    if res<>0 then
      Writeln('Problem reading executable');
    if res=0 then
      close(f)
    else
      RunError(res);
  end
else
  RunError(res);
end.
