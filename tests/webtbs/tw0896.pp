
var
  dat,dat2 : file of byte;
  j : longint;
  Buffer,Buffer2 : Array[0..2047] of byte;

begin
  for j:=0 to 2047 do
    Buffer[j]:=j and $ff;
  Assign(dat,'tbug896.tmp');
  Rewrite(dat,1);
  for j:= 0 to 2047 do
    write (dat,Buffer[j]);
  Close(dat);
  Assign(dat2,'tbug896a.tmp');
  Rewrite(dat2);
  for j:= 0 to 2047 do
    write (dat2,Buffer[j]);
  Close(dat2);
  Reset(dat);
  Reset(dat2,1);
  for j:=0 to 2047 do
    begin
      read(dat,Buffer[j]);
      read(dat2,Buffer2[j]);
      if Buffer[j]<>Buffer2[j] then
        begin
          Writeln('Error in typed file handling');
          Halt(1);
        end;
    end;
  Close(dat);
  close(dat2);
  Erase(dat);
  Erase(dat2);
end.
