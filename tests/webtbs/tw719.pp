uses
  sysutils;

var
  S : string;
  SR : TSearchRec;
  r : longint;
begin
r:=FindFirst('*.*',faAnyFile,SR);
while r=0 do
  begin
    S:=DateTimeToStr(FileDateToDateTime(FileAge(SR.Name)));
    Writeln(SR.Name,' has Date ',S);
    r:=FindNext(SR);
  end;
FindClose(SR);
end.
