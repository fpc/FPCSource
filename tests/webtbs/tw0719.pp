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
    r:=FileAge(SR.Name);
    if r<>-1 then
      begin
        S:=DateTimeToStr(FileDateToDateTime(r));
        Writeln(SR.Name,' has Date ',S);
      end;
    r:=FindNext(SR);
  end;
FindClose(SR);
end.
