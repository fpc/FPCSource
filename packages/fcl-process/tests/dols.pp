program dols;

uses sysutils;

var
  Idx,Count : integer;
  Dir : String;
  Info : TSearchRec;
  Long : Boolean;


begin
  Dir:=GetCurrentDir;
  Idx:=1;
  if ParamStr(Idx)='-l' then
    begin
    Inc(Idx);
    Long:=True;
    end;
  if ParamStr(Idx)<>'' then
    Dir:=ParamStr(Idx);
  Dir:=IncludeTrailingPathDelimiter(Dir);
  Count:=0;
  If FindFirst(Dir+AllFilesMask,faAnyFile,Info)=0 then
    try
      Repeat
        if Long then
          Write(Info.Size:14,' ',DateTimeToStr(Info.TimeStamp));
        Writeln(Info.Name);
        Inc(Count);
      Until FindNext(Info)<>0;

    finally
      FindClose(Info);
    end;
  Writeln('Total: ',Count);
end.

