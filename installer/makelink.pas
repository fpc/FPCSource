
program MakeLink;


uses
  Strings,
  Windows,
  WinShell;


procedure ReadFailure;
begin
  Writeln('Reading of config file failed');
  Halt(1);
end;

var
  DesktopPath : PChar;
  Line,
  Typ : String;
  ExeName,
  LinkName,
  IconName,
  Comment,
  WorkingDir : Array [0..MAX_PATH] of char;
  f : text;

begin
  DesktopPath := StrAlloc (MAX_PATH);

  { How could we know if the installer as access to global
    desktop dir ?? }
  GetDesktopFolder (true, DesktopPath);

  if (ParamCount=2) and (ParamStr(1)='-F') then
    begin
      Assign(f,ParamStr(2));
      Reset(f);
      if IOResult<>0 then
        ReadFailure;
      while not eof(f) do
        begin
          readln(f,line);
          typ:=copy(line,1,pos('=',line)-1);
          line:=copy(line,pos('=',line)+1,high(line));
          if typ='ExePath' then
            StrPCopy(ExeName,Line);
          if typ='LinkName' then
            StrPCopy(LinkName,Line);
          if typ='IconPath' then
            StrPCopy(IconName,Line);
          if typ='Comment' then
            StrPCopy(Comment,Line);
          if typ='WorkingDir' then
            StrPCopy(WorkingDir,Line);
        end;
    end;
  StrCat (DesktopPath, '\');
  StrCat (DesktopPath,LinkName);
  CreateShortcut (DesktopPath,
    ExeName,
    nil,
    WorkingDir,
    Comment,
    IconName,
    0);
  StrDispose (DesktopPath);
end.
