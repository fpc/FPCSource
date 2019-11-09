unit utdirex;

interface
{$mode objfpc}
{$H+}

uses
  sysutils;

implementation

uses punit,utrtl;

{$I+}

const
  AllowOneTrailingSeparator: boolean = false;
  AllowMultipleTrailingSeparators: boolean = false;

Function TestDirectoryExists(Test : Integer;Const DirName : string; ExpectedResult : boolean) : Boolean;

begin
  Result:=AssertEquals('Test '+IntToStr(Test),ExpectedResult,Sysutils.DirectoryExists (DirName));
end;

Function TestParents(BaseN : Integer;var dir : string) : Boolean;

var
  sep_pos,maxpos,i : longint;
  N : integer;
begin
  Result:=True;
  N:=0;
  while Result do
    begin
    Inc(N);
    sep_pos:=0;
    for i:=length(dir) downto 1 do
      if dir[i] in AllowDirectorySeparators then
        begin
        sep_pos:=i;
        break;
        end;
    if (sep_pos=0) then
      exit;
    maxpos:=sep_pos;
    dir:=copy(dir,1,maxpos);
    Result:=TestDirectoryExists(BaseN+2*N,dir,AllowOneTrailingSeparator);
    if Result and (length(dir)>1) then
      begin
      dir:=copy(dir,1,maxpos-1);
      Result:=TestDirectoryExists(BaseN+2*N+1,dir,true);
      end
    else
      exit;
    end;
end;

Function DoTestDirectoryExists : AnsiString;

var
  dir,dir1,dir2,StoredDir : string;
  P: shortstring;
  ch : char;
begin
  Result:='';
  StoredDir:='';
  P:=ExtractFilePath(paramstr(0));
  if ShowDebugOutput then
    begin
    Writeln('Path="',P,'"');
    Writeln('DirectorySeparator="',DirectorySeparator,'"');
    Write('AllowDirectorySeparators="');
    for ch:=low(char) to high(char) do
      if ch in AllowDirectorySeparators then
        Write(ch);
    Writeln('"');
    end;

{ The following would be already tested at the beginning of TestParents
  TestDirectoryExists(P,true);
}
{ The following check wouldn't work correctly if running the test executable
  from a root drive - not a typical case, but still worth mentioning... }
  if DirectoryExists(P) then
   AllowOneTrailingSeparator:=true
  else if ShowDebugOutput then
     WriteLn ('Warning: Some code may expect support for a trailing directory separator!');
  if DirectoryExists(P+DirectorySeparator) then
    AllowMultipleTrailingSeparators:=true;

  dir:=P;
  if ShowDebugOutput then
    Writeln('Calling TestParents with dir="',dir,'"');
  TestParents(100,dir);
  dir:=P;
{$IFDEF MACOS}
 {$WARNING The following test is wrong for Mac OS!}
{$ENDIF MACOS}
{$IFDEF AMIGA}
 {$WARNING The following test is wrong for Amiga (volumes are not detected properly)!}
{$ENDIF AMIGA}
{$IFDEF NETWARE}
 {$WARNING The following test is wrong for Amiga (volumes are not detected properly)!}
{$ENDIF NETWARE}
{$IFNDEF UNIX}
  if (length(dir)>2) and (dir[2]= DriveSeparator) and (dir[3]=DirectorySeparator) then
    begin
      GetDir(0,StoredDir);
      ChDir(Copy(Dir,1,3));
      if ShowDebugOutput then
        Writeln('Calling TestParents with dir="',dir,'" from directory '
                                               + Copy (Dir, 1, 3) + ' (root)');
      TestParents(200,dir);
      ChDir(StoredDir);
    end;
{$ELSE UNIX}
  GetDir(0,StoredDir);
  ChDir(DirectorySeparator);
  if ShowDebugOutput then
    Writeln('Calling TestParents with dir="',dir,'" from directory '
                                             + DirectorySeparator + ' (root)');
  if not TestParents(200,dir) then exit;
  ChDir(StoredDir);
{$ENDIF UNIX}
  dir:=P+'_Dummy';
  if not TestDirectoryExists(1,dir,false) then exit;
  dir1:=P+'_Dummy'+DirectorySeparator;
  if not TestDirectoryExists(2,dir1,false) then exit;
  mkdir(dir);
  if not TestDirectoryExists(3,dir,true) then exit;
  if not TestDirectoryExists(4,dir1,AllowOneTrailingSeparator) then exit;
  { Check that using two directory separators fails }
  if not TestDirectoryExists(5,dir1+DirectorySeparator,AllowMultipleTrailingSeparators) then exit;
  if ('/' in AllowDirectorySeparators) and ('/' <> DirectorySeparator) then
   begin
    if not TestDirectoryExists(6,dir+'/',AllowOneTrailingSeparator) then exit;
    if not TestDirectoryExists(7,dir1+'/',AllowMultipleTrailingSeparators) then exit;
    if not TestDirectoryExists(8,dir1+'//',AllowMultipleTrailingSeparators) then exit;
   end;
  if not TestDirectoryExists (9,dir1 + DirectorySeparator + DirectorySeparator, AllowMultipleTrailingSeparators) then exit;
  dir2:=dir1+'_Dummy2';
  if not TestDirectoryExists(10,dir2,false) then exit;
  mkdir(dir2);
  if not TestDirectoryExists(11,dir2,true) then exit;
  rmdir(dir2);
  rmdir(dir);
  if not TestDirectoryExists(12,dir,false) then exit;
  if not TestDirectoryExists(13,dir1,false) then exit;
end;

begin
  SysUtilsTest('TestDirectoryExists',@DoTestDirectoryExists);
end.


