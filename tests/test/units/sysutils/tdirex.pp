program test_directoryexists;

{$H+}

uses
  dos, sysutils;

{$I+}

const
  HasErrors : boolean = false;
  AllowOneTrailingSeparator: boolean = false;
  AllowMultipleTrailingSeparators: boolean = false;

procedure TestDirectoryExists(Const DirName : string; ExpectedResult : boolean);
var
  res : boolean;
begin
  Writeln('Testing "',DirName,'"');
  res:=Sysutils.DirectoryExists (DirName);
  if res <> ExpectedResult then
    begin
      Writeln('Unexpected result: ',res,' for "',DirName,'"');
      HasErrors:=true;
    end;
end;

procedure TestParents(var dir : string);
var
  sep_pos,maxpos,i : longint;
begin
  while true do
    begin
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
      TestDirectoryExists(dir,AllowOneTrailingSeparator);
      if length(dir)>1 then
        begin
          dir:=copy(dir,1,maxpos-1);
          TestDirectoryExists(dir,true);
        end
      else
      { if length(dir)<=1, exit test }
        exit;
    end;
end;

var
  dir,dir1,dir2,StoredDir : string;
  P,N,E : shortstring;
  ch : char;
begin
  Dos.FSplit(paramstr(0),P,N,E);
  Writeln('Path="',P,'"');
  Writeln('Name="',N,'"');
  Writeln('Ext="',E,'"');
  Writeln('DirectorySeparator="',DirectorySeparator,'"');
  Write('AllowDirectorySeparators="');
  for ch:=low(char) to high(char) do
    if ch in AllowDirectorySeparators then
      Write(ch);
  Writeln('"');

{ The following would be already tested at the beginning of TestParents
  TestDirectoryExists(P,true);
}
{ The following check wouldn't work correctly if running the test executable
  from a root drive - not a typical case, but still worth mentioning... }
  if DirectoryExists(P) then
   AllowOneTrailingSeparator:=true
  else
   WriteLn ('Warning: Some code may expect support for a trailing directory separator!');
  if DirectoryExists(P+DirectorySeparator) then
    AllowMultipleTrailingSeparators:=true;

  dir:=P;
  Writeln('Calling TestParents with dir="',dir,'"');
  TestParents(dir);
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
      Writeln('Calling TestParents with dir="',dir,'" from directory '
                                               + Copy (Dir, 1, 3) + ' (root)');
      TestParents(dir);
      ChDir(StoredDir);
    end;
{$ELSE UNIX}
  GetDir(0,StoredDir);
  ChDir(DirectorySeparator);
  Writeln('Calling TestParents with dir="',dir,'" from directory '
                                             + DirectorySeparator + ' (root)');
  TestParents(dir);
  ChDir(StoredDir);
{$ENDIF UNIX}
  dir:=P+'_Dummy';
  TestDirectoryExists(dir,false);
  dir1:=P+'_Dummy'+DirectorySeparator;
  TestDirectoryExists(dir1,false);
  mkdir(dir);
  TestDirectoryExists(dir,true);
  TestDirectoryExists(dir1,AllowOneTrailingSeparator);
  { Check that using two directory separators fails }
  TestDirectoryExists(dir1+DirectorySeparator,AllowMultipleTrailingSeparators);
  if ('/' in AllowDirectorySeparators) and ('/' <> DirectorySeparator) then
   begin
    TestDirectoryExists(dir+'/',AllowOneTrailingSeparator);
    TestDirectoryExists(dir1+'/',AllowMultipleTrailingSeparators);
    TestDirectoryExists(dir1+'//',AllowMultipleTrailingSeparators)
   end;
  TestDirectoryExists (dir1 + DirectorySeparator + DirectorySeparator, AllowMultipleTrailingSeparators);
  dir2:=dir1+'_Dummy2';
  TestDirectoryExists(dir2,false);
  mkdir(dir2);
  TestDirectoryExists(dir2,true);
  rmdir(dir2);
  rmdir(dir);
  TestDirectoryExists(dir,false);
  TestDirectoryExists(dir1,false);
  if HasErrors then
    begin
      Writeln('Program encountered errors');
      Halt(1);
    end
  else
   WriteLn ('All OK');
end.


