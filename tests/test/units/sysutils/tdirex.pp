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
begin
  Dos.FSplit(paramstr(0),P,N,E);
  Writeln('Path="',P,'"');
  Writeln('Name="',N,'"');
  Writeln('Ext="',E,'"');
  Writeln('DirectorySeparator="',DirectorySeparator,'"');
  TestDirectoryExists(P,true);

  if DirectoryExists(P+DirectorySeparator) then
    AllowOneTrailingSeparator:=true;
  if DirectoryExists(P+DirectorySeparator) and
     DirectoryExists(P+DirectorySeparator+DirectorySeparator) then
    AllowMultipleTrailingSeparators:=true;

  dir:=P;
  TestParents(dir);
  dir:=P;
  if (length(dir)>2) and (dir[2]=':') and (dir[3]=DirectorySeparator) then
    begin
      GetDir(0,StoredDir);
      Writeln('Testing from Root drive');
      ChDir(Copy(Dir,1,3));
      TestParents(dir);
      ChDir(StoredDir);
    end;
  dir:=P+'_Dummy';
  TestDirectoryExists(dir,false);
  dir1:=P+'_Dummy'+DirectorySeparator;
  TestDirectoryExists(dir1,false);
  mkdir(dir);
  TestDirectoryExists(dir,true);
  TestDirectoryExists(dir1,true);
  { Check that using two directory separators fails }
  TestDirectoryExists(dir1+DirectorySeparator,AllowOneTrailingSeparator);
  TestDirectoryExists(dir1+'/',AllowOneTrailingSeparator);
  TestDirectoryExists(dir1+'//',AllowMultipleTrailingSeparators);
  if DirectorySeparator='\' then
    TestDirectoryExists(dir1+'\\',AllowMultipleTrailingSeparators);
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
    end;
end.


