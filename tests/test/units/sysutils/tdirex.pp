program test_directoryexists;

{$H+}

uses
  dos, sysutils;

{$I+}

const
  HasErrors : boolean = false;
  AllowTrailingSeparators: boolean = false;

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
  backslashpos,slashpos,maxpos,i : longint;
begin
  while true do
    begin
      backslashpos:=0;
      for i:=length(dir) downto 1 do
        if dir[i]='\' then
          begin
            backslashpos:=i;
            break;
          end;
      slashpos:=0;
      for i:=length(dir) downto 1 do
        if dir[i]='/' then
          begin
            slashpos:=i;
            break;
          end;
      if (backslashpos=0) and (slashpos=0) then
        exit;
      if slashpos>backslashpos then
        maxpos:=slashpos
      else
        maxpos:=backslashpos;
      dir:=copy(dir,1,maxpos);
      TestDirectoryExists(dir,AllowTrailingSeparators);
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
  if DirectoryExists(P+DirectorySeparator) and
     DirectoryExists(P+DirectorySeparator+DirectorySeparator) then
    AllowTrailingSeparators:=true;

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
  TestDirectoryExists(dir1+DirectorySeparator,AllowTrailingSeparators);
  TestDirectoryExists(dir1+'/',AllowTrailingSeparators);
  TestDirectoryExists(dir1+'//',AllowTrailingSeparators);
  if DirectorySeparator='\' then
    TestDirectoryExists(dir1+'\\',AllowTrailingSeparators);
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


