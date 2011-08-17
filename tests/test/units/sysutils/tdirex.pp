program test_directoryexists;

{$H+}

uses
  dos, sysutils;

const
  HasErrors : boolean = false;
  AllowsTrailingSepartors: boolean = false;

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
  slashpos:=1;
  while (backslashpos<>0) or (slashpos<>0) do
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
      TestDirectoryExists(dir,true);
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
  dir,dir1,dir2 : string;
  P,N,E : shortstring;
begin
  Dos.FSplit(paramstr(0),P,N,E);
  Writeln('Path="',P,'"');
  Writeln('Name="',N,'"');
  Writeln('Ext="',E,'"');
  TestDirectoryExists(P,true);
  if DirectoryExists(P+DirectorySeparator) and
     DirectoryExists(P+DirectorySeparator+DirectorySeparator) then
    AllowsTrailingSepartors:=true;

  dir:=P;
  TestParents(dir);
  dir:=P+'_Dummy';
  TestDirectoryExists(dir,false);
  dir1:=P+'_Dummy'+DirectorySeparator;
  TestDirectoryExists(dir1,false);
  mkdir(dir);
  TestDirectoryExists(dir,true);
  TestDirectoryExists(dir1,true);
  { Check that using two directory separators fails }
  TestDirectoryExists(dir1+DirectorySeparator,AllowsTrailingSepartors);
  TestDirectoryExists(dir1+'/',AllowsTrailingSepartors);
  TestDirectoryExists(dir1+'//',AllowsTrailingSepartors);
  if DirectorySeparator='\' then
    TestDirectoryExists(dir1+'\\',AllowsTrailingSepartors);
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


