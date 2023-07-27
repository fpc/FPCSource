program regeninc;

uses Sysutils;

const
  data2incname = 'data2inc';
  ininame = 'fpcmake.ini';
  incname = 'fpcmake.inc';
  constname = 'fpcmakeini';
  
var
  Data2Inc : string;
  res : integer;

begin
  Data2Inc:=ExeSearch('data2inc',GetEnvironmentVariable('PATH'));
  if Data2Inc='' then
    begin
    Writeln('Data2inc not found');
    Halt(1);
    end;
  if not FileExists(ininame) then
    begin
    Writeln('file ',ininame,' not found, this file must be in the current directory');
    Halt(2);
    end;
  Res:=ExecuteProcess(data2inc,['-b','-s',ininame,incname,constname]);
  if Res<>0 then 
    Halt(Res);
end.