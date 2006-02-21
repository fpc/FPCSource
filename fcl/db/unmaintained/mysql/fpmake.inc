{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='db/mysql';
Targets.DefaultOS:=[win32,openbsd,netbsd,freebsd,darwin,linux];
T:=Targets.AddUnit('mysqldb4');
T.ResourceStrings:=True;
T:=Targets.AddUnit('mysqldb3');
T.ResourceStrings:=True;
Targets.Addexampleunit('mtest');
if Defaults.OS in Targets.DefaultOS then
  begin
  CleanFiles.add('mysql.o');
  CleanFiles.add('mysql.ppu');
  CleanFiles.add('mysql_com.o');
  CleanFiles.add('mysql_com.ppu');
  CleanFiles.add('mysql_version.o');
  CleanFiles.add('mysql_version.ppu');
  end;
