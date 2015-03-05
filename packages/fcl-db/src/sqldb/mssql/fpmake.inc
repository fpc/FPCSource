{ Make DB directory and all subdirectories }
{ FULLSQLDB will be defined if included from SQLDB directory alone}
{ FULLDB will be defined if included from db directory alone}
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='db/sqldb/mssql';
{ Drivers only for the following OSes }
Targets.DefaultOS:=[win32,win64,openbsd,netbsd,freebsd,linux,haiku,dragonfly];
T:=Targets.AddUnit('mssqlconn');
T.ResourceStrings:=False;
