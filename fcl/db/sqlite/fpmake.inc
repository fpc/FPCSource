{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='db/sqlite';
T:=Targets.AddUnit('sqliteds');
