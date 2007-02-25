{ Make passrc directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='passrc';
T:=Targets.AddUnit('pastree');
T.ResourceStrings:=True;
T:=Targets.AddUnit('pscanner');
T.ResourceStrings:=True;
T:=Targets.AddUnit('pparser');
T.ResourceStrings:=True;
T:=Targets.AddUnit('paswrite');
