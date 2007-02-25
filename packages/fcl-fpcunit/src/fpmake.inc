{ Make FPCUnit directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='fpcunit';
T:=Targets.AddUnit('fpcunit');
T.ResourceStrings:=true;
T:=Targets.AddUnit('testregistry');
T:=Targets.AddUnit('testreport');
T:=Targets.AddUnit('testutils');
T:=Targets.AddUnit('testdecorator');
T:=Targets.AddUnit('ubmockobject');
