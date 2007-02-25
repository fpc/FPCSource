{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }


{ db/sdf directory }
Targets.ResetDefaults;
Targets.DefaultDir:='db/sdf';
T:=Targets.AddUnit('sdfdata');
Targets.AddExampleUnit('testsdf');
Targets.AddExampleUnit('testfix');
