{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

{ Interbase directory } 
Targets.ResetDefaults;
Targets.DefaultDir:='db/interbase';
Targets.DefaultCPU:=[i386];
Targets.DefaultOS:=[win32,openbsd,netbsd,freebsd,linux];
T:=Targets.AddUnit('interbase');
T:=Targets.AddExampleUnit('testib');
if Defaults.OS in Targets.DefaultOS then
  begin
  CleanFiles.add('ibas40.o');
  CleanFiles.add('ibas40.ppu');
  CleanFiles.add('ibase60.o');
  CleanFiles.add('ibase60.ppu');
  end;
