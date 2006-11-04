{ Make lnet directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='lnet';
{Targets.IncludeDir:='sys'; ??}
Targets.DefaultOS:=[linux,freebsd,netbsd,openbsd,win32,win64];
T:=Targets.AddUnit('lnet');
T:=Targets.AddUnit('lcommon');
T:=Targets.AddUnit('levents');
T:=Targets.AddUnit('lcontrolstack');
T:=Targets.AddUnit('ltelnet');
T:=Targets.AddUnit('lftp');
T:=Targets.AddUnit('lsmtp');
T:=Targets.AddUnit('lhttputil');
T:=Targets.AddUnit('lhttp');
T:=Targets.AddUnit('lmimetypes');
T:=Targets.AddUnit('lprocess');
T:=Targets.AddUnit('lstrbuffer');
T:=Targets.AddUnit('fastcgi');
T:=Targets.AddUnit('lfastcgi');
T:=Targets.AddUnit('lspawnfcgi');
T:=Targets.AddUnit('lwebserver');
