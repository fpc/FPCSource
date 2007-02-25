{ Make net directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='net';
Targets.DefaultOS:=[linux,freebsd,netbsd,openbsd];
T:=Targets.AddUnit('servlets');
T:=Targets.AddUnit('fpsock');
T.ResourceStrings:=True;
T:=Targets.AddUnit('httpbase');
T:=Targets.AddUnit('httpclient');
T:=Targets.AddUnit('httpsvlt');
T.ResourceStrings:=True;
T:=Targets.AddUnit('xmlrpc');
Targets.AddProgram('mkxmlrpc');
T.ResourceStrings:=True;
