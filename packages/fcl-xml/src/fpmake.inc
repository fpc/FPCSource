{ Make XML directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='xml';
T:=Targets.AddUnit('sax');
T.ResourceStrings:=True;
T:=Targets.AddUnit('dom');
T:=Targets.AddUnit('sax_html');
T:=Targets.AddUnit('dom_html');
T:=Targets.AddUnit('xmlcfg');
T:=Targets.AddUnit('xmlread');
T:=Targets.AddUnit('xmlstreaming');
T:=Targets.AddUnit('xmlwrite');
T:=Targets.AddUnit('xhtml');
T:=Targets.AddUnit('htmldefs');
T:=Targets.AddUnit('htmwrite');
T:=Targets.AddUnit('xpath');
T:=Targets.AddUnit('htmlelements');
T:=Targets.AddUnit('htmlwriter');
