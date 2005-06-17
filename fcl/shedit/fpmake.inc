{ Make shedit directory and all subdirectories }
{ FULLFCL will be defined if included from fcl directory }

Targets.ResetDefaults;  
Targets.DefaultDir:='shedit';
T:=Targets.AddUnit('doc_text');
T:=Targets.AddUnit('shedit');
T:=Targets.AddUnit('sh_xml');
T:=Targets.AddUnit('sh_pas');
