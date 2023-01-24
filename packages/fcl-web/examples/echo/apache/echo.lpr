Library echo;

{$mode objfpc}{$H+}

{ $define use_apache22}

Uses
{$ifdef unix}
  cthreads, cwstring,
{$endif}
{$ifdef use_apache22}
  httpd,fpApache,
{$else}
  httpd24,fpApache24,
{$endif}
  wmecho;

Const

{ The following constant is used to export the module record. It must 
  always match the name in the LoadModule statement in the apache
  configuration file(s). It is case sensitive !}
  ModuleName='mod_fpcecho';

{ The following constant is used to determine whether the module will
  handle a request. It should match the name in the SetHandler statement
  in the apache configuration file(s). It is not case sensitive. }

  HandlerName=ModuleName;

Var
  DefaultModule : module; {$ifdef unix} public name ModuleName;{$endif unix}

Exports defaultmodule name ModuleName;

{$R *.res}

begin
  Application.ModuleName:=ModuleName;
  Application.HandlerName:=HandlerName;
  Application.SetModuleRecord(DefaultModule);
  Application.Initialize;
end.

