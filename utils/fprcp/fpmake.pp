{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fprcp(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-fprcp');
    P.ShortName:='fprcp';

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'fprcp.exe extracts from C header and Pascal files included into resource '+
                     'scripts numerical constants and replaces these constants to its values '+
                     'in resource script.';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.0.5';

    T:=P.Targets.AddProgram('fprcp.pp');
    T.Dependencies.AddUnit('comments');
    T.Dependencies.AddUnit('expr');
    T.Dependencies.AddUnit('pasprep');

    P.Targets.AddUnit('comments.pp').install:=false;
    P.Targets.AddUnit('expr.pp').install:=false;
    P.Targets.AddUnit('pasprep.pp').install:=false;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fprcp('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




