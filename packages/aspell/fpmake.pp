{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('aspell');
{$ifdef ALLPACKAGES}
    P.Directory:='aspell';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('aspell.pp');
    T:=P.Targets.AddUnit('spellcheck.pp');
    with T.Dependencies do
      begin
        AddUnit('aspell');
       end;

    P.Sources.AddSrc('LICENSE');
    P.Sources.AddSrc('LICENSE.ADDON');

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('example.pas');
    
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
