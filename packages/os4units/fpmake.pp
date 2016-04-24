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

    P:=AddPackage('os4units');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';

    P.Author := 'Marcus Sackrow';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A set of units for Amiga OS 4. PowerPC only.';
    P.NeedLibC:= false;

    P.CPUs:=[powerpc];
    P.OSes:=[Amiga];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('exec.pas');
    T:=P.Targets.AddUnit('timer.pas');
    T:=P.Targets.AddUnit('utility.pas');
    T:=P.Targets.AddUnit('amigados.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
