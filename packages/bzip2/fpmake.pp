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

    P:=AddPackage('bzip2');
{$ifdef ALLPACKAGES}
    P.Directory:='bzip2';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('bzip2.pas');
      with T.Dependencies do
        begin
          AddInclude('bzip2i386.inc',[i386],AllOSes);
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
