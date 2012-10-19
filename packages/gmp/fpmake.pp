{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('gmp');
{$ifdef ALLPACKAGES}
    P.Directory:='gmp';
{$endif ALLPACKAGES}
    P.Version:='2.6.2rc1';

    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'GMP';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gmp.pas');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
