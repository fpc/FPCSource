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

    P:=AddPackage('fastcgi');
{$ifdef ALLPACKAGES}
    P.Directory:='fastcgi';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'FastCGI header translation to Pascal';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fastcgi.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
