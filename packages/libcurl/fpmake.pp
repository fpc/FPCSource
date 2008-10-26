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

    P:=AddPackage('libcurl');
{$ifdef ALLPACKAGES}
    P.Directory:='libcurl';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Author := 'Library: Daniel Stenberg, header: Free Pascal development team';
    P.License := 'Library: MIT, header: LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Library to fetch files from URLs using many protocols.';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('libcurl.pp');

    P.ExamplePath.Add('tests');
    P.Targets.AddExampleProgram('testcurl.pp');
    P.Targets.AddExampleProgram('teststream.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
