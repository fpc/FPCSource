{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('libjack');
    P.ShortName:='ljack';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'Library: libjack';
    P.License := 'Audio Server: GPL, Library: LGPL';
    P.HomepageURL := 'https://jackaudio.org/';
    P.Email := '';
    P.Description := 'Headers for the JACK Audio Connection Kit library';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes-[qnx];
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('jack.pp');
    with T.Dependencies do
      begin
        AddInclude('systemdeps.inc');
        AddInclude('transport.inc');
        AddInclude('types.inc');
        AddInclude('weakmacros.inc');
      end;

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
