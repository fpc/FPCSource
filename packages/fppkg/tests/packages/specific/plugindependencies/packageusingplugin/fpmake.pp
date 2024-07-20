{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('packageusingplugin');
    P.Version:='3.2.4-rc1';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'A package that depends on a plugin';

    P.Dependencies.Add('fpmkunit');

    // In principle this is strange: when a package depends on a plugin, it should
    // have this plugin explicitely added as a dependency. But there are also
    // optional plugins which are automatically used when they are available.
    // Here we have a non-optional plugin, but without an explicit dependency
    // to test if the plugin is added automatically, as an optional plugin.
    TestPlugin;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('packageusingpluginunit.pas');
    Run;
    end;
end.
