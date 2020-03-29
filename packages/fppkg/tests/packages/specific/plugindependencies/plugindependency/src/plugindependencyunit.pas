unit PluginDependencyUnit;

{$mode objfpc}{$H+}

interface

uses
  fpmkunit;

procedure PluginDependencyUnitProc;

implementation

type
  TCustomInstallerHack = class(TCustomInstaller);

procedure PluginDependencyUnitProc;
begin
  TCustomInstallerHack(Installer).Log(vlInfo, 'The plugin-dependency is used');
end;

end.

