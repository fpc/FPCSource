unit PluginUnit;

{$mode objfpc}{$H+}

interface

uses
  fpmkunit,
  PluginDependencyUnit;

procedure TestPlugin;

implementation

procedure TestPlugin;
begin
  PluginDependencyUnitProc;
end;

end.

