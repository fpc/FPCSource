{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

var
  T : TTarget;
begin
  with Installer do
    begin
      {$i fpmake.inc}
      Run;
    end;
end.

