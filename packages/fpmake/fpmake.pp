{$mode objfpc}{$H+}
program fpmake;

uses fpmktype,fpmkunit;

var
  T : TTarget;
  S : TSource;
begin
  with Installer do
    begin
      {$i fpmake.inc}
      Run;
    end;
end.

