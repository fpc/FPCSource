{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    {$i fpmake.inc}
    Run;
    end;
end.

