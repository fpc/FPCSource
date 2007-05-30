{$mode objfpc}{$H+}
program fpmake;

uses fpmktype,fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    {$i fpmake.inc}
    Run;
    end;
end.

