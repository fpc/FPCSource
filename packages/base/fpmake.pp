{$mode objfpc}{$H+}
{$DEFINE BASEPACKAGES}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    { Base packages }
    {$i fpmake.inc}
    Run;
    end;
end.

