{$mode objfpc}{$H+}
{$DEFINE EXTRAPACKAGES}
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

