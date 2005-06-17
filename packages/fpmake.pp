{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    { Base packages }
    {$i base/fpmake.inc}

    { Extra packages}
    {$i extra/fpmake.inc}
    Run;
    end;
end.

