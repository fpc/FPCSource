{$mode objfpc}
{$H+}
{$define FULLSQLDB}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
   
begin
  ChangeDir('../..');
  With Installer do 
    begin
    {$i ../../fclmake.inc}
    {$i fpmake.inc}
    EndPackage;
    Run;
    end;
end.

