{$mode objfpc}{$H+}
program fpmake;

 { Generated automatically by fppkg on 19-2-07 }

uses fpmktype,fpmkunit;

Var
  T : TTarget;

begin
  With Installer do 
    begin
    { 
      fpmake
    } 
    StartPackage('fpmake');
    Version:='2.0.0';
    T:=Targets.AddUnit('fpmktype');
    T:=Targets.AddUnit('fpmkunit');
    EndPackage;
    Run;
    end;
end.

