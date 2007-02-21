{$mode objfpc}{$H+}
program fpmake;

 { Generated automatically by fppkg on 19-2-07 }

uses fpmktype,fpmkunit;

Var
  T : TTarget;
  S : TSource;
begin
  With Installer do 
    begin
    { 
      fpmake
    } 
    StartPackage('fpmake');
    Version:='2.0.0';
    Description:='Free Pascal Make Tool';
    T:=Targets.AddUnit('fpmktype');
    T:=Targets.AddUnit('fpmkunit');
    S:=Sources.AddSrcFiles('*.pp');
    EndPackage;
    Run;
    end;
end.

