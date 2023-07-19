{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('wasmtime');
    P.ShortName:='wasmtime';
    P.Description := 'Interface unit wasmtime webassembly library.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SourcePath.Add('src');
    P.OSes := [linux,darwin,win64];
    P.CPUs:=[x86_64];
    T:=P.Targets.AddUnit('wasmtime.pp');
    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('helloworld.pp');
    P.Targets.AddExampleProgram('gcd.pp');
    P.Targets.AddExampleProgram('memory.pp');
    P.Targets.AddExampleProgram('wasi.pp');
    
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
