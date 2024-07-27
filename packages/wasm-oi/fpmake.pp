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
    P:=AddPackage('wasm-oi');
    P.Dependencies.Add('rtl-objpas');
    P.ShortName:='wasmoi';
    P.Description := 'Javascript Object Inspector Bindings units for webassembly.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.OSes:=[wasi];
    P.CPUs:=[wasm32];
    P.SourcePath.Add('src');
    T:=P.Targets.AddUnit('wasm.debuginspector.shared.pas');

    T:=P.Targets.AddUnit('wasm.debuginspector.api.pas');
      T.Dependencies.AddUnit('wasm.debuginspector.shared');

    T:=P.Targets.AddUnit('wasm.debuginspector.rtti.pas');
      T.Dependencies.AddUnit('wasm.debuginspector.api');
      T.Dependencies.AddUnit('wasm.debuginspector.shared');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
