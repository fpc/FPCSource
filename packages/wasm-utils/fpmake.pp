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

    P:=AddPackage('wasm-utils');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-base');
    P.ShortName:='wasmutil';
    P.Description := 'Various utility units for webassembly.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.OSes:=[wasi];
    P.CPUs:=[wasm32];
    P.SourcePath.Add('src');
    
    T:=P.Targets.AddUnit('wasm.http.shared.pas');
    
    T:=P.Targets.AddUnit('wasm.http.api.pas');
      T.Dependencies.AddUnit('wasm.http.shared');
      
    T:=P.Targets.AddUnit('wasm.http.objects.pas');
      T.Dependencies.AddUnit('wasm.http.api');
      T.Dependencies.AddUnit('wasm.http.shared');

    T:=P.Targets.AddUnit('wasm.websocket.shared.pas');
    T:=P.Targets.AddUnit('wasm.websocket.api.pas');
      T.Dependencies.AddUnit('wasm.websocket.shared');
      
    T:=P.Targets.AddUnit('wasm.websocket.objects.pas');
      T.Dependencies.AddUnit('wasm.websocket.api');
      T.Dependencies.AddUnit('wasm.websocket.shared');
      
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
