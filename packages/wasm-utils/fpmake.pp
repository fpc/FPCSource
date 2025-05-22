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
    P.OSes:=[wasip1,wasip1threads];
    P.CPUs:=[wasm32];
    P.SourcePath.Add('src');
    // Logger
    T:=P.Targets.AddUnit('wasm.logger.api.pas');
    // Memutils
    T:=P.Targets.AddUnit('wasm.memutils.pas');
    
    // Timer
    T:=P.Targets.AddUnit('wasm.timer.shared.pas');
    T:=P.Targets.AddUnit('wasm.timer.api.pas');
      T.Dependencies.AddUnit('wasm.timer.shared');
      T.Dependencies.AddUnit('wasm.logger.api');
    T:=P.Targets.AddUnit('wasm.timer.objects.pas');
      T.Dependencies.AddUnit('wasm.timer.api');
      T.Dependencies.AddUnit('wasm.logger.api');
      
    // HTTP
    T:=P.Targets.AddUnit('wasm.http.shared.pas');

    T:=P.Targets.AddUnit('wasm.http.api.pas');
      T.Dependencies.AddUnit('wasm.http.shared');
      
    T:=P.Targets.AddUnit('wasm.http.objects.pas');
      T.Dependencies.AddUnit('wasm.http.api');
      T.Dependencies.AddUnit('wasm.http.shared');

    // Websocket
    T:=P.Targets.AddUnit('wasm.websocket.shared.pas');
    
    T:=P.Targets.AddUnit('wasm.websocket.api.pas');
      T.Dependencies.AddUnit('wasm.websocket.shared');
      T.Dependencies.AddUnit('wasm.timer.api');
      
    T:=P.Targets.AddUnit('wasm.websocket.objects.pas');
      T.Dependencies.AddUnit('wasm.websocket.api');
      T.Dependencies.AddUnit('wasm.websocket.shared');
    
    // Regexp  
    T:=P.Targets.AddUnit('wasm.regexp.shared.pas');
    
    T:=P.Targets.AddUnit('wasm.regexp.api.pas');
      T.Dependencies.AddUnit('wasm.regexp.shared');
      
    T:=P.Targets.AddUnit('wasm.regexp.objects.pas');
      T.Dependencies.AddUnit('wasm.regexp.api');
      T.Dependencies.AddUnit('wasm.regexp.shared');
    T:=P.Targets.AddUnit('wasm.pcrebridge.pas');
      T.Dependencies.AddUnit('wasm.regexp.api');
      T.Dependencies.AddUnit('wasm.regexp.shared');
      T.Dependencies.AddUnit('wasm.regexp.objects');
    T:=P.Targets.AddUnit('wasm.exceptions.pas');

    // Storage
    T:=P.Targets.AddUnit('wasm.storage.shared.pas');
    T:=P.Targets.AddUnit('wasm.storage.api.pas');
      T.Dependencies.AddUnit('wasm.storage.shared');
    T:=P.Targets.AddUnit('wasm.storage.objects.pas');
      T.Dependencies.AddUnit('wasm.storage.shared');
      T.Dependencies.AddUnit('wasm.storage.api');
    
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
