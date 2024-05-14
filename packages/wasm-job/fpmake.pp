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

    P:=AddPackage('wasm-job');
    P.Dependencies.Add('rtl-objpas');
    P.ShortName:='wasmjob';
    P.Description := 'Javascript Object Bindings units for webassembly.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SourcePath.Add('src');
    T:=P.Targets.AddUnit('job.shared.pas');
    T:=P.Targets.AddUnit('job.stub.pas',AllCPUs-[wasm32],AllOSes);
    T.Dependencies.AddUnit('job.shared');
    T:=P.Targets.AddUnit('job.js.pas',[wasm32],AllOSes);
    T.Dependencies.AddUnit('job.shared');
    P.NamespaceMap:='namespaces.lst';
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
