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
    P.OSes:=  [atari,emx,gba,go32v2,msdos,nativent,nds,netware,netwlibc,os2,sinclairql,human68k,symbian,watcom,wii,win32,win64,wince,freertos,wasip1,wasip1threads]+AllUnixOSes -[QNX]+AllAmigaLikeOSes;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [android];
    P.SourcePath.Add('src');
    T:=P.Targets.AddUnit('job.shared.pas');
    T:=P.Targets.AddUnit('job.stub.pas',AllCPUs-[wasm32],P.OSes);
    T.Dependencies.AddUnit('job.shared');
    T:=P.Targets.AddUnit('job.js.pas',[wasm32],AllOSes);
    T.Dependencies.AddUnit('job.shared');
    T:=P.Targets.AddUnit('job.threading.pas',[wasm32],AllOSes);
    T.Dependencies.AddUnit('job.js');
    P.NamespaceMap:='namespaces.lst';
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
