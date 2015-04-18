{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('libenet');
    P.ShortName:='libenet';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';
    P.SourcePath.Add('src');
    P.SourcePath.Add('examples');
    P.IncludePath.Add('src');
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx];
    P.Dependencies.Add('rtl-extra'); // winsock2
    
    T:=P.Targets.AddUnit('enettypes.pp');
    T:=P.Targets.AddUnit('enetlist.pp');
    T:=P.Targets.AddUnit('enetcallbacks.pp');
    with T.Dependencies do
      AddUnit('enettypes');   
    T:=P.Targets.AddUnit('enetplatform.pp');
    T:=P.Targets.AddUnit('enetprotocol.pp');
    T:=P.Targets.AddUnit('enettime.pp');
    T:=P.Targets.AddUnit('enet.pp');
    with T.Dependencies do
      begin
      AddUnit('enettypes');
      AddUnit('enetprotocol');
      AddUnit('enetlist');
      AddUnit('enetcallbacks');
      AddUnit('enetprotocol');
      end;
    T:=P.Targets.AddUnit('uenetclass.pp');
    with T.Dependencies do
      begin
      AddUnit('enet');
      AddUnit('enettime');
      AddUnit('enetprotocol');
      end;
    P.Targets.AddExampleProgram('serverapp.pp');
    P.Targets.AddExampleProgram('clientapp.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
