{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-net');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-net';
{$endif ALLPACKAGES}
    P.Version:='2.2.4';

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-passrc');
    P.Dependencies.Add('fcl-async');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/os2',[EMX]);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes-[EMX]);

    // IP and Sockets
    T:=P.Targets.AddUnit('netdb.pp',AllUnixOSes);
    T:=P.Targets.AddUnit('resolve.pp',AllUnixOSes+AllWindowsOSes+[OS2,EMX]);
      with T.Dependencies do
        begin
          AddInclude('resolve.inc');
          AddUnit('netdb');
        end;
    T:=P.Targets.AddUnit('ssockets.pp',AllUnixOSes+AllWindowsOSes+[OS2,EMX]);
      with T.Dependencies do
        begin
          AddUnit('resolve');
        end;

    // HTTP Client
    T:=P.Targets.AddUnit('servlets.pp');
    T:=P.Targets.AddUnit('fpsock.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('resolve');
        end;
    T:=P.Targets.AddUnit('httpbase.pp',AllUnixOSes);
    T:=P.Targets.AddUnit('httpclient.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('httpbase');
          AddUnit('fpsock');
        end;
    T:=P.Targets.AddUnit('httpsvlt.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('fpsock');
          AddUnit('httpbase');
          AddUnit('servlets');
        end;

    // XML-RPC
    T:=P.Targets.AddUnit('xmlrpc.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('ssockets');
          AddUnit('httpclient');
          AddUnit('httpsvlt');
        end;
    T:=P.Targets.AddProgram('mkxmlrpc.pp',AllUnixOSes);

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
