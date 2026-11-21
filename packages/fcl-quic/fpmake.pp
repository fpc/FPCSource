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
    P:=AddPackage('fcl-quic');
    P.ShortName:='fclq';

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Author := 'Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'QUIC transport protocol (RFC 9000, 9001, 9002) with an OpenSSL 3.5 TLS backend.';
    P.NeedLibC:= false;
    P.OSes:=AllUnixOSes+AllWindowsOSes-[android];

    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-hash');
    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('openssl');

    P.Version:='3.3.1';
    T:=P.Targets.AddUnit('src/fpquic.consts.pp');
    T:=P.Targets.AddUnit('src/fpquic.buffers.pp');
    T.Dependencies.AddUnit('fpquic.consts');
    T:=P.Targets.AddUnit('src/fpquic.frames.pp');
    T.Dependencies.AddUnit('fpquic.buffers');
    T:=P.Targets.AddUnit('src/fpquic.packets.pp');
    T.Dependencies.AddUnit('fpquic.buffers');
    T:=P.Targets.AddUnit('src/fpquic.transportparams.pp');
    T.Dependencies.AddUnit('fpquic.buffers');
    T:=P.Targets.AddUnit('src/fpquic.crypto.pp');
    T.Dependencies.AddUnit('fpquic.buffers');
    T:=P.Targets.AddUnit('src/fpquic.tls.pp');
    T.Dependencies.AddUnit('fpquic.consts');
    T:=P.Targets.AddUnit('src/fpquic.openssl.pp');
    T.Dependencies.AddUnit('fpquic.crypto');
    T.Dependencies.AddUnit('fpquic.tls');
    T:=P.Targets.AddUnit('src/fpquic.streams.pp');
    T.Dependencies.AddUnit('fpquic.transportparams');
    T:=P.Targets.AddUnit('src/fpquic.recovery.pp');
    T.Dependencies.AddUnit('fpquic.frames');
    T:=P.Targets.AddUnit('src/fpquic.connection.pp');
    T.Dependencies.AddUnit('fpquic.packets');
    T.Dependencies.AddUnit('fpquic.crypto');
    T.Dependencies.AddUnit('fpquic.tls');
    T.Dependencies.AddUnit('fpquic.streams');
    T.Dependencies.AddUnit('fpquic.recovery');
    T:=P.Targets.AddUnit('src/fpquic.udp.pp');
    T:=P.Targets.AddUnit('src/fpquic.endpoint.pp');
    T.Dependencies.AddUnit('fpquic.openssl');
    T.Dependencies.AddUnit('fpquic.connection');
    T.Dependencies.AddUnit('fpquic.udp');

    T:=P.Targets.AddExampleProgram('examples/quicechoserver.pp');
    T:=P.Targets.AddExampleProgram('examples/quicechoclient.pp');
    T:=P.Targets.AddExampleProgram('examples/quicfileserver.pp');
    T:=P.Targets.AddExampleProgram('examples/quicfileclient.pp');
    T:=P.Targets.AddExampleProgram('examples/quicsansio.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
