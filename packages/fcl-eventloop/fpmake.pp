{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-eventloop');
    P.ShortName:='fclev';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('pthreads',AllUnixOSes);

    P.Author := 'Free Pascal development team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'High-performance scalable asynchronous I/O event loop for Linux, BSD/Darwin and Windows.';
    P.OSes:=AllUnixOSes + AllWindowsOSes;

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/unix',AllUnixOSes);
    P.SourcePath.Add('src/bsd',AllBSDOSes + [darwin,iphonesim,ios]);
    P.SourcePath.Add('src/win',AllWindowsOSes);

    T:=P.Targets.AddUnit('eventloop.types.pp');
    T:=P.Targets.AddUnit('eventloop.core.pp');
      with T.Dependencies do
        AddUnit('eventloop.types');
    T:=P.Targets.AddUnit('eventloop.backend.epoll.pp', [linux, android]);
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.backend.uring.pp', [linux, android]);
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.backend.kqueue.pp', AllBSDOSes + [darwin,iphonesim,ios]);
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.backend.iocp.pp', AllWindowsOSes);
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.sockets.pp');
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.pipes.pp');
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('eventloop.files.pp');
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
        end;
    T:=P.Targets.AddUnit('fcleventloop.pp');
      with T.Dependencies do
        begin
          AddUnit('eventloop.types');
          AddUnit('eventloop.core');
          AddUnit('eventloop.sockets');
          AddUnit('eventloop.pipes');
          AddUnit('eventloop.files');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

