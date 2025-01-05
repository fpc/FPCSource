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

    P:=AddPackage('libjack');
    P.ShortName:='ljack';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'Library: libjack';
    P.License := 'Audio Server: GPL, Library: LGPL';
    P.HomepageURL := 'https://jackaudio.org/';
    P.Email := '';
    P.Description := 'Headers for the JACK Audio Connection Kit library';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes-[qnx];
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('jack.pp');
    with T.Dependencies do
      begin
        AddInclude('systemdeps.inc');
        AddInclude('transport.inc');
        AddInclude('types.inc');
        AddInclude('weakmacros.inc');
      end;

    T:=P.Targets.AddUnit('jackringbuffer.pp');
    with T.Dependencies do
      begin
        AddInclude('ringbuffer.inc');
        AddUnit('jack');
      end;

    T:=P.Targets.AddUnit('jackuuid.pp');
    with T.Dependencies do
      begin
        AddInclude('uuid.inc');
        AddUnit('jack');
      end;

    T:=P.Targets.AddUnit('jacksession.pp');
    with T.Dependencies do
      begin
        AddInclude('session.inc');
        AddInclude('weakmacros.inc');
        AddUnit('jack');
      end;

    T:=P.Targets.AddUnit('jackthread.pp');
    with T.Dependencies do
      begin
        AddInclude('thread.inc');
        AddInclude('weakmacros.inc');
        AddUnit('jack');
      end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('simple_client.pp');
    P.Targets.AddExampleProgram('simple_session_client.pp');
    P.Targets.AddExampleProgram('latent_client.pp');
    P.Targets.AddExampleProgram('transport_client.pp');
    P.Targets.AddExampleProgram('impulse_grabber.pp');
    P.Targets.AddExampleProgram('showtime.pp');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
