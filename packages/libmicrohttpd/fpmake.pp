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

    P:=AddPackage('libmicrohttpd');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.Author := 'Library: GNU foundation, header: Silvio Clecio';
    P.License := 'Library: LGPL or later, header: LGPL with modification';
    P.HomepageURL := 'https://www.gnu.org/software/libmicrohttpd/';
    P.Email := '';
    P.Description := 'Event and threaded based micro-http server library interface';
    P.NeedLibC:= true;
    P.Dependencies.Add('rtl-extra');
    P.OSes := AllUnixOSes + [win32,win64];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=P.Targets.AddUnit('libmicrohttpd.pp');
    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('basicauthentication.pp');
    T:=P.Targets.AddExampleProgram('benchmark_https.pp');
    T:=P.Targets.AddExampleProgram('benchmark.pp');
    T:=P.Targets.AddExampleProgram('chunked_example.pp');
    T:=P.Targets.AddExampleProgram('demo_https.pp');
    T:=P.Targets.AddExampleProgram('demo.pp');
    T:=P.Targets.AddExampleProgram('digest_auth_example.pp');
    T:=P.Targets.AddExampleProgram('dual_stack_example.pp');
    T:=P.Targets.AddExampleProgram('fileserver_example_dirs.pp');
    T:=P.Targets.AddExampleProgram('fileserver_example_external_select.pp');
    T:=P.Targets.AddExampleProgram('fileserver_example.pp');
    T:=P.Targets.AddExampleProgram('hellobrowser.pp');
    T:=P.Targets.AddExampleProgram('https_fileserver_example.pp');
    T:=P.Targets.AddExampleProgram('largepost.pp');
    T:=P.Targets.AddExampleProgram('logging.pp');
    T:=P.Targets.AddExampleProgram('minimal_example_comet.pp');
    T:=P.Targets.AddExampleProgram('minimal_example.pp');
    T:=P.Targets.AddExampleProgram('post_example.pp');
    T:=P.Targets.AddExampleProgram('querystring_example.pp');
    T:=P.Targets.AddExampleProgram('refuse_post_example.pp');
    T:=P.Targets.AddExampleProgram('responseheaders.pp');
    T:=P.Targets.AddExampleProgram('sessions.pp');
    T:=P.Targets.AddExampleProgram('simplepost.pp');
    T:=P.Targets.AddExampleProgram('tlsauthentication.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
