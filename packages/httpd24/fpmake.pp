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

    P:=AddPackage('httpd24');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.Author := 'Library: Apache Foundation, header: Felipe Monteiro de Carvalho';
    P.License := 'Library: Apache License 2, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the Apache 2.4 series www server';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx];

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/apr');
    P.SourcePath.Add('src/aprutil');
    P.IncludePath.Add('src');
    P.IncludePath.Add('src/apr');
    P.IncludePath.Add('src/aprutil');

    P.Dependencies.Add('rtl-extra');

    T:=P.Targets.AddUnit('apr24.pas');
      with T.Dependencies do
        begin
          AddInclude('apr_errno.inc');
          AddInclude('apr_pools.inc');
          AddInclude('apr_allocator.inc');
          AddInclude('apr_user.inc');
          AddInclude('apr_time.inc');
          AddInclude('apr_tables.inc');
          AddInclude('apr_file_info.inc');
          AddInclude('apr_strings.inc');
          AddInclude('apr_version.inc');
        end;
    T:=P.Targets.AddUnit('httpd24.pas');
      with T.Dependencies do
        begin
          AddInclude('util_cfgtree.inc');
          AddInclude('httpd.inc');
          AddInclude('ap_config.inc');
          AddInclude('ap_mmn.inc');
          AddInclude('ap_release.inc');
          AddInclude('ap_regex.inc');
          AddInclude('http_config.inc');
          AddInclude('http_core.inc');
          AddInclude('util_filter.inc');
          AddInclude('http_log.inc');
          AddInclude('http_protocol.inc');
          AddInclude('aprutil/apr_buckets.inc');
          AddInclude('aprutil/apr_uri.inc');
          AddUnit('apr24');
        end;
    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('mod_hello.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

