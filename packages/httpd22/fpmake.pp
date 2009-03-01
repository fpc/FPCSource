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

    P:=AddPackage('httpd22');
{$ifdef ALLPACKAGES}
    P.Directory:='httpd22';
{$endif ALLPACKAGES}
    P.Version:='2.2.4';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('apriconv.pas');
      with T.Dependencies do
        begin
          AddInclude('apr_iconv.inc');
          AddInclude('api_version.inc');
          AddUnit('apr');
        end;
    T:=P.Targets.AddUnit('apr.pas');
      with T.Dependencies do
        begin
          AddInclude('apr_errno.inc');
          AddInclude('apr_pools.inc');
          AddInclude('apr_allocator.inc');
          AddInclude('apr_general.inc');
          AddInclude('apr_dso.inc');
          AddInclude('apr_user.inc');
          AddInclude('apr_time.inc');
          AddInclude('apr_tables.inc');
          AddInclude('apr_file_info.inc');
          AddInclude('apr_file_io.inc');
          AddInclude('apr_strings.inc');
          AddInclude('apr_lib.inc');
          AddInclude('apr_signal.inc');
          AddInclude('apr_network_io.inc');
          AddInclude('apr_thread_proc.inc');
          AddInclude('apr_version.inc');
          AddInclude('apr_poll.inc');
        end;
    T:=P.Targets.AddUnit('aprutil.pas');
      with T.Dependencies do
        begin
          AddInclude('apr_xml.inc');
          AddInclude('apr_uri.inc');
          AddInclude('apr_md5.inc');
          AddInclude('apr_xlate.inc');
          AddUnit('apr');
        end;
    T:=P.Targets.AddUnit('httpd.pas');
      with T.Dependencies do
        begin
          AddInclude('ap_provider.inc');
          AddInclude('util_cfgtree.inc');
          AddInclude('httpd.inc');
          AddInclude('ap_config.inc');
          AddInclude('ap_mmn.inc');
          AddInclude('ap_release.inc');
          AddInclude('apr_buckets.inc');
          AddInclude('ap_regex.inc');
          AddInclude('http_config.inc');
          AddInclude('http_core.inc');
          AddInclude('apr_hash.inc');
          AddInclude('util_filter.inc');
          AddInclude('http_log.inc');
          AddInclude('http_main.inc');
          AddInclude('http_protocol.inc');
          AddInclude('http_request.inc');
          AddInclude('http_connection.inc');
          AddInclude('http_vhost.inc');
          AddInclude('util_script.inc');
          AddInclude('util_time.inc');
          AddInclude('util_md5.inc');
          AddInclude('ap_mpm.inc');
          AddUnit('apr');
          AddUnit('aprutil');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
