{$ifndef ALLPACKAGES}
program fpmake;

{$mode objfpc}{$h+}

uses fpmkunit;
{$endif}

Procedure Add_Google(ADirectory : string);

  function StdDep(T : TTarget) : TTarget;
  begin
    T.Dependencies.AddUnit('googlebase');
    T.Dependencies.AddUnit('googleservice');
    Result:=T;
  end;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('googleapi');
    P.ShortName:='googleap';
    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Google API client libraries.';
    P.NeedLibC:= false;
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,amiga,aros,dragonfly];
    P.Directory:=ADirectory;
    P.Version:='3.1.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-web');
    P.SourcePath.Add('src');
    T:=P.Targets.AddUnit('googlebase.pp');
    T:=P.Targets.AddUnit('googleclient.pp');
    T:=P.Targets.AddUnit('googleservice.pp');
    T.Dependencies.AddUnit('googleclient');
    T.Dependencies.AddUnit('googlebase');
    T:=StdDep(P.Targets.AddUnit('googlediscoverytopas.pp'));
    T:=StdDep(P.Targets.AddUnit('src/googleadexchangebuyer.pp'));
    T:=StdDep(P.Targets.AddUnit('googleadexchangeseller.pp'));
    T:=StdDep(P.Targets.AddUnit('googleadmin.pp'));
    T:=StdDep(P.Targets.AddUnit('googleadsense.pp'));
    T:=StdDep(P.Targets.AddUnit('googleadsensehost.pp'));
    T:=StdDep(P.Targets.AddUnit('googleanalytics.pp'));
    T:=StdDep(P.Targets.AddUnit('googleandroidenterprise.pp'));
    T:=StdDep(P.Targets.AddUnit('googleandroidpublisher.pp'));
    T:=StdDep(P.Targets.AddUnit('googleappsactivity.pp'));
    T:=StdDep(P.Targets.AddUnit('googleappstate.pp'));
    T:=StdDep(P.Targets.AddUnit('googleaudit.pp'));
    T:=StdDep(P.Targets.AddUnit('googleautoscaler.pp'));
    T:=StdDep(P.Targets.AddUnit('googlebigquery.pp'));
    T:=StdDep(P.Targets.AddUnit('googleblogger.pp'));
    T:=StdDep(P.Targets.AddUnit('googlebooks.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecalendar.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecivicinfo.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecloudmonitoring.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecompute.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecomputeaccounts.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecontainer.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecontent.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecoordinate.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecustomsearch.pp'));
    T:=StdDep(P.Targets.AddUnit('googledataflow.pp'));
    T:=StdDep(P.Targets.AddUnit('googledatastore.pp'));
    T:=StdDep(P.Targets.AddUnit('googledeploymentmanager.pp'));
    T:=StdDep(P.Targets.AddUnit('googledfareporting.pp'));
    T:=StdDep(P.Targets.AddUnit('googlediscovery.pp'));
    T:=StdDep(P.Targets.AddUnit('googledns.pp'));
    T:=StdDep(P.Targets.AddUnit('googledoubleclickbidmanager.pp'));
    T:=StdDep(P.Targets.AddUnit('googledoubleclicksearch.pp'));
    T:=StdDep(P.Targets.AddUnit('googledrive.pp'));
    T:=StdDep(P.Targets.AddUnit('googlefitness.pp'));
    T:=StdDep(P.Targets.AddUnit('googlefreebase.pp'));
    T:=StdDep(P.Targets.AddUnit('googlefusiontables.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegames.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegamesConfiguration.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegamesManagement.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegan.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegenomics.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegmail.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegroupsmigration.pp'));
    T:=StdDep(P.Targets.AddUnit('googlegroupssettings.pp'));
    T:=StdDep(P.Targets.AddUnit('googleidentitytoolkit.pp'));
    T:=StdDep(P.Targets.AddUnit('googlelicensing.pp'));
    T:=StdDep(P.Targets.AddUnit('googlemanager.pp'));
    T:=StdDep(P.Targets.AddUnit('googlemapsengine.pp'));
    T:=StdDep(P.Targets.AddUnit('googlemirror.pp'));
    T:=StdDep(P.Targets.AddUnit('googleoauth2.pp'));
    T:=StdDep(P.Targets.AddUnit('googlepagespeedonline.pp'));
    T:=StdDep(P.Targets.AddUnit('googleplus.pp'));
    T:=StdDep(P.Targets.AddUnit('googleplusDomains.pp'));
    T:=StdDep(P.Targets.AddUnit('googleprediction.pp'));
    T:=StdDep(P.Targets.AddUnit('googlepubsub.pp'));
    T:=StdDep(P.Targets.AddUnit('googleqpxExpress.pp'));
    T:=StdDep(P.Targets.AddUnit('googlereplicapool.pp'));
    T:=StdDep(P.Targets.AddUnit('googlereplicapoolupdater.pp'));
    T:=StdDep(P.Targets.AddUnit('googlereseller.pp'));
    T:=StdDep(P.Targets.AddUnit('googleresourceviews.pp'));
    T:=StdDep(P.Targets.AddUnit('googlesiteVerification.pp'));
    T:=StdDep(P.Targets.AddUnit('googlespectrum.pp'));
    T:=StdDep(P.Targets.AddUnit('googlesqladmin.pp'));
    T:=StdDep(P.Targets.AddUnit('googlestorage.pp'));
    T:=StdDep(P.Targets.AddUnit('googletagmanager.pp'));
    T:=StdDep(P.Targets.AddUnit('googletaskqueue.pp'));
    T:=StdDep(P.Targets.AddUnit('googletasks.pp'));
    T:=StdDep(P.Targets.AddUnit('googletranslate.pp'));
    T:=StdDep(P.Targets.AddUnit('googleurlshortener.pp'));
    T:=StdDep(P.Targets.AddUnit('googlewebfonts.pp'));
    T:=StdDep(P.Targets.AddUnit('googlewebmasters.pp'));
    T:=StdDep(P.Targets.AddUnit('googleyoutube.pp'));
    T:=StdDep(P.Targets.AddUnit('googleyoutubeAnalytics.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecloudlatencytest.pp'));
    T:=StdDep(P.Targets.AddUnit('googlecloudsearch.pp'));
    T:=StdDep(P.Targets.AddUnit('googlelogging.pp'));
    end;
end;

{$ifndef ALLPACKAGES}
begin
  Add_Google('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
