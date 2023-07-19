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

    P:=AddPackage('gst');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SupportBuildModes := [bmOneByOne];
    P.OSes:=AllUnixOSes+[Win32,Win64]-[darwin,iphonesim,ios,Android];
    if Defaults.CPU<>arm then
      P.OSes := P.OSes + [darwin];

    P.Author := 'Library: Wim Taymans and others, header: Michael Van Canneyt';
    P.License := 'Library: LGPL2.1, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Header to the GStreamer libgstreamer-1.0 library.';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.Dependencies.Add('gtk2');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=P.Targets.AddUnit('gst.pp');
    With T.Dependencies do
      begin
      AddInclude('elementfactory.inc');
      AddInclude('gstaliases.inc');
      AddInclude('gstallocator.inc');
      AddInclude('gstatomicqueue.inc');
      AddInclude('gstbin.inc');
      AddInclude('gstbuffer.inc');
      AddInclude('gstbufferlist.inc');
      AddInclude('gstbufferpool.inc');
      AddInclude('gstbus.inc');
      AddInclude('gstcapsfeatures.inc');
      AddInclude('gstcaps.inc');
      AddInclude('gstchildproxy.inc');
      AddInclude('gstclock.inc');
      AddInclude('gstcontext.inc');
      AddInclude('gstcontrolbinding.inc');
      AddInclude('gstcontrolsource.inc');
      AddInclude('gstdatetime.inc');
      AddInclude('gstdebugutils.inc');
      AddInclude('gstdevice.inc');
      AddInclude('gstdevicemonitor.inc');
      AddInclude('gstdeviceproviderfactory.inc');
      AddInclude('gstdeviceprovider.inc');
      AddInclude('gstdynamictypefactory.inc');
      AddInclude('gstelementfactory.inc');
      AddInclude('gstelement.inc');
      AddInclude('gstelementmetadata.inc');
      AddInclude('gstenum.inc');
      AddInclude('gstenumtypes.inc');
      AddInclude('gsterror.inc');
      AddInclude('gstevent.inc');
      AddInclude('gstformat.inc');
      AddInclude('gstghostpad.inc');
      AddInclude('gstinfo.inc');
      AddInclude('gstiterator.inc');
      AddInclude('gstmemory.inc');
      AddInclude('gstmessage.inc');
      AddInclude('gstmeta.inc');
      AddInclude('gstminiobject.inc');
      AddInclude('gstobject.inc');
      AddInclude('gstpad.inc');
      AddInclude('gstpadtemplate.inc');
      AddInclude('gstparamspecs.inc');
      AddInclude('gstparse.inc');
      AddInclude('gstpipeline.inc');
      AddInclude('gstpluginfeature.inc');
      AddInclude('gstplugin.inc');
      AddInclude('gstpoll.inc');
      AddInclude('gstpreset.inc');
      AddInclude('gstpromise.inc');
      AddInclude('gstprotection.inc');
      AddInclude('gstquery.inc');
      AddInclude('gstrec.inc');
      AddInclude('gstregistry.inc');
      AddInclude('gstsample.inc');
      AddInclude('gstsegment.inc');
      AddInclude('gststreamcollection.inc');
      AddInclude('gststreams.inc');
      AddInclude('gststructure.inc');
      AddInclude('gstsystemclock.inc');
      AddInclude('gsttaglist.inc');
      AddInclude('gsttagsetter.inc');
      AddInclude('gsttask.inc');
      AddInclude('gsttaskpool.inc');
      AddInclude('gsttoc.inc');
      AddInclude('gsttocsetter.inc');
      AddInclude('gsttracerfactory.inc');
      AddInclude('gsttracer.inc');
      AddInclude('gsttracerrecord.inc');
      AddInclude('gsttypefindfactory.inc');
      AddInclude('gsttypefind.inc');
      AddInclude('gsturi.inc');
      AddInclude('gstutils.inc');
      AddInclude('gstvalue.inc');
      AddInclude('gstversion.inc');
      // Implementations
      AddInclude('gstmessage_impl.inc');
       AddInclude('gstbin_impl.inc');
      end;
    P.Sources.AddExampleFiles('examples/camrecord.lpr',P.Directory,false,'.');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
