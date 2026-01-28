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

    P:=AddPackage('fpalsa');
    P.ShortName:='fpalsa';
    P.Author := 'Nikolay Nikolov';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'https://sourceforge.net/projects/fpalsa/';
    P.Email := '';
    P.Description := 'Headers for the Advanced Linux Sound Architecture (ALSA) library';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    // Keep this lis the same as fcl-net.
    P.OSes:=[linux];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('asoundlib.pp');
    with T.Dependencies do
    begin
      AddInclude('asoundef.inc');
      AddInclude('conf.inc');
      AddInclude('control_i.inc');
      AddInclude('control.inc');
      AddInclude('error.inc');
      AddInclude('global.inc');
      AddInclude('hwdep.inc');
      AddInclude('input.inc');
      AddInclude('mixer.inc');
      AddInclude('output.inc');
      AddInclude('pcm_i.inc');
      AddInclude('pcm.inc');
      AddInclude('rawmidi.inc');
      AddInclude('seq_event.inc');
      AddInclude('seq_i.inc');
      AddInclude('seq.inc');
      AddInclude('seq_midi_event.inc');
      AddInclude('seqmid_i.inc');
      AddInclude('seqmid.inc');
      AddInclude('timer.inc');
      AddInclude('version.inc');
    end;

    P.NamespaceMap:='namespaces.lst';
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
