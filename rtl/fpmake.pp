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

    P:=AddPackage('rtl');
{$ifdef ALLPACKAGES}
    P.Directory:='rtl';
{$endif ALLPACKAGES}
    P.Version:='2.2.0';

    // Where to find the sources using firstmatch
    P.SourcePath.Add('$(OS)');
    P.SourcePath.Add('unix',AllUnixOSes);
    P.SourcePath.Add('win',AllWindowsOSes);
    P.SourcePath.Add('$(CPU)');
    P.SourcePath.Add('inc');
    P.SourcePath.Add('objpas');

    // System unit
    T:=P.Targets.AddUnit('system.pp');
      T.IncludePath.Add('inc');
      T.IncludePath.Add('$(CPU)');
      T.IncludePath.Add('$(OS)');
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('unix',AllUnixOSes);
      T.IncludePath.Add('win',AllWindowsOSes);
      With T.Dependencies do
        begin
          // Headers
          AddInclude('setjumph.inc');
          AddInclude('systemh.inc');
          AddInclude('objpash.inc');
          AddInclude('mathh.inc');
          AddInclude('wstringh.inc');
          AddInclude('dynarrh.inc');
          AddInclude('compproc.inc');
          AddInclude('heaph.inc');
          AddInclude('threadh.inc');
          AddInclude('varianth.inc');
          AddInclude('sysosh.inc');
          AddInclude('resh.inc');
          // Implementations
          AddInclude('set.inc');
          AddInclude('int64p.inc');
          AddInclude('setjump.inc');
          AddInclude('systhrd.inc');
          AddInclude('sysos.inc');
          AddInclude('sysheap.inc');
          AddInclude('sysdir.inc');
          AddInclude('sysfile.inc');
          AddInclude('except.inc');
          AddInclude('threadvr.inc');
          AddInclude('filerec.inc');
          AddInclude('textrec.inc');
          AddInclude('generic.inc');
          AddInclude('genset.inc');
          AddInclude('genmath.inc');
          AddInclude('sstrings.inc');
          AddInclude('int64.inc');
          AddInclude('astrings.inc');
          AddInclude('wstrings.inc');
          AddInclude('wustrings.inc');
          AddInclude('aliases.inc');
          AddInclude('dynarr.inc');
          AddInclude('objpas.inc');
          AddInclude('variant.inc');
          AddInclude('rtti.inc');
          AddInclude('heap.inc');
          AddInclude('thread.inc');
          AddInclude('text.inc');
          AddInclude('file.inc');
          AddInclude('typefile.inc');
          AddInclude('innr.inc');
          AddInclude('$(CPU).inc');
          AddInclude('fastmove.inc',[i386],AllOSes);
          AddInclude('real2str.inc');
          // Unix implementations
          AddInclude('osdefs.inc',AllUnixOSes);
          AddInclude('sysunixh.inc',AllUnixOSes);
          AddInclude('elfres32.inc',AllUnixOSes);
          AddInclude('system.inc',AllUnixOSes);
          AddInclude('errno.inc',AllUnixOSes);
          AddInclude('ostypes.inc',AllUnixOSes);
          AddInclude('ptypes.inc',AllUnixOSes);
          AddInclude('ctypes.inc',AllUnixOSes);
          AddInclude('stat.inc',AllUnixOSes);
          AddInclude('signal.inc',AllUnixOSes);
          AddInclude('sighnd.inc',AllUnixOSes);
          AddInclude('sighndh.inc',AllUnixOSes);
          AddInclude('syscallh.inc',AllUnixOSes);
          AddInclude('syscall.inc',AllUnixOSes);
          AddInclude('sysnr.inc',AllUnixOSes);
          AddInclude('ossysc.inc',AllUnixOSes);
          AddInclude('osmacro.inc',AllUnixOSes);
        end;

    // Compile mode units
    T:=P.Targets.AddUnit('objpas.pp');
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('macpas.pp');
      T.Dependencies.AddUnit('system');

    // Unix units
    T:=P.Targets.AddUnit('unixtype.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('$(OS)');
      T.IncludePath.Add('unix');
      With T.Dependencies do
        begin
          AddUnit('system');
        end;
    T:=P.Targets.AddUnit('unixutil.pp',AllUnixOSes);
      T.IncludePath.Add('unix');
      T.IncludePath.Add('inc');
    T:=P.Targets.AddUnit('baseunix.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('$(OS)');
      T.IncludePath.Add('unix');
      T.IncludePath.Add('inc');
      With T.Dependencies do
        begin
          AddUnit('unixtype');
        end;
    T:=P.Targets.AddUnit('unix.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('$(OS)');
      T.IncludePath.Add('unix');
      T.IncludePath.Add('inc');
      T.Dependencies.AddUnit('baseunix');
      T.Dependencies.AddUnit('unixutil');
    T:=P.Targets.AddUnit('termio.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('$(OS)');
      T.IncludePath.Add('unix');
      T.IncludePath.Add('inc');
      T.Dependencies.AddUnit('baseunix');
    T:=P.Targets.AddUnit('unix/errors.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)');
      T.Dependencies.AddUnit('unixtype');
      T.Dependencies.AddInclude('errnostr.inc');
    T:=P.Targets.AddUnit('unix/syscall.pp',AllUnixOSes);
      T.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
      T.IncludePath.Add('$(OS)');
      T.Dependencies.AddInclude('sysnr.inc');
      T.Dependencies.AddInclude('syscallh.inc');
    T:=P.Targets.AddUnit('unix/terminfo.pp',AllUnixOSes);
      T.Dependencies.AddUnit('baseunix');

    // Linux only
    T:=P.Targets.AddUnit('linux/linux.pp',[Linux]);

{
    With Targets['sysutils'].dependencies do
      begin
      add('unix');
      add('errors');
      Add('unixtype');
      Add('baseunix');
      end;
}

    // Turbo Pascal RTL units
    T:=P.Targets.AddUnit('strings.pp');
      T.IncludePath.Add('$(CPU)');
      T.IncludePath.Add('inc');
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('strings.inc');
          AddInclude('stringss.inc');
          AddInclude('genstr.inc');
          AddInclude('genstrs.inc');
          AddInclude('stringsi.inc');
        end;
    T:=P.Targets.AddUnit('dos.pp');
      With T.Dependencies do
        begin
          AddUnit('strings');
          AddUnit('unix',AllUnixOSes);
          AddInclude('inc/dosh.inc');
        end;
    T:=P.Targets.AddUnit('crt.pp');
      With T.Dependencies do
        begin
          AddUnit('unix',AllUnixOSes);
          AddInclude('inc/crth.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
