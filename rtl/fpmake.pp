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
    P.SourcePath.Add('$(CPU)');
    P.SourcePath.Add('unix',AllUnixOSes);
    P.SourcePath.Add('win',AllWindowsOSes);
    P.SourcePath.Add('inc');
    P.SourcePath.Add('objpas');

    // Where to find the include files using firstmatch
    P.IncludePath.Add('$(OS)/$(CPU)',[Linux]);
    P.IncludePath.Add('$(OS)');
    P.IncludePath.Add('$(CPU)');
    P.IncludePath.Add('unix',AllUnixOSes);
    P.IncludePath.Add('win',AllWindowsOSes);
    P.IncludePath.Add('inc');
    P.IncludePath.Add('objpas');

    // System unit
    T:=P.Targets.AddUnit('system.pp');
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
          AddInclude('strpas.inc');
          AddInclude('math.inc');
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

    // Startup
    T:=P.Targets.AddUnit('si_c21g.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_c21g.inc');
        end;
    T:=P.Targets.AddUnit('si_c21.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_c21.inc');
        end;
    T:=P.Targets.AddUnit('si_c.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_c.inc');
        end;
    T:=P.Targets.AddUnit('si_dll.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_dll.inc');
        end;
    T:=P.Targets.AddUnit('si_prc.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_prc.inc');
        end;
    T:=P.Targets.AddUnit('si_uc.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('si_intf.inc');
          AddInclude('sysnr.inc');
          AddInclude('si_uc.inc');
        end;

    // Compile mode units
    T:=P.Targets.AddUnit('objpas.pp');
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('macpas.pp');
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('fpcylix.pp',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('cthreads');
          AddUnit('cwstring');
          AddUnit('dynlibs');
        end;

    // Unix units
    T:=P.Targets.AddUnit('unixtype.pp',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('system');
        end;
    T:=P.Targets.AddUnit('unixutil.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddInclude('textrec.inc');
          AddInclude('filerec.inc');
          AddUnit('system');
        end;
    T:=P.Targets.AddUnit('baseunix.pp',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('unixtype');
          AddInclude('osdefs.inc');
          AddInclude('aliasptp.inc');
          AddInclude('aliasctp.inc');
          AddInclude('errno.inc');
          AddInclude('ostypes.inc');
          AddInclude('stat.inc');
          AddInclude('signal.inc');
          AddInclude('sighndh.inc');
          AddInclude('bunxh.inc');
          AddInclude('bunxovlh.inc');
          AddInclude('genfunch.inc');
          AddInclude('genfuncs.inc');
          AddInclude('gensigset.inc');
          AddInclude('genfdset.inc');
          AddInclude('syscallh.inc');
          AddInclude('sysnr.inc');
          AddInclude('bsyscall.inc');
          AddInclude('bunxsysc.inc');
          AddInclude('settimeo.inc');
          AddInclude('osmacro.inc');
          AddInclude('bunxovl.inc');
          AddInclude('textrec.inc');
          AddInclude('filerec.inc');
        end;
    T:=P.Targets.AddUnit('unix.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('baseunix');
          AddUnit('unixutil');
          AddInclude('aliasptp.inc');
          AddInclude('aliasctp.inc');
          AddInclude('unxconst.inc');
          AddInclude('unxsysch.inc');
          AddInclude('unxovlh.inc');
          AddInclude('unxovl.inc');
          AddInclude('syscallh.inc');
          AddInclude('unxsysc.inc');
          AddInclude('textrec.inc');
          AddInclude('filerec.inc');
          AddInclude('unxfunc.inc');
          AddInclude('timezone.inc');
        end;
    T:=P.Targets.AddUnit('termio.pp',AllUnixOSes);
      with T.Dependencies do
        begin
          AddUnit('baseunix');
          AddInclude('termios.inc');
          AddInclude('termiosh.inc');
          AddInclude('textrec.inc');
          AddInclude('termiosproc.inc');
        end;
    T:=P.Targets.AddUnit('unix/errors.pp',AllUnixOSes);
      T.Dependencies.AddUnit('unixtype');
      T.Dependencies.AddInclude('errnostr.inc');
    T:=P.Targets.AddUnit('unix/syscall.pp',AllUnixOSes);
      T.Dependencies.AddInclude('sysnr.inc');
      T.Dependencies.AddInclude('syscallh.inc');
    T:=P.Targets.AddUnit('unix/terminfo.pp',AllUnixOSes);
      T.Dependencies.AddUnit('baseunix',AllUnixOSes);
    T:=P.Targets.AddUnit('unix/dl.pp',AllUnixOSes);
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('unix/ipc.pp',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('baseunix');
          AddUnit('syscall');
          AddInclude('ipccall.inc');
        end;

    // Linux units
    T:=P.Targets.AddUnit('linux/linux.pp',[Linux]);
      with T.Dependencies do
        begin
          AddUnit('baseunix');
          AddUnit('syscall');
        end;
    T:=P.Targets.AddUnit('linux/gpm.pp',[Linux]);
      With T.Dependencies do
        begin
          AddUnit('termio');
          AddUnit('sockets');
          AddUnit('strings');
          AddUnit('unix');
        end;
    T:=P.Targets.AddUnit('linux/linuxvcs.pp');
      with T.Dependencies do
        begin
          AddUnit('baseunix');
          AddUnit('strings');
        end;

    // Turbo Pascal RTL units
    T:=P.Targets.AddUnit('strings.pp');
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
          AddInclude('dosh.inc');
          AddInclude('filerec.inc');
          AddInclude('textrec.inc');
          AddInclude('dos.inc');
          AddInclude('fexpand.inc');
        end;
    T:=P.Targets.AddUnit('crt.pp');
      With T.Dependencies do
        begin
          AddUnit('unix',AllUnixOSes);
          AddUnit('termio',AllUnixOSes);
          AddInclude('crth.inc');
          AddInclude('textrec.inc');
        end;
    T:=P.Targets.AddUnit('objects.pp');
      T.Dependencies.AddUnit('dos');
    T:=P.Targets.AddUnit('ports.pp',[i386,x86_64],AllOSes);
      with T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('x86');
        end;
    T:=P.Targets.AddUnit('printer.pp');
      with T.Dependencies do
        begin
          AddInclude('printerh.inc');
          AddInclude('printer.inc');
          AddInclude('textrec.inc');
          AddUnit('unix',AllUnixOSes);
          AddUnit('strings');
        end;

    // Object Pascal RTL units
    T:=P.Targets.AddUnit('rtlconsts.pp');
      T.Dependencies.AddUnit('objpas');
      T.Dependencies.AddInclude('rtlconst.inc');
    T:=P.Targets.AddUnit('sysconst.pp');
      T.Dependencies.AddUnit('objpas');
    T:=P.Targets.AddUnit('sysutils.pp');
      T.IncludePath.Add('objpas/sysutils');
      With T.Dependencies do
        begin
          AddUnit('sysconst');
          AddUnit('unix',AllUnixOSes);
          AddUnit('errors',AllUnixOSes);
          AddInclude('sysutilh.inc');
          AddInclude('sysinth.inc');
          AddInclude('osutilsh.inc');
          AddInclude('datih.inc');
          AddInclude('sysstrh.inc');
          AddInclude('filerec.inc');
          AddInclude('textrec.inc');
          AddInclude('syspchh.inc');
          AddInclude('sysansih.inc');
          AddInclude('syswideh.inc');
          AddInclude('finah.inc');
          AddInclude('filutilh.inc');
          AddInclude('diskh.inc');
          AddInclude('systhrdh.inc');
          AddInclude('intfh.inc');
          AddInclude('sysutils.inc');
          AddInclude('fina.inc');
          AddInclude('fexpand.inc');
          AddInclude('varerror.inc');
          AddInclude('sysstr.inc');
          AddInclude('sysformt.inc');
          AddInclude('dati.inc');
          AddInclude('syspch.inc');
          AddInclude('strings.inc');
          AddInclude('genstr.inc');
          AddInclude('stringsi.inc');
          AddInclude('sysint.inc');
          AddInclude('sysansi.inc');
          AddInclude('syswide.inc');
          AddInclude('sysformt.inc');
          AddInclude('sysuthrd.inc');
          AddInclude('osutil.inc');
          AddInclude('sysuintf.inc');
          AddInclude('suuid.inc');
        end;
    T:=P.Targets.AddUnit('types.pp');
      T.Dependencies.AddUnit('objpas');
    T:=P.Targets.AddUnit('typinfo.pp');
      T.Dependencies.AddUnit('sysutils');
    T:=P.Targets.AddUnit('classes.pp');
      T.IncludePath.Add('objpas/classes');
      With T.Dependencies do
        begin
          AddUnit('sysutils');
          AddUnit('types');
          AddUnit('typinfo');
          AddUnit('unix',AllUnixOSes);
          AddInclude('classesh.inc');
          AddInclude('classes.inc');
          AddInclude('util.inc');
          AddInclude('bits.inc');
          AddInclude('streams.inc');
          AddInclude('parser.inc');
          AddInclude('collect.inc');
          AddInclude('lists.inc');
          AddInclude('stringl.inc');
          AddInclude('tthread.inc');
          AddInclude('persist.inc');
          AddInclude('compon.inc');
          AddInclude('action.inc');
          AddInclude('dm.inc');
          AddInclude('cregist.inc');
          AddInclude('intf.inc');
          AddInclude('filer.inc');
          AddInclude('reader.inc');
          AddInclude('writer.inc');
          AddInclude('twriter.inc');
        end;
    T:=P.Targets.AddUnit('convutils.pp');
      T.Dependencies.AddUnit('objpas');
      T.Dependencies.AddInclude('convutil.inc');
    T:=P.Targets.AddUnit('dateutils.pp');
      With T.Dependencies do
        begin
          AddUnit('sysutils');
          AddUnit('math');
          AddUnit('types');
          AddUnit('sysconst');
          AddInclude('dateutil.inc');
        end;
    T:=P.Targets.AddUnit('stdconvs.pp');
      T.Dependencies.AddUnit('convutils');
    T:=P.Targets.AddUnit('strutils.pp');
      T.Dependencies.AddUnit('sysutils');
    T:=P.Targets.AddUnit('varutils.pp');
      With T.Dependencies do
        begin
          AddUnit('sysutils');
          AddInclude('varutilh.inc');
          AddInclude('varerror.inc');
          AddInclude('cvarutil.inc');
          AddInclude('varutils.inc');
        end;
    T:=P.Targets.AddUnit('variants.pp');
      With T.Dependencies do
        begin
          AddUnit('sysutils');
          AddUnit('rtlconsts');
          AddUnit('typinfo');
        end;
    T:=P.Targets.AddUnit('fgl.pp');
      T.Dependencies.AddUnit('sysutils');
      T.Dependencies.AddUnit('types');
    T:=P.Targets.AddUnit('fmtbcd.pp');
      T.Dependencies.AddUnit('sysutils');
      T.Dependencies.AddUnit('variants');
    T:=P.Targets.AddUnit('math.pp');
      With T.Dependencies do
        begin
          AddUnit('sysutils');
          AddInclude('mathuh.inc');
          AddInclude('mathu.inc');
        end;

    // CPU dependent units
    T:=P.Targets.AddUnit('cpu.pp');
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('x86.pp',[i386,x86_64],AllOSes);
      T.Dependencies.AddUnit('system');
      T.Dependencies.AddUnit('baseunix',AllUnixOSes);
    T:=P.Targets.AddUnit('mmx.pp',[i386,x86_64],AllOSes);
      T.Dependencies.AddUnit('cpu');

    // C Interfacing units
    T:=P.Targets.AddUnit('ctypes');
      With T.Dependencies do
        begin
          AddUnit('system');
          AddUnit('unixtype',AllUnixOSes);
          AddInclude('aliasctp.inc',AllUnixOSes);
        end;
    T:=P.Targets.AddUnit('initc.pp');
      T.Dependencies.AddUnit('ctypes');
    T:=P.Targets.AddUnit('cmem.pp');
      T.Dependencies.AddUnit('system');
    T:=P.Targets.AddUnit('cthreads.pp',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('initc');
          AddUnit('unix');
          AddUnit('dl');
          AddInclude('pthread.inc');
        end;
    T:=P.Targets.AddUnit('cwstring',AllUnixOSes);
      With T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('initc');
          AddUnit('unix');
          AddUnit('ctypes');
        end;

    // Misc units
    T:=P.Targets.AddUnit('charset.pp');
      with T.Dependencies do
        begin
          AddUnit('system');
          AddUnit('objpas');
        end;
    T:=P.Targets.AddUnit('ucomplex.pp');
      with T.Dependencies do
        begin
          AddUnit('system');
          AddUnit('math');
        end;
    T:=P.Targets.AddUnit('matrix.pp');
      with T.Dependencies do
        begin
          AddUnit('system');
          AddInclude('mvecimp.inc');
          AddInclude('mmatimp.inc');
        end;
    T:=P.Targets.AddUnit('getopts.pp');
      with T.Dependencies do
        begin
          AddUnit('system');
        end;
    T:=P.Targets.AddUnit('dynlibs.pas');
      With T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('dl',AllUnixOSes);
          AddInclude('dynlibs.inc');
        end;

    // Debugging units
    T:=P.Targets.AddUnit('exeinfo.pp');
      with T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('strings');
        end;
    T:=P.Targets.AddUnit('heaptrc.pp');
      with T.Dependencies do
        begin
          AddUnit('system');
        end;
    T:=P.Targets.AddUnit('lineinfo.pp');
      with T.Dependencies do
        begin
          AddUnit('exeinfo');
        end;
    T:=P.Targets.AddUnit('lnfodwrf.pp');
      with T.Dependencies do
        begin
          AddUnit('exeinfo');
        end;

    // IO units
    T:=P.Targets.AddUnit('mouse.pp');
      with T.Dependencies do
        begin
          AddUnit('video');
          AddUnit('gpm',[Linux]);
          AddInclude('mouseh.inc');
          AddInclude('mouse.inc');
        end;
    T:=P.Targets.AddUnit('video.pp');
      with T.Dependencies do
        begin
          AddUnit('baseunix',AllUnixOSes);
          AddUnit('termio',AllUnixOSes);
          AddUnit('strings');
          AddUnit('linuxvcs',[Linux]);
          AddInclude('videoh.inc');
          AddInclude('video.inc');
          AddInclude('convert.inc');
        end;
    T:=P.Targets.AddUnit('keyboard.pp');
       with T.Dependencies do
         begin
           AddInclude('keybrdh.inc');
           AddInclude('keyboard.inc');
           AddInclude('keyscan.inc');
           AddUnit('mouse');
         end;
    T:=P.Targets.AddUnit('sockets.pp');
      with T.Dependencies do
        begin
          AddInclude('socketsh.inc');
          AddInclude('filerec.inc');
          AddInclude('textrec.inc');
          AddInclude('sockovl.inc');
          AddInclude('sockets.inc');
          AddInclude('unxsockh.inc',AllUnixOSes);
          AddInclude('unixsock.inc',AllUnixOSes);
          AddUnit('baseunix',AllUnixOSes);
        end;
    T:=P.Targets.AddUnit('serial.pp');
      with T.Dependencies do
        begin
          AddUnit('objpas');
          AddUnit('termio',AllUnixOSes);
          AddUnit('unix',AllUnixOSes);
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
