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

    P:=AddPackage('fv');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'Leon De Boer and Pierre Mueller';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,ios,solaris,netbsd,openbsd,linux,win16,win32,win64,os2,emx,netware,netwlibc,go32v2,msdos,aix,dragonfly]+AllAmigaLikeOSes;
    P.Email := '';
    P.Description := 'Free Vision, a portable Turbo Vision clone.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.Dependencies.add('rtl-console');
    P.Dependencies.add('rtl-extra');
    P.Dependencies.add('rtl-unicode');
    P.Dependencies.add('morphunits',[morphos]);
    P.Dependencies.add('arosunits',[aros]);
    if Defaults.CPU=m68k then
      P.Dependencies.Add('amunits',[amiga]);
    if Defaults.CPU=powerpc then
      P.Dependencies.Add('os4units',[amiga]);

    T:=P.Targets.AddUnit('app.pas');
      with T.Dependencies do
        begin
          AddInclude('app.inc');
          AddInclude('platform.inc');
          AddUnit('fvcommon');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('menus');
          AddUnit('histlist');
          AddUnit('dialogs');
          AddUnit('msgbox');
          AddUnit('fvconsts');
          AddUnit('fvclip',AllUnixOSes);
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('uapp.pas');
      with T.Dependencies do
        begin
          AddInclude('app.inc');
          AddInclude('platform.inc');
          AddUnit('ufvcommon');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('umenus');
          AddUnit('uhistlist');
          AddUnit('udialogs');
          AddUnit('umsgbox');
          AddUnit('fvconsts');
          AddUnit('ufvclip',AllUnixOSes);
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('asciitab.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('fvconsts');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('app');
        end;
    T:=P.Targets.AddUnit('buildfv.pas');
    T.Install := false; // Build-unit
      with T.Dependencies do
        begin
          AddUnit('fvcommon');
          AddUnit('drivers');
          AddUnit('fvconsts');
          AddUnit('views');
          AddUnit('validate');
          AddUnit('msgbox');
          AddUnit('dialogs');
          AddUnit('menus');
          AddUnit('app');
          AddUnit('stddlg');
          AddUnit('asciitab');
          AddUnit('tabs');
          AddUnit('outline');
          AddUnit('memory');
          AddUnit('colortxt');
          AddUnit('statuses');
          AddUnit('histlist');
          AddUnit('inplong');
          AddUnit('editors');
          AddUnit('gadgets');
          AddUnit('timeddlg');
          AddUnit('time');
        end;
    T:=P.Targets.AddUnit('colorsel.pas');
      with T.Dependencies do
        begin
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('colortxt.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('app');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('dialogs.pas');
      with T.Dependencies do
        begin
          AddInclude('dialogs.inc');
          AddInclude('platform.inc');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('validate');
          AddUnit('app');
          AddUnit('histlist');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('udialogs.pas');
      with T.Dependencies do
        begin
          AddInclude('dialogs.inc');
          AddInclude('platform.inc');
          AddUnit('ufvcommon');
          AddUnit('fvconsts');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('uvalidate');
          AddUnit('uapp');
          AddUnit('uhistlist');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('drivers.pas');
      with T.Dependencies do
        begin
          AddInclude('drivers.inc');
          AddInclude('platform.inc');
          AddUnit('sysmsg');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('udrivers.pas');
      with T.Dependencies do
        begin
          AddInclude('drivers.inc');
          AddInclude('platform.inc');
          AddUnit('sysmsg');
          AddUnit('ufvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('editors.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
          AddUnit('app');
          AddUnit('stddlg');
          AddUnit('msgbox');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('fvclip.pas',AllUnixOSes);
      with T.Dependencies do
        begin
          AddInclude('fvclip.inc');
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('fvconsts');
          AddUnit('app');
          AddUnit('fvcommon');
        end;
    T:=P.Targets.AddUnit('ufvclip.pas',AllUnixOSes);
      with T.Dependencies do
        begin
          AddInclude('fvclip.inc');
          AddInclude('platform.inc');
          AddUnit('udrivers');
          AddUnit('fvconsts');
          AddUnit('uapp');
          AddUnit('ufvcommon');
        end;
    T:=P.Targets.AddUnit('fvcommon.pas');
      with T.Dependencies do
        begin
          AddInclude('fvcommon.inc');
          AddInclude('platform.inc');
        end;
    T:=P.Targets.AddUnit('ufvcommon.pas');
      with T.Dependencies do
        begin
          AddInclude('fvcommon.inc');
          AddInclude('platform.inc');
        end;
    T:=P.Targets.AddUnit('fvconsts.pas');
    T:=P.Targets.AddUnit('gadgets.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('fvconsts');
          AddUnit('time');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('app');
        end;
    T:=P.Targets.AddUnit('histlist.pas');
      with T.Dependencies do
        begin
          AddInclude('histlist.inc');
          AddInclude('platform.inc');
          AddUnit('fvcommon');
        end;
    T:=P.Targets.AddUnit('uhistlist.pas');
      with T.Dependencies do
        begin
          AddInclude('histlist.inc');
          AddInclude('platform.inc');
          AddUnit('ufvcommon');
        end;
    T:=P.Targets.AddUnit('inplong.pas');
      with T.Dependencies do
        begin
          AddInclude('inplong.inc');
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('msgbox');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('uinplong.pas');
      with T.Dependencies do
        begin
          AddInclude('inplong.inc');
          AddInclude('platform.inc');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('udialogs');
          AddUnit('umsgbox');
          AddUnit('ufvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('memory.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('fvcommon');
        end;
    T:=P.Targets.AddUnit('menus.pas');
      with T.Dependencies do
        begin
          AddInclude('menus.inc');
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('umenus.pas');
      with T.Dependencies do
        begin
          AddInclude('menus.inc');
          AddInclude('platform.inc');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('ufvcommon');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('msgbox.pas');
      with T.Dependencies do
        begin
          AddInclude('msgbox.inc');
          AddInclude('platform.inc');
          AddUnit('dialogs');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('app');
          AddUnit('fvcommon');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('umsgbox.pas');
      with T.Dependencies do
        begin
          AddInclude('msgbox.inc');
          AddInclude('platform.inc');
          AddUnit('udialogs');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('uapp');
          AddUnit('ufvcommon');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('outline.pas');
      with T.Dependencies do
        begin
          AddInclude('outline.inc');
          AddUnit('drivers');
          AddUnit('views');
        end;
    T:=P.Targets.AddUnit('uoutline.pas');
      with T.Dependencies do
        begin
          AddInclude('outline.inc');
          AddUnit('udrivers');
          AddUnit('uviews');
        end;
    T:=P.Targets.AddUnit('statuses.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('msgbox');
          AddUnit('app');
        end;
    T:=P.Targets.AddUnit('stddlg.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
          AddUnit('fvconsts');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('dialogs');
          AddUnit('validate');
          AddUnit('app');
          AddUnit('histlist');
          AddUnit('msgbox');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('sysmsg.pas');
      with T.Dependencies do
        begin
          AddInclude('unixsmsg.inc',AllUnixOSes);
          AddInclude('w32smsg.inc',[win32,win64]);
          AddInclude('go32smsg.inc',[go32v2]);
        end;
    T:=P.Targets.AddUnit('tabs.pas');
      with T.Dependencies do
        begin
          AddInclude('tabs.inc');
          AddInclude('platform.inc');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('fvconsts');
          AddUnit('fvcommon');
          AddUnit('dialogs');
        end;
    T:=P.Targets.AddUnit('utabs.pas');
      with T.Dependencies do
        begin
          AddInclude('tabs.inc');
          AddInclude('platform.inc');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('fvconsts');
          AddUnit('ufvcommon');
          AddUnit('udialogs');
        end;
    T:=P.Targets.AddUnit('timeddlg.pas');
      with T.Dependencies do
        begin
          AddInclude('timeddlg.inc');
          AddInclude('platform.inc');
          AddUnit('dialogs');
          AddUnit('fvconsts');
          AddUnit('drivers');
          AddUnit('views');
          AddUnit('app');
          AddUnit('msgbox');
        end;
    T:=P.Targets.AddUnit('utimeddlg.pas');
      with T.Dependencies do
        begin
          AddInclude('timeddlg.inc');
          AddInclude('platform.inc');
          AddUnit('udialogs');
          AddUnit('fvconsts');
          AddUnit('udrivers');
          AddUnit('uviews');
          AddUnit('uapp');
          AddUnit('umsgbox');
        end;
    T:=P.Targets.AddUnit('time.pas');
      with T.Dependencies do
        begin
          AddInclude('platform.inc');
        end;
    T:=P.Targets.AddUnit('validate.pas');
      with T.Dependencies do
        begin
          AddInclude('validate.inc');
          AddInclude('platform.inc');
          AddUnit('fvcommon');
          AddUnit('fvconsts');
          AddUnit('msgbox');
        end;
    T:=P.Targets.AddUnit('uvalidate.pas');
      with T.Dependencies do
        begin
          AddInclude('validate.inc');
          AddInclude('platform.inc');
          AddUnit('ufvcommon');
          AddUnit('fvconsts');
          AddUnit('umsgbox');
        end;
    T:=P.Targets.AddUnit('views.pas');
      with T.Dependencies do
        begin
          AddInclude('views.inc');
          AddInclude('platform.inc');
          AddUnit('fvcommon');
          AddUnit('drivers');
          AddUnit('fvconsts');
        end;
    T:=P.Targets.AddUnit('uviews.pas');
      with T.Dependencies do
        begin
          AddInclude('views.inc');
          AddInclude('platform.inc');
          AddUnit('ufvcommon');
          AddUnit('udrivers');
          AddUnit('fvconsts');
        end;
    P.ExamplePath.Add('examples');
    P.ExamplePath.Add('src');
    P.Targets.AddExampleProgram('examples/testapp.pas');
    P.Targets.AddExampleProgram('src/platform.inc');
    // 'examples/Makefile
    // 'examples/testapp.lpi
    // 'examples/Makefile.fpc



    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
