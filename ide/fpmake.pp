{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses
  fpmkunit,
  sysutils;
{$endif ALLPACKAGES}

const
  NoGDBOption: boolean = false;

procedure ide_check_gdb_availability(Sender: TObject);

  function DetectLibGDBDir: string;

  var
    SearchPath: string;

  const
    LibGDBName = 'libgdb.a';

  begin
    result:='';
    // First look for the GDBLIBDIR environment variable
    SearchPath := GetEnvironmentVariable('GDBLIBDIR');
    if (SearchPath<>'') and DirectoryExists(SearchPath) and FileExists(IncludeTrailingPathDelimiter(SearchPath)+LibGDBName) then
      begin
        result:=IncludeTrailingPathDelimiter(SearchPath);
        exit;
      end;
    // Search in the default locations
    SearchPath := '..'+PathDelim+'libgdb'+PathDelim+OSToString(Defaults.OS)+PathDelim+CPUToString(Defaults.CPU)+PathDelim;
    if DirectoryExists(SearchPath) and FileExists(SearchPath+LibGDBName) then
      begin
        result := SearchPath;
        exit;
      end;
    SearchPath := '..'+PathDelim+'libgdb'+PathDelim+OSToString(Defaults.OS)+PathDelim;
    if DirectoryExists(SearchPath) and FileExists(SearchPath+LibGDBName) then
      begin
        result := SearchPath;
        exit;
      end;
    // No custom libgdb.a found, try using system default library if available
    SearchPath := '..'+PathDelim+'lib'+PathDelim;
    if DirectoryExists(SearchPath) and FileExists(SearchPath+LibGDBName) then
      begin
        result := SearchPath;
        installer.BuildEngine.Log(vlWarning, 'Using system default libgdb file located in '+Result);
        exit;
      end;
    SearchPath := '..'+PathDelim+'usr'+PathDelim+'lib'+PathDelim;
    if DirectoryExists(SearchPath) and FileExists(SearchPath+LibGDBName) then
      begin
        result := SearchPath;
        installer.BuildEngine.Log(vlWarning, 'Using system default libgdb file located in '+Result);
        exit;
      end;
    SearchPath := '..'+PathDelim+'usr'+PathDelim+'local'+PathDelim+'lib'+PathDelim;
    if DirectoryExists(SearchPath) and FileExists(SearchPath+LibGDBName) then
      begin
        result := SearchPath;
        installer.BuildEngine.Log(vlWarning, 'Using system default libgdb file located in '+Result);
        exit;
      end;
  end;

var
  s: string;
  GDBLibDir: string;
  P: TPackage;

begin
  P := sender as TPackage;
  with installer do
    begin
    if not (NoGDBOption) then
      begin
        // Detection of GDB.
        GDBLibDir := DetectLibGDBDir;
        if GDBLibDir<>'' then
          begin
            // Include GDB
            BuildEngine.log(vlCommand, 'LibGDB was found, build IDE with debugger support');
            if FileExists(GDBLibDir+'gdblib.inc') then
              begin
                P.Options.Add('-dUSE_GDBLIBINC');
                P.Options.Add('-I'+GDBLibDir);
              end;
            P.Options.Add('-Fl'+GDBLibDir);

            case Defaults.OS of
              win32,
              win64 :   begin
                          P.Options.Add('-Xe');
                          P.Options.Add('-k--allow-multiple-definition');
                        end;
              freebsd : begin
                          P.Options.Add('-Fl/usr/local/lib');
                          P.Options.Add('-Xd');
                        end;
              openbsd : begin
                          P.Options.Add('-Fl/usr/local/lib');
                          P.Options.Add('-Fl/usr/lib');
                          P.Options.Add('-Xd');
                        end;
              netbsd  : P.Options.Add('-Xd');
              linux   : P.Options.Add('-Xd');
              aix     : begin
                          P.Options.Add('-Xd');
                          P.Options.Add('-Fl/opt/freeware/lib');
                          P.Options.Add('-k-bbigtoc');
                        end;
            end; {case}

            P.NeedLibc := true;
          end
        else
          begin
          BuildEngine.log(vlCommand, 'LibGDB was not found, IDE has no debugger support');
          P.Options.Add('-dNODEBUG');
          end;
      end
    else
      begin
      BuildEngine.log(vlCommand, 'Debugger support disabled');
      P.Options.Add('-dNODEBUG');
      end;
    end;
end;

procedure add_ide(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
  CompilerTarget : TCpu;
  s: string;

begin
  AddCustomFpmakeCommandlineOption('CompilerTarget','Target CPU for the IDE''s compiler');
  AddCustomFpmakeCommandlineOption('NoGDB','If value=1 or ''Y'', no GDB support');
  With Installer do
    begin
    s := GetCustomFpmakeCommandlineOptionValue('NoGDB');
    if (s='1') or (s='Y') then
     NoGDBOption := true;
    s :=GetCustomFpmakeCommandlineOptionValue('CompilerTarget');
    if s <> '' then
      CompilerTarget:=StringToCPU(s)
    else
      CompilerTarget:=Defaults.CPU;

    P:=AddPackage('ide');
    P.Version:='2.7.1';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Dependencies.Add('rtl');
    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('fv');
    P.Dependencies.Add('chm');
    { This one is only needed if DEBUG is set }
    P.Dependencies.Add('regexpr');
    if not (NoGDBOption) then
      P.Dependencies.Add('gdbint',AllOSes-AllAmigaLikeOSes);
    P.Dependencies.Add('graph',[go32v2]);

    P.SupportBuildModes:=[bmOneByOne];

    P.Options.Add('-Ur');
    P.Options.Add('-dNOCATCH');
    P.Options.Add('-dBrowserCol');
    P.Options.Add('-dGDB');

    P.Options.Add('-d'+CPUToString(CompilerTarget));
    P.Options.Add('-Fu../compiler');
    P.Options.Add('-Fu../compiler/'+CPUToString(CompilerTarget));
    P.Options.Add('-Fu../compiler/targets');
    P.Options.Add('-Fu../compiler/systems');
    P.Options.Add('-Fi../compiler/'+CPUToString(CompilerTarget));
    P.Options.Add('-Fi../compiler');

    if CompilerTarget in [x86_64, i386] then
      P.Options.Add('-Fu../compiler/x86');
    if CompilerTarget in [powerpc, powerpc64] then
      P.Options.Add('-Fu../compiler/ppcgen');
    if CompilerTarget = x86_64 then
      P.Options.Add('-dNOOPT');
    if CompilerTarget = mipsel then
      P.Options.Add('-Fu../compiler/mips');

    P.Options.Add('-Sg');

    T:=P.Targets.AddProgram('fp.pas');
    T.Dependencies.AddUnit('compunit');

    T:=P.Targets.AddUnit('compunit.pas');
    T.Directory:='compiler';
    T.Install:=false;

    P.InstallFiles.Add('fp.ans','$(bininstalldir)');
    P.InstallFiles.Add('gplprog.pt','$(bininstalldir)');
    P.InstallFiles.Add('gplunit.pt','$(bininstalldir)');
    P.InstallFiles.Add('program.pt','$(bininstalldir)');
    P.InstallFiles.Add('unit.pt','$(bininstalldir)');
    P.InstallFiles.Add('cvsco.tdf','$(bininstalldir)');
    P.InstallFiles.Add('cvsdiff.tdf','$(bininstalldir)');
    P.InstallFiles.Add('cvsup.tdf','$(bininstalldir)');
    P.InstallFiles.Add('grep.tdf','$(bininstalldir)');
    P.InstallFiles.Add('tpgrep.tdf','$(bininstalldir)');
    P.InstallFiles.Add('fp32.ico', [win32, win64], '$(bininstalldir)');

    P.Sources.AddDoc('readme.ide');

    P.CleanFiles.Add('$(UNITSOUTPUTDIR)ppheap.ppu');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)compiler.ppu');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)comphook.ppu');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)cpuinfo.ppu');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)browcol.ppu');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)ppheap.o');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)compiler.o');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)comphook.o');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)cpuinfo.o');
    P.CleanFiles.Add('$(UNITSOUTPUTDIR)browcol.o');

    P.BeforeCompileProc:=@ide_check_gdb_availability;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_ide('');
  Installer.Run;
end.
{$endif ALLPACKAGES}

