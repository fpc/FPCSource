program fppkg;

{$mode objfpc}{$H+}

{$if defined(VER2_2) and (FPC_PATCH<1)}
  {$fatal At least FPC 2.2.1 is required to compile fppkg}
{$endif}

uses
  // General
{$ifdef unix}
  baseunix,
{$endif}
  Classes, SysUtils, TypInfo, custapp,
  // Repository handler objects
  fprepos, fpxmlrep,
  pkgmessages, pkgglobals, pkgoptions, pkgrepos,
  // Package Handler components
  pkghandler,pkgmkconv, pkgdownload,
  pkgfpmake, pkgcommands
  // Downloaders
{$if defined(unix) or defined(windows)}
  ,pkgwget
  ,pkglnet
{$endif}
  ;

Type
  { TMakeTool }

  TMakeTool = Class(TCustomApplication)
  Private
    ParaAction   : string;
    ParaPackages : TStringList;
    procedure MaybeCreateLocalDirs;
    procedure ShowUsage;
  Public
    Constructor Create;
    Destructor Destroy;override;
    Function GetConfigFileName : String;
    Procedure LoadGlobalDefaults;
    Procedure LoadCompilerDefaults;
    Procedure ProcessCommandLine;
    Procedure DoRun; Override;
  end;

  EMakeToolError = Class(Exception);


{ TMakeTool }

function TMakeTool.GetConfigFileName: String;
begin
  if HasOption('C','config-file') then
    Result:=GetOptionValue('C','config-file')
  else
    Result:=GetAppConfigFile(IsSuperUser,False);
end;


procedure TMakeTool.LoadGlobalDefaults;
var
  i : integer;
  cfgfile : String;
  GeneratedConfig : boolean;
begin
  // Default verbosity
  LogLevels:=DefaultLogLevels;
  for i:=1 to ParamCount do
    if (ParamStr(i)='-d') or (ParamStr(i)='--debug') then
      begin
        LogLevels:=AllLogLevels+[vlDebug];
        break;
      end;
  // Load file or create new default configuration
  cfgfile:=GetConfigFileName;
  GeneratedConfig:=false;
  if FileExists(cfgfile) then
    begin
      GlobalOptions.LoadGlobalFromFile(cfgfile);
      if GlobalOptions.Dirty then
        GlobalOptions.SaveGlobalToFile(cfgfile);
    end
  else
    begin
      ForceDirectories(ExtractFilePath(cfgfile));
      GlobalOptions.SaveGlobalToFile(cfgfile);
      GeneratedConfig:=true;
    end;
  GlobalOptions.CompilerConfig:=GlobalOptions.DefaultCompilerConfig;
  // Tracing of what we've done above, need to be done after the verbosity is set
  if GeneratedConfig then
    Log(vlDebug,SLogGeneratingGlobalConfig,[cfgfile])
  else
    Log(vlDebug,SLogLoadingGlobalConfig,[cfgfile])
end;


procedure TMakeTool.MaybeCreateLocalDirs;
begin
  ForceDirectories(GlobalOptions.BuildDir);
  ForceDirectories(GlobalOptions.ArchivesDir);
  ForceDirectories(GlobalOptions.CompilerConfigDir);
end;


procedure TMakeTool.LoadCompilerDefaults;
var
  S : String;
begin
  // Load default compiler config
  S:=GlobalOptions.CompilerConfigDir+GlobalOptions.CompilerConfig;
  if FileExists(S) then
    begin
      Log(vlDebug,SLogLoadingCompilerConfig,[S]);
      CompilerOptions.LoadCompilerFromFile(S)
    end
  else
    begin
      // Generate a default configuration if it doesn't exists
      if GlobalOptions.CompilerConfig='default' then
        begin
          Log(vlDebug,SLogGeneratingCompilerConfig,[S]);
          CompilerOptions.InitCompilerDefaults;
          CompilerOptions.SaveCompilerToFile(S);
          if CompilerOptions.Dirty then
            CompilerOptions.SaveCompilerToFile(S);
        end
      else
        Error(SErrMissingCompilerConfig,[S]);
    end;
  // Load FPMake compiler config, this is normally the same config as above
  S:=GlobalOptions.CompilerConfigDir+GlobalOptions.FPMakeCompilerConfig;
  if FileExists(S) then
    begin
      Log(vlDebug,SLogLoadingFPMakeCompilerConfig,[S]);
      FPMakeCompilerOptions.LoadCompilerFromFile(S);
      if FPMakeCompilerOptions.Dirty then
        FPMakeCompilerOptions.SaveCompilerToFile(S);
    end
  else
    Error(SErrMissingCompilerConfig,[S]);
end;


procedure TMakeTool.ShowUsage;
begin
  Writeln('Usage: ',Paramstr(0),' [options] <action> <package>');
  Writeln('Options:');
  Writeln('  -c --config        Set compiler configuration to use');
  Writeln('  -h --help          This help');
  Writeln('  -v --verbose       Show more information');
  Writeln('  -d --debug         Show debugging information');
  Writeln('  -g --global        Force installation to global (system-wide) directory');
  Writeln('  -f --force         Force installation also if the package is already installed');
  Writeln('  -r --recovery      Recovery mode, use always internal fpmkunit');
  Writeln('Actions:');
  Writeln('  update             Update packages list');
  Writeln('  showavail          List available packages');
  Writeln('  showall            Show all (including local) packages');
  Writeln('  build              Build package');
  Writeln('  compile            Compile package');
  Writeln('  install            Install package');
  Writeln('  clean              Clean package');
  Writeln('  archive            Create archive of package');
  Writeln('  download           Download package');
  Writeln('  convertmk          Convert Makefile.fpc to fpmake.pp');
//  Writeln('  addconfig          Add a compiler configuration for the supplied compiler');
  Halt(0);
end;

Constructor TMakeTool.Create;
begin
  inherited Create(nil);
  ParaPackages:=TStringList.Create;
end;


Destructor TMakeTool.Destroy;
begin
  FreeAndNil(ParaPackages);
  inherited Destroy;
end;


procedure TMakeTool.ProcessCommandLine;

  Function CheckOption(Index : Integer;Short,Long : String): Boolean;
  var
    O : String;
  begin
    O:=Paramstr(Index);
    Result:=(O='-'+short) or (O='--'+long) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
  end;

  Function OptionArg(Var Index : Integer) : String;
  Var
    P : Integer;
  begin
    if (Length(ParamStr(Index))>1) and (Paramstr(Index)[2]<>'-') then
      begin
        If Index<ParamCount then
          begin
            Inc(Index);
            Result:=Paramstr(Index);
          end
        else
          Error(SErrNeedArgument,[Index,ParamStr(Index)]);
      end
    else If length(ParamStr(Index))>2 then
      begin
        P:=Pos('=',Paramstr(Index));
        If (P=0) then
          Error(SErrNeedArgument,[Index,ParamStr(Index)])
        else
          begin
            Result:=Paramstr(Index);
            Delete(Result,1,P);
          end;
      end;
  end;

Var
  I : Integer;
  HasAction : Boolean;
begin
  I:=0;
  HasAction:=false;
  // We can't use the TCustomApplication option handling,
  // because they cannot handle [general opts] [command] [cmd-opts] [args]
  While (I<ParamCount) do
    begin
      Inc(I);
      // Check options.
      if CheckOption(I,'c','config') then
        GlobalOptions.CompilerConfig:=OptionArg(I)
      else if CheckOption(I,'v','verbose') then
        LogLevels:=AllLogLevels
      else if CheckOption(I,'d','debug') then
        LogLevels:=AllLogLevels+[vlDebug]
      else if CheckOption(I,'g','global') then
        GlobalOptions.InstallGlobal:=true
      else if CheckOption(I,'r','recovery') then
        GlobalOptions.RecoveryMode:=true
      else if CheckOption(I,'b','broken') then
        GlobalOptions.AllowBroken:=true
      else if CheckOption(I,'h','help') then
        begin
          ShowUsage;
          halt(0);
        end
      else if (Length(Paramstr(i))>0) and (Paramstr(I)[1]='-') then
        Raise EMakeToolError.CreateFmt(SErrInvalidArgument,[I,ParamStr(i)])
      else
      // It's a command or target.
        begin
          if HasAction then
            ParaPackages.Add(Paramstr(i))
          else
            begin
              ParaAction:=Paramstr(i);
              HasAction:=true;
            end;
        end;
    end;
  if not HasAction then
    ShowUsage;
end;


procedure TMakeTool.DoRun;
var
  ActionPackage : TFPPackage;
  OldCurrDir : String;
  i      : Integer;
  SL     : TStringList;
begin
  OldCurrDir:=GetCurrentDir;
  Try
    LoadGlobalDefaults;
    ProcessCommandLine;
    MaybeCreateLocalDirs;
    LoadCompilerDefaults;

    // Load local repository, update first if this is a new installation
    // errors will only be reported as warning. The user can be bootstrapping
    // and do an update later
    if not FileExists(GlobalOptions.LocalPackagesFile) then
      begin
        try
          pkghandler.ExecuteAction('','update');
        except
          on E: Exception do
            Log(vlWarning,E.Message);
        end;
      end;
    LoadLocalAvailableMirrors;
    LoadLocalAvailableRepository;
    FindInstalledPackages(FPMakeCompilerOptions,true);
    CheckFPMakeDependencies;
    // We only need to reload the status when we use a different
    // configuration for compiling fpmake
    if GlobalOptions.CompilerConfig<>GlobalOptions.FPMakeCompilerConfig then
      FindInstalledPackages(CompilerOptions,true);

    // Check for broken dependencies
    if not GlobalOptions.AllowBroken and
       not((ParaPackages.Count=0) and (ParaAction='fixbroken')) then
      begin
        SL:=TStringList.Create;
        if FindBrokenPackages(SL) then
          Error(SErrBrokenPackagesFound);
        FreeAndNil(SL);
      end;

    if ParaPackages.Count=0 then
      begin
        ActionPackage:=InstalledRepository.AddPackage(CurrentDirPackageName);
        pkghandler.ExecuteAction(CurrentDirPackageName,ParaAction);
      end
    else
      begin
        // Process packages
        for i:=0 to ParaPackages.Count-1 do
          begin
            if FileExists(ParaPackages[i]) then
              begin
                ActionPackage:=InstalledRepository.AddPackage(CmdLinePackageName);
                ActionPackage.LocalFileName:=ExpandFileName(ParaPackages[i]);
                pkghandler.ExecuteAction(CmdLinePackageName,ParaAction);
              end
            else
              begin
                Log(vlDebug,SLogCommandLineAction,['['+ParaPackages[i]+']',ParaAction]);
                pkghandler.ExecuteAction(ParaPackages[i],ParaAction);
              end;
          end;
      end;

    // Recompile all packages dependent on this package
    if (ParaAction='install') then
      pkghandler.ExecuteAction('','fixbroken');

    Terminate;

  except
    On E : Exception do
      begin
        Writeln(StdErr,SErrException);
        Writeln(StdErr,E.Message);
        Halt(1);
      end;
  end;
  SetCurrentDir(OldCurrDir);
end;


begin
  With TMakeTool.Create do
    try
      run;
    finally
      Free;
    end;
end.

