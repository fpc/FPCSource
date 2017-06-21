program fppkg;

{$mode objfpc}{$H+}

{$if defined(VER2_2) and (FPC_PATCH<1)}
  {$fatal At least FPC 2.2.1 is required to compile fppkg}
{$endif}

uses
  // General
{$ifdef unix}
  baseunix, cthreads,
{$endif}
  Classes, SysUtils, TypInfo, custapp,
  // Repository handler objects
  fprepos, fpxmlrep,
  pkgmessages, pkgglobals, pkgoptions, pkgrepos,
  // Package Handler components
  pkghandler,pkgmkconv, pkgdownload,
  pkgfpmake, pkgcommands,
  fpmkunit
  // Downloaders
{$if (defined(unix) and not defined(android)) or defined(windows)}
  ,pkgwget
  ,pkglnet
  ,pkgfphttp
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
    Procedure LoadGlobalDefaults;
    Procedure ProcessCommandLine(FirstPass: boolean);
    Procedure DoRun; Override;
  end;

  EMakeToolError = Class(Exception);


{ TMakeTool }

procedure TMakeTool.LoadGlobalDefaults;
var
  i : integer;
  cfgfile : String;
begin
  // Default verbosity
  LogLevels:=DefaultLogLevels;
  for i:=1 to ParamCount do
    if (ParamStr(i)='-d') or (ParamStr(i)='--debug') then
      begin
        LogLevels:=AllLogLevels+[llDebug];
        break;
      end;
  // First try config file from command line
  if HasOption('C','config-file') then
    cfgfile:=GetOptionValue('C','config-file')
  else
    cfgfile:='';
  pkgoptions.LoadGlobalDefaults(cfgfile);
end;


procedure TMakeTool.MaybeCreateLocalDirs;
begin
  ForceDirectories(GlobalOptions.BuildDir);
  ForceDirectories(GlobalOptions.ArchivesDir);
  ForceDirectories(GlobalOptions.CompilerConfigDir);
end;


procedure TMakeTool.ShowUsage;
begin
  Writeln('Usage: ',Paramstr(0),' [options] <action> <package>');
  Writeln('Options:');
  Writeln('  -C --config-file   Specify the configuration file to use');
  Writeln('  -c --config        Set compiler configuration to use');
  Writeln('  -h --help          This help');
  Writeln('  -v --verbose       Show more information');
  Writeln('  -d --debug         Show debugging information');
  Writeln('  -g --global        Force installation to global (system-wide) directory');
  Writeln('  -f --force         Force installation also if the package is already installed');
  Writeln('  -r --recovery      Recovery mode, use always internal fpmkunit');
  Writeln('  -b --broken        Do not stop on broken packages');
  Writeln('  -l --showlocation  Show if the packages are installed globally or locally');
  Writeln('  -o --options=value Pass extra options to the compiler');
  Writeln('  -n                 Do not read the default configuration files');
  Writeln('  -p --prefix=value  Specify the prefix');
  Writeln('  -s --skipbroken    Skip the rebuild of depending packages after installation');
  Writeln('  --compiler=value   Specify the compiler-executable');
  Writeln('  --cpu=value        Specify the target cpu to compile for');
  Writeln('  --os=value         Specify the target operating system to compile for');
  Writeln('Actions:');
  Writeln('  update            Update packages list');
  Writeln('  list              List available and installed packages');
  Writeln('  build             Build package');
  Writeln('  compile           Compile package');
  Writeln('  install           Install package');
  Writeln('  uninstall         Uninstall package');
  Writeln('  clean             Clean package');
  Writeln('  archive           Create archive of package');
  Writeln('  download          Download package');
  Writeln('  convertmk         Convert Makefile.fpc to fpmake.pp');
  Writeln('  info              Show more information about a package');
  Writeln('  fixbroken         Recompile all (broken) packages with changed dependencies');
  Writeln('  listsettings      Show the values for all fppkg settings');
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


procedure TMakeTool.ProcessCommandLine(FirstPass: boolean);

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

  function SplitSpaces(var SplitString: string) : string;
  var i : integer;
  begin
    i := pos(' ',SplitString);
    if i > 0 then
      begin
        result := copy(SplitString,1,i-1);
        delete(SplitString,1,i);
      end
    else
      begin
        result := SplitString;
        SplitString:='';
      end;
  end;

Var
  I : Integer;
  HasAction : Boolean;
  OptString : String;
begin
  I:=0;
  HasAction:=false;
  // We can't use the TCustomApplication option handling,
  // because they cannot handle [general opts] [command] [cmd-opts] [args]
  While (I<ParamCount) do
    begin
      Inc(I);
      // Check options.
      if CheckOption(I,'C','config-file') then
        begin
          // Do nothing, the config-file has already been read.
          OptionArg(I);
        end
      else if CheckOption(I,'c','config') then
        GlobalOptions.CompilerConfig:=OptionArg(I)
      else if CheckOption(I,'v','verbose') then
        LogLevels:=AllLogLevels
      else if CheckOption(I,'d','debug') then
        LogLevels:=AllLogLevels+[llDebug]
      else if CheckOption(I,'g','global') then
        GlobalOptions.InstallGlobal:=true
      else if CheckOption(I,'r','recovery') then
        GlobalOptions.RecoveryMode:=true
      else if CheckOption(I,'n','') then
        GlobalOptions.SkipConfigurationFiles:=true
      else if CheckOption(I,'b','broken') then
        GlobalOptions.AllowBroken:=true
      else if CheckOption(I,'l','showlocation') then
        GlobalOptions.ShowLocation:=true
      else if CheckOption(I,'s','skipbroken') then
        GlobalOptions.SkipFixBrokenAfterInstall:=true
      else if CheckOption(I,'o','options') and FirstPass then
        begin
          OptString := OptionArg(I);
          while OptString <> '' do
            CompilerOptions.Options.Add(SplitSpaces(OptString));
        end
      else if CheckOption(I,'p','prefix') then
        begin
          CompilerOptions.GlobalPrefix := OptionArg(I);
          CompilerOptions.LocalPrefix := OptionArg(I);
          FPMakeCompilerOptions.GlobalPrefix := OptionArg(I);
          FPMakeCompilerOptions.LocalPrefix := OptionArg(I);
        end
      else if CheckOption(I,'','compiler') then
        begin
          CompilerOptions.Compiler := OptionArg(I);
          FPMakeCompilerOptions.Compiler := OptionArg(I);
        end
      else if CheckOption(I,'','os') then
        CompilerOptions.CompilerOS := StringToOS(OptionArg(I))
      else if CheckOption(I,'','cpu') then
        CompilerOptions.CompilerCPU := StringToCPU(OptionArg(I))
      else if CheckOption(I,'h','help') then
        begin
          ShowUsage;
          halt(0);
        end
      else if (Length(Paramstr(i))>0) and (Paramstr(I)[1]='-') then
        begin
          if FirstPass then
            Raise EMakeToolError.CreateFmt(SErrInvalidArgument,[I,ParamStr(i)])
        end
      else
      // It's a command or target.
        begin
          if HasAction then
            begin
              if FirstPass then
                ParaPackages.Add(Paramstr(i))
            end
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
    ProcessCommandLine(true);

    // Scan is special, it doesn't need a valid local setup
    if (ParaAction='scan') then
      begin
        RebuildRemoteRepository;
        ListRemoteRepository;
        SaveRemoteRepository;
        halt(0);
      end;

    MaybeCreateLocalDirs;
    if not GlobalOptions.SkipConfigurationFiles then
      LoadCompilerDefaults
    else
      begin
        FPMakeCompilerOptions.InitCompilerDefaults;
        CompilerOptions.InitCompilerDefaults;
      end;

    // The command-line is parsed for the second time, to make it possible
    // to override the values in the compiler-configuration file. (like prefix)
    ProcessCommandLine(false);

    // If CompilerVersion, CompilerOS or CompilerCPU is still empty, use the
    // compiler-executable to get them
    FPMakeCompilerOptions.CheckCompilerValues;
    CompilerOptions.CheckCompilerValues;

    LoadLocalAvailableMirrors;

    // Load local repository, update first if this is a new installation
    // errors will only be reported as warning. The user can be bootstrapping
    // and do an update later
    if not FileExists(GlobalOptions.LocalPackagesFile) then
      begin
        try
          pkghandler.ExecuteAction('','update');
        except
          on E: Exception do
            pkgglobals.Log(llWarning,E.Message);
        end;
      end;
    LoadLocalAvailableRepository;
    FindInstalledPackages(FPMakeCompilerOptions,true);
    CheckFPMakeDependencies;
    // We only need to reload the status when we use a different
    // configuration for compiling fpmake or when the CPU, OS or compiler
    // are set in the command-line
    if (GlobalOptions.CompilerConfig<>GlobalOptions.FPMakeCompilerConfig) or
       (CompilerOptions.CompilerCPU<>FPMakeCompilerOptions.CompilerCPU) or
       (CompilerOptions.CompilerOS<>FPMakeCompilerOptions.CompilerOS) or
       (CompilerOptions.Compiler<>FPMakeCompilerOptions.Compiler) then
      FindInstalledPackages(CompilerOptions,true);

    // Check for broken dependencies
    if not GlobalOptions.AllowBroken and
       (((ParaAction='fixbroken') and (ParaPackages.Count>0)) or
        (ParaAction='compile') or
        (ParaAction='build') or
        (ParaAction='install') or
        (ParaAction='archive')) then
      begin
        pkgglobals.Log(llDebug,SLogCheckBrokenDependenvies);
        SL:=TStringList.Create;
        if FindBrokenPackages(SL) then
          Error(SErrBrokenPackagesFound);
        FreeAndNil(SL);
      end;

    if ParaPackages.Count=0 then
      begin
        ActionPackage:=AvailableRepository.AddPackage(CurrentDirPackageName);
        pkghandler.ExecuteAction(CurrentDirPackageName,ParaAction);
      end
    else
      begin
        // Process packages
        for i:=0 to ParaPackages.Count-1 do
          begin
            if sametext(ExtractFileExt(ParaPackages[i]),'.zip') and FileExists(ParaPackages[i]) then
              begin
                ActionPackage:=AvailableRepository.AddPackage(CmdLinePackageName);
                ActionPackage.LocalFileName:=ExpandFileName(ParaPackages[i]);
                pkghandler.ExecuteAction(CmdLinePackageName,ParaAction);
              end
            else
              begin
                pkgglobals.Log(llDebug,SLogCommandLineAction,['['+ParaPackages[i]+']',ParaAction]);
                pkghandler.ExecuteAction(ParaPackages[i],ParaAction);
              end;
          end;
      end;

    // Recompile all packages dependent on this package
    if (ParaAction='install') and not GlobalOptions.SkipFixBrokenAfterInstall then
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

