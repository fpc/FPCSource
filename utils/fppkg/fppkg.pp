program fppkg;

{$mode objfpc}{$H+}

uses
  // General
{$ifdef unix}
  baseunix,
{$endif}
  Classes, SysUtils, TypInfo, custapp,
  // Repository handler objects
  fprepos, fpxmlrep,fpmktype, pkgropts,
  // Package Handler components
  pkghandler, pkgmkconv, pkgdownload, pkgmessages;
  
Type

  TRunMode = (rmHelp,rmCompile,rmBuild,rmInstall,rmArchive,rmClean,rmDownload,rmUpdate);

  { TMakeTool }

  TMakeTool = Class(TCustomApplication)
  Private
    FDefaults: TPackagerOptions;
    FConvertOnly,
    FLogging : Boolean;
    FCompiler : String;
    FRunMode : TRunMode;
    FHaveMakefile : Boolean;
    FHaveFpmake : Boolean;
    FFPMakeSrc : String;
    FFPMakeBin : String;
    FVerbose: TVerbosities;
    FPackages : TStrings;
    Procedure Log(Msg : String);
    Procedure Error(Msg : String);
    Procedure Error(Fmt : String; Args : Array of const);
    Function GetCompiler : String;
  Public
    Procedure DownloadFile(Const URL,Dest : String);
    Function GetConfigFileName : String;
    Procedure LoadDefaults;
    Procedure ProcessCommandLine;
    procedure CreateFPMake;
    procedure CompileFPMake(Extra : Boolean);
    Function RunFPMake : Integer;
    Procedure DoRun; Override;
    Property Verbose : TVerbosities Read FVerbose Write FVerbose;
  end;

  EMakeToolError = Class(Exception);


{ TMakeTool }

procedure TMakeTool.CompileFPMake(Extra: Boolean);

Var
  O,C : String;

begin
  C:=GetCompiler;
  O:=FFPmakeSrc;
  If Extra then
    O:='-Fafpmkext '+O;
  Log(SLogCompilingFPMake+C+' '+O);
  If ExecuteProcess(C,O)<>0 then
    Error(SErrFailedToCompileFPCMake)
end;

procedure TMakeTool.CreateFPMake;
begin
  Log(SLogGeneratingFPMake);
  With TMakeFileConverter.Create(Nil) do
    try
      ConvertFile('Makefile.fpc','fpmake.pp');
    finally
      Free;
    end;
end;


Function TMakeTool.RunFPMake : Integer;

  Function MaybeQuote(Const S : String) : String;
  
  begin
    If Pos(' ',S)=0 then
      Result:=S
    else
      Result:='"'+S+'"';
  end;
  

Var
  I : integer;
  D,O : String;

begin
  Log(SLogRunningFPMake);
  D:=IncludeTrailingPathDelimiter(GetCurrentDir);
  O:='';
  For I:=1 to ParamCount do
    begin
    If (O<>'') then
      O:=O+' ';
    O:=O+MaybeQuote(ParamStr(I));
    end;
  Result:=ExecuteProcess(D+FFPMakeBin,O);
end;

procedure TMakeTool.Log(Msg: String);
begin
  If FLogging then
    Writeln(stdErr,Msg);
end;

procedure TMakeTool.Error(Msg: String);
begin
  Raise EMakeToolError.Create(Msg);
end;

procedure TMakeTool.Error(Fmt: String; Args: array of const);
begin
  Raise EMakeToolError.CreateFmt(Fmt,Args);
end;

function TMakeTool.GetCompiler: String;
begin
  If (FCompiler='') then
    begin
    {$if defined(cpui386)}
      FCompiler:='ppc386';
    {$elseif defined(cpuAlpha)}
      FCompiler:='ppcaxp';
    {$elseif defined(cpusparc)}
      FCompiler:='ppcsparc';
    {$elseif defined(cpuarm)}
      FCompiler:='ppcarm';
    {$elseif defined(cpum68k)}
      FCompiler:='ppcm68k';
   {$elseif defined(cpux86_64)}
      FCompiler:='ppcx64';
    {$elseif defined(cpupowerpc)}
      FCompiler:='ppcppc';
    {$else}
      {$Fatal Unknown architecture}
    {$endif}
    end;
  If (ExtractFilePath(FCompiler)<>'') then
    Result:=FCompiler
  else
    begin
    Result:=FileSearch(FCompiler,GetEnvironmentVariable('PATH'));
    If (Result='') then
      Result:=FCompiler;
    end;
end;

procedure TMakeTool.DownloadFile(const URL, Dest: String);
begin

end;

function TMakeTool.GetConfigFileName: String;

var
  G : Boolean;

begin
  if HasOption('C','config-file') then
    Result:=GetOptionValue('C','config-file')
  else
    begin
{$ifdef unix}
    g:=(fpgetuid=0);
{$else}
    G:=true;
{$endif}
    Result:=GetAppConfigFile(G,False);
    end
end;

procedure TMakeTool.LoadDefaults;
begin
  FDefaults:=TPackagerOptions.Create;
  FDefaults.LoadFromFile(GetConfigFileName);
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
  GlobalOpts : Boolean;
  cmd : string;
  
begin
  I:=0;
  FLogging:=False;
  FRunMode:=rmhelp;
  FConvertOnly:=False;
  GlobalOpts:=True;
  FPackages:=TStringList.Create;
  // We can't use the TCustomApplication option handling,
  // because they cannot handle [general opts] [command] [cmd-opts] [args]
  While (I<ParamCount) do
    begin
    Inc(I);
    // Check options.
    if CheckOption(I,'r','compiler') then
      FDefaults.Compiler:=OptionArg(I)
    else if CheckOption(I,'v','verbose') then
      Include(FVerbose,StringToVerbosity(OptionArg(I)))
    else if CheckOption(I,'h','help') then
      FRunMode:=rmhelp
    else if (Length(Paramstr(i))>0) and (Paramstr(I)[1]='-') then
      Raise EMakeToolError.CreateFmt(SErrInvalidArgument,[I,ParamStr(i)])
    else
      If GlobalOpts then
        begin
        // It's a command.
        Cmd:=Paramstr(I);
        if (Cmd='convert') then
          FConvertOnly:=True
        else if (Cmd='compile') then
          FRunMode:=rmCompile
        else if (Cmd='build') then
          FRunMode:=rmBuild
        else if (Cmd='install') then
          FRunMode:=rmInstall
        else if (cmd='clean') then
          FRunMode:=rmClean
        else if (cmd='archive') then
          FRunMode:=rmarchive
        else if (cmd='download') then
          FRunMode:=rmDownload
        else if (cmd='update') then
          FRunMode:=rmUpdate
        else
          Raise EMakeToolError.CreateFmt(SErrInvalidCommand,[Cmd]);
        end
      else // It's a package name.
        begin
        FPackages.Add(Paramstr(i));
        end;
    end;
end;

procedure TMakeTool.DoRun;


begin
  LoadDefaults;
  Try
    ProcessCommandLine;
    If FConvertOnly then
      CreateFPMake
    else
      begin
      FHaveMakefile:=FileExists('Makefile.fpc');
      FFPMakeSrc:='fpmake.pp';
      FHaveFpmake:=FileExists(FFPMakeSrc);
      If Not FHaveFPMake then
        begin
        FHaveFPMake:=FileExists('fpmake.pas');
        If FHaveFPMake then
          FFPMakeSrc:='fpmake.pas';
        end;
      if Not (FHaveFPMake or FHaveMakeFile) then
        Error(SErrMissingConfig);
      If (Not FHaveFPMake) or (FileAge(FFPMakeSrc)<FileAge('Makefile.fpc')) then
        CreateFPMake;
    {$ifndef unix}
      FFPMakeBin:='fpmake.exe';
    {$else}
      FFPMakeBin:='fpmake';
    {$endif}
      if FileAge(FFPMakeBin)<FileAge(FFPMakeSrc) then
        CompileFPMake(FRunMode in [rmArchive,rmDownload]);
      Halt(RunFPMake);
      end;
  except
    On E : Exception do
      begin
      Writeln(StdErr,Format(SErrRunning,[E.Message]));
      Halt(1);
      end;
  end;
end;


begin
  With TMakeTool.Create(Nil) do
    try
      run;
    finally
      Free;
    end;
end.

