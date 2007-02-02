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
  pkghandler, pkgmkconv, pkgdownload, pkgfpmake, pkgmessages;

Type
  { TMakeTool }

  TMakeTool = Class(TCustomApplication)
  Private
    FDefaults: TPackagerOptions;
    FCompiler : String;
    Function GetCompiler : String;
    procedure ShowUsage;
  Public
    Function GetConfigFileName : String;
    Procedure LoadDefaults;
    Procedure ProcessCommandLine;
    Procedure DoRun; Override;
    procedure ExecuteAction(const AAction:string;const Args:TActionArgs);
  end;

  EMakeToolError = Class(Exception);


{ TMakeTool }

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
    Result:=FileSearch(FCompiler+ExeExt,GetEnvironmentVariable('PATH'));
    If (Result='') then
      Result:=FCompiler+ExeExt;
    end;
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
  Verbosity:=[vError,vInfo,vCommands,vDebug];
  FDefaults:=TPackagerOptions.Create;
  FDefaults.LoadFromFile(GetConfigFileName);
end;


procedure TMakeTool.ShowUsage;
begin
  Writeln('Usage: ',Paramstr(0),' [options] <action> <package>');
  Writeln('Options:');
  Writeln('  -r --compiler      Set compiler');
  Writeln('  -h --help          This help');
  Writeln('  -v --verbose       Set verbosity');
  Writeln('Actions:');
  Writeln('  update             Update available packages');
  Writeln('  listpackages       List available packages');
  Writeln('  build              Build package');
  Writeln('  install            Install package');
  Writeln('  download           Download package');
  Writeln('  convertmk          Convert Makefile.fpc to fpmake.pp');
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
  Action : string;
  ParaPackages : TStringList;
  HasAction : Boolean;
begin
  try
    I:=0;
    HasAction:=false;
    ParaPackages:=TStringList.Create;
    // We can't use the TCustomApplication option handling,
    // because they cannot handle [general opts] [command] [cmd-opts] [args]
    While (I<ParamCount) do
      begin
        Inc(I);
        // Check options.
        if CheckOption(I,'r','compiler') then
          FDefaults.Compiler:=OptionArg(I)
        else if CheckOption(I,'v','verbose') then
          Include(Verbosity,StringToVerbosity(OptionArg(I)))
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
                Action:=Paramstr(i);
                HasAction:=true;
              end;
          end;
      end;
    if HasAction then
      begin
        if GetPkgHandler(Action)<>nil then
          begin
            for i:=0 to ParaPackages.Count-1 do
              ActionStack.Push(Action,[ParaPackages[i]])
          end
        else
          Raise EMakeToolError.CreateFmt(SErrInvalidCommand,[Action]);
      end
    else
      ShowUsage;
  finally
    FreeAndNil(ParaPackages);
  end;
end;


procedure TMakeTool.ExecuteAction(const AAction:string;const Args:TActionArgs);
var
  pkghandlerclass : TPackageHandlerClass;
  i : integer;
  logargs : string;
begin
  if vDebug in Verbosity then
    begin
      logargs:='';
      for i:=Low(Args) to High(Args) do
        begin
          if logargs='' then
            logargs:=Args[i]
          else
            logargs:=logargs+','+Args[i];
        end;
      Log(vDebug,SLogRunAction,[AAction,logargs]);
    end;
  pkghandlerclass:=GetPkgHandler(AAction);
  With pkghandlerclass.Create(FDefaults) do
    try
      Execute(Args);
    finally
      Free;
    end;
end;


procedure TMakeTool.DoRun;
var
  Action : string;
  Args   : TActionArgs;
begin
  LoadDefaults;
  Try
    ProcessCommandLine;
    
    repeat
      if not ActionStack.Pop(Action,Args) then
        break;
      ExecuteAction(Action,Args);
    until false;
    Terminate;
    
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

