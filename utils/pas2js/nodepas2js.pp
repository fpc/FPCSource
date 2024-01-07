program nodepas2js;

{$mode objfpc}
{$I pas2js_defines.inc}

uses
  JS, NodeJSApp,
  Classes, SysUtils,
  Pas2jsFileUtils, Pas2jsLogger, Pas2jsCompiler,
  Pas2JSFSCompiler, {Pas2JSCompilerPP, }Pas2JSCompilerCfg;

type

  { TPas2jsCLI }

  TPas2jsCLI = class(TNodeJSApplication)
  private
    FCompiler: TPas2jsFSCompiler;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Compiler: TPas2jsFsCompiler read FCompiler;
  end;

procedure TPas2jsCLI.DoRun;
var
  ParamList: TStringList;
  i: Integer;
begin
  ParamList:=TStringList.Create;
  try
    for i:=1 to ParamCount do
      ParamList.Add(Params[i]);
    try
      Compiler.Run(ParamStr(0),GetCurrentDirPJ,ParamList);
    except
      on E: ECompilerTerminate do ;
      on E: Exception do
      begin
        {AllowWriteln}
        writeln(E.Message);
        {AllowWriteln-}
        if ExitCode=0 then
          ExitCode:=ExitCodeErrorInternal;
      end
      else begin
        {AllowWriteln}
        writeln('ERROR value: ',JSExceptValue);
        {AllowWriteln-}
        if ExitCode=0 then
          ExitCode:=ExitCodeErrorInternal;
      end;
    end;
  finally
    ParamList.Free;
    Compiler.Log.CloseOutputFile;
  end;

  // stop program loop
  Terminate; // Keep ExitCode!
end;

constructor TPas2jsCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCompiler:=TPas2jsFSCompiler.Create;
  FCompiler.ConfigSupport:=TPas2JSFileConfigSupport.Create(FCompiler);
end;

destructor TPas2jsCLI.Destroy;
begin
  FreeAndNil(FCompiler);
  inherited Destroy;
end;

var
  Application: TPas2jsCLI;
begin
  Application:=TPas2jsCLI.Create(nil);
  Application.Run;
  Application.Free;
end.

