unit pkgFppkg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fprepos,
  pkgmessages,
  pkgglobals,
  pkgoptions;

type

  { TpkgFPpkg }

  TpkgFPpkg = class(TComponent)
  private
    FOptions: TFppkgOptions;
    FCompilerOptions: TCompilerOptions;
    FFpmakeCompilerOptions: TCompilerOptions;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeGlobalOptions(CfgFile: string);
    procedure InitializeCompilerOptions;
    property Options: TFppkgOptions read FOptions;
    property CompilerOptions: TCompilerOptions read FCompilerOptions;
    property FpmakeCompilerOptions: TCompilerOptions read FFpmakeCompilerOptions;
  end;

implementation

{ TpkgFPpkg }

constructor TpkgFPpkg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TFppkgOptions.Create;
  FCompilerOptions := TCompilerOptions.Create;
  FFpmakeCompilerOptions := TCompilerOptions.Create;
end;

destructor TpkgFPpkg.Destroy;
begin
  FCompilerOptions.Free;
  FFpmakeCompilerOptions.Free;
  FOptions.Free;
  inherited Destroy;
end;

procedure TpkgFPpkg.InitializeGlobalOptions(CfgFile: string);
var
  i : integer;
  GeneratedConfig,
  UseGlobalConfig : boolean;
begin
  GeneratedConfig:=false;
  UseGlobalConfig:=false;
  // First try specified config file
  if (CfgFile<>'') then
    begin
      if not FileExists(cfgfile) then
        Error(SErrNoSuchFile,[cfgfile]);
    end
  else
    begin
      // Now try if a local config-file exists
      cfgfile:=GetAppConfigFile(False,False);
      if not FileExists(cfgfile) then
        begin
          // If not, try to find a global configuration file
          cfgfile:=GetAppConfigFile(True,False);
          if FileExists(cfgfile) then
            UseGlobalConfig := true
          else
            begin
              // Create a new configuration file
              if not IsSuperUser then // Make a local, not global, configuration file
                cfgfile:=GetAppConfigFile(False,False);
              ForceDirectories(ExtractFilePath(cfgfile));
              FOptions.SaveToFile(cfgfile);
              GeneratedConfig:=true;
            end;
        end;
    end;
  // Load file or create new default configuration
  if not GeneratedConfig then
    begin
      FOptions.LoadFromFile(cfgfile);
    end;
  FOptions.CommandLineSection.CompilerConfig:=FOptions.GlobalSection.CompilerConfig;
  // Tracing of what we've done above, need to be done after the verbosity is set
  if GeneratedConfig then
    pkgglobals.Log(llDebug,SLogGeneratingGlobalConfig,[cfgfile])
  else
    pkgglobals.Log(llDebug,SLogLoadingGlobalConfig,[cfgfile]);
  // Log configuration
  FOptions.LogValues(llDebug);
end;

procedure TpkgFPpkg.InitializeCompilerOptions;
var
  S : String;
begin
  // Load default compiler config
  S:=FOptions.GlobalSection.CompilerConfigDir+FOptions.GlobalSection.CompilerConfig;
  FCompilerOptions.UpdateLocalRepositoryOption(FOptions);
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingCompilerConfig,[S]);
      FCompilerOptions.LoadCompilerFromFile(S)
    end
  else
    begin
      // Generate a default configuration if it doesn't exists
      if FOptions.GlobalSection.CompilerConfig='default' then
        begin
          pkgglobals.Log(llDebug,SLogGeneratingCompilerConfig,[S]);
          FCompilerOptions.InitCompilerDefaults;
          FCompilerOptions.SaveCompilerToFile(S);
          if FCompilerOptions.SaveInifileChanges then
            FCompilerOptions.SaveCompilerToFile(S);
        end
      else
        Error(SErrMissingCompilerConfig,[S]);
    end;
  // Log compiler configuration
  FCompilerOptions.LogValues(llDebug,'');
  // Load FPMake compiler config, this is normally the same config as above
  S:=FOptions.GlobalSection.CompilerConfigDir+FOptions.GlobalSection.FPMakeCompilerConfig;
  FFPMakeCompilerOptions.UpdateLocalRepositoryOption(FOptions);
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingFPMakeCompilerConfig,[S]);
      FFPMakeCompilerOptions.LoadCompilerFromFile(S);
      if FFPMakeCompilerOptions.SaveInifileChanges then
        FFPMakeCompilerOptions.SaveCompilerToFile(S);
    end
  else
    Error(SErrMissingCompilerConfig,[S]);
  // Log compiler configuration
  FFPMakeCompilerOptions.LogValues(llDebug,'fpmake-building');
end;

end.

