program mkfpdocproj;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, mgrfpdocproj;

type

  { TManageFPDocProjectApplication }

  TManageFPDocProjectApplication = class(TCustomApplication)
  private
    FMGR : TFPDocProjectManager;
    FPackageName,
    FInputFileName,
    FOutputFileName,
    FCmd : String;
    FCmdArgs,
    FCmdOptions: TStrings;
    procedure AddDescrFiles;
    procedure AddDescriptionDirs;
    procedure AddInputDirs;
    procedure AddInputFiles;
    procedure RemoveInputFiles;
    procedure RemoveDescrFiles;
    function CheckCmdOption(C: Char; S: String): Boolean;
    function GetCmdOption(C: Char; S: String): String;
    procedure SetOptions(Enable: Boolean);
  protected
    procedure ParseOptions;
    Procedure Error(Const Msg : String);
    procedure Usage(AExitCode: Integer);
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;
  end;

Resourcestring
  SErrNeedArgument = 'Option at position %d needs an argument: %s';

{ TManageFPDocProjectApplication }

procedure TManageFPDocProjectApplication.Usage(AExitCode : Integer);

begin
  // to be filled
  Halt(AExitCode);
end;

Function CheckOptionStr(O : String;Short : Char;Long : String): Boolean;
begin
  Result:=(O='-'+short) or (O='--'+long) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
end;

procedure TManageFPDocProjectApplication.ParseOptions;

  Function CheckOption(Index : Integer;Short : char;Long : String): Boolean;
  begin
    Result:=CheckOptionStr(ParamStr(Index),Short,Long);
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
          Error(Format(SErrNeedArgument,[Index,ParamStr(Index)]));
      end
    else If length(ParamStr(Index))>2 then
      begin
        P:=Pos('=',Paramstr(Index));
        If (P=0) then
          Error(Format(SErrNeedArgument,[Index,ParamStr(Index)]))
        else
          begin
            Result:=Paramstr(Index);
            Delete(Result,1,P);
          end;
      end;
  end;

Var
  I : Integer;
  S : String;

begin
  I:=0;
  // We can't use the TCustomApplication option handling,
  // because they cannot handle [general opts] [command] [cmd-opts] [args]
  While (I<ParamCount) do
    begin
    Inc(I);
    if Checkoption(I,'i','input') then
      FInputFileName:=OptionArg(i)
    else if Checkoption(I,'o','output') then
      FOutputFileName:=OptionArg(i)
    else if CheckOption(I,'p','package') then
      FPackageName:=OptionArg(i)
    else if CheckOption(I,'h','help') then
      begin
      Usage(0);
      end
    else
      begin
      S:=ParamStr(I);
      If (S<>'') then
        begin
        if (S[1]<>'-') then
          begin
          if (FCmd='') then
            FCmd:=lowercase(S)
          else
            FCmdArgs.Add(S)
          end
        end
      else
        FCmdOptions.Add(S);
      end;
    Inc(I);
    end;
  if (FOutputFileName='') then
    FOutputFileName:=FInputFileName;
  If (FOutputFileName='') then
    Error('Need an output filename');
  if (FPackageName='') then
    Error('Need a package name');
  if (FCmd='') then
    Error('Need a command');
end;

procedure TManageFPDocProjectApplication.Error(Const Msg: String);
begin
  Writeln('Error : ',Msg);
  Usage(1);
end;


Function TManageFPDocProjectApplication.CheckCmdOption(C : Char; S : String) : Boolean;

Var
  I : integer;

begin
  I:=0;
  Result:=False;
  While (Not Result) and (I<FCmdOptions.Count) do
    begin
    Result:=CheckOptionStr(FCmdOptions[i],C,S);
    Inc(I);
    end;
end;

Function TManageFPDocProjectApplication.GetCmdOption(C : Char; S : String) : String;

Var
  I,P : integer;
  B : Boolean;

begin
  I:=0;
  B:=False;
  While (Not B) and (I<FCmdOptions.Count) do
    begin
    B:=CheckOptionStr(FCmdOptions[i],C,S);
    if B then
      begin
      Result:=FCmdArgs[I];
      if (Length(S)>1) and (S[2]<>'-') then
        begin
        If I<FCmdArgs.Count-1 then
          begin
          Inc(I);
          Result:=FCmdArgs[I];
          end
        else
          Error(Format(SErrNeedArgument,[I,Result]));
        end
      else If length(Result)>2 then
        begin
        P:=Pos('=',Result);
        If (P=0) then
          Error(Format(SErrNeedArgument,[I,Result]))
        else
          Delete(Result,1,P);
        end;
      end;
    Inc(I);
    end;
end;

procedure TManageFPDocProjectApplication.AddDescriptionDirs;

Var
  Recursive: Boolean;
  Mask : String;
  I : Integer;
begin
  Recursive:=CheckCmdOption('r','recursive');
  Mask:=GetCmdOption('m','mask');
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.AddDescrFilesFromDirectory(FCmdArgs[i],Mask,Recursive);
end;

procedure TManageFPDocProjectApplication.AddInputDirs;

Var
  Recursive: Boolean;
  Options,Mask : String;
  I : Integer;
begin
  Recursive:=CheckCmdOption('r','recursive');
  Mask:=GetCmdOption('m','mask');
  Options:=GetCmdOption('o','options');
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.AddInputFilesFromDirectory(FCmdArgs[i],Mask,Options,Recursive);
end;

procedure TManageFPDocProjectApplication.AddInputFiles;

Var
  Options : String;
  I : Integer;

begin
  Options:=GetCmdOption('o','options');
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.AddInputFile(FCmdArgs[i],Options);
end;

procedure TManageFPDocProjectApplication.RemoveInputFiles;

Var
  I : Integer;

begin
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.RemoveInputFile(FCmdArgs[i]);
end;

procedure TManageFPDocProjectApplication.RemoveDescrFiles;
Var
  I : Integer;

begin
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.RemoveDescrFile(FCmdArgs[i]);
end;

procedure TManageFPDocProjectApplication.AddDescrFiles;

Var
  I : Integer;

begin
  For I:=0 to FCmdArgs.Count-1 do
    FMGr.AddDescrFile(FCmdArgs[i]);
end;

procedure TManageFPDocProjectApplication.SetOptions(Enable : Boolean);

Var
  I : Integer;

begin
  For I:=0 to FCmdArgs.Count-1 do
    FMgr.SetOption(FCmdArgs[i],Enable);
end;

procedure TManageFPDocProjectApplication.DoRun;

begin
  ParseOptions;
  if (FInputFileName='') then
    FMGR.AddPackage(FPackageName)
  else
    begin
    if (FCmd='expand-macros') then
      FMGR.ReadOptionFile(FInputFileName)
    else
      FMGR.ReadOptionFile(FInputFileName,FCMdArgs);
    FMGR.SelectPackage(FPackageName);
    end;
  if (FCmd='add-description-dirs') then
    AddDescriptionDirs
  else if (FCmd='add-input-dirs') then
    AddInputDirs
  else if (FCmd='add-input-files') then
    AddInputFiles
  else if (FCmd='add-descr-files') then
    AddDescrFiles
  else if (FCmd='remove-input-files') then
    RemoveInputFiles
  else if (FCmd='remove-descr-files') then
    RemoveDescrFiles
  else if (FCmd='set-options') then
    SetOptions(True)
  else if (FCmd='unset-options') then
    SetOptions(False)
  else if (FCMd<>'expand-macros') then
    Error(Format('Unknown command : "%s"',[FCmd]));
  FMgr.WriteOptionFile(FOutputFileName);
  Terminate;
end;

constructor TManageFPDocProjectApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FCmdArgs:=TStringList.Create;
  FCmdOptions:=TStringList.Create;
  FMGR:=TFPDocProjectManager.Create(Self);
 end;

destructor TManageFPDocProjectApplication.Destroy;
begin
  FreeAndNil(FMGR);
  FreeAndNil(FCmdArgs);
  FreeAndNil(FCmdOptions);
  inherited Destroy;
end;

var
  Application: TManageFPDocProjectApplication;
begin
  Application:=TManageFPDocProjectApplication.Create(nil);
  Application.Title:='Program to manipulate FPDoc project files';
  Application.Run;
  Application.Free;
end.

