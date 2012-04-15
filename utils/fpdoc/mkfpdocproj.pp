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
    procedure AddImportFiles;
    function CmdNeedsPackage: Boolean;
    procedure RemoveInputFiles;
    procedure RemoveDescrFiles;
    procedure AddPackages;
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

Var
  FN : String;
  I : Integer;

begin
  FN:=ChangeFileExt(ExtractFileName(ParamStr(0)),'');
  Writeln('Usage ',FN,' [options] command [command-options] command-args');
  Writeln('Where options is one of ');
  Writeln('  -i --input=file   Initialize project from named file.');
  Writeln('  -o --output=file  Write project to named file. Default is input file.');
  Writeln('  -p --package=name Package to perform operation on.');
  Writeln('command is one of:');
  Writeln('  add-packages');
  Writeln('    Add arguments as package definitions to the file.');
  Writeln('  add-description-dirs');
  Writeln('    Scan directories for XML files to add as descriptions of selected package.');
  Writeln('  add-input-dirs');
  Writeln('    Scan directories for .pp or .pas files to add as inputs of selected package.');
  Writeln('  add-input-files');
  Writeln('    Add files as inputs of selected package.');
  Writeln('  add-import-files');
  Writeln('    Add files (format: "filename,prefix") to imports of selected package.');
  Writeln('  add-descr-files');
  Writeln('    Add files as description files of selected package.');
  Writeln('  expand-macros');
  Writeln('    read file and expand macros. Arguments specify macro values as Name=Value pairs');
  Writeln('  remove-descr-files');
  Writeln('    Remove files from description files of selected package.');
  Writeln('  remove-input-files');
  Writeln('    Remove files from input files of selected package.');
  Writeln('  set-options');
  Writeln('    Set named options (true) for project file.');
  Writeln('    Valid option names : ');
  Writeln('      hide-protected , warn-no-node, show-private, stop-on-parser-error,');
  Writeln('      parse-impl, dont-trim');
  Writeln('  unset-options');
  Writeln('    UnSet named options (false) for project file.');
  Halt(AExitCode);
end;

Function CheckOptionStr(O : String;Short : Char;Long : String): Boolean;
begin
  Result:=(O='-'+short) or (O='--'+long) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
end;

function TManageFPDocProjectApplication.CmdNeedsPackage : Boolean;

begin
 Result:=(FCMd<>'expand-macros') and (FCMD<>'set-options') and (FCmd<>'unset-options');
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
    if (FCmd='') then
      begin
      if Checkoption(I,'i','input') then
        FInputFileName:=OptionArg(i)
      else if Checkoption(I,'o','output') then
        FOutputFileName:=OptionArg(i)
      else if CheckOption(I,'p','package') then
        FPackageName:=OptionArg(i)
      else if CheckOption(I,'h','help') then
        Usage(0)
      else if (ParamStr(i)<>'') then
        begin
        S:=ParamStr(i);
        if (S[1]='-') then
          Error('Unknown option : '+S)
        else
          FCmd:=lowercase(S)
        end
      end
    else
      begin
      S:=ParamStr(I);
      if (S<>'') then
         if (S[1]<>'-') then
           FCmdArgs.Add(S)
         else
           FCmdOptions.Add(S);
      end;
    end;
  if (FOutputFileName='') then
    FOutputFileName:=FInputFileName;
  If (FOutputFileName='') then
    Error('Need an output filename');
  if (FPackageName='') and CmdNeedsPackage then
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
      Result:=FCmdOptions[I];
      if (Length(Result)>1) and (Result[2]<>'-') then
        begin
        If I<FCmdOptions.Count-1 then
          begin
          Inc(I);
          Result:=FCmdOptions[I];
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
  if FCmdArgs.Count=0 then
    FMGr.AddDescrFilesFromDirectory('',Mask,Recursive)
  else
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
  if FCmdArgs.Count=0 then
    FMGr.AddInputFilesFromDirectory('',Mask,Options,Recursive)
  else
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

procedure TManageFPDocProjectApplication.AddImportFiles;

Var
  I,J : Integer;
  F,P : String;

begin
  For I:=0 to FCmdArgs.Count-1 do
    begin
    P:=FCmdArgs[i];
    J:=Pos(',',P);
    F:=Copy(P,1,J-1);
    Delete(P,1,J);
    FMGr.AddImportFile(F,P);
    end;
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

procedure TManageFPDocProjectApplication.AddPackages;

var
  I : Integer;

begin
  For I:=0 to FCmdArgs.Count-1 do
    FMgr.AddPackage(FCmdArgs[i]);
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
      begin
      FMGR.Macros:=FCmdArgs;
      FMGR.ExpandMacros:=true;
      FMGR.ReadOptionFile(FInputFileName)
      end
    else
      begin
      FMGR.ReadOptionFile(FInputFileName);
      if CmdNeedsPackage then
        FMGR.SelectPackage(FPackageName);
      end
    end;
  if (FCmd='add-packages') then
    AddPackages
  else if (FCmd='add-description-dirs') then
    AddDescriptionDirs
  else if (FCmd='add-input-dirs') then
    AddInputDirs
  else if (FCmd='add-input-files') then
    AddInputFiles
  else if (FCmd='add-import-files') then
    AddImportFiles
  else if (FCmd='add-description-files') then
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

