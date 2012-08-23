program pas2fpm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, passrcutil;

type

  { TUnitEntry }

  TUnitEntry = Class(TCollectionItem)
  private
    FIntfDeps: TStrings;
    FImplDeps: TStrings;
    FDone: Boolean;
    FErr: String;
    FFileName : String;
    FName: String;
    FProcessing: Boolean;
    Fres: Boolean;
    function GetName: String;
  Public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    Procedure CleanIntfDependencies(Verbose : Boolean);
    Procedure CleanImplDependencies(Verbose : Boolean);
    Procedure OrderDependencies(Order : TStrings);
    Function Nodependencies : Boolean;
    Property FileName : String Read FFileName Write FFileName;
    Property Name : String Read GetName;
    Property IntfDependencies : TStrings Read FIntfDeps;
    Property ImplDependencies : TStrings Read FImplDeps;
    Property Resources : Boolean Read Fres Write Fres;
    Property Err : String Read FErr Write Ferr;
    Property Done : Boolean Read FDone Write FDone;
    Property Processing : Boolean Read FProcessing Write FProcessing;
  end;

  { TUnitEntries }

  TUnitEntries = Class(TCollection)
  private
    function GetE(AIndex : Integer): TUnitEntry;
  public
    Function IndexOfEntry(Const AName : String) : Integer;
    Function FindEntry(Const AName : string) : TUnitEntry;
    Function AddEntry(Const AFileName : String) : TUnitEntry;
    Property Units[AIndex : Integer] : TUnitEntry Read GetE; default;
  end;


  { TPas2FPMakeApp }

  TPas2FPMakeApp = class(TCustomApplication)
  private
    procedure AddLine(const ALine: String);
    function CheckParams : boolean;
    procedure CreateSources;
    Procedure ProcessUnits;
    function  GetUnitProps(const FN: String; out Res: Boolean; UIn,UIm: TStrings; Out Err : string): Boolean;
    Function SimulateCompile(E,EFrom: TUnitEntry) : Boolean;
    procedure WriteProgEnd;
    procedure WriteProgStart;
    procedure WriteSources;
  protected
    FVerbose : Boolean;
    FFiles : TUnitEntries;
    FSrc,
    FUnits: TStrings;
    InterfaceUnitsOnly : Boolean;
    FPackageName : string;
    FOutputFile : string;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUnitEntries }

function TUnitEntries.GetE(AIndex : Integer): TUnitEntry;
begin
  Result:=Items[AIndex] as TUnitEntry;
end;

function TUnitEntries.IndexOfEntry(const AName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetE(Result).Name,AName)<>0) do
    Dec(Result);
end;

function TUnitEntries.FindEntry(const AName: string): TUnitEntry;

Var
  I:Integer;
begin
  I:=IndexofEntry(Aname);
  If (I<>-1) then
    Result:=GetE(I)
  else
    Result:=Nil;
end;

function TUnitEntries.AddEntry(Const AFileName: String): TUnitEntry;
begin
  Result:=Add as TunitEntry;
  Result.FileName:=AFileName;
end;

{ TUnitEntry }

function TUnitEntry.GetName: String;
begin
  Result:=ChangeFileExt(ExtractFileName(FileName),'');
end;

constructor TUnitEntry.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIntfDeps:=TStringList.Create;
  FImplDeps:=TStringList.Create;
end;

destructor TUnitEntry.Destroy;
begin
  FreeAndNil(FIntfDeps);
  FreeAndNil(FImplDeps);
  inherited Destroy;
end;

procedure TUnitEntry.CleanIntfDependencies(Verbose : Boolean);

Var
  I,J : Integer;
  U : TUnitEntry;

begin
  For I:=FintfDeps.Count-1 downto 0 do
    begin
    U:=FIntfDeps.Objects[i] as TUnitEntry;
    J:=U.ImplDependencies.IndexOf(Name);
    if J<>-1 then
      begin
      U.ImplDependencies.Delete(J);
      If Verbose then
        Writeln(StdErr,'Removing interdependency of ',Name,' from ',U.Name);
      end;
    end;

end;

procedure TUnitEntry.CleanImplDependencies(Verbose : Boolean);

Var
  I,J : Integer;
  U : TUnitEntry;

begin
  For I:=FImplDeps.Count-1 downto 0 do
    begin
    U:=FImplDeps.Objects[i] as TUnitEntry;
    J:=U.ImplDependencies.IndexOf(Name);
    if J<>-1 then
      begin
      U.ImplDependencies.Delete(J);
      If Verbose then
        Writeln(StdErr,'Removing interdependency of ',Name,' from ',U.Name);
      end;
    end;
end;

procedure TUnitEntry.OrderDependencies(Order: TStrings);

Var
  L : TStringList;
  I,CC : integer;

begin
  L:=TstringList.Create;
  try
    L.Assign(FintfDeps);
    L.Sorted:=True;
    CC:=L.Count;
    FintfDeps.Clear;
    For I:=0 to Order.Count-1 do
      if L.Indexof(Order[i])<>-1 then
        FIntfDeps.Add(Order[i]);
    If FintfDeps.Count<>CC then
      Writeln('Internal error 1');
    L.Sorted:=False;
    L.Assign(FimplDeps);
    CC:=L.Count;
    L.Sorted:=True;
    FImplDeps.Clear;
    For I:=0 to Order.Count-1 do
      if L.Indexof(Order[i])<>-1 then
        FImplDeps.Add(Order[i]);
    If FImplDeps.Count<>CC then
      Writeln('Internal error 2');
  finally
    L.free;
  end;
end;

function TUnitEntry.Nodependencies: Boolean;
begin
  Result:=(FIntfDeps.Count=0) and (FImplDeps.Count=0);
end;

{ TPas2FPMakeApp }

Function TPas2FPMakeApp.CheckParams : Boolean;

  Procedure AddFileMask(S : String);

  Var
    Info : TSearchRec;
    D : String;

  begin
    D:=ExtractFilePath(S);
    If FindFirst(S,0,Info)=0 then
      try
        Repeat
          FFiles.AddEntry(D+Info.Name);
          FUnits.Add(ChangeFileExt(ExtractFileName(info.name),''));
        until (FindNext(Info)<>0);
      finally
        FindClose(Info);
      end;
  end;

Var
  I : Integer;
  S : String;

begin
  Result:=True;
  I:=1;
  While I<=ParamCount do
    begin
    S:=Paramstr(i);
    if (S<>'') then
      begin
      if S[1]<>'-' then
        begin
        If (Pos('?',S)<>0) or (Pos('*',S)<>0) then
          AddFileMask(S)
        else if comparetext(ChangeFileExt(extractfilename(s),''),'fpmake')<>0 then
          begin
          FFiles.AddEntry(S);
          FUnits.Add(ChangeFileExt(ExtractFileName(S),''));
          end;
        end
      else
        begin
        If (s='o') then
          begin
          inc(I);
          FoutputFile:=ParamStr(i);
          end
        else If (s='-i') then
          InterfaceUnitsOnly:=True
        else If (s='-v') then
          FVerbose:=True
        else if (s='-p') then
          begin
          Inc(i);
          FPackageName:=ParamStr(i);
          end
        else
          begin
          Result:=False;
          exit;
          end;
        end;
      end;
    Inc(i);
    end;
  Result:=(FFiles.Count>0);
end;

procedure TPas2FPMakeApp.AddLine(Const ALine : String);

begin
  FSrc.Add(ALine);
end;

Function TPas2FPMakeApp.GetUnitProps(Const FN : String; Out Res : Boolean; UIn,UIm : TStrings; Out Err : string) : Boolean;

Var
  I,J : Integer;
  A : TPasSrcAnalysis;

begin
  Result:=False;
  try
    If FVerbose then
      Writeln(StdErr,'Analysing unit ',FN);
    A:=TPasSrcAnalysis.Create(Self);
    try
      A.FileName:=FN;
      Res:=A.HasResourcestrings;
        A.GetInterfaceUnits(Uin);
      if Not InterfaceUnitsOnly then
        A.GetImplementationUnits(Uim);
      For I:=Uin.Count-1 downto 0 do
        begin
        J:=FUnits.IndexOf(UIN[i]);
        if (j=-1) then
          Uin.Delete(i)
        else
          Uin.Objects[i]:=FUnits.Objects[J];
        end;
      For I:=Uim.Count-1 downto 0 do
        begin
        J:=FUnits.IndexOf(UIm[i]);
        if (j=-1) then
          Uim.Delete(i)
        else
          Uim.Objects[i]:=FUnits.Objects[J];
        end;
    finally
      A.Free;
    end;
    Result:=True;
  except
    On E : Exception do
      Err:=E.Message;
    // Ignore
  end;

end;

procedure TPas2FPMakeApp.WriteProgStart;

begin
  AddLine('program fpmake;');
  AddLine('');
  AddLine('uses fpmkunit;');
  AddLine('');
  AddLine('Var');
  AddLine('  T : TTarget;');
  AddLine('  P : TPackage;');
  AddLine('begin');
  AddLine('  With Installer do');
  AddLine('    begin');
  AddLine('    P:=AddPackage('''+FPackageName+''');');
  AddLine('    P.Version:=''0.0'';');
//  AddLine('    P.Dependencies.Add('fcl-base');
  AddLine('    P.Author := ''Your name'';');
  AddLine('    P.License := ''LGPL with modification'';');
  AddLine('    P.HomepageURL := ''www.yourcompany.com'';');
  AddLine('    P.Email := ''yourmail@yourcompany.com'';');
  AddLine('    P.Description := ''Your very nice program'';');
  AddLine('    // P.NeedLibC:= false;');
end;

procedure TPas2FPMakeApp.WriteProgEnd;

begin
  AddLine('    Run;');
  AddLine('    end;');
  AddLine('end.');
end;

procedure TPas2FPMakeApp.CreateSources;


Var
  I,j : Integer;
  U : TStrings;
  F : TUnitEntry;
  FN : String;

begin
  WriteProgStart;
  For I:=0 to FUnits.Count-1 do
    begin
    F:=FFiles.FindEntry(FUnits[i]);
    FN:=F.FileName;
    AddLine('    T:=P.Targets.AddUnit('''+FN+''');');
    if F.Err<>'' then
      AddLine('    // Failed to analyse unit "'+Fn+'". Error: "'+F.Err+'"')
    else
      begin
      if F.Resources then
        AddLine('    T.ResourceStrings := True;');
      U:=TStringList.Create;
      try
        U.AddStrings(F.IntfDependencies);
        U.AddStrings(F.ImplDependencies);
        if (U.Count>0) then
          begin
          AddLine('    with T.Dependencies do');
          AddLine('      begin');
          For J:=0 to U.Count-1 do
            AddLine('      AddUnit('''+U[j]+''');');
          AddLine('      end;');
          end;
      finally
        U.Free;
      end;
      end;
    end;
  WriteProgEnd;
end;

function TPas2FPMakeApp.SimulateCompile(E,EFrom: TUnitEntry): Boolean;

Var
  I : Integer;

begin
  Result:=True;
  if E.Done then
    begin
    Result:=Not E.Processing;
    if FVerbose then
      if Not Result then
        Writeln(StdErr,'Detected circular reference ',E.Name,' coming from ',EFrom.Name)
      else if Assigned(EFrom) then
        Writeln(StdErr,'Attempt to recompile ',E.Name,' coming from ',EFrom.Name)
      else
        Writeln(StdErr,'Attempt to recompile ',E.Name);
    exit;
    end;
  E.Done:=True;
  E.Processing:=True;
  For I:=0 to E.IntfDependencies.Count-1 do
    SimulateCompile(E.IntfDependencies.Objects[I] as TUnitEntry,E);
  For I:=0 to E.ImplDependencies.Count-1 do
    SimulateCompile(E.ImplDependencies.Objects[I] as TUnitEntry,E);
  E.Processing:=False;
  FUnits.Add(E.Name);
end;

procedure TPas2FPMakeApp.ProcessUnits;

Var
  I,J,k : integer;
  Err : String;
  F : TUnitEntry;
  R : Boolean;

begin
  For I:=0 to Funits.Count-1 do
    begin
    J:=FFiles.IndexOfEntry(FUnits[i]);
    Funits.Objects[i]:=FFiles[J];
    end;
  TStringList(FUnits).Sorted:=True;
  For I:=0 to FFiles.Count-1 do
    begin
    F:=FFiles[i];
    if not GetUnitProps(F.FileName,R,F.IntfDependencies,F.ImplDependencies,Err) then
      F.Err:=Err
    else
      F.Resources:=R;
    end;
  For I:=0 to FFiles.Count-1 do
    FFiles[i].CleanIntfDependencies(FVerbose);
  For I:=0 to FFiles.Count-1 do
    FFiles[i].CleanImplDependencies(FVerbose);
  TStringList(FUnits).Sorted:=False;
  FUnits.Clear;
  For I:=0 to FFiles.Count-1 do
    if FFiles[i].NoDependencies then
      begin
      FUnits.Add(FFiles[i].Name);
      FFiles[i].Done:=True;
      end;
  For I:=0 to FFiles.Count-1 do
    SimulateCompile(FFiles[i],Nil);
  // At this point, FUnits is in the order that the compiler should compile them.
   //  Now we order the dependencies.
   For I:=0 to FFiles.Count-1 do
     FFiles[i].OrderDependencies(FUnits);
end;

procedure TPas2FPMakeApp.WriteSources;

Var
  F : Text;

begin
  AssignFile(F,FOutputFile);
  Rewrite(F);
  try
    Write(F,FSrc.Text);
  finally
    CloseFile(F);
  end;
end;

procedure TPas2FPMakeApp.DoRun;

var
  ErrorMsg: String;

begin
  // parse parameters
  if HasOption('h','help') or Not CheckParams then
    begin
    WriteHelp;
    Terminate;
    exit;
    end;
  ProcessUnits;
  CreateSources;
  WriteSources;
  // stop program loop
  Terminate;
end;

constructor TPas2FPMakeApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FFiles:=TUnitEntries.Create(TUnitEntry);
  FSrc:=TStringList.Create;
  FUnits:=TStringList.Create;
  FPackageName:='Your package name here';
end;

destructor TPas2FPMakeApp.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FSrc);
  FreeAndNil(FUnits);
  inherited Destroy;
end;

procedure TPas2FPMakeApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' [options] file1 .. filen');
  Writeln('Where [options] is one or more of');
  Writeln(' -h               This help');
  Writeln(' -p packagename   Set package name');
  Writeln(' -i               Use interface units only for checking dependencies');
  Writeln(' -o outputfile    Set output filename (default is standard output)');
  Writeln(' -v               Write diagnostic output to stderr');
end;

var
  Application: TPas2FPMakeApp;
begin
  Application:=TPas2FPMakeApp.Create(nil);
  Application.Title:='Pascal to FPMake application';
  Application.Run;
  Application.Free;
end.

