{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program FPDoc;

uses
  SysUtils, Classes, Gettext, DOM, XMLWrite, PasTree, PParser, custapp,
  dGlobals,  // GLobal definitions, constants.
  dwriter,   // TFPDocWriter definition.
  dwlinear,  // Linear (abstract) writer
  dw_LaTeX,  // TLaTex writer
  dw_XML,    // XML writer
  dw_dxml,   // Delphi XML doc.
  dw_HTML,   // HTML writer
  dw_ipflin, // IPF writer (new linear output)
  dw_man,    // Man page writer
  dw_linrtf, // linear RTF writer
  dw_txt, fpdocproj, fpdocxmlopts;    // TXT writer

const
  DefOSTarget    = {$I %FPCTARGETOS%};
  DefCPUTarget   = {$I %FPCTARGETCPU%};
  DefFPCVersion  = {$I %FPCVERSION%};
  DefFPCDate     = {$I %FPCDATE%};

Type

  { TFPDocAplication }

  TFPDocAplication = Class(TCustomApplication)
  private
    FProject : TFPDocProject;
    FProjectFile : Boolean;
    FPackage : TFPDocPackage;
  Protected
    procedure ParseCommandLine;
    procedure Parseoption(const S: String);
    Procedure Usage(AnExitCode : Byte);
    procedure CreateDocumentation(APackage : TFPDocPackage; Options : TEngineOptions);
    Procedure DoRun; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function SelectedPackage : TFPDocPackage;
  end;


Procedure TFPDocAplication.Usage(AnExitCode : Byte);

Var
  I,P : Integer;
  S : String;
  L : TStringList;
  C : TFPDocWriterClass;
  Backend : String;

begin
  Writeln(Format(SCmdLineHelp,[ExtractFileName(Paramstr(0))]));
  Writeln(SUsageOption010);
  Writeln(SUsageOption020);
  Writeln(SUsageOption030);
  Writeln(SUsageOption040);
  Writeln(SUsageOption050);
  Writeln(SUsageOption060);
  Writeln(SUsageOption070);
  Writeln(SUsageOption080);
  Writeln(SUsageOption090);
  Writeln(SUsageOption100);
  Writeln(SUsageOption110);
  Writeln(SUsageOption120);
  Writeln(SUsageOption130);
  Writeln(SUsageOption140);
  Writeln(SUsageOption150);
  Writeln(SUsageOption160);
  Writeln(SUsageOption170);
  Writeln(SUsageOption180);
  Writeln(SUsageOption190);
  Writeln(SUsageOption200);
  L:=TStringList.Create;
  Try
    Backend:=FProject.OPtions.Backend;
    If (Backend='') then
      begin
      Writeln;
      Writeln(SUsageFormats);
      EnumWriters(L);
      For I:=0 to L.Count-1 do
        begin
        S:=L[i];
        P:=Pos('=',S);
        Writeln(Format(' %s - %s',[Copy(S,1,P-1)+Space(10-p),Copy(S,P+1,Length(S))]));
        end;
      Writeln(SUsageBackendHelp);
      end
    else
      begin
      Writeln;
      Writeln(Format(SUsageFormatSpecific,[Lowercase(backend)]));
      C:=GetWriterClass(Backend);
      C.Usage(L);
      If L.Count>0 then
        For I:=0 to (L.Count-1) div 2 do
          begin
          S:=L[i*2];
          Writeln(Format('%s %s',[S+Space(30-Length(S)),L[(i*2)+1]]));
          end;
      end;
  Finally
    L.Free;
  end;
  Halt(AnExitCode);
end;

destructor TFPDocAplication.Destroy;

begin
  FreeAndNil(FProject);
  Inherited;
end;

function TFPDocAplication.SelectedPackage: TFPDocPackage;
begin
  Result:=FPackage;
  if (FPackage=Nil) or (FPackage.Name='') then
    begin
    Writeln(SNeedPackageName);
    Usage(1);
    end;
end;


procedure TFPDocAplication.ParseCommandLine;

  Function ProjectOpt(Const s : string) : boolean;

  begin
    Result:=(Copy(s,1,3)='-p=') or (Copy(s,1,10)='--project=');
  end;

  Function PackageOpt(Const s : string) : boolean;

  begin
    Result:=((Copy(s,1,3)='-a=') or (Copy(s,1,10)='--package='));
  end;

var
  i : Integer;
  s : string;

begin
  // Check project
  for i := 1 to ParamCount do
    begin
    s:=ParamStr(I);
    If ProjectOpt(S) then
      ParseOption(s);
    If (FProject.Packages.Count=1) then
      FPackage:=FProject.Packages[0]
    else if (FProject.Options.DefaultPackageName<>'') then
      Fpackage:=FProject.Packages.FindPackage(FProject.Options.DefaultPackageName);
    end;
  If FProject.Packages.Count=0 then
    begin
    FPackage:=FProject.Packages.Add as  TFPDocPackage;
    end;
  // Check package
  for i := 1 to ParamCount do
    begin
    s:=ParamStr(I);
    If PackageOpt(S) then
      ParseOption(s);
    end;
  for i := 1 to ParamCount do
    begin
    s:=ParamStr(I);
    If Not (ProjectOpt(s) or PackageOpt(S)) then
      ParseOption(s);
    end;
  if (FPackage=Nil) or (FPackage.Name='') then
    begin
    Writeln(SNeedPackageName);
    Usage(1);
    end;
end;

procedure TFPDocAplication.Parseoption(Const S : String);

  procedure AddToFileList(List: TStrings; const FileName: String);
  var
    f: Text;
    s: String;
  begin
    if Copy(FileName, 1, 1) = '@' then
    begin
      AssignFile(f, Copy(FileName, 2, Length(FileName)));
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
        List.Add(s);
      end;
      Close(f);
    end else
      List.Add(FileName);
  end;

var
  i: Integer;
  Cmd, Arg: String;

begin
  if (s = '-h') or (s = '--help') then
    Usage(0)
  else if s = '--hide-protected' then
    FProject.Options.HideProtected := True
  else if s = '--warn-no-node' then
    FProject.Options.WarnNoNode := True
  else if s = '--show-private' then
    FProject.Options.ShowPrivate := False
  else if s = '--stop-on-parser-error' then
    FProject.Options.StopOnParseError := True
  else if s = '--dont-trim' then
    FProject.Options.donttrim := True
  else
    begin
    i := Pos('=', s);
    if i > 0 then
      begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
      end
    else
      begin
      Cmd := s;
      SetLength(Arg, 0);
      end;
    if (Cmd = '--project') or (Cmd='-p') then
      begin
      FProjectFile:=True;
      With TXMLFPDocOptions.Create(self) do
        try
          LoadOptionsFromFile(FProject,Arg);
        finally
          Free;
        end;
      end
    else if (Cmd = '--descr') then
      AddToFileList(SelectedPackage.Descriptions, Arg)
    else if (Cmd = '-f') or (Cmd = '--format') then
      begin
      Arg:=UpperCase(Arg);
      If FindWriterClass(Arg)=-1 then
        WriteLn(StdErr, Format(SCmdLineInvalidFormat, [Arg]))
      else
        FProject.Options.BackEnd:=Arg;
      end
    else if (Cmd = '-l') or (Cmd = '--lang') then
      FProject.Options.Language := Arg
    else if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(SelectedPackage.Inputs, Arg)
    else if (Cmd = '-o') or (Cmd = '--output') then
      SelectedPackage.Output := Arg
    else if Cmd = '--content' then
      SelectedPackage.ContentFile := Arg
    else if Cmd = '--import' then
      SelectedPackage.Imports.Add(Arg)
    else if Cmd = '--package' then
      begin
      If FProjectFile then
        FPackage:=FProject.Packages.FindPackage(Arg)
      else
        FPackage.Name:=Arg;
      end
    else if Cmd = '--ostarget' then
      FProject.Options.OSTarget := Arg
    else if Cmd = '--cputarget' then
      FProject.Options.CPUTarget := Arg
    else if Cmd = '--mo-dir' then
      FProject.Options.modir := Arg
    else if Cmd = '--parse-impl' then
      FProject.Options.InterfaceOnly:=false
    else
      begin
      FProject.Options.BackendOptions.Add(Cmd);
      FProject.Options.BackendOptions.Add(Arg);
      end;
    end;
end;


procedure TFPDocAplication.CreateDocumentation(APackage : TFPDocPackage; Options : TEngineOptions);

var
  i,j: Integer;
  WriterClass : TFPDocWriterClass;
  Writer : TFPDocWriter;
  Engine : TFPDocEngine;
  Cmd,Arg : String;

begin
  Engine:=TFPDocEngine.Create;
  try
    For J:=0 to Apackage.Imports.Count-1 do
      begin
      Arg:=Apackage.Imports[j];
      i := Pos(',', Arg);
      Engine.ReadContentFile(Copy(Arg,1,i-1),Copy(Arg,i+1,Length(Arg)));
      end;
    for i := 0 to APackage.Descriptions.Count - 1 do
      Engine.AddDocFile(APackage.Descriptions[i],Options.donttrim);
    Engine.SetPackageName(APackage.Name);
    Engine.Output:=APackage.Output;
    Engine.HideProtected:=Options.HideProtected;
    Engine.HidePrivate:=Not Options.ShowPrivate;
    if Length(Options.Language) > 0 then
      TranslateDocStrings(Options.Language);
    for i := 0 to Fpackage.Inputs.Count - 1 do
      try
        ParseSource(Engine, APackage.Inputs[i], Options.OSTarget, Options.CPUTarget);
      except
        on e: EParserError do
          If Options.StopOnParseError then
            Raise
          else
            WriteLn(StdErr, Format('%s(%d,%d): %s',
                    [e.Filename, e.Row, e.Column, e.Message]));
      end;
    WriterClass:=GetWriterClass(Options.Backend);
    Writer:=WriterClass.Create(Engine.Package,Engine);
    With Writer do
      Try
        If Options.BackendOptions.Count>0 then
          for I:=0 to ((Options.BackendOptions.Count-1) div 2) do
            begin
            Cmd:=Options.BackendOptions[I*2];
            Arg:=Options.BackendOptions[I*2+1];
            If not InterPretOption(Cmd,Arg) then
              WriteLn(StdErr, Format(SCmdLineInvalidOption,[Cmd+'='+Arg]));
            end;
        WriteDoc;
      Finally
        Free;
      end;
    if Length(FPackage.ContentFile) > 0 then
      Engine.WriteContentFile(FPackage.ContentFile);
  finally
    FreeAndNil(Engine);
  end;
end;


Procedure TFPDocAplication.DoRun;

begin
{$IFDEF Unix}
  gettext.TranslateResourceStrings('/usr/local/share/locale/%s/LC_MESSAGES/fpdoc.mo');
{$ELSE}
  gettext.TranslateResourceStrings('intl/fpdoc.%s.mo');
{$ENDIF}
  WriteLn(STitle);
  WriteLn(Format(SVersion, [DefFPCVersion, DefFPCDate]));
  WriteLn(SCopyright);
  WriteLn;
  ParseCommandLine;
  CreateDocumentation(FPackage,FProject.Options);
  WriteLn(SDone);
  Terminate;
end;

constructor TFPDocAplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException:=true;
  FProject:=TFPDOCproject.Create(Nil);
  FProject.Options.StopOnParseError:=False;
  FProject.Options.CPUTarget:=DefCPUTarget;
  FProject.Options.OSTarget:=DefOSTarget;
end;

begin
  With TFPDocAplication.Create(Nil) do
    try
      Run;
    finally
      Free;
    end;
end.
