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
  SysUtils, Classes, Gettext, DOM, XMLWrite, PasTree, PParser,
  dGlobals,  // GLobal definitions, constants.
  dwriter,   // TFPDocWriter definition.
  dwlinear,  // Linear (abstract) writer
  dw_LaTeX,  // TLaTex writer
  dw_XML,    // XML writer
  dw_HTML,   // HTML writer
  dw_ipf,    // IPF writer
  dw_man,    // Man page writer
  dw_linrtf, // lineair RTF writer
  dw_txt;    // TXT writer

const
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};
  FPCVersion: String = {$I %FPCVERSION%};
  FPCDate: String = {$I %FPCDATE%};

var
  Backend : String;
  BackendOptions : TStrings;
  InputFiles, DescrFiles: TStringList;
  PackageName, DocLang, ContentFile : String;
  Engine: TFPDocEngine;
  StopOnParserError : Boolean;

Procedure Usage(AnExitCode : Byte);

Var
  I,P : Integer;
  S : String;
  L : TStringList;
  C : TFPDocWriterClass;

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
  L:=TStringList.Create;
  Try
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
      Writeln(Format(SUsageFormatSpecific,[Lowercase(Backend)]));
      C:=GetWriterClass(backend);
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

procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  DescrFiles := TStringList.Create;
  BackendOptions := TStringList.Create;
  Engine := TFPDocEngine.Create;
  StopOnParserError:=False;
end;

procedure FreeOptions;
begin
  Engine.Free;
  BackendOptions.Free;
  DescrFiles.Free;
  InputFiles.Free;
end;

procedure ReadContentFile(const AParams: String);
var
  i: Integer;
begin
  i := Pos(',', AParams);
  Engine.ReadContentFile(Copy(AParams, 1, i - 1),
    Copy(AParams, i + 1, Length(AParams)));
end;

procedure ParseOption(const s: String);

  procedure AddToFileList(List: TStringList; const FileName: String);
  var
    f: Text;
    s: String;
  begin
    if Copy(FileName, 1, 1) = '@' then
    begin
      Assign(f, Copy(FileName, 2, Length(FileName)));
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
    Engine.HideProtected := True
  else if s = '--warn-no-node' then
    Engine.WarnNoNode := True
  else if s = '--show-private' then
    Engine.HidePrivate := False
  else if s = '--stop-on-parser-error' then
    StopOnParserError := True
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
    if Cmd = '--descr' then
      AddToFileList(DescrFiles, Arg)
    else if (Cmd = '-f') or (Cmd = '--format') then
      begin
      Arg:=UpperCase(Arg);
      If FindWriterClass(Arg)=-1 then
        WriteLn(StdErr, Format(SCmdLineInvalidFormat, [Arg]))
      else
        BackEnd:=Arg;
      end
    else if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(InputFiles, Arg)
    else if (Cmd = '-o') or (Cmd = '--output') then
      Engine.Output := Arg
    else if Cmd = '--content' then
      ContentFile := Arg
    else if Cmd = '--import' then
      ReadContentFile(Arg)
    else if Cmd = '--package' then
      PackageName := Arg
    else if Cmd = '--ostarget' then
      OSTarget := Arg
    else if Cmd = '--cputarget' then
      CPUTarget := Arg
    else
      begin
      BackendOptions.Add(Cmd);
      BackendOptions.Add(Arg);
      end;
    end;
end;

procedure ParseCommandLine;

var
  i: Integer;

begin
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
  If (BackEnd='') then
    BackEnd:='html';
  if (PackageName='') then
    begin
    Writeln(SNeedPackageName);
    Usage(1);
    end;
end;

procedure CreateDocumentation;

var
  i: Integer;
  WriterClass : TFPDocWriterClass;
  Writer : TFPDocWriter;

begin
  for i := 0 to DescrFiles.Count - 1 do
    Engine.AddDocFile(DescrFiles[i]);
  Engine.SetPackageName(PackageName);
  if Length(DocLang) > 0 then
    TranslateDocStrings(DocLang);
  for i := 0 to InputFiles.Count - 1 do
    try
      ParseSource(Engine, InputFiles[i], OSTarget, CPUTarget);
    except
      on e: EParserError do
        If StopOnParserError then
          Raise
        else 
          WriteLn(StdErr, Format('%s(%d,%d): %s',
                  [e.Filename, e.Row, e.Column, e.Message]));
    end;
  WriterClass:=GetWriterClass(Backend);
  Writer:=WriterClass.Create(Engine.Package,Engine);
  With Writer do
    Try
      If BackendOptions.Count>0 then
        for I:=0 to ((BackendOptions.Count-1) div 2) do
          If not InterPretOption(BackendOptions[I*2],BackendOptions[I*2+1]) then
            WriteLn(StdErr, Format(SCmdLineInvalidOption,[BackendOptions[I*2]+' '+BackendOptions[I*2+1]]));
      WriteDoc;
    Finally
      Free;
    end;
  if Length(ContentFile) > 0 then
    Engine.WriteContentFile(ContentFile);
end;



begin
{$IFDEF Unix}
  gettext.TranslateResourceStrings('/usr/local/share/locale/%s/LC_MESSAGES/fpdoc.mo');
{$ELSE}
  gettext.TranslateResourceStrings('intl/fpdoc.%s.mo');
{$ENDIF}
  WriteLn(STitle);
  WriteLn(Format(SVersion, [FPCVersion, FPCDate]));
  WriteLn(SCopyright);
  WriteLn;
  InitOptions;
  Try
    ParseCommandLine;
    CreateDocumentation;
    WriteLn(SDone);
  Finally
    FreeOptions;
  end;
end.
