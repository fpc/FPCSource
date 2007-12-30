{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Skeleton XML description file generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program MakeSkel;

uses
  SysUtils, Classes, Gettext,
  dGlobals, PasTree, PParser,PScanner;

resourcestring
  STitle = 'MakeSkel - FPDoc skeleton XML description file generator';
  SVersion = 'Version %s [%s]';
  SCopyright = '(c) 2000 - 2003 Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org';
  SCmdLineHelp = 'See documentation for usage.';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SNoPackageNameProvided = 'Please specify a package name with --package=<name>';
  SOutputMustNotBeDescr = 'Output file must be different from description filenames.';
  SCreatingNewNode = 'Creating documentation for new node : %s';
  SNodeNotReferenced = 'Documentation node "%s" no longer used';
  SDone = 'Done.';

type
  TCmdLineAction = (actionHelp, actionConvert);

  TSkelEngine = class(TFPDocEngine)
    FModules : TStringList;
    Procedure  DoWriteUnReferencedNodes(N : TDocNode; NodePath : String);
  public
    Destructor Destroy; override;
    function FindModule(const AName: String): TPasModule; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
    procedure WriteUnReferencedNodes;
  end;

const
  CmdLineAction: TCmdLineAction = actionConvert;
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};
  FPCVersion: String = {$I %FPCVERSION%};
  FPCDate: String = {$I %FPCDATE%};

var
  EmittedList, InputFiles, DescrFiles: TStringList;
  DocLang: String;
  Engine: TSkelEngine;
  UpdateMode,
  DisableErrors,
  DisableSeealso,
  DisableArguments,
  DisableProtected,
  DisablePrivate,
  DisableFunctionResults: Boolean;

  EmitClassSeparator: Boolean;
  PackageName, OutputName: String;
  f: Text;

function TSkelEngine.FindModule(const AName: String): TPasModule; 

Var
  I : Integer;

begin
  Result:=Inherited FindModule(AName);
  If (Result=Nil) then
    begin // Create dummy list and search in that.
    If (FModules=Nil) then
      begin
      FModules:=TStringList.Create;
      FModules.Sorted:=True;
      end;
    I:=FModules.IndexOf(AName);
    IF (I=-1) then
      begin
      Result:=TPasModule.Create(AName,Nil);
      FModules.AddObject(AName,Result);
      end
    else
      Result:=FModules.Objects[i] as TPasModule;  
    end;  
end;

Destructor TSkelEngine.Destroy; 

Var
  I : Integer;

begin
  If Assigned(FModules) then 
    begin
    For I:=0 to FModules.Count-1 do
      FModules.Objects[i].Free;
    FreeAndNil(FModules);    
    end;
end;


function TSkelEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

  Function WriteThisNode(APasElement : TPasElement; DocNode : TDocNode)  : Boolean;

  Var
    ParentVisible:Boolean;
    PT,PP : TPasElement;
  begin
    ParentVisible:=True;
    If (APasElement is TPasArgument) or (APasElement is TPasResultElement) then
      begin
      PT:=AParent;
      // Skip ProcedureType or PasFunctionType
      If (PT<>Nil) then
        begin
        if (PT is TPasProcedureType) or (PT is TPasFunctionType) then
          PT:=PT.Parent;
        If (PT<>Nil) and ((PT is TPasProcedure) or (PT is TPasProcedure))   then
          PP:=PT.Parent
        else
          PP:=Nil;
        If (PP<>Nil) and (PP is TPasClassType) then
          begin
          ParentVisible:=((not DisablePrivate or (PT.Visibility<>visPrivate)) and
                         (not DisableProtected or (PT.Visibility<>visProtected)));
          end;
        end;
      end;
    Result:=Assigned(AParent) and (Length(AName) > 0) and
            (ParentVisible and (not DisableArguments or (APasElement.ClassType <> TPasArgument))) and
            (ParentVisible and (not DisableFunctionResults or (APasElement.ClassType <> TPasResultElement))) and
            (not DisablePrivate or (AVisibility<>visPrivate)) and
            (not DisableProtected or (AVisibility<>visProtected)) and
            (Not Assigned(EmittedList) or (EmittedList.IndexOf(APasElement.FullName)=-1));
    If Result and updateMode then
      begin
      Result:=DocNode=Nil;
      If Result then
        Writeln(stderr,Format(ScreatingNewNode,[APasElement.PathName]));
      end;
  end;

  Function WriteOnlyShort(APasElement : TPasElement) : Boolean;

  begin
    Result:=(APasElement.ClassType=TPasArgument) or
            (APasElement.ClassType=TPasResultElement) or
            (APasElement.ClassType=TPasEnumValue);
  end;

  Function IsTypeVarConst(APasElement : TPasElement) : Boolean;

  begin
    With APasElement do
      Result:=(InheritsFrom(TPasType) and not InheritsFrom(TPasClassType)) or
              (InheritsFrom(TPasResString)) or
              (InheritsFrom(TPasVariable));

  end;

Var
  DN : TDocNode;

begin
  Result := AClass.Create(AName, AParent);
  If UpdateMode then
    begin
    DN:=FindDocNode(Result);    
    If Assigned(DN) then
      DN.IncRefCount;
    end
  else
    DN:=Nil;  
  Result.Visibility:=AVisibility;
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);
  if Result.ClassType = TPasModule then
    begin
    WriteLn(f);
    WriteLn(f, '<!--');
    WriteLn(f, '  ====================================================================');
    WriteLn(f, '    ', Result.Name);
    WriteLn(f, '  ====================================================================');
    WriteLn(f, '-->');
    WriteLn(f);
    WriteLn(f, '<module name="', Result.Name, '">');
    if not UpdateMode then
      begin
      WriteLn(f, '<short></short>');
      WriteLn(f, '<descr>');
      WriteLn(f, '</descr>');
      end;
    end
  else if WriteThisNode(Result,DN) then
    begin
    EmittedList.Add(Result.FullName); // So we don't emit again.
    WriteLn(f);
    if EmitClassSeparator and (Result.ClassType = TPasClassType) then
      begin
      WriteLn(f, '<!--');
      WriteLn(f, '  ********************************************************************');
      WriteLn(f, '    ', Result.PathName);
      WriteLn(f, '  ********************************************************************');
      WriteLn(f, '-->');
      WriteLn(f);
      end;
    Writeln(F,'<!-- ', Result.ElementTypeName,' Visibility: ',VisibilityNames[AVisibility], ' -->');
    WriteLn(f,'<element name="', Result.FullName, '">');
    WriteLn(f, '<short></short>');
    if Not WriteOnlyShort(Result) then
      begin
      WriteLn(f, '<descr>');
      WriteLn(f, '</descr>');
      if not (DisableErrors or IsTypeVarConst(Result)) then
        begin
        WriteLn(f, '<errors>');
        WriteLn(f, '</errors>');
        end;
      if not DisableSeealso then
        begin
        WriteLn(f, '<seealso>');
        WriteLn(f, '</seealso>');
        end;
      end;
    WriteLn(f, '</element>');
    end;
end;

Procedure  TSkelEngine.DoWriteUnReferencedNodes(N : TDocNode; NodePath : String);

begin
  If (N<>Nil) then
    begin
    If (NodePath<>'') then
      NodePath:=NodePath+'.';
    DoWriteUnReferencedNodes(N.FirstChild,NodePath+N.Name);
    While (N<>Nil) do
      begin
      if (N.RefCount=0) and (N.Node<>Nil) and (Not N.TopicNode) then
        Writeln(stderr,Format(SNodeNotReferenced,[NodePath+N.Name]));
      N:=N.NextSibling;
      end;
    end;
end;

procedure TSkelEngine.WriteUnReferencedNodes;

begin
  DoWriteUnReferencedNodes(RootDocNode,'');
end;

procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  DescrFiles := TStringList.Create;
  EmittedList:=TStringList.Create;
  EmittedList.Sorted:=True;
end;

procedure FreeOptions;
begin
  DescrFiles.Free;
  InputFiles.Free;
  EmittedList.Free;
end;

Procedure Usage;

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [options]');
  Writeln('Where [options] is one or more of :');
  Writeln(' --descr=filename    Filename for update.');
  Writeln(' --disable-arguments Do not create nodes for function arguments.');
  Writeln(' --disable-errors    Do not create errors node.');
  Writeln(' --disable-function-results');
  Writeln('                     Do not create nodes for function arguments.');
  Writeln(' --disable-private   Do not create nodes for class private fields.');
  Writeln(' --disable-protected Do not create nodes for class protected fields.');
  Writeln(' --disable-seealso   Do not create seealso node.');
  Writeln(' --emit-class-separator');
  Writeln('                     Emit descriptive comment between classes.');
  Writeln(' --help              Emit help.');
  Writeln(' --input=cmdline     Input file to create skeleton for.');
  Writeln('                     Use options are as for compiler.');
  Writeln(' --lang=language     Use selected language.');
  Writeln(' --output=filename   Send output to file.');
  Writeln(' --package=name      Specify package name (mandatory).');
  Writeln(' --update            Update mode. Output only missing nodes.');
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
    CmdLineAction := actionHelp
  else if s = '--update' then
    UpdateMode := True
  else if s = '--disable-arguments' then
    DisableArguments := True
  else if s = '--disable-errors' then
    DisableErrors := True
  else if s = '--disable-function-results' then
    DisableFunctionResults := True
  else if s = '--disable-seealso' then
    DisableSeealso := True
  else if s = '--disable-private' then
    DisablePrivate := True
  else if s = '--disable-protected' then
    begin
    DisableProtected := True;
    DisablePrivate :=True;
    end
  else if (s = '--emitclassseparator') or (s='--emit-class-separator') then
    EmitClassSeparator := True
  else
  begin
    i := Pos('=', s);
    if i > 0 then
    begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
    end else
    begin
      Cmd := s;
      SetLength(Arg, 0);
    end;
    if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(InputFiles, Arg)
    else if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '-o') or (Cmd = '--output') then
      OutputName := Arg
    else if Cmd = '--package' then
      PackageName := Arg
    else if Cmd = '--descr' then
      begin
      if FileExists(Arg) then
        DescrFiles.Add(Arg);
      end
    else
      WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]));
  end;
end;

procedure ParseCommandLine;

Const
{$IFDEF Unix}
  MoFileTemplate = '/usr/local/share/locale/%s/LC_MESSAGES/makeskel.mo';
{$ELSE}
  MoFileTemplate ='intl/makeskel.%s.mo';
{$ENDIF}

var
  MOFilename: string;
  i: Integer;
begin
  DocLang:='';
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
  If (DocLang<>'') then
    begin
    MOFilename:=Format(MOFileTemplate,[DocLang]);
    if FileExists(MOFilename) then
      gettext.TranslateResourceStrings(MoFileName)
    else
      writeln('NOTE: unable to find tranlation file ',MOFilename);
    // Translate internal documentation strings
    TranslateDocStrings(DocLang);
    end;
end;



var
  i,j: Integer;
  Module: TPasModule;
  N : TDocNode;

begin
  InitOptions;
  ParseCommandLine;
  WriteLn(STitle);
  WriteLn(Format(SVersion, [FPCVersion, FPCDate]));
  WriteLn(SCopyright);
  WriteLn;
  if CmdLineAction = actionHelp then
    Usage
  else
    begin
    // Action is to create the XML skeleton

    if Length(PackageName) = 0 then
      begin
      WriteLn(SNoPackageNameProvided);
      Halt(2);
      end;

    if DescrFiles.IndexOf(OutputName)<>-1 then
      begin
      Writeln(SOutputMustNotBeDescr);
      Halt(3)
      end;

    Assign(f, OutputName);
    Rewrite(f);

    WriteLn(f, '<?xml version="1.0" encoding="ISO-8859-1"?>');
    WriteLn(f, '<fpdoc-descriptions>');
    WriteLn(f, '<package name="', PackageName, '">');

    // Process all source files
    for i := 0 to InputFiles.Count - 1 do
    begin
      Engine := TSkelEngine.Create;
      try
       try
         Engine.SetPackageName(PackageName);
         if UpdateMode then
           For J:=0 to DescrFiles.Count-1 do
             Engine.AddDocFile(DescrFiles[J]);
         Module := ParseSource(Engine, InputFiles[i], OSTarget, CPUTarget);
         If UpdateMode then
           begin
           N:=Engine.FindDocNode(Module);
           If Assigned(N) then
             N.IncRefCount;
           end;
         WriteLn(f, '');
         WriteLn(f, '</module> <!-- ', Module.Name, ' -->');
         WriteLn(f, '');
       except
         on e:EFileNotFoundError do
           begin
             Writeln(StdErr,' file ', e.message, ' not found');
             close(f);
             Halt(1);
           end;
         on e:EParserError do
           begin
             Writeln(StdErr,'', e.filename,'(',e.row,',',e.column,') Fatal: ',e.message);
             close(f);
             Halt(1);
           end;
       end;
        If UpdateMode then
          Engine.WriteUnReferencedNodes;
      finally
        Engine.Free;
       end;
    end;

    WriteLn(f, '</package>');
    WriteLn(f, '</fpdoc-descriptions>');

    Close(f);
    WriteLn(SDone);
    end;

  FreeOptions;

end.
