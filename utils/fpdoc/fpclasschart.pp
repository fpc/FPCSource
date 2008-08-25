{

    FPClass chart -  Free Pascal class chart generation tool
    Copyright (c) 2008 - Michael Van Canneyt, michael@freepascal.org

    * Free Pascal class chart generation tool

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode objfpc}
{$h+}

program fpclasschart;

uses
  SysUtils, Classes, Typinfo, Gettext, dom, xmlread,
  dGlobals, PasTree, PParser,PScanner, xmlwrite;

resourcestring
  STitle = 'fpClassTree - Create class tree from pascal sources';
  SVersion = 'Version %s [%s]';
  SCopyright = '(c) 2008 - Michael Van Canneyt, michael@freepascal.org';
  SCmdLineHelp = 'See documentation for usage.';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SDone = 'Done.';
  SSkipMerge = 'Cannot merge %s into %s tree.';
  SErrNoSuchMergeFile = 'Merge file %s does not exist.';
  SMergedFile = 'Merged %d classes from file %s.';
  SClassesAdded = 'Added %d classes from %d files.';

Const
  RootNames : Array[TPasObjKind] of string
            = ('Objects', 'Classes', 'Interfaces');

type

  { TClassTreeEngine }

  TClassTreeEngine = class(TFPDocEngine)
  Private
    FClassTree : TXMLDocument;
    FTreeStart : TDomElement;
    FObjects : TStringList;
    FObjectKind : TPasObjKind;
    FParentObject : TPasClassType;
    function LookForElement(PE: TDomElement; AElement: TPasElement): TDomNode;
    function NodeMatch(N: TDomNode; AElement: TPasElement): Boolean;
    Function AddToClassTree(AElement : TPasElement; Var ACount : Integer) : TDomElement;
  public
    Constructor Create(AClassTree : TXMLDocument; AObjectKind : TPasObjKind);
    Destructor Destroy; override;
    Function BuildTree : Integer;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
  end;

const
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};
  FPCVersion: String = {$I %FPCVERSION%};
  FPCDate: String = {$I %FPCDATE%};

  

function TClassTreeEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;

Var
  DN : TDocNode;

begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility:=AVisibility;
  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);
  If AClass.InheritsFrom(TPasClassType) then
    FObjects.AddObject(AName,Result);
end;

Constructor TClassTreeEngine.Create(AClassTree : TXMLDocument; AObjectKind : TPasObjKind);

Var
  N : TDomNode;

begin
  FClassTree:=AClassTree;
  FTreeStart:=FClassTree.DocumentElement;
  FPackage:=TPasPackage.Create('dummy',Nil);
  FObjectKind:=AObjectKind;
  FObjects:=TStringList.Create;
  Case FObjectkind of
    okObject    : FParentObject:=TPasClassType.Create('TObject',FPackage);
    okClass     : FParentObject:=TPasClassType.Create('TObject',FPackage);
    okInterface : FParentObject:=TPasClassType.Create('IInterface',FPackage);
  end;
  FParentObject.ObjKind:=FObjectKind;
  Inherited Create;
end;

destructor TClassTreeEngine.Destroy;
begin
  FreeAndNil(FObjects);
  inherited Destroy;
end;

Function TClassTreeEngine.BuildTree : Integer;

Var
  I : Integer;
  PC : TPasClassType;

begin
  Result:=0;
  FObjects.Sorted:=True;
  For I:=0 to FObjects.Count-1 do
    begin
    PC:=TPasClassType(FObjects.Objects[i]);
    If (PC.ObjKind=FObjectKind) and Not PC.IsForward then
      AddToClassTree(PC as TPasElement,Result)
    end;
end;

Function TClassTreeEngine.NodeMatch(N : TDomNode; AElement : TPasElement) : Boolean;

begin
  Result:=(N.NodeType=ELEMENT_NODE) and (CompareText(N.NodeName,AElement.Name)=0)
end;

Function TClassTreeEngine.LookForElement(PE : TDomElement; AElement : TPasElement) : TDomNode;

Var
  N : TDomNode;

begin
  Result:=PE.FirstChild;
  While (Result<>Nil) and Not NodeMatch(Result,AElement) do
    Result:=Result.NextSibling;
  If (Result=Nil) then
    begin
    N:=PE.FirstChild;
    While (Result=Nil) and (N<>Nil) do
      begin
      if (N.NodeType=ELEMENT_NODE) then
        begin
        Result:=LookForElement(N as TDomElement,AElement);
        end;
      N:=N.NextSibling;
      end;
    end
end;

Function TClassTreeEngine.AddToClassTree(AElement : TPasElement; Var ACount : Integer) : TDomElement;

Var
  PC : TPasClassType;
  PE : TDomElement;
  M : TPasModule;
  N : TDomNode;

begin
  PE:=Nil;
  If (AElement is TPasClassType) then
    begin
    PC:=AElement as TPasClassType;
    If not Assigned(PC.AncestorType) and (CompareText(PC.Name,FParentObject.Name)<>0) then
      PC.AncestorType:=FParentObject;
    If Assigned(PC.AncestorType) then
      PE:=AddToClassTree(PC.AncestorType,ACount);
    end;
  If (PE=Nil) then
    PE:=FTreeStart;
  N:=LookForElement(PE,AElement);
  If (N<>Nil) then
    Result:=N as TDomElement
  else
    begin
    Inc(ACount);
    Result:=FClassTree.CreateElement(AElement.Name);
    If Not (AElement is TPasUnresolvedTypeRef) then
      begin
      M:=AElement.GetModule;
      if Assigned(M) then
        Result['unit']:=M.Name;
      end;
    PE.AppendChild(Result);
    end;
end;    
    
{ ---------------------------------------------------------------------
  Main program. Document all units.    
  ---------------------------------------------------------------------}

Function MergeNodes(Doc : TXMLDocument;Dest,Source : TDomElement) : Integer;

Var
  N : TDomNode;
  S,E : TDomElement;


begin
  N:=Source.FirstChild;
  While (N<>Nil) do
    begin
    if (N.NodeType=ELEMENT_NODE) then
      begin
      S:=N as TDomElement;
      E:=Dest.FindNode(N.NodeName) as TDomElement;
      If (E=Nil) then
        begin
        E:=Doc.CreateElement(N.NodeName);
        If S['unit']<>'' then
          E['Unit']:=S['unit'];
        Dest.AppendChild(E);
        Inc(Result);
        end;
      Result:=Result+MergeNodes(Doc,E,S);
      end;
    N:=N.NextSibling;
    end;
end;

Function MergeTrees (Dest,Source : TXMLDocument) : Integer;

Var
  S,D : TDomElement;
  Count : Integer;

begin
  Result:=0;
  D:=Dest.DocumentElement;
  S:=Source.DocumentElement;
  If (S.NodeName=D.NodeName) then
    Result:=MergeNodes(Dest,D,S)
  else
    Writeln(StdErr,Format(SSkipMerge,[S.NodeName,D.NodeName]));
end;
  
Function AnalyseFiles(Const AOutputName : String; InputFiles,MergeFiles : TStrings; AObjectKind : TPasObjKind) : String;


Var
  XML,XML2 : TXMLDocument;
  I,ACount : Integer;
  Engine: TClassTreeEngine;

begin
  XML:=TXMLDocument.Create;
  Try
    //XML.
    XML.AppendChild(XML.CreateElement(RootNames[AObjectKind]));
    For I:=0 to MergeFiles.Count-1 do
      begin
      XMl2:=TXMLDocument.Create;
      ReadXMLFile(XML2,MergeFiles[i]);
      try
        ACount:=MergeTrees(XML,XML2);
        WriteLn(StdErr,Format(SMergedFile,[ACount,MergeFiles[i]]));
      Finally
        FreeAndNil(XML2);
      end;
      end;
    ACount:=0;
    For I:=0 to InputFiles.Count-1 do
      begin
      Engine := TClassTreeEngine.Create(XML,AObjectKind);
      Try
        ParseSource(Engine,InputFiles[I],OSTarget,CPUTarget);
        ACount:=ACount+Engine.BuildTree;
      Finally
        Engine.Free;
      end;
      end;
    WriteXMlFile(XML,AOutputName);
    Writeln(StdErr,Format(SClassesAdded,[ACount,InputFiles.Count]));
  Finally
    XML.Free;
  end;
end;

{ ---------------------------------------------------------------------
    Option management
  ---------------------------------------------------------------------}


var  
  cmdObjectKind : TPasObjKind;
  InputFiles, 
  MergeFiles : TStringList;
  DocLang : String;
  PackageName, 
  OutputName: String;

procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  MergeFiles := TStringList.Create;
end;

procedure FreeOptions;

begin
  MergeFiles.Free;
  InputFiles.Free;
end;

{ ---------------------------------------------------------------------
  Usage  
  ---------------------------------------------------------------------}
  
Procedure Usage;

begin
  Writeln('Usage : ',ExtractFileName(Paramstr(0)),' [options]');
  Writeln('Where [options] is one or more of :');
  Writeln(' --merge=filename    Filename with object tree to merge.');
  Writeln(' --help              Emit help.');
  Writeln(' --input=cmdline     Input file to create skeleton for.');
  Writeln('                     Use options are as for compiler.');
  Writeln(' --kind=objectkind   Specify object kind. One of object, class, interface.');
  Writeln(' --lang=language     Use selected language.');
  Writeln(' --output=filename   Send output to file.');
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
  cmdObjectKind:=okClass;
  if (s = '-h') or (s = '--help') then
    begin
    Usage;
    Halt(0);
    end;
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
  else if (Cmd = '-k') or (Cmd = '--kind') then
    cmdObjectKind:=TPasObjKind(GetEnumValue(TypeInfo(TPasObjKind),'ok'+Arg))
  else if Cmd = '--merge' then
    begin
    if FileExists(Arg) then
      MergeFiles.Add(Arg)
    else
      Writeln(StdErr,Format(SErrNoSuchMergeFile,[arg]));
    end
  else
    begin
    WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]));
    Usage;
    Halt(1);
    end;
end;

Function ParseCommandLine : Integer;

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
  Result:=0;
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


{ ---------------------------------------------------------------------
  Main Program  
  ---------------------------------------------------------------------}
  
Procedure Run;
  
var
  E: Integer;

begin
  WriteLn(STitle);
  WriteLn(Format(SVersion, [FPCVersion, FPCDate]));
  WriteLn(SCopyright);
  InitOptions;
  Try
    E:=ParseCommandLine;
    If E<>0 then
      Halt(E);
    WriteLn;
    AnalyseFiles(OutputName,InputFiles,MergeFiles,cmdObjectKind);
    WriteLn(StdErr,SDone);
  Finally  
    FreeOptions;
  end;  
end;

Begin
  Run;  
end.

