unit prefixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PScanner, PParser, PasTree, strutils, regexpr;

Type
  { We have to override abstract TPasTreeContainer methods.
    See utils/fpdoc/dglobals.pp for an implementation of TFPDocEngine,
    a "real" engine. }
  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  { TPrefixer }

  TFileMode = (
    fmReplace,           // new file replaces old completely.
    fmReplaceNamespace,  // new file replaces old completely. Namespaces are used for uses clause.
    fmInclude,           // new file includes old, units in uses clause replaced with new names
    fmIncludeNamespace   // new file includes old. Namespaces are used for uses clause.
    );
  TIncludeNameMode = (
    inmIfdefElse, // Unit name in include file is set using {$IFDEF DEFINE} NEWNAME {$ELSE} OLDNAME {$ENDIF}
    inmIfndef     // Unit clause is included in main file, it is skipped in include file using {$IFNDEF DEFINE} unit OLDNAME; {$ENDIF}
  );

  TPrefixLog = Procedure (Sender : TObject; aType : TEventType; const aMsg : String) of object;

  TPrefixer = class(TComponent)
  private
    FCasedFileNames: Boolean;
    FCreateBackups: Boolean;
    FDefine: String;
    FDestFileName: String;
    FDestUnitName: String;
    FFileName: String;
    FKnownNameSpaces: TStrings;
    FNameMode: TIncludeNameMode;
    FNameSpace: String;
    FOnLog: TPrefixLog;
    FParams: TStrings;
    FSkipDestFileName: Boolean;
    FSources : TStrings;
    FDottedSources : TStrings;
    FNewUses : TStrings;
    FUnitFileMode: TFileMode;
    FFullFileName : String;
  Protected
    Procedure DoLog(aType : TEventType; Const aMsg : String);
    Procedure DoLog(aType : TEventType; Const aFmt : String; aArgs : array of const);
    procedure AddNameSpaces(Src: TStrings; aUses: TPasUsesClause);
    procedure CorrectUnitName(aName: String; aLineNr: Integer);
    procedure DoParseLog(Sender: TObject; const Msg: String);
    procedure GetAdditionalUnits(aUnitNames: TStrings; aSource: String);
    function GetDefine: String;
    function GetDestFileName: String;
    function GetDestUnitName: String;
    function MaybeBackup(const aFileName: string): Boolean;
    function ParseSource(AEngine: TPasTreeContainer;
      const FPCCommandLine: array of String; OSTarget, CPUTarget: String;
      Options: TParseSourceOptions): TPasModule;
    function ReplaceUnits(const aLine: string; aUnitNames : TStrings): String;
    function ReplaceWord(aLine, aName, aFull: String): String;
    function FindWord(aName,aLine : String): Boolean;
    procedure ReworkUses(aSection: TPasSection);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure PrintUses(aSection : TPasSection; aShowFileName : Boolean = false);
    procedure Execute;
    procedure ReworkUses(aUses,aNewUses : TStrings);
    class function ExtractPrefix(const aRule: String): String;
    class function ApplyRule(const aFile,aCasedName,aRule : String; PrettyPrint : Boolean) : String;
    class function ApplyAliasRule(const aName, aRule: String): String;
    // Create backups of created/changed files  ?
    Property CreateBackups : Boolean Read FCreateBackups Write FCreateBackups;
    // How to create the new file.
    Property UnitFileMode : TFileMode Read FUnitFileMode Write FUnitFileMode;
    // How to set the unit name in the case of an include file.
    Property IncludeUnitNameMode : TIncludeNameMode Read FNameMode Write FNameMode;
    // Define to use to protect dotted names. Default FPC_DOTTEDUNITS
    Property Define : String Read GetDefine Write FDefine;
    // Filename to process. For include modes, this file will be overwritten !
    Property FileName : String Read FFileName Write FFileName;
    // Do not write dotted Filename
    Property SkipDestFileName : Boolean Read FSkipDestFileName Write FSkipDestFileName;
    // Dotted Filename to produce (including path & extension). If not set, NameSpace.FileName is used.
    Property DestFileName : String Read GetDestFileName Write FDestFileName;
    // Filename to produce. If not set, DestFileName without extension is used.
    Property DestUnitName : String Read GetDestUnitName Write FDestUnitName;
    // Namespace to be used for this unit.
    Property NameSpace : String Read FNameSpace Write FNameSpace;
    // Namespaces for used units, in format UnitName=NameSpace
    Property KnownNameSpaces : TStrings Read FKnownNameSpaces;
    // Params needed to parse FileName
    Property Params : TStrings Read FParams;
    // if True, then the output files have the same case as the unit names.
    // If False, all filenames are lowercased.
    Property CasedFileNames : Boolean Read FCasedFileNames Write FCasedFileNames;
    // For messages
    Property OnLog : TPrefixLog Read FOnLog Write FOnLog;
  end;

implementation

function TSimpleEngine.CreateElement(AClass: TPTreeElement;
                                     const AName: String;
                                     AParent: TPasElement;
                                     AVisibility: TPasMemberVisibility;
                                     const ASourceFilename: String;
                                     ASourceLinenumber: Integer): TPasElement;
begin
  // Writeln(AName,' : ',AClass.ClassName,' at ',ASourceFilename,':',ASourceLinenumber);
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

constructor TPrefixer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FKnownNameSpaces:=TStringList.Create;
  FParams:=TStringList.Create;
  FSources:=TStringList.Create;
  FDottedSources:=TStringList.Create;
  FNewuses:=TStringList.Create;
  FUnitFileMode:=fmInclude;
  FNameMode:=inmIfndef;
end;

destructor TPrefixer.Destroy;
begin
  FreeAndNil(FKnownNameSpaces);
  FreeAndNil(FParams);
  FreeAndNil(FSources);
  FreeAndNil(FDottedSources);
  FreeAndNil(FNewuses);
  inherited Destroy;
end;

procedure TPrefixer.PrintUses(aSection: TPasSection; aShowFileName: Boolean);

Var
  aUses : TPasUsesUnit;
  aName : string;

begin
  if aSection=Nil then
    exit;
  for aUses in aSection.UsesClause do
    begin
    aName:='';
    if aShowFileName and assigned(aUses.InFileName) then
      aName:=AnsiDequotedStr(aUses.InFileName.Value,'''');
    if (aName='') and assigned(aUses.Expr) then
      aName:=aUses.Expr.GetDeclaration(False);
    if aName='' then
      aName:=aUses.Name;
    DoLog(etInfo,'%s, { location: %s:%d }',[aName,aUses.SourceFilename,aUses.SourceLinenumber]);
    end;
end;

function TPrefixer.ReplaceWord(aLine, aName, aFull: String): String;

var
  RE : TRegExpr;

begin
  RE:=TRegExpr.Create('\b'+aName+'\b');
  try
    Result:=RE.Replace(aLine,aFull);
    DoLog(etDebug, '%s: %s -> %s = %s',[aLine,aName,aFull,Result]);
  finally
    RE.Free;
  end;
end;

function TPrefixer.FindWord(aName, aLine: String): Boolean;
var
  RE : TRegExpr;

begin
  RE:=TRegExpr.Create('\b'+aName+'\b');
  try
    RE.ModifierI:=True;
    Result:=RE.Exec(aLine);
    DoLog(etDebug, '%s: %s = %s',[aLine,aName,BoolToStr(Result,'true','false')]);
  finally
    RE.Free;
  end;
end;

function TPrefixer.ReplaceUnits(const aLine: string; aUnitNames: TStrings): String;

Var
  res,aName,aFull,aNameSpace,aUnit : String;
  idx : Integer;

begin
  Res:=aLine;
  for aName in aUnitNames do
    begin
    Idx:=FKnownNameSpaces.IndexOfName(aName);
    if Idx<>-1 then
      begin
      FKnownNameSpaces.GetNameValue(Idx,aUnit,aNameSpace);
      if Copy(aNameSpace,1,1)='*' then
        aFull:=Copy(aNameSpace,2)
      else
        aFull:=aNameSpace+'.'+aUnit;
      Res:=ReplaceWord(Res,aName,aFull);
      end;
    end;
  Result:=Res;
end;


procedure TPrefixer.GetAdditionalUnits(aUnitNames : TStrings; aSource : String);

Var
  aRE : TRegExpr;
  aWords : TStringList;
  aWord : string;

begin
  awords:=nil;
  aRE:=TRegExpr.Create('(\w+)');
  Try
    aWords:=TstringList.Create;
    if aRe.Exec(aSource) then
      repeat
        aWord:=System.Copy(aSource, ARE.MatchPos[0], ARE.MatchLen[0]);
        if IndexText(aWord,['uses','define','undef','if','ifdef', 'endif','else'])=-1 then
          if (FKnownNameSpaces.IndexOfName(aWord)<>-1) then
            aUnitNames.Add(aWord); // Duplicates set to ignore
      until not Are.ExecNext;

  Finally
    aWords.Free;
    aRE.Free;
  end;
end;

procedure TPrefixer.ReworkUses(aSection: TPasSection);

Var
  aUses : TPasUsesUnit;
  S,aName,aLine,FNUses,FNMain : String;
  aUnitNames : TStringList;
  // all 0-based
  I,Idx, FUses,FUsesEnd, FFirst,FLast : Integer;


begin
  if (aSection=Nil)
      or (Length(aSection.UsesClause)=0)
      or ((Length(aSection.UsesClause)=1)
           and (SameText(aSection.UsesClause[0].Name,'System')))  then
    exit;
  FNMain:=ExtractFileName(FFileName);
  FFirst:=FSources.Count+1;
  FLast:=0;
  aUnitNames:=TStringList.Create;
  try
    aUnitNames.Sorted:=True;
    aUnitNames.Duplicates:=dupIgnore;
    for aUses in aSection.UsesClause do
      begin
      if aUses.SourceLinenumber-1>FLast then
        FLast:=aUses.SourceLinenumber-1;
      if aUses.SourceLinenumber-1<FFirst then
        FFirst:=aUses.SourceLinenumber-1;
      FNUses:=ExtractFileName(aUses.SourceFilename);
      aName:='';
      if (aName='') and assigned(aUses.Expr) then
        aName:=aUses.Expr.GetDeclaration(False);
      if aName='' then
        aName:=aUses.Name;
      if (FNUses<>FNMain) or (expandfilename(aUses.SourceFileName)<>FFullFileName) then
         Raise Exception.CreateFmt('Uses clause unit %s not in main unit: (uses: %s) <> %s',[aName,FNUses,FNMain]);
      aUnitNames.Add(aName);
      end;
    Fuses:=FFirst;
    if Fuses>=FSources.Count then
      FUses:=FSources.Count-1;
    While (FUses>=0) and (Pos('uses',lowerCase(FSources[FUses]))=0) do
      Dec(Fuses);
    FUsesEnd:=FLast; // Fuses is 0 bases
    While (FUsesEnd<FSources.Count) and (Pos(';',FSources[FUsesEnd])=0) do
      Inc(FusesEnd);
    DoLog(etDebug, 'Uses clause extends from %d: %s',[FUses,FSources[FUses]]);
    DoLog(etDebug, 'Uses clause extends to %d: %s',[FUsesEnd,FSources[FUsesEnd]]);
    S:='';
    For I:=Fuses to FUsesEnd do
      S:=S+#10+FSources[I];
    GetAdditionalUnits(aUnitNames,S);
    FNewuses.Clear;

    if UnitFileMode<>fmReplace then
      FNewuses.Add('{$IFDEF '+Define+'}');
    For Idx:=FUses to FUsesEnd do
      begin
      aLine:=FSources[Idx];
      If (Idx>=FFirst) and (Idx<=FLast) then
        begin
        aLine:=ReplaceUnits(aLine,aUnitNames);
        end;
      FNewUses.Add(aLine);
      end;
    // Check what we need to do with original sources
    if UnitFileMode<>fmReplace then
      begin
      FNewuses.Add('{$ELSE '+Define+'}');
      // Insert before uses
      FSources.Insert(FUsesEnd+1,'{$ENDIF '+Define+'}');
      end
    else
      begin
      // If we need to replace, we just remove all old lines before adding the new ones
      if UnitFileMode=fmReplace then
        For I:=FUsesEnd downto FUses do
          FSources.Delete(I);
      end;
    For I:=FNewUses.Count-1 downto 0 do
      FSources.Insert(FUses,FNewUses[i]);
  finally
    aUnitNames.Free;
  end;
end;

function TPrefixer.GetDefine: String;
begin
  Result:=FDefine;
  if Result='' then
    Result:='FPC_DOTTEDUNITS';
end;

function TPrefixer.MaybeBackup(const aFileName: string): Boolean;

Var
  BFN : String;
  FIn,Fout : TFileStream;

begin
  Result:=FileExists(aFileName);
  if Result then
     begin
     BFN:=aFileName+'.bak';
     Fout:=Nil;
     Fin:=TFileStream.Create(aFilename,fmOpenRead or fmShareDenyWrite);
     try
       Fout:=TFileStream.Create(BFN,fmCreate);
       Fout.CopyFrom(FIn,0);
     finally
       Fin.Free;
       Fout.Free;
     end;
     end;
end;

function TPrefixer.GetDestFileName: String;

Var
  DN, FN, NS : String;

begin
  Result:=FDestFileName;
  if Result='' then
    begin
    DN:=ExtractFilePath(FileName);
    FN:=ExtractFileName(FileName);
    NS:=NameSpace;
    if NS<>'' then
      NS:=NS+'.';
    if CasedFileNames then
      Result:=DN+NS+FN
    else
      Result:=DN+LowerCase(NS+FN);
    end;
end;

function TPrefixer.GetDestUnitName: String;

begin
  Result:=FDestUnitName;
  if Result='' then
    Result:=ChangeFileExt(ExtractFileName(DestFileName),'');
end;

procedure TPrefixer.CorrectUnitName(aName : String; aLineNr : Integer);

Var
  aLine,aReplace,aNewName : string;
  Idx : Integer;

begin
  aNewName:=DestUnitName;
  if (aNewName=aName) then
    exit; // nothing to do.
  case IncludeUnitNameMode of
  inmIfdefElse:
    begin
    aLine:=FSources[aLineNr];
    aReplace:='{$IFDEF '+Define+'} '+aNewName+' {$ELSE} '+aName+' {$ENDIF}';
    aLine:=ReplaceWord(aLine,aName,aReplace);
    end;
  inmIfndef:
    begin
    // Look for ;
    idx:=aLineNr-1;
    While (Idx<FSources.Count) and (Pos(';',FSources[Idx])=0) do
      Inc(Idx);
    if (Idx<FSources.Count-1) then
      FSources.Insert(Idx+1,'{$ENDIF '+DEFINE+'}');
    // Look for unit
    idx:=aLineNr;
    if Idx>=FSources.Count then
      Idx:=FSources.Count-1;
    While (Idx>=0) and Not FindWord('unit',FSources[Idx]) do
      Dec(Idx);
    if Idx>=0 then
      FSources.Insert(Idx,'{$IFNDEF '+DEFINE+'}');
    end;
  end;
end;

procedure TPrefixer.DoParseLog(Sender: TObject; const Msg: String);
begin
  DoLog(etDebug,Msg);
end;

procedure TPrefixer.DoLog(aType: TEventType; const aMsg: String);
begin
  if assigned(FOnLog) then
    FOnLog(Self,aType,aMsg);
end;

procedure TPrefixer.DoLog(aType: TEventType; const aFmt: String;
  aArgs: array of const);
begin
  DoLog(aType,Format(aFmt,aArgs));
end;

procedure TPrefixer.AddNameSpaces(Src : TStrings; aUses : TPasUsesClause);

Var
  aUsed : TPasUsesUnit;
  aDirective,aName,aNameSpace,aUnit : String;
  idx : Integer;

begin
  for aUsed in aUses do
    begin
    aName:='';
    if assigned(aUsed.Expr) then
      aName:=aUsed.Expr.GetDeclaration(False);
    if aName='' then
      aName:=aUsed.Name;
    Idx:=FKnownNameSpaces.IndexOfName(aName);
    if Idx<>-1 then
      begin
      FKnownNameSpaces.GetNameValue(Idx,aUnit,aNameSpace);
      aDirective:='{$NAMESPACE '+aNameSpace+'}';
      if Src.IndexOf(aDirective)=-1 then
        Src.Insert(0,aDirective);
      end;
    end;
end;

function TPrefixer.ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine: array of String; OSTarget, CPUTarget: String;
  Options: TParseSourceOptions): TPasModule;

var
  FileResolver: TBaseFileResolver;
  Parser: TPasParser;
  lFilename: String;
  Scanner: TPascalScanner;
  allowmem : Boolean;

  procedure ProcessCmdLinePart(S : String);
  var
    l,Len: Integer;

  begin
    if (S='') then
      exit;
    Len:=Length(S);
    if (s[1] = '-') and (len>1) then
    begin
      case s[2] of
        'd': // -d define
          begin
          s:=Copy(s, 3, Len);
          Scanner.AddDefine(UpperCase(S));
          if s='allowmem' then
            AllowMem:=True;
          end;
        'u': // -u undefine
          Scanner.RemoveDefine(UpperCase(Copy(s, 3, Len)));
        'F': // -F
          if (len>2) and (s[3] = 'i') then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Len));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Len));
        'S': // -S mode
          if  (len>2) then
            begin
            l:=3;
            While L<=Len do
              begin
              case S[l] of
                'c' : Scanner.Options:=Scanner.Options+[po_cassignments];
                'd' : Scanner.SetCompilerMode('DELPHI');
                '2' : Scanner.SetCompilerMode('OBJFPC');
                'h' : ; // do nothing
              end;
              inc(l);
              end;
            end;
        'M' :
           begin
           delete(S,1,2);
           l:=pos(':',S);
           if (L<>0) and (UpperCase(Copy(S,1,l-1))='MODESWITCH') then
             begin
             Delete(S,1,l);
             if SameText(S,'externalclass') then
               Scanner.ReadOnlyModeSwitches:=Scanner.ReadOnlyModeSwitches+[msExternalClass];
             Scanner.SetModeSwitch(S);
             end
           else
             Scanner.SetCompilerMode(S);
           end;
      end;
    end else
      if lFilename <> '' then
        raise ENotSupportedException.Create(SErrMultipleSourceFiles)
      else
        lFilename := s;
  end;

var
  S: String;
  opts : TPOptions;

begin
  AllowMem:=False;
  if DefaultFileResolverClass=Nil then
    raise ENotImplemented.Create(SErrFileSystemNotSupported);
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  try
    FileResolver := DefaultFileResolverClass.Create;
    if FileResolver is TFileResolver then
      TFileResolver(FileResolver).UseStreams:=poUseStreams in Options;
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.LogEvents:=AEngine.ScannerLogEvents;
    Scanner.OnLog:=AEngine.Onlog;
    if not (poSkipDefaultDefs in Options) then
      begin
      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');
      Scanner.AddDefine('FPC_LITTLE_ENDIAN');
      // TargetOS
      s := UpperCase(OSTarget);
      Scanner.AddDefine(s);
      Case s of
        'LINUX' : Scanner.AddDefine('UNIX');
        'FREEBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'NETBSD' :
          begin
          Scanner.AddDefine('BSD');
          Scanner.AddDefine('UNIX');
          end;
        'SUNOS' :
          begin
          Scanner.AddDefine('SOLARIS');
          Scanner.AddDefine('UNIX');
          end;
        'GO32V2' : Scanner.AddDefine('DPMI');
        'BEOS' : Scanner.AddDefine('UNIX');
        'QNX' : Scanner.AddDefine('UNIX');
        'AROS' : Scanner.AddDefine('HASAMIGA');
        'MORPHOS' : Scanner.AddDefine('HASAMIGA');
        'AMIGA' : Scanner.AddDefine('HASAMIGA');
      end;
      // TargetCPU
      s := UpperCase(CPUTarget);
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');
      end;
    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    if (poSkipDefaultDefs in Options) then
      Parser.ImplicitUses.Clear;
    lFilename := '';
    Parser.LogEvents:=AEngine.ParserLogEvents;
    Parser.OnLog:=AEngine.Onlog;

    For S in FPCCommandLine do
      ProcessCmdLinePart(S);
    if lFilename = '' then
      raise Exception.Create(SErrNoSourceGiven);
    FileResolver.AddIncludePath(ExtractFilePath(lFileName));
    opts:=[po_AsmWhole,po_AsmPascalComments];
    if AllowMem then
      Include(opts,po_allowmem);
    opts:=opts+Scanner.options;
    Parser.Options:=Parser.Options+opts;
    Parser.OnLog:=@DoParseLog;

    Scanner.OpenFile(lFilename);

    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
  end;
end;


procedure TPrefixer.Execute;

var
  M: TPasModule;
  P : TPasProgram absolute M;
  L : TPasLibrary absolute M;
  E: TPasTreeContainer;
  cmdline : Array of String;

begin
  FFullFileName:=ExpandFileName(FFileName);
  cmdline:=Params.ToStringArray;
  CmdLine:=Concat(CmdLine,[FileName]);
  E := TSimpleEngine.Create;
  M := nil;
  try
    E.OnLog:=@DoParseLog;
    E.ParserLogEvents:=[pleImplementation,pleInterface];
    FSources.LoadFromFile(FFileName);
    FDottedSources.Clear;
    M := Self.ParseSource(E, cmdline, 'linux', 'i386',[]);
    if UnitFileMode in [fmInclude,fmIncludeNamespace] then
      begin
      if IncludeUnitNameMode=inmIfndef then
        FDottedSources.Add('unit '+DestUnitName+';');
      FDottedSources.Add('{$DEFINE '+Define+'}');
      end;
    if M is TPasProgram then
      begin
      if UnitFileMode in [fmReplace,fmInclude] then
        ReworkUses(P.ProgramSection)
      else
        AddNameSpaces(FSources,P.ProgramSection.UsesClause);
      end
    else if M is TPasLibrary then
      begin
      if UnitFileMode in [fmReplace,fmInclude] then
        ReworkUses(L.LibrarySection)
      else
        AddNameSpaces(FSources,L.LibrarySection.UsesClause);
      end
    else
      begin
      if UnitFileMode in [fmReplace,fmInclude] then
        begin
        ReworkUses(M.ImplementationSection);
        ReworkUses(M.InterfaceSection);
        CorrectUnitName(M.Name,M.SourceLinenumber);
        end
      else
        begin
        AddNamespaces(FDottedSources,M.ImplementationSection.UsesClause);
        AddNameSpaces(FDottedSources,M.InterfaceSection.UsesClause);
        end;
      end;
    if UnitFileMode in [fmReplace,fmReplaceNamespace] then
      begin
      MaybeBackup(DestFileName);
      FSources.SaveToFile(DestFileName);
      end
    else
      begin
      MaybeBackup(FileName);
      FSources.SaveToFile(FileName);
      if not SkipDestFileName then
        begin
        FDottedSources.Add('{$i '+ExtractFileName(FileName)+'}');
        MaybeBackup(DestFileName);
        FDottedSources.SaveToFile(DestFileName);
        end;
      end;
  finally
    FreeAndNil(M);
    FreeAndNil(E)
  end;
end;

procedure TPrefixer.ReworkUses(aUses, aNewUses: TStrings);

Var
  S,aLine : String;
  aUnitNames : TStringList;
  I,Idx : Integer;


begin
  aUnitNames:=TStringList.Create;
  try
    aUnitNames.Sorted:=True;
    aUnitNames.Duplicates:=dupIgnore;
    S:='';
    For I:=0 to aUses.Count-1 do
      S:=S+#10+aUses[I];
    GetAdditionalUnits(aUnitNames,S);
    aNewuses.Clear;
    aNewuses.Add('{$IFDEF '+Define+'}');
    For Idx:=0 to aUses.Count-1 do
      begin
      aLine:=aUses[Idx];
      aLine:=ReplaceUnits(aLine,aUnitNames);
      aNewUses.Add(aLine);
      end;
    // Add original
    aNewuses.Add('{$ELSE '+Define+'}');
    aNewuses.AddStrings(aUses);
    aNewuses.Add('{$ENDIF '+Define+'}');
  finally
    aUnitNames.Free;
  end;
end;

class function TPrefixer.ExtractPrefix(const aRule: String) : String;

Var
  P : Integer;

begin
  // *Prefix.UnitName
  if Copy(aRule,1,1)='*' then
    begin
    P:=Pos('.',aRule);
    Result:=Copy(aRule,2,P-2);
    end
  else
    begin
    // Prefix,UnitNamerule
    P:=Pos(',',aRule);
    if P=0 then
      P:=Length(aRule)+1;
    Result:=Copy(aRule,1,P-1);
    end;
end;

class function TPrefixer.ApplyAliasRule(const aName, aRule: String) : String;

begin
  If Copy(aRule,1,1)='*' then
    Result:=Copy(aRule,2)
  else if aRule<>'' then
    Result:=aRule+'.'+aName
  else
    Result:=aName;
end;

class function TPrefixer.ApplyRule(const aFile, aCasedName,aRule: String;
  PrettyPrint: Boolean): String;

Var
  p,len : Integer;
  aExt,aDir,aName,aPrefix : String;

begin
  aPrefix:='';
  aDir:=ExtractFilePath(aFile);
  aExt:=ExtractFileExt(aFile);
  Result:=ExtractFileName(aFile);
  // *DottedUnitName
  // Prefix
  // Prefix,*UnitSuffix
  // Prefix,-DeleteFromOriginalAtStart
  // Prefix,DeleteFromOriginalAtEnd-
  P:=Pos(',',aRule);
  if P=0 then
    begin
    if aRule<>'' then
      if aRule[1]='*' then
        Result:=Copy(aRule,2)+aExt
      else if PrettyPrint and (aCasedName<>'') then
        Result:=aRule+'.'+aCasedName+aExt
      else
        aPrefix:=aRule+'.'
    end
  else
    begin
    aPrefix:=Copy(aRule,1,P-1)+'.';
    aName:=Copy(aRule,P+1);
    Len:=Length(AName);
    if Len>0 then
      begin
      Case aName[1] of
        '*' : Result:=Copy(aName,2)+ExtractFileExt(Result);
        '-' : if Pos(Copy(aName,2),Result)=1 then
               Delete(Result,1,Len-1);
      else
        if (aName[Len]='-') and (RPos(aName,Result)=Length(Result)-Len+1) then
          Result:=Copy(Result,1,Length(Result)-Len);
      end;
      end;
    end;
  if PrettyPrint then
    Result[1]:=Upcase(Result[1]);
  Result:=aDir+aPrefix+Result;
end;

end.

