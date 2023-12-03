unit namespacetool;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, types, prefixer;

Const
  DefaultSubdir = 'namespaced';
  DefaultDoneList = 'done.lst';

type

  { TNamespaceCreation }
  TSubDirMode = (
                 sdmAppend, // append dirmap result to subdir
                 sdmReplace // replace directory part with result of dirmap
                );
  TNamespaceToolLogEvent = procedure(Sender : TObject; EventType : TEventType; Const Msg : String) of object;
  TChangeFPMakeResult = (cmrFailed,cmrAlreadyDone,cmrOK);

  { TNamespaceTool }

  TNamespaceTool = class(TComponent)
  Private
    FDoneFileName : string;
    FDirMapFileName: string;
    FOnLog: TNamespaceToolLogEvent;
    FPrefixesFileName: string;
    FDefaultPrefix: string;
    FFPMakeNameSpaceFile : String;
    FSubDir : String;
    FCasedFiles,
    FUpdate,
    FDryRun,
    FWritePrefixes,
    FBackup: Boolean;
    FSubdirMode: TSubDirMode;
    FFPMakeMap : TStrings;
    FDirmap : TStrings;
    FKnownPrefixes : TStrings;
    FRestart : Boolean;
    FLastOpts: TStringDynArray;
    FLastRule,
    FLastDir : String;
    FForcedExt : String;
    procedure DoPrefixLog(Sender: TObject; aType: TEventType; const aMsg: String
      );
    procedure SetForcedExt(AValue: String);
    procedure SetSubdir(AValue: String);
  Protected

    procedure DoMsg(const aFmt: String; const aArgs: array of const;
      EventType: TEventType=etInfo); overload;
    procedure DoMsg(const aMessage: String; EventType: TEventType=etInfo); overload;
    // Add code to initialize namespace to fpmake in filename.
    function AddNamespaceNameToFpMake(const aFileName: string): TChangeFPMakeResult;
    // add file to FPMake namespaces file
    procedure AddToFPMakeMap(const aSrcFileName, aDestFileName: string);
    // Create directory if not dryrun
    procedure CreateDestDir(const aDestDir: string);
    // Actual HandleFileList
    procedure DoHandleFileList(const aFileName: String);
    // Return name of package dir from filename (first level of dir tree).
    function GetPackageDir(const aFileName: string): string;
    // Return unit name from file name.
    function GetUnitNameFromFile(aFile: String): string;
    // Split line into
    procedure SplitLine(aLine: String; out aFileName, aRule: String;
      var aOpts: TStringDynArray);
    // Write FPMake Namespaces file.
    procedure WritePackageNameSpaceFile(aDir: String; aList: TStrings; DoClear: Boolean=True);
  Public
    class procedure SplitRuleLine(aLine: String; out aFileName, aRule: String;
      var AlastDir, aLastRule: String; var aOpts, aLastOpts: TStringDynArray);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Initialize (load config files)
    Procedure Init;
    // Actual actions
    // Apply rule to a single unit file
    procedure HandleFile(const aFileName: String; aRule: string; aOptions: array of String);
    // Load file list and call handlefile for each
    procedure HandleFileList(const aFileName: String);
    // Create a 'known prefixes' file with the names of the files
    procedure CreateKnown(const aFileName: String);


    Property OnLog : TNamespaceToolLogEvent Read FOnLog Write FOnLog;
    Property ForcedExt : String Read FForcedExt Write SetForcedExt;
    Property DirMapFileName : String Read FDirMapFileName Write FDirMapFileName;
    Property PrefixesFileName : String Read FPrefixesFileName Write FPrefixesFileName;
    Property DefaultPrefix : String Read FDefaultPrefix Write FDefaultPrefix;
    Property Subdir : String Read FSubdir Write SetSubdir;
    Property SubdirMode : TSubDirMode Read FSubdirMode Write FSubdirMode;
    Property Backup : Boolean Read FBackup Write FBackup;
    Property Update : Boolean Read FUpdate Write FUpdate;
    Property DryRun : Boolean Read FDryRun Write FDryRun;
    Property Restart : Boolean Read FRestart Write FRestart;
    Property CasedFiles : Boolean Read FCasedFiles Write FCasedFiles;
    Property FPMakeNameSpaceFile : String Read FFPMakeNameSpaceFile Write FFPMakeNameSpaceFile;
    Property KnownPrefixes : TStrings Read FKnownPrefixes;
    Property DirMap : Tstrings Read FDirmap;
  end;

implementation

procedure TNamespaceTool.CreateDestDir(const aDestDir : string);

begin
  if not DirectoryExists(aDestDir) then
    begin
    DoMsg('Creating destination directory: %s',[aDestDir]);
    if not FDryRun then
      if not ForceDirectories(aDestDir) then
        Raise Exception.Create('Could not create destination directory '+aDestDir);
    end;
end;

procedure TNamespaceTool.DoMsg(const aFmt: String; const aArgs: array of const; EventType : TEventType = etInfo);
begin
  DoMsg(Format(aFmt,aArgs),EventType);
end;

procedure TNamespaceTool.DoMsg(const aMessage: String; EventType : TEventType = etInfo);
begin
  if assigned(OnLog) then
    OnLog(Self,EventType, aMessage);
end;

procedure TNamespaceTool.AddToFPMakeMap(const aSrcFileName,aDestFileName : string);

Var
  Src,Dest,aDir,aRule : String;

begin
  Src:=aSrcFileName;
  Dest:=aDestFileName;
  // Strip package dir
  aDir:=GetPackageDir(aSrcFileName);
  if Pos(aDir,Src)=1 then
    Delete(Src,1,Length(aDir));
  if Pos(aDir,Dest)=1 then
    Delete(Dest,1,Length(aDir));
  // Map file itself.
  FFPMakeMap.Values[Src]:=Dest;
  aDir:=ExtractFilePath(Src);
  // Map source directory to namespaced
  aRule:='{s*:'+aDir+'}';
  FFPMakeMap.Values[aRule]:=ExtractFilePath(Dest);
  // Add original to include directory
  aRule:='{i+:'+aDir+'}';
  if FFPMakeMap.IndexOf(aRule)=-1 then
    FFPMakeMap.Add(aRule);
end;

function TNamespaceTool.GetUnitNameFromFile(aFile : String) : string;

begin
  Result:=ExtractFileName(ChangeFileExt(aFile,''))
end;

procedure TNamespaceTool.SetForcedExt(AValue: String);
begin
  if FForcedExt=AValue then Exit;
  if (aValue<>'') and (aValue[1]<>'.') then
    aValue:='.'+aValue;
  FForcedExt:=AValue;
end;

procedure TNamespaceTool.DoPrefixLog(Sender: TObject; aType: TEventType;
  const aMsg: String);
begin
  DoMsg(aMsg,aType);
end;

procedure TNamespaceTool.SetSubdir(AValue: String);
begin
  if FSubdir=AValue then Exit;
  FSubdir:=AValue;
  if FSubDir<>'' then
    FSubDir:=IncludeTrailingPathDelimiter(FSubDir);
end;

procedure TNamespaceTool.HandleFile(const aFileName: String; aRule : string; aOptions: array of String);

Var
  aNewUnitName,aNewUnitFile,Ext,SrcDir,aUnitName,DestDir,aDummy,DestFN : String;
  P : TPrefixer;
  NeedUpdate : Boolean;
  Idx : Integer;

begin
  NeedUpdate:=False;
  Ext:=FForcedExt;
  if Ext='' then
    Ext:=ExtractFileExt(aFileName);
  // Construct File name
  aUnitName:=GetUnitNameFromFile(aFilename);
  // Construct destination dir.
  SrcDir:=ExtractFilePath(aFileName);
  DestDir:=FDirMap.Values[aUnitName];
  if DestDir='' then
    DestDir:=FDirMap.Values[ExcludeTrailingBackslash(SrcDir)];
  if DestDir='' then
    DestDir:=SrcDir;
  case SubDirMode of
    sdmAppend : DestDir:=FSubDir+DestDir;
    sdmReplace : ; // do nothing
  end;
  DestDir:=IncludeTrailingPathDelimiter(DestDir);
  // No rule, see if there is a filename rule in known prefixes
  if aRule='' then
    begin
    Idx:=FKnownPrefixes.IndexOfName(aUnitName);
    if Idx<>-1 then
      FKnownPrefixes.GetNameValue(Idx,aDummy,aRule);
    end;
  aNewUnitFile:=TPrefixer.ApplyRule(aFileName,aDummy,aRule,FCasedFiles and (aRule<>''));
  aNewUnitName:=GetUnitNameFromFile(aNewUnitFile);
  if SameText(aNewUnitName,aUnitName) then
    begin
    DoMsg('Rule for %s does not result in different unit name, skipping.',[aFileName],etWarning);
    exit;
    end;
  DestFN:=DestDir+aNewUnitName+Ext;
  // Add new file to FPMake map.
  AddToFPMakeMap(aFileName,DestFN);
  if FileExists(DestFN) then
    DoMsg('File %s already exists, skipping generation',[DestFN]);
  // Create directory.
  CreateDestDir(DestDir);
  DoMsg('Converting %s to %s',[aFileName,DestFN]);
  if not FDryRun then
    begin
    P:=TPrefixer.Create(Self);
    try
      P.OnLog:=@DoPrefixLog;
      P.UnitFileMode:=fmInclude;
      P.IncludeUnitNameMode:=inmIfndef;
      P.FileName:=aFileName;
      P.NameSpace:=TPrefixer.ExtractPrefix(aRule);
      P.KnownNameSpaces.AddStrings(FKnownPrefixes);
      P.SkipDestFileName:=FileExists(DestFN);
      P.DestFileName:=DestFN;
      P.CreateBackups:=FBackup;
      P.CasedFileNames:=FCasedFiles;
      P.Params.AddStrings(aOptions);
      P.Params.Add('-Fi'+ExtractFilePath(aFileName));
      P.Execute;
    finally
      P.Free;
    end;
    end;
  If NeedUpdate then
    begin
    FKnownPrefixes.Values[aUnitName]:='*'+aNewUnitName;
    FWritePrefixes:=True;
    end;

end;

Function TNamespaceTool.AddNamespaceNameToFpMake(const aFileName : string) : TChangeFPMakeResult;

const
  namespacelist = 'namespaces.lst';

Var
  aFile : TStringList;
  I : Integer;
  aLine : string;

begin
  Result:=cmrFailed;
  aFile:=TStringList.Create;
  try
    aFile.LoadFromFile(aFileName);
    i:=aFile.Count-1;
    while (I>=0) and (Result=cmrFailed) do
      begin
      if Pos('p.namespacemap',LowerCase(aFile[i]))>0 then
        result:=cmrAlreadyDone;
      Dec(I);
      end;
    i:=aFile.Count-1;
    while (I>=0) and (Result=cmrFailed) do
      begin
      aLine:=aFile[i];
      if pos('{$ifndef ALLPACKAGES}',aLine)>0 then
        if Pos('run',Lowercase(aFile[i+1]))>0 then
          begin
          aFile.Insert(I,'');
          aFile.Insert(I,Format('    P.NamespaceMap:=''%s'';',[namespacelist]));
          aFile.Insert(I,'');
          Result:=cmrOK;
          end;
      Dec(I);
      end;
    if Result=cmrOK then
      aFile.SaveToFile(aFileName);
  finally
    aFile.Free;
  end;
end;

procedure TNamespaceTool.WritePackageNameSpaceFile(aDir : String; aList : TStrings; DoClear : Boolean = True);

Var
  FN : String;

begin
  if aDir<>'' then
    aDir:=IncludeTrailingPathDelimiter(aDir);
  if (FFPMakeNameSpaceFile='') or (FFPMakeMap.Count=0) then
    exit;
  FN:=aDir+FFPMakeNameSpaceFile;
  DoMsg('Writing fpmake map file to %s, writing %d rules',[FN,FFPMakeMap.Count]);
  FFPMakeMap.SaveToFile(FN);
  if DoClear then
    FFPMakeMap.Clear;
  if FileExists(aDir+'fpmake.pp') then
    Case AddNamespaceNameToFpMake(aDir+'fpmake.pp') of
      cmrFailed : DoMsg('Failed to set NamespaceMap to file "%s"',[FN],etError);
      cmrAlreadyDone : DoMsg('NamespaceMap already set in "%s"',[FN],etWarning);
      cmrOK : DoMsg('Added NamespaceMap to file "%s"',[FN],etInfo);
    end

end;

constructor TNamespaceTool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDirmap:=TStringList.Create;
  FKnownPrefixes:=TStringList.Create;
  FFPMakeMap:=TStringList.Create;
  FDoneFileName:=DefaultDoneList;
end;

destructor TNamespaceTool.Destroy;
begin
  FreeAndNil(FDirmap);
  FreeAndNil(FKnownPrefixes);
  FreeAndNil(FFPMakeMap);
  inherited Destroy;
end;

procedure TNamespaceTool.Init;
begin
  if (PrefixesFileName<>'') then
    begin
    KnownPrefixes.LoadFromFile(PrefixesFileName);
    DoMsg('Load of %s results in %d known prefixes',[PrefixesFileName,KnownPrefixes.Count]);
    end;
  if (DirMapFileName<>'') then
    begin
    Dirmap.LoadFromFile(DirMapFileName);
    DoMsg('Load of %s results in %d directory mappings',[DirMapFileName,DirMap.Count]);
    end;
end;


procedure TNamespaceTool.SplitLine(aLine: String; out aFileName, aRule: String;
  var aOpts: TStringDynArray);


begin
  SplitRuleLine(aLine,aFileName,aRule,FLastDir,FLastRule,aOpts,FLastOpts);
end;

Class procedure TNamespaceTool.SplitRuleLine(aLine: String; out aFileName, aRule: String; var AlastDir, aLastRule : String; var aOpts, aLastOpts: TStringDynArray);

var
  I,P : Integer;
  aDir,FN,Opt : String;

begin
  aRule:='';
  aFileName:='';
  aOpts:=[];
  P:=Pos(';',aLine);
  if P=0 then
    begin
    FN:=aLine;
    SetLength(aOpts,0);
    end
  else
    begin
    FN:=Copy(aLine,1,P-1);
    Opt:=Trim(Copy(aLine,P+1));
    SetLength(aOpts,Length(Opt));
    I:=0;
    Repeat
      P:=Pos(' ',Opt);
      if P=0 then
        P:=Length(Opt)+1;
      if p>1 then
        begin
        aOpts[I]:=Copy(Opt,1,P-1);
        Opt:=Trim(Copy(Opt,P+1));
        inc(I);
        end;
    until (Opt='');
    SetLength(aOpts,I);
    end;
  P:=Pos('=',FN);
  if P<>0 then
    begin
    aRule:=Copy(FN,P+1);
    FN:=Copy(FN,1,P-1);
    end;
  aFileName:=FN;
  // Use previous rule ?
  aDir:=ExtractFilePath(FN);
  if aDir=aLastDir then
    begin
    if (aRule='') then
      aRule:=aLastRule;
    if Length(aOpts)=0 then
      aOpts:=aLastOpts;
    end;
  aLastDir:=aDir;
  aLastRule:=aRule;
  aLastOpts:=aOpts;
end;

function TNamespaceTool.GetPackageDir(const aFileName : string) : string;

Var
  P : Integer;

begin
  Result:='';
  if aFileName='' then
    exit;
  P:=Pos('/',aFileName,2);
  if P=0 then
    exit;
  Result:=Copy(aFileName,1,P);
  If Result[1]='/' then
    Delete(Result,1,1);
end;

procedure TNamespaceTool.HandleFileList(const aFileName : String);

begin
  DoHandleFileList(aFileName);
  if FWritePrefixes and Update then
    begin
    DoMsg('Updating known prefixes file: %s ',[PrefixesFileName]);
    if not FDryRun then
      FKnownPrefixes.SaveToFile(FPrefixesFileName);
    end;
end;

procedure TNamespaceTool.DoHandleFileList(const aFileName : String);

Var
  List,Done : TStringList;
  aLine,FN,FNDir, LastPackageDir,aRule : String;
  aOpts : TStringDynArray;

begin
  aOpts:=[];
  Done:=Nil;
  LastPackageDir:='';
  List:=TStringList.Create;
  try
    Done:=TStringList.Create;
    if (not FRestart) and fileExists(FDoneFileName) then
      Done.LoadFromFile(FDoneFileName);
    List.LoadFromFile(aFileName);
    For aLine in List do
      begin
      // Lines have 3 parts
      // FileName=Rule;Compile Options
      SplitLine(aLine,FN,aRule,aOpts);
      FNDir:=GetPackageDir(FN);
      if (LastPackageDir<>FNDir) then
        begin
        if (LastPackageDir<>'')  and (FFPMakeNameSpaceFile<>'') then
          WritePackageNameSpaceFile(LastPackageDir,List);
        LastPackageDir:=FNDir;
        end;
      if Done.indexOf(FN)=-1 then
        begin
        try
          HandleFile(FN,aRule,aOpts);
          Done.Add(FN);
        except
          On E : Exception do
            DoMsg('Error %s while handling file %s : %s',[E.ClassName,FN,E.Message],etError);
        end;
        end;
      end;
    if (LastPackageDir<>'')  and (FFPMakeNameSpaceFile<>'') then
      WritePackageNameSpaceFile(LastPackageDir,List);
  finally
    Done.SaveToFile(FDoneFileName);
    List.Free;
  end;
end;

procedure TNamespaceTool.CreateKnown(const aFileName: String);

Var
  List,Done : TStringList;
  aRule,aLine,FN,aUnit,aNewUnit : String;
  aOpts : TStringDynArray;

begin
  Done:=Nil;
  FLastDir:='';
  FLastRule:='';
  aOpts:=[];
  if FPrefixesFileName='' then
    FPrefixesFileName:=ChangeFileExt(aFileName,'.map');
  List:=TStringList.Create;
  try
    Done:=TStringList.Create;
    if FileExists(FPrefixesFileName) then
      Done.LoadFromFile(FPrefixesFileName);
    List.LoadFromFile(aFileName);
    // Lines have 3 parts
    // FileName=Rule;Compile Options
    For aLine in List do
      begin
      SplitLine(aLine,FN,aRule,aOpts);
      aUnit:=ChangeFileExt(ExtractFileName(FN),'');
      aNewUnit:=ChangeFileExt(ExtractFileName(TPrefixer.ApplyRule(FN,aUnit,aRule,FCasedFiles)),'');
      Done.Values[aUnit]:='*'+aNewUnit;
      end;
    if FDryRun then
      begin
      for aLine in Done do
        DoMsg(aLine)
      end
    else
      Done.SaveToFile(FPrefixesFileName);
  finally
    Done.SaveToFile('done.tmp');
    Done.Free;
    List.Free;
  end;

end;

end.

