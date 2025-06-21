unit dw_chm;

{$mode objfpc}
{$h+}

interface

uses Classes, DOM,
    dGlobals, PasTree, dwriter, dw_html, chmwriter, chmtypes, chmsitemap;

type

  { TCHmFileNameAllocator }

  TCHmFileNameAllocator = Class(TLongNameFileAllocator)
    // Override this, because the logic messes up the filenames for plain html files.
    function GetFilename(AElement: TPasElement; ASubindex: Integer): String; override;
  end;

  { TFpDocChmWriter }

  TFpDocChmWriter = class (TChmWriter)
  protected
    procedure FileAdded(AStream: TStream; const AEntry: TFileEntryRec); override;
  end;

  { TCHMHTMLWriter }

  TCHMHTMLWriter = class(THTMLWriter)
  private
    FOutChm: TStream;
    FChm: TFpDocChmWriter;
    FTempUncompressed: TStream;
    FTempUncompressedName: String;
    FChmTitle: String;
    FTOCName,
    FIndexName,
    FDefaultPage: String;
    FMakeSearchable,
    FNoBinToc,
    FNoBinIndex,
    FAutoTOC,
    FAutoIndex: Boolean;
    FOtherFiles: String;
    procedure ProcessOptions;
    function ResolveLinkIDAbs(const Name: String): DOMString;
    function RetrieveOtherFiles(const DataName: String; out PathInChm: String;
              out FileName: String; var Stream: TStream): Boolean;
    procedure LastFileAdded(Sender: TObject);
    function FindAlphaItem(AItems: TChmSiteMapItems; AName: String): TChmSiteMapItem;
    function GetAlphaItem(AItems: TChmSiteMapItems; AName: String): TChmSiteMapItem;
    procedure MultiAlphaItem(AItems: TChmSiteMapItems; AName: String;
            APasEl: TPasElement; Prefix:String);
    procedure GenerateTOC;
    procedure GenerateIndex;
  protected
    procedure DoWriteDocumentation; override;
  public
    function CreateAllocator: TFileAllocator; override;
    function  InterPretOption(const Cmd,Arg : String): boolean; override;

    class procedure Usage(List: TStrings); override;
    Class Function FileNameExtension : String; override;
    Class procedure SplitImport(var AFilename, ALinkPrefix: String); override;
  end;

implementation

uses fpdocstrs, SysUtils, HTMWrite, dw_basehtml;

{ TCHmFileNameAllocator }

function TCHmFileNameAllocator.GetFilename(AElement: TPasElement; ASubindex: Integer): String;
var
  n,s: String;
  i: Integer;
  excl: Boolean; //search
  MElement: TPasElement;
begin
  Result:='';
  excl := False;
  if AElement.ClassType = TPasPackage then
  begin
    Result := 'index';
    excl := True;
  end
  else if AElement.ClassType = TPasModule then
  begin
    Result := LowerCase(AElement.Name) + PathDelim + 'index';
    excl := True;
  end
  else
  begin
    if AElement is TPasOperator then
    begin
      if Assigned(AElement.Parent) then
        result:=LowerCase(AElement.Parent.PathName);
      With TPasOperator(aElement) do
        Result:= Result + 'op-'+OperatorTypeToOperatorName(OperatorType);
      s := '';
      N:=LowerCase(aElement.Name); // Should not contain any weird chars.
      Delete(N,1,Pos('(',N));
      i := 1;
      Repeat
        I:=Pos(',',N);
        if I=0 then
          I:=Pos(')',N);
        if I>1 then
          begin
          if (S<>'') then
            S:=S+'-';
          S:=S+Copy(N,1,I-1);
          end;
        Delete(N,1,I);
      until I=0;
      // First char is maybe :
      if (N<>'') and  (N[1]=':') then
        Delete(N,1,1);
      Result:=Result + '-'+ s + '-' + N;
    end
      else
    begin
      Result := LowerCase(AElement.PathName);
      excl := (ASubindex > 0);
    end;
    // cut off Package Name
    MElement:= AElement.GetModule;
    if Assigned(MElement) then
      AElement:= MElement;
    Result := Copy(Result, Length(AElement.Parent.Name) + 2, MaxInt);
    // to skip dots in unit name
    i := Length(AElement.Name);
    while (i <= Length(Result)) and (Result[i] <> '.') do
      Inc(i);
    if (i <= Length(Result)) and (i > 0) then
      Result[i] := PathDelim;
    if excl or (Length(Result)=0) then
      begin
        // exclude the from full text search index
        s:= '.'+ExtractFileName(Result + '.');
        n:= ExtractFileDir(Result);
        Result := n + DirectorySeparator + s;
        Result := Copy(Result, 1, Length(Result)-1);
      end;
  end;

  if ASubindex > 0 then
    Result := Result + '-' + GetFilePostfix(ASubindex);
  Result := Result + Extension;
end;

{ TFpDocChmWriter }

procedure TFpDocChmWriter.FileAdded ( AStream: TStream;
  const AEntry: TFileEntryRec ) ;
var FTsave : boolean;  
begin
  // Exclude Full text index for files starting from the dot
  if Pos('.', AEntry.Name) <> 1 then
    inherited FileAdded(AStream, AEntry)
  else
    begin
      FTsave:=FullTextSearch;
      FullTextSearch:=False;
      inherited FileAdded(AStream, AEntry);
      FullTextSearch:=FTsave;
    end;
end;

{ TCHMHTMLWriter }

function TCHMHTMLWriter.ResolveLinkIDAbs(const Name: String): DOMString;

begin
  Result:=UTF8Decode(FixHTMLpath(Engine.ResolveLink(Module,Name, True)));
  // for global index: don't make it relative to the current document.
end;

procedure TCHMHTMLWriter.ProcessOptions;
var
  TempStream: TMemoryStream;
begin
  if FDefaultPage = '' then
    FDefaultPage := 'index.html'
  else
  begin
    DoLog('Note: --index-page not assigned. Using default "index.html"');
  end;
  
  if CSSFile <> '' then
  begin
    if not FileExists(CSSFile) Then
      Raise Exception.CreateFmt('Can''t find CSS file "%S"',[CSSFILE]);
    TempStream := TMemoryStream.Create;
    TempStream.LoadFromFile(CSSFile);
    TempStream.Position := 0;
    FChm.AddStreamToArchive('fpdoc.css', '/', TempStream, True);
    TempStream.Free;
  end;

  FChm.DefaultPage := FDefaultPage;
  
  if FOtherFiles <> '' then
  begin
    FChm.FilesToCompress.LoadFromFile(FOtherFiles);
  end;

  FChm.FullTextSearch := FMakeSearchable;

end;

function TCHMHTMLWriter.RetrieveOtherFiles(const DataName: String; out
  PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
begin
  Result:=False;
  if Stream <> nil then
    Stream.Free;
  Stream := TMemoryStream.Create;
  TMemoryStream(Stream).LoadFromFile(DataName);
  FileName := ExtractFileName(DataName);
  
  if ExtractFileDir(DataName) <> '' then
    PathInChm := ExtractRelativepath(GetCurrentDir, ExtractFileDir(DataName))
  else
    PathInChm := '/';
  PathInChm:=FixHTMLpath(IncludeLeadingPathDelimiter(IncludeTrailingPathDelimiter(PathInChm)));
  Stream.Position := 0;
end;

procedure TCHMHTMLWriter.LastFileAdded(Sender: TObject);
var
  TmpStream: TMemoryStream;
begin
  TmpStream := TMemoryStream.Create;
  if FAutoTOC then
    GenerateTOC
  else
    if FTOCName <> '' then
    begin
      TmpStream.LoadFromFile(FTOCName);
      TmpStream.Position := 0;
      FChm.AppendTOC(TmpStream);
      TmpStream.Size := 0;
    end;
    
  if FAutoIndex then
    GenerateIndex
  else
    if FIndexName <> '' then
    begin
      TmpStream.LoadFromFile(FIndexName);
      TmpStream.Position := 0;
      FChm.AppendIndex(TmpStream);
    end;
  TmpStream.Free;
  DoLog('Finishing compressing...');
end;

function TOCSort(Item1, Item2: TChmSiteMapItem): Integer;
begin
  Result := CompareText(LowerCase(Item1.Text), LowerCase(Item2.Text));
end;

function TCHMHTMLWriter.FindAlphaItem(AItems: TChmSiteMapItems; AName: String
  ): TChmSiteMapItem;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to AItems.Count-1 do
  begin
    if AItems.Item[x].Text = AName then
      Exit(AItems.Item[x]);
  end;
end;

function TCHMHTMLWriter.GetAlphaItem(AItems: TChmSiteMapItems; AName: String
  ): TChmSiteMapItem;
begin
  Result := FindAlphaItem(AItems, AName);
  if Result <> nil then Exit;
  Result := AItems.NewItem;
  Result.Text := AName;
end;

procedure TCHMHTMLWriter.MultiAlphaItem(AItems: TChmSiteMapItems; AName: String;
  APasEl: TPasElement; Prefix: String);
var
  AChmItem, AChmChld: TChmSiteMapItem;
begin
  AChmItem:= FindAlphaItem(AItems, AName);
  if AChmItem = nil then
  begin
    // add new
    AChmItem := AItems.NewItem;
    AChmItem.Text :=  AName;
    AChmItem.addLocal(FixHTMLpath(Allocator.GetFilename(APasEl, 0)));
  end
    else
  begin
    // add as child
    AChmChld := AChmItem.Children.NewItem;
    AChmChld.Text := Prefix + '.' + AName;
    AChmChld.addLocal(FixHTMLpath(Allocator.GetFilename(APasEl, 0)));
  end;
end;

procedure TCHMHTMLWriter.GenerateTOC;
var
  TOC: TChmSiteMap;
  Element: TPasElement;
  j: Integer;
  i: Integer;
  AModule: TPasModule;
  Stream: TMemoryStream;
  TmpItem:  TChmSiteMapItem;
  ObjByUnitItem,
  AlphaObjItem,
   ObjUnitItem,
  RoutinesByUnitItem,
    RoutinesUnitItem,
  AlphaRoutinesItem: TChmSiteMapItem;

begin
  DoLog('Generating Table of contents...');
  if not Assigned(Package) then
  begin
    DoLog('Package is not assigned...');
    Exit;
  end;
  Toc := TChmSiteMap.Create(stTOC);
  Stream := TMemoryStream.Create;
  ObjByUnitItem := TOC.Items.NewItem;
  ObjByUnitItem.Text      := 'Classes and Objects, by Unit';
  AlphaObjItem := TOC.Items.NewItem;
  AlphaObjItem.Text       := 'Alphabetical Classes and Objects List';
  RoutinesByUnitItem := TOC.Items.NewItem;
  RoutinesByUnitItem.Text := 'Routines, by Unit';
  AlphaRoutinesItem  := TOC.Items.NewItem;
  AlphaRoutinesItem.Text  := 'Alphabetical Routines List';

  // objects and classes
  for i := 0 to Package.Modules.Count - 1 do
  begin
    AModule := TPasModule(Package.Modules[i]);
    If not assigned(AModule.InterfaceSection) Then
       Continue;
    ObjUnitItem := ObjByUnitItem.Children.NewItem;
    ObjUnitItem.Text := AModule.Name;
    ObjUnitItem.addLocal(FixHTMLpath(Allocator.GetFilename(AModule, ClassesSubindex)));
    RoutinesUnitItem := RoutinesByUnitItem.Children.NewItem;
    RoutinesUnitItem.Text := AModule.Name;
    RoutinesUnitItem.addLocal(FixHTMLpath(Allocator.GetFilename(AModule, ProcsSubindex)));
    for j := 0 to AModule.InterfaceSection.Classes.Count-1 do
    begin
      Element := TPasClassType(AModule.InterfaceSection.Classes[j]);
      // by unit
      TmpItem := ObjUnitItem.Children.NewItem;
      TmpItem.Text := Element.Name;
      TmpItem.addLocal(FixHTMLpath(Allocator.GetFilename(Element, 0)));

      //alpha
      TmpItem := GetAlphaItem(AlphaObjItem.Children, UpperCase(Copy(Element.Name, 1, 2))).Children.NewItem;
      TmpItem.Text := Element.Name;
      TmpItem.addLocal(FixHTMLpath(Allocator.GetFilename(Element, 0)));

    end;

    // non object procedures and functions
    for j := 0 to AModule.InterfaceSection.Functions.Count-1 do
    begin
      Element := TPasFunctionType(AModule.InterfaceSection.Functions[j]);
      // by unit
      TmpItem := RoutinesUnitItem.Children.NewItem;
      TmpItem.Text := Element.Name;
      TmpItem.addLocal(FixHTMLpath(Allocator.GetFilename(Element, 0)));

      // alpha
      TmpItem := GetAlphaItem(AlphaRoutinesItem.Children, UpperCase(Element.Name[1])).Children.NewItem;
      TmpItem.Text := Element.Name;
      TmpItem.addLocal(FixHTMLpath(Allocator.GetFilename(Element, 0)));
    end;
  end;
  // cleanup
  for i := ObjByUnitItem.Children.Count-1 downto 0 do
  begin
    if ObjByUnitItem.Children.Item[i].Children.Count = 0 then
      ObjByUnitItem.Children.Delete(i);
  end;

  for i := RoutinesByUnitItem.Children.Count-1 downto 0 do
  begin
    if RoutinesByUnitItem.Children.Item[i].Children.Count = 0 then
      RoutinesByUnitItem.Children.Delete(i);
  end;
  
  for i := TOC.Items.Count-1 downto 0 do
  begin
    if TOC.Items.Item[i].Children.Count = 0 then
      TOC.Items.Delete(i);
  end;
  
  // Sort
  for i := 0 to TOC.Items.Count-1 do
  begin
    TOC.Items.Item[i].Children.Sort(TListSortCompare(@TOCSort));
    for j := 0 to TOC.Items.Item[i].Children.Count-1 do
    begin
      TOC.Items.Item[i].Children.Item[j].Children.Sort(TListSortCompare(@TOCSort));
    end;
  end;

  if not fnobintoc then
    fchm.AppendBinaryTOCFromSiteMap(Toc);
  TOC.SaveToStream(Stream);
  TOC.Free;

  fchm.AppendTOC(Stream);
  Stream.Free;
  DoLog('Generating TOC done');
end;

type
  TClassMemberType = (cmtProcedure, cmtFunction, cmtConstructor, cmtDestructor,
      cmtInterface, cmtProperty, cmtVariable, cmtOperator, cmtConstant, cmtUnknown);
  
function ElementType(Element: TPasElement): TClassMemberType;
var
  C: TClass;
begin
  Result := cmtUnknown;
  if not Assigned(Element) then Exit;

  C:=Element.ClassType;
  if (C=TPasProcedure) or (C=TPasClassProcedure) then
    exit(cmtProcedure)
  else if (C=TPasFunction) or (C=TPasClassFunction) then
    exit(cmtFunction)
  else if (C=TPasConstructor) or (C=TPasClassConstructor) then
    exit(cmtConstructor)
  else if (C=TPasDestructor) or (C=TPasClassDestructor) then
    exit(cmtDestructor)
  else if (C=TPasOperator) or (C=TPasClassOperator) then
    exit(cmtOperator)
  else if C=TPasConst then
    exit(cmtConstant)
  else if C=TPasVariable then
    exit(cmtVariable)
  else if C=TPasProperty then
    exit(cmtProperty)
  else
    begin
    // Unknown
    exit(cmtUnknown);
    // WriteLn(' Warning El name: '+ Element.Name+' path: '+Element.PathName+' TypeName: '+Element.ElementTypeName);
    end;
end;

procedure TCHMHTMLWriter.GenerateIndex;
var
  Index: TChmSiteMap;
  i, j, k: Integer;
  TmpItem: TChmSiteMapItem;
  ParentItem: TChmSiteMapItem;
  AModule: TPasModule;
  TmpElement: TPasElement;
  ParentElement: TPasElement;
  MemberItem: TChmSiteMapItem;
  Stream: TMemoryStream;
  RedirectUrl,Urls,SName: String;

begin
  DoLog('Generating Index...');

  if not Assigned(Package) then
  begin
    DoLog('Package is not assigned...');
    Exit;
  end;
  Index := TChmSiteMap.Create(stIndex);
  Stream := TMemoryStream.Create;
  for i := 0 to Package.Modules.Count - 1 do
  //if false then
  begin
    AModule := TPasModule(Package.Modules[i]);
    if not assigned(AModule.InterfaceSection) then
      continue;
    ParentItem := Index.Items.NewItem;
    ParentItem.Text := AModule.Name;
    ParentItem.addLocal(FixHTMLpath(Allocator.GetFilename(AModule, 0)));

    //  classes
    for j := 0 to AModule.InterfaceSection.Classes.Count-1 do
    begin
      ParentElement := TPasClassType(AModule.InterfaceSection.Classes[j]);
      ParentItem := Index.Items.NewItem;
      ParentItem.Text := ParentELement.Name;
      ParentItem.addLocal(FixHTMLpath(Allocator.GetFilename(ParentElement, 0)));
      for k := 0 to TPasClassType(ParentElement).Members.Count-1 do
      begin
        TmpElement := TPasElement(TPasClassType(ParentElement).Members.Items[k]);
        if Engine.HidePrivate and(TmpElement.Visibility = visPrivate) then
          continue;
        if Engine.HideProtected and(TmpElement.Visibility = visProtected) then
          continue;
        Urls:=FixHTMLpath(Allocator.GetFilename(TmpElement, 0));
        RedirectUrl:='';
        if TmpElement is TPasEnumValue then
           RedirectUrl := UTF8Encode(ResolveLinkIDAbs(tmpElement.Parent.PathName))
         else
           RedirectUrl := UTF8Encode(ResolveLinkIDAbs(tmpElement.PathName));

        if(trim(RedirectUrl)<>'') and (RedirectUrl<>urls) then
          begin
            //writeln('Hint: Index Resolved:',urls,' to ',RedirectUrl);
            urls:=RedirectUrl;
          end;

        TmpItem := ParentItem.Children.NewItem;
        case ElementType(TmpElement) of
          cmtProcedure   : TmpItem.Text := TmpElement.Name + ' procedure';
          cmtFunction    : TmpItem.Text := TmpElement.Name + ' function';
          cmtConstructor : TmpItem.Text := TmpElement.Name + ' constructor';
          cmtDestructor  : TmpItem.Text := TmpElement.Name + ' destructor';
          cmtProperty    : TmpItem.Text := TmpElement.Name + ' property';
          cmtVariable    : TmpItem.Text := TmpElement.Name + ' variable';
          cmtInterface   : TmpItem.Text := TmpElement.Name + ' interface';
          cmtOperator    : TmpItem.Text := TmpElement.Name + ' operator';
          cmtConstant    : TmpItem.Text := TmpElement.Name + ' const';
          cmtUnknown     : TmpItem.Text := TmpElement.Name;
        end;
        TmpItem.addLocal(Urls);
        {
        ParentElement = Class
           TmpElement = Member
        }
        MemberItem := nil;
        MemberItem := GetAlphaItem(Index.Items, TmpElement.Name);
        // ahh! if MemberItem.Local is empty MemberType is not shown!
        MemberItem.addLocal(Urls);

        TmpItem := MemberItem.Children.NewItem;
        TmpItem.Text := ParentElement.Name;
        TmpItem.AddLocal(Urls);
      end;
    end;
    // routines
    for j := 0 to AModule.InterfaceSection.Functions.Count-1 do
    begin
      // routine name
      ParentElement := TPasElement(AModule.InterfaceSection.Functions[j]);
      case ElementType(ParentElement) of
        cmtProcedure   : SName:= ' procedure';
        cmtFunction    : SName:= ' function';
        cmtOperator    : SName:= ' operator';
        //cmtConstant    : SName:= ' const';
        else             SName:= ' unknown'
      end;
      SName:= ParentElement.Name + ' ' + SName;
      MultiAlphaItem(Index.Items, SName, ParentElement, AModule.Name);
    end;
    // consts
    for j := 0 to AModule.InterfaceSection.Consts.Count-1 do
    begin
      ParentElement := TPasElement(AModule.InterfaceSection.Consts[j]);
      SName:= ParentElement.Name + ' const';
      MultiAlphaItem(Index.Items, SName, ParentElement, AModule.Name);
    end;
    // types
    for j := 0 to AModule.InterfaceSection.Types.Count-1 do
    begin
      ParentElement := TPasType(AModule.InterfaceSection.Types[j]);
      TmpItem := Index.Items.NewItem;
      TmpItem.Text := ParentElement.Name;
      TmpItem.addLocal(FixHTMLpath(Allocator.GetFilename(ParentElement, 0)));
      // enums
      if ParentELement is TPasEnumType then
      begin
        ParentItem := TmpItem;
        for k := 0 to TPasEnumType(ParentElement).Values.Count-1 do
        begin
          TmpElement := TPasType(TPasEnumType(ParentElement).Values.Items[k]);
          // subitem
          TmpItem := ParentItem.Children.NewItem;
          TmpItem.Text := TmpElement.Name;
          TmpItem.addLocal(ParentItem.Local);
          // root level
          TmpItem := Index.Items.NewItem;
          TmpItem.Text := TmpElement.Name;
          TmpItem.addLocal(ParentItem.Local);
        end;
      end;
    end;
    // variables
    for j := 0 to AModule.InterfaceSection.Variables.Count-1 do
    begin
      ParentElement := TPasElement(AModule.InterfaceSection.Variables[j]);
      SName:= ParentElement.Name + ' variable';
      MultiAlphaItem(Index.Items, SName, ParentElement, AModule.Name);
    end;
    // declarations
    {
    for j := 0 to AModule.InterfaceSection.Declarations.Count-1 do
    begin
      ParentElement := TPasElement(AModule.InterfaceSection.Declarations[j]);
      TmpItem := Index.Items.NewItem;
      TmpItem.Text := ParentElement.Name;
      TmpItem.Local := FixHTMLpath(Allocator.GetFilename(ParentElement, 0));
    end;
    // resource strings
    for j := 0 to AModule.InterfaceSection.ResStrings.Count-1 do
    begin
      ParentElement := TPasElement(AModule.InterfaceSection.ResStrings[j]);
      TmpItem := Index.Items.NewItem;
      TmpItem.Text := ParentElement.Name;
      TmpItem.Local := FixHTMLpath(Allocator.GetFilename(ParentElement, 0));
    end;
    }
  end;

  // Sort
  Index.Items.Sort(TListSortCompare(@TOCSort));
  for i := 0 to Index.Items.Count-1 do
  begin
    Index.Items.Item[i].Children.Sort(TListSortCompare(@TOCSort));
  end;
  // save
  Index.SaveToStream(Stream);
  if not fnobinindex then
    fchm.AppendBinaryindexFromSitemap(index,false);
  Index.Free;
  Stream.Position :=0 ;
  FChm.AppendIndex(Stream);
  Stream.Free;
  DoLog('Generating Index Done');
end;

procedure TCHMHTMLWriter.DoWriteDocumentation;
var
  i: Integer;
  PageDoc: TXMLDocument;
  FileStream: TMemoryStream;
  IFileName,FileName: String;
  FilePath: String;
begin
  AllocatePages;
  DoLog(SWritingPages, [PageCount]);

  FileName := Engine.Output;
  if FileName = '' then
    Raise Exception.Create('Error: no --output option used.'); 
  
  if ExtractFileExt(FileName) <> FileNameExtension then
    FileName := ChangeFileExt(FileName, FileNameExtension);

  FOutChm := TFileStream.Create(FileName, fmOpenReadWrite or fmCreate);

  FTempUncompressedName := GetTempFileName+IntToStr(GetProcessID) +'.raw';
  FTempUncompressed := TFileStream.Create(FTempUncompressedName, fmOpenReadWrite  or fmCreate);
  FChm := TFpDocChmWriter.Create(FOutChm, False);
  FChm.Title := FChmTitle;
  FChm.TempRawStream := FTempUncompressed;
  FChm.OnGetFileData := @RetrieveOtherFiles;
  FChm.OnLastFile := @LastFileAdded;
  FChm.hasbinarytoc:=not fnobintoc;
  FChm.hasbinaryindex:=not fnobinindex;
  //FChm.Cores:=1;
  ProcessOptions;

  FileStream := TMemoryStream.Create;
  for i := 0 to PageInfos.Count - 1 do
    with TPageInfo(PageInfos[i]) do
    begin
      PageDoc := CreateHTMLPage(Element, SubpageIndex);
      try
        FileName := ExtractFileName(Allocator.GetFilename(Element, SubpageIndex));
        FilePath := '/'+FixHTMLpath(ExtractFilePath(Allocator.GetFilename(Element, SubpageIndex)));

        try
          WriteHTMLFile(PageDoc, FileStream);
          FChm.AddStreamToArchive(FileName, FilePath, FileStream, True);
        except
          on E: Exception do
            DoLog(Format(SErrCouldNotCreateFile, [FileName, e.Message]));
        end;
      finally
        PageDoc.Free;
        FileStream.Size := 0;
      end;
    end;
  FileStream.Free;

  DoLog('HTML Files written. Collecting other files and compressing...this could take some time');

  //write any found images to CHM stream
  FileStream := TMemoryStream.Create;
  for iFilename in ImageFileList do
  begin
{$ifdef imagetest}    DoLog('  adding image: '+iFileName); {$endif}
    if FileExists(iFileName) then
    begin
{$ifdef imagetest}    DoLog(' - found'); {$endif}
      FileName := ExtractFileName(iFileName);
      FilePath := '/'+FixHTMLpath(ExtractFilePath(iFileName));

      FileStream.LoadFromFile(iFileName);
      FChm.AddStreamToArchive(FileName, FilePath, FileStream, True);
      FileStream.Size := 0;
    end
    else
      {$ifdef imagetest}  DoLog(' - not found'){$endif};
  end;
  FileStream.Free;

  FChm.Execute;
  FChm.Free;
  DoLog('Collecting done');
  // we don't need to free FTempUncompressed it is freed into TFpDocChmWriter
  // FTempUncompressed.Free;
  FOutChm.Free;
  DeleteFile(FTempUncompressedName);
end;

function TCHMHTMLWriter.CreateAllocator: TFileAllocator;
begin
  Result:=TCHmFileNameAllocator.Create('.html');
end;

function TCHMHTMLWriter.InterPretOption(const Cmd, Arg: String): boolean;
begin
  Result:=True;
  FNoBinToc:=False;
  FnoBinIndex:=False;
  if Cmd = '--toc-file' then
    FTOCName := arg
  else if Cmd = '--index-file' then
    FIndexName := arg
  else if Cmd = '--default-page' then
    FDefaultPage := arg
  else if Cmd = '--other-files' then
    FOtherFiles := arg
  else if Cmd = '--auto-index' then
    FAutoIndex := True
  else if Cmd = '--auto-toc' then
    FAutoTOC := True
  else if Cmd = '--no-bintoc' then
    FNoBinToc := True
  else if Cmd = '--no-binindex' then
    FNoBinIndex := True
  else if Cmd = '--make-searchable' then
    FMakeSearchable := True
  else if Cmd = '--chm-title' then
    FChmTitle := arg
  else
    Result:=inherited InterPretOption(Cmd, Arg);

  if Length(FChmTitle) = 0 then
    FChmTitle := Copy(Package.Name, 2, Length(Package.Name));
end;

class procedure TCHMHTMLWriter.Usage(List: TStrings);
begin
  THTMLWriter.Usage(List);
  List.add('--default-page');
  List.Add(SCHMUsageDefPage);
  List.add('--toc-file');
  List.Add(SCHMUsageTOC);
  List.add('--index-file');
  List.Add(SCHMUsageIndex);
  List.add('--other-files');
  List.Add(SCHMUsageOtrFiles);
  List.add('--css-file');
  List.Add(SCHMUsageCSSFile);
  List.add('--auto-index');
  List.Add(SCHMUsageAutoIDX);
  List.add('--auto-toc');
  List.Add(SCHMUsageAutoTOC);
  List.add('--make-searchable');
  List.Add(SCHMUsageMakeSearch);
  List.Add('--chm-title');
  List.Add(SCHMUsageChmTitle);
end;

class function TCHMHTMLWriter.FileNameExtension: String;

begin
  result:='.chm';
end;

class procedure TCHMHTMLWriter.SplitImport(var AFilename, ALinkPrefix: String);
var
  i: integer;
begin
  i := Pos(',', AFilename);
  if i > 0 then
    begin  //split into filename and prefix
    ALinkPrefix := Copy(AFilename,i+1,Length(AFilename));
    SetLength(AFilename, i-1);
    if copy(ALinkPrefix,1,2)='..' then // workaround for project files.
      begin
        ALinkPrefix := 'ms-its:' + ChangeFileExt(ExtractFileName(AFilename), '.chm') + '::/';
        AFilename := ChangeFileExt(AFilename, '.xct');
      end;
    end
  else if ALinkPrefix = '' then
    begin  //synthesize outdir\pgk.xct, ms-its:pkg.chm::/
    ALinkPrefix := 'ms-its:' + ChangeFileExt(ExtractFileName(AFilename), '.chm') + '::/';
    AFilename := ChangeFileExt(AFilename, '.xct');
    end;
end;

initialization
  RegisterWriter(TCHMHTMLWriter,'chm','Compressed HTML file output using fpdoc.css stylesheet.');
finalization
  UnRegisterWriter('chm');
end.
