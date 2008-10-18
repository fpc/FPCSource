{%mainunit dw_html}
{$IFDEF chmInterface}
type

  { TCHMHTMLWriter }

  TCHMHTMLWriter = class(THTMLWriter)
  private
    FOutChm: TStream;
    FChm: TChmWriter;
    FTempUncompressed: TStream;
    FTempUncompressedName: String;
    FTOCName,
    FIndexName,
    FDefaultPage: String;
    FCSSFile: String;
    FAutoTOC,
    FAutoIndex: Boolean;
    FOtherFiles: String;
    procedure ProcessOptions;
    function RetrieveOtherFiles(const DataName: String; out PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
    procedure LastFileAdded(Sender: TObject);
    procedure GenerateTOC;
    procedure GenerateIndex;
  public
    procedure WriteHTMLPages; override;
    
    function  InterPretOption(const Cmd,Arg : String): boolean; override;

    class procedure Usage(List: TStrings); override;
  end;
{$ELSE} // implementation

{ TCHMHTMLWriter }

procedure TCHMHTMLWriter.ProcessOptions;
var
  TempStream: TMemoryStream;
begin
  if FDefaultPage = '' then
    FDefaultPage := 'index.html'
  else
  begin
    WriteLn('Note: --index-page not assigned. Using default "index.html"');
  end;
  
  if FCSSFile <> '' then
  begin
    TempStream := TMemoryStream.Create;
    TempStream.LoadFromFile(FCSSFile);
    TempStream.Position := 0;
    FChm.AddStreamToArchive('fpdoc.css', '/', TempStream, True);
    TempStream.Free;
  end;

  FChm.DefaultPage := FDefaultPage;
  
  if FOtherFiles <> '' then
  begin
    FChm.FilesToCompress.LoadFromFile(FOtherFiles);
  end;

end;

function TCHMHTMLWriter.RetrieveOtherFiles(const DataName: String; out
  PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
var
  Dir: String;
begin
  if Stream <> nil then
    Stream.Free;
  Stream := TMemoryStream.Create;
  TMemoryStream(Stream).LoadFromFile(DataName);
  FileName := ExtractFileName(DataName);
  
  if ExtractFileDir(DataName) <> '' then
    PathInChm := ExtractRelativepath(GetCurrentDir, ExtractFileDir(DataName))
  else
    PathInChm := '/';
  FixHTMLpath(PathInChm);
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
  WriteLn('Finishing compressing...');
end;

function TOCSort(Item1, Item2: TChmSiteMapItem): Integer;
begin
  Result := CompareText(LowerCase(Item1.Text), LowerCase(Item2.Text));
end;

function GetAlphaItem(AItems: TChmSiteMapItems; AName: String): TChmSiteMapItem;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to AItems.Count-1 do
  begin
    if AItems.Item[x].Text = AName then
      Exit(AItems.Item[x]);
  end;
  Result := AItems.NewItem;
  Result.Text := AName;
end;
     
procedure TCHMHTMLWriter.GenerateTOC;
var
  TOC: TChmSiteMap;
  Element: TPasElement;
  k: Integer;
  j: Integer;
  i: Integer;
  AModule: TPasModule;
  Member: TPasElement;
  Stream: TMemoryStream;
  TmpItem:  TChmSiteMapItem;
  ObjByUnitItem,
  AlphaObjItem,
   ObjUnitItem,
  RoutinesByUnitItem,
    RoutinesUnitItem,
  AlphaRoutinesItem: TChmSiteMapItem;

begin
  WriteLn('Generating Table of contents...');
  if Assigned(Package) then
  begin
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
      ObjUnitItem := ObjByUnitItem.Children.NewItem;
      ObjUnitItem.Text := AModule.Name;
      RoutinesUnitItem := RoutinesByUnitItem.Children.NewItem;
      RoutinesUnitItem.Text := AModule.Name;
      for j := 0 to AModule.InterfaceSection.Classes.Count-1 do
      begin
        Element := TPasClassType(AModule.InterfaceSection.Classes[j]);
        // by unit
        TmpItem := ObjUnitItem.Children.NewItem;
        TmpItem.Text := Element.Name;
        TmpItem.Local := Allocator.GetFilename(Element, 0);
        
        //alpha
        TmpItem := GetAlphaItem(AlphaObjItem.Children, UpperCase(Copy(Element.Name, 1, 2))).Children.NewItem;
        TmpItem.Text := Element.Name;
        TmpItem.Local := Allocator.GetFilename(Element, 0);
        
      end;
      
      // non object procedures and functions
      for j := 0 to AModule.InterfaceSection.Functions.Count-1 do
      begin
        Element := TPasFunctionType(AModule.InterfaceSection.Functions[j]);
        // by unit
        TmpItem := RoutinesUnitItem.Children.NewItem;
        TmpItem.Text := Element.Name;
        TmpItem.Local := Allocator.GetFilename(Element, 0);
        
        // alpha
        TmpItem := GetAlphaItem(AlphaRoutinesItem.Children, UpperCase(Element.Name[1])).Children.NewItem;
        TmpItem.Text := Element.Name;
        TmpItem.Local := Allocator.GetFilename(Element, 0);
      end;
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
  
  TOC.SaveToStream(Stream);
  TOC.Free;

  fchm.AppendTOC(Stream);
  Stream.Free;
end;

type
  TClassMemberType = (cmtProcedure, cmtFunction, cmtConstructor, cmtDestructor,
      cmtInterface, cmtProperty, cmtVariable, cmtUnknown);
  
function ElementType(Element: TPasElement): TClassMemberType;
var
  ETypeName: String;
begin
  Result := cmtUnknown;
  ETypeName := Element.ElementTypeName;
  //overloaded we don't care
  if ETypeName[1] = 'o' then ETypeName := Copy(ETypeName, 11, Length(ETypeName));
  
  if ETypeName[1] = 'f' then Exit(cmtFunction);
  if ETypeName[1] = 'c' then Exit(cmtConstructor);
  if ETypeName[1] = 'v' then Exit(cmtVariable);
  if ETypeName[1] = 'i' then Exit(cmtInterface);
  // the p's
  if ETypeName[4] = 'c' then Exit(cmtProcedure);
  if ETypeName[4] = 'p' then Exit(cmtProperty);
  
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
begin
  WriteLn('Generating Index...');

  if Assigned(Package) then
  begin
  try
    Index := TChmSiteMap.Create(stIndex);
    Stream := TMemoryStream.Create;
    for i := 0 to Package.Modules.Count - 1 do
    begin
      AModule := TPasModule(Package.Modules[i]);
      ParentItem := Index.Items.NewItem;
      ParentItem.Text := AModule.Name;
      ParentItem.Local := Allocator.GetFilename(AModule, 0);

      //  classes
      for j := 0 to AModule.InterfaceSection.Classes.Count-1 do
      begin
        ParentElement := TPasClassType(AModule.InterfaceSection.Classes[j]);
        ParentItem := Index.Items.NewItem;
        ParentItem.Text := ParentELement.Name;
        ParentItem.Local := Allocator.GetFilename(ParentElement, 0);
        for k := 0 to TPasClassType(ParentElement).Members.Count-1 do
        begin
          TmpElement := TPasElement(TPasClassType(ParentElement).Members.Items[k]);
          if Engine.HidePrivate and(TmpElement.Visibility = visPrivate) then
            continue;
          if Engine.HideProtected and(TmpElement.Visibility = visProtected) then
            continue;
          TmpItem := ParentItem.Children.NewItem;
          case ElementType(TmpElement) of
            cmtProcedure   : TmpItem.Text := TmpElement.Name + ' procedure';
            cmtFunction    : TmpItem.Text := TmpElement.Name + ' function';
            cmtConstructor : TmpItem.Text := TmpElement.Name + ' constructor';
            cmtDestructor  : TmpItem.Text := TmpElement.Name + ' destructor';
            cmtProperty    : TmpItem.Text := TmpElement.Name + ' property';
            cmtVariable    : TmpItem.Text := TmpElement.Name + ' variable';
            cmtInterface   : TmpItem.Text := TmpElement.Name + ' interface';
            cmtUnknown     : TmpItem.Text := TmpElement.Name;
          end;
          TmpItem.Local := Allocator.GetFilename(TmpElement, 0);
          {
          ParentElement = Class
             TmpElement = Member
          }
          MemberItem := nil;
          MemberItem := GetAlphaItem(Index.Items, TmpElement.Name);
          // ahh! if MemberItem.Local is empty MemberType is not shown!
          MemberItem.Local := Allocator.GetFilename(TmpElement, 0);

          TmpItem := MemberItem.Children.NewItem;
          TmpItem.Text := ParentElement.Name;
          TmpITem.Local := Allocator.GetFilename(TmpElement, 0);
        end;
      end;
      // routines
      for j := 0 to AModule.InterfaceSection.Functions.Count-1 do
      begin
        ParentElement := TPasProcedureType(AModule.InterfaceSection.Functions[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name + ' ' + TPasFunction(ParentElement).ElementTypeName;
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
      end;
      // consts
      for j := 0 to AModule.InterfaceSection.Consts.Count-1 do
      begin
        ParentElement := TPasElement(AModule.InterfaceSection.Consts[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name;
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
      end;
      // types
      for j := 0 to AModule.InterfaceSection.Types.Count-1 do
      begin
        ParentElement := TPasType(AModule.InterfaceSection.Types[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name;
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
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
            TmpItem.Local := ParentItem.Local;
            // root level
            TmpItem := Index.Items.NewItem;
            TmpItem.Text := TmpElement.Name;
            TmpItem.Local := ParentItem.Local;
          end;
        end;
      end;
      // variables
      for j := 0 to AModule.InterfaceSection.Variables.Count-1 do
      begin
        ParentElement := TPasElement(AModule.InterfaceSection.Variables[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name + ' var';
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
      end;
      // declarations
      {
      for j := 0 to AModule.InterfaceSection.Declarations.Count-1 do
      begin
        ParentElement := TPasElement(AModule.InterfaceSection.Declarations[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name;
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
      end;
      // resource strings
      for j := 0 to AModule.InterfaceSection.ResStrings.Count-1 do
      begin
        ParentElement := TPasElement(AModule.InterfaceSection.ResStrings[j]);
        TmpItem := Index.Items.NewItem;
        TmpItem.Text := ParentElement.Name;
        TmpItem.Local := Allocator.GetFilename(ParentElement, 0);
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
    Index.Free;
    Stream.Position :=0 ;
    FChm.AppendIndex(Stream);
    Stream.Free;
  except
    Dump_Stack(StdOut, get_frame);
    Halt(1);
  end;
  end;
end;

procedure TCHMHTMLWriter.WriteHTMLPages;
var
  i: Integer;
  PageDoc: TXMLDocument;
  FileStream: TMemoryStream;
  FileName: String;
  FilePath: String;
begin
  if Engine.Output = '' then
  begin
    WriteLn('Error: no --output option used.');
    Exit;
  end;
  
  if ExtractFileExt(Engine.Output) <> '.chm' then
    ChangeFileExt(Engine.OutPut, '.chm');

  FOutChm := TFileStream.Create(Engine.Output, fmOpenReadWrite or fmCreate);

  FTempUncompressedName := GetTempFileName+IntToStr(GetProcessID) +'.raw';
  FTempUncompressed := TFileStream.Create(FTempUncompressedName, fmOpenReadWrite  or fmCreate);
  FChm := TChmWriter.Create(FOutChm, False);
  FChm.Title := Copy(Package.Name, 2, Length(Package.Name));
  FChm.TempRawStream := FTempUncompressed;
  FChm.OnGetFileData := @RetrieveOtherFiles;
  FChm.OnLastFile := @LastFileAdded;
  
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
            WriteLn(Format(SErrCouldNotCreateFile, [FileName, e.Message]));
        end;
      finally
        PageDoc.Free;
        FileStream.Size := 0;
      end;
    end;
  FileStream.Free;
  WriteLn('HTML Files written. Collecting other files and compressing...this could take some time');
  FChm.Execute;
  FChm.Free;
  // we don't need to free FTempUncompressed
  // FTempUncompressed.Free;
  FOutChm.Free;
  DeleteFile(FTempUncompressedName);
end;

function TCHMHTMLWriter.InterPretOption(const Cmd, Arg: String): boolean;
begin
  Result:=True;
  if Cmd = '--toc-file' then
    FTOCName := arg
  else if Cmd = '--index-file' then
    FIndexName := arg
  else if Cmd = '--default-page' then
    FDefaultPage := arg
  else if Cmd = '--other-files' then
    FOtherFiles := arg
  else if Cmd = '--css-file' then
    FCSSFile := arg
  else if Cmd = '--auto-index' then
    FAutoIndex := True
  else if Cmd = '--auto-toc' then
    FAutoTOC := True
  else
    Result:=inherited InterPretOption(Cmd, Arg);
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
end;



{$ENDIF}
