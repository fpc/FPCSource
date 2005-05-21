{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Berczi Gabor

    HTML scanner objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WHTMLScn;

interface

uses Objects,
     WHTML;

const
     HTMLIndexMagicNo = ord('H')+ord('H') shl 8+ord('I') shl 16+ord('X') shl 24;
     HTMLIndexVersion = 2;

type
     PHTMLLinkScanner = ^THTMLLinkScanner;
     PHTMLLinkScanDocument = ^THTMLLinkScanDocument;

     TCustomHTMLLinkScanner = object(THTMLParser)
       function    DocAddTextChar(C: char): boolean; virtual;
       procedure   DocAnchor(Entered: boolean); virtual;
     public
    {a}function    CheckURL(const URL: string): boolean; virtual;
    {a}function    CheckText(const Text: string): boolean; virtual;
    {a}procedure   AddLink(const LinkText, LinkURL: string); virtual;
    {a}function    GetDocumentBaseURL: string; virtual;
     private
       CurLinkText: string;
       CurURL: string;
       CurDoc: string;
       InAnchor,InNameAnchor: boolean;
       LastSynonym: PHTMLLinkScanDocument;
     end;

     THTMLLinkScanDocument = object(TObject)
       constructor Init(const ADocName: string);
       function    GetName: string;
       function    GetUniqueName: string;
       function    GetAliasCount: sw_integer;
       function    GetAlias(Index: sw_integer): string;
       procedure   AddAlias(const Alias: string);
       constructor Load(var S: TStream);
       procedure   Store(var S: TStream);
       destructor  Done; virtual;
     private
       DocName: PString;
       Synonym: PHTMLLinkScanDocument;
       Aliases: PStringCollection;
     end;

     PHTMLLinkScanDocumentCollection = ^THTMLLinkScanDocumentCollection;
     THTMLLinkScanDocumentCollection = object(TSortedCollection)
       constructor Init(AScanner: PHTMLLinkScanner; ALimit, ADelta: Integer);
       function    Compare(Key1, Key2: Pointer): sw_Integer; virtual;
       function    At(Index: sw_Integer): PHTMLLinkScanDocument;
       function    SearchDocument(const DocName: string): PHTMLLinkScanDocument;
       procedure   MoveAliasesToSynonym;
     private
       Scanner: PHTMLLinkScanner;
     end;

     THTMLLinkScanner = object(TCustomHTMLLinkScanner)
       constructor Init(const ABaseDir: string);
       procedure   SetBaseDir(const ABaseDir: string);
       function    GetDocumentCount: sw_integer;
       function    GetDocumentURL(DocIndex: sw_integer): string;
       function    GetUniqueDocumentURL(DocIndex: sw_integer): string;
       function    GetDocumentAliasCount(DocIndex: sw_integer): sw_integer;
       function    GetDocumentAlias(DocIndex, AliasIndex: sw_integer): string;
       constructor LoadDocuments(var S: TStream);
       procedure   StoreDocuments(var S: TStream);
       destructor  Done; virtual;
     public
       procedure   AddLink(const LinkText, LinkURL: string); virtual;
     private
       Documents: PHTMLLinkScanDocumentCollection;
       BaseDir: PString;
       function    ExpandChildURL(const S: string): string;
       function    NormalizeChildURL(const S: string): string;
     end;

     THTMLLinkScanState = (ssScheduled,ssProcessing,ssScanned);

     PHTMLLinkScanFile = ^THTMLLinkScanFile;
     THTMLLinkScanFile = object(TObject)
       constructor Init(const ADocumentURL: string);
       function    GetDocumentURL: string;
       destructor  Done; virtual;
     private
       DocumentURL  : PString;
     public
       State        : THTMLLinkScanState;
     end;

     PHTMLLinkScanFileCollection = ^THTMLLinkScanFileCollection;
     THTMLLinkScanFileCollection = object(TSortedCollection)
       function At(Index: sw_Integer): PHTMLLinkScanFile;
       function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
       function SearchFile(const DocURL: string): PHTMLLinkScanFile;
       function FindFileWithState(AState: THTMLLinkScanState): PHTMLLinkScanFile;
     end;

     THTMLLinkScanOption = (soSubDocsOnly);
     THTMLLinkScanOptions = set of THTMLLinkScanOption;

     THTMLFileLinkScanner = object(THTMLLinkScanner)
       constructor Init(const ABaseDir: string);
       procedure   ProcessDocument(const DocumentURL: string; AOptions: THTMLLinkScanOptions);
       destructor  Done; virtual;
     public
       function    GetDocumentBaseURL: string; virtual;
       procedure   AddLink(const LinkText, LinkURL: string); virtual;
       function    CheckURL(const URL: string): boolean; virtual;
     private
       Options: THTMLLinkScanOptions;
       BaseURL: string;
       CurBaseURL: string;
       DocumentFiles: PHTMLLinkScanFileCollection;
       procedure   ScheduleDoc(const DocumentURL: string);
     public
       procedure   ProcessDoc(Doc: PHTMLLinkScanFile); virtual;
     end;

procedure RegisterWHTMLScan;

implementation

uses WUtils;

const
  RHTMLLinkScanDocument: TStreamRec = (
     ObjType: 19500;
     VmtLink: Ofs(TypeOf(THTMLLinkScanDocument)^);
     Load:    @THTMLLinkScanDocument.Load;
     Store:   @THTMLLinkScanDocument.Store
  );

const
  CurrentHTMLIndexVersion : sw_integer = HTMLIndexVersion;

function TCustomHTMLLinkScanner.DocAddTextChar(C: char): boolean;
var Added: boolean;
begin
  Added:=false;
  if InAnchor then
  begin
    CurLinkText:=CurLinkText+C;
    Added:=true;
  end;
  if ord(c)>32 then
    LastSynonym:=nil;
  DocAddTextChar:=Added;
end;

procedure TCustomHTMLLinkScanner.DocAnchor(Entered: boolean);
begin
  if Entered then
    begin
      CurLinkText:='';
      if DocGetTagParam('HREF',CurURL)=false then
      if DocGetTagParam('NAME',CurURL) then
        begin
          InNameAnchor:=true;
          If Pos('#',CurURL)=0 then
            CurURL:=CurDoc+'#'+CurURL;
        end
      else
        CurURL:='';
      CurURL:=Trim(CurURL);
      CurURL:=CompleteURL(GetDocumentBaseURL,CurURL);
    end
  else
    begin
      CurLinkText:=Trim(CurLinkText);
      if CheckURL(CurURL) and CheckText(CurLinkText) or InNameAnchor then
        AddLink(CurLinkText,CurURL);
      InNameAnchor:=false;
    end;
  InAnchor:=Entered;
end;

function TCustomHTMLLinkScanner.GetDocumentBaseURL: string;
begin
  { Abstract }
  GetDocumentBaseURL:='';
end;

function TCustomHTMLLinkScanner.CheckURL(const URL: string): boolean;
begin
  { Abstract }
  CheckURL:=true;
end;

function TCustomHTMLLinkScanner.CheckText(const Text: string): boolean;
begin
  { Abstract }
  CheckText:=true;
end;

procedure TCustomHTMLLinkScanner.AddLink(const LinkText, LinkURL: string);
begin
  { Abstract }
end;

constructor THTMLLinkScanDocument.Init(const ADocName: string);
begin
  inherited Init;
  SetStr(DocName,ADocName);
  New(Aliases, Init(10,10));
  Synonym:=nil;
end;

function THTMLLinkScanDocument.GetName: string;
begin
  GetName:=GetStr(DocName);
end;

function THTMLLinkScanDocument.GetUniqueName: string;
var
  PD: PHTMLLinkScanDocument;
begin
  PD:=@Self;
  while assigned(PD^.synonym) do
    PD:=PD^.Synonym;
  GetUniqueName:=GetStr(PD^.DocName);
end;


function THTMLLinkScanDocument.GetAliasCount: sw_integer;
begin
  GetAliasCount:=Aliases^.Count;
end;

function THTMLLinkScanDocument.GetAlias(Index: sw_integer): string;
begin
  GetAlias:=GetStr(Aliases^.At(Index));
end;

procedure THTMLLinkScanDocument.AddAlias(const Alias: string);
begin
  Aliases^.Insert(NewStr(Alias));
end;

constructor THTMLLinkScanDocument.Load(var S: TStream);
var
  i: sw_integer;
begin
  inherited Init;
  DocName:=S.ReadStr;
  if assigned(DocName) then
    for i:=1 to Length(DocName^) do
      if (DocName^[i]='\') or  (DocName^[i]='/') then
        DocName^[i]:=DirSep;
  New(Aliases, Load(S));
end;

procedure THTMLLinkScanDocument.Store(var S: TStream);
begin
  S.WriteStr(DocName);
  Aliases^.Store(S);
end;

destructor THTMLLinkScanDocument.Done;
begin
  inherited Done;
  if Assigned(Aliases) then Dispose(Aliases, Done); Aliases:=nil;
  if Assigned(DocName) then DisposeStr(DocName); DocName:=nil;
end;

constructor THTMLLinkScanDocumentCollection.Init(AScanner: PHTMLLinkScanner; ALimit, ADelta: Integer);
begin
  inherited Init(ALimit,ADelta);
  Scanner:=AScanner;
end;

function THTMLLinkScanDocumentCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var R: sw_integer;
    K1: PHTMLLinkScanDocument absolute Key1;
    K2: PHTMLLinkScanDocument absolute Key2;
    S1,S2: string;
begin
  S1:=K1^.GetName; S2:=K2^.GetName;
  if Assigned(Scanner) then
   begin S1:=Scanner^.ExpandChildURL(S1); S2:=Scanner^.ExpandChildURL(S2); end;
  S1:=UpcaseStr(S1); S2:=UpcaseStr(S2);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:= 1 else
  R:=0;
  Compare:=R;
end;

function THTMLLinkScanDocumentCollection.At(Index: sw_Integer): PHTMLLinkScanDocument;
begin
  At:=inherited At(Index);
end;

function THTMLLinkScanDocumentCollection.SearchDocument(const DocName: string): PHTMLLinkScanDocument;
var D,P: PHTMLLinkScanDocument;
    Index: sw_integer;
begin
  New(D, Init(DocName));
  if Search(D, Index)=false then P:=nil else
    P:=At(Index);
  Dispose(D, Done);
  SearchDocument:=P;
end;

procedure THTMLLinkScanDocumentCollection.MoveAliasesToSynonym;
  procedure MoveAliases(P: PHTMLLinkScanDocument);
  var
    PD: PHTMLLinkScanDocument;
    i: sw_integer;
  begin
    if not assigned(P^.synonym) then
      exit;
    PD:=P;
    while assigned(PD^.synonym) do
      PD:=PD^.Synonym;

    For i:=P^.GetAliasCount-1 downto 0 do
      begin
        PD^.AddAlias(P^.GetAlias(i));
        P^.Aliases^.AtFree(i);
      end;
  end;
begin
  ForEach(@MoveAliases);
end;

constructor THTMLLinkScanner.Init(const ABaseDir: string);
begin
  inherited Init;
  New(Documents, Init(@Self,50,100));
  SetBaseDir(ABaseDir);
end;

procedure THTMLLinkScanner.SetBaseDir(const ABaseDir: string);
begin
  if Assigned(BaseDir) then DisposeStr(BaseDir);
  BaseDir:=NewStr(CompleteDir(ABaseDir));
end;

function THTMLLinkScanner.GetDocumentCount: sw_integer;
begin
  GetDocumentCount:=Documents^.Count;
end;

function THTMLLinkScanner.ExpandChildURL(const S: string): string;
begin
  ExpandChildURL:=CompleteURL(GetStr(BaseDir),S);
end;

function THTMLLinkScanner.NormalizeChildURL(const S: string): string;
var URL: string;
begin
  URL:=S;
  if GetStr(BaseDir)<>'' then
   if copy(UpcaseStr(S),1,length(GetStr(BaseDir)))=UpcaseStr(GetStr(BaseDir)) then
     URL:=copy(S,length(GetStr(BaseDir))+1,length(S));
  NormalizeChildURL:=URL;
end;

function THTMLLinkScanner.GetDocumentURL(DocIndex: sw_integer): string;
begin
  GetDocumentURL:=ExpandChildURL(Documents^.At(DocIndex)^.GetName);
end;

function THTMLLinkScanner.GetUniqueDocumentURL(DocIndex: sw_integer): string;
begin
  GetUniqueDocumentURL:=ExpandChildURL(Documents^.At(DocIndex)^.GetUniqueName);
end;

function THTMLLinkScanner.GetDocumentAliasCount(DocIndex: sw_integer): sw_integer;
begin
  GetDocumentAliasCount:=Documents^.At(DocIndex)^.GetAliasCount;
end;

function THTMLLinkScanner.GetDocumentAlias(DocIndex, AliasIndex: sw_integer): string;
begin
  GetDocumentAlias:=Documents^.At(DocIndex)^.GetAlias(AliasIndex);
end;

procedure THTMLLinkScanner.AddLink(const LinkText, LinkURL: string);
var D: PHTMLLinkScanDocument;
    DoInsert: boolean;
    int: sw_integer;
    Text: string;
    error: word;
begin
  D:=Documents^.SearchDocument(LinkURL);
  if D=nil then
  begin
    New(D, Init(NormalizeChildURL(LinkURL)));
    Documents^.Insert(D);
  end;
  If assigned(LastSynonym) then
    LastSynonym^.Synonym:=D;
  DoInsert:=true;
  If (length(LinkText)=0) or (Pos(',',LinkText)=1) then
    DoInsert:=false;
  Val(LinkText,int,error);
  If (Error>1) and (LinkText[Error]=' ') then
    Text:=Trim(Copy(LinkText,error+1,length(LinkText)))
  else
    Text:=LinkText;
  IF DoInsert then
    D^.AddAlias(Text);
  If InNameAnchor then
    LastSynonym:=D;
end;

constructor THTMLLinkScanner.LoadDocuments(var S: TStream);
var P,L: longint;
    OK: boolean;
    PS: PString;
begin
  OK:=false;
  P:=S.GetPos;
  S.Read(L,sizeof(L));
  if (S.Status=stOK) and (L=HTMLIndexMagicNo) then
  begin
    S.Read(L,sizeof(L));
    CurrentHTMLIndexVersion:=L;
    OK:=(S.Status=stOK);
  end;
  if not OK then
    begin
      S.Reset;
      S.Seek(P);
    end
  else
    BaseDir:=S.ReadStr;
  New(Documents, Load(S));
  if not Assigned(Documents) then
    Fail;
  Documents^.MoveAliasesToSynonym;
  CurrentHTMLIndexVersion:=HTMLIndexVersion;
end;

procedure THTMLLinkScanner.StoreDocuments(var S: TStream);
var L: longint;
begin
  L:=HTMLIndexMagicNo;
  S.Write(L,sizeof(L));
  L:=HTMLIndexVersion;
  CurrentHTMLIndexVersion:=L;
  S.Write(L,sizeof(L));
  S.WriteStr(BaseDir);
  Documents^.MoveAliasesToSynonym;
  Documents^.Store(S);
end;

destructor THTMLLinkScanner.Done;
begin
  inherited Done;
  if Assigned(Documents) then Dispose(Documents, Done); Documents:=nil;
  if Assigned(BaseDir) then DisposeStr(BaseDir); BaseDir:=nil;
end;

constructor THTMLLinkScanFile.Init(const ADocumentURL: string);
begin
  inherited Init;
  SetStr(DocumentURL,ADocumentURL);
end;

function THTMLLinkScanFile.GetDocumentURL: string;
begin
  GetDocumentURL:=GetStr(DocumentURL);
end;

destructor THTMLLinkScanFile.Done;
begin
  inherited Done;
  if Assigned(DocumentURL) then DisposeStr(DocumentURL); DocumentURL:=nil;
end;

function THTMLLinkScanFileCollection.At(Index: sw_Integer): PHTMLLinkScanFile;
begin
  At:=inherited At(Index);
end;

function THTMLLinkScanFileCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var R: integer;
    K1: PHTMLLinkScanFile absolute Key1;
    K2: PHTMLLinkScanFile absolute Key2;
    S1,S2: string;
begin
  S1:=UpcaseStr(K1^.GetDocumentURL); S2:=UpcaseStr(K2^.GetDocumentURL);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:= 1 else
  R:=0;
  Compare:=R;
end;

function THTMLLinkScanFileCollection.SearchFile(const DocURL: string): PHTMLLinkScanFile;
var P,D: PHTMLLinkScanFile;
    Index: sw_integer;
begin
  New(D, Init(DocURL));
  if Search(D,Index)=false then P:=nil else
    P:=At(Index);
  Dispose(D, Done);
  SearchFile:=P;
end;

function THTMLLinkScanFileCollection.FindFileWithState(AState: THTMLLinkScanState): PHTMLLinkScanFile;
var I: sw_integer;
    P,D: PHTMLLinkScanFile;
begin
  P:=nil;
  for I:=0 to Count-1 do
  begin
    D:=At(I);
    if D^.State=AState then
      begin
        P:=D;
        Break;
      end;
  end;
  FindFileWithState:=P;
end;

constructor THTMLFileLinkScanner.Init(const ABaseDir: string);
begin
  inherited Init(ABaseDir);
  New(DocumentFiles, Init(50,100));
end;

procedure THTMLFileLinkScanner.ProcessDocument(const DocumentURL: string; AOptions: THTMLLinkScanOptions);
var P: PHTMLLinkScanFile;
begin
  CurBaseURL:=''; Options:=AOptions;
  ScheduleDoc(DocumentURL);
  repeat
    P:=DocumentFiles^.FindFileWithState(ssScheduled);
    if Assigned(P) then
      ProcessDoc(P);
  until P=nil;
end;

function THTMLFileLinkScanner.GetDocumentBaseURL: string;
begin
  GetDocumentBaseURL:=CurBaseURL;
end;

function THTMLFileLinkScanner.CheckURL(const URL: string): boolean;
var OK: boolean;
begin
  if soSubDocsOnly in Options then
    OK:=UpcaseStr(copy(URL,1,length(BaseURL)))=UpcaseStr(BaseURL)
  else
    OK:=true;
  CheckURL:=OK;
end;

procedure THTMLFileLinkScanner.AddLink(const LinkText, LinkURL: string);
var D: PHTMLLinkScanFile;
    P: sw_integer;
    DocURL: string;
begin
  P:=Pos('#',LinkURL);
  if P=0 then DocURL:=LinkURL else DocURL:=copy(LinkURL,1,P-1);
  D:=DocumentFiles^.SearchFile(DocURL);
  if Assigned(D)=false then
      ScheduleDoc(DocURL);
  inherited AddLink(LinkText,LinkURL);
end;

procedure THTMLFileLinkScanner.ProcessDoc(Doc: PHTMLLinkScanFile);
var F: PDOSTextFile;
begin
  if Assigned(Doc)=false then Exit;

  Doc^.State:=ssProcessing;
  CurDoc:=Doc^.GetDocumentURL;
  New(F, Init(Doc^.GetDocumentURL));
  if Assigned(F) then
  begin
    CurBaseURL:=CompleteURL(Doc^.GetDocumentURL,'');
    Process(F);
    Dispose(F, Done);
  end;
  Doc^.State:=ssScanned;
  CurDoc:='';
end;

procedure THTMLFileLinkScanner.ScheduleDoc(const DocumentURL: string);
var D: PHTMLLinkScanFile;
begin
  New(D, Init(DocumentURL));
  D^.State:=ssScheduled;
  DocumentFiles^.Insert(D);
end;

destructor THTMLFileLinkScanner.Done;
begin
  inherited Done;
  if Assigned(DocumentFiles) then Dispose(DocumentFiles, Done); DocumentFiles:=nil;
end;

procedure RegisterWHTMLScan;
begin
  RegisterType(RHTMLLinkScanDocument);
end;


END.
