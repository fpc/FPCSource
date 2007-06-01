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
    {a}procedure   AddRef(LinkURL: string); virtual;
    {a}procedure   AddNameID(AName: string); virtual;
    {a}procedure   AddID(AName: string); virtual;
    {a}function    GetDocumentBaseURL: string; virtual;
     private
       CurLinkText: string;
       CurURL: string;
       CurName,
       CurID: string;
       CurDoc: string;
       InAnchor,InNameAnchor,
       HasHRef : boolean;
       LastSynonym: PHTMLLinkScanDocument;
     end;

     TNameIDState = (IsReferenced, IsFound,IsID);
     TNameIDStates = set of TNameIDState;


     PNameID  = ^TNameID;
     TNameID  = object(TObject)
       constructor Init(const AName : string; Astate : TNameIDState);
       destructor  Done; virtual;
       procedure SetState(Astate : TNameIDState; enabled : boolean);
       procedure SetOrigin(const AOrigin : string);
       procedure SetLine(ALine : sw_integer);
       function GetLine : sw_integer;
       function GetState : TNameIDStates;
       function GetName : string;
       function GetOrigin : string;
     private
       Name : pstring;
       Origin : pstring;
       Line : sw_integer;
       State : TNameIDStates;
     end;

     PNameIDCollection = ^TNameIDCollection;
     TNameIDCollection = object(TSortedCollection)
       function At(Index: sw_Integer): PNameID;
       function Compare(Key1, Key2: Pointer): sw_Integer; virtual;
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
    {a}function    FindID(const AName : string) : PNameID; virtual;
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
       Documents:  PHTMLLinkScanDocumentCollection;
       BaseDir:    PString;
       function    ExpandChildURL(const S: string): string;
       function    NormalizeChildURL(const S: string): string;
     end;

     THTMLLinkScanState = (ssScheduled,ssProcessing,ssScanned);

     PHTMLLinkScanFile = ^THTMLLinkScanFile;
     THTMLLinkScanFile = object(TObject)
       constructor Init(const ADocumentURL: string);
       function    GetDocumentURL: string;
       destructor  Done; virtual;
       function    AddReferencedName (const AName : string) : PNameID;
       function    AddFoundName (const AName : string) : PNameID;
       procedure   CheckNameList;
       function    FindID(const AName : string) : PNameID; virtual;
     private
       DocumentURL  : PString;
       NameIDList   : PNameIDCollection;
       Owner        : PHTMLLinkScanner;
     public
       State        : THTMLLinkScanState;
     end;

     PHTMLLinkScanFileCollection = ^THTMLLinkScanFileCollection;
     THTMLLinkScanFileCollection = object(TSortedCollection)
       function   At(Index: sw_Integer): PHTMLLinkScanFile;
       function   Compare(Key1, Key2: Pointer): sw_Integer; virtual;
       function   SearchFile(const DocURL: string): PHTMLLinkScanFile;
       function   FindFileWithState(AState: THTMLLinkScanState): PHTMLLinkScanFile;
       procedure  CheckNameIDLists;
     end;

     THTMLLinkScanOption = (soSubDocsOnly);
     THTMLLinkScanOptions = set of THTMLLinkScanOption;

     THTMLFileLinkScanner = object(THTMLLinkScanner)
       constructor Init(const ABaseDir: string);
       procedure   ProcessDocument(const DocumentURL: string; AOptions: THTMLLinkScanOptions);
       destructor  Done; virtual;
     public
       function    GetDocumentBaseURL: string; virtual;
       function    FindID(const AName : string) : PNameID; virtual;
       procedure   AddLink(const LinkText, LinkURL: string); virtual;
       procedure   AddRef(LinkURL: string); virtual;
       procedure   AddNameID(AName: string); virtual;
       procedure   AddID(AName: string); virtual;
       function    CheckURL(const URL: string): boolean; virtual;
     private
       Options: THTMLLinkScanOptions;
       BaseURL: string;
       CurBaseURL: string;
       IDList   : PNameIDCollection;
       DocumentFiles: PHTMLLinkScanFileCollection;
       procedure   ScheduleDoc(const DocumentURL: string);
     public
       procedure   ProcessDoc(Doc: PHTMLLinkScanFile); virtual;
     end;

procedure RegisterWHTMLScan;

implementation

uses
  WUtils;

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
      if DocGetTagParam('HREF',CurURL) then
        HasHRef:=true
      else
        CurURL:='';
      if not DocGetTagParam('NAME',CurName) then
      if not DocGetTagParam('ID',CurName) then
        CurName:='';
      if not DocGetTagParam('ID',CurID) then
        CurID:='';
      if CurName<>'' then
        begin
          InNameAnchor:=true;
          If Pos('#',CurName)=0 then
            CurName:=CurDoc+'#'+CurName;
          CurName:=Trim(CurName);
          CurName:=CompleteURL(GetDocumentBaseURL,CurName);
          if CurURL='' then
            CurURL:=CurName;
        end
      else
        CurName:='';
      CurURL:=Trim(CurURL);
      if pos('#',CurURL)=1 then
        CurURL:=CurDoc+CurURL;
      CurURL:=CompleteURL(GetDocumentBaseURL,CurURL);
    end
  else
    begin
      CurLinkText:=Trim(CurLinkText);
      if HasHRef then
        begin
          if CheckURL(CurURL) and CheckText(CurLinkText) and
             not DisableCrossIndexing then
            begin
              AddLink(CurLinkText,CurURL);
    {$ifdef DEBUG}
              DebugMessage(CurDoc,' Adding ScanLink "'+CurLinkText+'" to "'+
                CurURL+'"',Line,1);
    {$endif DEBUG}
            end;
          { Be sure to parse referenced file,
            even if that link is not valid }
          AddRef(CurURL);
        end;
      if not HasHRef and InNameAnchor and CheckURL(CurName) and CheckText(CurLinkText) then
        begin
          AddLink(CurLinkText,CurName);
{$ifdef DEBUG}
          DebugMessage(CurDoc,' Adding ScanName "'+CurLinkText+'" to "'+CurName+'"',Line,1);
{$endif DEBUG}
        end;
      if InNameAnchor then
        begin
          AddNameID(CurName);
        end;
      if not HasHRef and (CurID<>'') then
        AddID(CurID);
      InNameAnchor:=false;
      HasHRef:=false;
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

procedure TCustomHTMLLinkScanner.AddRef(LinkURL: string);
begin
  { Abstract }
end;

procedure TCustomHTMLLinkScanner.AddNameID(AName: string);
begin
  { Abstract }
end;

procedure TCustomHTMLLinkScanner.AddID(AName: string);
begin
  { Abstract }
end;


constructor TNameID.Init(const AName : string; Astate : TNameIDState);
begin
  inherited Init;
  SetStr(Name,AName);
  Origin:=nil;
  State:=[AState];
end;

destructor  TNameID.Done;
begin
  if assigned(Name) then
    DisposeStr(Name);
  Name:=nil;
  if assigned(Origin) then
    DisposeStr(Origin);
  Origin:=nil;
  inherited Done;
end;

procedure TNameID.SetState(Astate : TNameIDState; enabled : boolean);
begin
  if enabled then
    Include(State,AState)
  else
    Exclude(State,AState);
end;


function TNameID.GetState : TNameIDStates;
begin
  GetState:=State;
end;

function TNameID.GetName : string;
begin
  GetName:=GetStr(Name);
end;

function TNameID.GetOrigin : string;
begin
  GetOrigin:=GetStr(Origin);
end;

procedure TNameID.SetOrigin(const AOrigin : string);
begin
  SetStr(Origin,AOrigin);
end;
procedure TNameID.SetLine(ALine : sw_integer);
begin
  Line:=ALine;
end;

function TNameID.GetLine : sw_integer;
begin
  GetLine:=Line;
end;


function TNameIDCollection.At(Index: sw_Integer): PNameID;
begin
  At:=Inherited At(Index);
end;

function TNameIDCollection.Compare(Key1, Key2: Pointer): sw_Integer;
var
  R: sw_integer;
  K1: PNameID absolute Key1;
  K2: PNameID absolute Key2;
  S1,S2: string;
begin
  S1:=K1^.GetName;
  S2:=K2^.GetName;
  S1:=UpcaseStr(S1); S2:=UpcaseStr(S2);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:= 1 else
  R:=0;
  Compare:=R;
end;


constructor THTMLLinkScanDocument.Init(const ADocName: string);
begin
  inherited Init;
  SetStr(DocName,ADocName);
  New(Aliases, Init(10,10));
{$ifdef DEBUG}
  DebugMessage('',' Adding New LinkScan document "'+ADocName+'"',1,1);
{$endif DEBUG}
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
{$ifdef DEBUG}
  DebugMessage('',' Adding alias "'+Alias+'" to LinkScan document "'+GetStr(DocName)+'"',1,1);
{$endif DEBUG}
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
  if Assigned(Aliases) then
    Dispose(Aliases, Done);
  Aliases:=nil;
  if Assigned(DocName) then
    DisposeStr(DocName);
  DocName:=nil;
  inherited Done;
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

function THTMLLinkScanner.FindID(const AName : string) : PNameID;
begin
  {abstract}FindID:=nil;
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
  if Assigned(Documents) then
    Dispose(Documents, Done);
  Documents:=nil;
  if Assigned(BaseDir) then
    DisposeStr(BaseDir);
  BaseDir:=nil;
  inherited Done;
end;

constructor THTMLLinkScanFile.Init(const ADocumentURL: string);
begin
  inherited Init;
  SetStr(DocumentURL,ADocumentURL);
  New(NameIDList, Init(5,10));
end;

function THTMLLinkScanFile.GetDocumentURL: string;
begin
  GetDocumentURL:=GetStr(DocumentURL);
end;

function THTMLLinkScanFile.AddReferencedName (const AName : string) : PNameID;
var
  index : sw_integer;
  PN : PNameID;
begin
  new(PN,init(AName,IsReferenced));
  if not NameIDList^.Search(PN,Index) then
    NameIDList^.Insert(PN)
  else
    begin
      dispose(PN,Done);
      PN:=NameIDList^.At(Index);
      PN^.SetState(IsReferenced,true);
    end;
  AddReferencedName:=PN;
end;

function THTMLLinkScanFile.AddFoundName (const AName : string) : PNameID;
var
  index : sw_integer;
  PN : PNameID;
begin
  new(PN,init(AName,IsFound));
  if not NameIDList^.Search(PN,Index) then
    NameIDList^.Insert(PN)
  else
    begin
      dispose(PN,Done);
      PN:=NameIDList^.At(Index);
      PN^.SetState(IsFound,true);
    end;
  AddFoundName:=PN;
end;

procedure THTMLLinkScanFile.CheckNameList;
var
  i : sw_integer;
  PN,PN2 : PNameID;
begin
{$ifdef DEBUG}
  for i:=0 to NameIDList^.Count-1 do
    begin
      PN:=NameIDList^.At(i);
      if not (IsFound in PN^.GetState) then
        begin
          if (IsReferenced in PN^.GetState) then
            DebugMessage(GetDocumentURL,'Name "'+PN^.GetName+'" from "'+
              PN^.GetOrigin+'" not found',1,1);
          PN2:=Owner^.FindID(PN^.GetName);
          if assigned(PN2) then
            begin
              DebugMessage('','ID found in "'+PN2^.GetOrigin+'"',1,1);
              if not (IsFound in PN2^.GetState) then
                DebugMessage('','ID not found',1,1);
            end;
        end;
    end;
{$endif DEBUG}
end;


function  THTMLLinkScanFile.FindID(const AName : string) : PNameID;
var
  PN : PNameID;
  Index : sw_integer;
begin
  new(PN,init(AName,IsID));
  if NameIDList^.Search(PN,Index) then
    begin
      dispose(PN,done);
      PN:=NameIDList^.At(Index);
      if (IsID in PN^.GetState) then
        FindId:=PN
      else
        FindID:=nil;
    end
  else
    begin
      dispose(PN,done);
      PN:=nil;
      FindID:=nil;
    end;

end;
destructor THTMLLinkScanFile.Done;
begin
  if Assigned(DocumentURL) then
    DisposeStr(DocumentURL);
  DocumentURL:=nil;
  dispose(NameIDList,done);
  NameIDList:=nil;
  inherited Done;
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

procedure THTMLLinkScanFileCollection.CheckNameIDLists;

  procedure DoCheckNameList(P : PHTMLLinkScanFile);
    begin
      P^.CheckNameList;
    end;

begin
  ForEach(@DoCheckNameList);
end;


constructor THTMLFileLinkScanner.Init(const ABaseDir: string);
begin
  inherited Init(ABaseDir);
  New(DocumentFiles, Init(50,100));
  New(IDList, Init(50,100));
{$ifdef DEBUG}
  DebugMessage('','THTMLFileLinkScanner Init "'+ABaseDir+'"',1,1);
{$endif DEBUG}
end;

procedure THTMLFileLinkScanner.ProcessDocument(const DocumentURL: string; AOptions: THTMLLinkScanOptions);
var P: PHTMLLinkScanFile;
begin
  CurBaseURL:='';
  Options:=AOptions;
  ScheduleDoc(DocumentURL);
  repeat
    P:=DocumentFiles^.FindFileWithState(ssScheduled);
    if Assigned(P) then
      ProcessDoc(P);
  until P=nil;
{$ifdef DEBUG}
  DebugMessage('','THTMLFileLinkScanner CheckNameList start ',1,1);
  DocumentFiles^.CheckNameIDLists;
  DebugMessage('','THTMLFileLinkScanner CheckNameList end ',1,1);
{$endif DEBUG}
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
  if not Assigned(D) then
      ScheduleDoc(DocURL);
  inherited AddLink(LinkText,LinkURL);
end;

procedure THTMLFileLinkScanner.AddRef(LinkURL: string);
var D: PHTMLLinkScanFile;
    P: sw_integer;
    DocURL: string;
    PN : PNameID;
begin
{$ifdef DEBUG}
  DebugMessage(CurDoc,' Adding Ref to "'+
    LinkURL+'"',Line,1);
{$endif DEBUG}
  P:=Pos('#',LinkURL);
  if P=0 then DocURL:=LinkURL else DocURL:=copy(LinkURL,1,P-1);
  D:=DocumentFiles^.SearchFile(DocURL);
  if not Assigned(D) then
      ScheduleDoc(DocURL);
  D:=DocumentFiles^.SearchFile(DocURL);
  if P>0 then
    begin
      PN:=D^.AddReferencedName(copy(LinkURL,P+1,length(LinkURL)));
      PN^.SetOrigin(CurDoc);
      PN^.SetLine(Line);
    end;
end;

procedure THTMLFileLinkScanner.AddNameID(AName : string);
var D: PHTMLLinkScanFile;
    P: sw_integer;
    PN : PNameID;
    DocURL: string;
begin
{$ifdef DEBUG}
  DebugMessage(CurDoc,' Adding NameID "'+
    CurName+'"',Line,1);
{$endif DEBUG}
  P:=Pos('#',AName);
  if P=0 then DocURL:=AName else DocURL:=copy(AName,1,P-1);
  D:=DocumentFiles^.SearchFile(DocURL);
  if not Assigned(D) then
      ScheduleDoc(DocURL);
  D:=DocumentFiles^.SearchFile(DocURL);
  PN:=D^.AddFoundName(copy(AName,P+1,length(AName)));
  PN^.SetOrigin(CurDoc);
  PN^.SetLine(Line);
end;

procedure THTMLFileLinkScanner.AddID(AName : string);
var
  D: PHTMLLinkScanFile;
  PN : PNameID;
  index : sw_integer;
begin
{$ifdef DEBUG}
  DebugMessage(CurDoc,' Adding Id "'+
    AName+'"',Line,1);
{$endif DEBUG}
  D:=DocumentFiles^.SearchFile(CurDoc);
  if not Assigned(D) then
      ScheduleDoc(CurDoc);
  D:=DocumentFiles^.SearchFile(CurDoc);
  PN:=D^.AddFoundName(AName);
  PN^.SetState(IsId,true);
  PN^.SetOrigin(CurDoc);
  PN^.SetLine(Line);

  new(PN,init(AName,IsID));
  if IDList^ .Search(PN,index) then
    begin
      dispose(PN,done);
{$ifdef DEBUG}
      PN:=IDList^.At(Index);
      DebugMessage(CurDoc,'ID "'+AName+'" already defined in "'+
        PN^.GetOrigin+'('+IntToStr(PN^.GetLine)+')"',Line,1);
{$endif DEBUG}
    end
  else
    begin
      IDList^.Insert(PN);
      PN^.SetOrigin(CurDoc);
      PN^.SetLine(Line);
    end;
end;

function THTMLFileLinkScanner.FindID(const AName : string) : PNameID;

  Function ContainsNamedID(D : PHTMLLinkScanFile) : boolean;
    begin
      ContainsNamedID:=D^.FindID(AName)<>nil;
    end;
var
  D : PHTMLLinkScanFile;
begin
  D:=DocumentFiles^.FirstThat(@ContainsNamedID);
  if assigned(D) then
    FindID:=D^.FindID(AName)
  else
    FindID:=nil;
end;

procedure THTMLFileLinkScanner.ProcessDoc(Doc: PHTMLLinkScanFile);
var F: PDOSTextFile;
begin
  if Assigned(Doc)=false then Exit;

  Doc^.State:=ssProcessing;
  CurDoc:=Doc^.GetDocumentURL;
  New(F, Init(CurDoc));
  if Assigned(F) then
    begin
      CurBaseURL:=CompleteURL(CurDoc,'');
{$ifdef DEBUG}
      DebugMessage(CurDoc,'Processing "'+CurDoc+'"',1,1);
{$endif DEBUG}
      Process(F);
{$ifdef DEBUG}
      DebugMessage(CurDoc,'Finished processing "'+CurDoc+'"',Line,1);
{$endif DEBUG}
      Dispose(F, Done);
    end
  else
    begin
{$ifdef DEBUG}
      DebugMessage(CurDoc,'file not found',1,1);
{$endif DEBUG}
    end;
  Doc^.State:=ssScanned;
  CurDoc:='';
end;

procedure THTMLFileLinkScanner.ScheduleDoc(const DocumentURL: string);
var D: PHTMLLinkScanFile;
begin
  New(D, Init(DocumentURL));
  D^.State:=ssScheduled;
  D^.Owner:=@Self;
{$ifdef DEBUG}
      DebugMessage('','Scheduling file "'+DocumentURL+'"',1,1);
{$endif DEBUG}
  DocumentFiles^.Insert(D);
end;

destructor THTMLFileLinkScanner.Done;
begin
  if Assigned(DocumentFiles) then
    Dispose(DocumentFiles, Done);
  DocumentFiles:=nil;
  if Assigned(IDList) then
    Dispose(IDList, Done);
  IDList:=nil;
  inherited Done;
end;

procedure RegisterWHTMLScan;
begin
  RegisterType(RHTMLLinkScanDocument);
end;


END.
