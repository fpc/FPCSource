{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    Memory database
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit memindexdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpindexer, contnrs;

Type
  TMatch = Class;

  { TDescrItem }

  TDescrItem = Class(TCollectionItem)
  private
    FDescription: String;
    FTheID: Integer;
  Protected
    Function BlockSize : Integer; virtual;
    Procedure WriteStringToStream(Astream : TStream; S : String);
    Procedure WriteToStream(S : TStream); virtual;
    Procedure WriteRefToStream(AStream : Tstream; AItem : TDescrItem);
    Function ReadStringFromStream(Astream : TStream) :  String;
    Function ReadFromStream(S : TStream) : Integer; virtual;
  Published
    // The description
    Property Description : String Read FDescription Write FDescription;
    // ID. Not used during work. Only used when loading/saving,
    // it then equals the Index. (index is slow, uses a linear search)
    Property TheID : Integer Read FTheID Write FTheID;
  end;

  { TDescrCollection }

  TDescrCollection = Class(TCollection)
  private
    FHash : TFPhashList;
    FLoadCount : Integer;
    function GetD(AIndex : Integer): TDescrItem;
    procedure SetD(AIndex : Integer; AValue: TDescrItem);
  Protected
    Procedure RebuildHash;
    Function Resolve(AIndex : Integer) : TDescrItem;
  Public
    destructor destroy; override;
    Procedure BeginLoading;
    Procedure EndLoading;
    Procedure AllocateIDS;
    Function FindDescr(Const ADescription : String) : TDescrItem;
    Function AddDescr(Const ADescription : String) : TDescrItem;
    Property Descr [AIndex : Integer] : TDescrItem Read GetD Write SetD; default;
  end;

  TLanguageItem = Class(TDescrItem);

  TMatchedItem = Class(TDescrItem)
  Private
    FList : TFPList;
    function GetMatch(AIndex : Integer): TMatch;
    function GetMatchCount: Integer;
  Protected
    Function AddMatch(AMatch : TMatch) : Integer;
    Procedure RemoveMatch(AMatch : TMatch);
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Property Matches [AIndex : Integer] : TMatch Read GetMatch; default;
    Property MatchCount :  Integer Read GetMatchCount;
  end;

  { TWordItem }

  TWordItem = Class(TMatchedItem);

  { TURLItem }

  TURLItem = Class(TMatchedItem)
  private
    FLanguage: TLanguageItem;
    FURLDate: TDateTime;
    FLangID : Integer;
  protected
    Function BlockSize : Integer; override;
    Procedure WriteToStream(S : TStream); override;
    Function ReadFromStream(S : TStream) : Integer; override;
    Procedure Resolve(Languages: TDescrCollection);
  Public
    Property URLDate : TDateTime Read FURLDate Write FURLDate;
    Property Language : TLanguageItem Read FLanguage Write FLanguage;
  end;



  { TMatch }

  TMatch = Class(TDescrItem)
  private
    FPosition: Int64;
    FURL: TURLItem;
    FWord: TWordItem;
    FWordID : Integer;
    FURLID : Integer;
    function GetContext: String;
    procedure SetContext(AValue: String);
    procedure SetURL(AValue: TURLItem);
    procedure SetWord(AValue: TWordItem);
  protected
    Function BlockSize : Integer; override;
    Procedure WriteToStream(S : TStream); override;
    Procedure Resolve(Words, URLS : TDescrCollection);
    Function ReadFromStream(S : TStream) : Integer; override;
  Public
    Property Word : TWordItem Read FWord Write SetWord;
    Property URL : TURLItem Read FURL Write SetURL;
    Property Position : Int64 Read FPosition Write FPosition;
    Property Context : String Read GetContext Write SetContext;
  end;

  { TMatches }

  TMatches = Class(TCollection)
  private
    function GetM(AIndex : Integer): TMatch;
    procedure SetM(AIndex : Integer; AValue: TMatch);
  Public
    Function AddMatch(AWord : TWordItem; AURL : TURLItem) : TMatch;
    Property Matches[AIndex : Integer] : TMatch Read GetM Write SetM; default;
  end;

  { TMemIndexDB }

  TMemIndexDB = class(TCustomIndexDB)
  Private
    FStream: TStream;
    FURLS : TDescrCollection;
    FLanguages : TDescrCollection;
    FWords : TDescrCollection;
    FMatches : TMatches;
    procedure GetMatches(AWord: String; SearchOptions: TSearchOptions;  AList: TFPList);
    procedure IntersectMatches(ListA, ListB: TFPList);
    procedure UnionMatches(ListA, ListB: TFPList);
  protected
    Procedure LoadFromStream; virtual; abstract;
    Procedure SaveToStream; virtual; abstract;
    procedure Clear;virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure Connect; override;
    procedure DisConnect; override;
    procedure CommitTrans; override;
    procedure BeginTrans; override;
    procedure CompactDB; override;
    procedure DeleteWordsFromFile(URL: string); override;
    procedure AddSearchData(ASearchData: TSearchWordData); override;
    procedure FindSearchData(SearchWord: TWordParser; FPSearch: TFPSearch; SearchOptions: TSearchOptions); override;
    procedure CreateIndexerTables; override;
    Property Stream : TStream Read FStream Write FStream;
  end;

  { TFileIndexDB }

  TFileIndexDB = Class(TMemIndexDB)
  private
    FFIleName: String;
    FWriteOnCommit: Boolean;
  Protected
    Procedure LoadFromStream; override;
    Procedure SaveToStream; override;
  Public
    procedure Connect; override;
    procedure DisConnect; override;
    procedure CommitTrans; override;
    Property FileName : String Read FFIleName Write FFileName;
    Property WriteOnCommit : Boolean Read FWriteOnCommit Write FWriteOnCommit;
  end;

implementation

uses bufstream;

{ TMemIndexDB }

Resourcestring
  SErrNoStream = 'No stream assigned';
  SInvalidStreamData = 'Invalid data at offset %d. Got %d, expected %d.';

{ TFileIndexDB }

Const
  FileVersion    = 1;
  LanguageBlock  = 1;
  URLBlock       = 2;
  WordBlock      = 3;
  MatchBlock     = 4;

{ TURLItem }

function TURLItem.BlockSize: Integer;
begin
  Result:=inherited BlockSize;
  Result:=Result+sizeOf(FURLDate)+SizeOf(Integer);
end;

procedure TURLItem.WriteToStream(S: TStream);

Var
  I : Integer;

begin
  inherited WriteToStream(S);
  S.WriteBuffer(FURLDate,SizeOf(FURLDate));
  WriteRefToStream(S,FLanguage);
end;

function TURLItem.ReadFromStream(S: TStream): Integer;
begin
  Result:=inherited ReadFromStream(S);
  S.ReadBuffer(FURLDate,SizeOf(FURLDate));
  S.ReadBuffer(FLangID,SizeOf(FLangID));
end;

procedure TURLItem.Resolve(Languages: TDescrCollection);
begin
  FLanguage:=TLanguageItem(Languages.Resolve(FLangID));
end;

{ TDescrItem }

function TDescrItem.BlockSize: Integer;
begin
  Result:=Sizeof(Integer)+Length(FDescription)*SizeOf(Char);
end;

procedure TDescrItem.WriteStringToStream(Astream: TStream; S: String);
Var
  L : Integer;
begin
  L:=Length(S);
  AStream.WriteBuffer(L,SizeOf(L));
  if (L>0) then
    AStream.WriteBuffer(S[1],L*SizeOf(Char));
end;

procedure TDescrItem.WriteToStream(S: TStream);

begin
  S.WriteDWord(BlockSize);
  WriteStringToStream(S,FDescription);
end;

procedure TDescrItem.WriteRefToStream(AStream : Tstream; AItem: TDescrItem);

Var
  I : Integer;

begin
  If AItem=Nil then
    I:=0
  else
    I:=AItem.TheID;
  AStream.WriteBuffer(I,SizeOf(I));
end;

function TDescrItem.ReadStringFromStream(Astream: TStream): String;
Var
  L : Integer;
begin
  AStream.ReadBuffer(L,SizeOf(L));
  SetLength(Result,L);
  if (L>0) then
    AStream.ReadBuffer(Pointer(Result)^,L*SizeOf(Char));
end;

function TDescrItem.ReadFromStream(S: TStream) : Integer;

begin
  S.ReadBuffer(Result,SizeOf(Result));
  Description:=ReadStringFromStream(S);
end;


procedure TFileIndexDB.LoadFromStream;
Var
  I,S,L : Integer;
  U : TURLItem;
  W : TWordItem;
  M : TMatch;
  Li : TLanguageItem;

begin
  Clear;
  L:=Stream.ReadDWord;
  if (L<>FileVersion) then
     Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,L,FileVersion]);
  L:=Stream.ReadDWord;
  if (L<>LanguageBlock) then
    Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,L,LanguageBlock]);
  L:=Stream.ReadDWord;
  FLanguages.BeginLoading;
  For I:=0 to L-1 do
    begin
    Li:=TLanguageItem(FLanguages.Add);
    S:=Li.ReadFromStream(Stream);
    if (S<>Li.BlockSize) then
      Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,S,Li.BlockSize]);
    end;
  FLanguages.EndLoading;
  L:=Stream.ReadDWord;
  if (L<>URLBlock) then
    Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,L,URLBlock]);
  L:=Stream.ReadDWord;
  FURLS.BeginLoading;
  For I:=0 to L-1 do
    begin
    U:=TURLItem(FURLS.AddDescr(''));
    S:=U.ReadFromStream(Stream);
    if (S<>U.BlockSize) then
      Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,S,U.BlockSize]);
    U.Resolve(FLanguages);
    end;
  FURLS.EndLoading;
  L:=Stream.ReadDWord;
  if (L<>WordBlock) then
    Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,L,WordBlock]);
  L:=Stream.ReadDWord;
  FWords.BeginLoading;
  For I:=0 to L-1 do
    begin
    W:=TWordItem(FWords.AddDescr(''));
    S:=W.ReadFromStream(Stream);
    if (S<>W.BlockSize) then
      Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,S,W.BlockSize]);
    end;
  FWords.EndLoading;
  L:=Stream.ReadDWord;
  if (L<>MatchBlock) then
    Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,L,MatchBlock]);
  L:=Stream.ReadDWord;
  For I:=0 to L-1 do
    begin
    M:=TMatch(FMatches.Add);
    S:=M.ReadFromStream(Stream);
    M.Resolve(FWords,FURLS);
    if (S<>M.BlockSize) then
      Raise EFPIndexEr.CreateFmt(SInvalidStreamData,[Stream.Position,S,M.BlockSize]);
    end;
end;

procedure TFileIndexDB.SaveToStream;

Var
  I : Integer;
  L : Integer;
  U : TURLItem;

begin
  Stream.WriteDWord(FileVersion);
  Stream.WriteDWord(LanguageBlock);
  Stream.WriteDWord(FLanguages.Count);
  For I:=0 to FLanguages.Count-1 do
    FLanguages[i].WriteToStream(Stream);
  Stream.WriteDWord(URLBlock);
  Stream.WriteDWord(FURLS.Count);
  For I:=0 to FURLS.Count-1 do
    FURLS[i].WriteToStream(Stream);
  Stream.WriteDWord(WordBlock);
  Stream.WriteDWord(FWords.Count);
  For I:=0 to FWords.Count-1 do
    FWords[i].WriteToStream(Stream);
  Stream.WriteDWord(MatchBlock);
  Stream.WriteDWord(FMatches.Count);
  For I:=0 to FMatches.Count-1 do
    FMatches[i].WriteToStream(Stream);
end;

procedure TFileIndexDB.Connect;

Var
  F : TFileStream;
  B : TReadBufStream;

begin
  B:=Nil;
  F:=Nil;
  if FileExists(FileName) then
    begin
    F:=TFileStream.Create(FileName,fmOpenRead);
    B:=TReadBufStream.Create(F,1000000);
    B.SourceOwner:=True;
    end;
  try
    Stream:=B;
    inherited Connect;
  finally
    Stream.Free;
  end;
end;

procedure TFileIndexDB.DisConnect;
Var
  F : TFileStream;
  B : TWriteBufStream;
begin
  F:=TFileStream.Create(FileName,fmCreate);
  B:=TWriteBufStream.Create(F,1000000);
  B.SourceOwner:=True;
  try
    Stream:=B;
    inherited DisConnect;
  finally
    Stream.Free;
    Stream:=Nil;
  end;
end;

procedure TFileIndexDB.CommitTrans;
begin
  If WriteOnCommit and (FileName<>'') then
    Disconnect;
end;

procedure TMemIndexDB.CompactDB;
begin
  // Do nothing
end;

procedure TMemIndexDB.BeginTrans;
begin
  // Do nothing
end;

procedure TMemIndexDB.CommitTrans;
begin
  // Do nothing
end;

procedure TMemIndexDB.Clear;
begin
  FMatches.Clear;
  FWords.Clear;
  FURLS.Clear;
  FLanguages.Clear;
end;

constructor TMemIndexDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FURLS:=TDescrCollection.Create(TURLItem);
  FWords:=TDescrCollection.Create(TWordItem);
  FLanguages:=TDescrCollection.Create(TLanguageItem);
  FMatches:=TMatches.Create(TMatch);
end;

destructor TMemIndexDB.Destroy;
begin
  Clear;
  FreeAndNil(FMatches);
  FreeAndNil(FWords);
  FreeAndNil(FURLS);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TMemIndexDB.Connect;
begin
  if Assigned(Stream) then
    LoadFromStream;
end;

procedure TMemIndexDB.DisConnect;
begin
  if Assigned(Stream) then
    begin
    FLanguages.AllocateIDs;
    FURLS.AllocateIDs;
    FWords.AllocateIDs;
    SaveToStream
    end;
end;

procedure TMemIndexDB.DeleteWordsFromFile(URL: string);
begin
 // inherited DeleteWordsFromFile(URL);
end;

procedure TMemIndexDB.AddSearchData(ASearchData: TSearchWordData);

Var
  AURL : TURLItem;
  AWord : TWordItem;
  M : TMatch;
  L : TLanguageItem;

begin
//  Writeln('Adding search data : ',ASearchData.URL,' : ',ASearchData.SearchWord);

  AURL:=TURLItem(FURLs.FindDescr(ASearchData.URL));
  If (AURL=Nil) then
    begin
    L:=TLanguageItem(FLanguages.FindDescr(ASearchData.Language));
    If (L=Nil) then
      begin
//      Writeln('adding language : ',ASearchData.Language);
      L:=TLanguageItem(FLanguages.AddDescr(ASearchData.Language));
      end;
//    Writeln('adding URL : ',ASearchData.URL);
    AURL:=TURLItem(FURLS.AddDescr(ASearchData.URL));
    AURL.URLDate:=ASearchData.FileDate;
    AURL.Language:=L;
    end;
  AWord:=TWordItem(FWords.FindDescr(ASearchData.SearchWord));
  If (AWord=Nil) then
    begin
//    Writeln('adding Word : ',ASearchData.SearchWord);
    AWord:=TWordItem(FWords.AddDescr(ASearchData.SearchWord));
    end;
//  Writeln('Adding match : ',ASearchData.Position, ' ',ASearchData.Context);
  M:=FMatches.AddMatch(AWord,AURL);
  M.Position:=ASearchData.Position;
  M.Context:=ASearchData.Context;
end;

procedure TMemIndexDB.UnionMatches(ListA,ListB : TFPList);

begin
  ListA.AddList(ListB);
end;

procedure TMemIndexDB.IntersectMatches(ListA,ListB : TFPList);

Var
  L : TFPList;
  URL : TURLItem;
  I,J : Integer;
  OK : Boolean;

begin
  For I:=ListA.Count-1 downto 0 do
    begin
    URL:=TMatch(ListA[i]).URL;
    OK:=False;
    J:=ListB.Count-1;
    While (J>=0) and (TMatch(ListB[i]).URL<>URL) do
      Dec(J);
    if (J=-1) then
      ListA.Delete(I);
    end;
end;

procedure TMemIndexDB.GetMatches(AWord : String; SearchOptions: TSearchOptions; AList : TFPList);

  Procedure AddMatches(W : TWordItem);
  Var
    I : Integer;

  begin
    For I:=0 to W.MatchCount-1 do
      AList.Add(W.Matches[i]);
  end;

Var
  W : TWordItem;
  I : Integer;

begin
  If (AWord='') then exit;
  if (AWord[1]='''') then
    Delete(AWord,1,1);
  I:=Length(AWord);
  if (AWord[i]='''') then
    Delete(AWord,i,1);
  AWord:=LowerCase(AWord);
  if soContains in SearchOptions then
    begin
    For I:=0 to FWords.Count-1 do
      begin
      W:=TWordItem(FWords[i]);
      If Pos(Aword,W.Description)<>0 then
        AddMatches(W);
      end
    end
  else
    begin
    W:=TWordItem(FWords.FindDescr(AWord));
    if (W<>Nil) then
      AddMatches(W);
    end;
end;

procedure TMemIndexDB.FindSearchData(SearchWord: TWordParser;
  FPSearch: TFPSearch; SearchOptions: TSearchOptions);

Var
  L,W : TFPList;
  S : String;
  I : Integer;
  M : TMatch;
  WD : TSearchWordData;

begin
  L:=TFPList.Create;
  try
    W:=TFPList.Create;
    for I:=0 to SearchWord.Count-1 do
      begin
      Case SearchWord.Token[i].TokenType of
        wtWord : begin
                 S:=LowerCase(SearchWord.Token[i].Value);
                 if (I=0) then
                   GetMatches(S,SearchOptions,L)
                 else
                   GetMatches(S,SearchOptions,W);
                 end;
        wtOr :
          UnionMatches(L,W);
        wtAnd :
          InterSectMatches(L,W);
      end;
      end;
    For I:=0 to L.Count-1 do
      begin
      M:=TMatch(L[i]);
      WD.SearchWord:=M.Word.Description;
      WD.Context:=M.Context;
      WD.FileDate:=M.URL.URLDate;
      WD.URL:=M.URL.Description;
      WD.Position:=M.Position;
      WD.Language:=M.URL.Language.Description;
      FPSearch.AddResult(i,WD);
      end;
  finally
    L.Free;
  end;
end;

procedure TMemIndexDB.CreateIndexerTables;
begin
  Clear;
end;

{ TMatches }

function TMatches.GetM(AIndex : Integer): TMatch;
begin
  Result:=TMatch(Items[AIndex]);
end;

procedure TMatches.SetM(AIndex : Integer; AValue: TMatch);
begin
  Items[AIndex]:=AValue;
end;

function TMatches.AddMatch(AWord: TWordItem; AURL: TURLItem): TMatch;
begin
  Result:=TMatch(Add);
  Result.URL:=AURl;
  Result.Word:=AWord;
end;

{ TMatch }

procedure TMatch.SetURL(AValue: TURLItem);
begin
  if FURL=AValue then exit;
  If (FURL<>Nil) then
    FURL.RemoveMatch(Self);
  FURL:=AValue;
  If (FURL<>Nil) then
    FURL.AddMatch(Self);
end;

function TMatch.GetContext: String;
begin
  Result:=Description;
end;

procedure TMatch.SetContext(AValue: String);
begin
  Description:=AValue;
end;

procedure TMatch.SetWord(AValue: TWordItem);
begin
  if FWord=AValue then exit;
  If (FWord<>Nil) then
    FWord.RemoveMatch(Self);
  FWord:=AValue;
  If (FWord<>Nil) then
    FWord.AddMatch(Self);
end;

function TMatch.BlockSize: Integer;
begin
  Result:=inherited BlockSize;
  Result:=Result+SizeOf(FPosition)+2*SizeOf(Integer);
end;

procedure TMatch.WriteToStream(S: TStream);

Var
  L : Integer;

begin
  inherited WriteToStream(S);
  S.WriteBuffer(FPosition,Sizeof(FPosition));
  WriteRefToStream(S,FWord);
  WriteRefToStream(S,FUrl);
end;

procedure TMatch.Resolve(Words, URLS: TDescrCollection);
begin
  Word:=TWordItem(Words.Resolve(FWordID));
  URL:=TURLItem(URLS.Resolve(FURLID));
end;

function TMatch.ReadFromStream(S: TStream): Integer;
begin
  Result:=inherited ReadFromStream(S);
  S.ReadBuffer(FPosition,Sizeof(FPosition));
  S.ReadBuffer(FWordID,SizeOf(FWordID));
  S.ReadBuffer(FURLID,SizeOf(FURLID));
end;

{ TDescrCollection }

function TDescrCollection.GetD(AIndex : Integer): TDescrItem;
begin
  Result:=TDescrItem(Items[AIndex])
end;

procedure TDescrCollection.SetD(AIndex : Integer; AValue: TDescrItem);
begin
  Items[AIndex]:=AValue;
end;

procedure TDescrCollection.RebuildHash;

Var
  I : Integer;
  D : TDescrItem;

begin
  if FHash<>Nil then
    FHash.Clear
  else
    FHash:=TFPhashList.Create;
  For I:=0 to Count-1 do
    begin
    D:=GetD(I);
    FHash.Add(D.Description,D);
    end;
end;

function TDescrCollection.Resolve(AIndex: Integer): TDescrItem;
begin
  If (Aindex=-1) or (AIndex>=Count) then
    Result:=Nil
  else
    Result:=TDescrItem(Items[AIndex]);
end;

destructor TDescrCollection.destroy;
begin
  FreeAndNil(FHash);
  inherited destroy;
end;


procedure TDescrCollection.BeginLoading;
begin
  Inc(FLoadCount);
end;

procedure TDescrCollection.EndLoading;
begin
  if (FLoadCount>0) then
    begin
    Dec(FLoadCount);
    If (FLoadCount=0) then
      RebuildHash;
    end;
end;

procedure TDescrCollection.AllocateIDS;

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    GetD(i).TheID:=I;
end;

function TDescrCollection.FindDescr(const ADescription: String): TDescrItem;
begin

  If FHash=Nil then
    Result:=Nil
  else
    Result:=TDescrItem(FHash.Find(ADescription));
end;

function TDescrCollection.AddDescr(const ADescription: String): TDescrItem;
begin
  Result:=Add as TDescrItem;
  Result.Description:=ADescription;
  if (FLoadCount=0) then
    begin
    If FHash=Nil then
      ReBuildHash
    else
      FHash.Add(ADescription,Result);
    end;
end;



{ TWordItem }

function TMatchedItem.GetMatch(AIndex : Integer): TMatch;
begin
  Result:=TMatch(FList[AIndex]);
end;

function TMatchedItem.GetMatchCount: Integer;
begin
  Result:=FList.Count;
end;

function TMatchedItem.AddMatch(AMatch: TMatch): Integer;
begin
  FList.Add(AMatch);
end;

procedure TMatchedItem.RemoveMatch(AMatch: TMatch);
begin
  Flist.Remove(AMatch);
end;

constructor TMatchedItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FList:=TFPList.Create;
end;

destructor TMatchedItem.Destroy;
begin
  FreeAndNil(Flist);
  inherited Destroy;
end;

end.

