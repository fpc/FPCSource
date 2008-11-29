{ Copyright (C) <2008> <Andrew Haines> htmlindexer.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit HTMLIndexer;
{$MODE OBJFPC}{$H+}
interface
uses Classes, SysUtils, FastHTMLParser;

Type

  { TIndexedWord }

  { TIndexDocument }

  TIndexDocument = class(TObject)
  private
    FDocumentIndex: Integer;
  public
    WordIndex: array of Integer;
    procedure AddWordIndex(AIndex: Integer);
    constructor Create(ADocumentIndex: Integer);
    property DocumentIndex: Integer read FDocumentIndex;
  end;




  TIndexedWord = class(TObject)
  private
    FIsTitle: Boolean;
    FNextWord: TIndexedWord;
    FPrevWord: TIndexedWord;
    FTheWord: string;
    FCachedTopic: TIndexDocument;
    FDocuments: Array of TIndexDocument;
    function GetDocument ( TopicIndexNum: Integer ) : TIndexDocument;
    function GetDocumentCount: Integer;
  public
    constructor Create(AWord: String; AIsTitle: Boolean);
    destructor Destroy; override;
    function GetLogicalDocument(AIndex: Integer): TIndexDocument;
    property TheWord: string read FTheWord; // Always lowercase
    property PrevWord: TIndexedWord read FPrevWord write FPrevWord;
    property NextWord: TIndexedWord read FNextWord write FNextWord;
    property DocumentTopic[TopicIndexNum: Integer]: TIndexDocument read GetDocument;
    property DocumentCount: Integer read GetDocumentCount;
    property IsTitle: Boolean read FIsTitle;
  end;

  { TIndexedWordList }

  TIndexedWordList = class(TObject)
  private
    FIndexTitlesOnly: Boolean;
    FIndexedFileCount: DWord;
    //vars while processing page
    FInTitle,
    FInBody: Boolean;
    FWordCount: Integer; // only words in body
    FDocTitle: String;
    FTopicIndex: Integer;
    //end vars
    FTotalDifferentWordLength: DWord;
    FTotalDIfferentWords: DWord;
    FTotalWordCount: DWord;
    FTotalWordLength: DWord;
    FLongestWord: DWord;
    FFirstWord: TIndexedWord;
    FCachedWord: TIndexedWord;
    FParser: THTMLParser;
    function AddGetWord(AWord: String; IsTitle: Boolean): TIndexedWord;
    function GetWordForward(AWord: String; StartWord: TIndexedWord; out WrongWord: TIndexedWord; AIsTitle: Boolean): TIndexedWord;
    function GetWordBackward(AWord: String; StartWord: TIndexedWord; out WrongWord: TIndexedWord; AIsTitle: Boolean): TIndexedWord;
    function CompareWord(AWord: String; AIndexWord: TIndexedWord; AIsTitle: Boolean): Integer;
    // callbacks
    procedure CBFoundTag(NoCaseTag, ActualTag: string);
    procedure CBFountText(Text: string);

    procedure EatWords(Words: String; IsTitle: Boolean);
  public
    constructor Create;
    destructor  Destroy; override;
    function  IndexFile(AStream: TStream; ATOPICIndex: Integer; AIndexOnlyTitles: Boolean): String; // returns the documents <Title>
    procedure Clear;
    procedure AddWord(const AWord: TIndexedWord; StartingWord: TIndexedWord; AIsTitle: Boolean);
    property FirstWord: TIndexedWord read FFirstWord;
    property IndexedFileCount: DWord read FIndexedFileCount;
    property LongestWord: DWord read FLongestWord;
    property TotalWordCount: DWord read FTotalWordCount;
    property TotalDIfferentWords: DWord read FTotalDIfferentWords;
    property TotalWordLength: DWord read FTotalWordLength;
    property TotalDifferentWordLength: DWord read FTotalDifferentWordLength;
    property Words[AWord: String; IsTitle: Boolean] : TIndexedWord read AddGetWord;
  end;

implementation

function Max(ANumber, BNumber: DWord): DWord;
begin
  if ANumber > BNumber then
    Result := ANumber
  else
    Result := BNumber;
end;

{ TIndexedWordList }

function TIndexedWordList.AddGetWord(AWord: String; IsTitle: Boolean): TIndexedWord;
var
  //StartWord,
  WrongWord: TIndexedWord;
begin
  Result := nil;
  AWord := LowerCase(AWord);

  {if FCachedWord <> nil then
    StartWord := FCachedWord
  else
    StartWord := FFirstWord;

  if StartWord <> nil then
  begin
    case CompareWord(AWord, StartWord, IsTitle) of
      0: Exit(WrongWord);
      1: Result := GetWordBackward(AWord, StartWord, WrongWord, IsTitle);
     -1: Result := GetWordForward(AWord, StartWord, WrongWord, IsTitle);
    end;
  end
  else}
    Result := GetWordForward(AWord, FFirstWord, WrongWord, IsTitle);

  if Result = nil then
  begin
    Inc(FTotalDifferentWordLength, Length(AWord));
    Inc(FTotalDIfferentWords);
    Result := TIndexedWord.Create(AWord,IsTitle);
    AddWord(Result, WrongWord,IsTitle);
    if IsTitle then
    ;//WriteLn('Creating word: ', AWord);
    FLongestWord := Max(FLongestWord, Length(AWord));
  end;
  Inc(FTotalWordLength, Length(AWord));
  Inc(FTotalWordCount);
end;

function TIndexedWordList.GetWordForward(AWord: String; StartWord: TIndexedWord; out WrongWord: TIndexedWord; AIsTitle: Boolean): TIndexedWord;
var
  FCurrentWord: TIndexedWord;
begin
  Result := nil;
  WrongWord := nil;
  FCurrentWord := StartWord;
  while (FCurrentWord <> nil) and (CompareWord(AWord, FCurrentWord, AIsTitle) <> 0) do
  begin
    WrongWord := FCurrentWord;
    case CompareWord(AWord, FCurrentWord, AIsTitle) of
      -1: FCurrentWord := nil;
       0: Exit(FCurrentWord);
       1: FCurrentWord := FCurrentWord.NextWord;
    end;
  end;

  if FCurrentWord <> nil then
    Result := FCurrentWord;
end;

function TIndexedWordList.GetWordBackward(AWord: String; StartWord: TIndexedWord; out WrongWord: TIndexedWord; AIsTitle: Boolean): TIndexedWord;
var
  FCurrentWord: TIndexedWord;
begin
  Result := nil;
  WrongWord := nil;
  FCurrentWord := StartWord;
  while (FCurrentWord <> nil) and (CompareWord(AWord, FCurrentWord, AIsTitle) <> 0) do
  begin
    WrongWord := FCurrentWord;
    case CompareWord(AWord, FCurrentWord, AIsTitle) of
      -1:
          begin
            WrongWord := FCurrentWord;
            FCurrentWord := nil
          end;
       0: Exit(FCurrentWord);
       1: FCurrentWord := FCurrentWord.PrevWord;
    end;
  end;
  if FCurrentWord <> nil then
    Result := FCurrentWord;
end;

function TIndexedWordList.CompareWord ( AWord: String;
  AIndexWord: TIndexedWord; AIsTitle: Boolean ) : Integer;
begin
  Result := CompareText(AWord, AIndexWord.TheWord);
  if Result = 0 then
  begin
    Result := Result + ord(AIndexWord.IsTitle);
    Result := Result - ord(AIsTitle);
  end;
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
  //if AIsTitle then
    //WriteLn('Looking for title word :', AWord);
  //WriteLn(Result);
end;

procedure TIndexedWordList.CBFoundTag(NoCaseTag, ActualTag: string);
begin
  if FInBody then begin
    if NoCaseTag = '</BODY>' then FInBody := False;
  end
  else begin
    //WriteLn('"',NoCaseTag,'"');
    if NoCaseTag      = '<TITLE>' then FInTitle := True
    else if NoCaseTag = '</TITLE>' then FInTitle := False
    else if NoCaseTag = '<BODY>' then FInBody := True
    else
  end;
  if FInBody and FIndexTitlesOnly then FParser.Done := True;
end;

procedure TIndexedWordList.CBFountText(Text: string);
begin
  if Length(Text) < 1 then
    Exit;
  EatWords(Text, FInTitle and not FInBody);
end;

procedure TIndexedWordList.EatWords ( Words: String; IsTitle: Boolean ) ;
var
  WordPtr: PChar;
  WordStart: PChar;
  InWord: Boolean;
  IsNumberWord: Boolean;
  function IsEndOfWord: Boolean;
  begin
    Result := not (WordPtr^ in ['a'..'z', '0'..'9', #01, #$DE, #$FE]);
    if  Result and IsNumberWord then
      Result :=  Result and (WordPtr[0] <> '.');
    if Result and InWord then
      Result := Result and (WordPtr[0] <> '''');
  ;
  end;
  var
    WordIndex: TIndexedWord;
    WordName: String;
    FPos: Integer;
begin
  if IsTitle then
    FDocTitle := Words;
  Words := LowerCase(Words);
  WordStart := PChar(Words);
  WordPtr := WordStart;
  IsNumberWord := False;
  InWord := False;
  repeat
    if InWord and IsEndOfWord then
    begin
      WordName := Copy(WordStart, 0, (WordPtr-WordStart));
      FPos := Pos('''', WordName);
      while FPos > 0 do
      begin
        Delete(WordName, FPos, 1);
        FPos := Pos('''', WordName);
      end;
      WordIndex := Self.Words[WordName, IsTitle];
      InWord := False;
      //if IsNumberWord then WriteLn('Following is NUMBER WORD: "', (WordStart[0]),'"'); ;
      IsNumberWord := False;
      WordIndex.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
      //WriteLn(FWordCount, ' "', WordName,'"');
      //if not IsTitle then
        Inc(FWordCount);

    end
    else if not InWord and not IsEndOfWord then
    begin
      InWord := True;
      WordStart := WordPtr;
      IsNumberWord := WordPtr^ in ['0'..'9'];
      //if IsNumberWord then WriteLn('Following is NUMBER WORD: "', WordPtr[0],'"'); ;
    end;
    Inc(WordPtr);
  until WordPtr^ = #0;

  if InWord then
  begin
    WordName := Copy(WordStart, 0, (WordPtr-WordStart));
    WordIndex := Self.Words[WordName, IsTitle];
    WordIndex.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
    InWord := False;
    //if IsNumberWord then WriteLn('Following is NUMBER WORD: "', (WordStart[0]),'"'); ;
    IsNumberWord := False;
    //WriteLn(FWordCount, ' "', WordName,'"');
    if not IsTitle then
      Inc(FWordCount);

  end;

end;

constructor TIndexedWordList.Create;
begin
  inherited;
end;

destructor TIndexedWordList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIndexedWordList.IndexFile(AStream: TStream; ATOPICIndex: Integer; AIndexOnlyTitles: Boolean): String;
var
  TheFile: String;
begin
  FInBody := False;
  FInTitle:= False;
  FIndexTitlesOnly := AIndexOnlyTitles;
  FWordCount := 0;
  FTopicIndex := ATOPICIndex;
  FIndexedFileCount := FIndexedFileCount +1;

  SetLength(TheFile, AStream.Size+1);
  AStream.Position := 0;
  AStream.Read(TheFile[1], AStream.Size);
  TheFile[Length(TheFile)] := #0;

  FParser := THTMLParser.Create(@TheFile[1]);
  FParser.OnFoundTag := @CBFoundTag;
  FParser.OnFoundText := @CBFountText;
  FParser.Exec;
  FParser.Free;

  Result := FDocTitle;
  FDocTitle := '';
  FInBody := False;
  FInTitle:= False;
  FWordCount := 0;
  FTopicIndex := -1;

  AStream.Position := 0;
end;

procedure TIndexedWordList.Clear;
var
  FCurrentWord: TIndexedWord;
begin
  FCurrentWord := FFirstWord;
  while FCurrentWord <> nil do
  begin
    FFirstWord := FCurrentWord.NextWord;
    FCurrentWord.Free;
    FCurrentWord := FFirstWord;
  end;
end;

procedure TIndexedWordList.AddWord(const AWord: TIndexedWord; StartingWord: TIndexedWord; AIsTitle: Boolean);
var
  WrongWord: TIndexedWord;
begin
  if FFirstWord = nil then
    FFirstWord := AWord
  else begin
    if StartingWord <> nil then
      WrongWord := StartingWord;
    case CompareWord(AWord.TheWord, StartingWord, AIsTitle) of
       1: GetWordForward(AWord.TheWord, StartingWord, WrongWord, AIsTitle);
       0: ; // uh oh
      -1: GetWordBackward(AWord.TheWord, StartingWord, WrongWord, AIsTitle);
    end;
    if WrongWord = nil then
       WrongWord := FirstWord;
    case CompareWord(AWord.TheWord, WrongWord, AIsTitle) of
       -1:
          begin
            AWord.PrevWord := WrongWord.PrevWord;
            if AWord.PrevWord <> nil then
              AWord.PrevWord.NextWord := AWord;
            WrongWord.PrevWord := AWord;
            AWord.NextWord := WrongWord;
          end;
        0: ;//WriteLn('Found word which shouldn''t happen'); // uh oh
        1:
          begin
            AWord.PrevWord := WrongWord;
            AWord.NextWord := WrongWord.NextWord;
            WrongWord.NextWord := AWord;
          end;
    end;
  end;
  if AWord.PrevWord = nil then
     FFirstWord := AWord;
  FCachedWord := AWord;
end;


{ TIndexedWord }

function TIndexedWord.GetDocument ( TopicIndexNum: Integer ) : TIndexDocument;
var
  i: Integer;
begin
  Result := nil;
  if (FCachedTopic <> nil) and (FCachedTopic.FDocumentIndex = TopicIndexNum) then
    Exit(FCachedTopic);

  for i := 0 to High(FDocuments) do
    if FDocuments[i].FDocumentIndex = TopicIndexNum then
      Exit(FDocuments[i]);
  if Result = nil then
  begin
    Result := TIndexDocument.Create(TopicIndexNum);
    SetLength(FDocuments, Length(FDocuments)+1);
    FDocuments[High(FDocuments)] := Result;
  end;
  FCachedTopic := Result;
end;

function TIndexedWord.GetDocumentCount: Integer;
begin
  Result := Length(FDocuments);
end;

constructor TIndexedWord.Create(AWord: String; AIsTitle: Boolean);
begin
  FTheWord := AWord;
  FIsTitle := AIsTitle;
end;

destructor TIndexedWord.Destroy;
var
  i: Integer;
begin
  if FPrevWord <> nil then
    FPrevWord.NextWord := FNextWord;
  if FNextWord <> nil then
    FNextWord.PrevWord := FPrevWord;
  for i := 0 to High(FDocuments) do
    FreeAndNil(FDocuments[i]);
  inherited Destroy;
end;

function TIndexedWord.GetLogicalDocument ( AIndex: Integer ) : TIndexDocument;
begin
  Result := FDocuments[AIndex];;
end;

{ TIndexDocument }

procedure TIndexDocument.AddWordIndex ( AIndex: Integer ) ;
begin
  SetLength(WordIndex, Length(WordIndex)+1);
  WordIndex[High(WordIndex)] := AIndex;
end;

constructor TIndexDocument.Create ( ADocumentIndex: Integer ) ;
begin
  FDocumentIndex := ADocumentIndex;
end;

end.
