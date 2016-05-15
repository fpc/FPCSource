program chmsearch;
{ Fulltext search demo by Reinier Olislagers}
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, ChmReader, chmfiftimain;


type
  TChmWLCTopic = record
    TopicIndex: DWord;
    LocationCodes: array of DWord;
  end;

  TChmWLCTopicArray = array of TChmWLCTopic;

procedure DoSearch(CHMFileName: string; Keyword: string);
type
  TTopicEntry = record
    Topic:Integer;
    Hits: Integer;
    TitleHits: Integer;
    FoundForThisRound: Boolean;
  end;
  TFoundTopics = array of TTopicEntry;
var
  FoundTopics: TFoundTopics;

  procedure DeleteTopic(ATopicIndex: Integer);
  var
    MoveSize: DWord;
  begin
    WriteLn('Deleting Topic');
    if ATopicIndex < High(FoundTopics) then
    begin
      MoveSize := SizeOf(TTopicEntry) * (High(FoundTopics) - (ATopicIndex+1));
      Move(FoundTopics[ATopicIndex+1], FoundTopics[ATopicIndex], MoveSize);
    end;
    SetLength(FoundTopics, Length(FoundTopics) -1);
  end;

  function GetTopicIndex(ATopicID: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to High(FoundTopics) do
    begin
      if FoundTopics[i].Topic = ATopicID then
        Exit(i);
    end;
  end;

  procedure UpdateTopic(TopicID: Integer; NewHits: Integer; NewTitleHits: Integer; AddNewTopic: Boolean);
  var
    TopicIndex: Integer;
  begin
    //WriteLn('Updating topic');
    TopicIndex := GetTopicIndex(TopicID);
    if TopicIndex = -1 then
    begin
      if AddNewTopic = False then
        Exit;
      SetLength(FoundTopics, Length(FoundTopics)+1);
      TopicIndex := High(FoundTopics);
      FoundTopics[TopicIndex].Topic := TopicID;
    end;

    FoundTopics[TopicIndex].FoundForThisRound := True;
    if NewHits > 0 then
      Inc(FoundTopics[TopicIndex].Hits, NewHits);
    if NewTitleHits > 0 then
      Inc(FoundTopics[TopicIndex].TitleHits, NewTitleHits);
  end;

var
  CHMRead: TCHMReader;
  CHMStream: TFileStream;
  TopicResults: chmfiftimain.TChmWLCTopicArray;
  TitleResults: chmfiftimain.TChmWLCTopicArray;
  FIftiMainStream: TMemoryStream;
  SearchReader: TChmSearchReader;
  DocTitle: String;
  DocURL: String;
  CurrTopic: Integer;
  k: Integer;

begin
  CHMStream := TFileStream.Create(CHMFileName, fmOpenRead or fmShareDenyWrite);
  ChmRead := TChmReader.Create(CHMStream,false);
  try
    FIftiMainStream := CHMRead.GetObject('/$FIftiMain');
    if FIftiMainStream = nil then
    begin
      writeln('Could not assign fiftimainstream. Aborting.');
      halt(3);
    end;
    SearchReader := TChmSearchReader.Create(FIftiMainStream, True); //frees the stream when done
    CHMRead.SearchReader := SearchReader;
    TopicResults := SearchReader.LookupWord(Keyword, TitleResults);
    //TopicResults := SearchReader.LookupWord(SearchWords[CurrTopic], TitleResults);
    // Body results
    for k := 0 to High(TopicResults) do
    begin
      UpdateTopic(TopicResults[k].TopicIndex, High(TopicResults[k].LocationCodes), 0, CurrTopic = 0);
      writeln('Updated topic body with index '+inttostr(TopicResults[k].TopicIndex));
    end;
    // Title results
    for k := 0 to High(TitleResults) do
    begin
      UpdateTopic(TitleResults[k].TopicIndex, 0, High(TitleResults[k].LocationCodes), CurrTopic = 0);
      writeln('Updated title topic with index '+inttostr(TitleResults[k].TopicIndex));
    end;

    // Remove documents that don't have results
    k := 0;
    writeln('Going to remove docs without results; count: '+Inttostr(Length(FoundTopics)));
    while k <= High(FoundTopics) do
    begin
      if FoundTopics[k].FoundForThisRound = False then
        DeleteTopic(k)
      else
      begin
        FoundTopics[k].FoundForThisRound := False;
        Inc(k);
      end;
    end;

    // Clear out results that don't contain all the words we are looking for
    // Now lookup titles and urls to add to final search results
    writeln('Found '+inttostr(Length(FoundTopics))+' topics');
    for CurrTopic := 0 to High(FoundTopics) do
    begin
      try
        DocURL := CHMRead.LookupTopicByID(FoundTopics[CurrTopic].Topic, DocTitle);
        if (Length(DocURL) > 0) and (DocURL[1] <> '/') then
          Insert('/', DocURL, 1);
        if DocTitle = '' then
          DocTitle := 'untitled';
        writeln('DocURL    : '+DocURL);
        writeln('DocTitle  : '+DocTitle);
      except
        on E: Exception do
        begin
          WriteLn('Exception');
          writeln(E.Message);
        end;
      end;
    end;
  finally
    CHMRead.Free;
    CHMStream.Free;
    //SearchReader.Free; //apparently not needed?!?!
  end;
end;

var
  SearchFor: string;
begin
  if paramstr(1)='' then
  begin
    writeln('No .chm file specified.');
    writeln('Substituting hardcoded value lcl.chm');
  end;
  writeln('Enter search keyword or blank to exit:');
  readln(SearchFor);
  while (trim(SearchFor)<>'') do
  begin
    if paramstr(1)='' then
      DoSearch('lcl.chm',SearchFor)
    else
      DoSearch(paramstr(1),SearchFor);
    writeln('Enter search keyword or blank to exit:');
    readln(SearchFor);
  end;
end.


