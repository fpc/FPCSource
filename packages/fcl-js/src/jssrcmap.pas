{ *********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 Mattias Gaertner.

    Javascript Source Map

    See Source Maps Revision 3:
    https://docs.google.com/document/d/1U1RGAehQwRypUTovF1KRlpiOFze0b-_2gc6fAH0KY0k/edit?hl=en_US&pli=1&pli=1#

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}
unit JSSrcMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, fpjson;

const
  Base64Chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  DefaultSrcMapHeader = ')]}'+LineEnding;

type

  { TSourceMapSegment }

  TSourceMapSegment = class
  public
    Index: integer; // index in FNodes
    GeneratedLine: integer;
    GeneratedColumn: integer;
    SrcFileIndex: integer; // index in FSources
    SrcLine: integer;
    SrcColumn: integer;
    NameIndex: integer; // index in FNames
  end;

  TSourceMapSrc = class
  public
    Filename: string;
    Source: String;
  end;

  { TSourceMap }

  TSourceMap = class
  private
    type

      { TStringToIndex }

      TStringToIndex = class
      private
        FItems: TFPHashList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        procedure Add(const Value: String; Index: integer);
        function FindValue(const Value: String): integer;
      end;
  private
    FAddMonotonous: boolean;
    FHeader: String;
    FGeneratedFilename: string;
    FNames: TStrings; // in adding order
    FNameToIndex: TStringToIndex; // name to index in FNames
    FItems: TFPList; // TSourceMapSegment, in adding order
    FSourceRoot: string;
    FSources: TFPList; // list of TSourceMapSrc, in adding order
    FSourceToIndex: TStringToIndex; // srcfile to index in FSources
    FVersion: integer;
    function GetNames(Index: integer): string;
    function GetItems(Index: integer): TSourceMapSegment;
    function GetSourceContents(Index: integer): String;
    function GetSourceFiles(Index: integer): String;
    procedure SetGeneratedFilename(const AValue: string);
    procedure SetSourceContents(Index: integer; const AValue: String);
  public
    constructor Create(const aGeneratedFilename: string);
    destructor Destroy; override;
    procedure Clear; virtual;
    function AddMapping(
      GeneratedLine: integer; // 1-based
      GeneratedCol: integer = 0; // 0-based
      const SourceFile: string = ''; // can be empty ''
      SrcLine: integer = 1; // 1-based
      SrcCol: integer = 0; // 0-based
      const Name: String = ''): TSourceMapSegment; virtual;
    property AddMonotonous: boolean read FAddMonotonous
      write FAddMonotonous default true;// true = AddMapping GeneratedLine/Col must be behind last add, false = check all adds for duplicate
    function CreateMappings: String; virtual;
    function ToJSON: TJSONObject; virtual;
    procedure SaveToStream(aStream: TStream); virtual;
    procedure SaveToFile(Filename: string); virtual;
    function ToString: string; override;
    property GeneratedFilename: string read FGeneratedFilename write SetGeneratedFilename;
    function IndexOfName(const Name: string; AddIfNotExists: boolean = false): integer;
    function IndexOfSourceFile(const SrcFile: string; AddIfNotExists: boolean = false): integer;
    function Count: integer;
    property Items[Index: integer]: TSourceMapSegment read GetItems; default; // segments
    function SourceCount: integer;
    property SourceRoot: string read FSourceRoot write FSourceRoot;
    property SourceFiles[Index: integer]: String read GetSourceFiles;
    property SourceContents[Index: integer]: String read GetSourceContents write SetSourceContents;
    function NameCount: integer;
    property Names[Index: integer]: string read GetNames;
    property Version: integer read FVersion; // 3
    property Header: String read FHeader write FHeader; // DefaultSrcMapHeader
  end;

function EncodeBase64VLQ(i: NativeInt): String; // base64 Variable Length Quantity
function DecodeBase64VLQ(const s: string): NativeInt; // base64 Variable Length Quantity
function DecodeBase64VLQ(var p: PChar): NativeInt; // base64 Variable Length Quantity

implementation

function EncodeBase64VLQ(i: NativeInt): String;
{ Convert signed number to base64-VLQ:
  Each base64 has 6bit, where the most significant bit is the continuation bit
  (1=there is a next base64 character).
  The first character contains the 5 least significant bits of the number.
  The last bit of the first character is the sign bit (1=negative).
  For example:
  A = 0 = %000000 => 0
  B = 1 = %000001 => -0
  C = 2 = %000010 => 1
  iF = 34 5 = %100010 %000101 = 00010 00101 = 1000101 = 69
}

  procedure RaiseRange;
  begin
    raise ERangeError.Create('EncodeBase64VLQ');
  end;

var
  digits: NativeInt;
begin
  Result:='';
  if i<0 then
    begin
    i:=-i;
    if i>(High(NativeInt)-1) shr 1 then
      RaiseRange;
    i:=(i shl 1)+1;
    end
  else
    begin
    if i>High(NativeInt) shr 1 then
      RaiseRange;
    i:=i shl 1;
    end;
  repeat
    digits:=i and %11111;
    i:=i shr 5;
    if i>0 then
      inc(digits,%100000); // need another char -> set continuation bit
    Result:=Result+Base64Chars[digits+1];
  until i=0;
end;

function DecodeBase64VLQ(const s: string): NativeInt;
var
  p: PChar;
begin
  if s='' then
    raise EConvertError.Create('DecodeBase64VLQ empty');
  p:=PChar(s);
  Result:=DecodeBase64VLQ(p);
  if p-PChar(s)<>length(s) then
    raise EConvertError.Create('DecodeBase64VLQ waste');
end;

function DecodeBase64VLQ(var p: PChar): NativeInt;
{ Convert base64-VLQ to signed number,
  For the fomat see EncodeBase64VLQ
}

  procedure RaiseInvalid;
  begin
    raise ERangeError.Create('DecodeBase64VLQ');
  end;

const
  MaxShift = 63-5; // actually log2(High(NativeInt))-5
var
  c: Char;
  digit, Shift: Integer;
begin
  Result:=0;
  Shift:=0;
  repeat
    c:=p^;
    case c of
    'A'..'Z': digit:=ord(c)-ord('A');
    'a'..'z': digit:=ord(c)-ord('a')+26;
    '0'..'9': digit:=ord(c)-ord('0')+52;
    '+': digit:=62;
    '/': digit:=63;
    else RaiseInvalid;
    end;
    inc(p);
    if Shift>MaxShift then
      RaiseInvalid;
    inc(Result,(digit and %11111) shl Shift);
    inc(Shift,5);
  until digit<%100000;
  if (Result and 1)>0 then
    Result:=-(Result shr 1)
  else
    Result:=Result shr 1;
end;

{ TSourceMap.TStringToIndex }

constructor TSourceMap.TStringToIndex.Create;
begin
  FItems:=TFPHashList.Create;
end;

destructor TSourceMap.TStringToIndex.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TSourceMap.TStringToIndex.Clear;
begin
  FItems.Clear;
end;

procedure TSourceMap.TStringToIndex.Add(const Value: String; Index: integer);
begin
  // Note: nil=0 means not found in TFPHashList
  FItems.Add(Value,{%H-}Pointer(PtrInt(Index+1)));
end;

function TSourceMap.TStringToIndex.FindValue(const Value: String
  ): integer;
begin
  // Note: nil=0 means not found in TFPHashList
  Result:=integer({%H-}PtrInt(FItems.Find(Value)))-1;
end;

{ TSourceMap }

procedure TSourceMap.SetGeneratedFilename(const AValue: string);
begin
  if FGeneratedFilename=AValue then Exit;
  FGeneratedFilename:=AValue;
end;

procedure TSourceMap.SetSourceContents(Index: integer; const AValue: String);
begin
  TSourceMapSrc(FSources[Index]).Source:=AValue;
end;

function TSourceMap.GetItems(Index: integer): TSourceMapSegment;
begin
  Result:=TSourceMapSegment(FItems[Index]);
end;

function TSourceMap.GetSourceContents(Index: integer): String;
begin
  Result:=TSourceMapSrc(FSources[Index]).Source;
end;

function TSourceMap.GetNames(Index: integer): string;
begin
  Result:=FNames[Index];
end;

function TSourceMap.GetSourceFiles(Index: integer): String;
begin
  Result:=TSourceMapSrc(FSources[Index]).Filename;
end;

constructor TSourceMap.Create(const aGeneratedFilename: string);
begin
  FVersion:=3;
  FNames:=TStringList.Create;
  FNameToIndex:=TStringToIndex.Create;
  FItems:=TFPList.Create;
  FSources:=TFPList.Create;
  FSourceToIndex:=TStringToIndex.Create;
  FAddMonotonous:=true;
  FHeader:=DefaultSrcMapHeader;
  GeneratedFilename:=aGeneratedFilename;
end;

destructor TSourceMap.Destroy;
begin
  Clear;
  FreeAndNil(FSourceToIndex);
  FreeAndNil(FSources);
  FreeAndNil(FItems);
  FreeAndNil(FNameToIndex);
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TSourceMap.Clear;
var
  i: Integer;
begin
  FSourceToIndex.Clear;
  for i:=0 to FSources.Count-1 do
    TObject(FSources[i]).Free;
  FSources.Clear;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FNameToIndex.Clear;
  FNames.Clear;
end;

function TSourceMap.AddMapping(GeneratedLine: integer; GeneratedCol: integer;
  const SourceFile: string; SrcLine: integer; SrcCol: integer;
  const Name: String): TSourceMapSegment;

  procedure RaiseInvalid(Msg: string);
  begin
    raise Exception.CreateFmt('%s (GeneratedLine=%d GeneratedCol=%d SrcFile="%s" SrcLine=%d SrcCol=%d Name="%s")',
      [Msg,GeneratedLine,GeneratedCol,SourceFile,SrcLine,SrcCol,Name]);
  end;

var
  NodeCnt, i: Integer;
  OtherNode: TSourceMapSegment;
begin
  if GeneratedLine<1 then
    RaiseInvalid('invalid GeneratedLine');
  if GeneratedCol<0 then
    RaiseInvalid('invalid GeneratedCol');
  if SourceFile='' then
    begin
    if Count=0 then
      RaiseInvalid('missing source file');
    if SrcLine<>1 then
      RaiseInvalid('invalid SrcLine');
    if SrcCol<>0 then
      RaiseInvalid('invalid SrcCol');
    if Name<>'' then
      RaiseInvalid('invalid Name');
    end
  else
    begin
    if SrcLine<1 then
      RaiseInvalid('invalid SrcLine');
    if SrcCol<0 then
      RaiseInvalid('invalid SrcCol');
    end;

  // check if generated line/col already exists
  NodeCnt:=Count;
  if AddMonotonous then
    begin
    if NodeCnt>0 then
      begin
      OtherNode:=Items[NodeCnt-1];
      if (OtherNode.GeneratedLine>GeneratedLine)
          or ((OtherNode.GeneratedLine=GeneratedLine)
            and (OtherNode.GeneratedColumn>GeneratedCol)) then
        RaiseInvalid('GeneratedLine/Col not monotonous');
      // Note: same line/col is allowed
      end;
    end
  else
    begin
    for i:=0 to NodeCnt-1 do
      begin
      OtherNode:=Items[i];
      if (OtherNode.GeneratedLine=GeneratedLine) and (OtherNode.GeneratedColumn=GeneratedCol) then
        RaiseInvalid('duplicate GeneratedLine/Col');
      end;
    end;

  // add
  Result:=TSourceMapSegment.Create;
  Result.Index:=FItems.Count;
  Result.GeneratedLine:=GeneratedLine;
  Result.GeneratedColumn:=GeneratedCol;
  if SourceFile='' then
    Result.SrcFileIndex:=-1
  else
    Result.SrcFileIndex:=IndexOfSourceFile(SourceFile,true);
  Result.SrcLine:=SrcLine;
  Result.SrcColumn:=SrcCol;
  if Name<>'' then
    Result.NameIndex:=IndexOfName(Name,true)
  else
    Result.NameIndex:=-1;
  FItems.Add(Result);
end;

function TSourceMap.CreateMappings: String;

  procedure Add(ms: TMemoryStream; const s: string);
  begin
    if s<>'' then
      ms.Write(s[1],length(s));
  end;

var
  ms: TMemoryStream;
  i, LastGeneratedLine, LastGeneratedColumn, j, LastSrcFileIndex, LastSrcLine,
    LastSrcColumn, SrcLine, LastNameIndex: Integer;
  Item: TSourceMapSegment;
begin
  Result:='';
  LastGeneratedLine:=1;
  LastGeneratedColumn:=0;
  LastSrcFileIndex:=0;
  LastSrcLine:=0;
  LastSrcColumn:=0;
  LastNameIndex:=0;
  ms:=TMemoryStream.Create;
  try
    for i:=0 to Count-1 do
      begin
      Item:=Items[i];
      if LastGeneratedLine<Item.GeneratedLine then
        begin
        // new line
        LastGeneratedColumn:=0;
        for j:=LastGeneratedLine+1 to Item.GeneratedLine do
          ms.WriteByte(ord(';'));
        LastGeneratedLine:=Item.GeneratedLine;
        end
      else if i>0 then
        begin
        // not the first segment
        if (LastGeneratedLine=Item.GeneratedLine)
            and (LastGeneratedColumn=Item.GeneratedColumn) then
          continue;
        ms.WriteByte(ord(','));
        end;
      // column diff
      Add(ms,EncodeBase64VLQ(Item.GeneratedColumn-LastGeneratedColumn));
      LastGeneratedColumn:=Item.GeneratedColumn;

      if Item.SrcFileIndex<0 then
        continue; // no source -> segment length 1
      // src file index diff
      Add(ms,EncodeBase64VLQ(Item.SrcFileIndex-LastSrcFileIndex));
      LastSrcFileIndex:=Item.SrcFileIndex;
      // src line diff
      SrcLine:=Item.SrcLine-1; // 0 based in version 3
      Add(ms,EncodeBase64VLQ(SrcLine-LastSrcLine));
      LastSrcLine:=SrcLine;
      // src column diff
      Add(ms,EncodeBase64VLQ(Item.SrcColumn-LastSrcColumn));
      LastSrcColumn:=Item.SrcColumn;
      // name index
      if Item.NameIndex<0 then
        continue; // no name -> segment length 4
      Add(ms,EncodeBase64VLQ(Item.NameIndex-LastNameIndex));
      LastNameIndex:=Item.NameIndex;
      end;
    SetLength(Result,ms.Size);
    if Result<>'' then
      Move(ms.Memory^,Result[1],ms.Size);
  finally
    ms.Free;
  end;
end;

function TSourceMap.ToJSON: TJSONObject;
var
  Obj: TJSONObject;
  i: Integer;
  Arr: TJSONArray;
  Mappings: String;
begin
  Result:=nil;
  Mappings:=CreateMappings;

  Obj:=TJSONObject.Create;
  try
    // "version" - integer
    Obj.Add('version',Version);

    // "file" - GeneratedFilename
    if GeneratedFilename<>'' then
      Obj.Add('file',GeneratedFilename);

    // "sourceRoot" - SourceRoot
    if SourceRoot<>'' then
      Obj.Add('sourceRoot',SourceRoot);

    // "sources" - array of filenames
    Arr:=TJSONArray.Create;
    Obj.Add('sources',Arr);
    for i:=0 to SourceCount-1 do
      Arr.Add(SourceFiles[i]);

    // "sourcesContent" - array of source content: null or source as string
    // only needed if there is a source
    i:=SourceCount-1;
    while i>=0 do
      if SourceContents[i]='' then
        dec(i)
      else
        begin
        // there is a source -> add array
        Arr:=TJSONArray.Create;
        Obj.Add('sourcesContent',Arr);
        for i:=0 to SourceCount-1 do
          if SourceContents[i]='' then
            Arr.Add(TJSONNull.Create)
          else
            Arr.Add(SourceContents[i]);
        break;
        end;

    // "names" - array of names
    Arr:=TJSONArray.Create;
    Obj.Add('names',Arr);
    for i:=0 to NameCount-1 do
      Arr.Add(Names[i]);

    // "mappings" - string
    Obj.Add('mappings',Mappings);

    Result:=Obj;
  finally
    if Result=nil then
      Obj.Free;
  end;
end;

procedure TSourceMap.SaveToStream(aStream: TStream);
var
  Obj: TJSONObject;
begin
  Obj:=ToJSON;
  try
    if Header<>'' then
      aStream.Write(Header[1],length(Header));
    Obj.DumpJSON(aStream);
  finally
    Obj.Free;
  end;
end;

procedure TSourceMap.SaveToFile(Filename: string);
var
  TheStream: TMemoryStream;
begin
  TheStream:=TMemoryStream.Create;
  try
    SaveToStream(TheStream);
    TheStream.Position:=0;
    TheStream.SaveToFile(Filename);
  finally
    TheStream.Free;
  end;
end;

function TSourceMap.ToString: string;
var
  Obj: TJSONObject;
begin
  Obj:=ToJSON;
  try
    Result:=Header+Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

function TSourceMap.IndexOfName(const Name: string; AddIfNotExists: boolean
  ): integer;
begin
  Result:=FNameToIndex.FindValue(Name);
  if (Result>=0) or not AddIfNotExists then exit;
  Result:=FNames.Count;
  FNames.Add(Name);
  FNameToIndex.Add(Name,Result);
end;

function TSourceMap.IndexOfSourceFile(const SrcFile: string;
  AddIfNotExists: boolean): integer;
var
  Src: TSourceMapSrc;
begin
  Result:=FSourceToIndex.FindValue(SrcFile);
  if (Result>=0) or not AddIfNotExists then exit;
  Src:=TSourceMapSrc.Create;
  Src.Filename:=SrcFile;
  Result:=FSources.Count;
  FSources.Add(Src);
  FSourceToIndex.Add(SrcFile,Result);
end;

function TSourceMap.Count: integer;
begin
  Result:=FItems.Count;
end;

function TSourceMap.SourceCount: integer;
begin
  Result:=FSources.Count;
end;

function TSourceMap.NameCount: integer;
begin
  Result:=FNames.Count;
end;

end.

