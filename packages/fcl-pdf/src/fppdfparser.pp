{ **********************************************************************
  This file is part of the Free Component Library

  PDF Parser
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

unit fppdfparser;

{$mode ObjFPC}{$H+}
{$J-}
{$ModeSwitch typehelpers}

{ $DEFINE DEBUGSTREAMS}
{ $DEFINE DUMPSTREAMS}

interface

uses
  Types, Typinfo, Classes, SysUtils, fppdfobjects, fppdfscanner, fppdfsource, streamex, fppdfpredict, fppdfcommands;

Const
  PDFMaxTrailerDistance = 6;  // Maximum number of bytes to scan backwards for trailer dictionary end: >>


Type
  { TPDFParser }
  TPDFFilterData = Record
    FilterName : String;
    Source : TStream;
    Dest : TStream;

    ParamDict : TPDFDictionary;
  end;

  EPDFParser = Class(EPDF)
    ErrorNumber : Integer;
    ObjectID,ObjectGeneration : Integer;
  end;

  TPDFFilterEvent = Procedure (Sender : TObject; Var aData : TPDFFilterData) of object;
  TPDFLogKind = (lkInfo,lkWarning,lkError);
  TPDFLogNotifyEvent = Procedure(sender : TObject; aKind : TPDFLogKind; const aMessage : string) of object;

  TPDFIndexPair = Array[1..2] of Longint;
  TPDFIndexPairArray = Array of TPDFIndexPair;
  TPDFProgressKind = (pkXRef,pkIndirect,pkContentStream);

  TPDFNewCommandEvent = procedure(Sender : TObject; aStream : TPDFPageObject; aCommand : TPDFCommand) of object;
  TPDFProgressEvent = Procedure(Sender : TObject; aKind : TPDFProgressKind;aCurrent,aCount : Integer) of object;

  TPDFParser = class
  Private
    FLoadObjects: Boolean;
    FOnLog: TPDFLogNotifyEvent;
    FOnProgress: TPDFProgressEvent;
    FOnUnknownFilter: TPDFFilterEvent;
    FResolveContentStreams: Boolean;
    FResolveObjects: Boolean;
    FResolveToUnicodeCMaps: Boolean;
    FScanner : TPDFScanner;
    FLastDict : TPDFDictionary; // Last created dictionary
    FloadingXRef : TPDFXRefArray;
    FDoc : TPDFDocument;

    procedure ParseCMAPBFChar(aMap: TPDFCMapData);
    procedure ParseCMAPBFRange(aMap: TPDFCMapData);
    procedure ParseCMAPCodeSpaceRange(aMap: TPDFCMapData);
    procedure ParseInlineImageData(var aOperands: TPDFTokenArray; aScanner: TPDFScanner );
    procedure SetResolveContentStreams(AValue: Boolean);
    procedure SetResolveToUnicodeCMaps(AValue: Boolean);
  Protected
    // Progress, Logging & Errors.
    procedure DoProgress(aKind : TPDFProgressKind; aCurrent,aCount : Integer);
    Procedure DoLog(aKind : TPDFLogKind; Const Msg : string);
    Procedure DoLog(aKind : TPDFLogKind; Const Fmt : string; Const Args : Array of const);
    Procedure DoInfo(Const Msg : string);
    Procedure DoInfo(Const Fmt : string; Const args : Array of const);
    Procedure DoWarning(Const Msg : string);
    Procedure DoWarning(Const Fmt : string; Const args : Array of const);
    Procedure DoError(const Nr : Integer; Const Msg : string);
    Procedure DoError(const Nr : Integer; Const Fmt : string; Const Args : Array of const);
    Procedure DoUnknownStruct(const aID : String);
  Protected
    // Factory methods
    Function CreateScanner(aFile : TStream; aBufferSize : Cardinal) : TPDFScanner; virtual;
    Function DefaultPDFPageClass : TPDFPageClass; virtual;
    function CreateCommand(const aName: string; aOperands: TPDFTokenArray): TPDFCommand; virtual;
    // Parsing & Token handling
    function ParseSingleValue(aStartToken: TPDFToken; SkipWhiteSpace : Boolean): TPDFObject;
    function GetIntegerToken(const aContext: String): Integer;
    function GetInt64Token(const aContext: String): Int64;
    function GetStringToken(const aContext: String): RawByteString;
    procedure UnsupportedToken(const aMsg: UTF8String; aToken: TPDFToken);
    function VerifyDocument: Int64;
    // Stream Filters
    function FindStreamLength(aReposition : Int64): Integer;
    function FilterStream(aStream: TStream; const aFilterName: String;  aParams: TPDFDictionary): TStream;
    function GetUnfilteredStream(aObj: TPDFIndirect): TStream;
    function ResolveFilters(aStream: TStream; aDict: TPDFDictionary): TStream;
    // XRef handling
    function GetXRefStreamSubSections(aObjectDict: TPDFDictionary): TPDFIndexPairArray;
    function ParseXREFStream(aStream: TStream; aObjectDict: TPDFDictionary): TPDFXRefList;
    function ParseXRefAt(aStartPos: Int64; out ParentObject : TPDFIndirect): TPDFXRefList;
    function GetLastXRef(StartAt: Int64; out FoundAt: Int64): TPDFStartXRef;
    procedure LoadIndirectObjects;
    // Object Stream
    function ParseIndirectObjectInStream(aSTMObj: TPDFIndirect; aIdx, aObjectID: Integer): TPDFIndirect;
    function ParseStreamObjectPositions(aSTMObj: TPDFIndirect  ): TPDFObjectPositionArray;
    // Content streams
    procedure DoResolveContentStreams(aDoc: TPDFDocument;
      aOnCommand: TPDFNewCommandEvent=nil);
    // Trailer
    function DetectTrailerAt(aStartPos: Int64): TPDFTrailer;
    // Dictionary handling
    procedure DoParseDictionary(aDict: TPDFDictionary; const aToken: TPDFToken);
    function ParseDictValue: TPDFObject;
    // Indirect Object handling
    function MaybeResolve(aObject: TPDFIndirect): TPDFIndirect; virtual;
    function ParseIndirectObject(aAt: Int64): TPDFIndirect;
    Procedure ParseIndirectObjectBody(aObject : TPDFIndirect);
    Function FindIndirectObject(aObjectID,aObjectGeneration : Integer) : TPDFIndirect;
    // Parsing
    procedure ParseCMap(aStream: TStream; aMap: TPDFCMap);
    function ParseCMap(aStream: TStream): TPDFCMap;
    function ParseIndirectObject(aID, aGen: Int64): TPDFIndirect; virtual;
    function ParseArray(aPrevToken: TPDFToken): TPDFArray; virtual;
    function ParseDictionary(const aToken: TPDFToken): TPDFDictionary; virtual;
    function ParseMalFormed(aToken: TPDFToken; aEndToken: RawByteString) : TPDFMalFormed; virtual;
    function ParseStreamValue(aStartToken: TPDFToken; aStreamLength: Integer): TPDFStream; virtual;
    function ParseTrailer(const aToken: TPDFToken): TPDFTrailer; virtual;
    function ParseValue(StopAt : TPDFTokenTypes): TPDFObject; virtual;
    function ParseXREF(const aToken: TPDFToken): TPDFXRefList; virtual;
    function ParseXREFItem(aIndex: Integer): TPDFXRef; virtual;
    function ParseStartXREF(const aToken: TPDFToken): TPDFStartXRef; virtual;
  Public
    Constructor Create(aFile : TStream; aBufferSize : Cardinal = PDFDefaultBufferSize); virtual;
    Destructor Destroy; override;
    function GetPageContentStream(aDoc: TPDFDocument; aPage: TPDFPageObject
      ): TStream;
    Function GetObject : TPDFObject;
    Procedure ParseDocument(aDoc : TPDFDocument); virtual;
    function LoadXREFobject(Itm: TPDFXRef; addToDocument : Boolean = True): TPDFIndirect; virtual;
    procedure ParseContentStream(aObject: TPDFPageObject; aStream: TStream;
      aOnCommand: TPDFNewCommandEvent); virtual;
    Procedure DoResolveToUnicodeCMaps(aDoc : TPDFDocument);
    class procedure Unpredict(var Data: TPDFFilterData);
    Class procedure AsciiHEXDecode(aSrc,aDest : TStream);
    Class Function AsciiHEXDecode(aSrc : TStream) : TStream;
    Class procedure Ascii85Decode(aSrc,aDest : TStream);
    Class Function Ascii85Decode(aSrc : TStream) : TStream;
    Class procedure LZWDecode(aSrc,aDest : TStream);
    Class Function LZWDecode(aSrc : TStream) : TStream;
    Class procedure Deflate(aSrc,aDest : TStream);
    Class Function Deflate(aSrc : TStream) : TStream;
    Class procedure RunlengthDecode(aSrc,aDest : TStream);
    Class Function RunlengthDecode(aSrc : TStream) : TStream;
    Property Document : TPDFDocument Read FDoc;
    // load all objects when XRef is parsed ?
    Property LoadObjects : Boolean Read FLoadObjects Write FLoadObjects  default True;
    // When loading objects, resolve objects ?
    Property ResolveObjects : Boolean Read FResolveObjects Write FResolveObjects default True;
    // Resolve content streams of pages ? Default true.
    Property ResolveContentStreams : Boolean Read FResolveContentStreams Write SetResolveContentStreams default true;
    // Resolve ToUnicode CMap maps ? Default false,
    Property ResolveToUnicodeCMaps : Boolean Read FResolveToUnicodeCMaps Write SetResolveToUnicodeCMaps default false;
    // Called when an unknown filter is encountered
    Property OnUnknownFilter : TPDFFilterEvent Read FOnUnknownFilter Write FOnUnknownFilter;
    // Log function
    Property OnLog : TPDFLogNotifyEvent Read FOnLog Write FOnLog;
    // Progress indicator when loading
    Property OnProgress : TPDFProgressEvent Read FOnProgress Write FOnProgress;
  end;

  { TPDFDocumentHelper }
Type
  TPDFLoadOption = (loLoadObjects,loResolveObjects,loResolveContentStreams,loResolveToUnicodeCMaps);
  TPDFLoadOptions = set of TPDFLoadOption;

  TPDFDocumentHelper = class Helper for TPDFDocument
    Procedure LoadFromFile(Const aFilename : String; aOnLog : TPDFLogNotifyEvent = nil; aOnProgress : TPDFProgressEvent = Nil);
    Procedure LoadFromStream(Const aStream : TStream; aOnLog : TPDFLogNotifyEvent = nil; aOnProgress : TPDFProgressEvent = Nil);
    Procedure LoadFromFile(Const aFilename : String; aOptions : TPDFLoadOptions; aOnLog : TPDFLogNotifyEvent = nil; aOnProgress : TPDFProgressEvent = Nil);
    Procedure LoadFromStream(Const aStream : TStream; aOptions : TPDFLoadOptions; aOnLog : TPDFLogNotifyEvent = nil; aOnProgress : TPDFProgressEvent = Nil);
  end;


Const
  PDFDefaultLoadOptions = [loLoadObjects,loResolveObjects,loResolveContentStreams,loResolveToUnicodeCMaps];

  // Error codes
  penUnknownToken = 1;
  penExpectedInteger = 2;
  penExpectedInt64 = 3;
  penNotStreamObject = 4;
  penNotObjectStream = 5;
  penObjectStreamWithoutDict = 6;
  penNoSuchObjectInStream = 7;
  penNotIndirectObjectAtToken = 8;
  penNotAtStreamStart = 9;
  penExpectedWhiteSpaceAfterStream = 10;
  penInvalidStreamEnd = 11;
  penNoArrayStartToken = 12;
  penNoDictionaryForLength = 13;
  penNoLengthEntryInDictionary = 14;
  penDictionaryNoLengthObject = 15;
  penDictionaryLengthNotValue = 16;
  penInvalidDictionaryRef = 17;
  penEOFWhileScanningString = 18;
  penInvalidHexString = 19;
  penNotAtDictStart = 20;
  penNotDictKeyToken = 21;
  penNotAtDictEnd = 22;
  penNotOnTrailerDict = 23;
  penInvalidTrailer = 24;
  penUnknownConstruct = 25;
  penMissingPDFHeader = 26;
  penMissingPDFVersion = 27;
  penInvalidPDFVersion = 28;
  penPDFEOFNotFound = 29;
  penObjStmObjectIDInvalid = 30;
  penObjStmObjectOffsetInvalid = 31;
  penSizeElementNotInteger = 32;
  penUnknownFilterValueClass = 33;
  penUnknownFilter = 34;
  penNoStartXRef = 35;
  penNoXrefAt = 36;
  penEOFWhileScanningCommands = 37;
  penNoSuchStreamObject = 38;
  penInvalidImageData = 39;
  penInvalidImageDataSize = 40;
  penContentStreamNotFound = 41;
  penExpectedHexInteger = 42;
  penExpectedIdentifierN = 43;
  penExpectedName = 44;

implementation

uses strutils, zstream, ascii85, chainstream, lzwstream, fppdfconsts;

resourcestring
  SErrNoStartXRef = 'No startxref found, starting at position %d';
  SErrNoXRefAt = 'No xref found at position %d';
  SErrExpectedWhiteSpaceAfterStream = 'Expected whitespace after stream';
  SErrUnknownToken = '%s: Unknown token "%s" : %s';
  SErrExpectedInteger = '%s: Expected an integer, got: %s';
  SErrExpectedInt64 = '%s: Expected an int64, got: %s';
  SErrNotAnIndirectObjectAtToken = 'Not an indirect object at token: %s';
  SErrInvalidStreamEnd = 'Invalid stream end token %d : ';
  SErrNoDictionaryForLength = 'No dictionary to get stream length from';
  SErrNoLengthEntryInDictionary = 'Dictionary does not have Length entry';
  SErrDictionaryLengthNotValue = 'Dictionary Length is not a value';
  SErrUnknownFilterValueClass = 'Unknown filter value class';
  SErrUnknownFilter =  'Unknown stream filter : %s';
  SErrInvalidDictionaryRef = 'Invalid dictionary reference value: %s ';
  SErrDictionaryNoLengthObject = 'Invalid dictionary length object reference [%d %d]';
  sErrContentStreamNotFound = 'Invalid content stream object reference [%d %d]';

  // SErrDictionaryNoLengthInObject = 'Invalid dictionary length object reference [%d %d] : No length in object';
  SErrNoArrayStartToken = 'Not at array start: %s';
  SErrNotAtStreamStart = 'Not at stream start: %s';

  SErrObjectIsNotObjectStream = 'Object %d is not a ObjStm object.';
  SErrStreamObjectWithoutDict = 'ObjStm Object %d does not have a dictionary';
  SErrNotStreamObject = 'Object %d is not a stream, it is a %s object';
  SErrExpectedString = ': Expected string';
  SErrXRefindex = 'XRef index';
  SErrXREFVersion = 'XRef generation';
  SErrXREFUseToken = 'XRef use token';
  SErrXREFStartindex = 'XRef start index';
  SErrXRefCount = 'XREF count';
  SErrNotAtDictStart = 'Invalid dictionary: not at <<, but found: %s';
  SErrNotDictKeyToken = 'Invalid dictionary: token is not a key start %s';
  SErrNotAtDictEnd = 'Invalid dictionary: end not at >> but found: %s';
  SErrNotOnTrailerDict = 'Not on trailer dictionary: %s';
  SErrInvalidTrailer = 'Invalid trailer';
  SErrUnknownConstruct = 'Unknown PDF construct (%s)';
  SErrMissingPDFHeader = 'Missing PDF header';
  SErrMissingPDFVersion = 'Missing PDF version';
  SErrInvalidPDFversion = 'Invalid PDF version: %s';
  SErrPDFEOFNotFound = 'PDF %%EOF not found';
  SWarnObjectIDDoesNotMatch = 'Indirect object stream at index %d has id %d, expected %d';
  SErrObjStmObjectIDInvalid = 'ObjStm Object %d index %d is invalid. Expected object ID';
  SErrObjStmObjectOffsetInvalid = 'ObjStm Object %d index %d is invalid. Expected offset';
  SerrSizeElementNotInteger = 'Size Element %d of W in XREf stream dictionary is not an integer value';
  SErrEOFWhileScanningCommands = 'EOF while scanning commands';
  SErrNoSuchStreamObject = 'No stream object %d for indirect object %d';
  SErrInvalidImageData = 'Invalid image data';
  SErrInvalidImageDataSize = 'Invalid image data size';
  SErrExpectedHexInteger = '%s: Expected a hexadecimal integer, got: %s';
  SErrExpectedIdentifierN = '%s: Expected identifier "%s", got "%s"';
  SErrExpectedName = '%s: Expected name "%s", got "%s"';

{ TPDFDocumentHelper }

procedure TPDFDocumentHelper.LoadFromFile(const aFilename: String;aOnLog: TPDFLogNotifyEvent; aOnProgress: TPDFProgressEvent);
begin
  LoadFromFile(aFileName,PDFDefaultLoadOptions,aOnLog,aOnProgress);
end;

procedure TPDFDocumentHelper.LoadFromFile(const aFilename: String;
  aOptions : TPDFLoadOptions; aOnLog: TPDFLogNotifyEvent; aOnProgress: TPDFProgressEvent);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F,aOptions, aOnLog,aOnProgress)
  finally
    F.Free;
  end;

end;

procedure TPDFDocumentHelper.LoadFromStream(const aStream: TStream;
  aOnLog: TPDFLogNotifyEvent; aOnProgress: TPDFProgressEvent);

begin
  LoadFromStream(aStream,PDFDefaultLoadOptions,aOnLog,aOnProgress);
end;

procedure TPDFDocumentHelper.LoadFromStream(const aStream: TStream;aOptions : TPDFLoadOptions;
  aOnLog: TPDFLogNotifyEvent; aOnProgress: TPDFProgressEvent);
Var
  aParser:TPDFParser;
begin
  aParser:=TPDFParser.Create(aStream);
  try
    aParser.OnLog:=aOnLog;
    aParser.OnProgress:=aOnProgress;
    aParser.LoadObjects:=loLoadObjects in aOptions;
    aParser.ResolveObjects:=loResolveObjects in aOptions;
    aParser.ResolveContentStreams:=loResolveContentStreams in aOptions;
    aParser.ResolveToUnicodeCMaps:=loResolveToUnicodeCMaps in aOptions;
    aParser.ParseDocument(Self);
  Finally
    aParser.Free;
  end;
end;

{$IFDEF DEBUGSTREAMS}
{$IFDEF DUMPSTREAMS}

Procedure DumpStream(S : Tstream; const aMessage : String);

var
  i : Integer;
  B : TBytesStream;

begin
  if S is TBytesStream then
    B:=TBytesStream(S)
  else
    begin
    B:=TBytesStream.Create([]);
    B.CopyFrom(S,0);
    end;
  try
    Write(aMessage,': ');
    For I:=0 to Length(B.Bytes)-1 do
      Write(B.Bytes[I], ' ');
    Writeln;
  finally
    if S<>B then
      B.Free;
  end;
end;
{$ENDIF}

Var
  aFileCount : Integer = 0;

procedure SaveStreamToFile(const aContext : String; aStream : TStream; const aFileName : String = '');

Var
  FN : String;
  aPos : Int64;

begin
  inc(aFileCount);
  FN:=aFileName;
  if FN='' then
    FN:=Format('streamdata-%.5d-%s.txt',[aFileCount,aContext]);
  aPos:=aStream.Position;
  with TFileStream.Create(FN,fmCreate) do
    try
      CopyFrom(aStream,0);
      aStream.Position:=aPos;
    finally
      Free;
    end;
end;
{$ENDIF}

{ TPDFParser }

function TPDFParser.CreateScanner(aFile: TStream; aBufferSize : Cardinal): TPDFScanner;
begin
  Result:=TPDFScanner.Create(aFile,aBufferSize);
end;

constructor TPDFParser.Create(aFile: TStream; aBufferSize : Cardinal);
begin
  FScanner:=CreateScanner(aFile,aBufferSize);
  FLoadObjects:=True;
  FResolveObjects:=True;
  FResolveContentStreams:=True;
  FResolveToUnicodeCMaps:=False;
end;


procedure TPDFParser.UnsupportedToken(const aMsg: UTF8String; aToken: TPDFToken
  );

Var
  N : String;
  S : RawByteString;

begin
  S:=Copy(aToken.TokenData,1,127);
  N:=GetEnumName(TypeInfo(TPDFTokenType), Ord(aToken.TokenType));
  DoError(penUnknownToken, SErrUnknownToken, [aMsg, N, S])
end;

function TPDFParser.FindIndirectObject(aObjectID, aObjectGeneration: Integer ): TPDFIndirect;

Var
  XRef : TPDFXRef;

begin
  Result:=FDoc.FindInDirectObject(aObjectID,aObjectGeneration);
  if assigned(FloadingXRef) and (aObjectID<Length(FloadingXRef)) then
    begin
    XRef:=FloadingXRef[aObjectID];
    Result:=Xref.Instance; // Should always be Nil
    if Result=nil then
      Result:=LoadXRefObject(XRef);
    end;
end;

// On exit, we're on the first token after the XREF offset.
function TPDFParser.ParseStartXREF(const aToken: TPDFToken): TPDFStartXRef;

Var
  Token : TPDFToken;
  Idx : Int64;

begin
  Result:=TPDFStartXRef.Create();
  try
    token:=FScanner.GetToken(False);
    if (Token.TokenType=ptNumber) and TryStrToInt64(Token.TokenData,Idx) then
      Result.Index:=Idx
    else
      begin
      FScanner.Unget(Token);
      UnsupportedToken(SErrXrefIndex,Token);
      end;
  except
    Result.Free;
    Raise;
  end;
end;

function TPDFParser.GetIntegerToken(const aContext: String) : Integer;

Var
  Token : TPDFToken;

begin
  Token:=FScanner.GetToken(False);
  if Not Token.IsInteger then
    DoError(penExpectedInteger,SErrExpectedInteger, [aContext, Token.TokenData])
  else
    Result:=Token.AsInteger;
end;

function TPDFParser.GetInt64Token(const aContext: String): Int64;
Var
  Token : TPDFToken;

begin
  Token:=FScanner.GetToken(False);
  if Not Token.IsInt64 then
    DOError(penExpectedInt64,SErrExpectedInt64, [aContext, Token.TokenData])
  else
    Result:=Token.AsInt64;
end;

function TPDFParser.GetStringToken(const aContext: String) : RawByteString;

Var
  Token : TPDFToken;

begin
  Token:=FScanner.GetToken(False);
  if (Token.TokenType=ptKeyword) then
    Result:=Token.TokenData
  else
    UnsupportedToken(aContext+SErrExpectedString,Token);
end;

// Parse inderect object at specified position in stream

function TPDFParser.ParseIndirectObject(aAt : Int64): TPDFIndirect;

Var
  lToken,lToken2,ltoken3 : TPDFToken;

begin
  FScanner.Reposition(aAt);
  lToken:=FScanner.GetToken(False);
  if not lToken.IsInteger then
     DoError(penNotIndirectObjectAtToken,SErrNotAnIndirectObjectAtToken,[lToken.TokenData]);
  lToken2:=FScanner.GetToken(False);
  if Not lToken.IsInteger then
    DoError(penNotIndirectObjectAtToken,SErrNotAnIndirectObjectAtToken,[lToken2.TokenData]);
  lToken3:=FScanner.GetToken(False);
  if not lToken3.CheckString(SPDFObj) then
    DoError(penNotIndirectObjectAtToken,SErrNotAnIndirectObjectAtToken,[lToken3.TokenData]);
  Result:=ParseIndirectObject(lToken.AsInt64,lToken2.AsInteger);
end;

procedure TPDFParser.ParseIndirectObjectBody(aObject: TPDFIndirect);
Var
  aObj : TPDFObject;
  aValue : TPDFValue absolute aObj;
  Done : Boolean;
  lToken : TPDFToken;

begin
  DoProgress(pkIndirect,aObject.ObjectID,Length(FloadingXRef));
  Done:=False;
  Repeat
    // Get first value token
    lToken:=FScanner.GetToken(False);
    aObj:=ParseSingleValue(lToken,True);
    if aObj is TPDFDictionary then
      FLastDict:=TPDFDictionary(aObj);
    if Assigned(aObj) then
      begin
      Done:=((aObj is TPDFValue) and (aValue.Value=SPDFEndObj)) ;
       if Done then
        aObj.Free
      else
        aObject.Add(aObj);
      end;
  until Done or FScanner.Source.IsEOF;
end;

function TPDFParser.MaybeResolve(aObject : TPDFIndirect) : TPDFIndirect;

begin
  Result:=aObject;
  if Self.ResolveObjects then
    if (aObject.ObjectType<>'') then
      Result:=aObject.ResolveObjectType;
end;

function TPDFParser.CreateCommand(const aName: string; aOperands: TPDFTokenArray
  ): TPDFCommand;

Var
  aClass : TPDFCommandClass;

begin
  aClass:= TPDFCommand.FindClassForCommand(aName);
  if aClass=Nil then
    AClass:=TPDFUnknownCommand;
  Result:=aClass.Create(aName,aOperands);
end;

procedure TPDFParser.ParseInlineImageData(var aOperands: TPDFTokenArray;
  aScanner: TPDFScanner);

Var
  aData : TPDFImageData;
  lToken: TPDFToken;
  aSize : integer;
  B : TBytes;

begin
  TPDFImageDataCommand.ParseImageOperands(aOperands,aData);
  With aData do
    aSize:=(Height*Width*((BitsPercomponent+7) div 8)) * ColorSpaceComponents;
  if (aSize<=0) then
    DoError(penInvalidImageDataSize,SErrInvalidImageDataSize)
  else
    begin
    if not (Length(aData.filters)=0) then
      Raise ENotImplemented.Create('No support for filtered image data');
    If TPDFScanner.CharacterClass(aScanner.Source.GetByte())<>ccWhitespace then
      DoError(penInvalidImageData,SErrInvalidImageData);
    B:=[];
    SetLength(B,aSize);
    aScanner.Source.CopyBytes(B,aSize);
    lToken:=Default(TPDFToken);
    lToken.TokenType:=ptByteString;
    SetLength(lToken.TokenData,aSize);
    Move(B,lToken.TokenData[1],aSize);
    aOperands[Length(AOperands)-1]:=ltoken;
    end;
end;

procedure TPDFParser.ParseContentStream(aObject: TPDFPageObject; aStream : TStream; aOnCommand : TPDFNewCommandEvent);

var
  aScanner : TPDFScanner;
  aToken : TPDFToken;
  aOperands,aTokens : TPDFTokenArray;
  CmdName : RawByteString;
  Cmd : TPDFCommand;
  I,Len : integer;

begin
  aOperands:=[];
  aTokens:=[];
  Len:=100;
  SetLength(aTokens,Len);
{$IFDEF DEBUGSTREAMS}
  SaveStreamToFile('pagecontent',aStream,'content.txt');
{$ENDIF}
  aScanner:=FScanner;

  try
    FScanner:=TPDFScanner.Create(aStream);
     While Not FScanner.Source.IsEOF do
       begin
       I:=0;
       aToken:=FScanner.GetToken(False);
       While Not (atoken.TokenType in [ptKeyWord,ptEOF]) do
         begin
         Len:=Length(aTokens);
         if I>=Len then
           begin
           inc(Len,100);
           SetLength(aTokens,Len);
           end;
         aTokens[i]:=aToken;
         Inc(I);
         aToken:=FScanner.GetToken(False);
         end;
       if (aToken.TokenType=ptEOF) then
         begin
         if (I>0) then
           DoError(penEOFWhileScanningCommands,SErrEOFWhileScanningCommands);
         end
       else
         begin
         CmdName:=atoken.AsString;
         if CmdName='ID' then
           Inc(I);
         aOperands:=Copy(aTokens,0,I);
         if CmdName='ID' then
           begin
           aOperands[I-1]:=Default(TPDFToken);
           ParseInlineImageData(aOperands,FScanner);
           end;
         Cmd:=CreateCommand(CmdName,aOperands);
         if Assigned(aOnCommand) then
           aOnCommand(Self,aObject,Cmd)
         else
           aObject.CommandList.Add(Cmd);
         end;
      end;
  finally
    if FSCanner<>aScanner then
      FreeAndNil(FScanner);
    FScanner:=aScanner;
  end;
end;

// On entry, we're on begincodespacerange.
// On exit, we're on endcodespacerange
procedure TPDFParser.ParseCMAPCodeSpaceRange(aMap: TPDFCMapData);

Var
  L : TPDFCodeSpaceRangeArray;
  aCount: Integer;
  Len : Integer;

  Procedure Grow;

  begin
    if aCount<Len then
      exit;
    Len:=Length(l);
    Len:=Len+100;
    SetLength(L,Len);
  end;

Var
  lToken :TPDFToken;
  aRange : TPDFCodeSpaceRange;

begin
  Len:=0;
  aCount:=0;
  lToken:=FScanner.GetToken(False);
  While lToken.IsHexString do
    begin
    Grow;
    aRange.RangeStart:=lToken.AsBEHexInteger;
    lToken:=FScanner.GetToken(False);
    if not ltoken.IsHexString then
      DoError(penExpectedHexInteger,SErrExpectedHexInteger,['codespacerange',ltoken.TokenData]);
    aRange.RangeEnd:=lToken.AsBEHexInteger;
    l[aCount]:=aRange;
    inc(aCount);
    lToken:=FScanner.GetToken(false);
    end;
  if not lToken.CheckString('endcodespacerange') then
    DoError(penExpectedIdentifierN,SErrExpectedIdentifierN,['codespacerange','endcodespacerange',ltoken.TokenData]);
  SetLength(L,aCount);
  aMap.CodeSpaceRange:=Concat(aMap.CodeSpaceRange,L);
end;

// On entry, we're on beginbfchar.
// On exit, we're on endbfchar
procedure TPDFParser.ParseCMAPBFChar(aMap: TPDFCMapData);

Var
  L : TPDFBFCharArray;
  aCount: Integer;
  Len : Integer;

  Procedure Grow;

  begin
    if aCount<Len then
      exit;
    Len:=Length(l);
    Len:=Len+100;
    SetLength(L,Len);
  end;

Var
  lToken :TPDFToken;
  aChar : TPDFBFChar;

begin
  Len:=0;
  aCount:=0;
  lToken:=FScanner.GetToken(False);
  While lToken.IsHexString do
    begin
    Grow;
    aChar.Src:=lToken.AsBEHexInteger;
    lToken:=FScanner.GetToken(False);
    if ltoken.IsHexString then
      aChar.Dest:=ltoken.AsBEHexInteger
    else if ltoken.IsName then
      aChar.DestName:=lToken.AsName
    else
      DoError(penExpectedHexInteger,SErrExpectedHexInteger,['bfchar',ltoken.TokenData]);
    l[aCount]:=aChar;
    inc(aCount);
    lToken:=FScanner.GetToken(False);
    end;
  if not lToken.CheckString('endbfchar') then
    DoError(penExpectedIdentifierN,SErrExpectedIdentifierN,['bfchar','endbfchar',ltoken.TokenData]);
  SetLength(L,aCount);
  aMap.BFChars:=Concat(aMap.BFChars,L);
end;

procedure TPDFParser.ParseCMAPBFRange(aMap: TPDFCMapData);

Var
  L : TPDFCIDRangeArray;
  aNames : TPDFCIDUnicodeCharOrNameArray;
  aNameCount,
  aCount: Integer;
  NameLen,
  Len : Integer;

  Procedure Grow;

  begin
    if aCount<Len then
      exit;
    Len:=Length(l);
    Len:=Len+100;
    SetLength(L,Len);
  end;

  Procedure GrowNames;

  begin
    if aNameCount<NameLen then
      exit;
    NameLen:=Length(aNames);
    NameLen:=NameLen+100;
    SetLength(aNames,NameLen);
  end;

Var
  lToken :TPDFToken;
  aRange : TPDFCIDRange;

begin
  Len:=0;
  aCount:=0;
  lToken:=FScanner.GetToken(False);
  While lToken.IsHexString do
    begin
    Grow;
    aRange.RangeStart:=lToken.AsBEHexInteger;
    lToken:=FScanner.GetToken(False);
    if not ltoken.IsHexString then
      DoError(penExpectedHexInteger,SErrExpectedHexInteger,['bfrange',ltoken.TokenData]);
    aRange.RangeEnd:=lToken.AsBEHexInteger;
    lToken:=FScanner.GetToken(False);
    if ltoken.IsHexString then
      aRange.CharOffset:=ltoken.AsBEHexInteger
    else if ltoken.TokenType=ptSquareOpen then
      begin
      aNames:=[];
      aNameCount:=0;
      NameLen:=0;
      lToken:=FScanner.GetToken(False);
      While ltoken.TokenType<>ptSquareClose do
        begin
        GrowNames;
        if ltoken.IsHexString then
          aNames[aNameCount].UnicodeChar:=lToken.AsBEHexInteger
        else if ltoken.IsName then
          aNames[aNameCount].Name:=ltoken.AsName
        else
          DoError(penExpectedName,SErrExpectedName,['bfrange',ltoken.TokenData]);
        inc(aNameCount);
        lToken:=FScanner.GetToken(False);
        end;
      SetLength(aNames,aNameCount);
      aRange.CharNames:=aNames;
      end
    else
      DoError(penExpectedHexInteger,SErrExpectedHexInteger,['bfrange',ltoken.TokenData]);
    l[aCount]:=aRange;
    inc(aCount);
    lToken:=FScanner.GetToken(False);
    end;
  if not lToken.CheckString('endbfrange') then
    DoError(penExpectedIdentifierN,SErrExpectedIdentifierN,['bfrange','endbfrange',ltoken.TokenData]);
  SetLength(L,aCount);
  aMap.BFRange:=Concat(aMap.BFRange,L);
end;


procedure TPDFParser.ParseCMap(aStream: TStream; aMap: TPDFCMap);

Var
  aScanner: TPDFScanner;
  lToken : TPDFToken;
  Data : TPDFCMapData;

begin
  aScanner:=FScanner;
  FScanner:=Nil;
  try
    FScanner:=TPDFScanner.Create(aStream);
    Data:=TPDFCMapData.Create;
    aMap.Data:=Data;
    lToken:=FScanner.GetToken(False);

    while ltoken.TokenType<>ptEOF do
      begin
      if lToken.CheckString('begincodespacerange') then
        ParseCMAPCodeSpaceRange(Data)
      else if lToken.CheckString('beginbfrange') then
        ParseCMAPBFRange(Data)
      else if lToken.CheckString('beginbfchar') then
        ParseCMAPBFChar(Data)
      else if lToken.CheckString('endcmap') then
        break;
      lToken:=FScanner.GetToken(False);
      end;
  finally
    FScanner.Free;
    FScanner:=aScanner;
  end;
end;

function TPDFParser.ParseCMap(aStream : TStream) : TPDFCMap;

begin
  Result:=TPDFCMap.Create;
  try
{$IFDEF DEBUGSTREAMS}
    SaveStreamToFile('parsecmap',aStream);
{$ENDIF}
    ParseCMap(aStream,Result);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TPDFParser.DoResolveToUnicodeCMaps(aDoc: TPDFDocument);

var
  Obj : TPDFObject;
  aCmapObj : TPDFIndirect;
  aFont : TPDFFontObject absolute obj;
  aStream : TStream;

begin
  For Obj in aDoc do
    if Obj is TPDFFontObject then
      begin
      if not Assigned(aFont.UnicodeCMap) then
        begin
        aCMApObj:=aFont.ResolveToUnicodeMapStream(aDoc);
        if assigned(aCmapObj) and assigned(aCmapObj.Stream) then
          begin
          aStream:=TBytesStream.Create(aCMAPObj.Stream.Data);
          aStream:=ResolveFilters(aStream,aCmapObj.ObjectDict);
          aCMAPObj.UnfilteredStream:=aStream;
          aFont.UnicodeCMap:=ParseCMap(aStream);
          end;
        end;
      end;
end;

// On entry, we are on obj, on exit, we are on endObj
function TPDFParser.ParseIndirectObject(aID, aGen: Int64): TPDFIndirect;

Var
  Obj : TPDFIndirect;

begin
  Result:=nil;
  Obj:=TPDFIndirect.Create();
  try
    Obj.ObjectID:=aID;
    Obj.ObjectGeneration:=aGen;
    ParseIndirectObjectBody(Obj);
    try
      Result:=MaybeResolve(Obj);
    finally
      if Result<>Obj then
        FreeAndNil(Obj);
    end;
  except
    on E : Exception do
      begin
      if E is EPDFParser then
        begin
        E.Message:=Format('(Obj %d %d) ',[aId,aGen])+E.Message;
        EPDFParser(E).ObjectGeneration:=aGen;
        EPDFParser(E).ObjectID:=aID;;
        end;
      FLastDict:=Nil;
      Result.Free;
      Raise
      end;
  end;
end;


function TPDFParser.ParseXREFItem(aIndex : Integer): TPDFXRef;

Var
  ObjectOffset : Int64;
  ObjectGeneration : Integer;
  N : String;

begin
  ObjectOffset:=GetInt64Token(SErrXREFindex);
  ObjectGeneration:=GetIntegerToken(SErrXREFVersion);
  N:=GetStringToken(SErrXREFUseToken);
  Result:=TPDFXRef.Create();
  Result.InUse:=(N='n');
  Result.ReferenceIndex:=aIndex;
  Result.ObjectOffset:=ObjectOffset;
  Result.ObjectGeneration:=ObjectGeneration;
end;

// On exit, we're on the first token after the XREF items
function TPDFParser.ParseXREF(const aToken: TPDFToken): TPDFXRefList;

Var
  I,aStartIndex,aCount : Integer;
  lToken : TPDFToken;
  Itm : TPDFXRef;

begin
  Result:=TPDFXRefList.Create();
  try
    Repeat
      aStartIndex:=GetIntegerToken(SErrXREFStartindex);
      aCount:=GetIntegerToken(SErrXRefCount);
      For I:=1 to aCount do
        begin
        Itm:=ParseXREFItem(aStartIndex);
        if aStartIndex>=Length(FloadingXRef) then
          begin
          DoWarning('Object ID %d Out Of Range [0..%d[',[aStartIndex,Length(FloadingXRef)]);
          Itm.Free;
          end
        else if FloadingXRef[aStartIndex]=Nil then
          begin
          FloadingXRef[aStartIndex]:=Itm;
          Result.Add(Itm);
          end
        else
          Itm.Free;
        Inc(aStartindex);
        DoProgress(pkXRef,I,aCount);
        end;
      lToken:=FScanner.GetToken(False);
      FScanner.Unget(lToken);
    until Not lToken.IsInteger;
  except
    Result.Free;
    Raise;
  end;
end;

// On exit, we're on the first End token or EOF
function TPDFParser.ParseMalFormed(aToken: TPDFToken; aEndToken: RawByteString
  ): TPDFMalFormed;

var
  Token : TPDFToken;
  Content : Array of TPDFToken;

begin
  Result:=Nil;
  Content:=[atoken];
  Token:=FScanner.GetToken;
  While (Token.TokenType<>ptEOF) and ((aEndToken<>'') and (Token.TokenData<>aEndToken)) do
    begin
    Content:=Concat(Content,[Token]);
    Token:=FScanner.GetToken;
    end;
  Result:=TPDFMalFormed.Create(Content);
end;

function TPDFParser.ParseStreamValue(aStartToken: TPDFToken;
  aStreamLength: Integer): TPDFStream;

Var
  S : TBytesStream;
  B : TBytes;
  lToken : TPDFToken;

begin
  if not aStartToken.CheckString(SPDFStream) then
    DoError(penNotAtStreamStart,SErrNotAtStreamStart,[aStartToken.TokenData]);
  B:=[];
  Result:=Nil;
  if FScanner.FindByte(10)=-1 then
    DoError(penExpectedWhiteSpaceAfterStream,SErrExpectedWhiteSpaceAfterStream);
  FScanner.Source.Next;
  SetLength(B,aStreamLength);
  if aStreamLength>0 then
    begin
    FScanner.CopyBytes(B,aStreamLength);
    lToken:=FScanner.GetToken(False);
    end
  else
    begin
    S:=TBytesStream.Create(B);
    try
      FScanner.ReadStreamData(S);
    finally
      B:=S.Bytes;
    end;
    end;
  if Not ((lToken.TokenType=ptKeyword) and (lToken.TokenData=SPDFEndStream)) then
    DoError(penInvalidStreamEnd,SErrInvalidStreamEnd, [ltoken.TokenType, ltoken.TokenData]);
  Result:=TPDFStream.Create(B);
end;

// Start on [, end at ]
function TPDFParser.ParseArray(aPrevToken: TPDFToken): TPDFArray;

var
  lToken : TPDFToken;
  V : TPDFObject;

begin
  if aPrevToken.TokenType<>ptSquareOpen then
    DoError(penNoArrayStartToken,SErrNoArrayStartToken,[aPrevToken.TokenData]);
  Result:=TPDFArray.Create();
  lToken:=FScanner.GetToken(False);
  While Not (lToken.TokenType in [ptSquareClose,ptEOF]) do
    begin
    V:=ParseSingleValue(lToken,True);
    if V<>Nil then
      Result.Add(V);
    lToken:=FScanner.GetToken(False);
    end;
end;

function TPDFParser.FindStreamLength(aReposition: Int64): Integer;

Var
  aVal : TPDFObject;
  Ref : TPDFRef absolute aVal;
  Obj : TPDFIndirect;


begin
  if Not Assigned(FLastDict) then
    DoError(penNoDictionaryForLength,SErrNoDictionaryForLength);
  aVal:=FLastDict.FindValue(SPDFKeyLength);
  if Not Assigned(aVal) then
    DoError(penNoLengthEntryInDictionary,SErrNoLengthEntryInDictionary);
  if (aVal is TPDFValue) then
    Result:=TPDFValue(aVal).AsInteger
  else if aVal is TPDFRef then
    begin
    Obj:=FindIndirectObject(Ref.ObjectID,Ref.ObjectGeneration);
    if Not assigned(Obj) then
      begin
      with Ref do
        DoError(penDictionaryNoLengthObject,SErrDictionaryNoLengthObject,[Ref.ObjectID,Ref.ObjectGeneration]);
      end
    else
      begin
      if Not ((Obj.Count=1) and Obj.Objects[0].InheritsFrom(TPDFValue)) then
        DoError(penDictionaryNoLengthObject,SErrDictionaryNoLengthObject,[Ref.ObjectID,Ref.ObjectGeneration]);
      aVal:=Obj.Objects[0];
      Result:=TPDFValue(aVal).AsInteger
      end;
    if FScanner.Position<>aRePosition then
      FScanner.Reposition(aRePosition,True);
    end
  else
    DoError(penDictionaryLengthNotValue,SErrDictionaryLengthNotValue);
end;

// On entry, we're on the first token of the value or whitespace
function TPDFParser.ParseSingleValue(aStartToken: TPDFToken; SkipWhiteSpace : Boolean): TPDFObject;

begin
  Result:=nil;
  if (aStartToken.isWhiteSpace) and SkipWhiteSpace  then
    aStartToken:=FScanner.GetToken(False);
  Case aStartToken.TokenType of
    ptWhiteSpace:
      Result:=Nil;
    ptKeyword:
      begin
      Case LowerCase(aStartToken.TokenData) of
        SPDFStream :
          begin
          Result:=ParseStreamValue(aStartToken,FindStreamLength(FScanner.Position));
          end
      else
        Result:=TPDFValue.Create(aStartToken.TokenData,aStartToken.TokenType);
      end;
      end;
    ptName:
      Result:=TPDFValue.Create(aStartToken.TokenData,aStartToken.TokenType);
    ptShl:
      Result:=ParseDictionary(aStartToken);
    ptNumber:
      Result:=TPDFValue.Create(aStartToken.TokenData,aStartToken.TokenType);
    ptHexString,
    ptString:
      Result:=TPDFValue.Create(aStartToken.AsString,aStartToken.TokenType);
    ptSquareOpen:
      Result:=ParseArray(aStartToken);
    else
      Result:=Nil;
      // if Not (aStartToken.TokenType in StopAt) then
      //  Raise EPDFParser.CreateFmt('Unknown value from token type %d : %s',[Ord(aStartToken.TokenType),aStartToken.TokenData]);
    end;

end;

// On entry, we're still on the start token of the value
function TPDFParser.ParseDictValue: TPDFObject;

Var
  aObj: TPDFObject;
  aID,aGen : Int64;
  lToken : TPDFToken;
  aValue : TPDFValue absolute aObj ;


begin
  lToken:=FScanner.GetToken;
  aObj:=ParseSingleValue(lToken,True);
  if (aObj is TPDFValue) and aValue.IsInt64 then
      begin
      aID:=aValue.AsInt64;
      lToken:=FSCanner.GetToken(false);
      if (lToken.TokenType in [ptName,ptShr,ptEOF]) then
        FScanner.Unget(lToken)
      else if lToken.IsInteger then
        begin
        FreeAndNil(aObj);
        aGen:=lToken.AsInt64;
        lToken:=FScanner.GetToken(False);
        if not lToken.CheckString(SPDFTokenR) then
          DoError(penInvalidDictionaryRef,SErrInvalidDictionaryRef,[lToken.TokenData]);
        aObj:=TPDFRef.Create(aID,aGen)
        end
      else if lToken.isString and not lToken.CheckString(SPDFPageKeyID) then
        FScanner.Unget(lToken)
      end;
  Result:=aObj;
end;

function TPDFParser.ParseValue(StopAt: TPDFTokenTypes): TPDFObject;

// Starts before First token of value
// Ends at last token of value
var
  lToken : TPDFToken;
  Container : TPDFContainer;

begin
  Result:=Nil;
  Container:=Nil;
  lToken:=FScanner.GetToken(ptWhiteSpace in StopAt);
  repeat
    If (Result<>Nil) and (Container=Nil) then
      begin
      Container:=TPDFContainer.Create();
      Container.Add(Result);
      Result:=Nil;
      end;
    Result:=ParseSingleValue(lToken,Not (ptWhiteSpace in StopAt));
    If (Result<>Nil) and (Container<>Nil) then
      begin
      Container.Add(Result);
      Result:=Nil;
      end;
    lToken:=FScanner.GetToken(ptWhiteSpace in StopAt);
    if Not (lToken.TokenType in StopAt) then
      lToken:=FScanner.GetToken(ptWhiteSpace in StopAt);
  Until (lToken.TokenType in StopAt);
  If Result=Nil then
    Result:=Container;
  FScanner.Unget(lToken);
end;





// On entry, we're at <<, on exit we're on >>

procedure TPDFParser.DoParseDictionary(aDict: TPDFDictionary;
  const aToken: TPDFToken);

Var
  aName : RawByteString;
  aValue : TPDFObject;
  lToken : TPDFToken;

begin
  aName:='';
  aValue:=Nil;
  if aToken.TokenType<>ptShl then
    DoError(penNotAtDictStart,SErrNotAtDictStart,[aToken.TokenData]);
  lToken:=FScanner.GetToken(False);
  While Not (lToken.TokenType in [ptEOF,ptShr]) do
    begin
    if lToken.TokenType<>ptName then
      DoError(penNotDictKeyToken,SErrNotDictKeyToken,[lToken.TokenData]);
    aName:=lToken.TokenData;
    aValue:=ParseDictValue();
    aDict.AddEntry(aName,aValue);
    lToken:=FScanner.GetToken(False);
    end;
  if lToken.TokenType<>ptShr then
    DoError(penNotAtDictEnd,SErrNotAtDictEnd,[aToken.TokenData]);
end;

class procedure TPDFParser.AsciiHEXDecode(aSrc, aDest: TStream);

Var
  B,B2 : TBytes;
  Direct : Boolean;

begin
  if aSrc is TBytesStream then
    B:=TBytesStream(aSrc).Bytes
  else
    begin
    SetLength(B,aSrc.Size);
    aSrc.ReadBuffer(B[0],aSrc.Size);
    end;
  Direct:=(aDest is TBytesStream) and (aDest.Size>=(Length(B) div 2));
  if Direct then
    B2:=TBytesStream(aSrc).Bytes
  else
    begin
    SetLength(B2,(aSrc.Size div 2));
    aSrc.ReadBuffer(B[0],aSrc.Size);
    end;
  HexToBin(PChar(B),PChar(B2),Length(B2));
  if not Direct then
    aDest.WriteBuffer(B2[0],Length(B2));
end;

class function TPDFParser.AsciiHEXDecode(aSrc: TStream): TStream;

var
  B : TBytes;

begin
  B:=[];
  SetLength(B,aSrc.Size div 2);
  Result:=TBytesStream.Create(B);
  try
    AsciiHEXDecode(aSrc,Result);
  except
    Result.Free;
    Raise;
  end;
end;

class procedure TPDFParser.Ascii85Decode(aSrc, aDest: TStream);

Var
  Decode : TASCII85DecoderStream;

begin
  Decode:=TASCII85DecoderStream.create(aSrc);
  try
    Decode.SourceOwner:=False;
    aDest.CopyFrom(Decode,0);
  finally
    Decode.Free;
  end;
end;

class function TPDFParser.Ascii85Decode(aSrc: TStream): TStream;
begin
  Result:=TBytesStream.Create([]);
  try
    Ascii85Decode(aSrc,Result);
  except
    Result.Free;
    Raise;
  end;
end;

class procedure TPDFParser.LZWDecode(aSrc, aDest: TStream);
Var
  Defl : TLZWDecompressionStream;

begin
  Defl:=TLZWDecompressionStream.create(aSrc,[zoTIFFCodes]);
  try
    Defl.SourceOwner:=False;
    aDest.CopyFrom(aSrc,0);
  finally
    Defl.Free;
  end;
end;

class procedure TPDFParser.Deflate(aSrc, aDest: TStream);

Var
  Defl : TDecompressionStream;

begin
  Defl:=TDecompressionStream.create(aSrc,False);
  try
    Defl.SourceOwner:=False;
    aDest.CopyFrom(Defl,0);
  finally
    Defl.Free;
  end;
end;

class function TPDFParser.LZWDecode(aSrc: TStream): TStream;

begin
  Result:=TBytesStream.Create([]);
  try
    LZWDecode(aSrc,Result);
  except
    Result.Free;
    Raise;
  end;
end;

class function TPDFParser.Deflate(aSrc: TStream): TStream;

begin
  Result:=TBytesStream.Create([]);
  try
    Deflate(aSrc,Result);
  except
    Result.Free;
    Raise;
  end;
end;

class procedure TPDFParser.RunlengthDecode(aSrc, aDest: TStream);

Var
  I : Integer;
  RLE,B,Cnt,C : Byte;
  {%H-}Buf : Array[0..128] of byte;

begin
  RLE:=128;
  For I:=0 to aSrc.Size-1 do
    begin
    aSrc.Read(RLE,1);
    if RLE<128 then
      aDest.CopyFrom(aSrc,RLE)
    else if RLE>128 then
      begin
      B:=aSrc.ReadByte;
      CNT:=257-RLE;
      For C:=0 to Cnt-1 do
        Buf[C]:=B;
      aDest.WriteBuffer(Buf,Cnt);
      end
    else if RLE=128 then
      Break;
    end;
end;

class function TPDFParser.RunlengthDecode(aSrc: TStream): TStream;
begin
  Result:=TBytesStream.Create([]);
  try
    RunlengthDecode(aSrc,Result);
  except
    Result.Free;
    Raise;
  end;

end;

// On entry, we're at <<, on exit we're on >>
function TPDFParser.ParseDictionary(const aToken: TPDFToken): TPDFDictionary;

begin
  if aToken.TokenType<>ptShl then
    DoError(penNotAtDictStart,SErrNotAtDictStart,[aToken.TokenData]);
  Result:=TPDFDictionary.Create();
  try
    DoParseDictionary(Result,aToken);
  except
    Result.Free;
    Raise;
  end;
end;

// on entry, we're on 'Trailer';
function TPDFParser.ParseTrailer(const aToken: TPDFToken): TPDFTrailer;

Var
  lToken : TPDFToken;

begin
  if not aToken.CheckString(SPDFTrailer) then
    DoError(penNotOnTrailerDict,SErrNotOnTrailerDict,[aToken.TokenData]);
  Result:=TPDFTrailer.Create();
  try
    lToken:=FScanner.GetToken(False);
    if lToken.TokenType<>ptShl then
      DoError(penInvalidTrailer,SErrInvalidTrailer);
    DoParseDictionary(Result,LToken);
  except
    Result.Free;
    Raise;

  end;
end;

function TPDFParser.GetObject: TPDFObject;

Var
  Content : Array of TPDFToken;
  Token,Token2,Token3 : TPDFToken;
  TokenStr : RawByteString;
  Context : TPDFContext;
  I1,I2 : Int64;

begin
  Result:=Nil;
  Context:=cNone;
  Content:=[];
  Repeat
    Token:=FScanner.GetToken(Context=cObj);
    TokenStr:=Token.TokenData;
  //  Writeln('Examining Token (',Token.TokenType,') : >>>',Token.TokenData,'<<<');
    Case Token.TokenType of
      ptEOF:
        exit;
      ptComment:
        begin
        if Context<>cNone then
          Content:=Concat(Content,[Token])
        else
          begin
          Result:=TPDFComment.Create(Token.TokenData);
          Exit;
          end;
        end;
      ptName:
        begin
          Token2:=FScanner.GetToken(True);
          if Context<>cNone then
            begin
            Token.TokenType:=ptName;
            Token.TokenData:=Token.TokenData+Token2.TokenData;
            Content:=Concat(Content,[Token]);
            end
          else
            begin
            DoUnknownStruct('20221112110649');
            end;
        end;
      ptShl,ptShr,
      ptSquareOpen,ptSquareClose,
      ptCurlyOpen,ptCurlyClose,
      ptHexString,
      ptString:
        begin
        if Context<>cNone then
          Content:=Concat(Content,[Token])
        else
          DoUnknownStruct('20221112110702');
        end;
      ptWhiteSpace:
        begin
        if Context<>cNone then
          Content:=Concat(Content,[Token])
        else
          DoUnknownStruct('20221112110708');
        end;
     else // Case
        if TryStrToInt64(TokenStr,I1) then
          begin
          Token2:=FScanner.GetToken(False);
          if TryStrToInt64(Token2.TokenData,I2) then
            begin
            Token3:=FScanner.GetToken(False);
            if Token3.TokenData=SPDFObj then
              begin
              Context:=cNone;
              Content:=[];
              Exit(ParseIndirectObject(I1,I2))
              end
            else
              begin
              FScanner.Unget(Token3);
              FScanner.Unget(Token2);
              DoUnknownStruct('20221112110726');

              end;
            end
          else
            begin
            FScanner.Unget(Token2);
            DoUnknownStruct('20221112110742');
            end;
          end
        else if TokenStr=SPDFTrailer then
          begin
          Context:=cNone;
          Content:=[];
          exit(ParseTrailer(Token));
          end
        else if TokenStr=SPDFXref then
          begin
          Context:=cNone;
          Content:=[];
          exit(ParseXREF(Token));
          end
        else if TokenStr=SPDFStartxref then
          begin
          Context:=cNone;
          Content:=[];
          Exit(ParseStartXReF(Token));
          end
        else
          begin
          Context:=cNone;
          Content:=[];
          Exit(ParseMalFormed(Token,''));
          end;

    end; // Case
  Until Token.TokenType=ptEOF;
end;

function TPDFParser.VerifyDocument: Int64;

Var
  lToken : TPDFToken;
  Ver : String;

begin
  if Not FScanner.CompareBytes('%PDF-') then
    DoError(penMissingPDFHeader,SErrMissingPDFHeader);
  lToken:=FScanner.GetToken;
  if (lToken.TokenType<>ptNumber) then
    DoError(penMissingPDFVersion,SerrMissingPDFversion);
  Ver:=lToken.TokenData;
  if Ver='' then
    DoError(penInvalidPDFversion,sErrInvalidPDFversion,[ver]);
  FDoc.PDFversion:=Copy(Ver,2,Length(Ver)-1);
  FScanner.Reposition(-1,False);
  Result:=FScanner.FindBytesBackWard('%%EOF',False);
  if Result=-1 then
    DoError(penPDFEOFNotFound,SErrPDFEOFNotFound);
end;

function TPDFParser.GetLastXRef(StartAt: Int64; out FoundAt : Int64): TPDFStartXRef;

Var
  lToken : TPDFToken;

begin
  if (FScanner.Position<>StartAt) then
    FScanner.Reposition(StartAt,False);
  if FScanner.FindBytesBackward(SPDFStartXRef,False)=-1 then
    DoError(penNoStartXRef,SErrNoStartXRef,[StartAt]);
  lToken:=FScanner.GetToken(False);
  if (lToken.TokenData<>SPDFStartxref) then
    DoError(penNoStartXRef,SErrNoStartXRef,[lToken.TokenPos]);
  FoundAt:=lToken.TokenPos;
  Result:=ParseStartXREF(lToken);
end;

class procedure TPDFParser.Unpredict(var Data: TPDFFilterData);

Var
  Tmp : TBytesStream;
  Buf,B2 : TBytes;
  aCount : Integer;
  aPredictor,aColors,aColumns,aBitsPerComponent : Integer;
  aStream : TPDFPredictStream;

  Function MaybeInt(aKey : string; const aDefault : integer) : Integer;

  begin
  if Data.ParamDict.ContainsKey(aKey) then
    Result:=Data.ParamDict.GetIntegerValue(aKey)
  else
    Result:=aDefault;
  end;

begin
  Buf:=[];
  B2:=[];
  if Data.ParamDict.ContainsKey(SPDFKeyPredictor) then
    begin
    aPredictor:=MaybeInt(SPDFKeyPredictor,1);
    if aPredictor<>1 then
      begin
      aColumns:=MaybeInt(SPDFKeyColumns,1);
      aColors:=MaybeInt(SPDFKeyColors,1);
      aBitsperComponent:=MaybeInt(SPDFKeyBitsPerComponent,8);
      Data.Dest.Position:=0;
      tmp:=nil;
      aStream:=TPDFPredictStream.Create(Data.Dest,aPredictor,aColumns,aColors,aBitsPerComponent);
      try
        SetLength(B2,Data.Dest.Size);
        Tmp:=TBytesStream.Create(B2);
        Tmp.Position:=0;
        aStream.SourceOwner:=True;
        SetLength(Buf,Data.Dest.Size);
        Repeat
          aCount:=aStream.Read(Buf[0],Length(Buf));
          if aCount>0 then
            Tmp.WriteBuffer(Buf[0],aCount);
        Until (aCount=0);
{$IFDEF DEBUGSTREAMS}
        // DumpStream(Tmp,'Buffer');
{$ENDIF}
        Tmp.Position:=0;

        Data.Dest:=Tmp;
      finally
        if Data.Dest<>Tmp then
          Tmp.Free;
        aStream.Free;
      end;
      end;
    end;
end;


function TPDFParser.FilterStream(aStream : TStream; const aFilterName : String; aParams : TPDFDictionary) : TStream;

Var
  Data : TPDFFilterData;

begin
  Result:=Nil;
  Data:=Default(TPDFFilterData);
  Data.FilterName:=aFilterName;
  Data.Source:=aStream;
  Data.ParamDict:=aParams;
  if (Data.FilterName<>'') and (Data.FilterName[1]='/') then
    Delete(Data.FilterName,1,1);
  try
    Case Data.Filtername of
      SPDFFilterFlateDecode :
        begin
        Data.Dest:=Deflate(Data.Source);
        if assigned(Data.ParamDict) and Data.ParamDict.ContainsKey(SPDFKeyPredictor) then
          Unpredict(Data);
{$IFDEF DUMPSTREAMS}
        DumpStream(Data.Dest,'Unpredicted Buffer data');
{$ENDIF}
        end;
      SPDFFilterLZWDecode:
        begin
        Data.Dest:=LZWDecode(Data.Source);
        if assigned(Data.ParamDict) and Data.ParamDict.ContainsKey(SPDFKeyPredictor) then
          Unpredict(Data);
  {$IFDEF DUMPSTREAMS}
        DumpStream(Data.Dest,'Unpredicted Buffer data');
  {$ENDIF}
        end;
      SPDFFilterASCIIHexDecode:
        Data.Dest:=ASCIIHexDecode(Data.Source);
      SPDFFilterASCII85Decode:
        Data.Dest:=ASCII85Decode(Data.Source);
      SPDFFilterRunLengthDecode:
        Data.Dest:=ASCII85Decode(Data.Source);
    else
      if Assigned(FOnUnknownFilter) then
        FOnUnknownFilter(Self,Data)
      else
        DoError(penUnknownFilter,SErrUnknownFilter,[aFilterName]);
    end;
    Result:=Data.Dest;
  except
    Data.Dest.Free;
    Raise;
  end;
end;

function TPDFParser.ResolveFilters(aStream : TStream; aDict : TPDFDictionary): TStream;

var
  aFilters : TPDFObject;
  aFilter : TPDFObject;
  aPDFValue : TPDFValue absolute aFilter;
  aParams : TPDFObject;
  aParamDict : TPDFDictionary;
  aRes, aRes2 : TStream;

begin
  aRes:=aStream;
  aFilters:=aDict.FindValue(SPDFKeyFilter);
  aParams:=aDict.FindValue(SPDFKeyDecodeParms);
  if (aParams<>Nil) and (aParams is TPDFDictionary) then
    aParamDict:=TPDFDictionary(aParams)
  else if aParams is TPDFArray then
    DoUnknownStruct('20221113170651')
  else
    aParamDict:=Nil;
  if assigned(aFilters) then
    For aFilter in aFilters do
      begin
      if aFilter is TPDFValue then
        begin
        aRes2:=FilterStream(aRes,aPDFValue.Value,aParamDict);
        aRes.Free;
        aRes:=aRes2;
        aRes.Position:=0;
  {$IFDEF DUMPSTREAMS}
        DumpStream(aRes,'Filtered stream intermediate result');
  {$ENDIF}
        end
      else
        DoError(penUnknownFilterValueClass,SErrUnknownFilterValueClass, [aFilter.ClassName]);
      end;
  Result:=aRes;
end;

function TPDFParser.GetXRefStreamSubSections(aObjectDict : TPDFDictionary) : TPDFIndexPairArray;

Var
  I,aSize : integer;
  aPair : TPDFIndexPair;
  Idx : TPDFArray;
  Index : TPDFIndexPairArray;

begin
  Index:=[];
  aSize:=aObjectDict.GetIntegerValue(SPDFKeySize);
  if aObjectDict.ContainsKey(SPDFKeyIndex) then
    Idx:=aObjectDict.GetArrayValue(SPDFKeyIndex)
  else
    Idx:=nil;
  if not Assigned(Idx) then
    begin
    SetLength(Index,1);
    aPair[1]:=0;
    aPair[2]:=aSize;
    Index[0]:=aPair;
    end
   else
    begin
    SetLength(Index,Idx.Count Div 2);
    I:=0;
    While (I<Idx.Count) do
      begin
      aPair[1]:=Idx.GetIntegerAt(I);
      aPair[2]:=Idx.GetIntegerAt(I+1);
      index[I div 2]:=aPair;
      Inc(I,2);
      end;
    end;
  Result:=Index;
end;

function TPDFParser.ParseXREFStream(aStream : TStream; aObjectDict : TPDFDictionary) : TPDFXRefList;

  Function GetData (Var P : PByte; aSize : Integer; aDefault : Integer) : Integer;

  Var
    I : Integer;

  begin
    Result:=0;
    if aSize=0 then
      Result:=aDefault
    else
      For I:=1 to aSize do
        begin
        // write(' ',P^);
        Result:=256*Result+P^;
        Inc(P);
        end;
  end;


Var
  Entry : Array of Byte;
  Sizes : Array[0..2] of Byte;
  Indexes : TPDFIndexPairArray;
  Fields : Array[0..2] of Integer;
  aID,aFirst : integer;
  aPair : TPDFIndexPair;
  O,O2 : TPDFObject;
  W : TPDFArray absolute O;
  V : TPDFValue absolute O2;
  I,J,aSize : Integer;
  D : PByte;
  Itm: TPDFXRef;


begin
  Entry:=[];
  aSize:=aObjectDict.GetIntegerValue(SPDFKeySize);
  if Length(FloadingXRef)=0 then
    Setlength(FloadingXRef,aSize)
  else if aSize>Length(FloadingXRef) then
    begin
    DoWarning('Increasing size of XRef table from %d to %d',[Length(FloadingXRef),aSize]);
    Setlength(FloadingXRef,aSize);
    end;
  W:=aObjectDict.GetArrayValue(SPDFKeyW);
  For I:=0 to 2 do
    begin
    O2:=W.Objects[I];
    if Not ((O2 is TPDFValue) and V.IsInteger) then
      DoError(penSizeElementNotInteger, SerrSizeElementNotInteger ,[i]);
    Sizes[i]:=V.AsInteger;
    end;
  // Writeln(AObjectDict.GetDescription);
  // Writeln('Sizes : ',Sizes[0],',',Sizes[1],',',Sizes[2]);
  SetLength(Entry,Sizes[0]+Sizes[1]+Sizes[2]);
{$IFDEF DUMPSTREAMS}
  DumpStream(aStream,'Stream');
{$ENDIF}
  //Writeln('Stream: ');
  Indexes:=GetXRefStreamSubSections(aObjectDict);
  Result:=TPDFXRefList.Create();
  try
    for I:=0 to Length(Indexes)-1 do
      begin
      aPair:=Indexes[I];
      aFirst:=aPair[1];
      aSize:=aPair[2];
      For J:=0 to aSize-1 do
        begin
        aStream.ReadBuffer(Entry[0],Length(Entry));
        {
        Write('Entry : (');
        For j:=0 to Length(Entry)-1 do
          System.Write(Entry[j],' ');
        writeln(')');
        }
        D:=@Entry[0];
        Fields[0]:=GetData(D,Sizes[0],1);
        Fields[1]:=GetData(D,Sizes[1],0);
        Fields[2]:=GetData(D,Sizes[2],0);
        aID:=aFirst+J;
        // Writeln;
        // Writeln('Compressed Xref (',aID,') : ',Fields[0],',',Fields[1],',',Fields[2]);
        if FloadingXRef[aID]=Nil then
          begin
          Itm:=TPDFXRef.Create();
          Itm.ReferenceIndex:=aID;
          Itm.Inuse:=Fields[0]<>0;
          Itm.Compressed:=(Fields[0]=2);
          Case Fields[0] of
          0 :
            begin
            Itm.NextFreeObject:=Fields[1];
            Itm.ObjectGeneration:=Fields[2];
            end;
          1 :
            begin
            Itm.ObjectOffset:=Fields[1];
            Itm.ObjectGeneration:=Fields[2];
            end;
          2:
            begin
            Itm.StreamObjectNr:=Fields[1];
            Itm.ObjectIndex:=Fields[2];
            end;
          end;
          FloadingXRef[aID]:=Itm;
        // Writeln(' -> ',Itm.GetDescription);
          Result.Add(Itm);
          end;
        DoProgress(pkXref,J,aSize);
        end;
      end;
  except
    Result.Free;
    Raise;
  end;
end;

function TPDFParser.GetUnfilteredStream(aObj : TPDFIndirect) : TStream;

var
  aStream : TStream;


begin
  Result:=aObj.UnfilteredStream;
  if Result<>nil then
    exit;
  aStream:=TBytesStream.Create(aObj.Stream.Data);
  try
    aStream:=ResolveFilters(aStream,aObj.ObjectDict);
    aStream.Position:=0;
    aObj.UnfilteredStream:=aStream;
    Result:=aStream;
  except
    aStream.Free;
    Raise;
  end;
end;

function TPDFParser.ParseXRefAt(aStartPos: Int64; out ParentObject : TPDFIndirect): TPDFXRefList;

Var
  lToken,lToken2,lToken3 : TPDFToken;
  aStream : TStream;

begin
  // Writeln('Parsing XREF at : ',aStartPos);
  Result:=Nil;
  ParentObject:=Nil;
  if (FScanner.Position<>aStartPos) then
    FScanner.Reposition(aStartPos,True);
  lToken:=FScanner.GetToken(False);
  if (lToken.TokenData=SPDFXref) then
    Result:=ParseXREF(lToken)
  else
    if (lToken.IsInteger) then
      begin
      lToken2:=FScanner.GetToken(False);
      if lToken2.IsInteger then
        begin
        lToken3:=FScanner.GetToken(False);
        if lToken3.CheckString(SPDFObj) then
          begin
          ParentObject:=ParseIndirectObject(ltoken.AsInteger,ltoken2.AsInteger);
          if (ParentObject.ObjectType=SPDFTypeXref) then
            begin
            if Not (ParentObject is TPDFIndirectXRef) then
              // This ongtre MUST be done
              ParentObject:=ParentObject.ResolveObjectType;
            aStream:=GetUnfilteredStream(ParentObject);
            Result:=ParseXREFStream(aStream,ParentObject.ObjectDict);
            (ParentObject as TPDFIndirectXRef).XRef:=Result;
            Result.Compressed:=True;
            // Writeln('Parent : ',ParentObject.ObjectID);
            end;
          end;
        end;
      end;
  if (Result=Nil) then
    DoError(penNoXrefAt,SErrNoXRefAt,[aStartPos])
end;

function TPDFParser.ParseStreamObjectPositions(aSTMObj: TPDFIndirect ) :TPDFObjectPositionArray;

Var
  I,N : Integer;
  Position : TPDFObjectPosition;
  lToken : TPDFToken;

begin
  Result:=[];
  N:=aStmObj.ObjectDict.GetIntegerValue(SPDFKeyN);
  SetLength(Result,N);
  For I:=0 to N-1 do
    begin
    lToken:=FScanner.GetToken(False);
    if Not lToken.IsInteger then
      DoError(penObjStmObjectIDInvalid, SErrObjStmObjectIDInvalid ,[aSTMObj.ObjectID,I]);
    Position.ID:=lToken.AsInteger;
    lToken:=FScanner.GetToken(False);
    if Not lToken.IsInteger then
      DoError(penObjStmObjectOffsetInvalid, SErrObjStmObjectOffsetInvalid,[aSTMObj.ObjectID,I]);
    Position.Offset:=lToken.AsInteger;
    Result[i]:=Position;
    end;
  aStmObj.ObjectPositions:=Result;
end;

procedure TPDFParser.SetResolveContentStreams(AValue: Boolean);
begin
  if FResolveContentStreams=AValue then Exit;
  FResolveContentStreams:=AValue;
  if aValue then
    ResolveObjects:=true;
end;

procedure TPDFParser.SetResolveToUnicodeCMaps(AValue: Boolean);
begin
  if FResolveToUnicodeCMaps=AValue then Exit;
  FResolveToUnicodeCMaps:=AValue;
  if aValue then
    ResolveObjects:=True;
end;

procedure TPDFParser.DoProgress(aKind: TPDFProgressKind; aCurrent, aCount: Integer);
begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,aKind,aCurrent,aCount)
end;

procedure TPDFParser.DoLog(aKind: TPDFLogKind; const Msg: string);
begin
  if assigned(FonLog) then
    FonLog(Self,aKind,Msg);
end;

procedure TPDFParser.DoLog(aKind: TPDFLogKind; const Fmt: string;
  const Args: array of const);
begin
  DoLog(aKind,Format(Fmt,Args));
end;

procedure TPDFParser.DoInfo(const Msg: string);
begin
  DoLog(lkInfo,Msg);
end;

procedure TPDFParser.DoInfo(const Fmt: string; const args: array of const);
begin
  DoInfo(Format(Fmt,Args));
end;

procedure TPDFParser.DoWarning(const Msg: string);
begin
  DoLog(lkWarning,Msg);
end;

procedure TPDFParser.DoWarning(const Fmt: string; const args: array of const);
begin
  DoWarning(Format(Fmt,Args));
end;

procedure TPDFParser.DoError(const Nr: Integer; const Msg: string);

Var
  Err : EPDFParser;

begin
  DoLog(lkError,'Error nr %d : %s',[Nr,Msg]);
  Err:=EPDFParser.Create(Msg);
  Err.ErrorNumber:=nr;
  Raise Err;
end;


procedure TPDFParser.DoError(const Nr: Integer; const Fmt: string;
  const Args: array of const);
begin
  DoError(Nr,Format(Fmt,Args))
end;

procedure TPDFParser.DoUnknownStruct(const aID: String);
begin
  DoError(penUnknownConstruct,SErrUnknownConstruct,[aID]);
end;


function TPDFParser.ParseIndirectObjectInStream(aSTMObj: TPDFIndirect; aIdx,
  aObjectID: Integer): TPDFIndirect;

Var
  aStream,wstream : TStream;
  First : Integer;
  aScanner : TPDFScanner;
  aEndOffset : Integer;
  Position : TPDFObjectPosition;
  Positions : TPDFObjectPositionArray;
  aStartOffset : Integer;
  Obj : TPDFIndirect;
  {$IFDEF DEBUGSTREAMS}
  sname : string;
  {$endif}
begin
  DoProgress(pkIndirect,aObjectID,Length(FloadingXRef));
{$IFDEF DEBUGSTREAMS}
  SName:='objectstream-'+IntToStr(aObjectID);
  Writeln('Searching Indirect object stream. ID ',aObjectID,' index ',aIdx);
{$ENDIF}
  if aSTMObj.ObjectType<>SPDFTypeObjStm then
    DoError(penNotObjectStream,SErrObjectIsNotObjectStream,[aSTMObj.ObjectID]);
  if aStmObj.ObjectDict=Nil then
    DoError(penObjectStreamWithoutDict,SErrStreamObjectWithoutDict,[aSTMObj.ObjectID]);
  aStream:=GetUnFilteredStream(aSTMObj);
{$IFDEF DEBUGSTREAMS}
  SaveStreamToFile(sname,aStream);
{$ENDIF}
  wStream:=Nil;
  aScanner:=Fscanner;
  try
    FScanner:=TPDFScanner.Create(aStream);
    if Length(aStmObj.ObjectPositions)>0 then
      Positions:=aStmObj.ObjectPositions
    else
      Positions:=ParseStreamObjectPositions(aStmObj);
    First:=aStmObj.ObjectDict.GetIntegerValue(SPDFKeyFirst);
    Position:=Positions[aIdx];
    aStartOffset:=First+Position.Offset;
    if aIdx<Length(Positions)-1 then
      aEndOffset:=First+Positions[aIdx+1].Offset
    else
      aEndOffset:=aStream.Size;
    aStream.Position:=aStartOffset;
    wStream:=TWindowedStream.Create(aStream,aEndOffset-aStartOffset);
    {$IFDEF DEBUGSTREAMS}
    SaveStreamToFile(sname+'-idx-'+IntToStr(aIdx),wStream);
    {$ENDIF}
    FreeAndNil(FScanner);
    FScanner:=TPDFScanner.Create(wStream);
    Result:=TPDFIndirect.Create();
    Result.ObjectID:=Position.ID;
    if Position.ID<>aObjectID then
      DoWarning(SWarnObjectIDDoesNotMatch,[aIdx,Result.ObjectID,aObjectID]);
    try
      ParseIndirectObjectBody(Result);
    except
      Result.Free;
      Raise;
    end;
    if ResolveObjects then
      begin
      Obj:=Result;
      try
        Result:=Obj.ResolveObjectType;
      finally
        if Obj<>Result then
          Obj.Free;
      end;
      end;
  finally
    wStream.Free;
    if FScanner<>aScanner then
      FScanner.Free;
    FScanner:=aScanner;
  end;
end;

function TPDFParser.DefaultPDFPageClass: TPDFPageClass;
begin
  Result:=TPDFPageObject;
end;

function TPDFParser.LoadXREFobject(Itm: TPDFXRef; addToDocument : Boolean = True): TPDFIndirect;

var
  StmRef : TPDFXRef;
  StmObj: TPDFIndirect;

begin
  Result:=Nil;
{  Writeln('Checking ',Itm.GetDescription);
  if Itm.ObjectIndex=131 then
    Writeln('Indirect');}
  if not Itm.Compressed then
    begin
    Result:=ParseIndirectObject(Itm.ObjectOffset);
    end
  else
    begin
    StmObj:=FDoc.FindInDirectObject(Itm.StreamObjectNr,0);
    if StmObj=nil then
      begin
      if Assigned(FloadingXRef) then
        begin
        StmRef:=FloadingXRef[Itm.StreamObjectNr];
        if StmRef<>Nil then
           StmObj:=LoadXRefObject(StmRef);
        end;
      if StmObj=Nil then
        DoError(penNoSuchStreamObject,SErrNoSuchStreamObject,[Itm.StreamObjectNr,Itm.ReferenceIndex]);
      end;
    if StmObj.ObjectType<>SPDFTypeObjStm then
      DoError(penNotStreamObject,SErrNotStreamObject,[Itm.StreamObjectNr,SPDFTypeObjStm]);
    Result:=ParseIndirectObjectInStream(StmObj,Itm.ObjectIndex, Itm.ReferenceIndex);
    end;
  if assigned(Result) then
    begin
    Result.ObjectID:=Itm.ReferenceIndex;
    Itm.Instance:=Result;
    if AddToDocument then
      if not FDoc.AddInDirectObject(Result) then
        FreeAndNil(Result)
    end;
end;

procedure TPDFParser.LoadIndirectObjects;

var
  I : Integer;
  Itm : TPDFXRef;
  UseCompressed : Boolean;

begin
  For UseCompressed:=False to True do
    begin
    For I:=0 to Length(FloadingXRef)-1 do
      begin
      Itm:=FloadingXRef[i];
      if Not Assigned(Itm) then
        begin
        if not UseCompressed then
          DoWarning('No reference to object ID %d available',[I])
        end
      else if (Itm.Instance=Nil) and Itm.InUse and (Itm.Compressed=UseCompressed) then
        if Itm.ReferenceIndex>0 then
          begin
          LoadXRefObject(Itm);
{          if Assigned(Ind) then
            Writeln('Loaded ',Ind.GetDescription);}
          end;
      end;
   end;
end;

function TPDFParser.DetectTrailerAt(aStartPos: Int64): TPDFTrailer;

Var
  aDictEnd,aPos : Int64;
  lToken : TPDFToken;

begin
  Result:=Nil;
  FScanner.Reposition(aStartPos,False);
  aPos:=FScanner.Position;
  aDictEnd:=FScanner.FindBytesBackward('>>');
  if (aPos-aDictEnd)>PDFMaxTrailerDistance then
    exit;
  aDictEnd:=FScanner.FindBytesBackward(SPDFTrailer);
  lToken:=FScanner.GetToken(False);
  Result:=ParseTrailer(lToken);
end;

procedure TPDFParser.ParseDocument(aDoc: TPDFDocument);

var
  aXRefStartPos, aEOFPos : Int64;
  Start : TPDFStartXRef;
  XRef : TPDFXRefList;
  XRefObj : TPDFIndirect;
  Trailer : TPDFTrailer;
  XRefDict : TPDFDictionary;
  aStartIndex : Int64;
  lToken : TPDFToken;

begin
  FDoc:=aDoc;
  aEOFPos:=VerifyDocument;
  Start:=GetLastXRef(aEOFPos,aXRefStartPos);
  FDoc.Add(Start);
  Trailer:=DetectTrailerAt(aXRefStartPos);
  FDoc.TrailerDict:=Trailer;
  if Assigned(Trailer) then
    begin
    FDoc.Add(Trailer);
    if Trailer.ContainsKey(SPDFKeySize) then
      SetLength(FloadingXRef,Trailer.GetIntegerValue(SPDFKeySize));
    XRefDict:=Trailer;
    end;
  aStartIndex:=Start.Index;
  While (aStartIndex>0) do
    begin
    XRef:=ParseXRefAt(aStartIndex,XRefObj);
    if XRef<>nil then
      begin
      if Assigned(XRefObj) then
        begin
        if not FDoc.AddInDirectObject(XRefObj) then
          XRefObj.Free
        else
          begin
          XRefDict:=XRefObj.ObjectDict;
          if FDoc.TrailerDict=Nil then
            FDoc.TrailerDict:=XRefDict;
          end;
        end
      else
        FDoc.Add(XRef);
      end;
    If Assigned(XRefDict) and (XRefDict.ContainsKey(SPDFKeyPrev)) then
      aStartIndex:=XRefDict.GetInt64Value(SPDFKeyPrev)
    else
      begin
      // Updated PDF documents refer to the original PDF document's XREF.
      // This XRef is then followed by a trailer which contains a 'Prev'.
      // We check for this here.
      lToken:=FScanner.GetToken(False);
      if Not ltoken.CheckString(SPDFTrailer) then
        begin
        FScanner.Unget(lToken);
        aStartIndex:=0;
        end
      else
        begin
        Trailer:=ParseTrailer(lToken);
        If Assigned(Trailer) then
          begin
          if Trailer.ContainsKey('Prev') then
            aStartIndex:=Trailer.GetInt64Value('Prev')
          else
            aStartIndex:=0;
          FDoc.Add(Trailer);
          end
        else
          aStartIndex:=0;
        end;
      end;
    XRefDict:=Nil;
    end;
  FDoc.SetXrefArray(FloadingXRef);
  if LoadObjects then
    begin
    LoadIndirectObjects;
    If ResolveContentStreams then
      DoResolveContentStreams(FDoc);
    if ResolveToUnicodeCMaps then
      DoResolveToUnicodeCMaps(FDoc);
    end;
end;

function TPDFParser.GetPageContentStream(aDoc : TPDFDocument; aPage: TPDFPageObject) : TStream;

  Function GetContentStream(Idx : Integer) : TStream;
  var
    Ref : TPDFRefData;
    Obj : TPDFIndirect;

  begin
    Ref:=aPage.ContentRef[Idx];
    Obj:=aDoc.FindInDirectObject(Ref);
    if Assigned(Obj) and Assigned(Obj.ObjectDict) and Assigned(Obj.Stream) then
      Result:=GetUnfilteredStream(Obj)
    else
      DoError(penContentStreamNotFound,sErrContentStreamNotFound,[Ref.ObjectID,Ref.ObjectGeneration]);
  end;

Var
  I : Integer;
  Streams : Array of TStream;

begin
  Streams:=[];
  Result:=Nil;
  try
    if aPage.ContentCount=1 then
      Result:=GetContentStream(0)
    else
      begin
      SetLength(Streams,aPage.ContentCount);
      For I:=0 to aPage.ContentCount-1 do
        Streams[I]:=GetContentStream(I);
      Result:=TChainedStream.Create(Streams);
      Streams:=[];
      end;

  except
    FreeAndNil(Result);
    For I:=0 to Length(Streams)-1 do
      Streams[I].Free;
    Raise;
  end;
end;

procedure TPDFParser.DoResolveContentStreams(aDoc: TPDFDocument; aOnCommand : TPDFNewCommandEvent = Nil);

Var
  I,aCount : Integer;
  aPage : TPDFPageObject;
  aStream : TStream;

begin
  aCount:=aDoc.PageCount;
  For I:=0 to aCount-1 do
    begin
    aPage:=aDoc.Page[I];
    DoProgress(pkContentStream,I+1,aCount);
    aStream:=GetPageContentStream(aDoc,aPage);
    try
      ParseContentStream(aPage,aStream,aOnCommand)
    finally
      if aStream is TChainedStream then
        aStream.Free;
    end;
    end;
end;

destructor TPDFParser.Destroy;
begin
  FreeAndNil(FScanner);
  inherited Destroy;
end;


end.
