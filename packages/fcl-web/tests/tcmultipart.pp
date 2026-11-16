{ Tests multipart parsing and the temporary files of uploads in TRequest. }
unit tcmultipart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs;

type

  { TTestRequest }

  TTestRequest = class(TRequest)
  private
    FFixedName: String;
    FFixedCount: Integer;
  protected
    function GetTempUploadFileName(const AName, AFileName: String; ASize: Int64): String; override;
  public
    // Sets a multipart body with aBoundary
    procedure SetBody(const aBoundary: String; const aBody: RawByteString);
    // Parses the body set with SetBody in one go
    procedure Parse;
    // Parses the body set with SetBody in chunks of aChunkSize bytes, as the streaming servers do
    procedure ParseStreaming(aChunkSize: Integer);
    // The name GetTempUploadFileName returns for its next aCount calls
    procedure UseFixedName(const aName: String; aCount: Integer);
  end;

  { TTestMultipart }

  TTestMultipart = class(TTestCase)
  private
    FDir: String;
    FSaveUploadDir: String;
    FSaveMaxParts: Integer;
    FSaveMaxFileSize: Int64;
    function FileBody(const aData: RawByteString): RawByteString;
    function ReadFile(const aFileName: String): RawByteString;
    procedure WriteFile(const aFileName: String; const aData: RawByteString);
    function FilesInDir: Integer;
    function NewRequest(const aBody: RawByteString): TTestRequest;
    function PartsBody(aCount: Integer): RawByteString;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUploadFile;
    procedure TestUploadFileName;
    procedure TestDeletedOnFree;
    procedure TestKeepUploadedFiles;
    procedure TestRenamedUploadRemains;
    procedure TestExistingNameNotOverwritten;
    procedure TestExistingNameAlways;
    procedure TestConcurrentUploads;
    procedure TestParseFields;
    procedure TestParseStreaming;
    procedure TestStreamingChunkSizes;
    procedure TestStreamingNoPreamble;
    procedure TestStreamingErrorDeletesFile;
    procedure TestManyParts;
    procedure TestMaxParts;
  end;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

const
  cBoundary = 'XyZ12';
  CRLF = #13#10;

type

  { TUploadThread }

  TUploadThread = class(TThread)
  private
    FTest: TTestMultipart;
    FTag: String;
    FRounds: Integer;
    FNames: TStrings;
    FError: String;
  protected
    procedure Execute; override;
  public
    constructor Create(aTest: TTestMultipart; const aTag: String; aRounds: Integer; aNames: TStrings);
    property Error: String read FError;
  end;

{ TTestRequest }

function TTestRequest.GetTempUploadFileName(const AName, AFileName: String; ASize: Int64): String;

begin
  if FFixedCount>0 then
    begin
    Dec(FFixedCount);
    Result:=FFixedName;
    end
  else
    Result:=inherited GetTempUploadFileName(AName,AFileName,ASize);
end;


procedure TTestRequest.SetBody(const aBoundary: String; const aBody: RawByteString);

var
  lBytes: TBytes;

begin
  Method:='POST';
  ContentType:='multipart/form-data; boundary='+aBoundary;
  lBytes:=[];
  SetLength(lBytes,Length(aBody));
  if Length(aBody)>0 then
    Move(aBody[1],lBytes[0],Length(aBody));
  ContentBytes:=lBytes;
  ContentLength:=Length(aBody);
end;


procedure TTestRequest.Parse;

begin
  InitRequestVars;
end;


procedure TTestRequest.ParseStreaming(aChunkSize: Integer);

var
  lBytes: TBytes;
  lPos, lSize: Integer;

begin
  lBytes:=ContentBytes;
  ProcessStreamingContent(cssStart,lBytes[0],0);
  lPos:=0;
  while lPos<Length(lBytes) do
    begin
    lSize:=Length(lBytes)-lPos;
    if lSize>aChunkSize then
      lSize:=aChunkSize;
    ProcessStreamingContent(cssData,lBytes[lPos],lSize);
    Inc(lPos,lSize);
    end;
  ProcessStreamingContent(cssEnd,lBytes[0],0);
end;


procedure TTestRequest.UseFixedName(const aName: String; aCount: Integer);

begin
  FFixedName:=aName;
  FFixedCount:=aCount;
end;


{ TUploadThread }

constructor TUploadThread.Create(aTest: TTestMultipart; const aTag: String; aRounds: Integer; aNames: TStrings);

begin
  FTest:=aTest;
  FTag:=aTag;
  FRounds:=aRounds;
  FNames:=aNames;
  inherited Create(False);
end;


procedure TUploadThread.Execute;

var
  I: Integer;
  lData: RawByteString;
  lRequest: TTestRequest;

begin
  try
    for I:=1 to FRounds do
      begin
      lData:=FTag+IntToStr(I)+StringOfChar('x',1000);
      lRequest:=FTest.NewRequest(FTest.FileBody(lData));
      try
        lRequest.KeepUploadedFiles:=True;
        lRequest.Parse;
        if FTest.ReadFile(lRequest.Files[0].LocalFileName)<>lData then
          FError:='file of '+FTag+IntToStr(I)+' has other data';
        FNames.Add(lRequest.Files[0].LocalFileName);
      finally
        lRequest.Free;
      end;
      end;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
end;


{ TTestMultipart }

function TTestMultipart.FileBody(const aData: RawByteString): RawByteString;

begin
  Result:='--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="upload"; filename="data.bin"'+CRLF
    +'Content-Type: application/octet-stream'+CRLF
    +CRLF
    +aData+CRLF
    +'--'+cBoundary+'--'+CRLF;
end;


function TTestMultipart.ReadFile(const aFileName: String): RawByteString;

var
  lStream: TFileStream;

begin
  Result:='';
  lStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result,lStream.Size);
    if Length(Result)>0 then
      lStream.ReadBuffer(Result[1],Length(Result));
  finally
    lStream.Free;
  end;
end;


procedure TTestMultipart.WriteFile(const aFileName: String; const aData: RawByteString);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName,fmCreate);
  try
    if Length(aData)>0 then
      lStream.WriteBuffer(aData[1],Length(aData));
  finally
    lStream.Free;
  end;
end;


function TTestMultipart.FilesInDir: Integer;

var
  lInfo: TSearchRec;

begin
  Result:=0;
  if FindFirst(FDir+'*',faAnyFile,lInfo)=0 then
    try
      repeat
        if (lInfo.Attr and faDirectory)=0 then
          Inc(Result);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
end;


function TTestMultipart.NewRequest(const aBody: RawByteString): TTestRequest;

begin
  Result:=TTestRequest.Create;
  Result.SetBody(cBoundary,aBody);
end;


function TTestMultipart.PartsBody(aCount: Integer): RawByteString;

var
  I: Integer;
  lPart: RawByteString;
  lStream: TStringStream;

begin
  lPart:='--'+cBoundary+CRLF+'Content-Disposition: form-data; name="f"'+CRLF+CRLF+'v'+CRLF;
  lStream:=TStringStream.Create('');
  try
    for I:=1 to aCount do
      lStream.WriteString(lPart);
    lStream.WriteString('--'+cBoundary+'--'+CRLF);
    Result:=lStream.DataString;
  finally
    lStream.Free;
  end;
end;


procedure TTestMultipart.SetUp;

begin
  FDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tcmultipart'+IntToStr(GetProcessID);
  ForceDirectories(FDir);
  FDir:=IncludeTrailingPathDelimiter(FDir);
  FSaveUploadDir:=TRequest.DefaultRequestUploadDir;
  FSaveMaxParts:=TMimeItems.MaxParts;
  FSaveMaxFileSize:=TUploadedFiles.MaxUploadFileSize;
  TRequest.DefaultRequestUploadDir:=FDir;
end;


procedure TTestMultipart.TearDown;

var
  lInfo: TSearchRec;

begin
  TRequest.DefaultRequestUploadDir:=FSaveUploadDir;
  TMimeItems.MaxParts:=FSaveMaxParts;
  TUploadedFiles.MaxUploadFileSize:=FSaveMaxFileSize;
  if FindFirst(FDir+'*',faAnyFile,lInfo)=0 then
    try
      repeat
        if (lInfo.Attr and faDirectory)=0 then
          DeleteFile(FDir+lInfo.Name);
      until FindNext(lInfo)<>0;
    finally
      FindClose(lInfo);
    end;
  RemoveDir(FDir);
end;


procedure TTestMultipart.TestUploadFile;

var
  lRequest: TTestRequest;
{$IFDEF UNIX}
  lStat: TStat;
{$ENDIF}

begin
  lRequest:=NewRequest(FileBody('hello world'));
  try
    lRequest.Parse;
    AssertEquals('One uploaded file',1,lRequest.Files.Count);
    AssertEquals('Uploaded file is in the upload directory',FDir,ExtractFilePath(lRequest.Files[0].LocalFileName));
    AssertEquals('Uploaded file data','hello world',ReadFile(lRequest.Files[0].LocalFileName));
    AssertEquals('Uploaded file name','data.bin',lRequest.Files[0].FileName);
{$IFDEF UNIX}
    AssertTrue('Uploaded file is readable by the owner only',
      (fpStat(lRequest.Files[0].LocalFileName,lStat)=0) and ((lStat.st_mode and &777)=&600));
{$ENDIF}
  finally
    lRequest.Free;
  end;
end;


procedure TTestMultipart.TestUploadFileName;

var
  lRequest: TTestRequest;
  lName, lHex: String;
  I: Integer;

begin
  lRequest:=NewRequest(FileBody('abc'));
  try
    lRequest.Parse;
    lName:=ExtractFileName(lRequest.Files[0].LocalFileName);
    AssertEquals('Name starts with CGI','CGI',Copy(lName,1,3));
    AssertEquals('Name ends with .tmp','.tmp',ExtractFileExt(lName));
    lHex:=Copy(lName,4,Length(lName)-7);
    AssertEquals('Name has 32 random hexadecimal digits',32,Length(lHex));
    for I:=1 to Length(lHex) do
      AssertTrue('Name digit '+IntToStr(I)+' is hexadecimal',lHex[I] in ['0'..'9','A'..'F','a'..'f']);
  finally
    lRequest.Free;
  end;
end;


procedure TTestMultipart.TestDeletedOnFree;

var
  lRequest: TTestRequest;
  lName: String;

begin
  lRequest:=NewRequest(FileBody('to be deleted'));
  try
    lRequest.Parse;
    lName:=lRequest.Files[0].LocalFileName;
    AssertTrue('Uploaded file exists while the request exists',FileExists(lName));
    AssertNotNull('Uploaded file stream can be opened',lRequest.Files[0].Stream);
  finally
    lRequest.Free;
  end;
  AssertFalse('Uploaded file is deleted with the request',FileExists(lName));
  AssertEquals('No files left in the upload directory',0,FilesInDir);
end;


procedure TTestMultipart.TestKeepUploadedFiles;

var
  lRequest: TTestRequest;
  lName: String;

begin
  AssertFalse('DefaultKeepUploadedFiles is False',TRequest.DefaultKeepUploadedFiles);
  lRequest:=NewRequest(FileBody('keep me'));
  try
    AssertFalse('KeepUploadedFiles starts as DefaultKeepUploadedFiles',lRequest.KeepUploadedFiles);
    lRequest.KeepUploadedFiles:=True;
    lRequest.Parse;
    lName:=lRequest.Files[0].LocalFileName;
  finally
    lRequest.Free;
  end;
  AssertTrue('Uploaded file remains with KeepUploadedFiles',FileExists(lName));
  AssertEquals('Kept file data','keep me',ReadFile(lName));
end;


procedure TTestMultipart.TestRenamedUploadRemains;

var
  lRequest: TTestRequest;
  lNewName: String;

begin
  lNewName:=FDir+'kept.bin';
  lRequest:=NewRequest(FileBody('moved'));
  try
    lRequest.Parse;
    AssertTrue('Rename uploaded file',RenameFile(lRequest.Files[0].LocalFileName,lNewName));
  finally
    lRequest.Free;
  end;
  AssertTrue('Renamed upload remains after the request is freed',FileExists(lNewName));
  AssertEquals('Renamed upload data','moved',ReadFile(lNewName));
end;


procedure TTestMultipart.TestExistingNameNotOverwritten;

var
  lRequest: TTestRequest;
  lExisting: String;

begin
  lExisting:=FDir+'existing.tmp';
  WriteFile(lExisting,'do not touch');
  lRequest:=NewRequest(FileBody('upload data'));
  try
    lRequest.UseFixedName(lExisting,2);
    lRequest.Parse;
    AssertEquals('Existing file is not overwritten','do not touch',ReadFile(lExisting));
    AssertTrue('Upload uses another name',lRequest.Files[0].LocalFileName<>lExisting);
    AssertEquals('Upload data in the other name','upload data',ReadFile(lRequest.Files[0].LocalFileName));
  finally
    lRequest.Free;
  end;
  AssertTrue('Existing file remains after the request is freed',FileExists(lExisting));
end;


procedure TTestMultipart.TestExistingNameAlways;

var
  lRequest: TTestRequest;
  lExisting, lClass: String;

begin
  lExisting:=FDir+'existing.tmp';
  WriteFile(lExisting,'do not touch');
  lClass:='';
  lRequest:=NewRequest(FileBody('upload data'));
  try
    lRequest.UseFixedName(lExisting,MaxInt);
    try
      lRequest.Parse;
    except
      on E: Exception do
        lClass:=E.ClassName;
    end;
  finally
    lRequest.Free;
  end;
  AssertEquals('A name that always exists raises EFCreateError','EFCreateError',lClass);
  AssertEquals('Existing file is not overwritten','do not touch',ReadFile(lExisting));
end;


procedure TTestMultipart.TestConcurrentUploads;

const
  cRounds = 100;

var
  lNames: array[0..1] of TStringList;
  lThreads: array[0..1] of TUploadThread;
  lAll: TStringList;
  I: Integer;

begin
  lAll:=TStringList.Create;
  try
    for I:=0 to 1 do
      lNames[I]:=TStringList.Create;
    for I:=0 to 1 do
      lThreads[I]:=TUploadThread.Create(Self,'T'+IntToStr(I)+'-',cRounds,lNames[I]);
    for I:=0 to 1 do
      begin
      lThreads[I].WaitFor;
      AssertEquals('Thread '+IntToStr(I)+' error','',lThreads[I].Error);
      lAll.AddStrings(lNames[I]);
      end;
    lAll.Sorted:=True;
    lAll.Duplicates:=dupIgnore;
    AssertEquals('Every upload has its own file',2*cRounds,lAll.Count);
    AssertEquals('Every upload file remains',2*cRounds,FilesInDir);
  finally
    for I:=0 to 1 do
      begin
      lThreads[I].Free;
      lNames[I].Free;
      end;
    lAll.Free;
  end;
end;


procedure TTestMultipart.TestParseFields;

var
  lRequest: TTestRequest;
  lBody: RawByteString;
  lFields: TStrings;

begin
  lBody:='preamble'+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="a;b=c"'+CRLF
    +CRLF
    +'value1'+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="f"; filename="x;y.txt"'+CRLF
    +'Content-Type: text/plain'+CRLF
    +CRLF
    +'file'+CRLF+'data'+CRLF
    +'--'+cBoundary+CRLF
    +CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="last"'+CRLF
    +CRLF
    +'z'+CRLF
    +'--'+cBoundary+'--';
  lRequest:=NewRequest(lBody);
  try
    lRequest.Parse;
    lFields:=lRequest.ContentFields;
    AssertEquals('Number of fields',4,lFields.Count);
    AssertTrue('Quoted name with ; and =',lFields.IndexOf('a;b=c=value1')>=0);
    AssertTrue('File field has the file name',lFields.IndexOf('f=x;y.txt')>=0);
    AssertTrue('Empty part gets a dummy name',lFields.IndexOf('DummyFileItem2=')>=0);
    AssertTrue('Last part without CRLF after the final marker',lFields.IndexOf('last=z')>=0);
    AssertEquals('One uploaded file',1,lRequest.Files.Count);
    AssertEquals('File content type','text/plain',lRequest.Files[0].ContentType);
    AssertEquals('File data','file'+CRLF+'data',ReadFile(lRequest.Files[0].LocalFileName));
  finally
    lRequest.Free;
  end;
end;


procedure TTestMultipart.TestParseStreaming;

var
  lRequest: TTestRequest;
  lBody, lData: RawByteString;
  lName: String;

begin
  lData:=StringOfChar('d',5000);
  lBody:='--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="text"'+CRLF
    +CRLF
    +'some text'+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="upload"; filename="s.bin"'+CRLF
    +CRLF
    +lData+CRLF
    +'--'+cBoundary+'--'+CRLF;
  lRequest:=NewRequest(lBody);
  try
    lRequest.ParseStreaming(64);
    AssertEquals('Text field','some text',lRequest.ContentFields.Values['text']);
    AssertEquals('One uploaded file',1,lRequest.Files.Count);
    lName:=lRequest.Files[0].LocalFileName;
    AssertEquals('Streamed file data',lData,ReadFile(lName));
  finally
    lRequest.Free;
  end;
  AssertFalse('Streamed file is deleted with the request',FileExists(lName));
end;


procedure TTestMultipart.TestStreamingChunkSizes;

const
  cSizes: array[0..13] of Integer = (1,2,3,4,5,6,7,8,11,12,13,16,64,100000);

var
  lRequest: TTestRequest;
  lBody, lData: RawByteString;
  I: Integer;
  lMsg: String;

begin
  lData:='abc'+CRLF+'--XyZ1x'+StringOfChar('d',300)+CRLF+'--';
  lBody:='preamble'+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="text"'+CRLF
    +CRLF
    +'some text'+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="upload"; filename="s.bin"'+CRLF
    +CRLF
    +lData+CRLF
    +'--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="last"'+CRLF
    +CRLF
    +'z'+CRLF
    +'--'+cBoundary+'--'+CRLF
    +'epilogue';
  for I:=Low(cSizes) to High(cSizes) do
    begin
    lMsg:='chunks of '+IntToStr(cSizes[I])+': ';
    lRequest:=NewRequest(lBody);
    try
      lRequest.ParseStreaming(cSizes[I]);
      AssertEquals(lMsg+'number of fields',3,lRequest.ContentFields.Count);
      AssertEquals(lMsg+'text field','some text',lRequest.ContentFields.Values['text']);
      AssertEquals(lMsg+'field after the file','z',lRequest.ContentFields.Values['last']);
      AssertEquals(lMsg+'one uploaded file',1,lRequest.Files.Count);
      AssertEquals(lMsg+'file data',lData,ReadFile(lRequest.Files[0].LocalFileName));
      AssertEquals(lMsg+'preamble','preamble',lRequest.Content);
    finally
      lRequest.Free;
    end;
    end;
end;


procedure TTestMultipart.TestStreamingNoPreamble;

var
  lRequest: TTestRequest;
  lBody: RawByteString;
  I: Integer;

begin
  lBody:='--'+cBoundary+CRLF
    +'Content-Disposition: form-data; name="a"'+CRLF
    +CRLF
    +'1'+CRLF
    +'--'+cBoundary+'--';
  for I:=1 to 20 do
    begin
    lRequest:=NewRequest(lBody);
    try
      lRequest.ParseStreaming(I);
      AssertEquals('chunks of '+IntToStr(I)+': field','1',lRequest.ContentFields.Values['a']);
      AssertEquals('chunks of '+IntToStr(I)+': no preamble','',lRequest.Content);
    finally
      lRequest.Free;
    end;
    end;
end;


procedure TTestMultipart.TestStreamingErrorDeletesFile;

var
  lRequest: TTestRequest;
  lStatus: Integer;

begin
  TUploadedFiles.MaxUploadFileSize:=10;
  lStatus:=0;
  lRequest:=NewRequest(FileBody(StringOfChar('e',100)));
  try
    try
      lRequest.ParseStreaming(16);
    except
      on E: EHTTP do
        lStatus:=E.StatusCode;
    end;
  finally
    lRequest.Free;
  end;
  AssertEquals('Too large upload is refused',413,lStatus);
  AssertEquals('No file left after a refused streamed upload',0,FilesInDir);
end;


procedure TTestMultipart.TestManyParts;

const
  cParts = 50000;

var
  lRequest: TTestRequest;
  lStart: QWord;

begin
  TMimeItems.MaxParts:=0;
  lRequest:=NewRequest(PartsBody(cParts));
  try
    lStart:=GetTickCount64;
    lRequest.Parse;
    AssertTrue('50000 parts parse in less than 2 seconds',GetTickCount64-lStart<2000);
    AssertEquals('Number of fields',cParts,lRequest.ContentFields.Count);
  finally
    lRequest.Free;
  end;
end;


procedure TTestMultipart.TestMaxParts;

var
  lRequest: TTestRequest;
  lStatus: Integer;

begin
  AssertEquals('Default MaxParts',DefaultMaxMimeParts,TMimeItems.MaxParts);
  TMimeItems.MaxParts:=100;
  lRequest:=NewRequest(PartsBody(100));
  try
    lRequest.Parse;
    AssertEquals('MaxParts parts are allowed',100,lRequest.ContentFields.Count);
  finally
    lRequest.Free;
  end;
  lStatus:=0;
  lRequest:=NewRequest(PartsBody(101));
  try
    try
      lRequest.Parse;
    except
      on E: EHTTP do
        lStatus:=E.StatusCode;
    end;
  finally
    lRequest.Free;
  end;
  AssertEquals('More than MaxParts parts are refused',413,lStatus);
end;


initialization
  RegisterTest(TTestMultipart);
end.
