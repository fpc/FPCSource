program tczipper;
{
    This file is part of the Free Pascal packages.
    Copyright (c) 2012-2014 by the Free Pascal Development Team
    Created by Reinier Olislagers

    Tests zip/unzip functionality provided by the FPC zipper.pp unit.
    If passed a zip file name as first argument, it will try and decompress
    and list the contents of the zip file.

    See the file COPYING.FPC, included in this distribution,
    for details about the license.

 **********************************************************************}
{$mode objfpc}{$h+}

//Define this if you want to inspect the generated zips etc
{$define KEEPTESTFILES}

uses
  SysUtils, classes,
  zipper, unzip, zdeflate, zinflate, zip, md5, zstream, nullstream;

type

  { TCallBackHandler }

  TCallBackHandler = class(TObject) //Callbacks used in zip/unzip processing
  private
    FPerformChecks: boolean;
    FOriginalContent: ansistring;
    FShowContent: boolean;
    FStreamResult: boolean;
  public
    property PerformChecks: boolean read FPerformChecks write FPerformChecks; //If false, do not perform any consistency checks
    property OriginalContent: ansistring read FOriginalContent write FOriginalContent; //Zip entry uncompressed content used in TestZipEntries
    property ShowContent: boolean read FShowContent write FShowContent; //Show contents of zip when extracting?
    property StreamResult: boolean read FStreamResult; //For handler to report success/failure
    procedure EndOfFile(Sender:TObject; const Ratio:double);
    procedure StartOfFile(Sender:TObject; const AFileName:ansistring);
    procedure DoCreateZipOutputStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry); //Used to verify zip entry decompressed contents
    constructor Create;
  end;

procedure TCallBackHandler.EndOfFile(Sender: TObject; const Ratio: double);
begin
  writeln('End of file handler hit; compression ratio: '+floattostr(ratio));
  if (FPerformChecks) and (Ratio<0) then
  begin
    writeln('Found compression ratio '+floattostr(Ratio)+', which should never be lower than 0.');
    halt(1);
  end;
end;

procedure TCallBackHandler.StartOfFile(Sender: TObject; const AFileName: ansistring);
begin
  writeln('Start of file handler hit; filename: '+AFileName);
  if (FPerformChecks) and (AFileName='') then
  begin
    writeln('Archive filename should not be empty.');
    halt(1);
  end;
end;

procedure TCallBackHandler.DoCreateZipOutputStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream:=TMemoryStream.Create;
end;

procedure TCallBackHandler.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  DecompressedContent: ansistring;
begin
  //writeln('At end of '+AItem.ArchiveFileName);
  AStream.Position:=0;
  SetLength(DecompressedContent,Astream.Size);
  if AStream.Size>0 then
    (AStream as TMemoryStream).Read(DecompressedContent[1], AStream.Size);
  if (FPerformChecks) and (DecompressedContent<>OriginalContent) then
  begin
    FStreamResult:=false;
    writeln('TestZipEntries failed: found entry '+AItem.ArchiveFileName+
      ' has value ');
    writeln('*'+DecompressedContent+'*');
    writeln('expected ');
    writeln('*'+OriginalContent+'*');
  end;
  if (FPerformChecks=false) and (ShowContent=true) then
  begin
    //display only
    writeln('TestZipEntries info: found entry '+AItem.ArchiveFileName+
      ' has value ');
    writeln('*'+DecompressedContent+'*');
  end;
  Astream.Free;
end;

constructor TCallBackHandler.Create;
begin
  FOriginalContent:='A'; //nice short demo content
  FStreamResult:=true;
  FPerformChecks:=true; //perform verification by default
  FShowContent:=true;
end;


function CompareCompressDecompress: boolean;
var
  CallBackHandler: TCallBackHandler;
  CompressedFile: ansistring;
  FileContents: TStringList;
  UncompressedFile1: ansistring;
  UncompressedFile1Hash: ansistring;
  UncompressedFile2: ansistring;
  UncompressedFile2Hash: ansistring;
  OurZipper: TZipper;
  UnZipper: TUnZipper;
begin
  result:=true;

  FileContents:=TStringList.Create;
  OurZipper:=TZipper.Create;
  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  try
    // Set up uncompressed files
    FileContents.Add('This is an uncompressed file.');
    FileContents.Add('And another line.');
    UncompressedFile1:=SysUtils.GetTempFileName('', 'UN1');
    FileContents.SaveToFile(UncompressedFile1);
    FileContents.Clear;
    FileContents.Add('Have you looked into using fpcup today?');
    FileContents.Add('It works nicely with fpc and goes well with a fruity red wine, too.');
    // Second GetTempFileName call needs to be done after saving first file because
    // GetTempFileName checks for existing file names and may give the *same* file name
    // if called before
    UncompressedFile2:=SysUtils.GetTempFileName('', 'UN2');
    FileContents.SaveToFile(UncompressedFile2);
    // Remember their content, so we can compare later.
    UncompressedFile1Hash:=MD5Print(MD5File(UncompressedFile1, MDDefBufSize));
    UncompressedFile2Hash:=MD5Print(MD5File(UncompressedFile2, MDDefBufSize));

    // Test zip functionality.
    CompressedFile:=SysUtils.GetTempFileName('', 'CC');
    OurZipper.FileName:=CompressedFile;
    // Add the files only with their filenames, we don't want to create
    // subdirectories:
    OurZipper.Entries.AddFileEntry(UncompressedFile1,ExtractFileName(UncompressedFile1));
    OurZipper.Entries.AddFileEntry(UncompressedFile2,ExtractFileName(UncompressedFile2));
    OurZipper.OnStartFile:=@CallBackHandler.StartOfFile;
    OurZipper.OnEndFile:=@CallBackHandler.EndOfFile;
    OurZipper.ZipAllFiles;
    if not FileExists(CompressedFile) then
    begin
      writeln('Zip file was not created.');
      exit(false);
    end;

    // Delete original files
    {$IFNDEF KEEPTESTFILES}
    DeleteFile(UncompressedFile1);
    DeleteFile(UncompressedFile2);
    {$ENDIF}

    // Now unzip
    Unzipper.FileName:=CompressedFile;
    Unzipper.OutputPath:=ExtractFilePath(UncompressedFile1);
    UnZipper.OnStartFile:=@CallBackHandler.StartOfFile;
    UnZipper.OnEndFile:=@CallBackHandler.EndOfFile;
    Unzipper.Examine;
    Unzipper.UnZipAllFiles;

    // Now we should have the uncompressed files again
    if (not FileExists(UncompressedFile1)) or
      (not FileExists(UncompressedFile2)) then
    begin
      writeln('Unzip failed: could not find decompressed files.');
      exit(false);
    end;

    // Compare hashes
    if
      (UncompressedFile1Hash<>MD5Print(MD5File(UncompressedFile1, MDDefBufSize)))
      or
      (UncompressedFile2Hash<>MD5Print(MD5File(UncompressedFile2, MDDefBufSize)))
    then
    begin
      writeln('Unzip failed: uncompressed files are not the same as the originals.');
      exit(false);
    end;

  finally
    FileContents.Free;
    CallBackHandler.Free;
    OurZipper.Free;
    UnZipper.Free;
    {$IFNDEF KEEPTESTFILES}
    try
      if FileExists(CompressedFile) then DeleteFile(CompressedFile);
      if FileExists(UncompressedFile1) then DeleteFile(UncompressedFile1);
      if FileExists(UncompressedFile2) then DeleteFile(UncompressedFile2);
    finally
      // Ignore errors: OS should eventually clean out temp files anyway
    end;
    {$ENDIF}
  end;
end;

function CompressSmallStreams: boolean;
// Compresses some small streams using default compression and
// no compression (storage)
// Just storing is the best option; compression will enlarge the zip.
// Test verifies that the entries in the zip are not bigger than
// the originals.
var
  DestFile: ansistring;
  z: TZipper;
  zfe: TZipFileEntry;
  s: ansistring = 'abcd';
  DefaultStream, StoreStream: TStringStream;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'CS1');
  z:=TZipper.Create;
  z.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    StoreStream:=TStringStream.Create(s);

    //DefaultStream - compression  level = Default
    zfe:=z.Entries.AddFileEntry(DefaultStream, 'Compressed');
    z.ZipAllFiles;

    if (z.Entries[0].Size>zfe.Size) then
    begin
      result:=false;
      writeln('Small stream test default compression failed: compressed size '+
        inttostr(z.Entries[0].Size) + ' > original size '+inttostr(zfe.Size));
      exit;
    end;

  finally
    DefaultStream.Free;
    StoreStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
  DestFile:=SysUtils.GetTempFileName('', 'CS2');
  z:=TZipper.Create;
  z.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    StoreStream:=TStringStream.Create(s);

    //StoreStream - compression  level = Store
    zfe:=z.Entries.AddFileEntry(StoreStream, 'Uncompressed');
    zfe.CompressionLevel:=clnone;
    z.ZipAllFiles;

    if (z.Entries[0].Size>zfe.Size) then
    begin
      result:=false;
      writeln('Small stream test uncompressed failed: compressed size '+
        inttostr(z.Entries[0].Size) + ' > original size '+inttostr(zfe.Size));
      exit;
    end;
  finally
    DefaultStream.Free;
    StoreStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}

  //The result can be checked with the command (on Linux):
  //unzip -v <DestFile>
  //The column Size Shows that compressed files are bigger than source files
end;

function ShowZipFile(ZipFile: ansistring): boolean;
// Reads zip file and lists entries
var
  CallBackHandler: TCallBackHandler;
  i: integer;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
begin
  result:=true;
  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  UnzipArchiveFiles:=TStringList.Create;
  try
    CallBackHandler.PerformChecks:=false; //only display output
    UnZipper.FileName:=ZipFile;
    Unzipper.Examine;
    writeln('ShowZipFile: zip file has '+inttostr(UnZipper.Entries.Count)+' entries');

    i:=0;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    while i<Unzipper.Entries.Count do
    begin
      if CallBackHandler.StreamResult then
      begin
        UnzipArchiveFiles.Clear;
        UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
        Unzipper.UnZipFiles(UnzipArchiveFiles);
        // This will kick off the DoCreateOutZipStream/DoDoneOutZipStream handlers
        inc(i);
      end
      else
      begin
        break; // Handler has reported error; stop loop
      end;
    end;
  finally
    Unzipper.Free;
    CallBackHandler.Free;
    UnzipArchiveFiles.Free;
  end;
end;

function TestZipEntries(Entries: qword): boolean;
// Adds Entries amount of zip file entries and reads them
// Starting from 65535 entries, the zip needs to be in zip64 format
var
  CallBackHandler: TCallBackHandler;
  DestFile: ansistring;
  i: qword;
  j: integer;
  OriginalContent: ansistring = 'A'; //Uncompressed content for zip file entry
  ContentStreams: TFPList;
  ContentStream: TStringStream;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'E'+inttostr(Entries)+'_');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ContentStreams:=TFPList.Create;
  try
    i:=0;
    while i<Entries do
    begin
      ContentStream:=TStringStream.Create(OriginalContent);
      ContentStreams.Add(ContentStream);
      // Start filenames at 1
      Zipper.Entries.AddFileEntry(TStringStream(ContentStreams.Items[i]), format('%U',[i+1]));
      inc(i);
    end;
    Zipper.ZipAllFiles;
  finally
    for j:=0 to ContentStreams.Count-1 do
      TObject(ContentStreams[j]).Free;
    ContentStreams.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  UnzipArchiveFiles:=TStringList.Create;
  try
    CallBackHandler.OriginalContent:=OriginalContent;
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>Entries) then
    begin
      result:=false;
      writeln('TestZipEntries failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected '+inttostr(Entries));
      exit;
    end;
    i:=0;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    while i<Entries do
    begin
      if CallBackHandler.StreamResult then
      begin
        UnzipArchiveFiles.Clear;
        UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
        Unzipper.UnZipFiles(UnzipArchiveFiles);
        // This will kick off the DoCreateOutZipStream/DoDoneOutZipStream handlers
        inc(i);
      end
      else
      begin
        break; // Handler has reported error; stop loop
      end;
    end;
  finally
    Unzipper.Free;
    CallBackHandler.Free;
    UnzipArchiveFiles.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function TestEmptyZipEntries(Entries: qword): boolean;
// Same as TestZipEntries, except uses empty data:
// useful for testing large number of files
var
  CallBackHandler: TCallBackHandler;
  DestFile: ansistring;
  i: qword;
  j: integer;
  ContentStreams: TFPList;
  ContentStream: TNullStream;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'EZ'+inttostr(Entries)+'_');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ContentStreams:=TFPList.Create;
  try
    i:=0;
    while i<Entries do
    begin
      ContentStream:=TNullStream.Create;
      ContentStreams.Add(ContentStream);
      // Start filenames at 1
      Zipper.Entries.AddFileEntry(TStringStream(ContentStreams.Items[i]), format('%U',[i+1]));
      if (i mod 100)=0 then
        write(i,' ');
      inc(i);
    end;
    Writeln;
    Zipper.ZipAllFiles;
  finally
    for j:=0 to ContentStreams.Count-1 do
      TObject(ContentStreams[j]).Free;
    ContentStreams.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  UnzipArchiveFiles:=TStringList.Create;
  CallBackHandler:=TCallBackHandler.Create;
  try
    // Use callbacks to dump zip output into the bit bucket
    CallBackHandler.PerformChecks:=false;
    CallBackHandler.ShowContent:=false;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>Entries) then
    begin
      result:=false;
      writeln('TestEmptyZipEntries failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected '+inttostr(Entries));
      exit;
    end;
    i:=0;
    while i<Entries do
    begin
      UnzipArchiveFiles.Clear;
      UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
      Unzipper.UnZipFiles(UnzipArchiveFiles);
      if (i mod 100)=0 then
        write(i,' ');
      inc(i);
    end;
    writeln();
  finally
    CallBackHandler.Free;
    Unzipper.Free;
    UnzipArchiveFiles.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function SaveToFileTest: boolean;
var
  NewFileName: ansistring;
  OldFileName: ansistring;
  z: TZipper;
  zfe: TZipFileEntry;
  s: ansistring = 'abcd';
  DefaultStream: TStringStream;
begin
  result:=true;
  OldFileName:=SysUtils.GetTempFileName('', 'OLD');
  NewFileName:=SysUtils.GetTempFileName('', 'NEW');
  z:=TZipper.Create;
  z.FileName:=OldFileName;
  try
    DefaultStream:=TStringStream.Create(s);
    zfe:=z.Entries.AddFileEntry(DefaultStream, 'Compressed');
    z.ZipAllFiles; //saves to OldFileName
    DeleteFile(NewFileName); //delete if present
    z.SaveToFile(NewFileName); //should save to newfilename
    if not(FileExists(NewFileName)) then
    begin
      writeln('Failure: file '+NewFileName+' does not exist.');
      result:=false;
    end
    else
    begin
      result:=true;
    end;
  finally
    DefaultStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;



function TestLargeFileName: boolean;
// Zips/unzips 259-character filename
var
  ArchiveFile: ansistring;
  DestFile: ansistring;
  s: ansistring = 'a';
  DefaultStream: TStringStream;
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  ArchiveFile:=StringOfChar('A',259);
  DestFile:=SysUtils.GetTempFileName('', 'TL');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    Zipper.Entries.AddFileEntry(DefaultStream, ArchiveFile);
    Zipper.ZipAllFiles;
  finally
    DefaultStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (Unzipper.Entries[0].ArchiveFileName<>ArchiveFile) then
    begin
      result:=false;
      writeln('TestLargeFileName failed: found filename length '+
        inttostr(Length(Unzipper.Entries[0].ArchiveFileName)));
      writeln('*'+Unzipper.Entries[0].ArchiveFileName + '*');
      writeln('Expected length '+inttostr(Length(ArchiveFile)));
      writeln('*'+ArchiveFile+'*');
      exit;
    end;
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function TestWindowsPath: boolean;
// Zips filename in a subdirectory with a \ used as separator
// Zip standard requires using /
// On Linux, \ should be seen as a regular part of the filename
var
  FileWithBackslash: ansistring;
  DestFile: ansistring;
  s: ansistring = 'a';
  DefaultStream: TStringStream;
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  FileWithBackslash:='test\afile.txt'; //on Windows, zip should handle this and internally replace \ with /
  // On *nix, this should just be a long file
  DestFile:=SysUtils.GetTempFileName('', 'TW');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    Zipper.Entries.AddFileEntry(DefaultStream, FileWithBackslash);
    Zipper.ZipAllFiles;
  finally
    DefaultStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    {$ifdef mswindows}
    if (pos('\',Unzipper.Entries[0].ArchiveFileName)>0) then
    begin
      result:=false;
      writeln('Failed: found \ in archive filename; expected /:');
      writeln('*'+Unzipper.Entries[0].ArchiveFileName+'*');
      exit;
    end;
    {$else}
    if (pos('\',Unzipper.Entries[0].ArchiveFileName)<=0) then
    begin
      result:=false;
      writeln('Failed: did not find / in archive filename:');
      writeln('*'+Unzipper.Entries[0].ArchiveFileName+'*');
      exit;
    end;
    {$endif}
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;


function TestDiskFileSeparator: boolean;
// Zips a file in a subdirectory added by disk file name only; the archive name must use /
var
  lTempDir, lOldDir, lDiskName, lZipFile: ansistring;
  lFile: TStringList;
  lEntry: TZipFileEntry;
  lZipper: TZipper;
  lUnZipper: TUnZipper;
begin
  result:=true;
  lDiskName:='subdir'+DirectorySeparator+'a.txt';
  lZipper:=TZipper.Create;
  try
    lEntry:=lZipper.Entries.Add as TZipFileEntry;
    lEntry.DiskFileName:=lDiskName;
    if lEntry.ArchiveFileName<>'subdir/a.txt' then
    begin
      result:=false;
      writeln('Failed: archive name of an entry with only a disk name is *'+lEntry.ArchiveFileName+
        '*, expected *subdir/a.txt*');
      exit;
    end;
  finally
    lZipper.Free;
  end;

  lTempDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tczipper'+IntToStr(GetProcessID);
  ForceDirectories(lTempDir+DirectorySeparator+'subdir');
  lZipFile:=lTempDir+DirectorySeparator+'test.zip';
  lOldDir:=GetCurrentDir;
  lFile:=TStringList.Create;
  try
    lFile.Add('a');
    lFile.SaveToFile(lTempDir+DirectorySeparator+lDiskName);
    SetCurrentDir(lTempDir);
    lZipper:=TZipper.Create;
    try
      lZipper.FileName:=lZipFile;
      lZipper.Entries.AddFileEntry(lDiskName);
      lZipper.ZipAllFiles;
    finally
      lZipper.Free;
    end;
    SetCurrentDir(lOldDir);
    lUnZipper:=TUnZipper.Create;
    try
      lUnZipper.FileName:=lZipFile;
      lUnZipper.Examine;
      if (lUnZipper.Entries.Count<>1) or (lUnZipper.Entries[0].ArchiveFileName<>'subdir/a.txt') then
      begin
        result:=false;
        if lUnZipper.Entries.Count>0 then
          writeln('Failed: archive name in zip is *'+lUnZipper.Entries[0].ArchiveFileName+'*, expected *subdir/a.txt*')
        else
          writeln('Failed: zip has no entries');
      end;
    finally
      lUnZipper.Free;
    end;
  finally
    SetCurrentDir(lOldDir);
    lFile.Free;
    DeleteFile(lZipFile);
    DeleteFile(lTempDir+DirectorySeparator+lDiskName);
    RemoveDir(lTempDir+DirectorySeparator+'subdir');
    RemoveDir(lTempDir);
  end;
end;


// Write a zip aZipFile with entry 'dir\b.txt', made on host aOS
procedure WriteBackslashZip(const aZipFile: ansistring; aOS: byte);
var
  lZipper: TZipper;
  lContent: TStringStream;
  lData: TBytes;
  lFile: TFileStream;
  i: integer;
begin
  lContent:=TStringStream.Create('b');
  lZipper:=TZipper.Create;
  try
    lZipper.FileName:=aZipFile;
    lZipper.Entries.AddFileEntry(lContent,'dirXb.txt');
    lZipper.ZipAllFiles;
  finally
    lZipper.Free;
    lContent.Free;
  end;
  lFile:=TFileStream.Create(aZipFile,fmOpenRead);
  try
    SetLength(lData,lFile.Size);
    lFile.ReadBuffer(lData[0],Length(lData));
  finally
    lFile.Free;
  end;
  for i:=0 to Length(lData)-9 do
    if (lData[i]=Ord('d')) and (lData[i+1]=Ord('i')) and (lData[i+2]=Ord('r')) and (lData[i+3]=Ord('X'))
      and (lData[i+4]=Ord('b')) then
      lData[i+3]:=Ord('\');
  for i:=0 to Length(lData)-6 do
    if (lData[i]=$50) and (lData[i+1]=$4B) and (lData[i+2]=1) and (lData[i+3]=2) then
      lData[i+5]:=aOS;
  lFile:=TFileStream.Create(aZipFile,fmCreate);
  try
    lFile.WriteBuffer(lData[0],Length(lData));
  finally
    lFile.Free;
  end;
end;


// Archive name of the single entry in aZipFile, examined with ConvertBackslashes set to aConvert
function ExaminedName(const aZipFile: ansistring; aConvert: boolean): ansistring;
var
  lUnZipper: TUnZipper;
begin
  lUnZipper:=TUnZipper.Create;
  try
    lUnZipper.FileName:=aZipFile;
    lUnZipper.ConvertBackslashes:=aConvert;
    lUnZipper.Examine;
    Result:=lUnZipper.Entries[0].ArchiveFileName;
  finally
    lUnZipper.Free;
  end;
end;


function TestConvertBackslashes: boolean;
// Reads entries with \ in their name, created on a FAT and on a Unix host
var
  lTempDir, lZipFile, lName, lDefault: ansistring;
  lUnZipper: TUnZipper;

  procedure Check(const aWhat, aActual, aExpected: ansistring);
  begin
    if aActual<>aExpected then
    begin
      result:=false;
      writeln('Failed: '+aWhat+': *'+aActual+'*, expected *'+aExpected+'*');
    end;
  end;

begin
  result:=true;
  if DirectorySeparator='/' then
    lDefault:='dir\b.txt'
  else
    lDefault:='dir/b.txt';
  lTempDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tcbackslash'+IntToStr(GetProcessID);
  ForceDirectories(lTempDir);
  lZipFile:=lTempDir+DirectorySeparator+'backslash.zip';
  try
    WriteBackslashZip(lZipFile,OS_FAT);
    Check('FAT entry without ConvertBackslashes',ExaminedName(lZipFile,false),lDefault);
    Check('FAT entry with ConvertBackslashes',ExaminedName(lZipFile,true),'dir/b.txt');
    lUnZipper:=TUnZipper.Create;
    try
      lUnZipper.FileName:=lZipFile;
      lUnZipper.OutputPath:=lTempDir;
      lUnZipper.ConvertBackslashes:=true;
      lUnZipper.UnZipAllFiles;
    finally
      lUnZipper.Free;
    end;
    lName:=lTempDir+DirectorySeparator+'dir'+DirectorySeparator+'b.txt';
    if not FileExists(lName) then
    begin
      result:=false;
      writeln('Failed: FAT entry with ConvertBackslashes not extracted to '+lName);
    end;
    DeleteFile(lName);
    RemoveDir(lTempDir+DirectorySeparator+'dir');
    WriteBackslashZip(lZipFile,OS_UNIX);
    Check('Unix entry with ConvertBackslashes',ExaminedName(lZipFile,true),lDefault);
  finally
    DeleteFile(lZipFile);
    RemoveDir(lTempDir);
  end;
end;


type
  { TTerminateHandler }

  TTerminateHandler = class(TObject)
  public
    UnZipper: TUnZipper;
    CurrentFile: ansistring;
    // Record the name of the entry being extracted
    procedure DoStartFile(Sender: TObject; const AFileName: ansistring);
    // Terminate the unzipper while big.bin is extracted
    procedure DoProgress(Sender: TObject; const Pct: double);
  end;

procedure TTerminateHandler.DoStartFile(Sender: TObject; const AFileName: ansistring);
begin
  CurrentFile:=ExtractFileName(AFileName);
end;

procedure TTerminateHandler.DoProgress(Sender: TObject; const Pct: double);
begin
  if CurrentFile='big.bin' then
    UnZipper.Terminate;
end;


function TestTerminate: boolean;
// Terminates extraction in the middle of a large entry
var
  lTempDir, lZipFile: ansistring;
  lZipper: TZipper;
  lUnZipper: TUnZipper;
  lSmall, lLast, lBig: TStringStream;
  lData: ansistring;
  lHandler: TTerminateHandler;
  i: integer;
  lSeed: cardinal;

  procedure Check(aCondition: boolean; const aMessage: ansistring);
  begin
    if not aCondition then
    begin
      result:=false;
      writeln('Failed: '+aMessage);
    end;
  end;

begin
  result:=true;
  lTempDir:=IncludeTrailingPathDelimiter(GetTempDir(false))+'tcterminate'+IntToStr(GetProcessID);
  ForceDirectories(lTempDir);
  lZipFile:=lTempDir+DirectorySeparator+'terminate.zip';
  SetLength(lData,2*1024*1024);
  lSeed:=12345;
  for i:=1 to Length(lData) do
  begin
    lSeed:=lSeed*1103515245+12345;
    lData[i]:=AnsiChar(lSeed shr 24);
  end;
  lSmall:=TStringStream.Create('first');
  lBig:=TStringStream.Create(lData);
  lLast:=TStringStream.Create('last');
  lZipper:=TZipper.Create;
  try
    lZipper.FileName:=lZipFile;
    lZipper.Entries.AddFileEntry(lSmall,'a.txt');
    lZipper.Entries.AddFileEntry(lBig,'big.bin');
    lZipper.Entries.AddFileEntry(lLast,'c.txt');
    lZipper.ZipAllFiles;
  finally
    lZipper.Free;
    lSmall.Free;
    lBig.Free;
    lLast.Free;
  end;
  lHandler:=TTerminateHandler.Create;
  lUnZipper:=TUnZipper.Create;
  try
    lHandler.UnZipper:=lUnZipper;
    lUnZipper.FileName:=lZipFile;
    lUnZipper.OutputPath:=lTempDir;
    lUnZipper.OnStartFile:=@lHandler.DoStartFile;
    lUnZipper.OnProgress:=@lHandler.DoProgress;
    try
      lUnZipper.UnZipAllFiles;
    except
      on E: Exception do
        Check(false,'UnZipAllFiles raised '+E.ClassName+': '+E.Message);
    end;
    Check(lUnZipper.Terminated,'Terminated is True after Terminate');
    Check(FileExists(lTempDir+DirectorySeparator+'a.txt'),'entry before the terminated one is extracted');
    Check(not FileExists(lTempDir+DirectorySeparator+'big.bin'),'partially extracted big.bin is removed');
    Check(not FileExists(lTempDir+DirectorySeparator+'c.txt'),'entry after the terminated one is not extracted');
  finally
    lUnZipper.Free;
    lHandler.Free;
    DeleteFile(lTempDir+DirectorySeparator+'a.txt');
    DeleteFile(lTempDir+DirectorySeparator+'big.bin');
    DeleteFile(lTempDir+DirectorySeparator+'c.txt');
    DeleteFile(lZipFile);
    RemoveDir(lTempDir);
  end;
end;


function TestLargeZip64: boolean;
// Tests single zip file with large uncompressed content
// which forces it to zip64 format
var
  ArchiveFile: ansistring;
  DestFile: ansistring;
  ContentStream: TNullStream; //empty contents
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'LZ');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ArchiveFile:='HugeString.txt';

  ContentStream:=TNullStream.Create;
  // About 4Gb; content of 4 bytes+1 added
  ContentStream.Size:=(1+$FFFFFFFF);
  ContentStream.Position:=0;
  writeln('Buffer created');
  try
    Zipper.Entries.AddFileEntry(ContentStream, ArchiveFile);
    writeln('entry added');
    Zipper.ZipAllFiles;
  finally
    ContentStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>1) then
    begin
      result:=false;
      writeln('TestLargeZip64 failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected 1');
      exit;
    end;
    if (Unzipper.Entries[0].ArchiveFileName<>ArchiveFile) then
    begin
      result:=false;
      writeln('TestLargeZip64 failed: found filename length '+
        inttostr(Length(Unzipper.Entries[0].ArchiveFileName)));
      writeln('*'+Unzipper.Entries[0].ArchiveFileName + '*');
      writeln('Expected length '+inttostr(Length(ArchiveFile)));
      writeln('*'+ArchiveFile+'*');
      exit;
    end;
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

var
  code: cardinal; //test result code: 0 for success
begin
  code:=0;
  try
    if FileExists(ParamStr(1)) then
    begin
      writeln('');
      writeln('Started investigating file '+ParamStr(1));
      ShowZipFile(ParamStr(1));
      writeln('Finished investigating file '+ParamStr(1));
      writeln('');
    end;

    writeln('CompareCompressDecompress started');
    try
      if not(CompareCompressDecompress) then code:=code+2; //1 already taken by callback handler
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+2;
      end;
    end;
    writeln('CompareCompressDecompress finished');
    writeln('');

    writeln('CompressSmallStreams started');
    try
      if not(CompressSmallStreams) then code:=code+4;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+4;
      end;
    end;
    writeln('CompressSmallStreams finished');
    writeln('');

    writeln('TestZipEntries(2) started');
    try
      if not(TestZipEntries(2)) then code:=code+8;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+8;
      end;
    end;
    writeln('TestZipEntries(2) finished');
    writeln('');

    writeln('TestLargeFileName started');
    try
      if not(TestLargeFileName) then code:=code+16;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+16;
      end;
    end;
    writeln('TestLargeFileName finished');
    writeln('');

    writeln('TestWindowsPath started');
    try
      if not(TestWindowsPath) then code:=code+32;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+32;
      end;
    end;
    writeln('TestWindowsPath finished');
    writeln('');

    writeln('TestDiskFileSeparator started');
    try
      if not(TestDiskFileSeparator) then code:=code+512;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+512;
      end;
    end;
    writeln('TestDiskFileSeparator finished');
    writeln('');

    writeln('TestConvertBackslashes started');
    try
      if not(TestConvertBackslashes) then code:=code+1024;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+1024;
      end;
    end;
    writeln('TestConvertBackslashes finished');
    writeln('');

    writeln('TestTerminate started');
    try
      if not(TestTerminate) then code:=code+2048;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+2048;
      end;
    end;
    writeln('TestTerminate finished');
    writeln('');

    writeln('TestEmptyZipEntries(10) started');
    // Run testemptyzipentries with a small number to test the test itself... as
    // well as zip structure generated with empty files.
    try
      if not(TestEmptyZipEntries(10)) then code:=code+64;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+64;
      end;
    end;
    writeln('TestEmptyZipEntries(10) finished');
    writeln('');

    writeln('SaveToFileTest started');
    try
      if not(SaveToFileTest) then code:=code+128;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+128;
      end;
    end;
    writeln('SaveToFileTest finished');
    writeln('');

    writeln('TestEmptyZipEntries(65537) started');
    writeln('(note: this will take a long time)');
    {Note: tested tools with this file:
    - info-zip unzip 6.0
    - Ionic's DotNetZip library unzip.exe utility version 1.9.1.8 works
    - 7zip's 7za 9.22 beta works.
    }
    try
      if not(TestEmptyZipEntries(65537)) then code:=code+256;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+256;
      end;
    end;
    writeln('TestEmptyZipEntries(65537) finished');
    writeln('');

    { This test will take a very long time as it tries to zip a 4Gb memory block.
    It is therefore commented out by default }
    {
    writeln('TestLargeZip64 - started');
    if not(TestLargeZip64) then code:=code+thefollowingstatuscode;
    writeln('TestLargeZip64 format - finished');
    writeln('');
    }
  except
    on E: Exception do
    begin
      writeln('');
      writeln('Exception: ');
      writeln(E.Message);
      writeln('');
      if code=0 then code:=maxint; //more or less random error code
    end;
  end;

  if code=0 then
    writeln('Basic zip/unzip tests passed: code '+inttostr(code))
  else
    writeln('Basic zip/unzip tests failed: code '+inttostr(code));
  ExitCode:=code;
end.
