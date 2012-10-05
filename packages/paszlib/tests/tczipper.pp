program tczipper;
{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2012 by the Free Pascal development team

    Tests zip/unzip functionality provided by the FPC zipper.pp unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}

uses SysUtils, classes, zipper, md5;

type
  TCallBackHandler = class(TObject)
  public
    procedure EndOfFile(Sender:TObject; const Ratio:double);
    procedure StartOfFile(Sender:TObject; const AFileName:string);
  end;


procedure TCallBackHandler.EndOfFile(Sender : TObject; Const Ratio : Double);
begin
  if (Ratio<0) then
  begin
    writeln('Found compression ratio '+floattostr(Ratio)+', which should never be lower than 0.');
    halt(3);
  end;
end;

procedure TCallBackHandler.StartOfFile(Sender : TObject; Const AFileName : String);
begin
  if AFileName='' then
  begin
    writeln('Archive filename should not be empty.');
    halt(4);
  end;
end;

var
  code: cardinal;
  CallBackHandler: TCallBackHandler;
  CompressedFile: string;
  FileContents: TStringList;
  UncompressedFile1: string;
  UncompressedFile1Hash: string;
  UncompressedFile2: string;
  UncompressedFile2Hash: string;
  OurZipper: TZipper;
  UnZipper: TUnZipper;
begin
  code := 0;
  UncompressedFile1:=SysUtils.GetTempFileName('', 'UNC');
  UncompressedFile2:=SysUtils.GetTempFileName('', 'UNC');
  CompressedFile:=SysUtils.GetTempFileName('', 'ZP');

  FileContents:=TStringList.Create;
  OurZipper:=TZipper.Create;
  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  try
    // Set up uncompressed files
    FileContents.Add('This is an uncompressed file.');
    FileContents.Add('And another line.');
    FileContents.SaveToFile(UncompressedFile1);
    FileContents.Clear;
    FileContents.Add('Have you looked into using fpcup today?');
    FileContents.Add('It works nicely with fpc and goes well with a fruity red wine, too.');
    FileContents.SaveToFile(UncompressedFile2);
    // Remember their content, so we can compare later.
    UncompressedFile1Hash:=MD5Print(MD5File(UncompressedFile1, MDDefBufSize));
    UncompressedFile2Hash:=MD5Print(MD5File(UncompressedFile2, MDDefBufSize));

    // Test zip functionality.
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
      halt(5);
    end;

    // Delete original files
    DeleteFile(UncompressedFile1);
    DeleteFile(UncompressedFile2);

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
      halt(6);
    end;

    // Compare hashes
    if
      (UncompressedFile1Hash<>MD5Print(MD5File(UncompressedFile1, MDDefBufSize)))
      or
      (UncompressedFile2Hash<>MD5Print(MD5File(UncompressedFile2, MDDefBufSize)))
    then
    begin
      writeln('Unzip failed: uncompressed files are not the same as the originals.');
      halt(7);
    end;

    if code = 0 then
      writeln('Basic zip/unzip tests passed')
    else
      writeln('Basic zip/unzip test failed: ', code);
  finally
    FileContents.Free;
    CallBackHandler.Free;
    OurZipper.Free;
    UnZipper.Free;
    try
      if FileExists(CompressedFile) then DeleteFile(CompressedFile);
      if FileExists(UncompressedFile1) then DeleteFile(UncompressedFile1);
      if FileExists(UncompressedFile2) then DeleteFile(UncompressedFile2);
    finally
      // Ignore errors; operating system should clean out temp files
    end; 
  end;
  Halt(code);
end.
