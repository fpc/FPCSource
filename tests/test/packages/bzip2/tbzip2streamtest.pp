{ %skiptarget=os2,emx }
(* This test program tries to link in a resource file in MS Windows format *)
(* which is not supported for the OS2 and EMX targets.                     *)
program bunzip2test;
{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2012 by the Free Pascal development team

    Tests bzip2 decompression.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
{$mode objfpc}{$h+}

uses SysUtils, classes, bzip2stream, md5;
// Uses new bunzip2 code (using classes, not objects) to test decompression of a bzip2 compressed file.

{$R testbzip2.res} //contains readme.txt.bz2
//Change this whenever you change the test resource:
const ExpectedHash='4ab247ef61f1f9a6fec26493aab823cd';

function Decompress(SourceFile, TargetFile: string): boolean;
var
  InFile:TFileStream;
  Decompressed:TDecompressBzip2Stream;
  OutFile:TFileStream;
  Buffer: Pointer;
  i: integer;
const buffersize=$2000;
begin
  result:=false; //fail by default
  InFile:=TFileStream.Create(SourceFile, fmOpenRead);
  try
    Decompressed:=TDecompressBzip2Stream.Create(InFile);
    OutFile:=TFileStream.Create(TargetFile, fmCreate);
    try
      //We don't have seek on the TDecompressBzip2stream, so can't use
      //CopyFrom...
      //Decompressed.CopyFrom(InFile, InFile.Size);
      GetMem(Buffer,BufferSize);
      repeat
        i:=Decompressed.Read(buffer^,BufferSize);
        if i>0 then
          OutFile.WriteBuffer(buffer^,i);
      until i<BufferSize;
      result:=true;
    finally
      Decompressed.Free;
      OutFile.Free;
  	  FreeMem(Buffer, BufferSize);
    end;
  finally
    InFile.Free;
  end;
end;

var
  code: cardinal;
  CompressedFile: string;
  ExampleFileResourceStream: TResourceStream;
  ExampleFileStream: TFileStream;
  UncompressedFile: string;
  UncompressedHash: string;
begin
  code := 0;
  UncompressedFile:=SysUtils.GetTempFileName(EmptyStr, 'UNC');
  CompressedFile:=SysUtils.GetTempFileName(EmptyStr, 'BZ2');

  try
    // Set up test bz2 file
    // create a resource stream which points to our resource
    ExampleFileResourceStream := TResourceStream.Create(HInstance, 'ALL', 'RT_RCDATA');
    try
      ExampleFileStream := TFileStream.Create(CompressedFile, fmCreate);
      try
        ExampleFileStream.CopyFrom(ExampleFileResourceStream, ExampleFileResourceStream.Size);
      finally
        ExampleFileStream.Free;
      end;
    finally
      ExampleFileResourceStream.Free;
    end;

    // Actual decompression
    if decompress(CompressedFile, UncompressedFile) then
    begin
      // Now check if contents match.
      UncompressedHash:=MD5Print(MD5File(UncompressedFile, MDDefBufSize));
      if UncompressedHash=ExpectedHash then
      begin
        code:=0; //success
      end
      else
      begin
        writeln('MD5 hash comparison between original file and uncompressed file failed');
        writeln('Got hash:'+UncompressedHash);
        writeln('Expected:'+ExpectedHash);
        code:=2;
      end;
    end
    else
    begin
      writeln('bunzip2 decompression failure');
      code:=1;
    end;
  
  
    if code = 0 then
      writeln('Basic bzip2 tests passed')
    else
      writeln('Basic bzip2 test failed: ', code);
  finally
    try
      if CompressedFile<>EmptyStr then DeleteFile(CompressedFile);
      if UncompressedFile<>EmptyStr then DeleteFile(UncompressedFile);
    finally
      // Ignore errors; operating system should clean out temp files
    end; 
  end;
  Halt(code);
end.
