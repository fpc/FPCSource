program pasbunzip2;
{
    This file is part of the Free Pascal packages.
    Copyright (c) 2012 Reinier Olislagers

    Tests bzip2 decompression.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

{ 
Example .bz2 file extractor.
Decompresses a .bz2 file into another file using the classes-based bzip2stream unit
Usage: bunzip2 compressedfile.bz2
}

uses classes, bzip2stream;

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
    try
      Decompressed:=TDecompressBzip2Stream.Create(InFile);
    except      
      // So[5mething went wrong, e.g. invalid format
      // Now get out of function with result false
      exit;
    end;
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
    end;
  finally
    InFile.Free;
  end;
end;

var
  CompressedFile:string;
  DecompressedFile:string;
begin
  CompressedFile:=ParamStr(1);
  if CompressedFile='' then
  begin
    writeln('Usage: '+ParamStr(0)+' <file>');
    halt(13); // show error in exit code
  end;
  DecompressedFile:=CompressedFile+'.out';
  if Decompress(CompressedFile, DecompressedFile) then
  begin
    writeln('Decompressed '+CompressedFile+' to '+DecompressedFile);
  end
  else
  begin  
    writeln('An error occurred while decompressing '+CompressedFile);
    halt(1); // show error in exit code
  end;
end.
