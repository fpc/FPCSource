{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi

    Source files handling

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit jarsourcehandler;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, sourcehandler, zipper;

const
  resbasedir = 'org'+DirectorySeparator+'freepascal'+DirectorySeparator+'rawresources'+DirectorySeparator;

type
  ESourceFilesException = class(Exception);
  ECantOpenFileException = class(ESourceFilesException);
  EUnknownInputFormatException = class(ESourceFilesException);
  
type

  { TSourceFiles }

  TJarSourceFiles = class(TSourceFiles)
  private
  protected
  public
    procedure Load(aResources : TZipper);reintroduce;
  end;
  
implementation

uses msghandler, closablefilestream;

{ TJarSourceFiles }

procedure TJarSourceFiles.Load(aResources: TZipper);
var aStream : TClosableFileStream;
    i : integer;
begin
  if fFileList.Count<>0 then
    begin
      aResources.Entries.AddFileEntry('org/');
      aResources.Entries.AddFileEntry('org/freepascal/');
      aResources.Entries.AddFileEntry(resbasedir);
    end;
  try
    for i:=0 to fFileList.Count-1 do
    begin
      Messages.DoVerbose(Format('Trying to open file %s...',[fFileList[i]]));
      try
        aStream:=TClosableFileStream.Create(fFileList[i],fmOpenRead or fmShareDenyWrite);
      except
        raise ECantOpenFileException.Create(fFileList[i]);
      end;
      astream.Free;
      aResources.Entries.AddFileEntry(fFileList[i],resbasedir+fFileList[i]);
    end;
    Messages.DoVerbose(Format('%d resources read.',[aResources.Entries.Count]));
  finally
  end;
end;

end.

