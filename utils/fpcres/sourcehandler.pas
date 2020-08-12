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

unit sourcehandler;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource;

type
  ESourceFilesException = class(Exception);
  ECantOpenFileException = class(ESourceFilesException);
  EUnknownInputFormatException = class(ESourceFilesException);
  
type

  { TSourceFiles }

  TSourceFiles = class
  private
  protected
    fFileList : TStringList;
    fRCIncludeDirs: TStringList;
    fRCDefines: TStringList;
    fStreamList : TFPList;
    fRCMode: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(aResources : TResources);
    property FileList : TStringList read fFileList;
    property RCIncludeDirs: TStringList read fRCIncludeDirs;
    property RCDefines: TStringList read fRCDefines;
    property RCMode: Boolean read fRCMode write fRCMode;
  end;
  
implementation

uses msghandler, closablefilestream, rcreader;

{ TSourceFiles }

constructor TSourceFiles.Create;
begin
  inherited Create;
  fFileList:=TStringList.Create;
  fStreamList:=TFPList.Create;
  fRCDefines:= TStringList.Create;
  fRCIncludeDirs:= TStringList.Create;
  fRCMode:=False;
end;

destructor TSourceFiles.Destroy;
var i : integer;
begin
  fRCIncludeDirs.Free;
  fRCDefines.Free;
  fFileList.Free;
  for i:=0 to fStreamList.Count-1 do
    TStream(fStreamList[i]).Free;
  fStreamList.Free;
  inherited;
end;

procedure TSourceFiles.Load(aResources: TResources);
var aReader : TAbstractResourceReader;
    aStream : TClosableFileStream;
    i : integer;
    tmpres : TResources;
    olddir : String;
begin
  olddir:=GetCurrentDir;
  tmpres:=TResources.Create;
  try
    for i:=0 to fFileList.Count-1 do
    begin
      Messages.DoVerbose(Format('Trying to open file %s...',[fFileList[i]]));
      try
        aStream:=TClosableFileStream.Create(fFileList[i],fmOpenRead or fmShareDenyWrite);
      except
        raise ECantOpenFileException.Create(fFileList[i]);
      end;
      fStreamList.Add(aStream);
      { the RC reader reads anything, so handle that separately }
      if fRCMode then
        aReader:=TRCResourceReader.Create
      else
        try
          aReader:=TResources.FindReader(aStream);
        except
          raise EUnknownInputFormatException.Create(fFileList[i]);
        end;
      Messages.DoVerbose(Format('Chosen reader: %s',[aReader.Description]));
      try
        Messages.DoVerbose('Reading resource information...');
        if aReader is TRCResourceReader then begin
          TRCResourceReader(aReader).RCIncludeDirs.Assign(fRCIncludeDirs);
          TRCResourceReader(aReader).RCDefines.Assign(fRCDefines);
          SetCurrentDir(ExtractFilePath(ExpandFileName(fFileList[i])));
        end;
        tmpres.LoadFromStream(aStream,aReader);
        aResources.MoveFrom(tmpres);
        Messages.DoVerbose('Resource information read');
      finally
        if aReader is TRCResourceReader then begin
          SetCurrentDir(olddir);
        end;
        aReader.Free;
      end;
    end;
    Messages.DoVerbose(Format('%d resources read.',[aResources.Count]));
  finally
    tmpres.Free;
  end;
end;

end.

