{ Copyright (C) <2005> <Andrew Haines> chmfilewriter.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit chmfilewriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chmwriter;

type
  TChmProject = class;

  TChmProgressCB = procedure (Project: TChmProject; CurrentFile: String) of object;

  { TChmProject }

  TChmProject = class
  private
    FAutoFollowLinks: Boolean;
    FDefaultFont: String;
    FDefaultPage: String;
    FFiles: TStrings;
    FIndexFileName: String;
    FMakeBinaryTOC: Boolean;
    FMakeBinaryIndex: Boolean;
    FMakeSearchable: Boolean;
    FFileName: String;
    FOnProgress: TChmProgressCB;
    FOutputFileName: String;
    FTableOfContentsFileName: String;
    FTitle: String;
  protected
    function GetData(const DataName: String; out PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
    procedure LastFileAdded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: String);
    procedure SaveToFile(AFileName: String);
    procedure WriteChm(AOutStream: TStream);
    function ProjectDir: String;
    // though stored in the project file, it is only there for the program that uses the unit
    // since we actually write to a stream
    property OutputFileName: String read FOutputFileName write FOutputFileName;
    property FileName: String read FFileName write FFileName;
    property Files: TStrings read FFiles write FFiles;
    property AutoFollowLinks: Boolean read FAutoFollowLinks write FAutoFollowLinks;
    property TableOfContentsFileName: String read FTableOfContentsFileName write FTableOfContentsFileName;
    property MakeBinaryTOC: Boolean read FMakeBinaryTOC write FMakeBinaryTOC;
    property MakeBinaryIndex: Boolean read FMakeBinaryIndex write FMakeBinaryIndex;
    property Title: String read FTitle write FTitle;
    property IndexFileName: String read FIndexFileName write FIndexFileName;
    property MakeSearchable: Boolean read FMakeSearchable write FMakeSearchable;
    property DefaultPage: String read FDefaultPage write FDefaultPage;
    property DefaultFont: String read FDefaultFont write FDefaultFont;

    property OnProgress: TChmProgressCB read FOnProgress write FOnProgress;
  end;

implementation

uses XmlCfg, chmsitemap;

{ TChmProject }

function TChmProject.GetData(const DataName: String; out PathInChm: String; out
  FileName: String; var Stream: TStream): Boolean;
begin
  Result := False; // Return true to abort compressing files

  TMemoryStream(Stream).LoadFromFile(ProjectDir+DataName);
  // clean up the filename
  FileName := StringReplace(ExtractFileName(DataName), '\', '/', [rfReplaceAll]);
  FileName := StringReplace(FileName, '//', '/', [rfReplaceAll]);

  PathInChm := '/'+ExtractFilePath(DataName);
  if Assigned(FOnProgress) then FOnProgress(Self, DataName);
end;

procedure TChmProject.LastFileAdded(Sender: TObject);
var
  IndexStream: TFileStream;
  TOCStream: TFileStream;
  Writer: TChmWriter;
  TOCSitemap  : TChmSiteMap;
  IndexSiteMap: TChmSiteMap;
begin
  // Assign the TOC and index files
  Writer := TChmWriter(Sender);
  {$ifdef chmindex}
    Writeln('binindex filename ',IndexFileName);
  {$endif}
  if (IndexFileName <> '') and FileExists(IndexFileName) then begin
    IndexStream := TFileStream.Create(IndexFileName, fmOpenRead);
    Writer.AppendIndex(IndexStream);
    if MakeBinaryIndex then
    begin
      {$ifdef chmindex}
        Writeln('into binindex ');
      {$endif}
      IndexStream.Position := 0;
      IndexSitemap := TChmSiteMap.Create(stIndex);
      indexSitemap.LoadFromStream(IndexStream);
      Writer.AppendBinaryIndexFromSiteMap(IndexSitemap,False);
      IndexSitemap.Free;	
    end;
    IndexStream.Free;
  end;
  if (TableOfContentsFileName <> '') and FileExists(TableOfContentsFileName) then begin
    TOCStream := TFileStream.Create(TableOfContentsFileName, fmOpenRead);
    Writer.AppendTOC(TOCStream);
    if MakeBinaryTOC then
    begin
      TOCStream.Position := 0;
      TOCSitemap := TChmSiteMap.Create(stTOC);
      TOCSitemap.LoadFromStream(TOCStream);
      Writer.AppendBinaryTOCFromSiteMap(TOCSitemap);
      TOCSitemap.Free;
    end;
    TOCStream.Free;
  end;

end;

constructor TChmProject.Create;
begin
  FFiles := TStringList.Create;
end;

destructor TChmProject.Destroy;
begin
  FFIles.Free;
  inherited Destroy;
end;

procedure TChmProject.LoadFromFile(AFileName: String);
var
  Cfg: TXMLConfig;
  FileCount: Integer;
  I: Integer;
begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.Filename := AFileName;
  FileName := AFileName;

  Files.Clear;
  FileCount := Cfg.GetValue('Files/Count/Value', 0);
  for I := 0 to FileCount-1 do begin
    Files.Add(Cfg.GetValue('Files/FileName'+IntToStr(I)+'/Value',''));
  end;
  IndexFileName := Cfg.GetValue('Files/IndexFile/Value','');
  TableOfContentsFileName := Cfg.GetValue('Files/TOCFile/Value','');
  // For chm file merging, bintoc must be false and binindex true. Change defaults in time?
  MakeBinaryTOC := Cfg.GetValue('Files/MakeBinaryTOC/Value', True);
  MakeBinaryIndex:= Cfg.GetValue('Files/MakeBinaryIndex/Value', False);
  AutoFollowLinks := Cfg.GetValue('Settings/AutoFollowLinks/Value', False);
  MakeSearchable := Cfg.GetValue('Settings/MakeSearchable/Value', False);
  DefaultPage := Cfg.GetValue('Settings/DefaultPage/Value', '');
  Title := Cfg.GetValue('Settings/Title/Value', '');
  OutputFileName := Cfg.GetValue('Settings/OutputFileName/Value', '');
  DefaultFont := Cfg.GetValue('Settings/DefaultFont/Value', '');
  Cfg.Free;
end;

procedure TChmProject.SaveToFile(AFileName: String);
var
  Cfg: TXMLConfig;
  I: Integer;

begin
  Cfg := TXMLConfig.Create(nil);
  Cfg.StartEmpty := True;
  Cfg.Filename := FileName;
  Cfg.Clear;
  Cfg.SetValue('Files/Count/Value', Files.Count);
  for I := 0 to Files.Count-1 do begin
    Cfg.SetValue('Files/FileName'+IntToStr(I)+'/Value', Files.Strings[I]);
  end;
  Cfg.SetValue('Files/IndexFile/Value', IndexFileName);
  Cfg.SetValue('Files/TOCFile/Value', TableOfContentsFileName);
  Cfg.SetValue('Files/MakeBinaryTOC/Value',MakeBinaryTOC);
  Cfg.SetValue('Files/MakeBinaryIndex/Value',MakeBinaryIndex);
  Cfg.SetValue('Settings/AutoFollowLinks/Value', AutoFollowLinks);
  Cfg.SetValue('Settings/MakeSearchable/Value', MakeSearchable);
  Cfg.SetValue('Settings/DefaultPage/Value', DefaultPage);
  Cfg.SetValue('Settings/Title/Value', Title);
  Cfg.SetValue('Settings/OutputFileName/Value', OutputFileName);
  Cfg.SetValue('Settings/DefaultFont/Value', DefaultFont);
  Cfg.Flush;
  Cfg.Free;
end;

function TChmProject.ProjectDir: String;
begin
  Result := ExtractFilePath(FileName);
end;

procedure TChmProject.WriteChm(AOutStream: TStream);
var
  Writer: TChmWriter;
  TOCStream,
  IndexStream: TFileStream;

begin
  IndexStream := nil;
  TOCStream := nil;

  Writer := TChmWriter.Create(AOutStream, False);

  // our callback to get data
  Writer.OnGetFileData := @GetData;
  Writer.OnLastFile    := @LastFileAdded;

  // give it the list of files
  Writer.FilesToCompress.AddStrings(Files);

  // now some settings in the chm
  Writer.DefaultPage := DefaultPage;
  Writer.Title := Title;
  Writer.DefaultFont := DefaultFont;
  Writer.FullTextSearch := MakeSearchable;
  Writer.HasBinaryTOC := MakeBinaryTOC;
  Writer.HasBinaryIndex := MakeBinaryIndex;

  // and write!
  Writer.Execute;

  if Assigned(TOCStream) then TOCStream.Free;
  if Assigned(IndexStream) then IndexStream.Free;
end;



end.

