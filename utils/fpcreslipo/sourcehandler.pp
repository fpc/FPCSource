{

    FPCResLipo - Free Pascal External Resource Thinner
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
  Classes, SysUtils, resource, externalreader, externalwriter;

type
  ESourceFilesException = class(Exception);
  ECantOpenFileException = class(ESourceFilesException);
  EUnknownInputFormatException = class(ESourceFilesException);
  ECantCreateFileException = class(ESourceFilesException);

type

  { TSourceFile }

  TSourceFile = class
  private
    fFname : string;
    fStream : TStream;
    fResources : TResources;
    fProcessed : TResources;
    fEndianess : byte;
    fModified : boolean;
    function Delete : boolean;
  protected
  public
    constructor Create(aFileName : string);
    destructor Destroy; override;
    procedure Update;
    property FileName : string read fFname;
    property Resources : TResources read fResources;
    property Processed : TResources read fProcessed;
    property Endianess : byte read fEndianess;
    property Modified : boolean read fModified write fModified;
  end;

  { TSourceFiles }

  TSourceFiles = class
  private
    similarities, simcount : array of integer;
    fList : TFPList;
    function GetItem(index : integer) : TSourceFile;
    function GetCount : integer;
    procedure ResetSimArrays;
    function GetMostCommon : integer;
    procedure CheckSimilarities(idx : integer; aType,aName : TResourceDesc; aLangID : TLangID);
    procedure ExtractCommon(idx : integer; outRes : TResources; aType,aName : TResourceDesc; aLangID : TLangID);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewSourceFile(aFileName : string);
    procedure Process(outRes : TResources);
    procedure Update;
    property Items[index : integer] : TSourceFile read GetItem;
    property Count : integer read GetCount;
  end;
  
implementation

uses msghandler;

{ TSourceFile }

function TSourceFile.Delete : boolean;
begin
  FreeAndNil(fResources);
  FreeAndNil(fStream);
  Result:=DeleteFile(fFname);
  if not Result then
    Messages.DoError(Format('Can''t delete file %s.',[fFname]))
end;

constructor TSourceFile.Create(aFileName: string);
var aReader : TExternalResourceReader;
begin
  fModified:=false;
  fFName:=aFileName;
  Messages.DoVerbose(Format('Trying to open file %s...',[fFName]));
  try
    fStream:=TFileStream.Create(fFName,fmOpenRead or fmShareDenyWrite);
  except
    raise ECantOpenFileException.Create(fFName);
  end;
  aReader:=TExternalResourceReader.Create;
  fResources:=TResources.Create;
  try
    try
      try
        Messages.DoVerbose('Reading resource information...');
        fResources.LoadFromStream(fStream,aReader);
        Messages.DoVerbose(Format('%d resources read.',[fResources.Count]));
        fEndianess:=aReader.Endianess;
      except
        on e : EResourceReaderWrongFormatException do
          raise EUnknownInputFormatException.Create(fFname);
      end;
    except
      FreeAndNil(fResources);
      FreeAndNil(fStream);
    end;
  finally
    aReader.Free;
  end;
  fProcessed:=TResources.Create;
end;

destructor TSourceFile.Destroy;
begin
  if fResources<>nil then fResources.Free;
  if fProcessed<>nil then fProcessed.Free;
  if fStream<>nil then fStream.Free;
end;

procedure TSourceFile.Update;
var tmp : string;
    aWriter : TExternalResourceWriter;
    aStream : TFileStream;
begin
  if not fModified then
  begin
    Messages.DoVerbose(Format('File %s is unchanged.',[fFname]));
    exit;
  end;
  if Resources.Count=0 then
  begin
    if Delete then
      Messages.DoVerbose(Format('No more resources in file %s, deleted',[fFname]));
    exit;
  end;
  
  tmp:=ExtractFileDir(fFname);
  if tmp='' then tmp:='.';
  tmp:=GetTempFileName(tmp,'tmp');
  
  Messages.DoVerbose(Format('Updating file %s...',[fFname]));
  try
    aStream:=TFileStream.Create(tmp,fmCreate or fmShareDenyWrite);
  except
    raise ECantCreateFileException.Create(tmp);
  end;
  try
    aWriter:=TExternalResourceWriter.Create;
    aWriter.Endianess:=Endianess;
    try
      Resources.WriteToStream(aStream,aWriter);
      Messages.DoVerbose(Format('%d resources written.',[Resources.Count]));
    finally
      aWriter.Free;
    end;
  finally
    aStream.Free;
  end;
  
  if not Delete then exit;
  if not RenameFile(tmp,fFname) then
    Messages.DoError(Format('Can''t rename file %s to %s.',[tmp,fFname]))
  else
    Messages.DoVerbose(Format('File %s updated',[fFname]));
end;

{ TSourceFiles }

function TSourceFiles.GetItem(index : integer) : TSourceFile;
begin
  Result:=TSourceFile(fList[index]);
end;

function TSourceFiles.GetCount: integer;
begin
  Result:=fList.Count;
end;

procedure TSourceFiles.ResetSimArrays;
var i : integer;
begin
  for i:=0 to Count-1 do
  begin
    similarities[i]:=i;
    simcount[i]:=1;
  end;
end;

function TSourceFiles.GetMostCommon: integer;
var i : integer;
    max, maxidx : integer;
begin
  max:=0;
  maxidx:=0;
  for i:=0 to Count-1 do
    if simcount[i]>max then
    begin
      max:=simcount[i];
      maxidx:=i;
    end;
  Result:=maxidx;
end;

procedure TSourceFiles.CheckSimilarities(idx: integer; aType,
  aName: TResourceDesc; aLangID: TLangID);
var i,j : integer;
    res1, res2 : TAbstractResource;
begin
  for i:=idx to Count-1 do
  begin
    if similarities[i]<>i then continue;
    try
      res1:=Items[i].Resources.Find(aType,aName,aLangID);
    except
      on e : EResourceNotFoundException do continue;
    end;
    for j:=idx+1 to Count-1 do
    begin
      try
        res2:=Items[j].Resources.Find(aType,aName,aLangID);
      except
        on e : EResourceNotFoundException do continue;
      end;
      if res1.CompareContents(res2) then
      begin
        dec(simcount[similarities[j]]);
        inc(simcount[similarities[i]]);
        similarities[j]:=similarities[i];
      end;
    end;
  end;
end;

procedure TSourceFiles.ExtractCommon(idx: integer; outRes: TResources; aType,
  aName: TResourceDesc; aLangID: TLangID);
var maxidx,i : integer;
    res : TAbstractResource;
begin
  maxidx:=GetMostCommon;
  if simcount[maxidx]<=1 then
  begin
    for i:=idx to Count-1 do
    begin
      try
        res:=Items[i].Resources.Remove(aType,aName,aLangID);
      except
        on e : EResourceNotFoundException do continue;
      end;
      Items[i].Processed.Add(res);
    end;
    exit;
  end;

  res:=Items[maxidx].Resources.Remove(aType,aName,aLangID);
  Items[maxidx].Modified:=true;
  outRes.Add(res);
  for i:=idx to Count-1 do
  begin
    if i=maxidx then continue;
    try
      res:=Items[i].Resources.Remove(aType,aName,aLangID);
    except
      on e : EResourceNotFoundException do continue;
    end;
    if similarities[i]=similarities[maxidx] then
    begin
      res.Free;
      Items[i].Modified:=true;
    end
    else
      Items[i].Processed.Add(res);
  end;
end;

constructor TSourceFiles.Create;
begin
  fList:=TFPList.Create;
end;

destructor TSourceFiles.Destroy;
var i : integer;
begin
  for i:=0 to fList.Count-1 do
    TSourceFile(fList[i]).Free;
  fList.Free;
end;

procedure TSourceFiles.NewSourceFile(aFileName : string);
var aFile : TSourceFile;
begin
  aFile:=TSourceFile.Create(aFileName);
  fList.Add(aFile);
end;

procedure TSourceFiles.Process(outRes: TResources);
var i : integer;
    res : TAbstractResource;
begin
  setlength(similarities,Count);
  setlength(simcount,Count);
  for i:=0 to Count-1 do
  begin
    while Items[i].Resources.Count>0 do
    begin
      ResetSimArrays;
      res:=Items[i].Resources[Items[i].Resources.Count-1];
      if res.Owner<>nil then
        res:=res.Owner;

      CheckSimilarities(i,res._Type,res.Name,res.LangID);
      ExtractCommon(i,outRes,res._Type,res.Name,res.LangID);
    end;
    Items[i].Resources.MoveFrom(Items[i].Processed);
  end;
end;

procedure TSourceFiles.Update;
var i : integer;
begin
  for i:=0 to Count-1 do
    Items[i].Update;
end;

end.

