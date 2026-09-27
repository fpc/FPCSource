{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TImageHandlers implementations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TImageHandlersManager }

constructor TImageHandlersManager.Create;
begin
  inherited create;
  FData := Tlist.Create;
end;

destructor TImageHandlersManager.Destroy;
var r : integer;
begin
  for r := FData.count-1 downto 0 do
    TIHData(FData[r]).Free;
  FData.Free;
  inherited Destroy;
end;

function CalcDefExt (TheExtensions:string) : string;
var p : integer;
begin
  p := pos (';',TheExtensions);
  if p = 0 then
    result := TheExtensions
  else
    result := copy(TheExtensions, 1, p-1);
end;

procedure TImageHandlersManager.RegisterImageHandlers (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    FPImgError (StrTypeAlreadyExist,[ATypeName]);
  ih := TIHData.Create;
  with ih do
    begin
    FTypeName := ATypeName;
    FExtension := lowercase(TheExtensions);
    FDefaultExt := CalcDefExt (TheExtensions);
    FReader := AReader;
    FWriter := AWriter;
    end;
  FData.Add (ih);
end;

procedure TImageHandlersManager.RegisterImageReader (const ATypeName,TheExtensions:string;
                   AReader:TFPCustomImageReaderClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    begin
      if assigned (ih.FReader) then
        FPImgError (StrTypeReaderAlreadyExist,[ATypeName])
      else
        ih.FReader := AReader;
    end
  else
    begin
    ih := TIHData.Create;
    with ih do
      begin
      FTypeName := ATypeName;
      FExtension := Lowercase(TheExtensions);
      FDefaultExt := CalcDefExt (TheExtensions);
      FReader := AReader;
      FWriter := nil;
      end;
    FData.Add (ih);
    end;
end;

procedure TImageHandlersManager.RegisterImageWriter (const ATypeName,TheExtensions:string;
                   AWriter:TFPCustomImageWriterClass);
var ih : TIHData;
begin
  ih := GetData (ATypeName);
  if assigned (ih) then
    begin
    if assigned (ih.FWriter) then
      FPImgError (StrTypeWriterAlreadyExist,[ATypeName])
    else
      ih.FWriter := AWriter;
    end
  else
    begin
    ih := TIHData.Create;
    with ih do
      begin
      FTypeName := ATypeName;
      FExtension := lowercase(TheExtensions);
      FDefaultExt := CalcDefExt (TheExtensions);
      FReader := nil;
      FWriter := AWriter;
      end;
    FData.Add (ih);
    end;
end;

procedure TImageHandlersManager.UnregisterImageHandlers(const ATypeName: string; ARemoveReader: boolean = True;
  ARemoveWriter: boolean = True);
var r : integer;
    ih: TIHData;
begin
  if not ARemoveReader and not ARemoveWriter then
    exit;
  r := FData.count;
  repeat
    dec (r);
  until (r < 0) or (compareText (TIHData(FData[r]).FTypeName, ATypeName) = 0);
  if r >= 0 then begin
    ih:= TIHData(FData[r]);
    if ARemoveReader then
      ih.FReader:= nil;
    if ARemoveWriter then
      ih.FWriter:= nil;
    if not Assigned(ih.FReader) and not Assigned(ih.FWriter) then begin
      FData.Delete(r);
      ih.Free;
    end;
  end;
end;

function TImageHandlersManager.GetCount : integer;
begin
  result := FData.Count;
end;

function TImageHandlersManager.GetData (const ATypeName:string) : TIHData;
var r : integer;
begin
  r := FData.count;
  repeat
    dec (r);
  until (r < 0) or (compareText (TIHData(FData[r]).FTypeName, ATypeName) = 0);
  if r >= 0 then
    result := TIHData(FData[r])
  else
    result := nil;
end;

function TImageHandlersManager.GetData (index:integer) : TIHData;
begin
  if (index >= 0) and (index < FData.count) then
    result := TIHData (FData[index])
  else
    result := nil;
end;

function TImageHandlersManager.GetTypeName (index:integer) : string;
var ih : TIHData;
begin
  ih := TIHData (FData[index]);
  result := ih.FTypeName;
end;

function TImageHandlersManager.GetReader (const TypeName:string) : TFPCustomImageReaderClass;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FReader
  else
    result := nil;
end;

function TImageHandlersManager.GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FWriter
  else
    result := nil;
end;

function TImageHandlersManager.GetExt (const TypeName:string) : string;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FExtension
  else
    result := '';
end;

function TImageHandlersManager.GetDefExt (const TypeName:string) : string;
var ih : TIHData;
begin
  ih := GetData (TypeName);
  if assigned(ih) then
    result := ih.FDefaultExt
  else
    result := '';
end;

{ TFPCustomImageHandler }

constructor TFPCustomImageHandler.create;
begin
  inherited create;
end;

procedure TFPCustomImageHandler.Progress(Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);

begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,Stage,PercentDone,RedrawNow,R,Msg,Continue)
  else If Assigned(FImage) then
    // It is debatable whether we should pass ourselves or the image ?
    FImage.Progress(Self,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;

{ TFPCustomImageReader }

constructor TFPCustomImageReader.Create;
begin
  inherited create;
  FDefImageClass := TFPMemoryImage;
  FComposite := True;
  FFramesInfo := DefaultFramesInfo;
end;

function TFPCustomImageReader.ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
begin
  try
    if not assigned(Str) then
      raise FPImageException.Create(ErrorText[StrNoStream]);
    FStream := Str;
    if not assigned(img) then
      result := FDefImageClass.Create(0,0)
    else
      result := Img;
    FImage := result;
    try
      if FImage.UsePalette then
        FImage.Palette.Clear;
      if CheckContents (Str) then
        begin
        InternalRead (Str, result)
        end
      else
        raise FPImageException.Create ('Wrong image format');
    except
      if not assigned(img) then
        FreeAndNil(result);
      raise;
    end;
  finally
    FStream := nil;
    FImage := nil;
  end;
end;

function TFPCustomImageReader.CheckContents (Str:TStream) : boolean;
var InRead : boolean;
begin
  InRead := assigned(FStream);
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  try
    FSTream := Str;
    result := InternalCheck (Str);
  finally
    if not InRead then
      FStream := nil;
  end;
end;

class function TFPCustomImageReader.InternalSize(Str: TStream): TPoint;

begin
  Result.X:=-1;
  Result.Y:=-1;
end;

class function TFPCustomImageReader.ImageSize(Str: TStream): TPoint;
var
  P: Int64;
begin
  P:=Str.Position;
  result := InternalSize (Str);
  Str.Position:=P;
end;


function DefaultFramesInfo: TFPFramesInfo;

begin
  Result.Width := 0;
  Result.Height := 0;
  Result.FrameCount := -1;
  Result.LoopCount := 0;
  Result.Background := colTransparent;
end;


function DefaultFrameInfo: TFPFrameInfo;

begin
  Result.Kind := fkPage;
  Result.Left := 0;
  Result.Top := 0;
  Result.Delay := 0;
  Result.Disposal := fdNone;
  Result.Blend := fbSource;
  Result.Name := '';
end;


function TFPCustomImageReader.InternalBeginFrames(Str: TStream): TFPFramesInfo;

begin
  Result := DefaultFramesInfo;
  Result.FrameCount := 1;
end;


function TFPCustomImageReader.InternalReadFrame(Str: TStream; Img: TFPCustomImage; var aInfo: TFPFrameInfo): Boolean;

begin
  Result := FFramesRead = 0;
  if Result then
    InternalRead(Str, Img);
end;


procedure TFPCustomImageReader.InternalEndFrames(Str: TStream);

begin
end;


function TFPCustomImageReader.BeginFrames(Str: TStream): TFPFramesInfo;

begin
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  FFrameStream := nil;
  FFramesRead := 0;
  FFramesInfo := DefaultFramesInfo;
  if not CheckContents(Str) then
    raise FPImageException.Create('Wrong image format');
  FStream := Str;
  try
    FFramesInfo := InternalBeginFrames(Str);
  finally
    FStream := nil;
  end;
  FFrameStream := Str;
  Result := FFramesInfo;
end;


function TFPCustomImageReader.ReadNextFrame(Img: TFPCustomImage; out aInfo: TFPFrameInfo): Boolean;

begin
  if not assigned(FFrameStream) then
    raise FPImageException.Create('BeginFrames was not called');
  if not assigned(Img) then
    raise FPImageException.Create('No image to read a frame into');
  aInfo := DefaultFrameInfo;
  FStream := FFrameStream;
  FImage := Img;
  try
    if Img.UsePalette then
      Img.Palette.Clear;
    Result := InternalReadFrame(FFrameStream, Img, aInfo);
  finally
    FStream := nil;
    FImage := nil;
  end;
  if Result then
    begin
    Inc(FFramesRead);
    if (FFramesInfo.Width = 0) and (FFramesInfo.Height = 0) then
      begin
      FFramesInfo.Width := Img.Width;
      FFramesInfo.Height := Img.Height;
      end;
    end;
end;


function TFPCustomImageReader.ReadNextFrame(out aInfo: TFPFrameInfo): TFPCustomImage;

begin
  Result := FDefImageClass.Create(0, 0);
  try
    if not ReadNextFrame(Result, aInfo) then
      FreeAndNil(Result);
  except
    Result.Free;
    raise;
  end;
end;


procedure TFPCustomImageReader.EndFrames;

begin
  if not assigned(FFrameStream) then
    exit;
  try
    InternalEndFrames(FFrameStream);
  finally
    FFrameStream := nil;
  end;
end;


{ TFPCustomImageWriter }

procedure TFPCustomImageWriter.ImageWrite (Str:TStream; Img:TFPCustomImage; aTruncate : Boolean = True);
begin
  if not assigned(img) then
    raise FPImageException.Create(ErrorText[StrNoImageToWrite]);
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  try
    FStream := str;
    FImage := img;
    if aTruncate then
      begin
      Str.position := 0;
      Str.Size := 0;
      end;
    InternalWrite(Str, Img);
  finally
    FStream := nil;
    FImage := nil;
  end;
end;


class function TFPCustomImageWriter.FrameKinds: TFPFrameKinds;

begin
  Result := [];
end;


procedure TFPCustomImageWriter.InternalBeginFrames(Str: TStream; const aInfo: TFPFramesInfo);

begin
end;


procedure TFPCustomImageWriter.InternalWriteFrame(Str: TStream; Img: TFPCustomImage; const aInfo: TFPFrameInfo);

begin
  if FFramesWritten > 0 then
    raise FPImageException.CreateFmt('%s writes one image only', [ClassName]);
  InternalWrite(Str, Img);
end;


procedure TFPCustomImageWriter.InternalEndFrames(Str: TStream);

begin
end;


procedure TFPCustomImageWriter.BeginFrames(Str: TStream; const aInfo: TFPFramesInfo; aTruncate: Boolean);

begin
  if not assigned(Str) then
    raise FPImageException.Create(ErrorText[StrNoStream]);
  FFrameStream := nil;
  FFramesWritten := 0;
  FFramesInfo := aInfo;
  if aTruncate then
    begin
    Str.Position := 0;
    Str.Size := 0;
    end;
  FStream := Str;
  try
    InternalBeginFrames(Str, aInfo);
  finally
    FStream := nil;
  end;
  FFrameStream := Str;
end;


procedure TFPCustomImageWriter.WriteNextFrame(Img: TFPCustomImage; const aInfo: TFPFrameInfo);

begin
  if not assigned(FFrameStream) then
    raise FPImageException.Create('BeginFrames was not called');
  if not assigned(Img) then
    raise FPImageException.Create(ErrorText[StrNoImageToWrite]);
  FStream := FFrameStream;
  FImage := Img;
  try
    InternalWriteFrame(FFrameStream, Img, aInfo);
  finally
    FStream := nil;
    FImage := nil;
  end;
  Inc(FFramesWritten);
end;


procedure TFPCustomImageWriter.EndFrames;

begin
  if not assigned(FFrameStream) then
    raise FPImageException.Create('BeginFrames was not called');
  FStream := FFrameStream;
  try
    if FFramesWritten = 0 then
      raise FPImageException.Create(ErrorText[StrNoImageToWrite]);
    InternalEndFrames(FFrameStream);
  finally
    FStream := nil;
    FFrameStream := nil;
  end;
end;


