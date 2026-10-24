{
    TFPImageList: the frames of an animation, pages or icon sizes, read and written with the frame
    methods of the readers and writers; TFPFrameCompositor draws animation frames over each other.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpimagelist;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Math, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, Math, FpImage;
{$ENDIF FPC_DOTTEDUNITS}

type
  { One frame of a TFPImageList. }
  TFPImageFrame = class
  public
    Image: TFPCustomImage;
    Info: TFPFrameInfo;
    OwnsImage: Boolean;
    destructor Destroy; override;
  end;

  { A list of images with their frame information. }
  TFPImageList = class
  private
    FFrames: TFPList;
    FOwnsImages: Boolean;
    FInfo: TFPFramesInfo;
    FImageClass: TFPCustomImageClass;
    function GetCount: Integer;
    function GetFrame(aIndex: Integer): TFPImageFrame;
    function GetImage(aIndex: Integer): TFPCustomImage;
  public
    constructor Create(aOwnsImages: Boolean = True);
    destructor Destroy; override;
    // Removes every frame.
    procedure Clear;
    // Adds aImage as a frame described by aInfo, owned by the list when OwnsImages; returns its index.
    function Add(aImage: TFPCustomImage; const aInfo: TFPFrameInfo): Integer; overload;
    // Adds aImage as a page at 0,0; returns its index.
    function Add(aImage: TFPCustomImage): Integer; overload;
    // Removes frame aIndex.
    procedure Delete(aIndex: Integer);
    // Replaces the frames by those aReader reads from aStream; the list owns the images it creates.
    procedure LoadFromStream(aStream: TStream; aReader: TFPCustomImageReader); overload;
    // Replaces the frames by those of aStream, in the format its contents are found to be.
    procedure LoadFromStream(aStream: TStream); overload;
    // Replaces the frames by those aReader reads from the file aFileName.
    procedure LoadFromFile(const aFileName: String; aReader: TFPCustomImageReader); overload;
    // Replaces the frames by those of the file aFileName, in the format of its extension or else its contents.
    function LoadFromFile(const aFileName: String): Boolean; overload;
    // Writes every frame to aStream with aWriter.
    procedure SaveToStream(aStream: TStream; aWriter: TFPCustomImageWriter);
    // Writes every frame to the file aFileName with aWriter.
    procedure SaveToFile(const aFileName: String; aWriter: TFPCustomImageWriter); overload;
    // Writes every frame to the file aFileName in the format of its extension; False when no writer is found.
    function SaveToFile(const aFileName: String): Boolean; overload;
    // Number of frames.
    property Count: Integer read GetCount;
    // The frames, in order.
    property Frames[aIndex: Integer]: TFPImageFrame read GetFrame; default;
    // The images of the frames.
    property Images[aIndex: Integer]: TFPCustomImage read GetImage;
    // The frames as a whole: canvas size, loop count, background.
    property Info: TFPFramesInfo read FInfo write FInfo;
    // Whether images added with Add are freed with their frame.
    property OwnsImages: Boolean read FOwnsImages write FOwnsImages;
    // Class of the images created when loading.
    property ImageClass: TFPCustomImageClass read FImageClass write FImageClass;
  end;

  { Draws animation frames onto a canvas, following their placement, blending and disposal. }
  TFPFrameCompositor = class
  private
    FCanvas: TFPMemoryImage;
    FRestore: TFPMemoryImage;
    FBackground: TFPColor;
    FPrevious: TRect;
    FPreviousDisposal: TFPFrameDisposal;
    FHasPrevious: Boolean;
    procedure Fill(const aRect: TRect; const aColor: TFPColor);
  public
    // Creates a canvas of aWidth x aHeight pixels filled with aBackground.
    constructor Create(aWidth, aHeight: Integer; const aBackground: TFPColor);
    destructor Destroy; override;
    // Fills the canvas with the background and forgets the frames drawn.
    procedure Reset;
    // Disposes of the frame drawn before, draws aFrame at the place and with the blending of aInfo,
    // and copies the canvas to aResult when it is assigned.
    procedure Add(aFrame: TFPCustomImage; const aInfo: TFPFrameInfo; aResult: TFPCustomImage);
    // The canvas with every frame added so far.
    property Canvas: TFPMemoryImage read FCanvas;
    // The colour disposal to the background leaves.
    property Background: TFPColor read FBackground write FBackground;
  end;

// Returns aSource drawn over aDest with straight alpha.
function AlphaOver(const aSource, aDest: TFPColor): TFPColor;

implementation

function AlphaOver(const aSource, aDest: TFPColor): TFPColor;

var
  lSA, lDA, lOut: Int64;

begin
  if aSource.Alpha = alphaOpaque then
    exit(aSource);
  if aSource.Alpha = alphaTransparent then
    exit(aDest);
  lSA := aSource.Alpha;
  lDA := Int64(aDest.Alpha) * (65535 - lSA) div 65535;
  lOut := lSA + lDA;
  if lOut = 0 then
    exit(colTransparent);
  Result.Red := (Int64(aSource.Red) * lSA + Int64(aDest.Red) * lDA + lOut div 2) div lOut;
  Result.Green := (Int64(aSource.Green) * lSA + Int64(aDest.Green) * lDA + lOut div 2) div lOut;
  Result.Blue := (Int64(aSource.Blue) * lSA + Int64(aDest.Blue) * lDA + lOut div 2) div lOut;
  Result.Alpha := lOut;
end;


{ TFPImageFrame }

destructor TFPImageFrame.Destroy;

begin
  if OwnsImage then
    Image.Free;
  inherited Destroy;
end;


{ TFPImageList }

constructor TFPImageList.Create(aOwnsImages: Boolean);

begin
  inherited Create;
  FFrames := TFPList.Create;
  FOwnsImages := aOwnsImages;
  FInfo := DefaultFramesInfo;
  FImageClass := TFPMemoryImage;
end;


destructor TFPImageList.Destroy;

begin
  Clear;
  FFrames.Free;
  inherited Destroy;
end;


function TFPImageList.GetCount: Integer;

begin
  Result := FFrames.Count;
end;


function TFPImageList.GetFrame(aIndex: Integer): TFPImageFrame;

begin
  if (aIndex < 0) or (aIndex >= FFrames.Count) then
    raise FPImageException.CreateFmt('Frame index %d out of range', [aIndex]);
  Result := TFPImageFrame(FFrames[aIndex]);
end;


function TFPImageList.GetImage(aIndex: Integer): TFPCustomImage;

begin
  Result := GetFrame(aIndex).Image;
end;


procedure TFPImageList.Clear;

var
  i: Integer;

begin
  for i := FFrames.Count - 1 downto 0 do
    TFPImageFrame(FFrames[i]).Free;
  FFrames.Clear;
  FInfo := DefaultFramesInfo;
end;


function TFPImageList.Add(aImage: TFPCustomImage; const aInfo: TFPFrameInfo): Integer;

var
  lFrame: TFPImageFrame;

begin
  if not Assigned(aImage) then
    raise FPImageException.Create('No image to add');
  lFrame := TFPImageFrame.Create;
  lFrame.Image := aImage;
  lFrame.Info := aInfo;
  lFrame.OwnsImage := FOwnsImages;
  Result := FFrames.Add(lFrame);
end;


function TFPImageList.Add(aImage: TFPCustomImage): Integer;

begin
  Result := Add(aImage, DefaultFrameInfo);
end;


procedure TFPImageList.Delete(aIndex: Integer);

begin
  GetFrame(aIndex).Free;
  FFrames.Delete(aIndex);
end;


procedure TFPImageList.LoadFromStream(aStream: TStream; aReader: TFPCustomImageReader);

var
  lImage: TFPCustomImage;
  lInfo: TFPFrameInfo;

begin
  Clear;
  aReader.BeginFrames(aStream);
  try
    repeat
      lImage := FImageClass.Create(0, 0);
      try
        if not aReader.ReadNextFrame(lImage, lInfo) then
          begin
          FreeAndNil(lImage);
          break;
          end;
      except
        lImage.Free;
        raise;
      end;
      Add(lImage, lInfo);
      Frames[Count - 1].OwnsImage := True;
    until False;
  finally
    aReader.EndFrames;
  end;
  FInfo := aReader.FramesInfo;
  FInfo.FrameCount := Count;
end;


procedure TFPImageList.LoadFromStream(aStream: TStream);

var
  lClass: TFPCustomImageReaderClass;
  lReader: TFPCustomImageReader;

begin
  lClass := TFPCustomImage.FindReaderFromStream(aStream);
  if lClass = nil then
    raise FPImageException.Create('No reader found for the stream');
  lReader := lClass.Create;
  try
    LoadFromStream(aStream, lReader);
  finally
    lReader.Free;
  end;
end;


procedure TFPImageList.LoadFromFile(const aFileName: String; aReader: TFPCustomImageReader);

var
  lStream: TFileStream;

begin
  lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(lStream, aReader);
  finally
    lStream.Free;
  end;
end;


function TFPImageList.LoadFromFile(const aFileName: String): Boolean;

var
  lClass: TFPCustomImageReaderClass;
  lReader: TFPCustomImageReader;
  lStream: TFileStream;

begin
  lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    lClass := TFPCustomImage.FindReaderFromFileName(aFileName);
    if Assigned(lClass) then
      begin
      lReader := lClass.Create;
      try
        if not lReader.CheckContents(lStream) then
          lClass := nil;
      finally
        lReader.Free;
        lStream.Position := 0;
      end;
      end;
    if not Assigned(lClass) then
      lClass := TFPCustomImage.FindReaderFromStream(lStream);
    Result := Assigned(lClass);
    if Result then
      begin
      lReader := lClass.Create;
      try
        LoadFromStream(lStream, lReader);
      finally
        lReader.Free;
      end;
      end;
  finally
    lStream.Free;
  end;
end;


procedure TFPImageList.SaveToStream(aStream: TStream; aWriter: TFPCustomImageWriter);

var
  lInfo: TFPFramesInfo;
  i: Integer;

begin
  if Count = 0 then
    raise FPImageException.Create('No frames to write');
  lInfo := FInfo;
  lInfo.FrameCount := Count;
  if (lInfo.Width = 0) and (lInfo.Height = 0) then
    begin
    lInfo.Width := Images[0].Width;
    lInfo.Height := Images[0].Height;
    end;
  aWriter.BeginFrames(aStream, lInfo);
  for i := 0 to Count - 1 do
    aWriter.WriteNextFrame(Frames[i].Image, Frames[i].Info);
  aWriter.EndFrames;
end;


procedure TFPImageList.SaveToFile(const aFileName: String; aWriter: TFPCustomImageWriter);

var
  lStream: TFileStream;

begin
  lStream := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(lStream, aWriter);
  finally
    lStream.Free;
  end;
end;


function TFPImageList.SaveToFile(const aFileName: String): Boolean;

var
  lClass: TFPCustomImageWriterClass;
  lWriter: TFPCustomImageWriter;

begin
  lClass := TFPCustomImage.FindWriterFromFileName(aFileName);
  Result := Assigned(lClass);
  if not Result then
    exit;
  lWriter := lClass.Create;
  try
    SaveToFile(aFileName, lWriter);
  finally
    lWriter.Free;
  end;
end;


{ TFPFrameCompositor }

constructor TFPFrameCompositor.Create(aWidth, aHeight: Integer; const aBackground: TFPColor);

begin
  inherited Create;
  FBackground := aBackground;
  FCanvas := TFPMemoryImage.Create(aWidth, aHeight);
  Reset;
end;


destructor TFPFrameCompositor.Destroy;

begin
  FRestore.Free;
  FCanvas.Free;
  inherited Destroy;
end;


procedure TFPFrameCompositor.Fill(const aRect: TRect; const aColor: TFPColor);

var
  x, y: Integer;

begin
  for y := Max(aRect.Top, 0) to Min(aRect.Bottom, FCanvas.Height) - 1 do
    for x := Max(aRect.Left, 0) to Min(aRect.Right, FCanvas.Width) - 1 do
      FCanvas.Colors[x, y] := aColor;
end;


procedure TFPFrameCompositor.Reset;

begin
  Fill(Rect(0, 0, FCanvas.Width, FCanvas.Height), FBackground);
  FHasPrevious := False;
end;


procedure TFPFrameCompositor.Add(aFrame: TFPCustomImage; const aInfo: TFPFrameInfo; aResult: TFPCustomImage);

var
  x, y, lX, lY: Integer;

begin
  if FHasPrevious then
    case FPreviousDisposal of
      fdBackground:
        Fill(FPrevious, FBackground);
      fdPrevious:
        if Assigned(FRestore) then
          for y := 0 to FCanvas.Height - 1 do
            for x := 0 to FCanvas.Width - 1 do
              FCanvas.Colors[x, y] := FRestore.Colors[x, y];
    end;
  if aInfo.Disposal = fdPrevious then
    begin
    if not Assigned(FRestore) then
      FRestore := TFPMemoryImage.Create(0, 0);
    FRestore.SetSize(FCanvas.Width, FCanvas.Height);
    for y := 0 to FCanvas.Height - 1 do
      for x := 0 to FCanvas.Width - 1 do
        FRestore.Colors[x, y] := FCanvas.Colors[x, y];
    end;
  for y := 0 to aFrame.Height - 1 do
    begin
    lY := aInfo.Top + y;
    if (lY < 0) or (lY >= FCanvas.Height) then
      continue;
    for x := 0 to aFrame.Width - 1 do
      begin
      lX := aInfo.Left + x;
      if (lX < 0) or (lX >= FCanvas.Width) then
        continue;
      if aInfo.Blend = fbOver then
        FCanvas.Colors[lX, lY] := AlphaOver(aFrame.Colors[x, y], FCanvas.Colors[lX, lY])
      else
        FCanvas.Colors[lX, lY] := aFrame.Colors[x, y];
      end;
    end;
  FPrevious := Rect(aInfo.Left, aInfo.Top, aInfo.Left + aFrame.Width, aInfo.Top + aFrame.Height);
  FPreviousDisposal := aInfo.Disposal;
  FHasPrevious := True;
  if Assigned(aResult) then
    begin
    aResult.SetSize(FCanvas.Width, FCanvas.Height);
    for y := 0 to FCanvas.Height - 1 do
      for x := 0 to FCanvas.Width - 1 do
        aResult.Colors[x, y] := FCanvas.Colors[x, y];
    end;
end;


end.
