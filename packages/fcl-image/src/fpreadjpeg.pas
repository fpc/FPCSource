{ Copyright (C) 2003 Mattias Gaertner

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
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

  ToDo:
    - palette

    2023-07  - Massimo Magnano
             - procedure inside InternalRead moved to protected methods (virtual)
             - added Resolution support
}
unit FPReadJPEG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FPImage, JPEGcomn, JPEGLib, JdAPImin, JDataSrc, JdAPIstd, JmoreCfg;

type
  //MaxM: these common types should stay only in JPEGcomn units, but we should change LCL uses
  TFPJPEGCompressionQuality = 1..100;   // 100 = best quality, 25 = pretty awful

  PFPJPEGProgressManager = ^TFPJPEGProgressManager;
  TFPJPEGProgressManager = record
    pub : jpeg_progress_mgr;
    instance: TObject;
    last_pass: Integer;
    last_pct: Integer;
    last_time: Integer;
    last_scanline: Integer;
  end;

  TJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
  TJPEGReadPerformance = (jpBestQuality, jpBestSpeed);

  TExifOrientation = ( // all angles are clockwise
    eoUnknown, eoNormal, eoMirrorHor, eoRotate180, eoMirrorVert,
    eoMirrorHorRot270, eoRotate90, eoMirrorHorRot90, eoRotate270
  );

  { TFPReaderJPEG }
  { This is a FPImage reader for jpeg images. }

  TFPReaderJPEG = class(TFPCustomImageReader)
  private
    FSmoothing,
    Continue: boolean;
    FMinHeight:integer;
    FMinWidth:integer;
    FWidth: Integer;
    FHeight: Integer;
    FGrayscale: boolean;
    FProgressiveEncoding: boolean;
    FError: jpeg_error_mgr;
    FProgressMgr: TFPJPEGProgressManager;
    FInfo: jpeg_decompress_struct;
    FScale: TJPEGScale;
    FPerformance: TJPEGReadPerformance;
    FOrientation: TExifOrientation;

    procedure SetPerformance(const AValue: TJPEGReadPerformance);
    procedure SetSmoothing(const AValue: boolean);
  protected
    procedure ReadHeader(Str: TStream; Img: TFPCustomImage); virtual;
    procedure ReadPixels(Str: TStream; Img: TFPCustomImage); virtual;
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
    class function InternalSize(Str:TStream): TPoint; override;
    property CompressInfo : jpeg_decompress_struct Read Finfo Write FInfo;
    property Orientation: TExifOrientation Read FOrientation Write FOrientation;
  public
    constructor Create; override;
    destructor Destroy; override;
    property GrayScale: boolean read FGrayscale;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property Performance: TJPEGReadPerformance read FPerformance write SetPerformance;
    property Scale: TJPEGScale read FScale write FScale;
    property MinWidth:integer read FMinWidth write FMinWidth;
    property MinHeight:integer read FMinHeight write FMinHeight;
  end;


implementation

uses FPColorSpace;

type
  int_Color_Table = array[0..MAXJSAMPLE+1-1] of int;
  int_table_ptr = ^int_Color_Table;
  INT32_Color_Table = array[0..MAXJSAMPLE+1-1] of INT32;
  INT32_table_ptr = ^INT32_Color_Table;
  my_cconvert_ptr = ^my_color_deconverter;
  my_color_deconverter = record
    pub : jpeg_color_deconverter; { public fields }

    { Private state for YCC^.RGB conversion }
    Cr_r_tab : int_table_ptr;   { => table for Cr to R conversion }
    Cb_b_tab : int_table_ptr;   { => table for Cb to B conversion }
    Cr_g_tab : INT32_table_ptr; { => table for Cr to G conversion }
    Cb_g_tab : INT32_table_ptr; { => table for Cb to G conversion }
  end;


procedure ReadCompleteStreamToStream(SrcStream, DestStream: TStream;
                                     StartSize: integer);
var
  NewLength: Integer;
  ReadLen: Integer;
  Buffer: string;
begin
  if (SrcStream is TMemoryStream) or (SrcStream is TFileStream)
  or (SrcStream is TStringStream)
  then begin
    // read as one block
    DestStream.CopyFrom(SrcStream,SrcStream.Size-SrcStream.Position);
  end else begin
    // read exponential
    if StartSize<=0 then StartSize:=1024;
    SetLength(Buffer,StartSize);
    NewLength:=0;
    repeat
      ReadLen:=SrcStream.Read(Buffer[NewLength+1],length(Buffer)-NewLength);
      inc(NewLength,ReadLen);
      if NewLength<length(Buffer) then break;
      SetLength(Buffer,length(Buffer)*2);
    until false;
    if NewLength>0 then
      DestStream.Write(Buffer[1],NewLength);
  end;
end;

procedure JPEGError(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  raise Exception.CreateFmt('JPEG error',[CurInfo^.err^.msg_code]);
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: Integer);
begin
  if CurInfo=nil then exit;
  if msg_level=0 then ;
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
end;

procedure FormatMessage(CurInfo: j_common_ptr; var buffer: string);
begin
  if CurInfo=nil then exit;
  {$ifdef FPC_Debug_Image}
     writeln('FormatMessage ',buffer);
  {$endif}
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;


var
  jpeg_std_error: jpeg_error_mgr;

procedure ProgressCallback(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  // ToDo
end;

{ TFPReaderJPEG }

procedure TFPReaderJPEG.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing=AValue then exit;
  FSmoothing:=AValue;
end;

procedure TFPReaderJPEG.SetPerformance(const AValue: TJPEGReadPerformance);
begin
  if FPerformance=AValue then exit;
  FPerformance:=AValue;
end;

procedure TFPReaderJPEG.ReadHeader(Str: TStream; Img: TFPCustomImage);
var
   S: TSize;

  function TranslateSize(const Sz: TSize): TSize;
  begin
    case FOrientation of
      eoUnknown, eoNormal, eoMirrorHor, eoMirrorVert, eoRotate180: Result := Sz;
      eoMirrorHorRot270, eoRotate90, eoMirrorHorRot90, eoRotate270:
      begin
        Result.Width := Sz.Height;
        Result.Height := Sz.Width;
      end;
    end;
  end;

begin
  jpeg_read_header(@FInfo, TRUE);

  if FInfo.saw_EXIF_marker and (FInfo.orientation >= Ord(Low(TExifOrientation))) and (FInfo.orientation <= Ord(High(TExifOrientation))) then
    FOrientation := TExifOrientation(FInfo.orientation)
  else
    FOrientation := Low(TExifOrientation);
  S := TranslateSize(TSize.Create(FInfo.image_width, FInfo.image_height));
  FWidth := S.Width;
  FHeight := S.Height;

  FGrayscale := FInfo.jpeg_color_space = JCS_GRAYSCALE;
  FProgressiveEncoding := jpeg_has_multiple_scans(@FInfo);

  Img.ResolutionUnit:=density_unitToResolutionUnit(CompressInfo.density_unit);
  Img.ResolutionX :=CompressInfo.X_density;
  Img.ResolutionY :=CompressInfo.Y_density;
end;

procedure TFPReaderJPEG.ReadPixels(Str: TStream; Img: TFPCustomImage);
var
  SampArray: JSAMPARRAY;
  SampRow: JSAMPROW;
  Color: TFPColor;
  LinesRead: Cardinal;
  x: Integer;
  y: Integer;
  c: word;
  Status,Scan: integer;
  ReturnValue,RestartLoop: Boolean;

  procedure InitReadingPixels;
  var d1,d2:integer;

    function DToScale(inp:integer):TJPEGScale;
    begin
      if inp>7 then Result:=jsEighth else
      if inp>3 then Result:=jsQuarter else
      if inp>1 then Result:=jsHalf else
      Result:=jsFullSize;
    end;

  begin
    FInfo.scale_num := 1;

    if (FMinWidth>0) and (FMinHeight>0) then
      if (FInfo.image_width>FMinWidth) or (FInfo.image_height>FMinHeight) then
        begin
        d1:=Round((FInfo.image_width / FMinWidth)-0.5);
        d2:=Round((FInfo.image_height /  FMinHeight)-0.5);
        if d1>d2 then fScale:=DToScale(d2) else fScale:=DtoScale(d1);
        end;

    FInfo.scale_denom :=1 shl Byte(FScale); //1
    FInfo.do_block_smoothing := FSmoothing;

    if FGrayscale then FInfo.out_color_space := JCS_GRAYSCALE;
    if (FInfo.out_color_space = JCS_GRAYSCALE) then
      begin
      FInfo.quantize_colors := True;
      FInfo.desired_number_of_colors := 256;
      end;

    if FPerformance = jpBestSpeed then
      begin
      FInfo.dct_method := JDCT_IFAST;
      FInfo.two_pass_quantize := False;
      FInfo.dither_mode := JDITHER_ORDERED;
      // FInfo.do_fancy_upsampling := False;  can create an AV inside jpeglib
      end;

    if FProgressiveEncoding then
      begin
      FInfo.enable_2pass_quant := FInfo.two_pass_quantize;
      FInfo.buffered_image := True;
      end;
  end;

  function TranslatePixel(const Px: TPoint): TPoint;
  begin
    case FOrientation of
      eoUnknown, eoNormal: Result := Px;
      eoMirrorHor:
      begin
        Result.X := FInfo.output_width-1-Px.X;
        Result.Y := Px.Y;
      end;
      eoRotate180:
      begin
        Result.X := FInfo.output_width-1-Px.X;
        Result.Y := FInfo.output_height-1-Px.Y;
      end;
      eoMirrorVert:
      begin
        Result.X := Px.X;
        Result.Y := FInfo.output_height-1-Px.Y;
      end;
      eoMirrorHorRot270:
      begin
        Result.X := Px.Y;
        Result.Y := Px.X;
      end;
      eoRotate90:
      begin
        Result.X := FInfo.output_height-1-Px.Y;
        Result.Y := Px.X;
      end;
      eoMirrorHorRot90:
      begin
        Result.X := FInfo.output_height-1-Px.Y;
        Result.Y := FInfo.output_width-1-Px.X;
      end;
      eoRotate270:
      begin
        Result.X := Px.Y;
        Result.Y := FInfo.output_width-1-Px.X;
      end;
    end;
  end;

  procedure SetPixel(x, y: integer; const C: TFPColor);
  var
    P: TPoint;
  begin
    P := TPoint.Create(x,y);
    P := TranslatePixel(P);
    Img.Colors[P.x, P.y] := C;
  end;

  function CorrectCMYK(const C: TFPColor): TFPColor;
  var
    MinColor: word;
  begin
    // accuracy not 100%
    if C.red<C.green then MinColor:=C.red
    else MinColor:= C.green;
    if C.blue<MinColor then MinColor:= C.blue;
    if MinColor+ C.alpha>$FF then MinColor:=$FF-C.alpha;
    Result.red:=(C.red-MinColor) shl 8;
    Result.green:=(C.green-MinColor) shl 8;
    Result.blue:=(C.blue-MinColor) shl 8;
    Result.alpha:=alphaOpaque;
  end;

  procedure OutputScanLines();
  var
    x: integer;
    //ycbcr:TYCbCr;
    cmyk:TStdCMYK;
    yy,cb,cr :Int;
    shift_temp : INT32;
    cconvert : my_cconvert_ptr;

  begin
    Color.Alpha:=alphaOpaque;
    y:=0;
    while (FInfo.output_scanline < FInfo.output_height) do begin
      // read one line per call
      LinesRead := jpeg_read_scanlines(@FInfo, SampArray, 1);
      if LinesRead<1 then begin
        ReturnValue:=false;
        break;
      end;

      Case FInfo.out_color_space of
      JCS_GRAYSCALE :
        for x:=0 to FInfo.output_width-1 do
        begin
          c:= SampRow^[x] shl 8;
          Color.Red:=c;
          Color.Green:=c;
          Color.Blue:=c;
          SetPixel(x, y, Color);
        end;
      JCS_YCbCr :
        for x:=0 to FInfo.output_width-1 do
        begin
          //MaxM: YCbCr is defined per CCIR 601-1
          //      Y (0 to 1.0) and Cb,Cr (-0.5 to 0.5) is normalized to the range 0..MAXJSAMPLE
          //      We have two ways to convert them, the most accurate is to denormalize
          //      the values like the following commented code, or as is and set SamplePrecision to CENTERJSAMPLE
          //      ycbcr.Y :=SampRow^[x*3+0]/256;
          //      ycbcr.Cb :=(SampRow^[x*3+1]-128)/256;
          //      ycbcr.Cr :=(SampRow^[x*3+2]-128)/256;
          //ycbcr :=TYCbCr.New(SampRow^[x*3+0], SampRow^[x*3+1], SampRow^[x*3+2]);
          //SetPixel(x, y, ycbcr.ToStdRGBA(YCBCr_601, CENTERJSAMPLE).ToExpandedPixel.ToFPColor(false));

          //Use the same Code of PasJPeg (ycc_rgb_convert function)
          yy :=SampRow^[x*3+0];
          cb :=SampRow^[x*3+1];
          cr :=SampRow^[x*3+2];
          cconvert :=my_cconvert_ptr(FInfo.cconvert);
          Color.Red :=  (FInfo.sample_range_limit^[yy + cconvert^.Cr_r_tab^[cr]]);
          shift_temp := cconvert^.Cb_g_tab^[cb] + cconvert^.Cr_g_tab^[cr];
          if shift_temp < 0 then   { SHIFT arithmetic RIGHT }
            Color.Green := (FInfo.sample_range_limit^[yy + int((shift_temp shr 16)
                                  or ( (not INT32(0)) shl (32-16)))])
          else
            Color.Green := (FInfo.sample_range_limit^[yy + int(shift_temp shr 16)]);

          Color.Blue :=  (FInfo.sample_range_limit^[yy + cconvert^.Cb_b_tab^[cb]]);

          Color.Red:=Color.Red shl 8;
          Color.Green:=Color.Green shl 8;
          Color.Blue:=Color.Blue shl 8;

          SetPixel(x, y, Color);
        end;
      JCS_CMYK, JCS_YCCK:
        for x:=0 to FInfo.output_width-1 do
        begin
          //SetPixel(x, y, CorrectCMYK(TFPColor.New(SampRow^[x*4+0], SampRow^[x*4+1], SampRow^[x*4+2], SampRow^[x*4+3])));

          cmyk :=TStdCMYK.New(SampRow^[x*4+0], SampRow^[x*4+1], SampRow^[x*4+2], SampRow^[x*4+3]);
          SetPixel(x, y, cmyk.ToExpandedPixel.ToFPColor(false));
        end;
      else
        for x:=0 to FInfo.output_width-1 do begin
          Color.Red:=SampRow^[x*3+0] shl 8;
          Color.Green:=SampRow^[x*3+1] shl 8;
          Color.Blue:=SampRow^[x*3+2] shl 8;
          SetPixel(x, y, Color);
        end;
      end;

      inc(y);
    end;
  end;
begin
  InitReadingPixels;

  Continue:=true;
  Progress(psStarting, 0, False, Rect(0,0,0,0), '', Continue);
  if not Continue then exit;

  jpeg_start_decompress(@FInfo);

  Img.SetSize(FWidth,FHeight);

  GetMem(SampArray,SizeOf(JSAMPROW));
  GetMem(SampRow,FInfo.output_width*FInfo.output_components);
  SampArray^[0]:=SampRow;
  try
    case FProgressiveEncoding of
      false:
        begin
          ReturnValue:=true;
          OutputScanLines();
          if FInfo.buffered_image then jpeg_finish_output(@FInfo);
        end;
      true:
        begin
          while true do begin
            (* The RestartLoop variable drops a placeholder for suspension
               mode, or partial jpeg decode, return and continue. In case
               of support this suspension, the RestartLoop:=True should be
               changed by an Exit and in the routine enter detects that it
               is being called from a suspended state to not
               reinitialize some buffer *)
            RestartLoop:=false;
            repeat
              status := jpeg_consume_input(@FInfo);
            until (status=JPEG_SUSPENDED) or (status=JPEG_REACHED_EOI);
            ReturnValue:=true;
            if FInfo.output_scanline = 0 then begin
              Scan := FInfo.input_scan_number;
              (* if we haven't displayed anything yet (output_scan_number==0)
                and we have enough data for a complete scan, force output
                of the last full scan *)
              if (FInfo.output_scan_number = 0) and (Scan > 1) and
                (status <> JPEG_REACHED_EOI) then Dec(Scan);

              if not jpeg_start_output(@FInfo, Scan) then begin
                RestartLoop:=true; (* I/O suspension *)
              end;
            end;

            if not RestartLoop then begin
              if (FInfo.output_scanline = $ffffff) then
                FInfo.output_scanline := 0;

              OutputScanLines();

              if ReturnValue=false then begin
                if (FInfo.output_scanline = 0) then begin
                   (* didn't manage to read any lines - flag so we don't call
                      jpeg_start_output() multiple times for the same scan *)
                   FInfo.output_scanline := $ffffff;
                end;
                RestartLoop:=true; (* I/O suspension *)
              end;

              if not RestartLoop then begin
                if (FInfo.output_scanline = FInfo.output_height) then begin
                  if not jpeg_finish_output(@FInfo) then begin
                    RestartLoop:=true; (* I/O suspension *)
                  end;

                  if not RestartLoop then begin
                    if (jpeg_input_complete(@FInfo) and
                       (FInfo.input_scan_number = FInfo.output_scan_number)) then
                      break;

                    FInfo.output_scanline := 0;
                  end;
                end;
              end;
            end;
            if RestartLoop then begin
              (* Suspension mode, but as not supported by this implementation
                 it will simple break the loop to avoid endless looping. *)
              break;
            end;
          end;
        end;
    end;
  finally
    FreeMem(SampRow);
    FreeMem(SampArray);
  end;

  jpeg_finish_decompress(@FInfo);

  Progress(psEnding, 100, false, Rect(0,0,0,0), '', Continue);
end;



procedure TFPReaderJPEG.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  MemStream: TMemoryStream;

begin
  FWidth:=0;
  FHeight:=0;
  MemStream:=nil;
  FillChar(FInfo,SizeOf(FInfo),0);
  try
    if Str is TMemoryStream then
      MemStream:=TMemoryStream(Str)
    else begin
      MemStream:=TMemoryStream.Create;
      ReadCompleteStreamToStream(Str,MemStream,1024);
      MemStream.Position:=0;
    end;
    if MemStream.Size > 0 then begin
      FError:=jpeg_std_error;
      FInfo.err := @FError;
      jpeg_CreateDecompress(@FInfo, JPEG_LIB_VERSION, SizeOf(FInfo));
      try
        FProgressMgr.pub.progress_monitor := @ProgressCallback;
        FProgressMgr.instance := Self;
        FInfo.progress := @FProgressMgr.pub;

        MemStream.Position:=0;
        jpeg_stdio_src(@FInfo, @MemStream);

        ReadHeader(MemStream, Img);
        ReadPixels(MemStream, Img);
      finally
        jpeg_Destroy_Decompress(@FInfo);
      end;
    end;
  finally
    if (MemStream<>nil) and (MemStream<>Str) then
      MemStream.Free;
  end;
end;

class function TFPReaderJPEG.InternalSize(Str: TStream): TPoint;
var
  JInfo: jpeg_decompress_struct;
  JError: jpeg_error_mgr;

begin
  FillChar(JInfo,SizeOf(JInfo),0);
  if Str.Position < Str.Size then begin
    JError:=jpeg_std_error;
    JInfo.err := @JError;
    jpeg_CreateDecompress(@JInfo, JPEG_LIB_VERSION, SizeOf(JInfo));
    try
       jpeg_stdio_src(@JInfo, @Str);

       jpeg_read_header(@JInfo, TRUE);
       Result.X := JInfo.image_width;
       Result.Y := JInfo.image_height;
    finally
      jpeg_Destroy_Decompress(@JInfo);
    end;
  end;
end;

function TFPReaderJPEG.InternalCheck(Str: TStream): boolean;
var
  Buf: array[0..1] of Byte = (0, 0);
  p: Int64;
begin
  if Str=nil then exit(false);
  p:=Str.Position;
  Result := (Str.Read(Buf, 2)=2) and (Buf[0]=$FF) and (Buf[1]=$D8); // byte sequence FFD8 = start of image
  Str.Position:=p;
end;

constructor TFPReaderJPEG.Create;
begin
  FScale:=jsFullSize;
  FPerformance:=jpBestSpeed;
  inherited Create;
end;

destructor TFPReaderJPEG.Destroy;
begin
  inherited Destroy;
end;

initialization
  with jpeg_std_error do begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:=@OutputMessage;
    format_message:=@FormatMessage;
    reset_error_mgr:=@ResetErrorMgr;
  end;
  ImageHandlers.RegisterImageReader ('JPEG Graphics', 'jpg;jpeg', TFPReaderJPEG);
end.
