{$mode objfpc}{$h+}
unit FPimage;

interface

uses sysutils, classes;

type

  TFPCustomImageReader = class;
  TFPCustomImageWriter = class;
  TFPCustomImage = class;

  FPImageException = class (exception);

  TFPColor = record
    red,green,blue,alpha : word;
  end;
  PFPColor = ^TFPColor;

  TColorFormat = (cfMono,cfGray2,cfGray4,cdGray8,cfGray16,cfGray24,
                  cfGrayA8,cfGrayA16,cfGrayA32,
                  cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
                  cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
                  cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48,
                  cfABGR8,cfABGR16,cfABGR32,cfABGR64);
  TColorData = int64;
  PColorData = ^TColorData;

  TDeviceColor = record
    Fmt : TColorFormat;
    Data : TColorData;
  end;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPColorArray = array [0..0] of TFPColor;
{$R-}
{$else not CPU68K}
  TFPColorArray = array [0..(maxint-1) div sizeof(TFPColor)] of TFPColor;
{$endif CPU68K}
  PFPColorArray = ^TFPColorArray;

  TFPImgProgressStage = (psStarting, psRunning, psEnding);
  TFPImgProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean) of object;
  // Delphi compatibility
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  TFPPalette = class
    protected
      FData : PFPColorArray;
      FCount, FCapacity : integer;
      procedure SetCount (Value:integer); virtual;
      function GetCount : integer;
      procedure SetColor (index:integer; const Value:TFPColor); virtual;
      function GetColor (index:integer) : TFPColor;
      procedure CheckIndex (index:integer); virtual;
      procedure EnlargeData; virtual;
    public
      constructor Create (ACount : integer);
      destructor Destroy; override;
      procedure Build (Img : TFPCustomImage); virtual;
      procedure Merge (pal : TFPPalette); virtual;
      function IndexOf (const AColor: TFPColor) : integer; virtual;
      function Add (const Value: TFPColor) : integer; virtual;
      property Color [Index : integer] : TFPColor read GetColor write SetColor; default;
      property Count : integer read GetCount write SetCount;
  end;

  TFPCustomImage = class
    private
      FOnProgress : TFPImgProgressEvent;
      FExtra : TStringlist;
      FPalette : TFPPalette;
      FHeight, FWidth : integer;
      procedure SetHeight (Value : integer);
      procedure SetWidth (Value : integer);
      procedure SetExtra (const key:String; const AValue:string);
      function GetExtra (const key:String) : string;
      procedure SetExtraValue (index:integer; const AValue:string);
      function GetExtraValue (index:integer) : string;
      procedure SetExtraKey (index:integer; const AValue:string);
      function GetExtraKey (index:integer) : string;
      procedure CheckIndex (x,y:integer);
      procedure CheckPaletteIndex (PalIndex:integer);
      procedure SetColor (x,y:integer; const Value:TFPColor);
      function GetColor (x,y:integer) : TFPColor;
      procedure SetPixel (x,y:integer; Value:integer);
      function GetPixel (x,y:integer) : integer;
      function GetUsePalette : boolean;
      procedure SetUsePalette (Value:boolean);
    protected
      // Procedures to store the data. Implemented in descendants
      procedure SetInternalColor (x,y:integer; const Value:TFPColor); virtual;
      function GetInternalColor (x,y:integer) : TFPColor; virtual;
      procedure SetInternalPixel (x,y:integer; Value:integer); virtual; abstract;
      function GetInternalPixel (x,y:integer) : integer; virtual; abstract;
      procedure Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
    public
      constructor create (AWidth,AHeight:integer); virtual;
      destructor destroy; override;
      // Saving and loading
      procedure LoadFromStream (Str:TStream; Handler:TFPCustomImageReader);
      procedure LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
      procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
      // Size and data
      procedure SetSize (AWidth, AHeight : integer); virtual;
      property  Height : integer read FHeight write SetHeight;
      property  Width : integer read FWidth write SetWidth;
      property  Colors [x,y:integer] : TFPColor read GetColor write SetColor; default;
      // Use of palette for colors
      property  UsePalette : boolean read GetUsePalette write SetUsePalette;
      property  Palette : TFPPalette read FPalette;
      property  Pixels [x,y:integer] : integer read GetPixel write SetPixel;
      // Info unrelated with the image representation
      property  Extra [const key:string] : string read GetExtra write SetExtra;
      property  ExtraValue [index:integer] : string read GetExtraValue write SetExtraValue;
      property  ExtraKey [index:integer] : string read GetExtraKey write SetExtraKey;
      procedure RemoveExtra (const key:string);
      function  ExtraCount : integer;
      property OnProgress: TFPImgProgressEvent read FOnProgress write FOnProgress;
  end;
  TFPCustomImageClass = class of TFPCustomImage;

{$ifdef CPU68K}
  { 1.0 m68k cpu compiler does not allow
    types larger than 32k....
    if we remove range checking all should be fine PM }
  TFPIntegerArray = array [0..0] of integer;
{$R-}
{$else not CPU68K}
  TFPIntegerArray = array [0..(maxint-1) div sizeof(integer)] of integer;
{$endif CPU68K}
  PFPIntegerArray = ^TFPIntegerArray;

  TFPMemoryImage = class (TFPCustomImage)
    private
      FData : PFPIntegerArray;
    protected
      procedure SetInternalPixel (x,y:integer; Value:integer); override;
      function GetInternalPixel (x,y:integer) : integer; override;
    public
      constructor create (AWidth,AHeight:integer); override;
      destructor destroy; override;
      procedure SetSize (AWidth, AHeight : integer); override;
  end;

  TFPCustomImageHandler = class
    private
      FOnProgress : TFPImgProgressEvent;
      FStream : TStream;
      FImage : TFPCustomImage;
    protected
      procedure Progress(Stage: TProgressStage; PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
      property TheStream : TStream read FStream;
      property TheImage : TFPCustomImage read FImage;
    public
      constructor Create; virtual;
      Property OnProgress : TFPImgProgressEvent Read FOnProgress Write FOnProgress;
  end;

  TFPCustomImageReader = class (TFPCustomImageHandler)
    private
      FDefImageClass:TFPCustomImageClass;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); virtual; abstract;
      function  InternalCheck (Str:TStream) : boolean; virtual; abstract;
    public
      constructor Create; override;
      function ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
      // reads image
      function CheckContents (Str:TStream) : boolean;
      // Gives True if contents is readable
      property DefaultImageClass : TFPCustomImageClass read FDefImageClass write FDefImageClass;
      // Image Class to create when no img is given for reading
  end;
  TFPCustomImageReaderClass = class of TFPCustomImageReader;

  TFPCustomImageWriter = class (TFPCustomImageHandler)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); virtual; abstract;
    public
      procedure ImageWrite (Str:TStream; Img:TFPCustomImage);
      // writes given image to stream
  end;
  TFPCustomImageWriterClass = class of TFPCustomImageWriter;

  TIHData = class
    private
      FExtention, FTypeName, FDefaultExt : string;
      FReader : TFPCustomImageReaderClass;
      FWriter : TFPCustomImageWriterClass;
  end;

  TImageHandlersManager = class
    private
      FData : TList;
      function GetReader (const TypeName:string) : TFPCustomImageReaderClass;
      function GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
      function GetExt (const TypeName:string) : string;
      function GetDefExt (const TypeName:string) : string;
      function GetTypeName (index:integer) : string;
      function GetData (const ATypeName:string) : TIHData;
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterImageHandlers (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
      procedure RegisterImageReader (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass);
      procedure RegisterImageWriter (const ATypeName,TheExtentions:string;
                   AWriter:TFPCustomImageWriterClass);
      property Count : integer read GetCount;
      property ImageReader [const TypeName:string] : TFPCustomImageReaderClass read GetReader;
      property ImageWriter [const TypeName:string] : TFPCustomImageWriterClass read GetWriter;
      property Extentions [const TypeName:string] : string read GetExt;
      property DefaultExtention [const TypeName:string] : string read GetDefExt;
      property TypeNames [index:integer] : string read GetTypeName;
    end;

function ShiftAndFill (initial:word; CorrectBits:byte):word;
function FillOtherBits (initial:word;CorrectBits:byte):word;
function ConvertColor (const From : TDeviceColor) : TFPColor;
function ConvertColor (const From : TColorData; FromFmt:TColorFormat) : TFPColor;
function ConvertColorToData (const From : TFPColor; Fmt : TColorFormat) : TColorData;
function ConvertColorToData (const From : TDeviceColor; Fmt : TColorFormat) : TColorData;
function ConvertColor (const From : TFPColor; Fmt : TColorFormat) : TDeviceColor;
function ConvertColor (const From : TDeviceColor; Fmt : TColorFormat) : TDeviceColor;

operator = (const c,d:TFPColor) : boolean;
operator or (const c,d:TFPColor) : TFPColor;
operator and (const c,d:TFPColor) : TFPColor; 
operator xor (const c,d:TFPColor) : TFPColor; 
function CompareColors(const Color1, Color2: TFPColor): integer;

var ImageHandlers : TImageHandlersManager;

type
  TErrorTextIndices = (
    StrInvalidIndex,
    StrNoImageToWrite,
    StrNoFile,
    StrNoStream,
    StrPalette,
    StrImageX,
    StrImageY,
    StrImageExtra,
    StrTypeAlreadyExist,
    StrTypeReaderAlreadyExist,
    StrTypeWriterAlreadyExist,
    StrNoPaletteAvailable
    );

const
  // MG: ToDo: move to implementation and add a function to map to resourcestrings
  ErrorText : array[TErrorTextIndices] of string =
    ('Invalid %s index %d',
     'No image to write',
     'File "%s" does not exist',
     'No stream to write to',
     'palette',
     'horizontal pixel',
     'vertical pixel',
     'extra',
     'Image type "%s" already exists',
     'Image type "%s" already has a reader class',
     'Image type "%s" already has a writer class',
     'No palette available'
     );

{$i FPColors.inc}

implementation

procedure FPImgError (Fmt:TErrorTextIndices; data : array of const);
begin
  raise FPImageException.CreateFmt (ErrorText[Fmt],data);
end;

procedure FPImgError (Fmt:TErrorTextIndices);
begin
  raise FPImageException.Create (ErrorText[Fmt]);
end;

{$i FPPalette.inc}
{$i FPHandler.inc}
{$i FPImage.inc}
{$i FPColCnv.inc}

operator = (const c,d:TFPColor) : boolean;
begin
  result := (c.Red = d.Red) and
            (c.Green = d.Green) and
            (c.Blue = d.Blue) and
            (c.Alpha = d.Alpha);
end;

function GetFullColorData (color:TFPColor) : TColorData;
begin
  result := PColorData(@color)^;
end;

function SetFullColorData (color:TColorData) : TFPColor;
begin
  result := PFPColor (@color)^;
end;

operator or (const c,d:TFPColor) : TFPColor; 
begin
  result := SetFullColorData(GetFullColorData(c) OR GetFullColorData(d));
end;

operator and (const c,d:TFPColor) : TFPColor; 
begin
  result := SetFullColorData(GetFullColorData(c) AND GetFullColorData(d));
end;

operator xor (const c,d:TFPColor) : TFPColor; 
begin
  result := SetFullColorData(GetFullColorData(c) XOR GetFullColorData(d));
end;

initialization
  ImageHandlers := TImageHandlersManager.Create;
  ColorBits [cfRGBA64,1] := ColorBits [cfRGBA64,0] shl 32;
  ColorBits [cfRGBA64,2] := ColorBits [cfRGBA64,1] shl 16;
  ColorBits [cfRGB48,1] := ColorBits [cfRGB48,1] shl 16;
  ColorBits [cfABGR64,0] := ColorBits [cfABGR64,0] shl 32;
  ColorBits [cfABGR64,3] := ColorBits [cfABGR64,1] shl 16;
  ColorBits [cfBGR48,3] := ColorBits [cfBGR48,1] shl 16;
finalization
  ImageHandlers.Free;

end.
