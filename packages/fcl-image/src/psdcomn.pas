{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2023 by Massimo Magnano

    PSD reader/writer common code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
unit PSDcomn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, FPimage;

const
  { Image color modes  }
  PSD_BITMAP = 0;       { Bitmap image  }
  PSD_GRAYSCALE = 1;	{ Greyscale image  }
  PSD_INDEXED = 2;	{ Indexed image  }
  PSD_RGB = 3;	        { RGB image  }
  PSD_CMYK = 4;	        { CMYK  }
  PSD_MULTICHANNEL = 7;	{ Multichannel image }
  PSD_DUOTONE = 8;	{ Duotone image }
  PSD_LAB = 9;	        { L*a*b image  }

  { Image color spaces  }
  PSD_CS_RGB = 0;	{ RGB  }
  PSD_CS_HSB = 1;	{ Hue, Saturation, Brightness  }
  PSD_CS_CMYK = 2;	{ CMYK  }
  PSD_CS_PANTONE = 3;	{ Pantone matching system (Lab) }
  PSD_CS_FOCOLTONE = 4;	{ Focoltone color system (CMYK) }
  PSD_CS_TRUMATCH = 5;	{ Trumatch color (CMYK) }
  PSD_CS_TOYO = 6;	{ Toyo 88 colorfinder 1050 (Lab) }
  PSD_CS_LAB = 7;	{ L*a*b }
  PSD_CS_GRAYSCALE = 8;	{ Grey scale  }
  PSD_CS_HKS = 10;	{ HKS colors (CMYK) }
  PSD_CS_DIC = 11;	{ DIC color guide (Lab) }
  PSD_CS_ANPA = 3000;	{ Anpa color (Lab) }

  { Image Resource IDs  }
  PSD_ResourceSectionSignature ='8BIM';

  PSD_PS2_IMAGE_INFO = $03e8;   { Obsolete - ps 2.0 image info  }
  PSD_MAC_PRINT_INFO = $03e9;   { Optional - Mac print manager print info record  }
  PSD_PS2_COLOR_TAB = $03eb;    { Obsolete - ps 2.0 indexed color table  }
  PSD_RESN_INFO = $03ed;        { ResolutionInfo structure  }
  PSD_ALPHA_NAMES = $03ee;      { Alpha channel names  }
  PSD_DISPLAY_INFO = $03ef;     { Superceded by PSD_DISPLAY_INFO_NEW for ps CS3 and higher - DisplayInfo structure  }
  PSD_CAPTION = $03f0;          { Optional - Caption string  }
  PSD_BORDER_INFO = $03f1;      { Border info  }
  PSD_BACKGROUND_COL = $03f2;   { Background color  }
  PSD_PRINT_FLAGS = $03f3;      { Print flags  }
  PSD_GREY_HALFTONE = $03f4;    { Greyscale and multichannel halftoning info  }
  PSD_COLOR_HALFTONE = $03f5;   { Color halftoning info  }
  PSD_DUOTONE_HALFTONE = $03f6; { Duotone halftoning info  }
  PSD_GREY_XFER = $03f7;        { Greyscale and multichannel transfer functions  }
  PSD_COLOR_XFER = $03f8;       { Color transfer functions  }
  PSD_DUOTONE_XFER = $03f9;     { Duotone transfer functions  }
  PSD_DUOTONE_INFO = $03fa;     { Duotone image information  }
  PSD_EFFECTIVE_BW = $03fb;     { Effective black & white values for dot range  }
  PSD_OBSOLETE_01 = $03fc;      { Obsolete  }
  PSD_EPS_OPT = $03fd;          { EPS options  }
  PSD_QUICK_MASK = $03fe;       { Quick mask info  }
  PSD_OBSOLETE_02 = $03ff;      { Obsolete  }
  PSD_LAYER_STATE = $0400;      { Layer state info  }
  PSD_WORKING_PATH = $0401;     { Working path (not saved)  }
  PSD_LAYER_GROUP = $0402;      { Layers group info  }
  PSD_OBSOLETE_03 = $0403;      { Obsolete  }
  PSD_IPTC_NAA_DATA = $0404;    { IPTC-NAA record (IMV4.pdf)  }
  PSD_IMAGE_MODE_RAW = $0405;   { Image mode for raw format files  }
  PSD_JPEG_QUAL = $0406;        { JPEG quality  }
  PSD_GRID_GUIDE = $0408;       { Grid & guide info  }
  PSD_THUMB_RES = $0409;        { Thumbnail resource  }
  PSD_COPYRIGHT_FLG = $040a;    { Copyright flag  }
  PSD_URL = $040b;              { URL string  }
  PSD_THUMB_RES2 = $040c;       { Thumbnail resource  }
  PSD_GLOBAL_ANGLE = $040d;     { Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Global angle  }
  PSD_COLOR_SAMPLER = $040e;    { Superceded by PSD_NEW_COLOR_SAMPLER for ps CS3 and higher - Color samplers resource  }
  PSD_ICC_PROFILE = $040f;      { ICC Profile  }
  PSD_WATERMARK = $0410;        { Watermark  }
  PSD_ICC_UNTAGGED = $0411;     { Do not use ICC profile flag  }
  PSD_EFFECTS_VISIBLE = $0412;  { Show / hide all effects layers  }
  PSD_SPOT_HALFTONE = $0413;    { Spot halftone  }
  PSD_DOC_IDS = $0414;          { Document specific IDs  }
  PSD_ALPHA_NAMES_UNI = $0415;  { Unicode alpha names  }
  PSD_IDX_COL_TAB_CNT = $0416;  { Indexed color table count  }
  PSD_IDX_TRANSPARENT = $0417;  { Index of transparent color (if any)  }
  PSD_GLOBAL_ALT = $0419;       { Global altitude  }
  PSD_SLICES = $041a;           { Slices  }
  PSD_WORKFLOW_URL_UNI = $041b; { Workflow URL - Unicode string  }
  PSD_JUMP_TO_XPEP = $041c;     { Jump to XPEP (?)  }
  PSD_ALPHA_ID = $041d;         { Alpha IDs  }
  PSD_URL_LIST_UNI = $041e;     { URL list - unicode  }
  PSD_VERSION_INFO = $0421;     { Version info  }
  PSD_EXIF_DATA = $0422;        { Exif data block 1  }
  PSD_EXIF_DATA_3 = $0423;      { Exif data block 3 (?)  }
  PSD_XMP_DATA = $0424;         { XMP data block  }
  PSD_CAPTION_DIGEST = $0425;   { Caption digest  }
  PSD_PRINT_SCALE = $0426;      { Print scale  }
  PSD_PIXEL_AR = $0428;         { Pixel aspect ratio  }
  PSD_LAYER_COMPS = $0429;      { Layer comps  }
  PSD_ALT_DUOTONE_COLOR = $042A;{ Alternative Duotone colors  }
  PSD_ALT_SPOT_COLOR = $042B;   { Alternative Spot colors  }
  PSD_LAYER_SELECT_ID = $042D;  { Layer selection ID  }
  PSD_HDR_TONING_INFO = $042E;  { HDR toning information  }
  PSD_PRINT_INFO_SCALE = $042F; { Print scale  }
  PSD_LAYER_GROUP_E_ID = $0430; { Layer group(s) enabled ID  }
  PSD_COLOR_SAMPLER_NEW = $0431;{ Color sampler resource for ps CS3 and higher PSD files  }
  PSD_MEASURE_SCALE = $0432;    { Measurement scale  }
  PSD_TIMELINE_INFO = $0433;    { Timeline information  }
  PSD_SHEET_DISCLOSE = $0434;   { Sheet discloser  }
  PSD_DISPLAY_INFO_NEW = $0435; { DisplayInfo structure for ps CS3 and higher PSD files  }
  PSD_ONION_SKINS = $0436;      { Onion skins  }
  PSD_COUNT_INFO = $0438;       { Count information }
  PSD_PRINT_INFO = $043A;       { Print information added in ps CS5 }
  PSD_PRINT_STYLE = $043B;      { Print style  }
  PSD_MAC_NSPRINTINFO = $043C;  { Mac NSPrintInfo }
  PSD_WIN_DEVMODE = $043D;      { Windows DEVMODE  }
  PSD_AUTO_SAVE_PATH = $043E;   { Auto save file path  }
  PSD_AUTO_SAVE_FORMAT = $043F; { Auto save format  }
  PSD_PATH_INFO_FIRST = $07d0;  { First path info block  }
  PSD_PATH_INFO_LAST = $0bb6;   { Last path info block  }
  PSD_CLIPPING_PATH = $0bb7;    { Name of clipping path  }
  PSD_PLUGIN_R_FIRST = $0FA0;   { First plugin resource  }
  PSD_PLUGIN_R_LAST = $1387;    { Last plugin resource  }
  PSD_IMAGEREADY_VARS = $1B58;  { Imageready variables  }
  PSD_IMAGEREADY_DATA = $1B59;  { Imageready data sets  }
  PSD_LIGHTROOM_WORK = $1F40;   { Lightroom workflow  }
  PSD_PRINT_FLAGS_2 = $2710;    { Print flags  }

  { Display resolution units  }
  PSD_RES_INCH = 1; { Pixels / inch  }
  PSD_RES_CM = 2;   { Pixels / cm  }

  { Width and height units  }
  PSD_UNIT_INCH = 1;  { inches  }
  PSD_UNIT_CM = 2;    { cm  }
  PSD_UNIT_POINT = 3; { points  (72 points =   1 inch)  }
  PSD_UNIT_PICA = 4;  { pica    ( 6 pica   =   1 inch)  }
  PSD_UNIT_COLUMN = 5;{ columns ( column defined in ps prefs, default = 2.5 inches)  }

type
  TRGB = packed record
    Red, Green, Blue : Byte;
  end;

  TLab = record
    L, a, b: byte;
  end;

  { File Header Section }
  TPSDHeader = packed record
    Signature : array[0..3] of Char;   // File IDs '8BPS'
    Version : word;                    // Version number, always 1
    Reserved : array[0..5] of Byte;    // Reserved, must be zeroed
    Channels : Word;                   // Number of color channels (1-24) including alpha channels
    Rows : Cardinal;                   // Height of image in pixels (1-30000)
    Columns : Cardinal;                // Width of image in pixels (1-30000)
    Depth : Word;                      // Number of bits per channel (1, 8, and 16)
    Mode: Word;                        // Color mode (see previous  Image color modes consts)
  end;

  { Image Resource Blocks }
  TPSDResourceBlock = packed record
    Types : array[0..3] of Char;   // Always "8BIM"
    ID:word;                       // see previous Image Resource IDs consts
    NameLen:Byte;                  // Pascal-format string, 2 bytes or longer
    Name:Char;
  end;
  PPSDResourceBlock =^TPSDResourceBlock;

  TPSDResourceBlockData = packed record
    Size:LongWord;
    Data:Byte;
  end;
  PPSDResourceBlockData =^TPSDResourceBlockData;

  //MaxM: Resolution always recorded in a fixed point implied decimal int32
  //      with 16 bits before point and 16 after (cast as DWord and divide resolution by 2^16
  TResolutionInfo = record
    hRes:Cardinal;     // Fixed-point number: pixels per inch  (see note before)
    hResUnit:word;     // 1=pixels per inch, 2=pixels per centimeter
    WidthUnit:word;    // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
    vRes:Cardinal;     // Fixed-point number: pixels per inch  (see note before)
    vResUnit:word;     // 1=pixels per inch, 2=pixels per centimeter
    HeightUnit:word;   // 1=in, 2=cm, 3=pt, 4=picas, 5=columns
  end;

  TDisplayInfo = record
    ColorSpace:word;
    Color:array[0..3] of word;
    Opacity:word;          // 0-100
    Kind:byte;             // 0=selected, 1=protected
    Padding:byte;          // Always zero
  end;


function PSDResolutionUnitToResolutionUnit(APSDResolutionUnit: Word): TResolutionUnit;
function ResolutionUnitToPSdResolutionUnit(AResolutionUnit: TResolutionUnit): Word;

implementation

function PSDResolutionUnitToResolutionUnit(APSDResolutionUnit: Word): TResolutionUnit;
begin
  Case APSDResolutionUnit of
  PSD_RES_INCH: Result :=ruPixelsPerInch;
  PSD_RES_CM: Result :=ruPixelsPerCentimeter;
  else Result :=ruNone;
  end;
end;

function ResolutionUnitToPSdResolutionUnit(AResolutionUnit: TResolutionUnit): Word;
begin
  Case AResolutionUnit of
  ruPixelsPerInch: Result :=PSD_RES_INCH;
  ruPixelsPerCentimeter: Result :=PSD_RES_CM;
  else Result :=0;
  end;
end;

end.

