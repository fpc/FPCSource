{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Yuri Prokushev (prokushev@freemail.ru).

    OS/2 Presentation Manager Device Context constants, types and
    function declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Warning: This code is alfa. Future versions
 of this unit might not be compatible.}
 
unit pmdev;

interface

uses
  os2def;

//General DEV return values
const
  DEV_ERROR                       =0;
  DEV_OK                          =1;

//DC type for DevOpenDC

  OD_QUEUED                       =2;
  OD_DIRECT                       =5;
  OD_INFO                         =6;
  OD_METAFILE                     =7;
  OD_MEMORY                       =8;
  OD_METAFILE_NOQUERY             =9;

//codes for DevQueryCaps
  CAPS_FAMILY                     =0;
  CAPS_IO_CAPS                    =1;
  CAPS_TECHNOLOGY                 =2;
  CAPS_DRIVER_VERSION             =3;
  CAPS_WIDTH                      =4;  //pels            
  CAPS_HEIGHT                     =5;  //pels            
  CAPS_WIDTH_IN_CHARS             =6;
  CAPS_HEIGHT_IN_CHARS            =7;
  CAPS_HORIZONTAL_RESOLUTION      =8;  //pels per meter  
  CAPS_VERTICAL_RESOLUTION        =9;  //pels per meter  
  CAPS_CHAR_WIDTH                 =10; //pels            
  CAPS_CHAR_HEIGHT                =11; //pels            
  CAPS_SMALL_CHAR_WIDTH           =12; //pels            
  CAPS_SMALL_CHAR_HEIGHT          =13; //pels            
  CAPS_COLORS                     =14;
  CAPS_COLOR_PLANES               =15;
  CAPS_COLOR_BITCOUNT             =16;
  CAPS_COLOR_TABLE_SUPPORT        =17;
  CAPS_MOUSE_BUTTONS              =18;
  CAPS_FOREGROUND_MIX_SUPPORT     =19;
  CAPS_BACKGROUND_MIX_SUPPORT     =20;
  CAPS_DEVICE_WINDOWING           =31;
  CAPS_ADDITIONAL_GRAPHICS        =32;
  CAPS_VIO_LOADABLE_FONTS         =21;
  CAPS_WINDOW_BYTE_ALIGNMENT      =22;
  CAPS_BITMAP_FORMATS             =23;
  CAPS_RASTER_CAPS                =24;
  CAPS_MARKER_HEIGHT              =25; //pels            
  CAPS_MARKER_WIDTH               =26; //pels            
  CAPS_DEVICE_FONTS               =27;
  CAPS_GRAPHICS_SUBSET            =28;
  CAPS_GRAPHICS_VERSION           =29;
  CAPS_GRAPHICS_VECTOR_SUBSET     =30;
  CAPS_PHYS_COLORS                =33;
  CAPS_COLOR_INDEX                =34;
  CAPS_GRAPHICS_CHAR_WIDTH        =35;
  CAPS_GRAPHICS_CHAR_HEIGHT       =36;
  CAPS_HORIZONTAL_FONT_RES        =37;
  CAPS_VERTICAL_FONT_RES          =38;
  CAPS_DEVICE_FONT_SIM            =39;
  CAPS_LINEWIDTH_THICK            =40;
  CAPS_DEVICE_POLYSET_POINTS      =41;
  
//Constants for CAPS_IO_CAPS
  CAPS_IO_DUMMY                   =1;
  CAPS_IO_SUPPORTS_OP             =2;
  CAPS_IO_SUPPORTS_IP             =3;
  CAPS_IO_SUPPORTS_IO             =4;
  
//Constants for CAPS_TECHNOLOGY
  CAPS_TECH_UNKNOWN               =0;
  CAPS_TECH_VECTOR_PLOTTER        =1;
  CAPS_TECH_RASTER_DISPLAY        =2;
  CAPS_TECH_RASTER_PRINTER        =3;
  CAPS_TECH_RASTER_CAMERA         =4;
  CAPS_TECH_POSTSCRIPT            =5;
  
//Constants for CAPS_COLOR_TABLE_SUPPORT
  CAPS_COLTABL_RGB_8              =1;
  CAPS_COLTABL_RGB_8_PLUS         =2;
  CAPS_COLTABL_TRUE_MIX           =4;
  CAPS_COLTABL_REALIZE            =8;

//Constants for CAPS_FOREGROUND_MIX_SUPPORT
  CAPS_FM_OR                      =1;
  CAPS_FM_OVERPAINT               =2;
  CAPS_FM_XOR                     =8;
  CAPS_FM_LEAVEALONE             =16;
  CAPS_FM_AND                    =32;
  CAPS_FM_GENERAL_BOOLEAN        =64;

//Constants for CAPS_BACKGROUND_MIX_SUPPORT
  CAPS_BM_OR                      =1;
  CAPS_BM_OVERPAINT               =2;
  CAPS_BM_XOR                     =8;
  CAPS_BM_LEAVEALONE             =16;
  CAPS_BM_AND                    =32;
  CAPS_BM_GENERAL_BOOLEAN        =64;
  CAPS_BM_SRCTRANSPARENT        =128;
  CAPS_BM_DESTTRANSPARENT       =256;

//Constants for CAPS_DEVICE_WINDOWING
  CAPS_DEV_WINDOWING_SUPPORT      =1;

//Constants for CAPS_ADDITIONAL_GRAPHICS
  CAPS_VDD_DDB_TRANSFER            =1;
  CAPS_GRAPHICS_KERNING_SUPPORT    =2;
  CAPS_FONT_OUTLINE_DEFAULT        =4;
  CAPS_FONT_IMAGE_DEFAULT          =8;
//bits represented by values 16L and 32L are reserved
  CAPS_SCALED_DEFAULT_MARKERS     =64;
  CAPS_COLOR_CURSOR_SUPPORT      =128;
  CAPS_PALETTE_MANAGER           =256;
  CAPS_COSMETIC_WIDELINE_SUPPORT =512;
  CAPS_DIRECT_FILL              =1024;
  CAPS_REBUILD_FILLS            =2048;
  CAPS_CLIP_FILLS               =$00001000; //4096L
  CAPS_ENHANCED_FONTMETRICS     =$00002000; //8192L
  CAPS_TRANSFORM_SUPPORT        =$00004000; //16384L

//Constants for CAPS_WINDOW_BYTE_ALIGNMENT
  CAPS_BYTE_ALIGN_REQUIRED        =0;
  CAPS_BYTE_ALIGN_RECOMMENDED     =1;
  CAPS_BYTE_ALIGN_NOT_REQUIRED    =2;

//Constants for CAPS_RASTER_CAPS
  CAPS_RASTER_BITBLT              =1;
  CAPS_RASTER_BANDING             =2;
  CAPS_RASTER_BITBLT_SCALING      =4;
  CAPS_RASTER_SET_PEL            =16;
  CAPS_RASTER_FONTS              =32;
  CAPS_RASTER_FLOOD_FILL         =64;

//structures for DEVESC_QUERYVIOCELLSIZES

type
  PVioSizeCount=^VioSizeCount;
  VioSizeCount=record
    maxcount: Longint;
    count: Longint;
  end;

  PVioFontCellSize=^VioFontCellSize;
  VioFontCellSize=record
    cx: Longint;
    cy: Longint;
  end;

//structure for DEVESC_GETSCALINGFACTOR
  PSFactors=^SFactors;
  SFactors=record
    x: Longint;
    y: Longint;
  end;

//structure for DEVESC_NEXTBAND
  PBandRect=^BandRect;
  BandRect=record
    xleft: Longint;
    ybottom: Longint;
    xright: Longint;
    ytop: Longint;
  end;            

//return codes for DevEscape
const
  DEVESC_ERROR                  =-1;
  DEVESC_NOTIMPLEMENTED         =0;

//codes for DevEscape
  DEVESC_QUERYESCSUPPORT      =   0;
  DEVESC_GETSCALINGFACTOR     =   1;
  DEVESC_QUERYVIOCELLSIZES    =   2;
  DEVESC_GETCP                =8000;

  DEVESC_STARTDOC             =8150;
  DEVESC_ENDDOC               =8151;
  DEVESC_NEXTBAND             =8152;
  DEVESC_ABORTDOC             =8153;

  DEVESC_NEWFRAME            =16300;
  DEVESC_DRAFTMODE           =16301;
  DEVESC_FLUSHOUTPUT         =16302;
  DEVESC_RAWDATA             =16303;
  DEVESC_SETMODE             =16304;

  DEVESC_DBE_FIRST           =24450;
  DEVESC_DBE_LAST            =24455;

//DevEscape codes for adding extra space to character strings
  DEVESC_CHAR_EXTRA          =16998;
  DEVESC_BREAK_EXTRA         =16999;

//codes for DevEscape PM_Q_ESC spool files
  DEVESC_STD_JOURNAL         =32600;

//structure for DEVESC_SETMODE
type
  PEscMode=^EscMode;
  EscMode=record
    mode: cardinal;
    modedata: byte;
  end;

//return codes for DevPostDeviceModes
const
  DPDM_ERROR                   =-1;
  DPDM_NONE                    =0;

//codes for DevPostDeviceModes
  DPDM_POSTJOBPROP             =0;
  DPDM_CHANGEPROP              =1;
  DPDM_QUERYJOBPROP            =2;

//string types for DevQueryDeviceNames
type
  Str8 = Array[0..7] of Char;
  Str16= Array[0..15] of Char;
  Str32= Array[0..31] of Char;
  Str64= Array[0..63] of Char;

//return code for DevQueryHardcopyCaps
const
  DQHC_ERROR                    =-1;

//codes for DevQueryHardcopyCaps
const
  HCAPS_CURRENT                 =1;
  HCAPS_SELECTABLE              =2;

//structure for DevQueryHardcopyCaps
type
  PHCInfo=^HCInfo;
  HCInfo=record
    szFormname: Array[0..31] of Char;
    cx: Longint;
    cy: Longint;
    xLeftClip: Longint;
    yBottomClip: Longint;
    xRightClip: Longint;
    yTopClip: Longint;
    xPels: Longint;
    yPels: Longint;
    flAttributes: Longint;
  end;

function DevOpenDC(ahab: HAB; lType: Longint; pszToken: PChar; lCount: Longint; var pdopData: DevOpenStruc; hdcComp: cardinal): cardinal; cdecl;
function DevCloseDC(ahdc: cardinal): cardinal; cdecl;
function DevEscape(ahdc: cardinal; lCode, lInCount: Longint; var pbInData; var plOutCount: Longint; var pbOutData): Longint; cdecl;
function DevQueryCaps(ahdc: cardinal; lStart, lCount: Longint; var alArray: Longint): Longbool; cdecl;
function DevQueryDeviceNames(ahab: HAB; pszDriverName: PChar; var pldn: Longint; aDeviceName: Str32; aDeviceDesc: Str64; var pldt: Longint; aDataType: Str16): Longbool; cdecl;
function DevQueryHardcopyCaps(ahdc: cardinal; lStartForm, lForms: Longint; var phciHcInfo: HCInfo): Longint; cdecl;
function DevPostDeviceModes(ahab: HAB; var pdrivDriverData: DrivData; pszDriverName, pszDeviceName, pszName: PChar; flOptions: cardinal): Longint; cdecl;
                  
implementation

function DevOpenDC(ahab: HAB; lType: Longint; pszToken: PChar; lCount: Longint; var pdopData: DevOpenStruc; hdcComp: cardinal): cardinal; cdecl;
    external 'PMGPI' index 610;
function DevCloseDC(ahdc: cardinal): cardinal; cdecl;
    external 'PMGPI' index 604;
function DevEscape(ahdc: cardinal; lCode, lInCount: Longint; var pbInData; var plOutCount: Longint; var pbOutData): Longint; cdecl;
    external 'PMGPI' index 605;
function DevQueryCaps(ahdc: cardinal; lStart, lCount: Longint;var alArray: Longint): Longbool; cdecl;
    external 'PMGPI' index 606;
function DevQueryDeviceNames(ahab: HAB; pszDriverName: PChar; var pldn: Longint; aDeviceName: Str32; aDeviceDesc: Str64; var pldt: Longint; aDataType: Str16): Longbool; cdecl;
    external 'PMGPI' index 607;
function DevQueryHardcopyCaps(ahdc: cardinal; lStartForm, lForms: Longint; var phciHcInfo: HCInfo): Longint; cdecl;
    external 'PMGPI' index 608;
function DevPostDeviceModes(ahab: HAB; var pdrivDriverData: DrivData; pszDriverName, pszDeviceName, pszName: PChar; flOptions: cardinal): Longint; cdecl;
    external 'PMGPI' index 609;
                    
end.

{

$Log$
Revision 1.1  2002-10-18 18:08:52  hajny
  + first version


}
