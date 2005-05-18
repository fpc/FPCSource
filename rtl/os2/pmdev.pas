{
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
  Str16= string [15];
  Str32= string [31];
  Str64= string [63];

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
    szFormname: Str32;
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

    { -----------------------------------------------------------------
          Tuple Item used for QUERYSIZE
        }
    { djpQRT  }
    { I - Property                       }
    { I - type (DJP_ALL or DJP_CURRENT)  }
    {                                                                }

    type

       TdjpQueryTuple = record
            ulProperty : Cardinal;
            lType : Longint;
         end;
       TQUERYTUPLE = TdjpQueryTuple;
       TPQUERYTUPLE = ^TdjpQueryTuple;
    { -----------------------------------------------------------------
          Query Size Structure for DEVESC_QUERYSIZE
        }
    { djpQRS  }
    { I - Size of entire structure       }
    { O - Size returned;                 }
    { I - Start of tuple list            }
    {                                 use DJP_NONE for end of list   }

       TdjpQuerySize = record
            cb : Cardinal;
            ulSizeNeeded : Cardinal;
            aTuples : array[0..0] of TQUERYTUPLE;
         end;
       TQUERYSIZE = TdjpQuerySize;
       TPQUERYSIZE = ^TdjpQuerySize;
    { was #define dname def_expr }
//    function QUERYSIZE_HEADER_SIZE : longint;
        { return type might be wrong }

    { -----------------------------------------------------------------
          Dynamic Job Property Item
        }
    { djpITM  }
    { I/O - sizeof DJP_ITEM structure    }
    { I   - Which property               }
    { I/O - DJP_ALL or DJP_CURRENT.      }
    {                                   DJP_ERROR_XXX if error.      }
    { O   - How many elements have been  }
    {                                   returned                     }
    { O   - Variably sized based on      }
    {                                   ulProperty.  The smallest    }
    {                                   is a ULONG in size           }

    type

       TdjpItem = record
            cb : Cardinal;
            ulProperty : Cardinal;
            lType : Longint;
            ulNumReturned : Cardinal;
            ulValue : Cardinal;
         end;
       TDJP_ITEM = TdjpItem;
       TPDJP_ITEM = ^TdjpItem;
    { was #define dname def_expr }
//    function DJP_HEADER_SIZE : longint;
        { return type might be wrong }

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
//    function DJP_NEXT_STRUCTP(p : longint) : TPDJP_ITEM;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
//    function DJP_ELEMENTP(s,t : longint) : ^Tt;

(* error
   #define DJP_SET_ELEMENT(s,t,e) ( *DJP_ELEMENTP (s,t) = (e))
in define line 83 *)
    { -----------------------------------------------------------------
          Types of Dynamic Job Properties

          To see if the driver is enabled use,
              DevEscape      (DEVESC_QUERYESCSUPPORT,
                              DEVESC_STARTDOC_WPROP, ...)
          and DevQueryDevice (DEVQRY_QUERYSUPPORT,
                              DEVESC_QUERYJOBPROPERTIES, ...)

          NOTE: The C/S in the defines indicate the complexity or size of
                the information.  If it is s, then the size is ULONG sized
                and no special processing for the next element needs to be
                done.
                The J/P in the defines indicate the class.  J stands for job
                properties and P stands for printer properties.
        }
    { also End Of List marker  }

    const
       DJP_NONE = 0;
       DJP_SJ_ORIENTATION = 1;
       DJP_CJ_RESOLUTION = 2;
       DJP_SJ_BITSPERPEL = 3;
       DJP_SJ_COLOR = 4;
       DJP_SJ_PRINTQUALITY = 5;
       DJP_SJ_PAPERSIZE = 6;
       DJP_SJ_TRAYTYPE = 7;
       DJP_SJ_MEDIA = 8;
       DJP_SJ_MEDIA_COLOR = 9;
       DJP_CJ_FORM = 10;
       DJP_CJ_MIXEDFORMS = 11;
       DJP_SJ_FONTDOWNLOADING = 12;
       DJP_SJ_DUPLEX = 13;
       DJP_SJ_COLLATE = 14;
       DJP_SJ_FEED = 15;
       DJP_SJ_COPIES = 16;
       DJP_SJ_SCALING = 17;
       DJP_SJ_FORMFEEDCONTROL = 18;
       DJP_SJ_N_UP = 19;
       DJP_CJ_OUTPUTBIN = 20;
       DJP_CJ_TRAYNAME = 21;
    { Types for DEVESC_QUERYJOBPROPERTIES / DEVESC_SETJOBPROPERTIES
        }
    { enumerate the property  }
       DJP_ALL = 1;
    { from job properties     }
       DJP_CURRENT = 2;
    { Errors for DEVESC_QUERYJOBPROPERTIES / DEVESC_SETJOBPROPERTIES
        }
    { driver doesnt support that property  }
       DJP_ERROR_NOT_SUPPORTED = -(1);
    { not in the valid range               }
       DJP_ERROR_OUT_OF_RANGE = -(2);
    { not enumerateable                    }
       DJP_ERROR_NOT_ENUM = -(3);
    { field not proper value               }
       DJP_ERROR_INV_PARMS = -(4);
    { -----------------------------------------------------------------
          DJP_SJ_ORIENTATION
        }
       DJP_ORI_PORTRAIT = 1;
       DJP_ORI_LANDSCAPE = 2;
       DJP_ORI_REV_PORTRAIT = 3;
       DJP_ORI_REV_LANDSCAPE = 4;

    type

       TDJPT_ORIENTATION = Cardinal;

       TPDJPT_ORIENTATION = Cardinal;
    { -----------------------------------------------------------------
          DJP_CJ_RESOLUTION
        }
    { djpRES  }
    { X resolution (in dots per inch)  }
    { Y resolution (in dots per inch)  }

       TdjpResolution = record
            usXResolution : Word;
            usYResolution : Word;
         end;
       TDJPT_RESOLUTION = TdjpResolution;
       TPDJPT_RESOLUTION = ^TdjpResolution;
    { -----------------------------------------------------------------
          DJP_SJ_BITSPERPEL
        }

       TDJPT_BITSPERPEL = Cardinal;

       TPDJPT_BITSPERPEL = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_COLOR
        }

    const
       DJP_CLR_MONOCHROME = 1;
       DJP_CLR_COLOR = 2;

    type

       TDJPT_COLOR = Cardinal;

       TPDJPT_COLOR = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_PRINTQUALITY

          Note: DJP_PQL_DRAFT is the worst quality.  In the future, there
                may be better qualities (such as DJP_PQL_ULTRA_HIGH) which
                will be numerically greater than DJP_PQL_HIGH.
        }

    const
       DJP_PQL_DRAFT = 1;
       DJP_PQL_LOW = 2;
       DJP_PQL_MEDIUM = 3;
       DJP_PQL_HIGH = 4;
       DJP_PQL_LAST = DJP_PQL_HIGH;

    type

       TDJPT_PRINTQUALITY = Cardinal;

       TPDJPT_PRINTQUALITY = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_PAPERSIZE

          Note: it is recommended to use DJP_CJ_FORM to chage the papersize.
                                                        approximate size
        }
    {  inches       millimeters  }

    const
       DJP_PSI_NONE = 0;
    {  8.5  x 11    216 x 279    }
       DJP_PSI_LETTER = 1;
    {  8.5  x 14    216 x 356    }
       DJP_PSI_LEGAL = 2;
    { 13.58 x 11    345 x 279    }
       DJP_PSI_WIDE = 3;
    { 17    x 22    431 x 558    }
       DJP_PSI_CSHEET = 4;
    { 22    x 34    558 x 863    }
       DJP_PSI_DSHEET = 5;
    { 34    x 44    863 x 1117   }
       DJP_PSI_ESHEET = 6;
    {                            }
       DJP_PSI_LETTERSMALL = 7;
    { 11    x 17    279 x 431    }
       DJP_PSI_TABLOID = 8;
    { 17    x 11    431 x 279    }
       DJP_PSI_LEDGER = 9;
    {  5.5  x  8.5  139 x 216    }
       DJP_PSI_STATEMENT = 10;
    {  7.25 x 10.5  184 x 266    }
       DJP_PSI_EXECUTIVE = 11;
    { 33.11 x 46.81 841 x 1189   }
       DJP_PSI_A0 = 12;
    { 23.39 x 33.11 594 x 841    }
       DJP_PSI_A1 = 13;
    { 16.54 x 23.39 420 x 594    }
       DJP_PSI_A2 = 14;
    { 11.7  x 16.54 297 x 420    }
       DJP_PSI_A3 = 15;
    {  8.3  x 11.7  210 x 297    }
       DJP_PSI_A4 = 16;
    {                            }
       DJP_PSI_A4_SMALL = 17;
    {  5.83 x  8.27 148 x 210    }
       DJP_PSI_A5 = 18;
    {  9.84 x 13.94 250 x 354    }
       DJP_PSI_B4 = 19;
    {  7.17 x 10.12 182 x 257    }
       DJP_PSI_B5 = 20;
    {  8.5  x 13    216 x 330    }
       DJP_PSI_FOLIO = 21;
    {  8.46 x 10.83 215 x 275    }
       DJP_PSI_QUATRO = 22;
    { 10    x 14    254 x 355    }
       DJP_PSI_10X14 = 23;
    { 11    x 17    279 x 431    }
       DJP_PSI_11X17 = 24;
    {                            }
       DJP_PSI_NOTE = 25;
    {                            }
       DJP_PSI_ENV_9 = 26;
    {                            }
       DJP_PSI_ENV_10 = 27;
    {                            }
       DJP_PSI_ENV_11 = 28;
    {                            }
       DJP_PSI_ENV_12 = 29;
    {                            }
       DJP_PSI_ENV_14 = 30;
    {                            }
       DJP_PSI_ENV_DL = 31;
    {                            }
       DJP_PSI_ENV_A2 = 32;
    {                            }
       DJP_PSI_ENV_C3 = 33;
    {                            }
       DJP_PSI_ENV_C4 = 34;
    {                            }
       DJP_PSI_ENV_C5 = 35;
    {                            }
       DJP_PSI_ENV_C6 = 36;
    {                            }
       DJP_PSI_ENV_C65 = 37;
    {                            }
       DJP_PSI_ENV_C9 = 38;
    {                            }
       DJP_PSI_ENV_C10 = 39;
    {                            }
       DJP_PSI_ENV_B4 = 40;
    {                            }
       DJP_PSI_ENV_B5 = 41;
    {                            }
       DJP_PSI_ENV_B6 = 42;
    {                            }
       DJP_PSI_ENV_ITALY = 43;
    {                            }
       DJP_PSI_ENV_MONARCH = 44;
    {                            }
       DJP_PSI_ENV_PERSONAL = 45;
    {                            }
       DJP_PSI_FANFOLD_US = 46;
    {                            }
       DJP_PSI_FANFOLD_STD_GERMAN = 47;
    {                            }
       DJP_PSI_FANFOLD_LGL_GERMAN = 48;
    {                            }
       DJP_PSI_ARCHITECT_BSHEET = 49;
    {                            }
       DJP_PSI_ARCHITECT_CSHEET = 50;
    {                            }
       DJP_PSI_ARCHITECT_DSHEET = 51;
    {                            }
       DJP_PSI_ARCHITECT_ESHEET = 52;
    {                            }
       DJP_PSI_CARD_A6 = 53;
    {                            }
       DJP_PSI_CARD_4X6 = 54;
    {                            }
       DJP_PSI_CARD_5X8 = 55;
    {                            }
       DJP_PSI_CARD_HAGAKI = 56;
    {  1.10 x 3.50   28 x 89     }
       DJP_PSI_LABEL_STANDARD = 57;
    {  3.98 x 2.13  101 x 54     }
       DJP_PSI_LABEL_SHIPPING = 58;
    {  2.76 x 2.13   70 x 54     }
       DJP_PSI_LABEL_DISK = 59;
    {  3.50 x 1.42   89 x 36     }
       DJP_PSI_LABEL_EURO = 60;
    {                            }
       DJP_PSI_CARD_OUFUKU_HAGAKI = 61;
    {                            }
       DJP_PSI_B0 = 62;
    {                            }
       DJP_PSI_B1 = 63;
    {                            }
       DJP_PSI_B2 = 64;
    {                            }
       DJP_PSI_B3 = 65;
    {                            }
       DJP_PSI_B6 = 66;
    {                            }
       DJP_PSI_B7 = 67;
    {                            }
       DJP_PSI_B8 = 68;
    {                            }
       DJP_PSI_B9 = 69;
    {                            }
       DJP_PSI_B10 = 70;
    {                            }
       DJP_PSI_B0_JIS = 71;
    {                            }
       DJP_PSI_B1_JIS = 72;
    {                            }
       DJP_PSI_B2_JIS = 73;
    {                            }
       DJP_PSI_B3_JIS = 74;
    {                            }
       DJP_PSI_B4_JIS = 75;
    {                            }
       DJP_PSI_B5_JIS = 76;
    {                            }
       DJP_PSI_B6_JIS = 77;
    {                            }
       DJP_PSI_B7_JIS = 78;
    {                            }
       DJP_PSI_B8_JIS = 79;
    {                            }
       DJP_PSI_B9_JIS = 80;
    {                            }
       DJP_PSI_B10_JIS = 81;
       DJP_PSI_LAST = DJP_PSI_B10_JIS;

    type

       TDJPT_PAPERSIZE = Longint;

       TPDJPT_PAPERSIZE = Longint;
    { -----------------------------------------------------------------
          DJP_SJ_TRAYTYPE

          Note: it is recommended to use DJP_CJ_FORM to chage the tray type.
        }

    const
       DJP_TRY_NONE = 0;
       DJP_TRY_UPPER = 1;
       DJP_TRY_ONLYONE = DJP_TRY_UPPER;
       DJP_TRY_LOWER = 2;
       DJP_TRY_MIDDLE = 3;
       DJP_TRY_MANUAL = 4;
       DJP_TRY_ENVELOPE = 5;
       DJP_TRY_ENVMANUAL = 6;
       DJP_TRY_AUTO = 7;
       DJP_TRY_TRACTOR = 8;
       DJP_TRY_SMALLFMT = 9;
       DJP_TRY_LARGEFMT = 10;
       DJP_TRY_LARGECAPACITY = 11;
       DJP_TRY_CASSETTE = 12;
       DJP_TRY_LAST = DJP_TRY_CASSETTE;

    type

       TDJPT_TRAYTYPE = Cardinal;

       TPDJPT_TRAYTYPE = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_MEDIA

          Note: it is recommended to use DJP_CJ_FORM to chage the media type.
        }

    const
       DJP_MED_NONE = 0;
       DJP_MED_PLAIN = 1;
       DJP_MED_TRANSPARENCY = 2;
       DJP_MED_GLOSSY = 3;
       DJP_MED_SPECIAL = 4;
       DJP_MED_COATED = 5;
       DJP_MED_BACKPRINT = 6;
       DJP_MED_CLOTH = 7;
       DJP_MED_THICK = 8;
       DJP_MED_STATIONARY = 9;
       DJP_MED_ENVELOPE = 10;
       DJP_MED_CONTINUOUS_LONG = 11;
       DJP_MED_CONTINUOUS_SHORT = 12;
       DJP_MED_TAB_STOCK = 13;
       DJP_MED_MULTI_PART_FORM = 14;
       DJP_MED_LABELS = 15;
       DJP_MED_LAST = DJP_MED_LABELS;

    type

       TDJPT_MEDIA = Cardinal;

       TPDJPT_MEDIA = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_MEDIA_COLOR

          Select the media color (for the same media types).
        }

    const
       DJP_MDC_BLUE = 1;
       DJP_MDC_BLUFF = 2;
       DJP_MDC_GOLDENROD = 3;
       DJP_MDC_GREEN = 4;
       DJP_MDC_PINK = 5;
       DJP_MDC_TRANSPARENT = 6;
       DJP_MDC_WHITE = 7;
       DJP_MDC_YELLOW = 8;
       DJP_MDC_LAST = DJP_MDC_YELLOW;

    type

       TDJPT_MEDIA_COLOR = Cardinal;

       TPDJPT_MEDIA_COLOR = Cardinal;
    { -----------------------------------------------------------------
          DJP_CJ_FORM

          Setting will match all three fields.  If szTrayname or szMedianame
             is null then it will be defaulted to the first one found.

          Querying will return all fields filled in.
        }
    { djpFRM  }
    { System Form name                   }
    { System Tray name                   }
    { System Media name                  }
    {                      v-= Informational only =-v                           }
    { Corresponding hard copy info       }
    { Display Form name  (translated)    }
    { Display Tray name  (translated)    }
    { Display Media name (translated)    }
    { Simple form id  (if not DJP_NONE)  }
    { Simple tray id  (if not DJP_NONE)  }
    { Simple media id (if not DJP_NONE)  }

       TdjpForm = record
            szFormname : Str32;
            szTrayname : Str32;
            szMedianame : Str32;
            ahcInfo : HCINFO;
            szDisplayFormname : Str64;
            szDisplayTrayname : Str64;
            szDisplayMedianame : Str64;
            djppsFormID : TDJPT_PAPERSIZE;
            djpttTrayID : TDJPT_TRAYTYPE;
            djpmdMediaID : TDJPT_MEDIA;
         end;
       TDJPT_FORM = TdjpForm;
       TPDJPT_FORM = ^TdjpForm;
    { -----------------------------------------------------------------
          DJP_CJ_MIXEDFORMS

          This is unique in that both setting and querying can have multiple
          elements.  Both the first page and the last page are DJP_MXF_INFINITY
          (which is the separator for individual elements).
          Some examples are:

          - Only one form (form1) for the entire job.
            (DJP_MXF_INFINITY, DJP_MXF_INFINITY, form1)

          - Page 1 has form1, then pages 2 ... n have form2.
            (DJP_MXF_INFINITY, 1, form1) (2, DJP_MXF_INFINITY, form2)

          - Even pages have form1 and odd pages have form2
            (DJP_MXF_INFINITY, DJP_MXF_ODD, form1) (DJP_MXF_EVEN, DJP_MXF_INFINITY, form2)
            or (DJP_MXF_INFINITY, DJP_MXF_EVEN, form2) (DJP_MXF_ODD, DJP_MXF_INFINITY, form1)

          - First page has form1, even pages have form2 and odd pages have form3
            (DJP_MXF_INFINITY, 1, form1) (DJP_MXF_ODD, DJP_MXF_ODD, form1) (DJP_MXF_EVEN, DJP_MXF_INFINITY, form2)
            or (DJP_MXF_INFINITY, 1, form1) (DJP_MXF_EVEN, DJP_MXF_EVEN, form2) (DJP_MXF_ODD, DJP_MXF_INFINITY, form1)

        }

    const
       DJP_MXF_INFINITY = -(1);
       DJP_MXF_ODD = -(2);
       DJP_MXF_EVEN = -(3);
    { djpMXF  }
    { Starting page number            }
    { Ending page number              }
    { Form associated with the range  }

    type

       TdjpMixedForms = record
            lStartRange : Longint;
            lEndRange : Longint;
            djpfmForm : TDJPT_FORM;
         end;
       TDJPT_MIXEDFORMS = TdjpMixedForms;
       TPDJPT_MIXEDFORMS = ^TdjpMixedForms;
    { -----------------------------------------------------------------
          DJP_SJ_FONTDOWNLOADING
        }
    { Device does not support downloading }

    const
       DJP_FDL_NONE = 0;
    { Download fonts to printer           }
       DJP_FDL_DOWNLOAD = 1;
    { Rasterize fonts                     }
       DJP_FDL_BITMAP = 2;
    { Substitute device fonts for system  }
       DJP_FDL_SUBSTITUTE = 3;

    type

       TDJPT_FONTDOWNLOADING = Cardinal;

       TPDJPT_FONTDOWNLOADING = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_DUPLEX
        }
    { Device does not support duplex      }

    const
       DJP_DUP_NONE = 0;
    { Duplex is turned off                }
       DJP_DUP_OFF = 1;
       DJP_DUP_BOOK = 2;
       DJP_DUP_FLIP = 3;

    type

       TDJPT_DUPLEX = Cardinal;

       TPDJPT_DUPLEX = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_COLLATE
        }
    { Device does not support collation   }

    const
       DJP_COL_NONE = 0;
       DJP_COL_OFF = 1;
       DJP_COL_ON = 2;
    { more for printer dialogs than       }
       DJP_COL_PRINTER_SETTING = 3;
    { programmatic control.  Use          }
    { the setting on the printer panel.   }

    type

       TDJPT_COLLATE = Cardinal;

       TPDJPT_COLLATE = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_FEED
        }

    const
       DJP_FED_MANUAL = 1;
       DJP_FED_AUTOMATIC = 2;

    type

       TDJPT_FEED = Cardinal;

       TPDJPT_FEED = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_COPIES

          This is the number of copies on a per page basis.  This is not
          enumerateable.
        }

       TDJPT_COPIES = Cardinal;

       TPDJPT_COPIES = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_SCALING

          This is a percentage value.  This is not enumerateable.
        }

       TDJPT_SCALING = Longint;

       TPDJPT_SCALING = Longint;
    { -----------------------------------------------------------------
          DJP_SJ_FORMFEEDCONTROL

          This is a property that effects raw data jobs (print from the
          command line, DOS print jobs, Windows print jobs). This checks
          the very last byte of the data stream to see if it is a form
          feed control character.
        }
    { Never add         }

    const
       DJP_FFC_NONE = 1;
    { Add if not seen   }
       DJP_FFC_CONDITIONAL = 2;
    { Always add        }
       DJP_FFC_COMPULSORY = 3;

    type

       TDJPT_FORMFEEDCONTROL = Cardinal;

       TPDJPT_FORMFEEDCONTROL = Cardinal;
    { -----------------------------------------------------------------
          DJP_SJ_N_UP

          Number of logical pages per physical page (ex: 2-up, 4-up)
        }

       TDJPT_NUP = Longint;

       TPDJPT_NUP = Longint;
    { -----------------------------------------------------------------
          DJP_CJ_OUTPUTBIN

          Setting will only use szBinname.

          Querying will return all fields filled in.
        }
    { djpOBN  }
    { System Bin name                    }
    {                      v-= Informational only =-v                           }
    { Display Bin name  (translated)     }
    { Bin id # (-1 for no id)            }

       TdjpOutputBin = record
            szBinname : Str32;
            szDisplayBinname : Str64;
            lBinId : Longint;
         end;
       TDJPT_OUTPUTBIN = TdjpOutputBin;
       TPDJPT_OUTPUTBIN = ^TdjpOutputBin;
    { -----------------------------------------------------------------
          DJP_CJ_TRAYNAME

          Setting will match only szTrayname.  The perfered way to set which
          tray to use is DJP_CJ_FORM.  Otherwise, you are not guaranteed a
          unique match for all three form, tray, and media possibilities.

          Querying will return all fields filled in.
        }
    { djpTry  }
    { System Tray name                   }
    {                      v-= Informational only =-v                           }
    { Display Tray name  (translated)    }
    { Simple tray id  (if not DJP_NONE)  }

       TdjpInputTray = record
            szTrayname : Str32;
            szDisplayTrayname : Str64;
            djpttTrayID : TDJPT_TRAYTYPE;
         end;
       TDJPT_TRAYNAME = TdjpInputTray;
       TPDJPT_TRAYNAME = ^TdjpInputTray;


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

    { was
   #define QUERYSIZE_HEADER_SIZE  (sizeof (QUERYSIZE) - sizeof (((PQUERYSIZE)NULL)->aTuples))
    }
//    function QUERYSIZE_HEADER_SIZE : longint;
//        { return type might be wrong }
//        begin
//           QUERYSIZE_HEADER_SIZE:=(sizeof(QUERYSIZE)) - (sizeof((TPQUERYSIZE(NULL))^.aTuples));
//        end;

//   #define DJP_HEADER_SIZE        (sizeof (DJP_ITEM) - sizeof (((PDJP_ITEM)NULL)->ulValue))
//   #define DJP_NEXT_STRUCTP(p)    ((PDJP_ITEM)((PBYTE)(p) + (p)->cb))
//   #define DJP_ELEMENTP(s,t)      ((t*)&((s).ulValue))
//   #define DJP_SET_ELEMENT(s,t,e) (*DJP_ELEMENTP (s,t) = (e))

    { was #define dname def_expr }
//    function DJP_HEADER_SIZE : longint;
//        { return type might be wrong }
//        begin
//           DJP_HEADER_SIZE:=(sizeof(DJP_ITEM)) - (sizeof((TPDJP_ITEM(NULL))^.ulValue));
//        end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
//    function DJP_NEXT_STRUCTP(p : longint) : TPDJP_ITEM;
//      begin
//         DJP_NEXT_STRUCTP:=TPDJP_ITEM((TPBYTE(p)) + (p^.cb));
//      end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
//    function DJP_ELEMENTP(s,t : longint) : ^Tt;
//      begin
//         DJP_ELEMENTP:=^Tt(@(s.ulValue));
//      end;

end.
