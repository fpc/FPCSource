{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Free Pascal development team

    datatypes.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit datatypes;

interface

uses
  Exec, AmigaDOS, Utility, AGraphics, Intuition, IFFParse;

const
  DATATYPESNAME = 'datatypes.library';

// datatypes
const
  ID_DTYP = Ord('D') shl 24 + Ord('T') shl 16 + Ord('Y') shl 8 + Ord('P'); // DTYP
  ID_DTHD = Ord('D') shl 24 + Ord('T') shl 16 + Ord('H') shl 8 + Ord('D'); // DTHD

type
  PDataTypeHeader = ^TDataTypeHeader;
  TDataTypeHeader = record
    dth_Name: STRPTR;       // Name of the data type
    dth_BaseName: STRPTR;   // Base name of the data type
    dth_Pattern: STRPTR;    // File name match pattern
    dth_Mask: PSmallInt;    // Comparison mask (binary)
    dth_GroupID: LongWord;  // DataType Group GID_*
    dth_ID: LongWord;       // DataType ID (same as IFF FORM type) ID_*
    dth_MaskLen: SmallInt;  // Length of the comparison mask
    dth_Pad: SmallInt;      // Unused at present (must be 0)
    dth_Flags: Word;        // Flags DTF_*
    dth_Priority: Word;
 end;

const
  DTHSIZE = SizeOf(TDataTypeHeader);

  // TDataTypeHeader.dth_Flags
  DTF_TYPE_MASK = $000F;
  DTF_BINARY    = $0000;
  DTF_ASCII     = $0001;
  DTF_IFF       = $0002;
  DTF_MISC      = $0003;
  DTF_CASE      = $0010; // Case is important
  DTF_SYSTEM1   = $1000; // For system use only

// TDataTypeHeader.dth_GroupID and TDataTypeHeader.dth_ID

  // System file -- executable, directory, library, font and so on.
  GID_SYSTEM    = Ord('s') shl 24 + Ord('y') shl 16 + Ord('s') shl 8 + Ord('t'); // syst
  ID_BINARY     = Ord('b') shl 24 + Ord('i') shl 16 + Ord('n') shl 8 + Ord('a'); // bina
  ID_EXECUTABLE = Ord('e') shl 24 + Ord('x') shl 16 + Ord('e') shl 8 + Ord('c'); // exec
  ID_DIRECTORY  = Ord('d') shl 24 + Ord('i') shl 16 + Ord('r') shl 8 + Ord('e'); // dire
  ID_IFF        = Ord('i') shl 24 + Ord('f') shl 16 + Ord('f') shl 8 + Ord(#0);  // iff

  // Text, formatted or not
  GID_TEXT = Ord('t') shl 24 + Ord('e') shl 16 + Ord('x') shl 8 + Ord('t'); // text
  ID_ASCII = Ord('a') shl 24 + Ord('s') shl 16 + Ord('c') shl 8 + Ord('i'); // asci

  // Formatted text combined with graphics or other DataTypes
  GID_DOCUMENT = Ord('d') shl 24 + Ord('o') shl 16 + Ord('c') shl 8 + Ord('u'); // docu

  // Sound
  GID_SOUND = Ord('s') shl 24 + Ord('o') shl 16 + Ord('u') shl 8 + Ord('n'); // soun

  // Musical instrument
  GID_INSTRUMENT = Ord('i') shl 24 + Ord('n') shl 16 + Ord('s') shl 8 + Ord('t'); // inst

  // Musical score
  GID_MUSIC = Ord('m') shl 24 + Ord('u') shl 16 + Ord('s') shl 8 + Ord('i'); // musi

  // Picture
  GID_PICTURE = Ord('p') shl 24 + Ord('i') shl 16 + Ord('c') shl 8 + Ord('t'); // pict

  //* Animated pictures */
  GID_ANIMATION = Ord('a') shl 24 + Ord('n') shl 16 + Ord('i') shl 8 + Ord('m'); // anim

  //* Animation with audio */
  GID_MOVIE = Ord('m') shl 24 + Ord('o') shl 16 + Ord('v') shl 8 + Ord('i'); // movi

  ID_CODE = Ord('D') shl 24 + Ord('T')  shl 16 + Ord('C') shl 8 + Ord('D'); // DTCD

type
  PTHookContext = ^TDTHookContext;
  TDTHookContext = record
    dthc_SysBase: PLibrary;
    dthc_DOSBase: PLibrary;
    dthc_IFFParseBase: PLibrary;
    dthc_UtilityBase: PLibrary;
    // File context
    dthc_Lock: BPTR;
    dthc_FIB: PFileInfoBlock;
    dthc_FileHandle: BPTR;       // Pointer to file handle (may be nil)
    dthc_IFF: PIFFHandle;        // Pointer to IFFHandle (may be nil)
    dthc_Buffer: STRPTR;         // Buffer...
    dthc_BufferLength: LongWord; // ... and corresponding length
 end;
const
  ID_TOOL = Ord('D') shl 24 + Ord('T') shl 16 + Ord('T') shl 8 + Ord('L'); // DTTL

type
  PTool = ^TTool;
  TTool = record
    tn_Which: Word;     // TW_*
    tn_Flags: Word;     // Flags TF_*
    tn_Program: STRPTR; // Application to use
  end;

const
  // TTool.tn_Which field
  TW_MISC   = 0;
  TW_INFO   = 1;
  TW_BROWSE = 2;
  TW_EDIT   = 3;
  TW_PRINT  = 4;
  TW_MAIL   = 5;
  // TTool.tnFlags field
  TF_LAUNCH_MASK = $000F;
  TF_SHELL       = $0001;
  TF_WORKBENCH   = $0002;
  TF_RX          = $0003;

  // Tags for use with FindToolNodeA(), GetToolAttrsA() and so on
  TOOLA_Dummy      = TAG_USER;
  TOOLA_Program    = TOOLA_Dummy + 1;
  TOOLA_Which      = TOOLA_Dummy + 2;
  TOOLA_LaunchType = TOOLA_Dummy + 3;

const
  ID_TAGS = Ord('D') shl 24 + Ord('T') shl 16 + Ord('T') shl 8 + Ord('G'); // DTTG

type
  PDataType = ^TDataType;
  TDataType = record
    dtn_Node1: TNode;            // These two nodes are for...
    dtn_Node2: TNode;            // ...system use only!
    dtn_Header: PDataTypeHeader;
    dtn_ToolList: TList;         // Tool nodes
    dtn_FunctionName: STRPTR;    // Name of comparison routine
    dtn_AttrList: PTagItem;      // Object creation tags
    dtn_Length: LongWord;        // Length of the memory block
  end;

const
  DTNSIZE = SizeOf(TDataType);

type
  PToolNode = ^TToolNode;
  TToolNode = record
    tn_Node: TNode;
    tn_Tool: TTool;
    tn_Length: LongWord; // Length of the memory block
  end;

const
  TNSIZE = SizeOf(TToolNode);

  ID_NAME = Ord('N') shl 24 + Ord('A') shl 16 + Ord('M') shl 8 + Ord('E');

  // Text ID's
  DTERROR_UNKNOWN_DATATYPE     = 2000;
  DTERROR_COULDNT_SAVE         = 2001;
  DTERROR_COULDNT_OPEN         = 2002;
  DTERROR_COULDNT_SEND_MESSAGE = 2003;

  // new for V40
  DTERROR_COULDNT_OPEN_CLIPBOARD = 2004;
  DTERROR_Reserved               = 2005;
  DTERROR_UNKNOWN_COMPRESSION    = 2006;
  DTERROR_NOT_ENOUGH_DATA        = 2007;
  DTERROR_INVALID_DATA           = 2008;

  DTMSG_TYPE_OFFSET = 2100;

// datatypesclass
const
  DATATYPESCLASS = 'datatypesclass';

  DTA_Dummy = TAG_USER + $1000;

  DTA_TextAttr       = DTA_Dummy + 10; // (PTextAttr) Default TextAttr to use for text within the object
  DTA_TopVert        = DTA_Dummy + 11; // (LongInt) Top vertical unit
  DTA_VisibleVert    = DTA_Dummy + 12; // (LongInt) Number of visible vertical units
  DTA_TotalVert      = DTA_Dummy + 13; // (LongInt) Total number of vertical units
  DTA_VertUnit       = DTA_Dummy + 14; // (LongInt) Number of pixels per vertical unit
  DTA_TopHoriz       = DTA_Dummy + 15; // (LongInt) Top horizontal unit
  DTA_VisibleHoriz   = DTA_Dummy + 16; // (LongInt) Number of visible horizontal units
  DTA_TotalHoriz     = DTA_Dummy + 17; // (LongInt) Total number of horiziontal units
  DTA_HorizUnit      = DTA_Dummy + 18; // (LongInt) Number of pixels per horizontal unit
  DTA_NodeName       = DTA_Dummy + 19; // (PByte) Name of the current element within the object
  DTA_Title          = DTA_Dummy + 20; // (STRPTR) Object's title
  DTA_TriggerMethods = DTA_Dummy + 21; // (PDTMethod) Pointer to a nil terminated array of trigger methods
  DTA_Data           = DTA_Dummy + 22; // (APTR) Object data
  DTA_TextFont       = DTA_Dummy + 23; // (PTextFont) Default font to use
  DTA_Methods        = DTA_Dummy + 24; // (PLongWord) Pointer to an array (terminated with #0) of methods that the object supports
  DTA_PrinterStatus  = DTA_Dummy + 25; // (LongInt) Printer error message
  DTA_PrinterProc    = DTA_Dummy + 26; // (PProcess) PRIVATE! Pointer to the print process
  DTA_LayoutProc     = DTA_Dummy + 27; // (PProcess) PRIVATE! Pointer to the print process
  DTA_Busy           = DTA_Dummy + 28; // Turns the application's busy pointer on and off
  DTA_Sync           = DTA_Dummy + 29; // Indicate that new information has been loaded into an object.
                                       // (This is used for models that cache the DTA_TopVert-like tags.)
  DTA_BaseName       = DTA_Dummy + 30; // Base name of the class
  DTA_GroupID        = DTA_Dummy + 31; // Group that the object must belong to
  DTA_ErrorLevel     = DTA_Dummy + 32; // Error level
  DTA_ErrorNumber    = DTA_Dummy + 33; // datatypes.library error number
  DTA_ErrorString    = DTA_Dummy + 34; // Argument for datatypes.library error
  DTA_Conductor      = DTA_Dummy + 35; // (PByte) Name of a realtime.library conductor -- defaults to "Main"
  DTA_ControlPanel   = DTA_Dummy + 36; // (LongBool) Specify whether a control panel should be embedded into the object or not
                                       // (for example in the animation datatype) -- defaults to True
  DTA_Immediate      = DTA_Dummy + 37; // (LongBool) Should the object begin playing immediately? -- defaults to False
  DTA_Repeat         = DTA_Dummy + 38; // (LongBool) Indicate that the object should repeat playing -- defaults to False
  // V44
  DTA_SourceAddress  = DTA_Dummy + 39; // (APTR) V44: Address of object if of type DTST_MEMORY
  DTA_SourceSize     = DTA_Dummy + 40; // (LongWord) V44: Size of object if of type DTST_MEMORY

  // DTObject attributes
  DTA_Name          = DTA_Dummy + 100;
  DTA_SourceType    = DTA_Dummy + 101;
  DTA_Handle        = DTA_Dummy + 102;
  DTA_DataType      = DTA_Dummy + 103;
  DTA_Domain        = DTA_Dummy + 104;
  // Left, Top, Width, Height -> use the Gadgetclass Tags
  DTA_ObjName       = DTA_Dummy + 109;
  DTA_ObjAuthor     = DTA_Dummy + 110;
  DTA_ObjAnnotation = DTA_Dummy + 111;
  DTA_ObjCopyright  = DTA_Dummy + 112;
  DTA_ObjVersion    = DTA_Dummy + 113;
  DTA_ObjectID      = DTA_Dummy + 114;
  DTA_UserData      = DTA_Dummy + 115;
  // RelLeft, RelTop, RelWidth, RelHeight -> use the Gadgetclass Tags
  DTA_FrameInfo     = DTA_Dummy + 116;
  DTA_SelectDomain  = DTA_Dummy + 121;
  DTA_TotalPVert    = DTA_Dummy + 122;
  DTA_TotalPHoriz   = DTA_Dummy + 123;
  DTA_NominalVert   = DTA_Dummy + 124;
  DTA_NominalHoriz  = DTA_Dummy + 125;

  // Printing attributes
  DTA_DestCols      = DTA_Dummy + 400; // (LongInt) Destination x width
  DTA_DestRows      = DTA_Dummy + 401; // (LongInt) Destination y height
  DTA_Special       = DTA_Dummy + 402; // (Word) Option flags
  DTA_RastPort      = DTA_Dummy + 403; // (PRastPort) RastPort used when printing
  DTA_ARexxPortName = DTA_Dummy + 404; // (STRPTR) Pointer to base name for ARexx port

  DTST_RAM       = 1;
  DTST_FILE      = 2;
  DTST_CLIPBOARD = 3;
  DTST_HOTLINK   = 4;
  DTST_MEMORY    = 5; // V44

// This structure is attached to the Gadget.SpecialInfo field of the gadget. Use Get/Set calls to access it.
type
  PDTSpecialInfo = ^TDTSpecialInfo;
  TDTSpecialInfo = record
    si_Lock: TSignalSemaphore;
    si_Flags: LongWord;

    si_TopVert: LongInt;   // Top row (in units)
    si_VisVert: LongInt;   // Number of visible rows (in units)
    si_TotVert: LongInt;   // Total number of rows (in units)
    si_OTopVert: LongInt;  // Previous top (in units)
    si_VertUnit: LongInt;  // Number of pixels in vertical unit

    si_TopHoriz: LongInt;  // Top column (in units)
    si_VisHoriz: LongInt;  // Number of visible columns (in units)
    si_TotHoriz: LongInt;  // Total number of columns (in units)
    si_OTopHoriz: LongInt; // Previous top (in units)
    si_HorizUnit: LongInt; // Number of pixels in horizontal unit
 end;

const
  DTSIF_LAYOUT     = 1 shl 0; // Object is in layout processing
  DTSIF_NEWSIZE    = 1 shl 1; // Object needs to be layed out
  DTSIF_DRAGGING   = 1 shl 2;
  DTSIF_DRAGSELECT = 1 shl 3;
  DTSIF_HIGHLIGHT  = 1 shl 4;
  DTSIF_PRINTING   = 1 shl 5; // Object is being printed
  DTSIF_LAYOUTPROC = 1 shl 6; // Object is in layout process

type
  PDTMethod = ^TDTMethod;
  TDTMethod = record
    dtm_Label: STRPTR;
    dtm_Command: STRPTR;
    dtm_Method: LongWord;
  end;

const
  DTM_Dummy          = $600;

  DTM_FRAMEBOX       = $601; // Get the environment an object requires
  DTM_PROCLAYOUT     = $602;
  DTM_ASYNCLAYOUT    = $603;
  DTM_REMOVEDTOBJECT = $604; // When RemoveDTObject() is called

  DTM_SELECT         = $605;
  DTM_CLEARSELECTED  = $606;

  DTM_COPY           = $607;
  DTM_PRINT          = $608;
  DTM_ABORTPRINT     = $609;

  DTM_NEWMEMBER      = $610;
  DTM_DISPOSEMEMBER  = $611;

  DTM_GOTO           = $630;
  DTM_TRIGGER        = $631;

  DTM_OBTAINDRAWINFO  = $640;
  DTM_DRAW            = $641;
  DTM_RELEASEDRAWINFO = $642;

  DTM_WRITE           = $650;

type
  PFrameInfo = ^TFrameInfo;
  TFrameInfo = record
    fri_PropertyFlags: LongWord; // DisplayInfo
    fri_Resolution: TPoint;      // DisplayInfo

    fri_RedBits: Byte;
    fri_GreenBits: Byte;
    fri_BlueBits: Byte;

    fri_Dimensions: record
      Width: LongWord;
      Height: LongWord;
      Depth: LongWord;
    end;

    fri_Screen: PScreen;
    fri_ColorMap: PColorMap;

    fri_Flags: LongWord; // FIF_*
  end;

const
  FIF_SCALABLE   = $1;
  FIF_SCROLLABLE = $2;
  FIF_REMAPPABLE = $4;

type
  // DTM_REMOVEDTOBJECT, DTM_CLEARSELECTED, DTM_COPY, DTM_ABORTPRINT
  PdtGeneral = ^TdtGeneral;
  TdtGeneral = record
    MethodID: PtrUInt;
    dtg_GInfo: PGadgetInfo;
  end;

  // DTM_SELECT
  PdtSelect = ^TdtSelect;
  TdtSelect = record
    MethodID: PtrUInt;
    dts_GInfo: PGadgetInfo;
    dts_Select: TRectangle;
  end;

  // DTM_FRAMEBOX
  PdtFrameBox = ^TdtFrameBox;
  TdtFrameBox = record
    MethodID: PtrUInt;
    dtf_GInfo: PGadgetInfo;
    dtf_ContentsInfo: PFrameInfo;
    dtf_FrameInfo: PFrameInfo;    // Input
    dtf_SizeFrameInfo: LongWord;  // Output
    dtf_FrameFlags: LongWord;
  end;

  // DTM_GOTO
  PdtGoto = ^TdtGoto;
  TdtGoto = record
    MethodID: PtrUInt;
    dtg_GInfo: PGadgetInfo;
    dtg_NodeName: STRPTR;   // Node to goto
    dtg_AttrList: PTagItem; // Additional attributes
  end;

  // DTM_TRIGGER
  PdtTrigger = ^TdtTrigger;
  TdtTrigger = record
    MethodID: PtrUInt;
    dtt_GInfo: PGadgetInfo;
    dtt_Function: LongWord;
    dtt_Data: APTR;
  end;

const
  STMF_METHOD_MASK   = $0000FFFF;
  STMF_DATA_MASK     = $00FF0000;
  STMF_RESERVED_MASK = $FF000000;

  STMD_VOID    = $00010000;
  STMD_ULONG   = $00020000;
  STMD_STRPTR  = $00030000;
  STMD_TAGLIST = $00040000;

  STM_DONE        = 0;
  STM_PAUSE       = 1;
  STM_PLAY        = 2;
  STM_CONTENTS    = 3;
  STM_INDEX       = 4;
  STM_RETRACE     = 5;
  STM_BROWSE_PREV = 6;
  STM_BROWSE_NEXT = 7;

  STM_NEXT_FIELD     =  8;
  STM_PREV_FIELD     =  9;
  STM_ACTIVATE_FIELD = 10;

  STM_COMMAND = 11;

  STM_REWIND      = 12;
  STM_FASTFORWARD = 13;
  STM_STOP        = 14;
  STM_RESUME      = 15;
  STM_LOCATE      = 16;
  //* 17 is reserved for help */
  STM_SEARCH      = 18;
  STM_SEARCH_NEXT = 19;
  STM_SEARCH_PREV = 20;

  STM_USER = 100;

  // skipped PrinterIO, dtPrint -> needs printer unit

type
  // DTM_DRAW
  PdtDraw = ^TdtDraw;
  TdtDraw = record
    MethodID: PtrUInt;
    dtd_RPort: PRastPort;
    dtd_Left: LongInt;
    dtd_Top: LongInt;
    dtd_Width: LongInt;
    dtd_Height: LongInt;
    dtd_TopHoriz: LongInt;
    dtd_TopVert: LongInt;
    dtd_AttrList: PTagItem; // Additional attributes
  end;

  // DTM_RELEASERAWINFO
  PdtReleaseDrawInfo = ^TdtReleaseDrawInfo;
  TdtReleaseDrawInfo = record
    MethodID: PtrUInt;
    dtr_Handle: APTR; // Handle as returned by DTM_OBTAINDRAWINFO
  end;

  // DTM_WRITE
  PdtWrite = ^TdtWrite;
  TdtWrite = record
    MethodID: PtrUInt;
    dtw_GInfo: PGadgetInfo; // Gadget information
    dtw_FileHandle: BPTR;   // File handle to write to
    dtw_Mode: LongWord;
    dtw_AttrList: PTagItem; // Additional attributes
  end;

const
  DTWM_IFF = 0; // Save data as IFF data
  DTWM_RAW = 1; // Save data as local data format

// amigaguideclass
const
  AMIGAGUIDEDTCLASS = 'amigaguide.datatype';

  AGDTA_Dummy      = DTA_Dummy + 700;
  AGDTA_Secure     = AGDTA_Dummy + 1;
  AGDTA_HelpGroup  = AGDTA_Dummy + 2;

//  pictureclass
const
  PICTUREDTCLASS = 'picture.datatype';

type
  PBitMapHeader = ^TBitMapHeader;
  TBitMapHeader = record
    bmh_Width: Word;
    bmh_Height: Word;
    bmh_Left: SmallInt;
    bmh_Top: SmallInt;
    bmh_Depth: Byte;
    bmh_Masking: Byte;
    bmh_Compression: Byte;
    bmh_Pad: Byte;
    bmh_Transparent: Word;
    bmh_XAspect: Byte;
    bmh_YAspect: Byte;
    bmh_PageWidth: SmallInt;
    bmh_PageHeight: SmallInt;
  end;

  PColorRegister = ^TColorRegister;
  TColorRegister = record
    red: Byte;
    green: Byte;
    blue: Byte;
  end;

const
  PDTA_ModeID           = DTA_Dummy + 200;
  PDTA_BitMapHeader     = DTA_Dummy + 201;
  PDTA_BitMap           = DTA_Dummy + 202;
  PDTA_ColorRegisters   = DTA_Dummy + 203;
  PDTA_CRegs            = DTA_Dummy + 204;
  PDTA_GRegs            = DTA_Dummy + 205;
  PDTA_ColorTable       = DTA_Dummy + 206;
  PDTA_ColorTable2      = DTA_Dummy + 207;
  PDTA_Allocated        = DTA_Dummy + 208;
  PDTA_NumColors        = DTA_Dummy + 209;
  PDTA_NumAlloc         = DTA_Dummy + 210;
  PDTA_Remap            = DTA_Dummy + 211;
  PDTA_Screen           = DTA_Dummy + 212;
  PDTA_FreeSourceBitMap = DTA_Dummy + 213;
  PDTA_Grab             = DTA_Dummy + 214;
  PDTA_DestBitMap       = DTA_Dummy + 215;
  PDTA_ClassBitMap      = DTA_Dummy + 216;
  PDTA_NumSparse        = DTA_Dummy + 217;
  PDTA_SparseTable      = DTA_Dummy + 218;

  PDTA_SourceMode       = DTA_Dummy + 250; // Set the interface mode for the sub datatype. See below.
  PDTA_DestMode         = DTA_Dummy + 251; // Set the interface mode for the app datatype. See below.
  PDTA_UseFriendBitMap  = DTA_Dummy + 255; // Make the allocated bitmap be a "friend" bitmap (LongBool)

  // Interface modes
  PMODE_V42 = 0; // Mode used for backward compatibility
  PMODE_V43 = 1; // Use the new features

  mskNone                = 0;
  mskHasMask             = 1;
  mskHasTransparentColor = 2;
  mskLasso               = 3;
  mskHasAlpha            = 4;

  cmpNone     = 0;
  cmpByteRun1 = 1;
  cmpByteRun2 = 2;

  ID_ILBM = Ord('I') shl 24 + Ord('L') shl 16 + Ord('B') shl 8 + Ord('M'); // ILBM
  ID_BMHD = Ord('B') shl 24 + Ord('M') shl 16 + Ord('H') shl 8 + Ord('D'); // BMHD
  ID_BODY = Ord('B') shl 24 + Ord('O') shl 16 + Ord('D') shl 8 + Ord('Y'); // BODY
  ID_CMAP = Ord('C') shl 24 + Ord('M') shl 16 + Ord('A') shl 8 + Ord('P'); // CMAP
  ID_CRNG = Ord('C') shl 24 + Ord('R') shl 16 + Ord('N') shl 8 + Ord('G'); // CRNG
  ID_GRAB = Ord('G') shl 24 + Ord('R') shl 16 + Ord('A') shl 8 + Ord('B'); // GRAB
  ID_SPRT = Ord('S') shl 24 + Ord('P') shl 16 + Ord('R') shl 8 + Ord('T'); // SPRT
  ID_DEST = Ord('D') shl 24 + Ord('E') shl 16 + Ord('S') shl 8 + Ord('T'); // DEST
  ID_CAMG = Ord('C') shl 24 + Ord('A') shl 16 + Ord('M') shl 8 + Ord('G'); // CAMG

 {Support for the V44 picture.datatype

    It is not clear, if AROS should support AmigaOS3.5 .

    But if you want V44-support define DT_V44_SUPPORT

    Joerg Dietrich}

{$IFDEF DT_V44_SUPPORT}
const
  PDTANUMPICTURES_Unknown = 0;

  PDTA_WhichPicture    = DTA_Dummy + 219;
  PDTA_GetNumPictures  = DTA_Dummy + 220;
  PDTA_MaxDitherPens   = DTA_Dummy + 221;
  PDTA_DitherQuality   = DTA_Dummy + 222;
  PDTA_AllocatedPens   = DTA_Dummy + 223;
  PDTA_ScaleQuality    = DTA_Dummy + 224;
  PDTA_DelayRead       = DTA_Dummy + 225;
  PDTA_DelayedRead     = DTA_Dummy + 226;

  PDTA_SourceMode      = DTA_Dummy + 250;
  PDTA_DestMode        = DTA_Dummy + 251;
  PDTA_UseFriendBitMap = DTA_Dummy + 255;
  PDTA_MaskPlane       = DTA_Dummy + 258;

  PDTM_Dummy           = DTM_Dummy + $60;
  PDTM_WRITEPIXELARRAY = PDTM_Dummy + 0;
  PDTM_READPIXELARRAY  = PDTM_Dummy + 1;
  PDTM_SCALE           = PDTM_Dummy + 2;

type
  PpdtBlitPixelArray = ^TpdtBlitPixelArray;
  TpdtBlitPixelArray = record
    MethodID: PtrUInt;
    pbpa_PixelData: APTR;
    pbpa_PixelFormat: LongWord;
    pbpa_PixelArrayMod: LongWord;
    pbpa_Left: LongWord;
    pbpa_Top: LongWord;
    pbpa_Width: LongWord;
    pbpa_Height: LongWord;
  end;

  PpdtScale = ^TpdtScale;
  TpdtScale = record
    MethodID: PtrUInt;
    ps_NewWidth: LongWord;
    ps_NewHeight: LongWord;
    ps_Flags: LongWord;
  end;

const
  // Flags for ps_Flags, for AROS only
  PScale_KeepAspect = $10;  // Keep aspect ratio when scaling, fit inside given x, y coordinates

  PBPAFMT_RGB       = 0;
  PBPAFMT_RGBA      = 1;
  PBPAFMT_ARGB      = 2;
  PBPAFMT_LUT8      = 3;
  PBPAFMT_GREY8     = 4;
{$ENDIF DT_V44_SUPPORT}

// soundclass

const
  SOUNDDTCLASS = 'sound.datatype';

const
  // Tags
  SDTA_Dummy         = DTA_Dummy + 500;
  SDTA_VoiceHeader   = SDTA_Dummy + 1;
  SDTA_Sample        = SDTA_Dummy + 2;
  SDTA_SampleLength  = SDTA_Dummy + 3;
  SDTA_Period        = SDTA_Dummy + 4;
  SDTA_Volume        = SDTA_Dummy + 5;
  SDTA_Cycles        = SDTA_Dummy + 6;
  SDTA_SignalTask    = SDTA_Dummy + 7;
  SDTA_SignalBit     = SDTA_Dummy + 8;
  SDTA_SignalBitMask = SDTA_SignalBit;
  SDTA_Continuous    = SDTA_Dummy + 9;

  // New in V44
  SDTA_SignalBitNumber = SDTA_Dummy + 10;
  SDTA_SamplesPerSec    = SDTA_Dummy + 11;
  SDTA_ReplayPeriod     = SDTA_Dummy + 12;
  SDTA_LeftSample       = SDTA_Dummy + 13;
  SDTA_RightSample      = SDTA_Dummy + 14;
  SDTA_Pan              = SDTA_Dummy + 15;
  SDTA_FreeSampleData   = SDTA_Dummy + 16;
  SDTA_SyncSampleChange = SDTA_Dummy + 17;

  // Data compression methods
  CMP_NONE     = 0;
  CMP_FIBDELTA = 1;

  // Unity = Fixed 1.0 = maximum volume
  Unity = $10000;

type
  PVoiceHeader = ^TVoiceHeader;
  TVoiceHeader = record
    vh_OneShotHiSamples: LongWord;
    vh_RepeatHiSamples: LongWord;
    vh_SamplesPerHiCycle: LongWord;
    vh_SamplesPerSec: Word;
    vh_Octaves: Byte;
    vh_Compression: Byte;
    vh_Volume: LongWord;
  end;

const
  // Channel allocation */
  SAMPLETYPE_Left   = 2;
  SAMPLETYPE_Right  = 4;
  SAMPLETYPE_Stereo = 6;

type
  TSampleType = LongInt;

const
  // IFF types
  ID_8SVX = Ord('8') shl 24 + Ord('S') shl 16 + Ord('V') shl 8 + Ord('X'); // 8SVX
  ID_VHDR = Ord('V') shl 24 + Ord('H') shl 16 + Ord('D') shl 8 + Ord('R'); // VHDR
  ID_CHAN = Ord('C') shl 24 + Ord('H') shl 16 + Ord('A') shl 8 + Ord('N'); // CHAN

// soundclassext
const
  SDTA_SampleType = SDTA_Dummy + 30;
  SDTA_Panning    = SDTA_Dummy + 31;
  SDTA_Frequency  = SDTA_Dummy + 32;

  SDTST_M8S  = 0;
  SDTST_S8S  = 1;
  SDTST_M16S = 2;
  SDTST_S16S = 3;

// textclass

const
  TEXTDTCLASS = 'text.datatype';

  // attributes
  TDTA_Buffer      = DTA_Dummy + 300;
  TDTA_BufferLen   = DTA_Dummy + 301;
  TDTA_LineList    = DTA_Dummy + 302;
  TDTA_WordSelect  = DTA_Dummy + 303;
  TDTA_WordDelim   = DTA_Dummy + 304;
  TDTA_WordWrap    = DTA_Dummy + 305;


Type
  // There is one line structure for every line of text in the document.
  PLine = ^TLine;
  TLine = record
    ln_Link: TMinNode;
    ln_Text: STRPTR;
    ln_TextLen: LongWord;
    ln_XOffset: Word;
    ln_YOffset: Word;
    ln_Width: Word;
    ln_Height: Word;
    ln_Flags: Word;      // LNF_*
    ln_FgPen: ShortInt;
    ln_BgPen: ShortInt;
    ln_Style: LongWord;
    ln_Data: APTR;
  end;

const
  // ln_Flags
  LNF_LF       = 1 shl 0;
  LNF_LINK     = 1 shl 1;
  LNF_OBJECT   = 1 shl 2;
  LNF_SELECTED = 1 shl 3;

  ID_FTXT = Ord('F') shl 24 + Ord('T') shl 16 + Ord('X') shl 8 + Ord('T'); // FTXT
  ID_CHRS = Ord('C') shl 24 + Ord('H') shl 16 + Ord('R') shl 8 + Ord('S'); // CHRS

// animationclass
const
  ANIMATIONDTCLASS = 'animation.datatype';

  // Tags
  ADTA_Dummy           = DTA_Dummy + 600;
  ADTA_ModeID          = PDTA_ModeID;
  ADTA_KeyFrame        = PDTA_BitMap;
  ADTA_ColorRegisters  = PDTA_ColorRegisters;
  ADTA_CRegs           = PDTA_CRegs;
  ADTA_GRegs           = PDTA_GRegs;
  ADTA_ColorTable      = PDTA_ColorTable;
  ADTA_ColorTable2     = PDTA_ColorTable2;
  ADTA_Allocated       = PDTA_Allocated;
  ADTA_NumColors       = PDTA_NumColors;
  ADTA_NumAlloc        = PDTA_NumAlloc;
  ADTA_Remap           = PDTA_Remap;
  ADTA_Screen          = PDTA_Screen;
  ADTA_Width           = ADTA_Dummy + 1;
  ADTA_Height          = ADTA_Dummy + 2;
  ADTA_Depth           = ADTA_Dummy + 3;
  ADTA_Frames          = ADTA_Dummy + 4;
  ADTA_Frame           = ADTA_Dummy + 5;
  ADTA_FramesPerSecond = ADTA_Dummy + 6;
  ADTA_FrameIncrement  = ADTA_Dummy + 7;
  ADTA_Sample          = SDTA_Sample;
  ADTA_SampleLength    = SDTA_SampleLength;
  ADTA_Period          = SDTA_Period;
  ADTA_Volume          = SDTA_Volume;
  ADTA_Cycles          = SDTA_Cycles;

  // New in V44
  ADTA_PreloadFrameCount = ADTA_Dummy + 8;
  ADTA_LeftSample        = SDTA_LeftSample;
  ADTA_RightSample       = SDTA_RightSample;
  ADTA_SamplesPerSec     = SDTA_SamplesPerSec;

  // IFF ANIM chunks
  ID_ANIM = Ord('A') shl 24 + Ord('N') shl 16 + Ord('I') shl 8 + Ord('M'); // ANIM
  ID_ANHD = Ord('A') shl 24 + Ord('N') shl 16 + Ord('H') shl 8 + Ord('D'); // ANHD
  ID_DLTA = Ord('D') shl 24 + Ord('L') shl 16 + Ord('T') shl 8 + Ord('A'); // DLTA

type
  PAnimHeader = ^TAnimHeader;
  TAnimHeader = record
    ah_Operation: Byte;
    ah_Mask: Byte;
    ah_Height: Word;
    ah_Width: Word;
    ah_Left: SmallInt;
    ah_Top: SmallInt;
    ah_AbsTime: LongWord;
    ah_RelTime: LongWord;
    ah_Interleave: Byte;
    ah_Pad0: Byte;
    ah_Flags: LongWord;
    ah_Pad: array[0..15] of Byte;
  end;

const
  // Methods
  ADTM_Dummy       = $700;
  ADTM_LOADFRAME   = $701;
  ADTM_UNLOADFRAME = $702;
  ADTM_START       = $703;
  ADTM_PAUSE       = $704;
  ADTM_STOP        = $705;
  ADTM_LOCATE      = $706;
  // New on V44
  ADTM_LOADNEWFORMATFRAME   = $707;
  ADTM_UNLOADNEWFORMATFRAME = $708;

type
  PadtFrame = ^TadtFrame;
  TadtFrame = record
    MethodID: PtrUInt;
    alf_TimeStamp: LongWord;
    alf_Frame: LongWord;
    alf_Duration: LongWord;
    alf_BitMap: PBitMap;
    alf_CMap: PColorMap;
    alf_Sample: PShortInt;
    alf_SampleLength: LongWord;
    alf_Period: LongWord;
    alf_UserData: APTR;
  end;

  PadtNewFormatFrame = ^TadtNewFormatFrame;
  TadtNewFormatFrame = record
    MethodID: PtrUInt;
    alf_TimeStamp: LongWord;
    alf_Frame: LongWord;
    alf_Duration: LongWord;
    alf_BitMap: PBitMap;
    alf_CMap: PColorMap;
    alf_Sample: PShortInt;
    alf_SampleLength: LongWord;
    alf_Period: LongWord;
    alf_UserData: APTR;
    alf_Size: LongWord;
    alf_LeftSample: PShortInt;
    alf_RightSample: PShortInt;
    alf_SamplesPerSec: LongWord;
  end;

  PadtStart = ^tadtStart;
  TadtStart = record
    MethodID: PtrUInt;
    asa_Frame: LongWord;
  end;

function SDTM_ISSTEREO(SampleType: LongWord): Boolean; inline;
function SDTM_CHANNELS(SampleType: LongWord): LongWord; inline;
function SDTM_BYTESPERSAMPLE(x: LongWord): LongWord; inline;
function SDTM_BYTESPERPOINT(x: LongWord): LongWord; inline;

var
  DataTypesBase: PLibrary;

function ObtainDataTypeA(Typ: LongWord; Handle: APTR; Attrs: PTagItem): PDataType; syscall DataTypesBase 6;
procedure ReleaseDataType(Dt: PDataType); syscall DataTypesBase 7;
function NewDTObjectA(Name: APTR; Attrs: PTagItem): PObject_; syscall DataTypesBase 8;
procedure DisposeDTObject(O: PObject_); syscall DataTypesBase 9;
function SetDTAttrsA(O: PObject_; Win: PWindow; Req: PRequester; Attrs: PTagItem): LongWord; syscall DataTypesBase 10;
function GetDTAttrsA(O: PObject_; Attrs: PTagItem): LongWord; syscall DataTypesBase 11;
function AddDTObject(Win: PWindow; Req: PRequester; Obj: PObject_; Pos: LongInt): LongInt; syscall DataTypesBase 12;
procedure RefreshDTObjectA(Obj: PObject_; Window: PWindow; Req: PRequester; Attrs: PTagItem); syscall DataTypesBase 13;
function DoAsyncLayout(Obj: PObject_; Gpl: PgpLayout): LongWord; syscall DataTypesBase 14;
function DoDTMethodA(O: PObject_; Win: PWindow; Req: PRequester; Msg: PMsg): PtrUInt; syscall DataTypesBase 15;
function RemoveDTObject(Window: PWindow; Obj: PObject_): LongInt; syscall DataTypesBase 16;
function GetDTMethods(Obj: PObject_): PLongWord; syscall DataTypesBase 17;
function GetDTTriggerMethods(Obj: PObject_): PDTMethod; syscall DataTypesBase 18;
function PrintDTObjectA(Obj: PObject_; Window: PWindow; Requester: PRequester; Msg: Pointer {PdtPrint}): LongWord; syscall DataTypesBase 19;
function ObtainDTDrawInfoA(O: PObject_; Attrs: PTagItem): APTR; syscall DataTypesBase 20;
function DrawDTObjectA(Rp: PRastPort; O: PObject_; x, y, w, h, th, tv: LongInt; Attrs: PTagItem): LongInt; syscall DataTypesBase 21;
procedure ReleaseDTDrawInfo(O: PObject_; Handle: APTR); syscall DataTypesBase 22;
function GetDTString(Id: LongWord): CONST_STRPTR; syscall DataTypesBase 23;
procedure LockDataType(Dt: PDataType); syscall DataTypesBase 40;
function FindToolNodeA(ToolList: PList; Attrs: PTagItem): PToolNode; syscall DataTypesBase 41;
function LaunchToolA(Tool: PTool; Project: STRPTR; Attrs: PTagItem): LongWord; syscall DataTypesBase 42;
function FindMethod(Methods: PLongWord; SearchModeID: LongWord): PLongWord; syscall DataTypesBase 43;
function FindTriggerMethod(Methods: PDTMethod; Command: STRPTR; Method: LongWord): PDTMethod; syscall DataTypesBase 44;
function CopyDTMethods(Methods: PLongWord; Include: PLongWord; Exclude: PLongWord): PLongWord; syscall DataTypesBase 45;
function CopyDTTriggerMethods(Methods: PDTMethod; Include: PDTMethod; Exclude: PDTMethod): PDTMethod; syscall DataTypesBase 46;
procedure FreeDTMethods(Methods: APTR); syscall DataTypesBase 47;
function GetDTTriggerMethodDataFlags(Method: LongWord): LongWord; syscall DataTypesBase 48;
function SaveDTObjectA(O: PObject_; Win: PWindow; Req: PRequester; File_: STRPTR; Mode: LongWord; SaveIcon: Bool; Attrs: PTagItem): LongWord; syscall DataTypesBase 49;
function StartDragSelect(O: PObject_): LongWord; syscall DataTypesBase 50;
function DoDTDomainA(O: PObject_; Win: PWindow; Req: PRequester; RPort: PRastPort; Which: LongWord; Domain: PIBox; Attrs: PTagItem): LongWord; syscall DataTypesBase 51;

function ObtainDataType(Typ: LongWord; Handle: APTR; const TagList: array of PtrUInt): PDataType; inline;
function NewDTObject(Name: APTR; const TagList: array of PtrUInt): PObject_; inline;
function SetDTAttrs(O: PObject_; Win: PWindow; Req: PRequester; const TagList: array of PtrUInt): LongWord; inline;
function GetDTAttrs(O: PObject_; const TagList: array of PtrUInt): LongWord; inline;
procedure RefreshDTObject(Obj: PObject_; Window: PWindow; Req: PRequester; const TagList: array of PtrUInt); inline;
function DoDTMethod(O: PObject_; Win: PWindow; Req: PRequester; const ArgList: array of PtrUInt): PtrUInt; inline;
function PrintDTObject(Obj: PObject_; Window: PWindow; Requester: PRequester; const ArgList: array of PtrUInt): LongWord; inline;
function ObtainDTDrawInfo(O: PObject_; const TagList: array of PtrUInt): APTR; inline;
function DrawDTObject(Rp: PRastPort; O: PObject_; x, y, w, h, th, tv: LongInt; const TagList: array of PtrUInt): LongInt; inline;
function FindToolNode(ToolList: PList; const TagList: array of PtrUInt): PToolNode; inline;
function LaunchTool(Tool: PTool; Project: STRPTR; const TagList: array of PtrUInt): LongWord; inline;
function SaveDTObject(O: PObject_; Win: PWindow; Req: PRequester; File_: STRPTR; Mode: LongWord; SaveIcon: Bool; const TagList: array of PtrUInt): LongWord; inline;
function DoDTDomain(O: PObject_; Win: PWindow; Req: PRequester; RPort: PRastPort; Which: LongWord; Domain: PIBox; const TagList: array of PtrUInt): LongWord; inline;

implementation

function SDTM_ISSTEREO(SampleType: LongWord): Boolean; inline;
begin
  SDTM_ISSTEREO := Boolean(SampleType and 1);
end;

function SDTM_CHANNELS(SampleType: LongWord): LongWord; inline;
begin
  SDTM_CHANNELS := 1 + (SampleType and 1);
end;

function SDTM_BYTESPERSAMPLE(x: LongWord): LongWord; inline;
begin
  if x >= SDTST_M16S then
    SDTM_BYTESPERSAMPLE := 2
  else
    SDTM_BYTESPERSAMPLE := 1;
end;

function SDTM_BYTESPERPOINT(x: LongWord): LongWord;
begin
  SDTM_BYTESPERPOINT := SDTM_CHANNELS(x) * SDTM_BYTESPERSAMPLE(x);
end;

function ObtainDataType(Typ: LongWord; Handle: APTR; const TagList: array of PtrUInt): PDataType;
begin
  ObtainDataType := ObtainDataTypeA(Typ, Handle, @TagList);
end;

function NewDTObject(Name: APTR; const TagList: array of PtrUInt): PObject_;
begin
  NewDTObject := NewDTObjectA(Name, @TagList);
end;

function SetDTAttrs(O: PObject_; Win: PWindow; Req: PRequester; const TagList: array of PtrUInt): LongWord;
begin
  SetDTAttrs := SetDTAttrsA(O, Win, Req, @TagList);
end;

function GetDTAttrs(O: PObject_; const TagList: array of PtrUInt): LongWord;
begin
  GetDTAttrs := GetDTAttrsA(O, @TagList);
end;

procedure RefreshDTObject(Obj: PObject_; Window: PWindow; Req: PRequester; const TagList: array of PtrUInt);
begin
  RefreshDTObjectA(Obj, Window, Req, @TagList);
end;

function DoDTMethod(O: PObject_; Win: PWindow; Req: PRequester; const ArgList: array of PtrUInt): PtrUInt;
begin
  DoDTMethod := DoDTMethodA(O, Win, Req, @ArgList);
end;

function PrintDTObject(Obj: PObject_; Window: PWindow; Requester: PRequester; const ArgList: array of PtrUInt): LongWord;
begin
  PrintDTObject := PrintDTObjectA(Obj, Window, Requester, @ArgList);
end;

function ObtainDTDrawInfo(O: PObject_; const TagList: array of PtrUInt): APTR;
begin
  ObtainDTDrawInfo := ObtainDTDrawInfoA(O, @TagList);
end;

function DrawDTObject(Rp: PRastPort; O: PObject_; x, y, w, h, th, tv: LongInt; const TagList: array of PtrUInt): LongInt;
begin
  DrawDTObject := DrawDTObjectA(RP, O, x, y, w, h, th, tv, @TagList);
end;

function FindToolNode(ToolList: PList; const TagList: array of PtrUInt): PToolNode;
begin
  FindToolNode := FindToolNodeA(ToolList, @TagList);
end;

function LaunchTool(Tool: PTool; Project: STRPTR; const TagList: array of PtrUInt): LongWord;
begin
  LaunchTool := LaunchToolA(Tool, Project, @TagList);
end;

function SaveDTObject(O: PObject_; Win: PWindow; Req: PRequester; File_: STRPTR; Mode: LongWord; SaveIcon: Bool; const TagList: array of PtrUInt): LongWord;
begin
  SaveDTObject := SaveDTObjectA(O, Win, Req, File_, Mode, SaveIcon, @TagList);
end;

function DoDTDomain(O: PObject_; Win: PWindow; Req: PRequester; RPort: PRastPort; Which: LongWord; Domain: PIBox; const TagList: array of PtrUInt): LongWord;
begin
  DoDTDomain := DoDTDomainA(O, Win, Req, RPort, Which, Domain, @TagList);
end;

initialization
  DataTypesBase := OpenLibrary(DATATYPESNAME, 0);
finalization
  CloseLibrary(DataTypesBase);

end.
