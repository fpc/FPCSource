{Set tabsize to 4.}
{****************************************************************************


                           VIOCALLS interface unit
                     Free Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Klaempfl
                    Copyright (c) 1999-2000 by Daniel Mantione
                      Copyright (c) 1999-2000 by Tomas Hajny

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This is an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit VioCalls;

{ Interface library to VIOCALLS.DLL (through EMXWRAP.DLL)

Variant records and aliases for some record types created to maintain highest
possible level of compatibility with other existing OS/2 compilers.

Changelog:

    People:

        TH - Tomas Hajny (xhajt03@mbox.vol.cz on Internet)

    Date:           Description of change:              Changed by:

     -              First released version 0.50         TH
    00/09/24        TVioCursorInfo definition extended,
                    new names for VioScroll* added      TH

Coding style:

    I have tried to use the same coding style as Daniel Mantione in unit
    DOSCALLS, although I can't say I would write it the same way otherwise
    (I would write much more spaces myself, at least). Try to use it as well,
    please. Original note by Daniel Mantione follows:


    It may be well possible that coding style feels a bit strange to you.
    Nevertheless I friendly ask you to try to make your changes not look all
    to different. To make life easier, set your IDE to use tab characters,
    turn optimal fill, autoindent and backspace unindents on and set a
    tabsize of 4.}


{***************************************************************************}
interface
{***************************************************************************}

{$IFDEF FPC}
    {$PACKRECORDS 1}
{$ENDIF FPC}


const
{return codes / error constants}
    NO_ERROR                        =  0;
    Error_Invalid_Parameter         = 87;
    ERROR_VIO_INVALID_MASK          =349;
    ERROR_VIO_PTR                   =350;
    ERROR_VIO_APTR                  =351;
    ERROR_VIO_RPTR                  =352;
    ERROR_VIO_CPTR                  =353;
    ERROR_VIO_LPTR                  =354;
    ERROR_VIO_MODE                  =355;
    ERROR_VIO_WIDTH                 =356;
    ERROR_VIO_ATTR                  =357;
    ERROR_VIO_ROW                   =358;
    ERROR_VIO_COL                   =359;
    ERROR_VIO_TOPROW                =360;
    ERROR_VIO_BOTROW                =361;
    ERROR_VIO_RIGHTCOL              =362;
    ERROR_VIO_LEFTCOL               =363;
    ERROR_VIO_WAIT_FLAG             =366;
    ERROR_VIO_UNLOCK                =367;
    ERROR_VIO_SMG_ONLY              =402;
    ERROR_VIO_INVALID_ASCIIZ        =403;
    ERROR_VIO_DEREGISTER            =404;
    ERROR_VIO_NO_POPUP              =405;
    ERROR_VIO_EXISTING_POPUP        =406;
    ERROR_VIO_INVALID_PARMS         =421;
    ERROR_VIO_FUNCTION_OWNED        =422;
    ERROR_VIO_RETURN                =423;
    ERROR_VIO_REGISTER              =426;
    ERROR_VIO_NO_MODE_THREAD        =427;
    ERROR_VIO_NO_SAVE_RESTORE_THD   =428;
    ERROR_VIO_IN_BG                 =429;
    ERROR_VIO_ILLEGAL_DURING_POPUP  =430;
    ERROR_VIO_LOCK                  =434;
    ERROR_VIO_INVALID_HANDLE        =436;
    ERROR_VIO_ILLEGAL_DURING_LOCK   =437;
    ERROR_VIO_INVALID_LENGTH        =438;
    ERROR_VIO_DETACHED              =465;
    ERROR_VIO_FONT                  =467;
    ERROR_VIO_USER_FONT             =468;
    ERROR_VIO_BAD_CP                =469;
    ERROR_VIO_NO_CP                 =470;
    ERROR_VIO_NA_CP                 =471;
    ERROR_VIO_INTERNAL_RESOURCE     =479;
    ERROR_VIO_SHELL_INIT            =480;
    ERROR_VIO_TRANSPARENT_POPUP     =483;
    ERROR_VIO_BAD_RESERVE           =486;
    ERROR_VIO_EXTENDED_SG           =494;
    ERROR_VIO_NOT_PRES_MGR_SG       =495;
    ERROR_VIO_SHIELD_OWNED          =496;
    ERROR_VIO_NO_MORE_HANDLES       =497;
    ERROR_VIO_SEE_ERROR_LOG         =498;
    ERROR_VIO_ASSOCIATED_DC         =499;

{severity codes}
    SEVERITY_NOERROR        =$0000;
    SEVERITY_WARNING        =$0004;
    SEVERITY_ERROR          =$0008;
    SEVERITY_SEVERE         =$000C;
    SEVERITY_UNRECOVERABLE  =$0010;

{base component error values}
    WINERR_BASE =$1000;    {Window Manager}
    GPIERR_BASE =$2000;    {Graphics Presentation Interface}
    DEVERR_BASE =$3000;    {Device Manager}
    SPLERR_BASE =$4000;    {Spooler}


{first parameter registration constants}
    VR_VIOGETCURPOS     =$00000001;
    VR_VIOGETCURTYPE    =$00000002;
    VR_VIOGETMODE       =$00000004;
    VR_VIOGETBUF        =$00000008;
    VR_VIOGETPHYSBUF    =$00000010;
    VR_VIOSETCURPOS     =$00000020;
    VR_VIOSETCURTYPE    =$00000040;
    VR_VIOSETMODE       =$00000080;
    VR_VIOSHOWBUF       =$00000100;
    VR_VIOREADCHARSTR   =$00000200;
    VR_VIOREADCELLSTR   =$00000400;
    VR_VIOWRTNCHAR      =$00000800;
    VR_VIOWRTNATTR      =$00001000;
    VR_VIOWRTNCELL      =$00002000;
    VR_VIOWRTTTY        =$00004000;
    VR_VIOWRTCHARSTR    =$00008000;
    VR_VIOWRTCHARSTRATT =$00010000;
    VR_VIOWRTCELLSTR    =$00020000;
    VR_VIOSCROLLUP      =$00040000;
    VR_VIOSCROLLDN      =$00080000;
    VR_VIOSCROLLLF      =$00100000;
    VR_VIOSCROLLRT      =$00200000;
    VR_VIOSETANSI       =$00400000;
    VR_VIOGETANSI       =$00800000;
    VR_VIOPRTSC         =$01000000;
    VR_VIOSCRLOCK       =$02000000;
    VR_VIOSCRUNLOCK     =$04000000;
    VR_VIOSAVREDRAWWAIT =$08000000;
    VR_VIOSAVREDRAWUNDO =$10000000;
    VR_VIOPOPUP         =$20000000;
    VR_VIOENDPOPUP      =$40000000;
    VR_VIOPRTSCTOGGLE   =$80000000;

{second parameter registration constants}
    VR_VIOMODEWAIT      =$00000001;
    VR_VIOMODEUNDO      =$00000002;
    VR_VIOGETFONT       =$00000004;
    VR_VIOGETCONFIG     =$00000008;
    VR_VIOSETCP         =$00000010;
    VR_VIOGETCP         =$00000020;
    VR_VIOSETFONT       =$00000040;
    VR_VIOGETSTATE      =$00000080;
    VR_VIOSETSTATE      =$00000100;

{constants for TVioModeInfo.Color}
    COLORS_2    =$0001;
    COLORS_4    =$0002;
    COLORS_16   =$0004;

{constants for TVioModeInfo.fbType}
    VGMT_OTHER          =$01;
    VGMT_GRAPHICS       =$02;
    VGMT_DISABLEBURST   =$04;

{constants for CharType in VioCheckCharType}
    VCC_SBCSCHAR        =0;
    VCC_DBCSFULLCHAR    =1;
    VCC_DBCS1STHALF     =2;
    VCC_DBCS2NDHALF     =3;

{constants for Mode in VioGetAnsi/VioSetAnsi}
    ANSI_ON     =1;
    ANSI_OFF    =0;

{constants for RequestType in VioSavRedrawWait}
    VSRWI_SAVEANDREDRAW =0;
    VSRWI_REDRAW        =1;

{constants for NotifyType in VioSavRedrawWait}
    VSRWN_SAVE          =0;
    VSRWN_REDRAW        =1;

{constants for Ownership in VioSavRedrawUndo}
    UNDOI_GETOWNER      =0;
    UNDOI_RELEASEOWNER  =1;

{constants for KillThread in VioSavRedrawUndo}
    UNDOK_ERRORCODE     =0;
    UNDOK_TERMINATE     =1;

    VMWR_POPUP  =0;
    VMWN_POPUP  =0;

{constants for WaitFlag in VioScrLock}
    LOCKIO_NOWAIT   =0;
    LOCKIO_WAIT     =1;

{constants for Status in VioScrLock}
    LOCK_SUCCESS    =0;
    LOCK_FAIL       =1;

{constants for OptionFlags in VioPopup}
    VP_NOWAIT       =$0000;
    VP_WAIT         =$0001;
    VP_OPAQUE       =$0000;
    VP_TRANSPARENT  =$0002;

{constants for TVioConfigInfo.Adapter}
    DISPLAY_MONOCHROME     =$0000;
    DISPLAY_CGA            =$0001;
    DISPLAY_EGA            =$0002;
    DISPLAY_VGA            =$0003;
    DISPLAY_8514A          =$0007;
    DISPLAY_IMAGEADAPTER   =$0008;
    DISPLAY_XGA            =$0009;

{constants for TVioConfigInfo.Display}
    MONITOR_MONOCHROME     =$0000;
    MONITOR_COLOR          =$0001;
    MONITOR_ENHANCED       =$0002;
    MONITOR_8503           =$0003;
    MONITOR_851X_COLOR     =$0004;
    MONITOR_8514           =$0009;
    MONITOR_FLATPANEL      =$000A;
    MONITOR_8507_8604      =$000B;
    MONITOR_8515           =$000C;
    MONITOR_9515           =$000F;
    MONITOR_9517           =$0011;
    MONITOR_9518           =$0012;

{constants for TVioConfigInfo.Configuration, TVioSetTarget.DefaultAlgorithm
and usConfigID in VioGetConfig}
    VIO_CONFIG_CURRENT     =0;
    VIO_CONFIG_PRIMARY     =1;
    VIO_CONFIG_SECONDARY   =2;

{constants for TVioFontInfo.rType}
    VGFI_GETCURFONT        =0;
    VGFI_GETROMFONT        =1;

{constants for TFAttrs.fsSelection}
    FATTR_SEL_ITALIC        =$0001;
    FATTR_SEL_UNDERSCORE    =$0002;
    FATTR_SEL_OUTLINE       =$0008;
    FATTR_SEL_STRIKEOUT     =$0010;
    FATTR_SEL_BOLD          =$0020;

{constants for TFAttrs.fsType}
    FATTR_TYPE_KERNING      =$0004;
    FATTR_TYPE_MBCS         =$0008;
    FATTR_TYPE_DBCS         =$0010;
    FATTR_TYPE_ANTIALIASED  =$0020;

{constants for TFAttrs.fsFontUse}
    FATTR_FONTUSE_NOMIX         =$0002;
    FATTR_FONTUSE_OUTLINE       =$0004;
    FATTR_FONTUSE_TRANSFORMABLE =$0008;

{size for fields in the font records}
    FACESIZE    =32;

{constants for TFontMetrics.fsType}
    FM_TYPE_FIXED       =$0001;
    FM_TYPE_LICENSED    =$0002;
    FM_TYPE_KERNING     =$0004;
    FM_TYPE_DBCS        =$0010;
    FM_TYPE_MBCS        =$0018;
    FM_TYPE_64K         =$8000;
    FM_TYPE_ATOMS       =$4000;
    FM_TYPE_FAMTRUNC    =$2000;
    FM_TYPE_FACETRUNC   =$1000;

{constants for TFontMetrics.fsDefn}
    FM_DEFN_OUTLINE =$0001;
    FM_DEFN_IFI     =$0002;
    FM_DEFN_WIN     =$0004;
    FM_DEFN_GENERIC =$8000;

{constants for TFontMetrics.fsSelection}
    FM_SEL_ITALIC           =$0001;
    FM_SEL_UNDERSCORE       =$0002;
    FM_SEL_NEGATIVE         =$0004;
    FM_SEL_OUTLINE          =$0008;    { Hollow Outline Font }
    FM_SEL_STRIKEOUT        =$0010;
    FM_SEL_BOLD             =$0020;
    FM_SEL_ISO9241_TESTED   =$0040;
{ISO 9241 is an international standard covering health and safety
in the work place for users of visual display terminals. Part 3 of
this standard covers clarity and legibility of text displayed on
computer screens, it places requirements on minimum sizes and
luminance contrast. The presence of FM_SEL_ISO9241_TESTED flag in the
font metrics indicates that the font has been tested for compliance
to the standard. The FM_ISO_XXX flags indicate the results of the
test on the IBM 9515, 9517 and 9518 color displays at the supported
dimensions of 640x480 and 1024x768. To ensure compliance the
sXDeviceRes and sYDeviceRes must also match the target display
resolution.}

{constants for TPanose.fbPassedISO and TPanose.fbFailedISO}
    FM_ISO_9518_640     =$01;
    FM_ISO_9515_640     =$02;
    FM_ISO_9515_1024    =$04;
    FM_ISO_9517_640     =$08;
    FM_ISO_9517_1024    =$10;

{constant for TFontMetrics.fsCapabilities}
    FM_CAP_NOMIX    =$0001;


type
{unnecessary, just FYI}
    THVio=word;
    PHVio=^THVio;
    THVPS=word;
    PHVPS=^THVPS;

    TQWord=record
        Lo:cardinal;
        Hi:cardinal;
    end;
    PQWord=^TQWord;

{Record type for VioSetCurType/VioGetCurType; the second variant makes the use
 of percentage-based (negative) and hidden cursor type (-1) specification
 a bit easier}
    TVioCursorInfo=record
        case boolean of
        false:(
        yStart:word;    {Cursor start (top) scan line (0-based)}
        cEnd:word;      {Cursor end (bottom) scan line}
        cx:word;        {Cursor width (0=default width)}
        Attr:word);     {Cursor colour attribute (-1=hidden)}
        true:(
        yStartInt: integer;
        cEndInt:integer;
        cxInt:integer;
        AttrInt:integer);
    end;
    PVioCursorInfo=^TVioCursorInfo;
    VioCursorInfo=TVioCursorInfo;

{Record type for VioSetMode/GetMode}
    TVioModeInfo=record
        cb:word;                    {Size of the record}
        case boolean of
        false:(
        fbType,                     {8-bit mask identifying the mode}
                                    {- see VGMT_* constants         }
        Color:byte;                 {Number of colour bits available}
                                    {(1=>2 colours, 2=>4,...) - see }
                                    {COLORS_* constants             }
        Col,                        {Number of text character columns}
        Row,                        {Number of text character rows}
        HRes,                       {Display width in pixels}
        VRes:word;                  {Display height in pixels}
        fmt_ID,                     {Format of the attributes}
        Attrib:byte;                {Number of attributes in fmt_ID field}
        Buf_Addr,                   {Address of the physical display buffer}
        Buf_Length,                 {Length of the physical display buffer}
        Full_Length,                {Size of the buffer needed to save}
                                    {the whole physical buffer        }
        Partial_Length:cardinal;    {Size of the buffer needed to save}
                                    {the part of the physical buffer  }
                                    {overwritten with VioPopup        }
        Ext_Data_Addr:pointer);     {Address of an extended-mode data}
        true:(
        fbType2,                (* should be fbType, Color, etc., but this *)
        Color2:char;            (* construct is unsupported currently      *)
        Col2,
        Row2,
        HRes2,
        VRes2:word;
        fmt_ID2,
        Attrib2:char);
    end;
    PVioModeInfo=^TVioModeInfo;
    VioModeInfo=TVioModeInfo;

{record type for VioGetPhysBuf}
    TVioPhysBuf=record
        pBuf:pointer;       {Absolute screen address}
        cb:cardinal;        {Length of the buffer in bytes}
        case boolean of
        false:(Sel:word);   {Selector for video access}
        true:(aSel:array[0..0] of word);
    end;
    PVioPhysBuf=^TVioPhysBuf;
    VioPhysBuf=TVioPhysBuf;

{record type for VioGetConfig}
(*   #pragma pack(2) ??? *)
    type
        TVioConfigInfo=record
            cb:word;                {Size of the record}
            Adapter:word;           {Adapter type (see DISPLAY_* constants)}
            Display:word;           {Display type (see MONITOR_* constants)}
            cbMemory:cardinal;      {Amount of RAM in bytes on the adapter}
            Configuration:word;     {Configuration ID (see  }
                                    {VIO_CONFIG_* constants)}
            VDHVersion:word;        {Reserved, set to zero}
            Flags:word;             {Flags; 1 sets the default}
                                    {power-on configuration   }
            HWBufferSize:cardinal;  {Size of the buffer needed to save}
                                    {the full adapter state (not      }
                                    {including the physical buffer)   }
            FullSaveSize:cardinal;  {Size of the buffer needed to}
                                    {save the full adapter state }
            PartSaveSize:cardinal;  {Size of the buffer needed to save}
                                    {the part of the physical buffer  }
                                    {overwritten with VioPopup        }
            EmAdaptersOff:word;     {Offset of the information   }
                                    {about emulated adapter types}
            EmDisplaysOff:word;     {Offset of the information   }
                                    {about emulated display types}
       end;
       PVioConfigInfo=^TVioConfigInfo;
       VioConfigInfo=TVioConfigInfo;

{record type for VioGetFont/VioSetFont}
    TVioFontInfo=record
        cb:word;            {Size of the data record}
        case byte of
        1:(
        rType,              {Request type}
        cxCell,             {Columns per cell (cell width)}
        cyCell:word;        {Rows per cell (cell height)}
        pbData:pointer;     {Address of caller's buffer}
        cbData:word);       {Size of caller's buffer in bytes}
        2:(
        aType,
        cxCell2,
        cyCell2:word;
        pbData2:longint);   (* should be pbData, but this construct *)
        3:(_type:word);     (* is not supported currently           *)
    end;
    PVioFontInfo=^TVioFontInfo;
    VioFontInfo=TVioFontInfo;  (* *)

{record types for VioGetState/VioSetState}
    TVioPalState=record
        cb:word;                        {Size of the record}
        rtype:word;                     {0=palette}
        iFirst:word;                    {The first register}
        AColor:array[0..15] of word;    {Up to 16 register values}
    end;
    PVioPalState=^TVioPalState;
    VioPalState=TVioPalState;

    TVioOverscan=record
        cb:word;    {Size of the record}
        rType:word; {1=border colour}
        Color:word; {The colour of the border area}
    end;
    PVioOverscan=^TVioOverscan;
    VioOverScan=TVioOverScan;

    TVioIntensity=record
        cb:word;    {Size of the record}
        rType:word; {2=blink/bold settings}
        fs:word;    {The flink/bold background switch}
    end;
    PVioIntensity=^TVioIntensity;
    VioIntensity=TVioIntensity;

    TVioColorReg=record
        cb:word;                {Size of the record}
        rType:word;             {3=colour registers}
        FirstColorReg:word;     {The first colour register}
        NumColorRegs:word;      {Number of colour registers}
        ColorRegAddr:pointer;   {pointer to an array with colour values}
    end;
    PVioColorReg=^TVioColorReg;
    VioColorReg=TVioColorReg;

    TVioSetULineLoc=record
        cb:word;        {Size of the record}
        rType:word;     {5=underline}
        ScanLine:word;  {Location of the underline (32=no underline)}
    end;
    PVioSetULineLoc=^TVioSetULineLoc;
    VioSetULineLoc=TVioSetULineLoc;

    TVioSetTarget=record
        cb:word;                {Size of the record}
        rType:word;             {6=target for VioSetMode}
        DefaultAlgorithm:word;  {Default/primary/secondary   }
                                {(see VIO_CONFIG_* constants)}
    end;
    PVioSetTarget=^TVioSetTarget;
    VioSetTarget=TVioSetTarget;

    TStr8=array[0..7] of char;
    PStr8=^TStr8;

{font record type for Vio/GpiCreateLogFont}
    TFAttrs=record
        usRecordLength:word;
        fsSelection:word;
        lMatch:longint;
        szFacename:array[0..FACESIZE-1] of char;
        idRegistry:word;
        usCodePage:word;
        lMaxBaselineExt:longint;
        lAveCharWidth:longint;
        fsType:word;
        fsFontUse:word;
    end;
    PFAttrs=^TFAttrs;
    FAttrs=TFAttrs;

{font metrics returned by GpiQueryFonts and others}
    TPanose=record
        bFamilyType:byte;
        bSerifStyle:byte;
        bWeight:byte;
        bProportion:byte;
        bContrast:byte;
        bStrokeVariation:byte;
        bArmStyle:byte;
        bLetterform:byte;
        bMidline:byte;
        bXHeight:byte;
        fbPassedISO:byte;
        fbFailedISO:byte;
    end;
    PPanose=^TPanose;

    TFontMetrics=record
        szFamilyname:array[0..FACESIZE-1] of char;
        szFacename:array[0..FACESIZE-1] of char;
        idRegistry:word;
        usCodePage:word;
        lEmHeight:longint;
        lXHeight:longint;
        lMaxAscender:longint;
        lMaxDescender:longint;
        lLowerCaseAscent:longint;
        lLowerCaseDescent:longint;
        lInternalLeading:longint;
        lExternalLeading:longint;
        lAveCharWidth:longint;
        lMaxCharInc:longint;
        lEmInc:longint;
        lMaxBaselineExt:longint;
        sCharSlope:longint;
        sInlineDir:integer;
        sCharRot:integer;
        usWeightClass:word;
        usWidthClass:word;
        sXDeviceRes:integer;
        sYDeviceRes:integer;
        sFirstChar:integer;
        sLastChar:integer;
        sDefaultChar:integer;
        sBreakChar:integer;
        sNominalPointSize:integer;
        sMinimumPointSize:integer;
        sMaximumPointSize:integer;
        fsType:word;
        fsDefn:word;
        fsSelection:word;
        fsCapabilities:word;
        lSubscriptXSize:longint;
        lSubscriptYSize:longint;
        lSubscriptXOffset:longint;
        lSubscriptYOffset:longint;
        lSuperscriptXSize:longint;
        lSuperscriptYSize:longint;
        lSuperscriptXOffset:longint;
        lSuperscriptYOffset:longint;
        lUnderscoreSize:longint;
        lUnderscorePosition:longint;
        lStrikeoutSize:longint;
        lStrikeoutPosition:longint;
        sKerningPairs:integer;
        sFamilyClass:integer;
        lMatch:longint;
        FamilyNameAtom:longint;
        FaceNameAtom:longint;
        Panose:TPanose;
    end;
    PFontMetrics=^TFontMetrics;
    FontMetrics=TFontMetrics;


function VioRegister(ModuleName,ProcName:PChar;FnMask1,FnMask2:cardinal):word;
                                                                         cdecl;

function VioRegister(ModuleName,ProcName:string;FnMask1,FnMask2:cardinal):word;

function VioGlobalReg(ModuleName,ProcName:PChar;FnMask1,FnMask2:cardinal;
                                                      Return:word):word; cdecl;

function VioGlobalReg(ModuleName,ProcName:string;FnMask1,FnMask2:cardinal;
                                                             Return:word):word;

function VioDeRegister:word; cdecl;

function VioGetBuf(var LVBAddr:pointer;var LVBLength:word;VioHandle:word):word;
                                                                         cdecl;

function VioGetCurPos(var Row,Column:word;VioHandle:word):word; cdecl;

function VioSetCurPos(Row,Column,VioHandle:word):word; cdecl;

function VioGetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;

function VioSetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;

function VioGetMode(var Mode:TVioModeInfo;VioHandle:word):word; cdecl;

function VioSetMode(var Mode:TVioModeInfo;VioHandle:word):word; cdecl;

function VioGetPhysBuf(var PBData:TVioPhysBuf;Reserved:word):word; cdecl;

function VioReadCellStr(var Buf;var BufLen:word;
                                        Row,Column,VioHandle:word):word; cdecl;

function VioReadCharStr(var Buf;var BufLen:word;
                                        Row,Column,VioHandle:word):word; cdecl;

function VioWrtCellStr(CellStr:pointer;Len,Row,Column,VioHandle:word):word;
                                                                         cdecl;

function VioWrtCharStr(CharStr:pointer;Len,Row,Column,VioHandle:word):word;
                                                                         cdecl;

function VioScrollDn(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollDown(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollUp(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollLf(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollLeft(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollRt(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioScrollRight(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;

function VioWrtNAttr(var Attr:byte;Times,Row,Column,VioHandle:word):word;
                                                                         cdecl;

function VioWrtNCell(var Cell:word;Times,Row,Column,VioHandle:word):word;
                                                                         cdecl;

function VioWrtNChar(var Ch:byte;Times,Row,Column,VioHandle:word):word; cdecl;

function VioWrtNChar(var Ch:char;Times,Row,Column,VioHandle:word):word; cdecl;

function VioWrtTTY(CharStr:pointer;Len,VioHandle:word):word; cdecl;

function VioWrtCharStrAtt(CharStr:pointer;Len,Row,Column:word;var Attr:byte;
                                                   VioHandle:word):word; cdecl;

function VioCheckCharType(var CharType:word;Row,Column,VioHandle:word):word;
                                                                         cdecl;

function VioShowBuf(BufOfs,Len,VioHandle:word):word; cdecl;

function VioSetAnsi(Mode,VioHandle:word):word; cdecl;

function VioGetAnsi(var Mode:word;VioHandle:word):word; cdecl;

function VioPrtSc(VioHandle:word):word; cdecl;

function VioPrtScToggle(VioHandle:word):word; cdecl;

(*
function VioRedrawSize(var RedrawSize:cardinal):word;
*)

function VioSavRedrawWait(RequestType:word;var NotifyType:word;
                                                    Reserved:word):word; cdecl;

function VioSavRedrawUndo(Ownership,KillThread,Reserved:word):word; cdecl;

function VioModeWait(RequestType:word;var NotifyType:word;Reserved:word):word;
                                                                         cdecl;

function VioModeUndo(Ownership,KillThread,Reserved:word):word; cdecl;

function VioScrLock(WaitFlag:word;var Status:word;VioHandle:word):word; cdecl;

function VioScrUnLock(VioHandle:word):word; cdecl;

function VioPopUp(var OptionFlags:word;VioHandle:word):word; cdecl;

function VioEndPopUp(VioHandle:word):word; cdecl;

function VioGetConfig(ConfigId:word;var VideoConfig:TVioConfigInfo;
                                                   VioHandle:word):word; cdecl;

function VioGetFont(var FontData:TVioFontInfo;VioHandle:word):word; cdecl;

function VioSetFont(var FontData:TVioFontInfo;VioHandle:word):word; cdecl;

function VioGetCp(Reserved:word;var CodePage:word;VioHandle:word):word; cdecl;

function VioSetCp(Reserved:word;CodePage:word;VioHandle:word):word; cdecl;

function VioGetState(var State;VioHandle:word):word; cdecl;

function VioSetState(var State;VioHandle:word):word; cdecl;

function VioAssociate(DC:cardinal;VPS:word):word; cdecl;

function VioCreateLogFont(var FAtAttrs:TFAttrs;LLCId:longint;var Name:TStr8;
                                                         VPS:word):word; cdecl;

function VioCreatePS(var VPS:word;Depth,Width,Format,Attrs:integer;
                                                    Reserved:word):word; cdecl;

function VioDeleteSetId(LLCId:longint;VPS:word):word; cdecl;

function VioDestroyPS(VPS:word):word; cdecl;

function VioGetDeviceCellSize(var Height,Width:integer;VPS:word):word; cdecl;

function VioGetOrg(var Row,Column:integer;VPS:word):word; cdecl;

function VioQueryFonts(var Remfonts:longint;var fmMetrics:TFontMetrics;
                MetricsLength:longint;var Fonts:longint;FaceName:PChar;
                                      flOptions:cardinal;VPS:word):word; cdecl;

function VioQueryFonts(var Remfonts:longint;var fmMetrics:TFontMetrics;
                MetricsLength:longint;var Fonts:longint;FaceName:string;
                                             flOptions:cardinal;VPS:word):word;

function VioQuerySetIds(var allCIds:longint;var Names:TStr8;
                       var alTypes:longint;Count:longint;VPS:word):word; cdecl;

function VioSetDeviceCellSize(Height,Width:integer;VPS:word):word; cdecl;

function VioSetOrg(Row,Column:integer;VPS:word):word; cdecl;

function VioShowPS(Depth,Width,offCell:integer;VPS:word):word; cdecl;

{Default message processing for AVio PS's - imported from PMVIOP.DLL}
function WinDefAVioWindowProc(WND:cardinal;Msg:word;mp1,mp2:cardinal):pointer;
                                                                         cdecl;


(* Following routines are not supported
   (just have a look in some C header
   file - you probably won't find it there either).
VioFree (index 4)
Avs_Prtsc (index 14)
VioSrfUnblock (index 16)
VioSrfBlock (index 17)
VioSave (index 20)
VioHetInit (index 34)
VioSswSwitch (index 36)
Avs_PrtscToggle (index 38)
VioInit (index 39)
VioRestore (index 41)
VioShellInit (index 54)
VioGetPSAddress (index 67)
VioQueryConsole (index 68)
XVioSetCAState (index 71)
XVioCheckCharType (index 72)
XVioDestroyCA (index 73)
XVioCreateCA (index 74)
XVioGetCAState (index 76)
*)


{***************************************************************************}
implementation
{***************************************************************************}


function VioRegister(ModuleName,ProcName:PChar;FnMask1,FnMask2:cardinal):word;
                                                                         cdecl;
external 'EMXWRAP' index 145;
{external 'VIOCALLS' index 45;}

function VioRegister(ModuleName,ProcName:string;FnMask1,FnMask2:cardinal):word;
begin
    if byte(ModuleName[0])>8 then byte(ModuleName[0]):=8;
    ModuleName[Succ(byte(ModuleName[0]))]:=#0;
    if byte(ProcName[0])>32 then byte(ProcName[0]):=32;
    ProcName[Succ(byte(ProcName[0]))]:=#0;
    VioRegister:=VioRegister(@ModuleName[1],@ProcName[1],FnMask1,FnMask2);
end;

function VioGlobalReg(ModuleName,ProcName:PChar;FnMask1,FnMask2:cardinal;
                                                      Return:word):word; cdecl;
external 'EMXWRAP' index 170;
{external 'VIOCALLS' index 70;}

function VioGlobalReg(ModuleName,ProcName:string;FnMask1,FnMask2:cardinal;
                                                             Return:word):word;
begin
    if byte(ModuleName[0])>8 then byte(ModuleName[0]):=8;
    ModuleName[Succ(byte(ModuleName[0]))]:=#0;
    if byte(ProcName[0])>32 then byte(ProcName[0]):=32;
    ProcName[Succ(byte(ProcName[0]))]:=#0;
    VioGlobalReg:=VioGlobalReg(@ModuleName[1],@ProcName[1],FnMask1,FnMask2,
                                                                       Return);
end;

function VioDeRegister:word; cdecl;
external 'EMXWRAP' index 106;
{external 'VIOCALLS' index 6;}

function VioGetBuf(var LVBAddr:pointer;var LVBLength:word;VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 131;
{external 'VIOCALLS' index 31;}

function VioGetCurPos(var Row,Column:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 109;
{external 'VIOCALLS' index 9;}

function VioSetCurPos(Row,Column,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 115;
{external 'VIOCALLS' index 15;}

function VioGetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 127;
{external 'VIOCALLS' index 27;}

function VioSetCurType(var CurData:TVioCursorInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 132;
{external 'VIOCALLS' index 32;}

function VioGetMode(var Mode:TVioModeInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 121;
{external 'VIOCALLS' index 21;}

function VioSetMode(var Mode:TVioModeInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 122;
{external 'VIOCALLS' index 22;}

function VioGetPhysBuf(var PBData:TVioPhysBuf;Reserved:word):word; cdecl;
external 'EMXWRAP' index 102;
{external 'VIOCALLS' index 2;}

function VioReadCellStr(var Buf;var BufLen:word;
                                        Row,Column,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 124;
{external 'VIOCALLS' index 24;}

function VioReadCharStr(var Buf;var BufLen:word;
                                        Row,Column,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 130;
{external 'VIOCALLS' index 30;}

function VioWrtCellStr(CellStr:pointer;Len,Row,Column,VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 110;
{external 'VIOCALLS' index 10;}

function VioWrtCharStr(CharStr:pointer;Len,Row,Column,VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 113;
{external 'VIOCALLS' index 13;}

function VioScrollDn(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 147;
{external 'VIOCALLS' index 47;}

function VioScrollDown(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 147;
{external 'VIOCALLS' index 47;}

function VioScrollUp(TopRow,LeftCol,BotRow,RightCol,Lines:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 107;
{external 'VIOCALLS' index 7;}

function VioScrollLf(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 144;
{external 'VIOCALLS' index 44;}

function VioScrollLeft(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 144;
{external 'VIOCALLS' index 44;}

function VioScrollRt(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 112;
{external 'VIOCALLS' index 12;}

function VioScrollRight(TopRow,LeftCol,BotRow,RightCol,Col:word;var Cell:word;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 112;
{external 'VIOCALLS' index 12;}

function VioWrtNAttr(var Attr:byte;Times,Row,Column,VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 126;
{external 'VIOCALLS' index 26;}

function VioWrtNCell(var Cell:word;Times,Row,Column,VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 152;
{external 'VIOCALLS' index 52;}

function VioWrtNChar(var Ch:byte;Times,Row,Column,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 153;
{external 'VIOCALLS' index 53;}

function VioWrtNChar(var Ch:char;Times,Row,Column,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 153;
{external 'VIOCALLS' index 53;}

function VioWrtTTY(CharStr:pointer;Len,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 119;
{external 'VIOCALLS' index 19;}

function VioWrtCharStrAtt(CharStr:pointer;Len,Row,Column:word;var Attr:byte;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 148;
{external 'VIOCALLS' index 48;}

function VioCheckCharType(var CharType:word;Row,Column,VioHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 175;
{external 'VIOCALLS' index 75;}

function VioShowBuf(BufOfs,Len,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 143;
{external 'VIOCALLS' index 43;}

function VioSetAnsi(Mode,VioHandle:word):word; cdecl;
external 'EMXWRAP' index 105;
{external 'VIOCALLS' index 5;}

function VioGetAnsi(var Mode:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 103;
{external 'VIOCALLS' index 3;}

function VioPrtSc(VioHandle:word):word; cdecl;
external 'EMXWRAP' index 108;
{external 'VIOCALLS' index 8;}

function VioPrtScToggle(VioHandle:word):word; cdecl;
external 'EMXWRAP' index 150;
{external 'VIOCALLS' index 50;}

(*
function VioRedrawSize(var RedrawSize:cardinal):word;
!!!not defined in EMXWRAP.DLL!!!
{external 'VIOCALLS' index 69;}
*)

function VioSavRedrawWait(RequestType:word;var NotifyType:word;
                                                    Reserved:word):word; cdecl;
external 'EMXWRAP' index 125;
{external 'VIOCALLS' index 25;}

function VioSavRedrawUndo(Ownership,KillThread,Reserved:word):word; cdecl;
external 'EMXWRAP' index 128;
{external 'VIOCALLS' index 28;}

function VioModeWait(RequestType:word;var NotifyType:word;Reserved:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 137;
{external 'VIOCALLS' index 37;}

function VioModeUndo(Ownership,KillThread,Reserved:word):word; cdecl;
external 'EMXWRAP' index 135;
{external 'VIOCALLS' index 35;}

function VioScrLock(WaitFlag:word;var Status:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 123;
{external 'VIOCALLS' index 23;}

function VioScrUnLock(VioHandle:word):word; cdecl;
external 'EMXWRAP' index 118;
{external 'VIOCALLS' index 18;}

function VioPopUp(var OptionFlags:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 111;
{external 'VIOCALLS' index 11;}

function VioEndPopUp(VioHandle:word):word; cdecl;
external 'EMXWRAP' index 101;
{external 'VIOCALLS' index 1;}

function VioGetConfig(ConfigId:word;var VideoConfig:TVioConfigInfo;
                                                   VioHandle:word):word; cdecl;
external 'EMXWRAP' index 146;
{external 'VIOCALLS' index 46;}

function VioGetFont(var FontData:TVioFontInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 129;
{external 'VIOCALLS' index 29;}

function VioSetFont(var FontData:TVioFontInfo;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 133;
{external 'VIOCALLS' index 33;}

function VioGetCp(Reserved:word;var CodePage:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 140;
{external 'VIOCALLS' index 40;}

function VioSetCp(Reserved:word;CodePage:word;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 142;
{external 'VIOCALLS' index 42;}

function VioGetState(var State;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 149;
{external 'VIOCALLS' index 49;}

function VioSetState(var State;VioHandle:word):word; cdecl;
external 'EMXWRAP' index 151;
{external 'VIOCALLS' index 51;}


{Extended functions for windowed VIO follow.}
function VioAssociate(DC:cardinal;VPS:word):word; cdecl;
external 'EMXWRAP' index 155;
{external 'VIOCALLS' index 55;}

function VioCreateLogFont(var FAtAttrs:TFAttrs;LLCId:longint;var Name:TStr8;
                                                         VPS:word):word; cdecl;
external 'EMXWRAP' index 160;
{external 'VIOCALLS' index 60;}

function VioCreatePS(var VPS:word;Depth,Width,Format,Attrs:integer;
                                                    Reserved:word):word; cdecl;
external 'EMXWRAP' index 156;
{external 'VIOCALLS' index 56;}

function VioDeleteSetId(LLCId:longint;VPS:word):word; cdecl;
external 'EMXWRAP' index 157;
{external 'VIOCALLS' index 57;}

function VioDestroyPS(VPS:word):word; cdecl;
external 'EMXWRAP' index 161;
{external 'VIOCALLS' index 61;}

function VioGetDeviceCellSize(var Height,Width:integer;VPS:word):word; cdecl;
external 'EMXWRAP' index 158;
{external 'VIOCALLS' index 58;}

function VioGetOrg(var Row,Column:integer;VPS:word):word; cdecl;
external 'EMXWRAP' index 159;
{external 'VIOCALLS' index 59;}

function VioQueryFonts(var Remfonts:longint;var fmMetrics:TFontMetrics;
                MetricsLength:longint;var Fonts:longint;FaceName:PChar;
                                      flOptions:cardinal;VPS:word):word; cdecl;
external 'EMXWRAP' index 164;
{external 'VIOCALLS' index 64;}

function VioQueryFonts(var Remfonts:longint;var fmMetrics:TFontMetrics;
                MetricsLength:longint;var Fonts:longint;FaceName:string;
                                             flOptions:cardinal;VPS:word):word;

var B:byte;

begin
    B:=byte(FaceName[0]);
    if B=0 then VioQueryFonts:=VioQueryFonts(RemFonts,fmMetrics,MetricsLength,
                                                  Fonts,nil,flOptions,VPS) else
    begin
        if B<>255 then
        begin
            FaceName[Succ(B)]:=#0;
            VioQueryFonts:=VioQueryFonts(RemFonts,fmMetrics,MetricsLength,
                                             Fonts,@FaceName[1],flOptions,VPS);
        end else
        begin
            Move(FaceName[1],FaceName[0],B);
            FaceName[B]:=#0;
            VioQueryFonts:=VioQueryFonts(RemFonts,fmMetrics,MetricsLength,
                                                Fonts,@FaceName,flOptions,VPS);
        end;
    end;
end;

function VioQuerySetIds(var allCIds:longint;var Names:TStr8;
                       var alTypes:longint;Count:longint;VPS:word):word; cdecl;
external 'EMXWRAP' index 162;
{external 'VIOCALLS' index 62;}

function VioSetDeviceCellSize(Height,Width:integer;VPS:word):word; cdecl;
external 'EMXWRAP' index 165;
{external 'VIOCALLS' index 65;}

function VioSetOrg(Row,Column:integer;VPS:word):word; cdecl;
external 'EMXWRAP' index 163;
{external 'VIOCALLS' index 63;}

function VioShowPS(Depth,Width,offCell:integer;VPS:word):word; cdecl;
external 'EMXWRAP' index 166;
{external 'VIOCALLS' index 66;}

function WinDefAVioWindowProc(WND:cardinal;Msg:word;mp1,mp2:cardinal):pointer;
                                                                         cdecl;
external 'EMXWRAP' index 30;
{external 'PMVIOP' index 30;}

end.
