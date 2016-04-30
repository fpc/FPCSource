{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    MUI functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$packrecords C}
unit mui;

interface

{   MUI - MagicUserInterface
   Copyright (C) 1992-2006 by Stefan Stuntz <stefan@stuntz.com>
 Copyright (C) 2006-2016 by Thore Boeckelmann, Jens Maus

 Main Header File

************************************************************************
 Class Tree
************************************************************************

 rootclass                   (BOOPSI's base class)
 +--Notify                   (implements notification mechanism)
 !  +--Family                (handles multiple children)
 !  !  +--Menustrip          (describes a complete menu strip)
 !  !  +--Menu               (describes a single menu)
 !  !  \--Menuitem           (describes a single menu item)
 !  +--Application           (main class for all applications)
 !  +--Window                (main class for all windows)
 !  !  \--Aboutmui           (About window of MUI preferences)
 !  \--Area                  (base class for all GUI elements)
 !     +--Dtpic              (datatypes based bitmaps)
 !     +--Rectangle          (spacing object)
 !     +--Balance            (balancing separator bar)
 !     +--Image              (image display)
 !     +--Bitmap             (draws bitmaps)
 !     !  \--Bodychunk       (makes bitmap from ILBM body chunk)
 !     +--Pixmap             (draws raw image data)
 !     +--Text               (text display)
 !     +--Gadget             (base class for intuition gadgets)
 !     !  \--Boopsi          (interface to BOOPSI gadgets)
 !     +--Gauge              (fule gauge)
 !     +--Scale              (percentage scale)
 !     +--Colorfield         (field with changeable color)
 !     +--Numeric            (base class for slider gadgets)
 !     !  +--Knob            (turning knob)
 !     !  +--Levelmeter      (level display)
 !     !  +--Numericbutton   (space saving popup slider)
 !     !  \--Slider          (traditional slider)
 !     !     \--Prop         (proportional gadget)
 !     +--Pendisplay         (displays a pen specification)
 !     !  \--Poppen          (popup button to adjust a pen spec)
 !     +--String             (string gadget)
 !     \--Group              (groups other GUI elements)
 !        +--List            (line-oriented list)
 !        !  +--Floattext    (special list with floating text)
 !        !  +--Volumelist   (special list with volumes)
 !        !  +--Scrmodelist  (special list with screen modes)
 !        !  \--Dirlist      (special list with files)
 !        +--Title           (handles page groups with titles)
 !        +--Register        (handles page groups with titles)
 !        !  \--Penadjust    (group to adjust a pen)
 !        +--Virtgroup       (handles virtual groups)
 !        +--Scrollgroup     (virtual groups with scrollbars)
 !        +--Scrollbar       (traditional scrollbar)
 !        +--Listview        (listview)
 !        +--Radio           (radio button)
 !        +--Cycle           (cycle gadget)
 !        +--Coloradjust     (several gadgets to adjust a color)
 !        +--Palette         (complete palette gadget)
 !        \--Popstring       (base class for popup objects)
 !           +--Popobject    (popup aynthing in a separate window)
 !           !  +--Poplist   (popup a simple listview)
 !           !  \--Popscreen (popup a list of public screens)
 !           \--Popasl       (popup an asl requester)
 \--Semaphore                (semaphore equipped objects)
    +--Dataspace             (handles general purpose data spaces)
    +--Datamap               (handles general purpose data spaces)
    +--Objectmap             (handles general purpose data spaces)
    \--Process               (simplify handlig sub-tasks)

************************************************************************
 General Header File Information
************************************************************************
 All macro and structure definitions follow these rules:

 Name                     | Meaning
 -------------------------+-------------------------------------
 MUIC_<class>             | Name of a class
 MUIM_<class>_<method>    | Method
 MUIP_<class>_<method>    | Methods parameter structure
 MUIV_<class>_<method>_<x>| Special method value
 MUIA_<class>_<attrib>    | Attribute
 MUIV_<class>_<attrib>_<x>| Special attribute value
 MUIE_<error>             | Error return code from MUI_Error()
 MUII_<name>              | Standard MUI image
 MUIX_<code>              | Control codes for text strings
 MUIO_<name>              | Object type for MUI_MakeObject()

 MUIA_... attribute definitions are followed by a comment
 consisting of the three possible letters I, S and G.
 I: it's possible to specify this attribute at object creation time.
 S: it's possible to change this attribute with SetAttrs().
 G: it's possible to get this attribute with GetAttr().

 Items marked with "Custom Class" are for use in custom classes only!}

uses
  exec, intuition, utility, agraphics, iffparse;


const
  MUIMASTER_NAME: PChar = 'muimaster.library';
  MUIMASTER_VMIN        = 20;
  MUIMASTER_VLATEST     = 20;

  IMSPEC_EXTERNAL_PREFIX = 'MUI:Images/';

  MUIB_MUI  = TAG_USER;                // Base for legacy MUI identifiers
  MUIB_RSVD = MUIB_MUI  or $10400000;  // Base for AROS reserved range
  MUIB_ZUNE = MUIB_RSVD or $00020000;  // Base for Zune core reserved range
  MUIB_AROS = MUIB_RSVD or $00070000;  // Base for AROS core reserved range

  PST_MUI = 'm';
  PST_CMAP = 'p';
  PST_RGB = 'r';
  PST_SYS = 's';

// MUI_PenSpec is a an ascii spec like this:
//   'm5'   = mui pen #5
//   'p123' = cmap entry #123
//   'rFFFFFFFF,00000000,00000000' =rgb #FF0000
//   's3'   = system pen #3
//   It needs to be like this, because for example nlist has default penspecs in it's source encoded like above which it directly passes to MUI_ObtainBestPen
// Black box specification structures for images, pens, frames
type
  PMUI_PenSpec = ^TMUI_PenSpec;
  TMUI_PenSpec = record
    ps_buf: array[0..31] of char; // black box
  end;

  PMUI_FrameSpec = ^TMUI_FrameSpec;
  TMUI_FrameSpec = record
    buf: array[0..31] of char;
  end;

// Public Screen Stuff
// NOTE: This stuff is only included to allow compilation of the supplied public screen manager for educational purposes. Everything
//       here is subject to change without notice and I guarantee to do that just for fun! More info can be found in the screen manager source file.
const
  PSD_INITIAL_NAME: PChar = '(unnamed)';
  PSD_INITIAL_TITLE: PChar = 'MUI Public Screen';

const
  PSD_NAME_FRONTMOST: PChar = '«Frontmost»';
  PSD_FILENAME_SAVE: PChar = 'envarc:mui/PublicScreens.iff';
  PSD_FILENAME_USE: PChar = 'env:mui/PublicScreens.iff';
  PSD_MAXLEN_NAME = 32;
  PSD_MAXLEN_TITLE = 128;
  PSD_MAXLEN_FONT = 48;
  PSD_MAXLEN_BACKGROUND = 256;
  PSD_NUMCOLS = 8;
  PSD_MAXSYSPENS = 20;
  PSD_NUMSYSPENS = 12;
  PSD_MAXMUIPENS = 10;
  PSD_NUMMUIPENS = 8;

type
  TMUI_RGBcolor = record
    Red: LongWord;
    Green: LongWord;
    Blue: LongWord;
  end;
  PMUI_RGBColor = ^TMUI_RGBColor;

  TMUI_PubScreenDesc = record
    Version: LongInt;
    Name: array[0..PSD_MAXLEN_NAME-1] of char;
    Title: array[0..PSD_MAXLEN_TITLE-1] of char;
    Font: array[0..PSD_MAXLEN_FONT-1] of char;
    Background: array[0..PSD_MAXLEN_BACKGROUND-1] of char;

    DisplayID: LongWord;

    DisplayWidth: Word;
    DisplayHeight : Word;

    DisplayDepth: byte;
    OverscanType: byte;
    AutoScroll: byte;
    NoDrag: byte;
    Exclusive : byte;
    Interleaved : byte;
    SysDefault : byte;
    Behind : byte;
    AutoClose : byte;
    CloseGadget : byte;
    DummyWasForeign : byte;

    SystemPens: array[0..(PSD_MAXSYSPENS)-1] of byte;
    Reserved: array[0..((1 + (7 * 4)) - PSD_MAXSYSPENS)-1] of byte;

    Palette: array[0..(PSD_NUMCOLS)-1] of tMUI_RGBcolor;
    rsvd: array[0..(PSD_MAXSYSPENS - PSD_NUMCOLS)-1] of tMUI_RGBcolor;
    rsvd2: array[0..(PSD_MAXMUIPENS)-1] of tMUI_PenSpec;

    Changed: LongInt;
    UserData: APTR;
  end;
  PMUI_PubScreenDesc = ^TMUI_PubScreenDesc;

  TMUIS_InfoClient = record
    Node: TMinNode;
    Task: PTask;
    SigBit: LongWord;
  end;
  PMUIS_InfoClient = ^TMUIS_InfoClient;

// Object Types for MUI_MakeObject()

const
  MUIO_Label         = 1;   // label: STRPTR, flags: LongWord
  MUIO_Button        = 2;   // label: STRPTR
  MUIO_Checkmark     = 3;   // label: STRPTR
  MUIO_Cycle         = 4;   // label: STRPTR, entries: STRPTR
  MUIO_Radio         = 5;   // label: STRPTR, entries: STRPTR
  MUIO_Slider        = 6;   // label: STRPTR, min: LongInt, max: LongInt
  MUIO_String        = 7;   // label: STRPTR, maxlen: LongInt
  MUIO_PopButton     = 8;   // imagespec: STRPTR
  MUIO_HSpace        = 9;   // Space: LongInt
  MUIO_VSpace        = 10;  // Space: LongInt
  MUIO_HBar          = 11;  // Space: LongInt
  MUIO_VBar          = 12;  // Space: LongInt
  MUIO_MenustripNM   = 13;  // Nm: PNewMenu, Flags: LongWord
  MUIO_Menuitem      = 14;  // Label: STRPTR, shortcut: STRPTR, flags: LongWord, data: LongWord
  MUIO_BarTitle      = 15;  // Label: STRPTR
  MUIO_NumericButton = 16;  // Label: STRPTR, Min: LongInt, Max: LongInt, format: STRPTR
  // flags for MUIO_Menuitem
  MUIO_Menuitem_CopyStrings = 1 shl 30;
  //flags for MUIO_Label type
  MUIO_Label_SingleFrame    = 1 shl 8;
  MUIO_Label_DoubleFrame    = 1 shl 9;
  MUIO_Label_LeftAligned    = 1 shl 10;
  MUIO_Label_Centered       = 1 shl 11;
  MUIO_Label_FreeVert       = 1 shl 12;
  MUIO_Label_Tiny           = 1 shl 13;
  MUIO_Label_DontCopy       = 1 shl 14;
  // flag for MUIO_MenustripNM: check for "localized" menu items such as 'O'#0'Open'
  MUIO_MenustripNM_CommandKeyCheck = 1 shl 0; // check for "localized" menu items such as "O\0Open"


// ARexx Interface
type
  TMUI_Command = record
    mc_Name: PChar;
    mc_Template: PChar;
    mc_Parameters: LongInt;
    mc_Hook: PHook;
    mc_Reserved: array[0..4] of LongInt;
  end;
  PMUI_Command = ^TMUI_Command;

const
  MC_TEMPLATE_ID = -1;
  MUI_RXERR_BADDEFINITION  = -1;
  MUI_RXERR_OUTOFMEMORY    = -2;
  MUI_RXERR_UNKNOWNCOMMAND = -3;
  MUI_RXERR_BADSYNTAX      = -4;

// Return values for MUI_Error()
  MUIE_OK                  = 0;
  MUIE_OutOfMemory         = 1;
  MUIE_OutOfGfxMemory      = 2;
  MUIE_InvalidWindowObject = 3;
  MUIE_MissingLibrary      = 4;
  MUIE_NoARexx             = 5;
  MUIE_SingleTask          = 6;

// Standard MUI Images & Backgrounds
  //These images are configured with the preferences program.
  MUII_WindowBack    = 0;
  MUII_RequesterBack = 1;
  MUII_ButtonBack    = 2;
  MUII_ListBack      = 3;
  MUII_TextBack      = 4;
  MUII_PropBack      = 5;
  MUII_PopupBack     = 6;
  MUII_SelectedBack  = 7;
  MUII_ListCursor    = 8;
  MUII_ListSelect    = 9;
  MUII_ListSelCur    = 10;
  MUII_ArrowUp       = 11;
  MUII_ArrowDown     = 12;
  MUII_ArrowLeft     = 13;
  MUII_ArrowRight    = 14;
  MUII_CheckMark     = 15;
  MUII_RadioButton   = 16;
  MUII_Cycle         = 17;
  MUII_PopUp         = 18;
  MUII_PopFile       = 19;
  MUII_PopDrawer     = 20;
  MUII_PropKnob      = 21;
  MUII_Drawer        = 22;
  MUII_HardDisk      = 23;
  MUII_Disk          = 24;
  MUII_Chip          = 25;
  MUII_Volume        = 26;
  MUII_RegisterBack  = 27;
  MUII_Network       = 28;
  MUII_Assign        = 29;
  MUII_TapePlay      = 30;
  MUII_TapePlayBack  = 31;
  MUII_TapePause     = 32;
  MUII_TapeStop      = 33;
  MUII_TapeRecord    = 34;
  MUII_GroupBack     = 35;
  MUII_SliderBack    = 36;
  MUII_SliderKnob    = 37;
  MUII_TapeUp        = 38;
  MUII_TapeDown      = 39;
  MUII_PageBack      = 40;
  MUII_ReadListBack  = 41;
  MUII_PopFont       = 42;
  MUII_ImageButtonBack   = 43;
  MUII_ImageSelectedBack = 44;
  MUII_GaugeFull         = 45;
  MUII_GaugeEmpty        = 46;
  MUII_Menudisplay       = 47;
  MUII_PullOpen          = 48;
  MUII_StringBack        = 49;
  MUII_StringActiveBack  = 50;
  MUII_ListTitle         = 51;
  MUII_GroupTitle        = 52;
  MUII_RegisterTitle     = 53;
  MUII_Close             = 54;
  MUII_Count             = 55;
  // These are direct color combinations and are not affected by users prefs.
  // Generally, you should avoid using them. Better use one of the customized images above.
  MUII_BACKGROUND     = 128; // These are direct color
  MUII_SHADOW         = 129; // combinations and are not
  MUII_SHINE          = 130; // affected by users prefs.
  MUII_FILL           = 131;
  MUII_SHADOWBACK     = 132; // Generally, you should
  MUII_SHADOWFILL     = 133; // avoid using them. Better
  MUII_SHADOWSHINE    = 134; // use one of the customized
  MUII_FILLBACK       = 135; // images above.
  MUII_FILLSHINE      = 136;
  MUII_SHINEBACK      = 137;
  MUII_FILLBACK2      = 138;
  MUII_HSHINEBACK     = 139;
  MUII_HSHADOWBACK    = 140;
  MUII_HSHINESHINE    = 141;
  MUII_HSHADOWSHADOW  = 142;
  MUII_MARKSHINE      = 143;
  MUII_MARKHALFSHINE  = 144;
  MUII_MARKBACKGROUND = 145;
  MUII_LASTPAT        = 146;

// Special values for some methods, Special Values for MUIM_Notify
  MUIV_TriggerValue    = $49893131;
  MUIV_NotTriggerValue = $49893133;
  MUIV_EveryTime       = $49893131; // as TrigVal

  MUIV_Notify_Self               = 1;
  MUIV_Notify_Window             = 2;
  MUIV_Notify_Application        = 3;
  MUIV_Notify_Parent             = 4;
  MUIV_Notify_ParentParent       = 5;
  MUIV_Notify_ParentParentParent = 6;

const
   MUIV_DragQuery_Refuse    = 0;
   MUIV_DragQuery_Accept    = 1;

   MUIV_DragReport_Abort    = 0;
   MUIV_DragReport_Continue = 1;
   MUIV_DragReport_Lock     = 2;
   MUIV_DragReport_Refresh  = 3;

 //      Parameter structures for some classes
type
   PMUI_Palette_Entry = ^TMUI_Palette_Entry;
   TMUI_Palette_Entry = record
     mpe_ID: LongInt;
     mpe_Red: LongWord;
     mpe_Green: LongWord;
     mpe_Blue: LongWord;
     mpe_Group: LongInt;
   end;
const
   MUIV_Palette_Entry_End = -1;

// Window Event Handler (don't touch!)
// event handlers are inserted according to their priority. certain flags, see below for definitions.
// object which should receive MUIM_HandleEvent. if <> nil, MUIM_HandleEvent is invoked on exactly this class with CoerceMethod().
// one or more IDCMP flags this handler should react on.
// MUIM_Window_AddEventHandler/RemoveEventHandler
type
  PMUI_EventHandlerNode = ^TMUI_EventHandlerNode;
  TMUI_EventHandlerNode = record
    ehn_Node: TMinNode;
    ehn_Reserved: byte;
    ehn_Priority: byte;
    ehn_Flags: word;
    ehn_Object: PObject_;
    ehn_Class: PIClass;
    ehn_Events: LongWord;
  end;

const
  // flags for ehn_Flags
  MUI_EHF_ALWAYSKEYS     = 1 shl 0;  // not for public use
  MUI_EHF_GUIMODE        = 1 shl 1;  // handler will not be called if object is not visible or disabled
                                     // set this if you dont want your handler to be called when your object is disabled or invisible
  MUI_EHF_ISACTIVEGRP    = 1 shl 12; // not for public use */
  MUI_EHF_ISACTIVE       = 1 shl 13; // this flag is maintained by MUI and READ-ONLY:  set when ehn_Object is a window's active or default object. */
  MUI_EHF_ISCALLING      = 1 shl 14; // not for public use */
  MUI_EHF_ISENABLED      = 1 shl 15; // this flag is maintained by MUI and READ-ONLY: it is set when the handler is added (after MUIM_Window_AddEventHandler) */
                                     // and cleared when the handler is removed (after MUIM_Window_RemEventHandler). you may not change the state of this flag yourself, but you may read it */
                                     // to find out whether your handler is currently added to a window or not.
  // return values for MUIM_HandleEvent (bit-masked, all other bits must be 0)
  MUI_EventHandlerRC_Eat = 1 shl 0;  // stop MUI from calling other handlers

// MUI's draw pens
  MPEN_SHINE      = 0;
  MPEN_HALFSHINE  = 1;
  MPEN_BACKGROUND = 2;
  MPEN_HALFSHADOW = 3;
  MPEN_SHADOW     = 4;
  MPEN_TEXT       = 5;
  MPEN_FILL       = 6;
  MPEN_MARK       = 7;
  MPEN_COUNT      = 8; // Number of pens, the single definintion is below

  MUIPEN_MASK     = $0000ffff;  // Mask for pens from MUI_ObtainPen()

  MUIV_Font_Inherit  = 0;
  MUIV_Font_Normal   = -1;
  MUIV_Font_List     = -2;
  MUIV_Font_Tiny     = -3;
  MUIV_Font_Fixed    = -4;
  MUIV_Font_Title    = -5;
  MUIV_Font_Big      = -6;
  MUIV_Font_Button   = -7;
  MUIV_Font_Knob     = -8;
  MUIV_Font_NegCount = -9;


// List Position Test
type
  PMUI_List_TestPos_Result = ^TMUI_List_TestPos_Result;
  TMUI_List_TestPos_Result = record
    entry: LongInt;    // number of entry, -1 if mouse not over valid entry
    column: SmallInt;  // numer of column, -1 if no valid column
    flags: Word;       // MUI_LPR_*
    xoffset: SmallInt; // x offset of mouse click relative to column start
    yoffset: SmallInt; // y offset of mouse click from center of line (negative values mean click was above center,   positive values mean click was below center)
  end;

const
   MUI_LPR_ABOVE = 1 shl 0;
   MUI_LPR_BELOW = 1 shl 1;
   MUI_LPR_LEFT  = 1 shl 2;
   MUI_LPR_RIGHT = 1 shl 3;

// Structure used by MUIM_Application_AddInputHandler/RemInputHandler
type
  PMUI_InputHandlerNode = ^TMUI_InputHandlerNode;
  TMUI_InputHandlerNode = record
    ihn_Node: TMinNode;
    ihn_Object: PObject_;
    ihn_stuff: record
      case LongInt of
        0 : (ihn_sigs: LongWord);
        1 : (ihn_timer: record
               ihn_millis: word;
               ihn_current: word;
             end);
      end;
      ihn_Flags: LongWord;
      ihn_Method: LongWord;
    end;
  // ihn_Signals = ihn_stuff.ihn_sigs;
  // ihn_Millis = ihn_stuff.(ihn_timer.ihn_millis);
  // ihn_Current = ihn_stuff.(ihn_timer.ihn_current);

  TMUI_DragImage = record
    bm: PBitmap;
    width: SmallInt;  // exact width and height of bitmap
    height: SmallInt;
    touchx: SmallInt; // position of pointer click relative to bitmap
    touchy: SmallInt;
    flags: LongWord;
  end;
  PMUI_DragImage =^TMUI_DragImage;

{  For Boopsi Image Implementors Only:

   If MUI is using a boopsi image object, it will send a special method
   immediately after object creation. This method has a parameter structure
   where the boopsi can fill in its minimum and maximum size and learn if
   its used in a horizontal or vertical context.

   The boopsi image must use the method id (MUIM_BoopsiQuery) as return
   value. That's how MUI sees that the method is implemented.

   Note: MUI does not depend on this method. If the boopsi image doesn't
         implement it, minimum size will be 0 and maximum size unlimited. }

const
   MUIM_BoopsiQuery = $80427157;

   MRI_RARRAY_SIZE  = 20;
type
  Tdt_frame_image = record
  end;
  Pdt_frame_image = ^Tdt_frame_image;

  // Info about the display environment on which all Area Objects have a reference to it.
  TMUI_RenderInfo = record
    mri_WindowObject: PObject_; // accessable in-between MUIM_Setup/MUIM_Cleanup
    mri_Screen: PScreen;        // accessable in-between MUIM_Setup/MUIM_Cleanup
    mri_DrawInfo: PDrawInfo;    // accessable in-between MUIM_Setup/MUIM_Cleanup
    mri_Pens: PWord;            // accessable in-between MUIM_Setup/MUIM_Cleanup
    mri_Window: PWindow;        // accessable in-between MUIM_Show/MUIM_Hide
    mri_RastPort: PRastPort;    // accessable in-between MUIM_Show/MUIM_Hide
    mri_Flags: LongWord;        // accessable in-between MUIM_Setup/MUIM_Cleanup

    // the following stuff is private
    mri_Colormap: PColorMap;
    mri_ScreenWidth: word;
    mri_ScreenHeight: word;
    mri_PensStorage: array[0..MPEN_COUNT-1] of word; // storage for pens, mri_Pens points to here

    mri_Fonts: array[0..(0-MUIV_Font_NegCount)-1] of PTextFont; // Opened text fonts, done by zune_get_font()

    mri_rArray: array[0..MRI_RARRAY_SIZE-1] of PRegion; // this is for AddClipping/AddClipRegion
    mri_rCount: Integer;

    mri_ClipRect: TRectangle;
    mri_BorderTop: word;    // The height of the windows top border (title)
    mri_BorderBottom: word; // The height of the window's bottom bodder
    mri_BorderLeft: word;   // The width of the window's left border
    mri_BorderRight: word;  // The width of the window's right border

    // Stuff for Borderscrollers
    mri_LeftImage: PObject_;  // Valid between MUIM_Setup/MUIM_Cleanup
    mri_RightImage: PObject_;
    mri_UpImage: PObject_;
    mri_DownImage: PObject_;
    mri_SizeImage: PObject_;

    mri_VertProp: PObject_;  // Valid between MUIM_Show/MUIM_Hide
    mri_HorizProp: PObject_;

    // buffering
    mri_BufferRP: TRastPort;
    mri_BufferBM: PBitmap;

    mri_FrameImage: array[0..15] of Pdt_frame_image;
  end;
  PMUI_RenderInfo = ^TMUI_RenderInfo;

const
  // Flags for mri_Flags
  MUIMRI_RECTFILL    = 1 shl 0; // If mri_Flags and MUIMRI_RECTFILL, RectFill() is quicker than Move()/Draw() for horizontal or vertical lines. on the current display.
  MUIMRI_TRUECOLOR   = 1 shl 1; // If mri_Flags and MUIMRI_TRUECOLOR, display environment is a cybergraphics emulated hicolor or true color display.
  MUIMRI_THINFRAMES  = 1 shl 2; // If mri_Flags and MUIMRI_THINFRAMES, MUI uses thin frames (1:1) apsect ratio instead of standard 2:1 frames.
  MUIMRI_REFRESHMODE = 1 shl 3; // If mri_Flags and MUIMRI_REFRESHMODE, MUI is currently refreshing a WFLG_SIMPLEREFRESH window and is between a BeginRefresh()/EndRefresh() pair.

type
  PMUI_BoopsiQuery = ^TMUI_BoopsiQuery;
  TMUI_BoopsiQuery = record
    mbq_MethodID: LongWord;          // always MUIM_BoopsiQuery
    mbq_Screen: PScreen;             // obsolete, use mbq_RenderInfo
    mbq_Flags: LongWord;             // read only, see below
    mbq_MinWidth: LongInt;           // write only, fill in min width
    mbq_MinHeight: LongInt;          // write only, fill in min height
    mbq_MaxWidth: LongInt;           // write only, fill in max width
    mbq_MaxHeight: LongInt;          // write only, fill in max height
    mbq_DefWidth: LongInt;           // write only, fill in def width
    mbq_DefHeight: LongInt;          // write only, fill in def height
    mbq_RenderInfo: PMUI_RenderInfo; // read only, display context
  end;                               // may grow in future ...
// old structure name
  MUIP_BoopsiQuery = TMUI_BoopsiQuery;

const
  MBQF_HORIZ        = 1 shl 0;   // object used in a horizontal context (else vertical) use this for unlimited MaxWidth/Height
  MBQ_MUI_MAXMAX    = 10000;     // use this for unlimited MaxWidth/Height
  IDCMP_MOUSEOBJECT = $40000000; // special idcmp message created by MUI

// Flags for Slave.mui MUIM_Slave_Delegate
  MUIF_Slave_Delegate_ForceSlave = 1 shl 0; // skip caller task check and always dispatch the method on Slave's thread; useful when calling
                                            // from a process which is neither the main MUI process nor the Slave's process


// *********************************************************************
// Notify
const
  MUIC_Notify: PChar = 'Notify.mui';

// Identifier base (for Zune extensions)
  MUIB_Notify = MUIB_ZUNE or $00001d00;

// Methods
  MUIM_CallHook      = MUIB_MUI or $42b96b; // V4
  MUIM_Export        = MUIB_MUI or $420f1c; // V12
  MUIM_FindUData     = MUIB_MUI or $42c196; // V8
  MUIM_GetConfigItem = MUIB_MUI or $423edb; // V11
  MUIM_GetUData      = MUIB_MUI or $42ed0c; // V8
  MUIM_Import        = MUIB_MUI or $42d012; // V12
  MUIM_KillNotify    = MUIB_MUI or $42d240; // V4
  MUIM_KillNotifyObj = MUIB_MUI or $42b145; // V16
  MUIM_MultiSet      = MUIB_MUI or $42d356; // V7
  MUIM_NoNotifySet   = MUIB_MUI or $42216f; // V9
  MUIM_Notify        = MUIB_MUI or $42c9cb; // V4
  MUIM_Set           = MUIB_MUI or $42549a; // V4
  MUIM_SetAsString   = MUIB_MUI or $422590; // V4
  MUIM_SetUData      = MUIB_MUI or $42c920; // V8
  MUIM_SetUDataOnce  = MUIB_MUI or $42ca19; // V11
  MUIM_WriteLong     = MUIB_MUI or $428d86; // V6
  MUIM_WriteString   = MUIB_MUI or $424bf4; // V6
  // AROS Specials
  MUIM_ConnectParent    = MUIB_Notify or 0; // Zune V1
  MUIM_DisconnectParent = MUIB_Notify or 1; // Zune V1

type
  TMUIP_CallHook = record
    MethodID: LongWord; // MUIM_CallHook
    Hook: PHook;
    param1: LongWord;   // more might follow
  end;
  PMUIP_CallHook = ^TMUIP_CallHook;

  TMUIP_Export = record
    MethodID : LongWord; // MUIM_Export
    dataspace : PObject_;
  end;
  PMUIP_Export = ^TMUIP_Export;

  TMUIP_FindUData = record
    MethodID : LongWord; // MUIM_FindUData
    udata: LongWord;
  end;
  PMUIP_FindUData = ^TMUIP_FindUData;

  TMUIP_GetConfigItem = record
    MethodID: LongWord; // MUIM_GetConfigItem
    id: LongWord;
    storage: PLongWord;
  end;
  PMUIP_GetConfigItem = ^TMUIP_GetConfigItem;

  TMUIP_GetUData = record
    MethodID: LongWord; // MUIM_GetUData
    udata: LongWord;
    attr: LongWord;
    storage: PLongWord;
  end;
  PMUIP_GetUData = ^TMUIP_GetUData;

  TMUIP_Import = record
    MethodID : LongWord; // MUIM_Import
    dataspace : PObject_;
  end;
  PMUIP_Import = ^TMUIP_Import;

  TMUIP_KillNotify = record
    MethodID : LongWord; // MUIM_KillNotify
    TrigAttr : LongWord;
  end;
  PMUIP_KillNotify = ^TMUIP_KillNotify;

  TMUIP_KillNotifyObj = record
    MethodID: LongWord; // MUIM_KillNotifyObj
    TrigAttr: LongWord;
    dest: PObject_;
  end;
  PMUIP_KillNotifyObj = ^TMUIP_KillNotifyObj;

  TMUIP_MultiSet = record
    MethodID: LongWord; // MUIM_MultiSet
    attr: LongWord;
    val: LongWord;
    obj: APTR;          // more might follow
  end;
  PMUIP_MultiSet = ^TMUIP_MultiSet;

  TMUIP_NoNotifySet = record
    MethodID: LongWord; // MUIM_NoNotifySet
    attr: LongWord;
    val: LongWord;          // more might follow
  end;
  PMUIP_NoNotifySet = ^TMUIP_NoNotifySet;

  TMUIP_Notify = record
    MethodID: LongWord; // MUIM_Notify
    TrigAttr: LongWord;
    TrigVal: LongWord;
    DestObj: APTR;
    FollowParams: LongWord; // more might follow
  end;
  PMUIP_Notify = ^TMUIP_Notify;

  TMUIP_Set = record
    MethodID: LongWord; // MUIM_Set
    attr: LongWord;
    val: LongWord;
  end;
  PMUIP_Set = ^TMUIP_Set;

  TMUIP_SetAsString = record
    MethodID: LongWord; // MUIM_SetAsString
    attr: LongWord;
    format: PChar;
    val: LongWord;
  end;
  PMUIP_SetAsString = ^TMUIP_SetAsString;

  TMUIP_SetUData = record
    MethodID: LongWord; // MUIM_SetUData
    udata: LongWord;
    attr: LongWord;
    val: LongWord;
  end;
  PMUIP_SetUData = ^TMUIP_SetUData;

  TMUIP_SetUDataOnce = record
    MethodID : LongWord; // MUIM_SetUDataOnce
    udata: LongWord;
    attr: LongWord;
    val: LongWord;
  end;
  PMUIP_SetUDataOnce = ^TMUIP_SetUDataOnce;

  TMUIP_WriteLong = record
    MethodID: LongWord; // MUIM_WriteLong
    val: LongWord;
    memory: PLongWord;
  end;
  PMUIP_WriteLong = ^TMUIP_WriteLong;

  TMUIP_WriteString = record
    MethodID: LongWord; // MUIM_WriteString
    str: PChar;
    memory: PChar;
  end;
  PMUIP_WriteString = ^TMUIP_WriteString;

  TMUIP_ConnectParent = record
    MethodID: LongWord; // MUIM_ConnectParent
    parent: PObject_;
  end;
  PMUIP_ConnectParent = ^TMUIP_ConnectParent;

  TMUIP_DisconnectParent = record
    MethodID: LongWord; // MUIM_DisconnectParent
  end;
  PMUIP_DisconnectParent = ^TMUIP_DisconnectParent;

// Attributes
const
  MUIA_ApplicationObject = MUIB_MUI or $42d3ee; // V4  ..g PObject
  MUIA_AppMessage        = MUIB_MUI or $421955; // V5  ..g PAppMessage
  MUIA_HelpLine          = MUIB_MUI or $42a825; // V4  isg LongInt
  MUIA_HelpNode          = MUIB_MUI or $420b85; // V4  isg STRPTR
  MUIA_NoNotify          = MUIB_MUI or $4237f9; // V7  .s. WordBool
  MUIA_ObjectID          = MUIB_MUI or $42d76e; // V11 isg LongWord
  MUIA_Parent            = MUIB_MUI or $42e35f; // V11 ..g PObject
  MUIA_Revision          = MUIB_MUI or $427eaa; // V4  ..g LongInt
  MUIA_UserData          = MUIB_MUI or $420313; // V4  isg LongWord
  MUIA_Version           = MUIB_MUI or $422301; // V4  ..g LongInt
  MUIA_NoNotifyMethod    = MUIB_MUI or $420a74; // V20 .s. LongWord

// *********************************************************************
// Family
const
   MUIC_Family: PChar = 'Family.mui';

// Identifier base (for Zune extensions)
  MUIB_Family = MUIB_ZUNE or $00000c00;

// Methods
   MUIM_Family_AddHead        = MUIB_MUI or $42e200; // V8
   MUIM_Family_AddTail        = MUIB_MUI or $42d752; // V8
   MUIM_Family_Insert         = MUIB_MUI or $424d34; // V8
   MUIM_Family_Remove         = MUIB_MUI or $42f8a9; // V8
   MUIM_Family_Sort           = MUIB_MUI or $421c49; // V8
   MUIM_Family_Transfer       = MUIB_MUI or $42c14a; // V8
   MUIM_Family_GetChild       = MUIB_MUI or $42c556; // V20
   MUIM_Family_DoChildMethods = MUIB_MUI or $429a3c; // V20
type
  TMUIP_Family_AddHead = record
    MethodID: LongWord; // MUIM_Family_AddHead
    obj: PObject_;
  end;
  PMUIP_Family_AddHead = ^TMUIP_Family_AddHead;

  TMUIP_Family_AddTail = record
    MethodID: LongWord; // MUIM_Family_AddTail
    obj: PObject_;
  end;
  PMUIP_Family_AddTail = ^TMUIP_Family_AddTail;

  TMUIP_Family_Insert = record
    MethodID: LongWord; // MUIM_Family_Insert
    obj: PObject_;
    pred: PObject_;
  end;
  PMUIP_Family_Insert = ^TMUIP_Family_Insert;

  TMUIP_Family_Remove = record
    MethodID : LongWord; // MUIM_Family_Remove
    obj : PObject_;
  end;
  PMUIP_Family_Remove = ^TMUIP_Family_Remove;

  TMUIP_Family_Sort = record
    MethodID : LongWord; // MUIM_Family_Sort
    obj : array[0..0] of PObject_;
  end;
  PMUIP_Family_Sort = ^TMUIP_Family_Sort;

  TMUIP_Family_Transfer = record
    MethodID: LongWord; // MUIM_Family_Transfer
    family: PObject_;
  end;
  PMUIP_Family_Transfer = ^TMUIP_Family_Transfer;

  TMUIP_Family_GetChild = record
    MethodID: LongWord; // MUIM_Family_GetChild
    nr: LongInt;        // MUIV_Family_GetChild_* or Number
    ref: PObject_;
  end;
  PMUIP_Family_GetChild = ^TMUIP_Family_GetChild;

// Attributes
const
  MUIA_Family_Child      = MUIB_MUI or $42c696; // V8  i.. PObject
  MUIA_Family_List       = MUIB_MUI or $424b9e; // V8  ..g PMinList
  MUIA_Family_ChildCount = MUIB_MUI or $42b25a; // V20 ..g LongInt

  MUIV_Family_GetChild_First    = 0;
  MUIV_Family_GetChild_Last     = -1;
  MUIV_Family_GetChild_Next     = -2;
  MUIV_Family_GetChild_Previous = -3;
  MUIV_Family_GetChild_Iterate  = -4;

  MUIV_Group_GetChild_First    = MUIV_Family_GetChild_First;
  MUIV_Group_GetChild_Last     = MUIV_Family_GetChild_Last;
  MUIV_Group_GetChild_Next     = MUIV_Family_GetChild_Next;
  MUIV_Group_GetChild_Previous = MUIV_Family_GetChild_Previous;
  MUIV_Group_GetChild_Iterate  = MUIV_Family_GetChild_Iterate;

// *********************************************************************
//  Application
const
  MUIC_Application: PChar = 'Application.mui';

// Identifier base (for Zune extensions)
  MUIB_Application = MUIB_ZUNE or $00000100;

// Methods
const
  MUIM_Application_AboutMUI         = MUIB_MUI or $42d21d; // V14
  MUIM_Application_AddInputHandler  = MUIB_MUI or $42f099; // V11
  MUIM_Application_CheckRefresh     = MUIB_MUI or $424d68; // V11
  MUIM_Application_GetMenuCheck     = MUIB_MUI or $42c0a7; // V4
  MUIM_Application_GetMenuState     = MUIB_MUI or $42a58f; // V4
  MUIM_Application_Input            = MUIB_MUI or $42d0f5; // V4
  MUIM_Application_InputBuffered    = MUIB_MUI or $427e59; // V4
  MUIM_Application_Load             = MUIB_MUI or $42f90d; // V4
  MUIM_Application_NewInput         = MUIB_MUI or $423ba6; // V11
  MUIM_Application_OpenConfigWindow = MUIB_MUI or $4299ba; // V11
  MUIM_Application_PushMethod       = MUIB_MUI or $429ef8; // V4
  MUIM_Application_RemInputHandler  = MUIB_MUI or $42e7af; // V11
  MUIM_Application_ReturnID         = MUIB_MUI or $4276ef; // V4
  MUIM_Application_Save             = MUIB_MUI or $4227ef; // V4
  MUIM_Application_SetConfigItem    = MUIB_MUI or $424a80; // V11
  MUIM_Application_SetMenuCheck     = MUIB_MUI or $42a707; // V4
  MUIM_Application_SetMenuState     = MUIB_MUI or $428bef; // V4
  MUIM_Application_ShowHelp         = MUIB_MUI or $426479; // V4
  MUIM_Application_UnpushMethod     = MUIB_MUI or $4211dd; // V20

// AROS specials
  MUIM_Application_SetConfigdata    = MUIB_Application or $00000000; // Zune 20030407
  MUIM_Application_OpenWindows      = MUIB_Application or $00000001; // Zune 20030407
  MUIM_Application_Iconify          = MUIB_Application or $00000002; // Zune: V1
  MUIM_Application_Execute          = MUIB_Application or $00000003;
  MUIM_Application_UpdateMenus      = MUIB_Application or $00000004; // Zune 20070712

type
  TMUIP_Application_AboutMUI = record
    MethodID: LongWord;  // MUIM_Application_AboutMUI
    refwindow: PObject_;
  end;
  PMUIP_Application_AboutMUI = ^TMUIP_Application_AboutMUI;

  TMUIP_Application_AddInputHandler = record
    MethodID: LongWord;  // MUIM_Application_AddInputHandler
    ihnode: PMUI_InputHandlerNode;
  end;
  PMUIP_Application_AddInputHandler = ^TMUIP_Application_AddInputHandler;

  TMUIP_Application_CheckRefresh = record
    MethodID: LongWord;  // MUIM_Application_CheckRefresh
  end;
  PMUIP_Application_CheckRefresh = ^TMUIP_Application_CheckRefresh;

  TMUIP_Application_GetMenuCheck = record
    MethodID: LongWord;  // MUIM_Application_GetMenuCheck
    MenuID: LongWord;
  end;
  PMUIP_Application_GetMenuCheck = ^TMUIP_Application_GetMenuCheck;

  TMUIP_Application_GetMenuState = record
    MethodID: LongWord;  // MUIM_Application_GetMenuState
    MenuID: LongWord;
  end;
  PMUIP_Application_GetMenuState = ^TMUIP_Application_GetMenuState;

  TMUIP_Application_Input = record
    MethodID: LongWord;  // MUIM_Application_Input
    signal: PLongWord;
  end;
  PMUIP_Application_Input = ^TMUIP_Application_Input;

  TMUIP_Application_InputBuffered = record
    MethodID: LongWord;  // MUIM_Application_InputBuffered
  end;
  PMUIP_Application_InputBuffered = ^TMUIP_Application_InputBuffered;

  TMUIP_Application_Load = record
    MethodID: LongWord;  // MUIM_Application_Load
    name: STRPTR;
  end;
  PMUIP_Application_Load = ^TMUIP_Application_Load;

  TMUIP_Application_NewInput = record
    MethodID: LongWord;  // MUIM_Application_NewInput
    signal: PLongWord;
  end;
  PMUIP_Application_NewInput = ^TMUIP_Application_NewInput;

  TMUIP_Application_OpenConfigWindow = record
    MethodID: LongWord;  // MUIM_Application_OpenConfigWindow
    flags: LongWord;
  end;
  PMUIP_Application_OpenConfigWindow = ^TMUIP_Application_OpenConfigWindow;

  TMUIP_Application_PushMethod = record
    MethodID: LongWord;  // MUIM_Application_PushMethod
    dest: PObject_;
    count: LongInt; // more elements may follow
  end;
  PMUIP_Application_PushMethod = ^TMUIP_Application_PushMethod;

  TMUIP_Application_UnpushMethod = record
    MethodID: LongWord;  // MUIM_Application_UnpushMethod
    dest: PObject_;
    NewMethodID: LongWord;
    method: LongWord;
  end;
  PMUIP_Application_UnpushMethod = ^TMUIP_Application_UnpushMethod;

  TMUIP_Application_RemInputHandler = record
    MethodID: LongWord;   // MUIM_Application_RemInputHandler
    ihnode: PMUI_InputHandlerNode;
  end;
  PMUIP_Application_RemInputHandler = ^TMUIP_Application_RemInputHandler;

  TMUIP_Application_ReturnID = record
    MethodID: LongWord;  // MUIM_Application_ReturnID
    retid: LongWord;
  end;
  PMUIP_Application_ReturnID = ^TMUIP_Application_ReturnID;

  TMUIP_Application_Save = record
    MethodID: LongWord;  // MUIM_Application_Save
    name: STRPTR;
  end;

  TMUIP_Application_SetConfigItem = record
    MethodID: LongWord;  // MUIM_Application_SetConfigItem
    item: LongWord;
    data: APTR;
  end;
  PMUIP_Application_SetConfigItem = ^TMUIP_Application_SetConfigItem;

  TMUIP_Application_SetMenuCheck = record
    MethodID: LongWord;  // MUIM_Application_SetMenuCheck
    MenuID: LongWord;
    stat: LongInt;
  end;
  PMUIP_Application_SetMenuCheck = ^TMUIP_Application_SetMenuCheck;

  TMUIP_Application_SetMenuState = record
    MethodID : LongWord;  // MUIM_Application_SetMenuState
    MenuID : LongWord;
    stat : LongInt;
  end;
  PMUIP_Application_SetMenuState = ^TMUIP_Application_SetMenuState;

  TMUIP_Application_ShowHelp = record
    MethodID: LongWord;  // MUIM_Application_ShowHelp
    window: PObject_;
    name: PChar;
    node: PChar;
    line: LongInt;
  end;
  PMUIP_Application_ShowHelp = ^TMUIP_Application_ShowHelp;

// Attributes
const
  MUIA_Application_Active          = MUIB_MUI or $4260ab; // V4  isg WordBool
  MUIA_Application_Author          = MUIB_MUI or $424842; // V4  i.g STRPTR
  MUIA_Application_Base            = MUIB_MUI or $42e07a; // V4  i.g STRPTR
  MUIA_Application_Broker          = MUIB_MUI or $42dbce; // V4  ..g PBroker
  MUIA_Application_BrokerHook      = MUIB_MUI or $428f4b; // V4  isg PHook
  MUIA_Application_BrokerPort      = MUIB_MUI or $42e0ad; // V6  ..g PMsgPort
  MUIA_Application_BrokerPri       = MUIB_MUI or $42c8d0; // V6  i.g LongInt
  MUIA_Application_Commands        = MUIB_MUI or $428648; // V4  isg PMUI_Command
  MUIA_Application_Copyright       = MUIB_MUI or $42ef4d; // V4  i.g STRPTR
  MUIA_Application_Description     = MUIB_MUI or $421fc6; // V4  i.g STRPTR
  MUIA_Application_DiskObject      = MUIB_MUI or $4235cb; // V4  isg PDiskObject
  MUIA_Application_DoubleStart     = MUIB_MUI or $423bc6; // V4  ..g WordBool
  MUIA_Application_DropObject      = MUIB_MUI or $421266; // V5  is. PObject
  MUIA_Application_ForceQuit       = MUIB_MUI or $4257df; // V8  ..g WordBool
  MUIA_Application_HelpFile        = MUIB_MUI or $4293f4; // V8  isg STRPTR
  MUIA_Application_Iconified       = MUIB_MUI or $42a07f; // V4  .sg WordBool
  MUIA_Application_MenuAction      = MUIB_MUI or $428961; // V4  ..g LongWord
  MUIA_Application_MenuHelp        = MUIB_MUI or $42540b; // V4  ..g LongWord
  MUIA_Application_Menustrip       = MUIB_MUI or $4252d9; // V8  i.. PObject
  MUIA_Application_RexxHook        = MUIB_MUI or $427c42; // V7  isg PHook
  MUIA_Application_RexxMsg         = MUIB_MUI or $42fd88; // V4  ..g PRxMsg
  MUIA_Application_RexxString      = MUIB_MUI or $42d711; // V4  .s. STRPTR
  MUIA_Application_SingleTask      = MUIB_MUI or $42a2c8; // V4  i.. WordBool
  MUIA_Application_Sleep           = MUIB_MUI or $425711; // V4  .s. WordBool
  MUIA_Application_Title           = MUIB_MUI or $4281b8; // V4  i.g STRPTR
  MUIA_Application_UseCommodities  = MUIB_MUI or $425ee5; // V10 i.. WordBool
  MUIA_Application_UsedClasses     = MUIB_MUI or $42e9a7; // V10 i.. STRPTR
  MUIA_Application_UseRexx         = MUIB_MUI or $422387; // V10 i.. WordBool
  MUIA_Application_SetWinPos       = MUIB_MUI or $432387;
  MUIA_Application_GetWinPos       = MUIB_MUI or $432388;
  MUIA_Application_SearchWinId     = MUIB_MUI or $432389;
  MUIA_Application_GetWinPosAddr   = MUIB_MUI or $432390;
  MUIA_Application_GetWinPosSize   = MUIB_MUI or $432391;
  MUIA_Application_CopyWinPosToApp = MUIB_MUI or $432392;
  MUIA_Application_Version         = MUIB_MUI or $42b33f; // V4  i.g STRPTR Standard DOS version string Example: "$VER: Program 1.3 (14.11.03)". Zune
                                                          //                Zune extension: If unspecified or NULL, it will be automatically
                                                          //                constructed from MUIA_Application_Title, MUIA_Application_Version_Number,
                                                          //                MUIA_Application_Version_Date and MUIA_Application_Version_Extra as
                                                          //                follows: "$VER: <title> <version> (<date>) [<extra>]".
  MUIA_Application_Window          = MUIB_MUI or $42bfe0; // V4  i.. PObject
  MUIA_Application_WindowList      = MUIB_MUI or $429abe; // V13 ..g PList

  MUIA_Application_Configdata      = MUIB_Application or $0; // Zune 20030407 .s. PObject
  MUIA_Application_Version_Number  = MUIB_Application or $1; // Zune          i.g STRPTR Version number. Examples: "1.5", "2.37.4b".
  MUIA_Application_Version_Date    = MUIB_Application or $2; // Zune          i.g STRPTR Date information on the standard international YYYY-MM-DD format.
  MUIA_Application_Version_Extra   = MUIB_Application or $3; // Zune          i.g STRPTR Arbitrary extra version information. Example: "nightly build".

// MUI Obsolete tags
{$ifdef MUI_OBSOLETE}
  MUIA_Application_Menu = MUIB_MUI or $420e1f; // V4  i.g PNewMenu
{$endif} // MUI_OBSOLETE

  MAXWINS = 300;

  MUIV_Application_Package_NetConnect = $a3ff7b49;

type
  TWindowPos = record
    id: LongWord;
    x1, y1, w1, h1: SmallInt;
    x2, y2, w2, h2: SmallInt;
  end;

const
// Flags for ihn_Flags set ihn_Ticks to number of 1/100 sec ticks you want to be triggered
  MUIIHNF_TIMER          = 1 shl 0; // you want to be called every ihn_Millis msecs
  MUIIHNF_TIMER_SCALE10  = 1 shl 1; // ihn_Millis is in 1/100 seconds instead
  MUIIHNF_TIMER_SCALE100 = 1 shl 2; // ihn_Millis is in 1/10 seconds instead setting both SCALE10|SCALE100 makes ihn_Millis 1/1 seconds

// Special values for the name field of MUIM_Application_Load/Save
  MUIV_Application_Save_ENV    = 0;
  MUIV_Application_Save_ENVARC = -1;
  MUIV_Application_Load_ENV    = 0;
  MUIV_Application_Load_ENVARC = -1;

//  Special Values MUIM_Application_ReturnID. Usually program should leave the event loop if this is set
  MUIV_Application_ReturnID_Quit = -1;

type
  TMUI_GlobalInfo = record
    priv0: LongWord;
    mgi_ApplicationObject: PObject_;
    // The following data is private only, might be extended!
  end;
  PMUI_GlobalInfo = ^TMUI_GlobalInfo;

// *********************************************************************
//  Window
const
  MUIC_Window: PChar = 'Window.mui';

// Identifier base (for Zune extensions)
  MUIB_Window = MUIB_ZUNE or $00003600;

// Methods
  MUIM_Window_ActionIconify   = MUIB_MUI or $422cc0; // V18 undoc
  MUIM_Window_AddEventHandler = MUIB_MUI or $4203b7; // V16
  MUIM_Window_Cleanup         = MUIB_MUI or $42ab26; // V18 undoc For custom classes only
  MUIM_Window_RemEventHandler = MUIB_MUI or $42679e; // V16
  MUIM_Window_ScreenToBack    = MUIB_MUI or $42913d; // V4
  MUIM_Window_ScreenToFront   = MUIB_MUI or $4227a4; // V4
  MUIM_Window_Setup           = MUIB_MUI or $42c34c; // V18 undoc For custom Classes only
  MUIM_Window_Snapshot        = MUIB_MUI or $42945e; // V11
  MUIM_Window_ToBack          = MUIB_MUI or $42152e; // V4
  MUIM_Window_ToFront         = MUIB_MUI or $42554f; // V4
  // AROS specials
  MUIM_Window_AllocGadgetID   = MUIB_Window or $1; // Zune: V1 - allocate a GadgetID for BOOPSI gadgets
  MUIM_Window_FreeGadgetID    = MUIB_Window or $4; // Zune: V1 - free the GadgetID for BOOPSI gadgets

type
  TMUIP_Window_ActionIconify = record
    MethodID: LongWord;  // MUIM_Window_ActionIconify
  end;
  PMUIP_Window_ActionIconify = ^TMUIP_Window_ActionIconify;

  TMUIP_Window_AddEventHandler = record
    MethodID: LongWord;  // MUIM_Window_AddEventHandler
    ehnode: PMUI_EventHandlerNode;
  end;
  PMUIP_Window_AddEventHandler = ^TMUIP_Window_AddEventHandler;

  TMUIP_Window_Cleanup = record
    MethodID: LongWord;  // MUIM_Window_Cleanup
  end;
  PMUIP_Window_Cleanup = ^TMUIP_Window_Cleanup;

  TMUIP_Window_RemEventHandler = record
    MethodID: LongWord; // MUIM_Window_RemEventHandler
    ehnode: PMUI_EventHandlerNode;
  end;
  PMUIP_Window_RemEventHandler = ^TMUIP_Window_RemEventHandler;

  TMUIP_Window_ScreenToBack = record
    MethodID: LongWord; // MUIM_Window_ScreenToBack
  end;
  PMUIP_Window_ScreenToBack = ^TMUIP_Window_ScreenToBack;

  TMUIP_Window_ScreenToFront = record
    MethodID: LongWord; // MUIM_Window_ScreenToFront
  end;
  PMUIP_Window_ScreenToFront = ^TMUIP_Window_ScreenToFront;

  TMUIP_Window_Setup = record
    MethodID: LongWord; // MUIM_Window_Setup
  end;
  PMUIP_Window_Setup = ^TMUIP_Window_Setup;

  TMUIP_Window_Snapshot = record
    MethodID: LongWord;  // MUIM_Window_Snapshot
    flags: LongInt;
  end;
  PMUIP_Window_Snapshot = ^TMUIP_Window_Snapshot;

  TMUIP_Window_ToBack = record
    MethodID: LongWord;  // MUIM_Window_ToBack
  end;
  PMUIP_Window_ToBack = ^TMUIP_Window_ToBack;

  TMUIP_Window_ToFront = record
    MethodID: LongWord;  // MUIM_Window_ToFront
  end;
  PMUIP_Window_ToFront = ^TMUIP_Window_ToFront;

  TMUIP_Window_AddControlCharHandler = record
    MethodID: LongWord;
    ccnode: PMUI_EventHandlerNode;
  end;
  PMUIP_Window_AddControlCharHandler = ^TMUIP_Window_AddControlCharHandler;

  TMUIP_Window_AllocGadgetID = record
    MethodID: LongWord;
  end;
  PMUIP_Window_AllocGadgetID = ^TMUIP_Window_AllocGadgetID;

  TMUIP_Window_DrawBackground = record
    MethodID: LongWord;
    left: LongInt;
    top: LongInt;
    width: LongInt;
    height: LongInt;
    xoffset: LongInt;
    yoffset: LongInt;
    flags: LongInt;
  end;
  PMUIP_Window_DrawBackground = ^TMUIP_Window_DrawBackground;

  TMUIP_Window_DragObject = record
    MethodID: LongWord;
    obj: PObject_;
    touchx: LongInt;
    touchy: LongInt;
    flags: LongWord;
  end;
  PMUIP_Window_DragObject = ^TMUIP_Window_DragObject;

  TMUIP_Window_FreeGadgetID = record
    MethodID: LongWord;
    gadgetid: LongInt;
  end;
  PMUIP_Window_FreeGadgetID = ^TMUIP_Window_FreeGadgetID;

  TMUIP_Window_RecalcDisplay = record
    MethodID: LongWord;
    originator: PObject_;
  end;
  PMUIP_Window_RecalcDisplay = ^TMUIP_Window_RecalcDisplay;

  TMUIP_Window_RemControlCharHandler = record
    MethodID: LongWord;
    ccnode: PMUI_EventHandlerNode;
  end;
  PMUIP_Window_RemControlCharHandler = ^TMUIP_Window_RemControlCharHandler;

  TMUIP_Window_UpdateMenu = record
    MethodID: LongWord;
  end;

{$ifdef MUI_OBSOLETE}
const
  MUIM_Window_GetMenuCheck  = MUIB_MUI or $420414; // V4
  MUIM_Window_GetMenuState  = MUIB_MUI or $420d2f; // V4
  MUIM_Window_SetCycleChain = MUIB_MUI or $426510; // V4
  MUIM_Window_SetMenuCheck  = MUIB_MUI or $422243; // V4
  MUIM_Window_SetMenuState  = MUIB_MUI or $422b5e; // V4
type
  TMUIP_Window_GetMenuCheck = record
    MethodID: LongWord;
    MenuID: LongWord;
  end;

  TMUIP_Window_GetMenuState = record
    MethodID: LongWord;
    MenuID: LongWord;
  end;

  TMUIP_Window_SetCycleChain = record
    MethodID: LongWord;
    obj: array[0..0] of PObject_;
  end;

  TMUIP_Window_SetMenuCheck = record
    MethodID: LongWord;
    MenuID: LongWord;
    stat: LongInt;
  end;

  TMUIP_Window_SetMenuState = record
    MethodID: LongWord;
    MenuID: LongWord;
    stat: LongInt;
  end;
{$endif}

// Attributes
const
  MUIA_Window_Activate      = MUIB_MUI or $428d2f; // V4  isg WordBool
  MUIA_Window_ActiveObject  = MUIB_MUI or $427925; // V4  .sg PObject
  MUIA_Window_AltHeight     = MUIB_MUI or $42cce3; // V4  i.g LongInt
  MUIA_Window_AltLeftEdge   = MUIB_MUI or $422d65; // V4  i.g LongInt
  MUIA_Window_AltTopEdge    = MUIB_MUI or $42e99b; // V4  i.g LongInt
  MUIA_Window_AltWidth      = MUIB_MUI or $4260f4; // V4  i.g LongInt
  MUIA_Window_AppWindow     = MUIB_MUI or $4280cf; // V5  i.. WordBool
  MUIA_Window_Backdrop      = MUIB_MUI or $42c0bb; // V4  i.. WordBool
  MUIA_Window_Borderless    = MUIB_MUI or $429b79; // V4  i.. WordBool
  MUIA_Window_CloseGadget   = MUIB_MUI or $42a110; // V4  i.. WordBool
  MUIA_Window_CloseRequest  = MUIB_MUI or $42e86e; // V4  ..g WordBool
  MUIA_Window_DefaultObject = MUIB_MUI or $4294d7; // V4  isg PObject
  MUIA_Window_DepthGadget   = MUIB_MUI or $421923; // V4  i.. WordBool
  MUIA_Window_DisableKeys   = MUIB_MUI or $424c36; // V15 isg LongWord undoc
  MUIA_Window_DragBar       = MUIB_MUI or $42045d; // V4  i.. WordBool
  MUIA_Window_FancyDrawing  = MUIB_MUI or $42bd0e; // V8  isg WordBool
  MUIA_Window_Height        = MUIB_MUI or $425846; // V4  i.g LongInt
  MUIA_Window_ID            = MUIB_MUI or $4201bd; // V4  isg LongWord
  MUIA_Window_InputEvent    = MUIB_MUI or $4247d8; // V4  ..g PInputEvent
  MUIA_Window_IsSubWindow   = MUIB_MUI or $42b5aa; // V4  isg WordPool
  MUIA_Window_LeftEdge      = MUIB_MUI or $426c65; // V4  i.g LongInt

  MUIA_Window_MenuAction       = MUIB_MUI or $427521; // V8  isg LongWord
  MUIA_Window_Menustrip        = MUIB_MUI or $42855e; // V8  i.g PObject
  MUIA_Window_MouseObject      = MUIB_MUI or $42bf9b; // V10 ..g PObject
  MUIA_Window_NeedsMouseObject = MUIB_MUI or $42372a; // V10 i.. WordBool
  MUIA_Window_NoMenus          = MUIB_MUI or $429df5; // V4  is. WordBool
  MUIA_Window_Open             = MUIB_MUI or $428aa0; // V4  .sg WordBool
  MUIA_Window_PublicScreen     = MUIB_MUI or $4278e4; // V6  isg STRPTR
  MUIA_Window_RefWindow        = MUIB_MUI or $4201f4; // V4  is. PObject
  MUIA_Window_RootObject       = MUIB_MUI or $42cba5; // V4  isg PObject
  MUIA_Window_Screen           = MUIB_MUI or $42df4f; // V4  isg PScreen
  MUIA_Window_ScreenTitle      = MUIB_MUI or $4234b0; // V5  isg STRPTR
  MUIA_Window_SizeGadget       = MUIB_MUI or $42e33d; // V4  i.. WordBool
  MUIA_Window_SizeRight        = MUIB_MUI or $424780; // V4  i.. WordBool
  MUIA_Window_Sleep            = MUIB_MUI or $42e7db; // V4  .sg WordBool
  MUIA_Window_Title            = MUIB_MUI or $42ad3d; // V4  isg STRPTR
  MUIA_Window_TopEdge          = MUIB_MUI or $427c66; // V4  i.g LongInt
  MUIA_Window_UseBottomBorderScroller = MUIB_MUI or $424e79; // V13 isg WordBool
  MUIA_Window_UseLeftBorderScroller   = MUIB_MUI or $42433e; // V13 isg WordBool
  MUIA_Window_UseRightBorderScroller  = MUIB_MUI or $42c05e; // V13 isg WordBool
  MUIA_Window_Width            = MUIB_MUI or $42dcae; // V4  i.g LongInt
  MUIA_Window_Window           = MUIB_MUI or $426a42; // V4  ..g PWindow
  // AROS specials
  MUIA_Window_EraseArea  = MUIB_Window or $0; // Zune i.. WordBool default: True
  MUIA_Window_ZoomGadget = MUIB_Window or $2; // Zune i.. WordBool Show ZoomGadget
  MUIA_Window_ToolBox    = MUIB_Window or $3; // Zune i.. WordBool default: False Window should be opened as ToolBox

// Values
  MUIV_Window_ActiveObject_None = 0;
  MUIV_Window_ActiveObject_Next = -1;
  MUIV_Window_ActiveObject_Prev = -2;

  MUIV_Window_AltHeight_Scaled     = -1000;
  MUIV_Window_AltLeftEdge_Centered = -1;
  MUIV_Window_AltLeftEdge_Moused   = -2;
  MUIV_Window_AltLeftEdge_NoChange = -1000;
  MUIV_Window_AltTopEdge_Centered  = -1;
  MUIV_Window_AltTopEdge_Moused    = -2;
  MUIV_Window_AltTopEdge_NoChange  = -1000;
  MUIV_Window_AltWidth_Scaled      = -1000;

  MUIV_Window_Height_Scaled        = -1000;
  MUIV_Window_Height_Default       = -1001;
  MUIV_Window_LeftEdge_Centered    = -1;
  MUIV_Window_LeftEdge_Moused      = -2;
  MUIV_Window_TopEdge_Centered     = -1;
  MUIV_Window_TopEdge_Moused       = -2;
  MUIV_Window_Width_Scaled         = -1000;
  MUIV_Window_Width_Default        = -1001;

  MUIV_Window_Button_MUI      = 1;
  MUIV_Window_Button_Snapshot = 2;
  MUIV_Window_Button_Iconify  = 4;
  MUIV_Window_Button_Popup    = 8;

{$ifdef MUI_OBSOLETE}
  MUIA_Window_Menu        = MUIB_MUI or $42db94; // V4  i.. PNewMenu

  MUIV_Window_Menu_NoMenu = -1;
{$endif}


// *********************************************************************
//  Area
const
  MUIC_Area: PChar = 'Area.mui';

// Identifier base (for Zune extensions)
  MUIB_Area = MUIB_ZUNE or $00000200;

// Methods
const
  MUIM_AskMinMax         = MUIB_MUI or $423874; // V4 for Custom Classes only
  MUIM_Cleanup           = MUIB_MUI or $42d985; // V4 for Custom Classes only
  MUIM_ContextMenuBuild  = MUIB_MUI or $429d2e; // V11
  MUIM_ContextMenuChoice = MUIB_MUI or $420f0e; // V11
  MUIM_CreateBubble      = MUIB_MUI or $421c41; // V18
  MUIM_CreateDragImage   = MUIB_MUI or $42eb6f; // V18 for Custom Classes only, undoc
  MUIM_CreateShortHelp   = MUIB_MUI or $428e93; // V11
  MUIM_CustomBackfill    = MUIB_MUI or $428d73; // Undoc
  MUIM_DeleteBubble      = MUIB_MUI or $4211af; // V18
  MUIM_DeleteDragImage   = MUIB_MUI or $423037; // V18 for Custom Classes only, undoc
  MUIM_DeleteShortHelp   = MUIB_MUI or $42d35a; // V11
  MUIM_DoDrag            = MUIB_MUI or $4216bb; // V18 for Custom Classes only, undoc
  MUIM_DragBegin         = MUIB_MUI or $42c03a; // V11
  MUIM_DragDrop          = MUIB_MUI or $42c555; // V11
  MUIM_DragFinish        = MUIB_MUI or $4251f0; // V11
  MUIM_DragQuery         = MUIB_MUI or $420261; // V11
  MUIM_DragReport        = MUIB_MUI or $42edad; // V11
  MUIM_Draw              = MUIB_MUI or $426f3f; // V4  Custom Class
  MUIM_DrawBackground    = MUIB_MUI or $4238ca; // V11
  MUIM_GoActive          = MUIB_MUI or $42491a; // Undoc
  MUIM_GoInactive        = MUIB_MUI or $422c0c; // Undoc
  MUIM_HandleEvent       = MUIB_MUI or $426d66; // V16 Custom Class
  MUIM_HandleInput       = MUIB_MUI or $422a1a; // V4  Custom Class
  MUIM_Hide              = MUIB_MUI or $42f20f; // V4  Custom Class
  MUIM_Setup             = MUIB_MUI or $428354; // V4  Custom Class
  MUIM_Show              = MUIB_MUI or $42cc84; // V4  Custom Class
  // AROS Specials
  MUIM_UnknownDropDestination = MUIB_MUI or $425550; // Zune
  MUIM_Layout                 = MUIB_Area or $0;
  MUIM_DrawParentBackground   = MUIB_Area or $1;

  MUIV_CreateBubble_DontHidePointer = 1 shl 0;
  MUIV_Application_OCW_ScreenPage   = 1 shl 1; // show just the screen page of the config window
   MUIV_ContextMenuBuild_Default    = $ffffffff;

type
// MUI_MinMax structure holds information about minimum, maximum and default dimensions of an object.
  TMUI_MinMax = record
    MinWidth: SmallInt;
    MinHeight: SmallInt;
    MaxWidth: SmallInt;
    MaxHeight: SmallInt;
    DefWidth: SmallInt;
    DefHeight: SmallInt;
  end;
  PMUI_MinMax = ^TMUI_MinMax;

  TMUIP_AskMinMax = record
    MethodID: LongWord;  // MUIM_AskMinMax
    MinMaxInfo: PMUI_MinMax;
  end;
  PMUIP_AskMinMax = ^TMUIP_AskMinMax;

  TMUIP_Cleanup = record
    MethodID: LongWord;  // MUIM_Cleanup
  end;
  PMUIP_Cleanup = ^TMUIP_Cleanup;

  TMUIP_ContextMenuBuild = record
    MethodID: LongWord;  // MUIM_ContextMenuBuild
    mx: LongInt;
    my: LongInt;
  end;
  PMUIP_ContextMenuBuild = ^TMUIP_ContextMenuBuild;

  TMUIP_ContextMenuChoice = record
    MethodID: LongWord;  // MUIM_ContextMenuChoice
    item: PObject_;
  end;
  PMUIP_ContextMenuChoice = ^TMUIP_ContextMenuChoice;

  TMUIP_CreateBubble = record
    MethodID: LongWord;  // MUIM_CreateBubble
    x: LongInt;
    y: LongInt;
    txt : PChar;
    flags: LongWord;
  end;
  PMUIP_CreateBubble = ^TMUIP_CreateBubble;

  TMUIP_CreateDragImage = record
    MethodID: LongWord;  // MUIM_CreateDragImage
    touchx: LongInt;
    touchy: LongInt;
    flags: LongWord;
  end;
  PMUIP_CreateDragImage = ^TMUIP_CreateDragImage;

  TMUIP_CreateShortHelp = record
    MethodID: LongWord;  // MUIM_CreateShortHelp
    mx: LongInt;
    my: LongInt;
  end;
  PMUIP_CreateShortHelp = ^TMUIP_CreateShortHelp;

  TMUIP_CustomBackfill = record
    MethodID: LongWord;  // MUIM_CustomBackfill
    left: LongInt;
    top: LongInt;
    right: LongInt;
    bottom: LongInt;
    xoffset: LongInt;
    yoffset: LongInt;
  end;
  PMUIP_CustomBackfill = ^TMUIP_CustomBackfill;

  TMUIP_DeleteBubble = record
    MethodID: LongWord;  // MUIM_DeleteBubble
    bubble: APTR;
  end;
  PMUIP_DeleteBubble = ^TMUIP_DeleteBubble;

  TMUIP_DeleteDragImage = record
    MethodID: LongWord;  // MUIM_DeleteDragImage
    di: PMUI_DragImage;
  end;
  PMUIP_DeleteDragImage = ^TMUIP_DeleteDragImage;

  TMUIP_DeleteShortHelp = record
    MethodID: LongWord;  // MUIM_DeleteShortHelp
    help: STRPTR;
  end;
  PMUIP_DeleteShortHelp = ^TMUIP_DeleteShortHelp;

  TMUIP_DoDrag = record
    MethodID: LongWord;  // MUIM_DoDrag
    touchx: LongInt;
    touchy: LongInt;
    flags: LongWord;
  end;
  PMUIP_DoDrag = ^TMUIP_DoDrag;

  TMUIP_UnknownDropDestination = record
    MethodID: LongWord;  // MUIM_UnknownDropDestination
    imsg: PIntuiMessage;
  end;
  PMUIP_UnknownDropDestination = ^TMUIP_UnknownDropDestination;

  TMUIP_DragBegin = record
    MethodID: LongWord;  // MUIM_DragBegin
    obj: PObject_;
  end;
  PMUIP_DragBegin = ^TMUIP_DragBegin;

  TMUIP_DragDrop = record
    MethodID: LongWord;  // MUIM_DragDrop
    obj: PObject_;
    x: LongInt;
    y: LongInt;
  end;
  PMUIP_DragDrop = ^TMUIP_DragDrop;

  TMUIP_DragFinish = record
    MethodID: LongWord;  // MUIM_DragFinish
    obj: PObject_;
  end;
  PMUIP_DragFinish = ^TMUIP_DragFinish;

  TMUIP_DragQuery = record
    MethodID: LongWord;  // MUIM_DragQuery
    obj: PObject_;
  end;
  PMUIP_DragQuery = ^TMUIP_DragQuery;

  TMUIP_DragReport = record
    MethodID: LongWord;  // MUIM_DragReport
    obj: PObject_;
    x: LongInt;
    y: LongInt;
    update: LongInt;
  end;
  PMUIP_DragReport = ^TMUIP_DragReport;

  TMUIP_Draw = record
    MethodID: LongWord;  // MUIM_Draw
    flags: LongWord;
  end;
  PMUIP_Draw = ^TMUIP_Draw;

  TMUIP_DrawBackground = record
    MethodID: LongWord;  // MUIM_DrawBackground
    left: LongInt;
    top: LongInt;
    width: LongInt;
    height: LongInt;
    xoffset: LongInt;
    yoffset: LongInt;
    flags: LongInt;
  end;
  PMUIP_DrawBackground = ^TMUIP_DrawBackground;

  TMUIP_DrawBackgroundBuffered = record
    MethodID: LongWord;
    rp: PRastPort;
    left: LongInt;
    top: LongInt;
    width: LongInt;
    height: LongInt;
    xoffset: LongInt;
    yoffset: LongInt;
    flags: LongInt;
  end;
  PMUIP_DrawBackgroundBuffered = ^TMUIP_DrawBackgroundBuffered;

  TMUIP_GoActive = record
    MethodID: LongWord;  // MUIM_GoActive
  end;
  PMUIP_GoActive = ^TMUIP_GoActive;

  TMUIP_GoInactive = record
    MethodID: LongWord;  // MUIM_GoInactive
  end;
  PMUIP_GoInactive = ^TMUIP_GoInactive;

  TMUIP_HandleEvent = record
    MethodID: LongWord;  // MUIM_HandleEvent
    imsg: PIntuiMessage;
    muikey: LongInt;
  end;
  PMUIP_HandleEvent = ^TMUIP_HandleEvent;

  TMUIP_HandleInput = record
    MethodID: LongWord;  // MUIM_HandleInput
    imsg: PIntuiMessage;
    muikey: LongInt;
  end;
  PMUIP_HandleInput = ^TMUIP_HandleInput;

  TMUIP_Hide = record
    MethodID: LongWord;  // MUIM_Hide
  end;
  PMUIP_Hide = ^TMUIP_Hide;

  TMUIP_Setup = record
    MethodID: LongWord;  // MUIM_Setup
    RenderInfo: PMUI_RenderInfo;
  end;
  PMUIP_Setup = ^TMUIP_Setup;

  TMUIP_Show = record
    MethodID: LongWord;  // MUIM_Show
  end;
  PMUIP_Show = ^TMUIP_Show;

  TMUIP_Layout = record
    MethodID: LongWord;  // MUIM_Layout
  end;
  PMUIP_Layout = ^TMUIP_Layout;

  TMUIP_DrawParentBackground = record
    MethodID: LongWord;  // MUIM_DrawParentBackground
    left: LongInt;
    top: LongInt;
    width: LongInt;
    height: LongInt;
    xoffset: LongInt;
    yoffset: LongInt;
    flags: LongInt;
  end;
  PMUIP_DrawParentBackground = ^TMUIP_DrawParentBackground;

const
  MUIF_DRAGIMAGE_HASMASK     = 1 shl 0; // Use provided mask for drawing Not supported at the moment
  MUIF_DRAGIMAGE_SOURCEALPHA = 1 shl 1; // Use drag image source alpha information for transparent drawing

// Attributes
const
  MUIA_Background         = MUIB_MUI or $42545b; // V4  is. LongInt
  MUIA_BottomEdge         = MUIB_MUI or $42e552; // V4  ..g LongInt
  MUIA_ContextMenu        = MUIB_MUI or $42b704; // V11 isg PObject
  MUIA_ContextMenuTrigger = MUIB_MUI or $42a2c1; // V11 ..g PObject
  MUIA_ControlChar        = MUIB_MUI or $42120b; // V4  isg Char
  MUIA_CycleChain         = MUIB_MUI or $421ce7; // V11 isg LongInt
  MUIA_Disabled           = MUIB_MUI or $423661; // V4  isg WordBool
  MUIA_DoubleBuffer       = MUIB_MUI or $42a9c7; // V20 isg WordBool
  MUIA_Draggable          = MUIB_MUI or $420b6e; // V11 isg WordBool
  MUIA_Dropable           = MUIB_MUI or $42fbce; // V11 isg WordBool
  MUIA_FillArea           = MUIB_MUI or $4294a3; // V4  is. WordBool

  MUIA_FixHeight          = MUIB_MUI or $42a92b; // V4  i.. LongInt
  MUIA_FixHeightTxt       = MUIB_MUI or $4276f2; // V4  i.. STRPTR
  MUIA_FixWidth           = MUIB_MUI or $42a3f1; // V4  i.. LongInt
  MUIA_FixWidthTxt        = MUIB_MUI or $42d044; // V4  i.. STRPTR

  MUIA_Font               = MUIB_MUI or $42be50; // V4  i.g PTextFont
  MUIA_Frame              = MUIB_MUI or $42ac64; // V4  i.. LongInt
  MUIA_FramePhantomHoriz  = MUIB_MUI or $42ed76; // V4  i.. WordBool
  MUIA_FrameTitle         = MUIB_MUI or $42d1c7; // V4  i.. STRPTR

  MUIA_Height             = MUIB_MUI or $423237; // V4  ..g LongInt
  MUIA_HorizDisappear     = MUIB_MUI or $429615; // V11 isg LongInt
  MUIA_HorizWeight        = MUIB_MUI or $426db9; // V4  isg SmallInt

  MUIA_InnerBottom        = MUIB_MUI or $42f2c0; // V4  i.g LongInt
  MUIA_InnerLeft          = MUIB_MUI or $4228f8; // V4  i.g LongInt
  MUIA_InnerRight         = MUIB_MUI or $4297ff; // V4  i.g LongInt
  MUIA_InnerTop           = MUIB_MUI or $421eb6; // V4  i.g LongInt
  MUIA_InputMode          = MUIB_MUI or $42fb04; // V4  i.. LongInt

  MUIA_LeftEdge           = MUIB_MUI or $42bec6; // V4  ..g LongInt
  MUIA_MaxHeight          = MUIB_MUI or $4293e4; // V11 i.. LongInt
  MUIA_MaxWidth           = MUIB_MUI or $42f112; // V11 i.. LongInt

  MUIA_Pressed            = MUIB_MUI or $423535; // V4  ..g WordBool
  MUIA_RightEdge          = MUIB_MUI or $42ba82; // V4  ..g LongInt
  MUIA_Selected           = MUIB_MUI or $42654b; // V4  isg WordBool
  MUIA_ShortHelp          = MUIB_MUI or $428fe3; // V11 isg STRPTR
  MUIA_ShowMe             = MUIB_MUI or $429ba8; // V4  isg WordBool
  MUIA_ShowSelState       = MUIB_MUI or $42caac; // V4  i.. WordBool
  MUIA_Timer              = MUIB_MUI or $426435; // V4  ..g LongInt
  MUIA_TopEdge            = MUIB_MUI or $42509b; // V4  ..g LongInt
  MUIA_VertDisappear      = MUIB_MUI or $42d12f; // V11 isg LongInt
  MUIA_VertWeight         = MUIB_MUI or $4298d0; // V4  isg SmallInt
  MUIA_Weight             = MUIB_MUI or $421d1f; // V4  i.. SmallInt
  MUIA_Width              = MUIB_MUI or $42b59c; // V4  ..g LongInt
  MUIA_Window             = MUIB_MUI or $421591; // V4  ..g PWindow
  MUIA_WindowObject       = MUIB_MUI or $42669e; // V4  ..g PObject

  // AROS Special
  MUIA_NestedDisabled = MUIB_Area or $0; // Zune 20030530  isg WordBool

{$ifdef MUI_OBSOLETE}
  MUIA_ExportID = MUIB_MUI or $42d76e; // V4  isg LongWord
{$endif}

type
  TMUI_ImageSpec_intern = record
  end;
  PMUI_ImageSpec_intern = ^TMUI_ImageSpec_intern;

type
  { (partial) instance data of area class  }
  TMUI_AreaData = record
    mad_RenderInfo : PMUI_RenderInfo;      // RenderInfo for this object
    mad_Background: PMUI_ImageSpec_intern; // bg setting - private
    mad_Font: PTextFont;                  // Font which is used to draw
    mad_MinMax: TMUI_MinMax;              // min/max/default sizes
    mad_Box: TIBox;                       // coordinates and dim of this object after layout
    mad_addleft: ShortInt;                // left offset (frame & innerspacing)
    mad_addtop: ShortInt;                 // top offset (frame & innerspacing)
    mad_subwidth: ShortInt;               // additional width (frame & innerspacing)
    mad_subheight: ShortInt;              // additional height (frame & innerspacing)
    mad_Flags: LongWord;                  // see definitions below
    mad_Flags2: LongWord;
    // 40 bytes up to here
    // The following data is private
  end;
  PMUI_AreaData = ^TMUI_AreaData;

const
  // mad_Flags Flags during MUIM_Draw
  MADF_DRAWOBJECT = 1 shl 0;
  MADF_DRAWUPDATE = 1 shl 1; // only update yourself
  MADF_DRAWALL    = 1 shl 31;

  MUIV_Frame_None        = 0;
  MUIV_Frame_Button      = 1;
  MUIV_Frame_ImageButton = 2;
  MUIV_Frame_Text        = 3;
  MUIV_Frame_String      = 4;
  MUIV_Frame_ReadList    = 5;
  MUIV_Frame_InputList   = 6;
  MUIV_Frame_Prop        = 7;
  MUIV_Frame_Gauge       = 8;
  MUIV_Frame_Group       = 9;
  MUIV_Frame_PopUp       = 10;
  MUIV_Frame_Virtual     = 11;
  MUIV_Frame_Slider      = 12;
  MUIV_Frame_Knob        = 13;
  MUIV_Frame_Drag        = 14;
  MUIV_Frame_Count       = 15;
  MUIV_Frame_Register    = 21;

  MUIV_InputMode_None      = 0; // $00
  MUIV_InputMode_RelVerify = 1; // $40 (1 shl 6)
  MUIV_InputMode_Immediate = 2; // $80 (1 shl 7)
  MUIV_InputMode_Toggle    = 3; // $c0 (1 shl 7) or (1 shl 6)

// *********************************************************************
//  Group
const
  MUIC_Group: PChar = 'Group.mui';

// Identifier base (for Zune extensions)
  MUIB_Group = MUIB_ZUNE or $00001000;

// Methods
const
  MUIM_Group_AddHead    = MUIB_MUI or $42e200; // V8
  MUIM_Group_AddTail    = MUIB_MUI or $42d752; // V8
  MUIM_Group_ExitChange = MUIB_MUI or $42d1cc; // V11
  MUIM_Group_InitChange = MUIB_MUI or $420887; // V11
  MUIM_Group_Sort       = MUIB_MUI or $427417; // V4
  MUIM_Group_Remove     = MUIB_MUI or $42f8a9; // V8
  // AROS special
  MUIM_Group_DoMethodNoForward = MUIB_Group or $0;

type
  TMUIP_Group_AddHead = record
    MethodID: LongWord;  // MUIM_Group_AddHead
    obj: PObject_;
  end;
  PMUIP_Group_AddHead = ^TMUIP_Group_AddHead;

  TMUIP_Group_AddTail = record
    MethodID : LongWord;  // MUIM_Group_AddTail
    obj: PObject_;
  end;
  PMUIP_Group_AddTail = ^TMUIP_Group_AddTail;

  TMUIP_Group_ExitChange = record
    MethodID : LongWord;  // MUIM_Group_ExitChange
  end;
  PMUIP_Group_ExitChange = ^TMUIP_Group_ExitChange;

  TMUIP_Group_InitChange = record
    MethodID: LongWord;  // MUIM_Group_InitChange
  end;
  PMUIP_Group_InitChange = ^TMUIP_Group_InitChange;

  TMUIP_Group_Sort = record
    MethodID: LongWord;  // MUIM_Group_Sort
    obj: array[0..0] of PObject_;
  end;
  PMUIP_Group_Sort = ^TMUIP_Group_Sort;

  TMUIP_Group_Remove = record
    MethodID : LongWord;  // MUIM_Group_Remove
    obj: PObject_;
  end;
  PMUIP_Group_Remove = ^TMUIP_Group_Remove;

  TMUIP_Group_DoMethodNoForward = record
    MethodID: LongWord;  // MUIM_Group_DoMethodNoForward
    DoMethodID: LongWord;
  end; // msg stuff follows
  PMUIP_Group_DoMethodNoForward = ^TMUIP_Group_DoMethodNoForward;

// Attributes
const
  MUIA_Group_ActivePage   = MUIB_MUI or $424199; // V5  isg LongInt
  MUIA_Group_Child        = MUIB_MUI or $4226e6; // V4  i.. PObject_
  MUIA_Group_ChildList    = MUIB_MUI or $424748; // V4  ..g PList
  MUIA_Group_Columns      = MUIB_MUI or $42f416; // V4  is. LongInt
  MUIA_Group_Forward      = MUIB_MUI or $421422; // V11 .s. WordBool
  MUIA_Group_Horiz        = MUIB_MUI or $42536b; // V4  i.. WordBool
  MUIA_Group_HorizSpacing = MUIB_MUI or $42c651; // V4  isg LongInt
  MUIA_Group_LayoutHook   = MUIB_MUI or $42c3b2; // V11 i.. PHook
  MUIA_Group_PageMode     = MUIB_MUI or $421a5f; // V5  i.. WordBool
  MUIA_Group_Rows         = MUIB_MUI or $42b68f; // V4  is. LongInt
  MUIA_Group_SameHeight   = MUIB_MUI or $42037e; // V4  i.. WordBool
  MUIA_Group_SameSize     = MUIB_MUI or $420860; // V4  i.. WordBool
  MUIA_Group_SameWidth    = MUIB_MUI or $42b3ec; // V4  i.. WordBool
  MUIA_Group_Spacing      = MUIB_MUI or $42866d; // V4  is. LongInt
  MUIA_Group_VertSpacing  = MUIB_MUI or $42e1bf; // V4  isg LongInt
  // AROS special
  MUIA_Group_Virtual      = MUIB_Group or $0; // Zune V1 i.. WordBool

  MUIV_Group_ActivePage_First   = 0;
  MUIV_Group_ActivePage_Last    = -1;
  MUIV_Group_ActivePage_Prev    = -2;
  MUIV_Group_ActivePage_Next    = -3;
  MUIV_Group_ActivePage_Advance = -4;

type
  // This is the message you get if your custom layout hook is called
  TMUI_LayoutMsg = record
    lm_Type: LongWord;      // type of message (see defines below)
    lm_Children: PMinList;  // list of this groups children, traverse with NextObject()
    lm_MinMax: TMUI_MinMax; // results for MUILM_MINMAX
    lm_Layout: record       // size (and result) for MUILM_LAYOUT
       Width: LongInt;
       Height: LongInt;
       priv5: LongWord;
       priv6: LongWord;
    end;
  end;
  PMUI_LayoutMsg = ^TMUI_LayoutMsg;

const
  MUILM_MINMAX  = 1;  // MUI wants you to calc your min & max sizes
  MUILM_LAYOUT  = 2;  // MUI wants you to layout your children
  MUILM_UNKNOWN = -1; // return this if your hook doesn't implement lm_Type

// *********************************************************************
//  Rectangle
const
  MUIC_Rectangle: PChar = 'Rectangle.mui';

// Identifier base (for Zune extensions)
  MUIB_Rectangle = MUIB_ZUNE or $00002b00;

// Attributes
const
  MUIA_Rectangle_BarTitle = MUIB_MUI or $426689; // V11 i.g STRPTR
  MUIA_Rectangle_HBar     = MUIB_MUI or $42c943; // V7  i.g WordBool
  MUIA_Rectangle_VBar     = MUIB_MUI or $422204; // V7  i.g WordBool

// *********************************************************************
//  Text
const
  MUIC_Text: PChar = 'Text.mui';

// Identifier base (for Zune extensions)
  MUIB_Text = MUIB_ZUNE or $00003500;

// Attributes
const
  MUIA_Text_Contents  = MUIB_MUI or $42f8dc; // V4  isg STRPTR
  MUIA_Text_HiChar    = MUIB_MUI or $4218ff; // V4  i.. Char
  MUIA_Text_HiCharIdx = MUIB_MUI or $4214f5; //     i.. Char
  MUIA_Text_PreParse  = MUIB_MUI or $42566d; // V4  isg STRPTR
  MUIA_Text_SetMax    = MUIB_MUI or $424d0a; // V4  i.. WordBool
  MUIA_Text_SetMin    = MUIB_MUI or $424e10; // V4  i.. WordBool
  MUIA_Text_SetVMax   = MUIB_MUI or $420d8b; // V11 i.. WordBool

// Control codes for text strings
  MUIX_R  = #27+'r'; // right justified
  MUIX_C  = #27+'c'; // centered
  MUIX_L  = #27+'l'; // left justified

  MUIX_N  = #27+'n'; // normal
  MUIX_B  = #27+'b'; // bold
  MUIX_I  = #27+'i'; // italic
  MUIX_U  = #27+'u'; // underlined

  MUIX_PT = #27+'2'; // text pen
  MUIX_PH = #27+'8'; // highlight text pen

// *********************************************************************
//  Numeric
const
  MUIC_Numeric: PChar = 'Numeric.mui';

// Identifier base (for Zune extensions)
  MUIB_Numeric = MUIB_ZUNE or $00001e00;

// Methods
const
  MUIM_Numeric_Decrease     = MUIB_MUI or $4243a7; // V11
  MUIM_Numeric_Increase     = MUIB_MUI or $426ecd; // V11
  MUIM_Numeric_ScaleToValue = MUIB_MUI or $42032c; // V11
  MUIM_Numeric_SetDefault   = MUIB_MUI or $42ab0a; // V11
  MUIM_Numeric_Stringify    = MUIB_MUI or $424891; // V11
  MUIM_Numeric_ValueToScale = MUIB_MUI or $423e4f; // V11
  // AROS special
  MUIM_Numeric_ValueToScaleExt = MUIB_Numeric or $0; // ZUNE only

type
  TMUIP_Numeric_Decrease = record
    MethodID: LongWord;  // MUIM_Numeric_Decrease
    amount: LongInt;
  end;
  PMUIP_Numeric_Decrease = ^TMUIP_Numeric_Decrease;

  TMUIP_Numeric_Increase = record
    MethodID: LongWord;  // MUIM_Numeric_Increase
    amount: LongInt;
  end;
  PMUIP_Numeric_Increase = ^TMUIP_Numeric_Increase;

  TMUIP_Numeric_ScaleToValue = record
    MethodID: LongWord;  // MUIM_Numeric_ScaleToValue
    scalemin: LongInt;
    scalemax: LongInt;
    scale: LongInt;
  end;
  PMUIP_Numeric_ScaleToValue = ^TMUIP_Numeric_ScaleToValue;

  TMUIP_Numeric_SetDefault = record
    MethodID: LongWord;  // MUIM_Numeric_SetDefault
  end;
  PMUIP_Numeric_SetDefault = ^TMUIP_Numeric_SetDefault;

  TMUIP_Numeric_Stringify = record
    MethodID: LongWord;  // MUIM_Numeric_Stringify
    value: LongInt;
  end;
  PMUIP_Numeric_Stringify = ^TMUIP_Numeric_Stringify;

  TMUIP_Numeric_ValueToScale = record
    MethodID: LongWord;  // MUIM_Numeric_ValueToScale
    scalemin: LongInt;
    scalemax: LongInt;
  end;
  PMUIP_Numeric_ValueToScale = ^TMUIP_Numeric_ValueToScale;

  TMUIP_Numeric_ValueToScaleExt = record
    MethodID: LongWord;  // MUIM_Numeric_ValueToScaleExt
    value: LongInt;
    scalemin: LongInt;
    scalemax: LongInt;
  end;
  PMUIP_Numeric_ValueToScaleExt = ^TMUIP_Numeric_ValueToScaleExt;

// Attributes
const
  MUIA_Numeric_CheckAllSizes = MUIB_MUI or $421594; // V11 isg WordBool
  MUIA_Numeric_Default       = MUIB_MUI or $4263e8; // V11 isg LongInt
  MUIA_Numeric_Format        = MUIB_MUI or $4263e9; // V11 isg STRPTR
  MUIA_Numeric_Max           = MUIB_MUI or $42d78a; // V11 isg LongInt
  MUIA_Numeric_Min           = MUIB_MUI or $42e404; // V11 isg LongInt
  MUIA_Numeric_Reverse       = MUIB_MUI or $42f2a0; // V11 isg WordBool
  MUIA_Numeric_RevLeftRight  = MUIB_MUI or $4294a7; // V11 isg WordBool
  MUIA_Numeric_RevUpDown     = MUIB_MUI or $4252dd; // V11 isg WordBool
  MUIA_Numeric_Value         = MUIB_MUI or $42ae3a; // V11 isg LongInt

// *********************************************************************
//  Slider
const
  MUIC_Slider: PChar = 'Slider.mui';

// Identifier base (for Zune extensions)
  MUIB_Slider = MUIB_ZUNE or $00003300;

// Attributes
const
   MUIA_Slider_Horiz = $8042fad1;{ V11 isg BOOL               }
   MUIA_Slider_Quiet = $80420b26;{ V6  i.. BOOL               }

{$ifdef MUI_OBSOLETE}
  MUIA_Slider_Level   = MUIB_MUI or $42ae3a; // V4  isg LongInt
  MUIA_Slider_Max     = MUIB_MUI or $42d78a; // V4  isg LongInt
  MUIA_Slider_Min     = MUIB_MUI or $42e404; // V4  isg LongInt
  MUIA_Slider_Reverse = MUIB_MUI or $42f2a0; // V4  isg WordBool
{$endif}

// *********************************************************************
//  String
const
  MUIC_String: PChar = 'String.mui';

// Identifier base (for Zune extensions)
  MUIB_String = MUIB_ZUNE or $00003400;

// Attributes
const
  MUIA_String_Accept         = MUIB_MUI or $42e3e1; // V4  isg STRPTR
  MUIA_String_Acknowledge    = MUIB_MUI or $42026c; // V4  ..g STRPTR
  MUIA_String_AdvanceOnCR    = MUIB_MUI or $4226de; // V11 isg WordBool
  MUIA_String_AttachedList   = MUIB_MUI or $420fd2; // V4  isg PObject_
  MUIA_String_BufferPos      = MUIB_MUI or $428b6c; // V4  .sg LongInt
  MUIA_String_Contents       = MUIB_MUI or $428ffd; // V4  isg STRPTR
  MUIA_String_DisplayPos     = MUIB_MUI or $42ccbf; // V4  .sg LongInt
  MUIA_String_EditHook       = MUIB_MUI or $424c33; // V7  isg PHook
  MUIA_String_Format         = MUIB_MUI or $427484; // V4  i.g LongInt
  MUIA_String_Integer        = MUIB_MUI or $426e8a; // V4  isg LongWord
  MUIA_String_LonelyEditHook = MUIB_MUI or $421569; // V11 isg WordBool
  MUIA_String_MaxLen         = MUIB_MUI or $424984; // V4  i.g LongInt
  MUIA_String_Reject         = MUIB_MUI or $42179c; // V4  isg STRPTR
  MUIA_String_Secret         = MUIB_MUI or $428769; // V4  i.g WordBool

  MUIV_String_Format_Left   = 0;
  MUIV_String_Format_Center = 1;
  MUIV_String_Format_Right  = 2;


// Extended features taken over from Alan Odgaard's BetterString MCC.
// Attribute and method IDs match those of BetterString class.
  MUIA_String_Columns      = $ad001005; // Zune
  MUIA_String_NoInput      = $ad001007; // Zune
  MUIA_String_SelectSize   = $ad001001; // Zune
  MUIA_String_StayActive   = $ad001003; // Zune
  MUIA_String_KeyUpFocus   = $ad001008; // Zune
  MUIA_String_KeyDownFocus = $ad001009; // Zune

  MUIM_String_ClearSelected = $ad001004; // Zune
  MUIM_String_FileNameStart = $ad001006; // Zune
  MUIM_String_Insert        = $ad001002; // Zune

  MUIV_String_Insert_StartOfString = $00000000; // Zune
  MUIV_String_Insert_EndOfString   = $fffffffe; // Zune
  MUIV_String_Insert_BufferPos     = $ffffffff; // Zune
  MUIV_String_BufferPos_End        = $ffffffff; // Zune

  MUIR_String_FileNameStart_Volume = $ffffffff; // Zune

type
  TMUIP_String_ClearSelected = record
    MethodID: LongWord;  // MUIM_String_ClearSelected
  end;
  PMUIP_String_ClearSelected = ^TMUIP_String_ClearSelected;

  TMUIP_String_FileNameStart = record
    MethodID: LongWord;  // MUIM_String_FileNameStart
    buffer: STRPTR;
    pos: LongInt;
  end;

  TMUIP_String_Insert = record
    MethodID: LongWord;
    text: STRPTR;  // MUIM_String_Insert
    pos: LongInt;
  end;

// *********************************************************************
//  Boopsi
const
  MUIC_Boopsi: PChar = 'Boopsi.mui';

// Identifier base (for Zune extensions)
  MUIB_Boopsi = MUIB_ZUNE or $00000600;

// Attributes
const
  MUIA_Boopsi_Class       = MUIB_MUI or $426999; // V4  isg PIClass
  MUIA_Boopsi_ClassID     = MUIB_MUI or $42bfa3; // V4  isg PChar
  MUIA_Boopsi_MaxHeight   = MUIB_MUI or $42757f; // V4  isg LongWord
  MUIA_Boopsi_MaxWidth    = MUIB_MUI or $42bcb1; // V4  isg LongWord
  MUIA_Boopsi_MinHeight   = MUIB_MUI or $422c93; // V4  isg LongWord
  MUIA_Boopsi_MinWidth    = MUIB_MUI or $428fb2; // V4  isg LongWord
  MUIA_Boopsi_Object      = MUIB_MUI or $420178; // V4  ..g PObject_
  MUIA_Boopsi_Remember    = MUIB_MUI or $42f4bd; // V4  i.. LongWord
  MUIA_Boopsi_Smart       = MUIB_MUI or $42b8d7; // V9  i.. BOOL
  MUIA_Boopsi_TagDrawInfo = MUIB_MUI or $42bae7; // V4  isg LongWord
  MUIA_Boopsi_TagScreen   = MUIB_MUI or $42bc71; // V4  isg LongWord
  MUIA_Boopsi_TagWindow   = MUIB_MUI or $42e11d; // V4  isg LongWord

// *********************************************************************
//  Prop
const
  MUIC_Prop: PChar = 'Prop.mui';

// Identifier base (for Zune extensions)
  MUIB_Prop = MUIB_ZUNE or $00002900;

// Methods
const
  MUIM_Prop_Decrease = MUIB_MUI or $420dd1; // V16
  MUIM_Prop_Increase = MUIB_MUI or $42cac0; // V16

type
  TMUIP_Prop_Decrease = record
    MethodID: LongWord;  // MUIM_Prop_Decrease
    amount: LongInt;
  end;
  PMUIP_Prop_Decrease = ^TMUIP_Prop_Decrease;

  TMUIP_Prop_Increase = record
    MethodID: LongWord; // MUIM_Prop_Increase
    amount: LongInt;
  end;
  PMUIP_Prop_Increase = ^TMUIP_Prop_Increase;

// Attributes
const
  MUIA_Prop_Entries      = $8042fbdb; // V4  isg LongInt
  MUIA_Prop_First        = $8042d4b2; // V4  isg LongInt
  MUIA_Prop_Horiz        = $8042f4f3; // V4  i.g WordBool
  MUIA_Prop_Slider       = $80429c3a; // V4  isg WordBool
  MUIA_Prop_UseWinBorder = $8042deee; // V13 i.. LongInt
  MUIA_Prop_Visible      = $8042fea6; // V4  isg LongInt

  MUIV_Prop_UseWinBorder_None   = 0;
  MUIV_Prop_UseWinBorder_Left   = 1;
  MUIV_Prop_UseWinBorder_Right  = 2;
  MUIV_Prop_UseWinBorder_Bottom = 3;

  MUIA_Prop_DeltaFactor  = MUIB_MUI or $427c5e; //    is. LongInt
  MUIA_Prop_DoSmooth     = MUIB_MUI or $4236ce; // V4 i.. LongInt

// *********************************************************************
//  Scrollbar
const
  MUIC_Scrollbar: PChar = 'Scrollbar.mui';

// Identifier base (for Zune extensions)
  MUIB_Scrollbar = MUIB_ZUNE or $00002e00;

// Attributes
const
   MUIA_Scrollbar_Type = MUIB_MUI or $42fb6b; // V11 i.. LongInt

   MUIV_Scrollbar_Type_Default = 0;
   MUIV_Scrollbar_Type_Bottom  = 1;
   MUIV_Scrollbar_Type_Top     = 2;
   MUIV_Scrollbar_Type_Sym     = 3;

// *********************************************************************
//  Register
const
  MUIC_Register: PChar = 'Register.mui';

// Identifier base  (for Zune extensions)
  MUIB_Register = MUIB_ZUNE or $00002c00;

// Attributes
const
  MUIA_Register_Frame  = MUIB_MUI or $42349b; // V7  i.g BOOL
  MUIA_Register_Titles = MUIB_MUI or $4297ec; // V7  i.g STRPTR
  // AROS special
  MUIA_Register_Columns = MUIB_Register or $0; // Zune V1  i..

// *********************************************************************
//  Menustrip
const
  MUIC_Menustrip: PChar = 'Menustrip.mui';

// Identifier base (for Zune extensions)
  MUIB_Menustrip = MUIB_ZUNE or $00001a00;

// Attributes
const
  MUIA_Menustrip_Enabled = MUIB_MUI or $42815b; // V8  isg WordBool

// *********************************************************************
//  Menu
const
  MUIC_Menu: PChar = 'Menu.mui';

// Identifier base (for Zune extensions)
  MUIB_Menu = MUIB_ZUNE or $00001b00;

// Attributes  }
const
   MUIA_Menu_Enabled = MUIB_MUI or $42ed48; // V8  isg WordBool
   MUIA_Menu_Title   = MUIB_MUI or $42a0e3; // V8  isg STRPTR

// *********************************************************************
//  Menuitem
const
  MUIC_Menuitem: PChar = 'Menuitem.mui';

// Identifier base (for Zune extensions)
  MUIB_Menuitem = MUIB_ZUNE or $00001c00;

// Attributes
const
  MUIA_Menuitem_Checked       = MUIB_MUI or $42562a; // V8  isg WordBool
  MUIA_Menuitem_Checkit       = MUIB_MUI or $425ace; // V8  isg WordBool
  MUIA_Menuitem_CommandString = MUIB_MUI or $42b9cc; // V16 isg WordBool
  MUIA_Menuitem_Enabled       = MUIB_MUI or $42ae0f; // V8  isg WordBool
  MUIA_Menuitem_Exclude       = MUIB_MUI or $420bc6; // V8  isg LongInt
  MUIA_Menuitem_Shortcut      = MUIB_MUI or $422030; // V8  isg STRPTR
  MUIA_Menuitem_Title         = MUIB_MUI or $4218be; // V8  isg STRPTR
  MUIA_Menuitem_Toggle        = MUIB_MUI or $424d5c; // V8  isg WordBool
  MUIA_Menuitem_Trigger       = MUIB_MUI or $426f32; // V8  ..g PMenuItem
  // AROS special
  MUIA_Menuitem_NewMenu       = MUIB_Menuitem or $0; // Zune: V1 ..g PNewMenu

  MUIV_Menuitem_Shortcut_Check = -1;

// *********************************************************************
//  Dataspace
const
  MUIC_Dataspace: PChar = 'Dataspace.mui';

// Identifier base (for Zune extensions)
  MUIB_Dataspace = MUIB_ZUNE or $00000b00;

// Methods
const
  MUIM_Dataspace_Add      = MUIB_MUI or $423366; // V11
  MUIM_Dataspace_Clear    = MUIB_MUI or $42b6c9; // V11
  MUIM_Dataspace_Find     = MUIB_MUI or $42832c; // V11
  MUIM_Dataspace_Merge    = MUIB_MUI or $423e2b; // V11
  MUIM_Dataspace_ReadIFF  = MUIB_MUI or $420dfb; // V11
  MUIM_Dataspace_Remove   = MUIB_MUI or $42dce1; // V11
  MUIM_Dataspace_WriteIFF = MUIB_MUI or $425e8e; // V11

type
  TMUIP_Dataspace_Add = record
    MethodID: LongWord;  // MUIM_Dataspace_Add
    data: APTR;
    len: LongInt;
    id: LongWord;
  end;
  PMUIP_Dataspace_Add = ^TMUIP_Dataspace_Add;

  TMUIP_Dataspace_Clear = record
    MethodID: LongWord;  // MUIM_Dataspace_Clear
  end;
  PMUIP_Dataspace_Clear = ^TMUIP_Dataspace_Clear;

  TMUIP_Dataspace_Find = record
    MethodID: LongWord;  // MUIM_Dataspace_Find
    id: LongWord;
  end;
  PMUIP_Dataspace_Find = ^TMUIP_Dataspace_Find;

  TMUIP_Dataspace_Merge = record
    MethodID: LongWord;  // MUIM_Dataspace_Merge
    dataspace: PObject_;
  end;
  PMUIP_Dataspace_Merge = ^TMUIP_Dataspace_Merge;

  TMUIP_Dataspace_ReadIFF = record
    MethodID: LongWord;  // MUIM_Dataspace_ReadIFF
    Handle: PIFFHandle;
  end;
  PMUIP_Dataspace_ReadIFF = ^TMUIP_Dataspace_ReadIFF;

  TMUIP_Dataspace_Remove = record
    MethodID: LongWord;  // MUIM_Dataspace_Remove
    id: LongWord;
  end;
  PMUIP_Dataspace_Remove = ^TMUIP_Dataspace_Remove;

  TMUIP_Dataspace_WriteIFF = record
    MethodID: LongWord;  // MUIM_Dataspace_WriteIFF
    handle: PIFFHandle;
    type_: LongWord;
    id: LongWord;
  end;
  PMUIP_Dataspace_WriteIFF = ^TMUIP_Dataspace_WriteIFF;

// Attributes
const
  MUIA_Dataspace_Pool = MUIB_MUI or $424cf9; // V11 i.. APTR

// *********************************************************************
//  Virtgroup
const
  MUIC_Virtgroup: PChar = 'Virtgroup.mui';

// Identifier base (for Zune extensions)
  MUIB_Virtgroup = MUIB_ZUNE or $00003700;

// Attributes
const
  MUIA_Virtgroup_Height = MUIB_MUI or $423038; // V6  ..g LongInt
  MUIA_Virtgroup_Input  = MUIB_MUI or $427f7e; // V11 i.. WordBool
  MUIA_Virtgroup_Left   = MUIB_MUI or $429371; // V6  isg LongInt
  MUIA_Virtgroup_Top    = MUIB_MUI or $425200; // V6  isg LongInt
  MUIA_Virtgroup_Width  = MUIB_MUI or $427c49; // V6  ..g LongInt

// *********************************************************************
//  Scrollgroup
const
  MUIC_Scrollgroup: PChar = 'Scrollgroup.mui';

// Identifier base (for Zune extensions)
  MUIB_Scrollgroup = MUIB_ZUNE or $00002f00;

// Attributes
const
  MUIA_Scrollgroup_Contents     = MUIB_MUI or $421261; // V4  i.g PObject_
  MUIA_Scrollgroup_FreeHoriz    = MUIB_MUI or $4292f3; // V9  i.. WordBool
  MUIA_Scrollgroup_FreeVert     = MUIB_MUI or $4224f2; // V9  i.. WordBool
  MUIA_Scrollgroup_HorizBar     = MUIB_MUI or $42b63d; // V16 ..g PObject_
  MUIA_Scrollgroup_UseWinBorder = MUIB_MUI or $4284c1; // V13 i.. WordBool
  MUIA_Scrollgroup_VertBar      = MUIB_MUI or $42cdc0; // V16 ..g PObject_

// *********************************************************************
//  ScrollButton
const
  MUIC_Scrollbutton: PChar = 'Scrollbutton.mui';

// Identifier base
  MUIB_Scrollbutton = MUIB_ZUNE or $00004100;

// Attributes
const
  MUIA_Scrollbutton_NewPosition = MUIB_Scrollbutton or $0; // --G  LongWord (2 x SmallInt)
  MUIA_Scrollbutton_Horiz       = MUIB_Scrollbutton or $1; // -SG  SmallInt
  MUIA_Scrollbutton_Vert        = MUIB_Scrollbutton or $2; // -SG  SmallInt
  MUIA_Scrollbutton_HorizProp   = MUIB_Scrollbutton or $3; // --G  PObject_
  MUIA_Scrollbutton_VertProp    = MUIB_Scrollbutton or $4; // --G  PObject_

// *********************************************************************
//  Semaphore
const
  MUIC_Semaphore: PChar = 'Semaphore.mui';

// Identifier base (for Zune extensions)
  MUIB_Semaphore = MUIB_ZUNE or $00003000;

// Methods
const
  MUIM_Semaphore_Attempt       = MUIB_MUI or $426ce2; // V11
  MUIM_Semaphore_AttemptShared = MUIB_MUI or $422551; // V11
  MUIM_Semaphore_Obtain        = MUIB_MUI or $4276f0; // V11
  MUIM_Semaphore_ObtainShared  = MUIB_MUI or $42ea02; // V11
  MUIM_Semaphore_Release       = MUIB_MUI or $421f2d; // V11

type
  TMUIP_Semaphore_Attempt = record
    MethodID: LongWord;  // MUIM_Semaphore_Attempt
  end;
  PMUIP_Semaphore_Attempt = ^TMUIP_Semaphore_Attempt;

  TMUIP_Semaphore_AttemptShared = record
    MethodID: LongWord;  // MUIM_Semaphore_AttemptShared
  end;
  PMUIP_Semaphore_AttemptShared = ^TMUIP_Semaphore_AttemptShared;

  TMUIP_Semaphore_Obtain = record
    MethodID: LongWord;  // MUIM_Semaphore_Obtain
  end;
  PMUIP_Semaphore_Obtain = ^TMUIP_Semaphore_Obtain;

  TMUIP_Semaphore_ObtainShared = record
    MethodID: LongWord;  // MUIM_Semaphore_ObtainShared
  end;
  PMUIP_Semaphore_ObtainShared = ^TMUIP_Semaphore_ObtainShared;

  TMUIP_Semaphore_Release = record
    MethodID: LongWord;  // MUIM_Semaphore_Release
  end;
  PMUIP_Semaphore_Release = ^TMUIP_Semaphore_Release;

// *********************************************************************
//  Bitmap
const
  MUIC_Bitmap: PChar = 'Bitmap.mui';

// Identifier base (for Zune extensions)
  MUIB_Bitmap = MUIB_ZUNE or $00000400;

// Attributes
const
  MUIA_Bitmap_Alpha          = MUIB_MUI or $423e71; // V20 isg LongWord
  MUIA_Bitmap_Bitmap         = MUIB_MUI or $4279bd; // V8  isg PBitMap
  MUIA_Bitmap_Height         = MUIB_MUI or $421560; // V8  isg LongInt
  MUIA_Bitmap_MappingTable   = MUIB_MUI or $42e23d; // V8  isg Byte
  MUIA_Bitmap_Precision      = MUIB_MUI or $420c74; // V11 isg LongInt
  MUIA_Bitmap_RemappedBitmap = MUIB_MUI or $423a47; // V11 ..g PBitMap
  MUIA_Bitmap_SourceColors   = MUIB_MUI or $425360; // V8  isg LongWord
  MUIA_Bitmap_Transparent    = MUIB_MUI or $422805; // V8  isg LongInt
  MUIA_Bitmap_UseFriend      = MUIB_MUI or $4239d8; // V11 i.. WordBool
  MUIA_Bitmap_Width          = MUIB_MUI or $42eb3a; // V8  isg LongInt

// *********************************************************************
//  Bodychunk
const
   MUIC_Bodychunk : PChar = 'Bodychunk.mui';

// Identifier base (for Zune extensions)
  MUIB_Bodychunk = MUIB_ZUNE or $00000500;

// Attributes
const
  MUIA_Bodychunk_Body        = MUIB_MUI or $42ca67; // V8  isg Byte
  MUIA_Bodychunk_Compression = MUIB_MUI or $42de5f; // V8  isg Byte
  MUIA_Bodychunk_Depth       = MUIB_MUI or $42c392; // V8  isg LongInt
  MUIA_Bodychunk_Masking     = MUIB_MUI or $423b0e; // V8  isg Byte

// *********************************************************************
//  ChunkyImage
const
   MUIC_ChunkyImage : PChar = 'ChunkyImage.mui';

// Identifier base (for Zune extensions)
  MUIB_ChunkyImage = MUIB_ZUNE or $00004000;

// Attributes
const
  MUIA_ChunkyImage_Pixels    = MUIB_ChunkyImage or $0; // V8  isg PByte
  MUIA_ChunkyImage_Palette   = MUIB_ChunkyImage or $1; // V8  isg PByte
  MUIA_ChunkyImage_NumColors = MUIB_ChunkyImage or $2; // V8  isg LongInt
  MUIA_ChunkyImage_Modulo    = MUIB_ChunkyImage or $3; // V8  isg LongInt

// *********************************************************************
//  Listview
const
  MUIC_Listview: PChar = 'Listview.mui';

// Identifier base (for Zune extensions)
  MUIB_Listview = MUIB_ZUNE or $00001900;

// Attributes
const
  MUIA_Listview_ClickColumn    = MUIB_MUI or $42d1b3; // V7  ..g LongInt
  MUIA_Listview_DefClickColumn = MUIB_MUI or $42b296; // V7  isg LongInt
  MUIA_Listview_DoubleClick    = MUIB_MUI or $424635; // V4  i.g WordBool
  MUIA_Listview_DragType       = MUIB_MUI or $425cd3; // V11 isg LongInt
  MUIA_Listview_Input          = MUIB_MUI or $42682d; // V4  i.. WordBool
  MUIA_Listview_List           = MUIB_MUI or $42bcce; // V4  i.g PObject_
  MUIA_Listview_MultiSelect    = MUIB_MUI or $427e08; // V7  i.. LongInt
  MUIA_Listview_ScrollerPos    = MUIB_MUI or $42b1b4; // V10 i.. WordBool
  MUIA_Listview_SelectChange   = MUIB_MUI or $42178f; // V4  ..g WordBool

  MUIV_Listview_DragType_None      = 0;
  MUIV_Listview_DragType_Immediate = 1;

  MUIV_Listview_MultiSelect_None    = 0;
  MUIV_Listview_MultiSelect_Default = 1;
  MUIV_Listview_MultiSelect_Shifted = 2;
  MUIV_Listview_MultiSelect_Always  = 3;

  MUIV_Listview_ScrollerPos_Default = 0;
  MUIV_Listview_ScrollerPos_Left    = 1;
  MUIV_Listview_ScrollerPos_Right   = 2;
  MUIV_Listview_ScrollerPos_None    = 3;

// *********************************************************************
//  List
const
  MUIC_List: PChar = 'List.mui';

// Identifier base (for Zune extensions)
  MUIB_List = MUIB_ZUNE or $00001400;

// Methods
const
  MUIM_List_Clear        = MUIB_MUI or $42ad89; // V4
  MUIM_List_Compare      = MUIB_MUI or $421b68; // V20
  MUIM_List_Construct    = MUIB_MUI or $42d662; // V20
  MUIM_List_CreateImage  = MUIB_MUI or $429804; // V11
  MUIM_List_DeleteImage  = MUIB_MUI or $420f58; // V11
  MUIM_List_Exchange     = MUIB_MUI or $42468c; // V4
  MUIM_List_GetEntry     = MUIB_MUI or $4280ec; // V4
  MUIM_List_Insert       = MUIB_MUI or $426c87; // V4
  MUIM_List_InsertSingle = MUIB_MUI or $4254d5; // V7
  MUIM_List_Jump         = MUIB_MUI or $42baab; // V4
  MUIM_List_Move         = MUIB_MUI or $4253c2; // V9
  MUIM_List_NextSelected = MUIB_MUI or $425f17; // V6
  MUIM_List_Redraw       = MUIB_MUI or $427993; // V4
  MUIM_List_Remove       = MUIB_MUI or $42647e; // V4
  MUIM_List_Select       = MUIB_MUI or $4252d8; // V4
  MUIM_List_Sort         = MUIB_MUI or $422275; // V4
  MUIM_List_TestPos      = MUIB_MUI or $425f48; // V11

type
  TMUIP_List_Clear = record
    MethodID: LongWord;  // MUIM_List_Clear
  end;
  PMUIP_List_Clear = ^TMUIP_List_Clear;

  TMUIP_List_Compare = record
    MethodID: LongWord;  // MUIM_List_Compare
    entry1: APTR;
    entry2: APTR;
    sort_type1: LongInt;
    sort_type2: LongInt;
  end;
  PMUIP_List_Compare = ^TMUIP_List_Compare;

  TMUIP_List_Construct = record
    MethodID: LongWord;  // MUIM_List_Construct
    entry: APTR;
    pool: APTR;
  end;
  PMUIP_List_Construct = ^TMUIP_List_Construct;

  TMUIP_List_CreateImage = record
    MethodID: LongWord;  // MUIM_List_CreateImage
    obj: PObject_;
    flags: LongWord;
  end;
  PMUIP_List_CreateImage = ^TMUIP_List_CreateImage;

  TMUIP_List_DeleteImage = record
    MethodID: LongWord;  // MUIM_List_DeleteImage
    listimg: APTR;
  end;
  PMUIP_List_DeleteImage = ^TMUIP_List_DeleteImage;

  TMUIP_List_Exchange = record
    MethodID: LongWord;  // MUIM_List_Exchange
    pos1: LongInt;
    pos2: LongInt;
  end;
  PMUIP_List_Exchange = ^TMUIP_List_Exchange;

  TMUIP_List_GetEntry = record
    MethodID: LongWord;  // MUIM_List_GetEntry
    pos: LongInt;
    entry: PAPTR;
  end;
  PMUIP_List_GetEntry = ^TMUIP_List_GetEntry;

  TMUIP_List_Insert = record
    MethodID: LongWord;  // MUIM_List_Insert
    entries: PAPTR;
    count: LongInt;
    pos: LongInt;
  end;
  PMUIP_List_Insert = ^TMUIP_List_Insert;

  TMUIP_List_InsertSingle = record
    MethodID: LongWord;  // MUIM_List_InsertSingle
    entry: APTR;
    pos: LongInt;
  end;
  PMUIP_List_InsertSingle = ^TMUIP_List_InsertSingle;

  TMUIP_List_Jump = record
    MethodID: LongWord;  // MUIM_List_Jump
    pos: LongInt;
  end;
  PMUIP_List_Jump = ^TMUIP_List_Jump;

  TMUIP_List_Move = record
    MethodID: LongWord;  // MUIM_List_Move
    from: LongInt;
    too: LongInt;
  end;
  PMUIP_List_Move = ^TMUIP_List_Move;

  TMUIP_List_NextSelected = record
    MethodID: LongWord;  // MUIM_List_NextSelected
    pos: PLongInt;
  end;
  PMUIP_List_NextSelected = ^TMUIP_List_NextSelected;

  TMUIP_List_Redraw = record
    MethodID: LongWord;  // MUIM_List_Redraw
    pos: LongInt;
  end;
  PMUIP_List_Redraw = ^TMUIP_List_Redraw;

  TMUIP_List_Remove = record
    MethodID: LongWord;  // MUIM_List_Remove
    pos: LongInt;
  end;
  PMUIP_List_Remove = ^TMUIP_List_Remove;

  TMUIP_List_Select = record
    MethodID: LongWord;  // MUIM_List_Select
    pos: LongInt;
    seltype: LongInt;
    state: PLongInt;
  end;
  PMUIP_List_Select = ^TMUIP_List_Select;

  TMUIP_List_Sort = record
    MethodID: LongWord;  // MUIM_List_Sort
  end;
  PMUIP_List_Sort = ^TMUIP_List_Sort;

  TMUIP_List_TestPos = record
    MethodID: LongWord;  // MUIM_List_TestPos
    x: LongInt;
    y: LongInt;
    res: PMUI_List_TestPos_Result;
  end;
  PMUIP_List_TestPos = ^TMUIP_List_TestPos;

// Attributes
const
  MUIA_List_Active         = MUIB_MUI or $42391c; // V4  isg LongInt
  MUIA_List_AdjustHeight   = MUIB_MUI or $42850d; // V4  i.. WordBool
  MUIA_List_AdjustWidth    = MUIB_MUI or $42354a; // V4  i.. WordBool
  MUIA_List_AutoVisible    = MUIB_MUI or $42a445; // V11 isg WordBool
  MUIA_List_CompareHook    = MUIB_MUI or $425c14; // V4  is. PHook
  MUIA_List_ConstructHook  = MUIB_MUI or $42894f; // V4  is. PHook
  MUIA_List_DestructHook   = MUIB_MUI or $4297ce; // V4  is. PHook
  MUIA_List_DisplayHook    = MUIB_MUI or $42b4d5; // V4  is. PHook
  MUIA_List_DragSortable   = MUIB_MUI or $426099; // V11 isg WordBool
  MUIA_List_DropMark       = MUIB_MUI or $42aba6; // V11 ..g LongInt
  MUIA_List_Entries        = MUIB_MUI or $421654; // V4  ..g LongInt
  MUIA_List_First          = MUIB_MUI or $4238d4; // V4  ..g LongInt
  MUIA_List_Format         = MUIB_MUI or $423c0a; // V4  isg STRPTR
  MUIA_List_InsertPosition = MUIB_MUI or $42d0cd; // V9  ..g LongInt
  MUIA_List_MinLineHeight  = MUIB_MUI or $42d1c3; // V4  i.. LongInt
  MUIA_List_MultiTestHook  = MUIB_MUI or $42c2c6; // V4  is. PHook
  MUIA_List_Pool           = MUIB_MUI or $423431; // V13 i.. APTR
  MUIA_List_PoolPuddleSize = MUIB_MUI or $42a4eb; // V13 i.. LongWord
  MUIA_List_PoolThreshSize = MUIB_MUI or $42c48c; // V13 i.. LongWord
  MUIA_List_Quiet          = MUIB_MUI or $42d8c7; // V4  .s. WordBool
  MUIA_List_ShowDropMarks  = MUIB_MUI or $42c6f3; // V11 isg WordBool
  MUIA_List_SourceArray    = MUIB_MUI or $42c0a0; // V4  i.. APTR
  MUIA_List_Title          = MUIB_MUI or $423e66; // V6  isg PChar
  MUIA_List_Visible        = MUIB_MUI or $42191f; // V4  ..g LongInt

  MUIV_List_Active_Off      = -1;
  MUIV_List_Active_Top      = -2;
  MUIV_List_Active_Bottom   = -3;
  MUIV_List_Active_Up       = -4;
  MUIV_List_Active_Down     = -5;
  MUIV_List_Active_PageUp   = -6;
  MUIV_List_Active_PageDown = -7;

  MUIV_List_ConstructHook_String = -1;
  MUIV_List_DestructHook_String  = -1;

  MUIV_List_CopyHook_String = -1;

  MUIV_List_CursorType_None = 0;
  MUIV_List_CursorType_Bar  = 1;
  MUIV_List_CursorType_Rect = 2;

  MUIV_List_Insert_Top    = 0;
  MUIV_List_Insert_Active = -1;
  MUIV_List_Insert_Sorted = -2;
  MUIV_List_Insert_Bottom = -3;

  MUIV_List_Remove_First    = 0;
  MUIV_List_Remove_Active   = -1;
  MUIV_List_Remove_Last     = -2;
  MUIV_List_Remove_Selected = -3;

  MUIV_List_Select_Off    = 0;
  MUIV_List_Select_On     = 1;
  MUIV_List_Select_Toggle = 2;
  MUIV_List_Select_Ask    = 3;

  MUIV_List_GetEntry_Active  = -1;
  MUIV_List_EditEntry_Active = -1;
  MUIV_List_Select_Active    = -1;
  MUIV_List_Select_All       = -2;

  MUIV_List_Redraw_Active = -1;
  MUIV_List_Redraw_All    = -2;
  MUIV_List_Redraw_Entry  = -3;

  MUIV_List_Move_Top      = 0;
  MUIV_List_Move_Active   = -1;
  MUIV_List_Move_Bottom   = -2;
  MUIV_List_Move_Next     = -3; // only valid for second parameter
  MUIV_List_Move_Previous = -4; // only valid for second parameter

  MUIV_List_Exchange_Top      = 0;
  MUIV_List_Exchange_Active   = -1;
  MUIV_List_Exchange_Bottom   = -2;
  MUIV_List_Exchange_Next     = -3; // only valid for second parameter
  MUIV_List_Exchange_Previous = -4; // only valid for second parameter

  MUIV_List_Jump_Top    = 0;
  MUIV_List_Jump_Active = -1;
  MUIV_List_Jump_Bottom = -2;
  MUIV_List_Jump_Up     = -4;
  MUIV_List_Jump_Down   = -3;

  MUIV_List_NextSelected_Start = -1;
  MUIV_List_NextSelected_End   = -1;

  MUIV_NList_SelectChange_Flag_Multi = 1 shl 0;

// *********************************************************************
//  Scrmodelist
const
  MUIC_Scrmodelist: PChar = 'Scrmodelist.mui';

// Identifier base (for Zune extensions)
  MUIB_Scrmodelist = MUIB_ZUNE or $00001700;

// *********************************************************************
//  Floattext
const
  MUIC_Floattext: PChar = 'Floattext.mui';

// Identifier base (for Zune extensions)
  MUIB_Floattext = MUIB_ZUNE or $00001500;

// Methods
  MUIM_Floattext_Append = MUIB_MUI or $42a221; // V20

type
  TMUIP_Floattext_Append = record
    MethodID: LongWord;  // MUIM_Floattext_Append
    Text: STRPTR;
  end;
  PMUIP_Floattext_Append = ^TMUIP_Floattext_Append;

// Attributes
const
  MUIA_Floattext_Justify   = MUIB_MUI or $42dc03; // V4  isg WordBool
  MUIA_Floattext_SkiPChars = MUIB_MUI or $425c7d; // V4  is. STRPTR
  MUIA_Floattext_TabSize   = MUIB_MUI or $427d17; // V4  is. LongInt
  MUIA_Floattext_Text      = MUIB_MUI or $42d16a; // V4  isg STRPTR

// *********************************************************************
//  Popstring
const
  MUIC_Popstring: PChar = 'Popstring.mui';

// Identifier base (for Zune extensions)
  MUIB_Popstring = MUIB_ZUNE or $00002800;

// Methods
const
  MUIM_Popstring_Close = MUIB_MUI or $42dc52; // V7
  MUIM_Popstring_Open  = MUIB_MUI or $4258ba; // V7

type
  TMUIP_Popstring_Close = record
    MethodID: LongWord;  // MUIM_Popstring_Close
    result: LongInt;
  end;
  PMUIP_Popstring_Close = ^TMUIP_Popstring_Close;

  TMUIP_Popstring_Open = record
    MethodID: LongWord;  // MUIM_Popstring_Open
  end;
  PMUIP_Popstring_Open = ^TMUIP_Popstring_Open;

// Attributes
const
  MUIA_Popstring_Button    = MUIB_MUI or $42d0b9; // V7  i.g PObject_
  MUIA_Popstring_CloseHook = MUIB_MUI or $4256bf; // V7  isg PHook
  MUIA_Popstring_OpenHook  = MUIB_MUI or $429d00; // V7  isg PHook
  MUIA_Popstring_String    = MUIB_MUI or $4239ea; // V7  i.g PObject_
  MUIA_Popstring_Toggle    = MUIB_MUI or $422b7a; // V7  isg WordBool

// *********************************************************************
//  Popobject
const
  MUIC_Popobject: PChar = 'Popobject.mui';

// Identifier base
  MUIB_Popobject = MUIB_ZUNE or $00002400;

// Attributes
const
  MUIA_Popobject_Follow     = MUIB_MUI or $424cb5; // V7  isg WordBool
  MUIA_Popobject_Light      = MUIB_MUI or $42a5a3; // V7  isg WordBool
  MUIA_Popobject_Object     = MUIB_MUI or $4293e3; // V7  i.g PObject_
  MUIA_Popobject_ObjStrHook = MUIB_MUI or $42db44; // V7  isg PHook
  MUIA_Popobject_StrObjHook = MUIB_MUI or $42fbe1; // V7  isg PHook
  MUIA_Popobject_Volatile   = MUIB_MUI or $4252ec; // V7  isg WordBool
  MUIA_Popobject_WindowHook = MUIB_MUI or $42f194; // V9  isg PHook

// *********************************************************************
//  Cycle
const
  MUIC_Cycle: PChar = 'Cycle.mui';

// Identifier base (for Zune extensions)
  MUIB_Cycle = MUIB_ZUNE or $00000a00;

// Attributes
const
  MUIA_Cycle_Active  = MUIB_MUI or $421788; // V4  isg LongInt
  MUIA_Cycle_Entries = MUIB_MUI or $420629; // V4  i.. STRPTR

  MUIV_Cycle_Active_Next = -1;
  MUIV_Cycle_Active_Prev = -2;

// *********************************************************************
//  Gauge
const
  MUIC_Gauge: PChar = 'Gauge.mui';

// Identifier base (for Zune extensions)
  MUIB_Gauge = MUIB_ZUNE or $00000f00;

// Attributes
const
  MUIA_Gauge_Current  = MUIB_MUI or $42f0dd; // V4  isg LongInt
  MUIA_Gauge_Divide   = MUIB_MUI or $42d8df; // V4  isg WordBool
  MUIA_Gauge_Horiz    = MUIB_MUI or $4232dd; // V4  i.. WordBool
  MUIA_Gauge_InfoText = MUIB_MUI or $42bf15; // V7  isg STRPTR
  MUIA_Gauge_Max      = MUIB_MUI or $42bcdb; // V4  isg LongInt
  // AROS special
  MUIA_Gauge_DupInfoText = MUIB_Gauge or $0; // ZUNE: V1  i.. WordBool - defaults to False

// *********************************************************************
//  Image
const
  MUIC_Image: PChar = 'Image.mui';

// Identifier base (for Zune extensions)
  MUIB_Image = MUIB_ZUNE or $00001300;

// Attributes
const
  MUIA_Image_FontMatch       = MUIB_MUI or $42815d; // V4  i.. WordBool
  MUIA_Image_FontMatchHeight = MUIB_MUI or $429f26; // V4  i.. WordBool
  MUIA_Image_FontMatchWidth  = MUIB_MUI or $4239bf; // V4  i.. WordBool
  MUIA_Image_FreeHoriz       = MUIB_MUI or $42da84; // V4  i.. WordBool
  MUIA_Image_FreeVert        = MUIB_MUI or $42ea28; // V4  i.. WordBool
  MUIA_Image_OldImage        = MUIB_MUI or $424f3d; // V4  i.. PImage
  MUIA_Image_Spec            = MUIB_MUI or $4233d5; // V4  i.. PChar
  MUIA_Image_State           = MUIB_MUI or $42a3ad; // V4  is. LongInt

// *********************************************************************
//  Imagedisplay
const
  MUIC_Imagedisplay: PChar = 'Imagedisplay.mui';

// Identifier base (for Zune extensions)
  MUIB_Imagedisplay = MUIB_ZUNE or $00001200;

// Attributes
  MUIA_Imagedisplay_Spec       = MUIB_MUI or $42a547; // V11 isg PMUI_ImageSpec
  MUIA_Imagedisplay_UseDefSize = MUIB_MUI or $42186d; // V11 i.. WordBool undoc
  // Aros Special
  MUIA_Imagedisplay_FreeHoriz = MUIB_Imagedisplay or $0; // Zune 20030323 i.. WordBool default: True
  MUIA_Imagedisplay_FreeVert  = MUIB_Imagedisplay or $1; // Zune 20030323 i.. WordBool default: True

// *********************************************************************
//  Popasl
const
  MUIC_Popasl: PChar = 'Popasl.mui';

// Identifier base (for Zune extensions)
  MUIB_Popasl = MUIB_ZUNE or $00002100;

// Attributes
const
  MUIA_Popasl_Active    = MUIB_MUI or $421b37; // V7  ..g WordBool
  MUIA_Popasl_StartHook = MUIB_MUI or $42b703; // V7  isg PHook
  MUIA_Popasl_StopHook  = MUIB_MUI or $42d8d2; // V7  isg PHook
  MUIA_Popasl_Type      = MUIB_MUI or $42df3d; // V7  i.g LongWord

// *********************************************************************
//  Settingsgroup
const
  MUIC_Settingsgroup: PChar = 'Settingsgroup.mui';

// Identifier base (for Zune extensions)
  MUIB_Settingsgroup = MUIB_ZUNE or $00003100;

// Methods
const
  MUIM_Settingsgroup_ConfigToGadgets = MUIB_MUI or $427043; // V11
  MUIM_Settingsgroup_GadgetsToConfig = MUIB_MUI or $425242; // V11

type
  TMUIP_Settingsgroup_ConfigToGadgets = record
    MethodID: LongWord;  // MUIM_Settingsgroup_ConfigToGadgets
    configdata: PObject_;
  end;
  PMUIP_Settingsgroup_ConfigToGadgets = ^TMUIP_Settingsgroup_ConfigToGadgets;

  TMUIP_Settingsgroup_GadgetsToConfig = record
    MethodID: LongWord;  // MUIM_Settingsgroup_GadgetsToConfig
    configdata: PObject_;
  end;
  PMUIP_Settingsgroup_GadgetsToConfig = ^TMUIP_Settingsgroup_GadgetsToConfig;

// *********************************************************************
//  Settings
const
  MUIC_Settings: PChar = 'Settings.mui';

// Identifier base (for Zune extensions)
  MUIB_Settings = MUIB_ZUNE or $00003200;

// *********************************************************************
//  Aboutmui
const
  MUIC_Aboutmui: PChar = 'Aboutmui.mui';

// Identifier base (for Zune extensions)
  MUIB_Aboutmui = MUIB_ZUNE or $0;

// Attributes
const
   MUIA_Aboutmui_Application = MUIB_MUI or $422523; // V11 i.. PObject

// *********************************************************************
//  Configdata
const
   MUIC_Configdata : PChar = 'Configdata.mui';

// Identifier base (for Zune extensions)
  MUIB_Configdata = MUIB_ZUNE or $00000900;

// Methods
  MUIM_Configdata_GetWindowPos = MUIB_Configdata or $0000002A;
  MUIM_Configdata_SetWindowPos = MUIB_Configdata or $0000002B;
  MUIM_Configdata_GetString    = MUIB_Configdata or $00000000;
  MUIM_Configdata_GetULong     = MUIB_Configdata or $00000001;
  MUIM_Configdata_SetULong     = MUIB_Configdata or $00000002;
  MUIM_Configdata_SetImspec    = MUIB_Configdata or $00000003;
  MUIM_Configdata_SetFramespec = MUIB_Configdata or $00000004;
  MUIM_Configdata_SetFont      = MUIB_Configdata or $00000005;
  MUIM_Configdata_Save         = MUIB_Configdata or $00000006;
  MUIM_Configdata_Load         = MUIB_Configdata or $00000007;
  MUIM_Configdata_SetPenspec   = MUIB_Configdata or $00000008;
  MUIM_Configdata_SetString    = MUIB_Configdata or $00000009;

type
  TMUIP_Configdata_GetString = record
    MethodID: LongWord;
    id: LongWord;
  end;
  PMUIP_Configdata_GetString = ^TMUIP_Configdata_GetString;

  TMUIP_Configdata_GetULong = record
    MethodID: LongWord;
    id: LongWord;
  end;
  PMUIP_Configdata_GetULong = ^TMUIP_Configdata_GetULong;

  TMUIP_Configdata_SetULong = record
    MethodID: LongWord;
    id: LongWord;
    val: LongWord;
  end;
  PMUIP_Configdata_SetULong = ^TMUIP_Configdata_SetULong;

  TMUIP_Configdata_SetImspec = record
    MethodID: LongWord;
    id: LongWord;
    imspec: STRPTR;
  end;
  PMUIP_Configdata_SetImspec = ^TMUIP_Configdata_SetImspec;

  TMUIP_Configdata_SetFramespec = record
    MethodID: LongWord;
    id: LongWord;
    framespec: STRPTR;
  end;
  PMUIP_Configdata_SetFramespec = ^TMUIP_Configdata_SetFramespec;

  TMUIP_Configdata_SetFont = record
    MethodID: LongWord;
    id: LongWord;
    font: STRPTR;
  end;
  PMUIP_Configdata_SetFont = ^TMUIP_Configdata_SetFont;

  TMUIP_Configdata_Save = record
    MethodID: LongWord;
    filename: STRPTR;
  end;
  PMUIP_Configdata_Save = ^TMUIP_Configdata_Save;

  TMUIP_Configdata_Load = record
    MethodID: LongWord;
    filename: STRPTR;
  end;
  PMUIP_Configdata_Load = ^TMUIP_Configdata_Load;

  TMUIP_Configdata_SetPenspec = record
    MethodID: LongWord;
    id: LongWord;
    penspec: STRPTR;
  end;
  PMUIP_Configdata_SetPenspec = ^TMUIP_Configdata_SetPenspec;

  TMUIP_Configdata_SetString = record
    MethodID: LongWord;
    id: LongWord;
    Newstring: STRPTR;
  end;
  PMUIP_Configdata_SetString = ^TMUIP_Configdata_SetString;

// Attributes
const
  MUIA_Configdata_Application     = MUIB_Configdata or $00000000; // Zune V1: i..  PObject_
  MUIA_Configdata_ApplicationBase = MUIB_Configdata or $00000002; // Zune V1: i..  PObject_

// The config items for MUIM_GetConfigItem
  MUICFG_Invalid                  = -1;
  MUICFG_Window_Spacing_Left      = $01; // LongWord, horiz pixels (def=4)
  MUICFG_Window_Spacing_Right     = $02; // LongWord, horiz pixels (def=4)
  MUICFG_Window_Spacing_Top       = $03; // LongWord, vert pixels (def=3)
  MUICFG_Window_Spacing_Bottom    = $04; // LongWord, vert pixels (def=3)
  MUICFG_Radio_HSpacing           = $05; // LongWord, horiz pixels (def=4)
  MUICFG_Radio_VSpacing           = $06; // LongWord, vertical pixels (def=1)
  MUICFG_Group_HSpacing           = $07; // LongWord, horiz pixels (def=6)
  MUICFG_Group_VSpacing           = $08; // LongWord, vertical pixels (def=3)
  MUICFG_Scrollbar_Arrangement    = $09; // LongWord, top = 0 (def), middle, bottom
  MUICFG_Listview_Refresh         = $0a; // LongWord, linear, mixed = 1 (def)
  MUICFG_Listview_Font_Leading    = $0b; // LongWord, vertical pixels (def=1)
  MUICFG_Listview_SmoothVal       = $0c; // LongWord, ? (def=0)
  MUICFG_Listview_Multi           = $0d; // LongWord, shifted = 0 (def), always
  MUICFG_GroupTitle_Position      = $0f; // LongWord, 1=centered
  MUICFG_GroupTitle_Color         = $10; // LongWord, 0=normal
  MUICFG_Cycle_MenuCtrl_Level     = $11; // LongWord, num of entries (def=2)
  MUICFG_Cycle_MenuCtrl_Position  = $12; // LongWord, below = 0 (def), on active
  MUICFG_Frame_Drag               = $18;
  MUICFG_Cycle_Menu_Recessed      = $19; // LongWord, false = 0 (def), true
  MUICFG_Cycle_MenuCtrl_Speed     = $1a; // LongWord, num of ticks (0..50) (def=0)
  MUICFG_Listview_Smoothed        = $1b; // LongWord, false = 0 (def), true
  MUICFG_Window_Redraw            = $1d; // LongWord, no clear, clear = 1 (def)
  MUICFG_Font_Normal              = $1e;
  MUICFG_Font_List                = $1f;
  MUICFG_Font_Tiny                = $20;
  MUICFG_Font_Fixed               = $21;
  MUICFG_Font_Title               = $22;
  MUICFG_Font_Big                 = $23;
  MUICFG_PublicScreen             = $24;
  MUICFG_Frame_Button             = $2b;
  MUICFG_Frame_ImageButton        = $2c;
  MUICFG_Frame_Text               = $2d;
  MUICFG_Frame_String             = $2e;
  MUICFG_Frame_ReadList           = $2f;
  MUICFG_Frame_InputList          = $30;
  MUICFG_Frame_Prop               = $31;
  MUICFG_Frame_Gauge              = $32;
  MUICFG_Frame_Group              = $33;
  MUICFG_Frame_PopUp              = $34;
  MUICFG_Frame_Virtual            = $35;
  MUICFG_Frame_Slider             = $36;
  MUICFG_Background_Window        = $37;
  MUICFG_Background_Requester     = $38;
  MUICFG_Background_Button        = $39;
  MUICFG_Background_List          = $3a;
  MUICFG_Background_Text          = $3b;
  MUICFG_Background_Prop          = $3c;
  MUICFG_Background_PopUp         = $3d;
  MUICFG_Background_Selected      = $3e;
  MUICFG_Background_ListCursor    = $3f;
  MUICFG_Background_ListSelect    = $40;
  MUICFG_Background_ListSelCur    = $41;
  MUICFG_Image_ArrowUp            = $42;
  MUICFG_Image_ArrowDown          = $43;
  MUICFG_Image_ArrowLeft          = $44;
  MUICFG_Image_ArrowRight         = $45;
  MUICFG_Image_CheckMark          = $46;
  MUICFG_Image_RadioButton        = $47;
  MUICFG_Image_Cycle              = $48;
  MUICFG_Image_PopUp              = $49;
  MUICFG_Image_PopFile            = $4a;
  MUICFG_Image_PopDrawer          = $4b;
  MUICFG_Image_PropKnob           = $4c;
  MUICFG_Image_Drawer             = $4d;
  MUICFG_Image_HardDisk           = $4e;
  MUICFG_Image_Disk               = $4f;
  MUICFG_Image_Chip               = $50;
  MUICFG_Image_Volume             = $51;
  MUICFG_Image_Network            = $52;
  MUICFG_Image_Assign             = $53;
  MUICFG_Background_Register      = $54;
  MUICFG_Image_TapePlay           = $55;
  MUICFG_Image_TapePlayBack       = $56;
  MUICFG_Image_TapePause          = $57;
  MUICFG_Image_TapeStop           = $58;
  MUICFG_Image_TapeRecord         = $59;
  MUICFG_Background_Framed        = $5a;
  MUICFG_Background_Slider        = $5b;
  MUICFG_Background_SliderKnob    = $5c;
  MUICFG_Image_TapeUp             = $5d;
  MUICFG_Image_TapeDown           = $5e;
  MUICFG_Keyboard_Press           = $5f;
  MUICFG_Keyboard_Toggle          = $60;
  MUICFG_Keyboard_Up              = $61;
  MUICFG_Keyboard_Down            = $62;
  MUICFG_Keyboard_PageUp          = $63;
  MUICFG_Keyboard_PageDown        = $64;
  MUICFG_Keyboard_Top             = $65;
  MUICFG_Keyboard_Bottom          = $66;
  MUICFG_Keyboard_Left            = $67;
  MUICFG_Keyboard_Right           = $68;
  MUICFG_Keyboard_WordLeft        = $69;
  MUICFG_Keyboard_WordRight       = $6a;
  MUICFG_Keyboard_LineStart       = $6b;
  MUICFG_Keyboard_LineEnd         = $6c;
  MUICFG_Keyboard_NextGadget      = $6d;
  MUICFG_Keyboard_PrevGadget      = $6e;
  MUICFG_Keyboard_GadgetOff       = $6f;
  MUICFG_Keyboard_CloseWindow     = $70;
  MUICFG_Keyboard_NextWindow      = $71;
  MUICFG_Keyboard_PrevWindow      = $72;
  MUICFG_Keyboard_Help            = $73;
  MUICFG_Keyboard_Popup           = $74;
  MUICFG_Window_Positions         = $7a;
  MUICFG_Balance_Look             = $7b; // LongWord, frame = 0 (def), object
  MUICFG_Font_Button              = $80;
  MUICFG_Scrollbar_Type           = $83; // LongWord, standard = 0 (def), newlook, custom
  MUICFG_String_Background        = $84;
  MUICFG_String_Text              = $85;
  MUICFG_String_ActiveBackground  = $86;
  MUICFG_String_ActiveText        = $87;
  MUICFG_Font_Knob                = $88;
  MUICFG_Drag_LeftButton          = $89; // LongWord, false, true (def)
  MUICFG_Drag_MiddleButton        = $8a; // LongWord, false (def), true
  MUICFG_Drag_LMBModifier         = $8b; // key desc (def = control)
  MUICFG_Drag_MMBModifier         = $8c; // key desc
  MUICFG_Drag_Autostart           = $8d; // LongWord, false = 0, true (def)
  MUICFG_Drag_Autostart_Length    = $8e; // LongWord, pixels (def = 3)
  MUICFG_ActiveObject_Color       = $8f; // penspec
  MUICFG_Frame_Knob               = $90;
  MUICFG_Dragndrop_Look           = $94; // LongWord, solid, ghosted on obj (def), ...
  MUICFG_Background_Page          = $95;
  MUICFG_Background_ReadList      = $96;
  MUICFG_String_Cursor            = $400;
  MUICFG_String_MarkedBackground  = $401;
  MUICFG_String_MarkedText        = $402;
  MUICFG_Register_TruncateTitles  = $403;
  MUICFG_Window_Refresh           = $404;
  MUICFG_Screen_Mode              = $505;
  MUICFG_Screen_Mode_ID           = $506;
  MUICFG_Screen_Width             = $507;
  MUICFG_Screen_Height            = $508;
  MUICFG_WindowPos                = $509;
  MUICFG_Window_Buttons           = $50a;

  MUICFG_CustomFrame_1            = $600;
  MUICFG_CustomFrame_2            = $601;
  MUICFG_CustomFrame_3            = $602;
  MUICFG_CustomFrame_4            = $603;
  MUICFG_CustomFrame_5            = $604;
  MUICFG_CustomFrame_6            = $605;
  MUICFG_CustomFrame_7            = $606;
  MUICFG_CustomFrame_8            = $607;
  MUICFG_CustomFrame_9            = $608;
  MUICFG_CustomFrame_10           = $609;
  MUICFG_CustomFrame_11           = $60a;
  MUICFG_CustomFrame_12           = $60b;
  MUICFG_CustomFrame_13           = $60c;
  MUICFG_CustomFrame_14           = $60d;
  MUICFG_CustomFrame_15           = $60e;
  MUICFG_CustomFrame_16           = $60f;

  MUICFG_PublicScreen_PopToFront  = $700;
  MUICFG_Iconification_Hotkey     = $701;
  MUICFG_Iconification_ShowIcon   = $702;
  MUICFG_Iconification_ShowMenu   = $703;
  MUICFG_Iconification_OnStartup  = $704;
  MUICFG_Interfaces_EnableARexx   = $705;
  MUICFG_BubbleHelp_FirstDelay    = $706;
  MUICFG_BubbleHelp_NextDelay     = $707;

// *********************************************************************
//  Imageadjust
const
  MUIC_Imageadjust: PChar = 'Imageadjust.mui';

// Identifier base (for AROS extensions)
  MUIB_Imageadjust = MUIB_ZUNE or $00001100;

// Attributes
const
  MUIA_Imageadjust_Type = MUIB_MUI or $422f2b; // V11 i.. LongInt
  MUIA_Imageadjust_Spec = MUIB_MUI or $4279e1; // ??? .g. PChar
  // AROS special
  MUIA_Imageadjust_Originator = MUIB_Imageadjust or $0; // Zune: i.. PObject_

  MUIV_Imageadjust_Type_All        = 0;
  MUIV_Imageadjust_Type_Image      = 1;
  MUIV_Imageadjust_Type_Background = 2;
  MUIV_Imageadjust_Type_Pen        = 3;

// *********************************************************************
//  Popimage
const
  MUIC_Popimage: PChar = 'Popimage.mui';

// Identifier base (for Zune extensions)
  MUIB_Popimage = MUIB_ZUNE or $00002300;

// *********************************************************************
//  Scale
const
  MUIC_Scale: PChar = 'Scale.mui';

// Identifier base (for Zune extensions)
  MUIB_Scale = MUIB_ZUNE or $00002d00;

// Attributes
const
  MUIA_Scale_Horiz = MUIB_MUI or $42919a; // V4  isg WordBool

// *********************************************************************
//  Radio
const
  MUIC_Radio: PChar = 'Radio.mui';

// Identifier base (for Zune extensions)
  MUIB_Radio = MUIB_ZUNE or $00002a00;

// Attributes
const
  MUIA_Radio_Active  = MUIB_MUI or $429b41; // V4  isg LongInt
  MUIA_Radio_Entries = MUIB_MUI or $42b6a1; // V4  i.. STRPTR

// *********************************************************************
//  Balance
const
  MUIC_Balance: PChar = 'Balance.mui';

// Identifier base (for Zune extensions)
  MUIB_Balance = MUIB_ZUNE or $00000300;

// Attributes
  MUIA_Balance_Quiet = MUIB_Balance or $0; // Zune V20 i.. LongInt

// *********************************************************************
//  Pendisplay
const
  MUIC_Pendisplay: PChar = 'Pendisplay.mui';

// Identifier base (for Zune extensions)
  MUIB_Pendisplay = MUIB_ZUNE or $00002000;

// Methods
const
  MUIM_Pendisplay_SetColormap = MUIB_MUI or $426c80; // V13
  MUIM_Pendisplay_SetMUIPen   = MUIB_MUI or $42039d; // V13
  MUIM_Pendisplay_SetRGB      = MUIB_MUI or $42c131; // V13

type
  TMUIP_Pendisplay_SetColormap = record
    MethodID: LongWord;  // MUIM_Pendisplay_SetColormap
    colormap: LongInt;
  end;
  PMUIP_Pendisplay_SetColormap = ^TMUIP_Pendisplay_SetColormap;

  TMUIP_Pendisplay_SetMUIPen = record
    MethodID: LongWord;  // MUIM_Pendisplay_SetMUIPen
    muipen: LongInt;
  end;
  PMUIP_Pendisplay_SetMUIPen = ^TMUIP_Pendisplay_SetMUIPen;

  TMUIP_Pendisplay_SetRGB = record
    MethodID: LongWord;  // MUIM_Pendisplay_SetRGB
    red: LongWord;
    green: LongWord;
    blue: LongWord;
  end;
  PMUIP_Pendisplay_SetRGB = ^TMUIP_Pendisplay_SetRGB;

// Attributes
const
  MUIA_Pendisplay_Pen       = MUIB_MUI or $42a748; // V13 ..g PObject_
  MUIA_Pendisplay_Reference = MUIB_MUI or $42dc24; // V13 isg PObject_
  MUIA_Pendisplay_RGBcolor  = MUIB_MUI or $42a1a9; // V11 isg PMUI_RGBcolor
  MUIA_Pendisplay_Spec      = MUIB_MUI or $42a204; // V11 isg PMUI_PenSpec

// *********************************************************************
//  Penadjust
const
  MUIC_Penadjust: PChar= 'Penadjust.mui';

// Identifier base (for Zune extensions)
  MUIB_Penadjust = MUIB_ZUNE or $00001f00;

// Attributes
const
  MUIA_Penadjust_PSIMode = MUIB_MUI or $421cbb; // V11 i.. WordBool

// *********************************************************************
//  Poppen
const
  MUIC_Poppen: PChar = 'Poppen.mui';

// Identifier base (for Zune extensions)
  MUIB_Poppen = MUIB_ZUNE or $00002700;

// *********************************************************************
//  Colorfield
const
  MUIC_Colorfield: PChar = 'Colorfield.mui';

// Identifier base (for Zune extensions)
  MUIB_Colorfield = MUIB_ZUNE or $00000800;

// Attributes
const
  MUIA_Colorfield_Blue  = MUIB_MUI or $42d3b0; // V4  isg LongWord
  MUIA_Colorfield_Green = MUIB_MUI or $424466; // V4  isg LongWord
  MUIA_Colorfield_Pen   = MUIB_MUI or $42713a; // V4  ..g LongWord
  MUIA_Colorfield_Red   = MUIB_MUI or $4279f6; // V4  isg LongWord
  MUIA_Colorfield_RGB   = MUIB_MUI or $42677a; // V4  isg LongWord

// *********************************************************************
//  Coloradjust
const
  MUIC_Coloradjust: PChar = 'Coloradjust.mui';

// Identifier base (for Zune extensions)
  MUIB_Coloradjust = MUIB_ZUNE or $00000700;

// Attributes
const
  MUIA_Coloradjust_Blue   = MUIB_MUI or $42b8a3; // V4  isg LongWord
  MUIA_Coloradjust_Green  = MUIB_MUI or $4285ab; // V4  isg LongWord
  MUIA_Coloradjust_ModeID = MUIB_MUI or $42ec59; // V4  isg LongWord
  MUIA_Coloradjust_Red    = MUIB_MUI or $420eaa; // V4  isg LongWord
  MUIA_Coloradjust_RGB    = MUIB_MUI or $42f899; // V4  isg LongWord

// *********************************************************************
//  Mccprefs
const
  MUIC_Mccprefs: PChar = 'Mccprefs.mui';

// *********************************************************************
//  Frameadjust
const
  MUIC_Frameadjust: PChar = 'Frameadjust.mui';

// Identifier base (for Zune extensions)
  MUIB_Frameadjust = MUIB_ZUNE or $00000d00;

// Attributes
  MUIA_Frameadjust_Spec = MUIB_Frameadjust or $0; // Zune 20030330 ig. STRPTR

// *********************************************************************
//  Framedisplay
const
  MUIC_Framedisplay: PChar = 'Framedisplay.mui';

// Identifier base (for Zune extensions)
  MUIB_Framedisplay = MUIB_ZUNE or $00000e00;

// Attributes
  MUIA_Framedisplay_Spec = MUIB_MUI or $421794; // MUI: V??  isg PMUI_FrameSpec

// *********************************************************************
//  Popframe
const
  MUIC_Popframe: PChar = 'Popframe.mui';

// Identifier base (for Zune extensions)
  MUIB_Popframe = MUIB_ZUNE or $00002200;

// *********************************************************************
//  Volumelist
const
  MUIC_Volumelist: PChar = 'Volumelist.mui';

// Identifier base (for Zune extensions)
  MUIB_Volumelist = MUIB_ZUNE or $1600;

// *********************************************************************
//  Dirlist
const
  MUIC_Dirlist: PChar = 'Dirlist.mui';

// Identifer base (for Zune extensions)
  MUIB_Dirlist = MUIB_ZUNE or $00001800;

// Methods
const
  MUIM_Dirlist_ReRead = MUIB_MUI or $422d71; // V4

type
  TMUIP_Dirlist_ReRead = record
    MethodID : LongWord;  // MUIM_Dirlist_ReRead
  end;

// Attributes
const
  MUIA_Dirlist_AcceptPattern = MUIB_MUI or $42760a; // V4  is. STRPTR
  MUIA_Dirlist_Directory     = MUIB_MUI or $42ea41; // V4  isg STRPTR
  MUIA_Dirlist_DrawersOnly   = MUIB_MUI or $42b379; // V4  is. WordBool
  MUIA_Dirlist_FilesOnly     = MUIB_MUI or $42896a; // V4  is. WordBool
  MUIA_Dirlist_FilterDrawers = MUIB_MUI or $424ad2; // V4  is. WordBool
  MUIA_Dirlist_FilterHook    = MUIB_MUI or $42ae19; // V4  is. PHook
  MUIA_Dirlist_MultiSelDirs  = MUIB_MUI or $428653; // V6  is. WordBool
  MUIA_Dirlist_NumBytes      = MUIB_MUI or $429e26; // V4  ..g LongInt
  MUIA_Dirlist_NumDrawers    = MUIB_MUI or $429cb8; // V4  ..g LongInt
  MUIA_Dirlist_NumFiles      = MUIB_MUI or $42a6f0; // V4  ..g LongInt
  MUIA_Dirlist_Path          = MUIB_MUI or $426176; // V4  ..g STRPTR
  MUIA_Dirlist_RejectIcons   = MUIB_MUI or $424808; // V4  is. WordBool
  MUIA_Dirlist_RejectPattern = MUIB_MUI or $4259c7; // V4  is. STRPTR
  MUIA_Dirlist_SortDirs      = MUIB_MUI or $42bbb9; // V4  is. LongInt
  MUIA_Dirlist_SortHighLow   = MUIB_MUI or $421896; // V4  is. WordBool
  MUIA_Dirlist_SortType      = MUIB_MUI or $4228bc; // V4  is. LongInt
  MUIA_Dirlist_Status        = MUIB_MUI or $4240de; // V4  ..g LongInt

  MUIV_Dirlist_SortDirs_First = 0;
  MUIV_Dirlist_SortDirs_Last  = 1;
  MUIV_Dirlist_SortDirs_Mix   = 2;

  MUIV_Dirlist_SortType_Name = 0;
  MUIV_Dirlist_SortType_Date = 1;
  MUIV_Dirlist_SortType_Size = 2;

  MUIV_Dirlist_Status_Invalid = 0;
  MUIV_Dirlist_Status_Reading = 1;
  MUIV_Dirlist_Status_Valid   = 2;

// *********************************************************************
//  Numericbutton
const
  MUIC_Numericbutton: PChar = 'Numericbutton.mui';

// *********************************************************************
//  Poplist
const
  MUIC_Poplist: PChar = 'Poplist.mui';

// Identifier base (for Zune extensions)
  MUIB_Poplist = MUIB_ZUNE or $00002500;

// Attributes
const
  MUIA_Poplist_Array = MUIB_MUI or $42084c; // V8  i.. PPChar

// *********************************************************************
//  Popscreen
const
  MUIC_Popscreen: PChar = 'Popscreen.mui';

// Identifier base (for Zune extensions)
  MUIB_Popscreen = MUIB_ZUNE or $00002600;

// *********************************************************************
//  Crawling
const
  MUIC_Crawling: PChar = 'Crawling.mui';

// *********************************************************************
//  Levelmeter
const
  MUIC_Levelmeter: PChar = 'Levelmeter.mui';

// Attributes
const
  MUIA_Levelmeter_Label = MUIB_MUI or $420dd5; // V11 isg STRPTR

// *********************************************************************
//  Knob
const
  MUIC_Knob: PChar = 'Knob.mui';

// *********************************************************************
//  Dtpic
const
  MUIC_Dtpic: PChar = 'Dtpic.mui';

// Identifier base (for Zune extensions)
  MUIB_Palette = MUIB_ZUNE or $00008a00;

// Attributes
const
  MUIA_Dtpic_Alpha          = MUIB_MUI or $42b4db; // V20 isg LongInt
  MUIA_Dtpic_DarkenSelState = MUIB_MUI or $423247; // V20 i.g WordBool
  MUIA_Dtpic_Fade           = MUIB_MUI or $420429; // V20 isg LongInt
  MUIA_Dtpic_LightenOnMouse = MUIB_MUI or $42966a; // V20 i.g WordBool
  MUIA_Dtpic_Name           = MUIB_MUI or $423d72; // V18 isg STRPTR

// *********************************************************************
//  Palette
const
  MUIC_Palette: PChar = 'Palette.mui';

// Attributes
const
  MUIA_Palette_Entries   = $8042a3d8;{ V6  i.g struct MUI_Palette_Entry    }
  MUIA_Palette_Groupable = $80423e67;{ V6  isg BOOL               }
  MUIA_Palette_Names     = $8042c3a2;{ V6  isg char               }

// *********************************************************************
//  Title
const
  MUIC_Title: PChar = 'Title.mui';

// *********************************************************************
//  Process
const
  MUIC_Process: PChar = 'Process.mui';

// Methods
  MUIM_Process_Kill    = MUIB_MUI or $4264cf; // V20
  MUIM_Process_Launch  = MUIB_MUI or $425df7; // V20
  MUIM_Process_Process = MUIB_MUI or $4230aa; // V20
  MUIM_Process_Signal  = MUIB_MUI or $42e791; // V20

type
  TMUIP_Process_Kill = record
    MethodID : LongWord;
    maxdelay: LongInt;
  end;

  TMUIP_Process_Launch = record
    MethodID : LongWord;
  end;

  TMUIP_Process_Process = record
    MethodID : LongWord;
    kill: PLongWord;
    proc: PObject_;
  end;

  TMUIP_Process_Signal = record
    MethodID : LongWord;
    sigs: LongWord;
  end;

// Attributes
const
  MUIA_Process_AutoLaunch   = MUIB_MUI or $428855; // V20 i.. LongWord
  MUIA_Process_Name         = MUIB_MUI or $42732b; // V20 i.. LongWord
  MUIA_Process_Priority     = MUIB_MUI or $422a54; // V20 i.. LongWord
  MUIA_Process_SourceClass  = MUIB_MUI or $42cf8b; // V20 i.. LongWord
  MUIA_Process_SourceObject = MUIB_MUI or $4212a2; // V20 i.. LongWord
  MUIA_Process_StackSize    = MUIB_MUI or $4230d0; // V20 i.. LongWord
  MUIA_Process_Task         = MUIB_MUI or $42b123; // V20 ..g LongWord

// *********************************************************************
//  Pixmap
const
  MUIC_Pixmap: PChar = 'Pixmap.mui';

// Identifier base (for Zune extensions)
  MUIB_Pixmap = MUIB_ZUNE or $00003600;

/// Methods
  MUIM_Pixmap_DrawSection = MUIB_MUI or $42ce0f; // private, V20

type
  TMUIP_Pixmap_DrawSection = record // private
    MethodID: LongWord;  // MUIM_Pixmap_DrawSection
    sx: LongInt;
    sy: LongInt;
    sw: LongInt;
    sh: LongInt;
    mri: PMUI_RenderInfo;
    dx: LongInt;
    dy: LongInt;
  end;

// Attributes
const
  MUIA_Pixmap_Alpha            = MUIB_MUI or $421fef; // V20 isg LongWord
  MUIA_Pixmap_CLUT             = MUIB_MUI or $42042a; // V20 isg LongWord
  MUIA_Pixmap_CompressedSize   = MUIB_MUI or $42e7e4; // V20 isg LongWord
  MUIA_Pixmap_Compression      = MUIB_MUI or $42ce74; // V20 isg LongWord
  MUIA_Pixmap_Data             = MUIB_MUI or $429ea0; // V20 isg APTR
  MUIA_Pixmap_Format           = MUIB_MUI or $42ab14; // V20 isg LongWord
  MUIA_Pixmap_Height           = MUIB_MUI or $4288be; // V20 isg LongInt
  MUIA_Pixmap_UncompressedData = MUIB_MUI or $42b085; // V20 ..g APTR
  MUIA_Pixmap_Width            = MUIB_MUI or $42ccb8; // V20 isg LongInt

  MUIV_Pixmap_Compression_None  = 0;
  MUIV_Pixmap_Compression_RLE   = 1;
  MUIV_Pixmap_Compression_BZip2 = 2;

  MUIV_Pixmap_Format_CLUT8  = 0;
  MUIV_Pixmap_Format_RGB24  = 1;
  MUIV_Pixmap_Format_ARGB32 = 2;

// *********************************************************************
//  Gadget
const
  MUIC_Gadget: PChar = 'Gadget.mui';

// Attributes
const
  MUIA_Gadget_Gadget = MUIB_MUI or $42ec1a; // V11 ..g PGadget

// *********************************************************************
//  Applist
const
  MUIC_Applist: PChar = 'Applist.mui';

// *********************************************************************
//  Cclist
const
  MUIC_Cclist: PChar = 'Cclist.mui';

type
// Instance data of notify class
  TMUI_NotifyData = record
    mnd_GlobalInfo: PMUI_GlobalInfo;
    mnd_UserData: LongWord;
    mnd_ObjectID: LongWord;
    priv1: LongWord;
    priv2: LongWord;
    priv3: LongWord;
    priv4: LongWord;
  end;
  PMUI_NotifyData = ^TMUI_NotifyData;

const
  // special maximum dimension in case it is unlimited
  MUI_MAXMAX = 10000;

type
  T__dummyAreaData__ = record
    mnd: TMUI_NotifyData;
    mad: TMUI_AreaData;
  end;
  P__dummyAreaData__ = ^T__dummyAreaData__;

const
// Possible keyevents (user configurable)
  MUIKEY_RELEASE      = -2;  // this one is faked only, so not configurable
  MUIKEY_NONE         = -1;
  MUIKEY_PRESS        = 0;
  MUIKEY_TOGGLE       = 1;
  MUIKEY_UP           = 2;
  MUIKEY_DOWN         = 3;
  MUIKEY_PAGEUP       = 4;
  MUIKEY_PAGEDOWN     = 5;
  MUIKEY_TOP          = 6;
  MUIKEY_BOTTOM       = 7;
  MUIKEY_LEFT         = 8;
  MUIKEY_RIGHT        = 9;
  MUIKEY_WORDLEFT     = 10;
  MUIKEY_WORDRIGHT    = 11;
  MUIKEY_LINESTART    = 12;
  MUIKEY_LINEEND      = 13;
  MUIKEY_GADGET_NEXT  = 14;
  MUIKEY_GADGET_PREV  = 15;
  MUIKEY_GADGET_OFF   = 16;
  MUIKEY_WINDOW_CLOSE = 17;
  MUIKEY_WINDOW_NEXT  = 18;
  MUIKEY_WINDOW_PREV  = 19;
  MUIKEY_HELP         = 20;
  MUIKEY_POPUP        = 21;
  MUIKEY_COUNT        = 22;

  // The mask definitions of the above keys
  MUIKEYF_PRESS = 1 shl MUIKEY_PRESS;
  MUIKEYF_TOGGLE = 1 shl MUIKEY_TOGGLE;
  MUIKEYF_UP = 1 shl MUIKEY_UP;
  MUIKEYF_DOWN = 1 shl MUIKEY_DOWN;
  MUIKEYF_PAGEUP = 1 shl MUIKEY_PAGEUP;
  MUIKEYF_PAGEDOWN = 1 shl MUIKEY_PAGEDOWN;
  MUIKEYF_TOP = 1 shl MUIKEY_TOP;
  MUIKEYF_BOTTOM = 1 shl MUIKEY_BOTTOM;
  MUIKEYF_LEFT = 1 shl MUIKEY_LEFT;
  MUIKEYF_RIGHT = 1 shl MUIKEY_RIGHT;
  MUIKEYF_WORDLEFT = 1 shl MUIKEY_WORDLEFT;
  MUIKEYF_WORDRIGHT = 1 shl MUIKEY_WORDRIGHT;
  MUIKEYF_LINESTART = 1 shl MUIKEY_LINESTART;
  MUIKEYF_LINEEND = 1 shl MUIKEY_LINEEND;
  MUIKEYF_GADGET_NEXT = 1 shl MUIKEY_GADGET_NEXT;
  MUIKEYF_GADGET_PREV = 1 shl MUIKEY_GADGET_PREV;
  MUIKEYF_GADGET_OFF = 1 shl MUIKEY_GADGET_OFF;
  MUIKEYF_WINDOW_CLOSE = 1 shl MUIKEY_WINDOW_CLOSE;
  MUIKEYF_WINDOW_NEXT = 1 shl MUIKEY_WINDOW_NEXT;
  MUIKEYF_WINDOW_PREV = 1 shl MUIKEY_WINDOW_PREV;
  MUIKEYF_HELP = 1 shl MUIKEY_HELP;
  MUIKEYF_POPUP = 1 shl MUIKEY_POPUP;

// MUI_CustomClass returned by MUI_CreateCustomClass()
// use for whatever you want  MUI has opened these libraries for you automatically. You can use them or decide to open
type
  TMUI_CustomClass = record
    mcc_UserData: APTR; // freely usable

    // Zune/MUI had the following libraries opened for you
    mcc_UtilityBase: PLibrary;
    mcc_DOSBase: PLibrary;
    mcc_GfxBase: PLibrary;
    mcc_IntuitionBase: PLibrary;

    mcc_Super: PIClass; // the boopsi class' superclass
    mcc_Class: PIClass; // the boopsi class

    // the following stuff is private
    mcc_Module: PLibrary; // non-null if external class
  end;
  PMUI_CustomClass = ^TMUI_CustomClass;

var
  MUIMasterBase: PLibrary = nil;
  IMui: PInterface = nil;

function MUIObtain(): LongWord; syscall IMui 60;
function MUIRelease(): LongWord; syscall IMui 64;
// 68 Expunge not implemented
// 72 Clone not implemented
function MUI_NewObjectA(const ClassID: STRPTR; Tags: PTagItem): PObject_; syscall IMui 76;
// 80 MUI_NewObject
procedure MUI_DisposeObject(Obj: PObject_); syscall IMui 84;
function MUI_RequestA(App: PObject_; Win: PObject_; Flags: LongWord; const Title: STRPTR; const Gadgets: STRPTR; const Format: STRPTR; Params: APTR): LongInt; syscall IMui 88;
// 92 MUI_Request
function MUI_AllocAslRequest(Type_: LongWord; TagList: PTagItem): Pointer; syscall IMui 96;
// 100 MUI_AllocAslRequestTags
function MUI_AslRequest(Req: APTR; TagList: PTagItem): LongBool; syscall IMui 104;
// 108 MUI_AslRequestTags
procedure MUI_FreeAslRequest(Req: APTR); syscall IMui 112;
function MUI_Error: LongInt; syscall IMui 116;
function MUI_SetError(ErrNum: LongInt): LongInt; syscall IMui 120;
function MUI_GetClass(const ClassID: STRPTR): PIClass; syscall IMui 124;
procedure MUI_FreeClass(Cl: PIClass); syscall IMui 128;
procedure MUI_RequestIDCMP(Obj: PObject_; Flags: LongWord); syscall IMui 132;
procedure MUI_RejectIDCMP(Obj: PObject_; Flags: LongWord); syscall IMui 136;
procedure MUI_Redraw(Obj: PObject_; Flags: LongWord); syscall IMui 140;
function MUI_CreateCustomClass(Base: PLibrary; const Supername: STRPTR; Supermcc: PMUI_CustomClass; DataSize: LongWord; Dispatcher: APTR): PMUI_CustomClass; syscall IMui 144;
function MUI_DeleteCustomClass(Mcc: PMUI_CustomClass): LongBool; syscall IMui 148;
function MUI_MakeObjectA(Type_: LongInt; Params: PLongWord): PObject_; syscall IMui 152;
// 156 MUI_MakeObject
function MUI_Layout(Obj: PObject_; l, t, w, h: LongInt; Flags: LongWord): LongBool; syscall IMui 160;
// 164 - 176 Private
function MUI_ObtainPen(Mri: PMUI_RenderInfo; Spec: PMUI_PenSpec; Flags: LongWord) : LongInt; syscall IMui 180;
procedure MUI_ReleasePen(Mri: PMUI_RenderInfo; Pen: LongInt); syscall IMui 184;
function MUI_AddClipping(Mri: PMUI_RenderInfo; l, t, w, h: LongInt): APTR; syscall IMui 188;
procedure MUI_RemoveClipping(Mri: PMUI_RenderInfo; h: APTR); syscall IMui 192;
function MUI_AddClipRegion(Mri: PMUI_RenderInfo; Region: PRegion): APTR; syscall IMui 196;
procedure MUI_RemoveClipRegion(Mri: PMUI_RenderInfo; Region: APTR); syscall IMui 200;
function MUI_BeginRefresh(Mri: PMUI_RenderInfo; Flags: LongWord): LongBool; syscall IMui 204;
procedure MUI_EndRefresh(Mri: PMUI_RenderInfo; Flags: LongWord); syscall IMui 208;
// 212 - 216 private
function MUI_Show(Obj: PObject_): LongWord; syscall IMui 220;
function MUI_Hide(Obj: PObject_): LongWord; syscall IMui 224;
function MUI_LayoutObj(Obj: PObject_; l, t, w, h: LongInt; Flags: LongWord): LongBool; syscall IMui 228;
function MUI_Offset(Obj: PObject_; X, Y: LongInt): LongBool; syscall IMui 232;
// 236 - 572 private

// some procedures to get some information about our object
function MUINotifyData(Obj: APTR): PMUI_NotifyData;
function MUIAreaData(Obj: APTR): PMUI_AreaData;
function MUIGlobalInfo(Obj: APTR): PMUI_GlobalInfo;
function MUIUserData(Obj: APTR): Pointer ;
function MUIRenderInfo(Obj: APTR): PMUI_RenderInfo;
function MUIPen(Pen: LongInt): LongInt;

// some more specialized functions to retain information about special
// object-data like rastport, window, etc.

// NOTE: These macros may only be used in custom classes and are
// only valid if your class is inbetween the specified methods!

function OBJ_App(Obj: APTR): PObject_;        // valid between MUIM_Setup/Cleanup
function OBJ_Win(Obj: APTR): PObject_;        // valid between MUIM_Setup/Cleanup
function OBJ_Dri(Obj: APTR): PDrawInfo;       // valid between MUIM_Setup/Cleanup
function OBJ_Screen(Obj: APTR): PScreen;      // valid between MUIM_Setup/Cleanup
function OBJ_Pens(Obj: APTR): PWord;          // valid between MUIM_Setup/Cleanup
function OBJ_Window(Obj: APTR): PWindow;        // valid between MUIM_Show/Hide
function OBJ_Rp(Obj: APTR): PRastPort;          // valid between MUIM_Show/Hide
function OBJ_Left(Obj: APTR): SmallInt;           // valid during MUIM_Draw
function OBJ_Top(Obj: APTR): SmallInt;            // valid during MUIM_Draw *)
function OBJ_Width(Obj: APTR): SmallInt;          // valid during MUIM_Draw *)
function OBJ_Height(Obj: APTR): SmallInt;         // valid during MUIM_Draw *)
function OBJ_Right(Obj: APTR): SmallInt;          // valid during MUIM_Draw *)
function OBJ_Bottom(Obj: APTR): SmallInt;         // valid during MUIM_Draw *)
function OBJ_AddLeft(Obj: APTR): SmallInt;        // valid during MUIM_Draw *)
function OBJ_AddTop(Obj: APTR): SmallInt;         // valid during MUIM_Draw *)
function OBJ_SubWidth(Obj: APTR): SmallInt;       // valid during MUIM_Draw *)
function OBJ_SubHeight(Obj: APTR): SmallInt;      // valid during MUIM_Draw *)
function OBJ_MLeft(Obj: APTR): SmallInt;          // valid during MUIM_Draw *)
function OBJ_MTop(Obj: APTR): SmallInt;           // valid during MUIM_Draw *)
function OBJ_MWidth(Obj: APTR): SmallInt;         // valid during MUIM_Draw *)
function OBJ_MHeight(Obj: APTR): SmallInt;        // valid during MUIM_Draw *)
function OBJ_MRight(Obj: APTR): SmallInt;         // valid during MUIM_Draw *)
function OBJ_MBottom(Obj: APTR): SmallInt;        // valid during MUIM_Draw *)
function OBJ_Font(Obj: APTR): PTextFont;      // valid between MUIM_Setup/Cleanup *)
function OBJ_MinWidth(Obj: APTR): LongWord;     // valid between MUIM_Show/Hide *)
function OBJ_MinHeight(Obj: APTR): LongWord;    // valid between MUIM_Show/Hide *)
function OBJ_MaxWidth(Obj: APTR): LongWord;     // valid between MUIM_Show/Hide *)
function OBJ_MaxHeight(Obj: APTR): LongWord;    // valid between MUIM_Show/Hide *)
function OBJ_DefWidth(Obj: APTR): LongWord;     // valid between MUIM_Show/Hide *)
function OBJ_DefHeight(Obj: APTR): LongWord;    // valid between MUIM_Show/Hide *)
function OBJ_Flags(Obj: APTR): LongWord;

function OBJ_Between(a, x, b: SmallInt): Boolean;
function OBJ_IsInObject(x, y: SmallInt; Obj: PObject_): Boolean;

function MUIV_Window_AltHeight_MinMax(p: LongInt): LongInt;
function MUIV_Window_AltHeight_Visible(p: LongInt): LongInt;
function MUIV_Window_AltHeight_Screen(p: LongInt): LongInt;
function MUIV_Window_AltTopEdge_Delta(p: LongInt): LongInt;
function MUIV_Window_AltWidth_MinMax(p: LongInt): LongInt;
function MUIV_Window_AltWidth_Visible(p: LongInt): LongInt;
function MUIV_Window_AltWidth_Screen(p: LongInt): LongInt;
function MUIV_Window_Height_MinMax(p: LongInt): LongInt;
function MUIV_Window_Height_Visible(p: LongInt): LongInt;
function MUIV_Window_Height_Screen(p: LongInt): LongInt;
function MUIV_Window_TopEdge_Delta(p: LongInt): LongInt;
function MUIV_Window_Width_MinMax(p: LongInt): LongInt;
function MUIV_Window_Width_Visible(p: LongInt): LongInt;
function MUIV_Window_Width_Screen(p: LongInt): LongInt;


// Functions and procedures with array of const go here
function MUI_AllocAslRequestTags(ReqTyp: Longword; const Tags: array of PtrUInt): Pointer;
function MUI_AslRequestTags(req: Pointer; const Tags : array of PtrUInt): LongBool;
function MUI_MakeObject(_Type: LongInt; const Params : array of PtrUInt): PObject_;
function MUI_NewObject(a0arg: PChar; const Tags: array of PtrUInt): PObject_;
function MUI_Request(App: Pointer; win: Pointer; Flags: LongWord; Title: PChar; Gadgets: PChar; Format: PChar; const Params: Array Of PtrUInt): LongInt;

implementation

function MUINotifyData(Obj: APTR): PMUI_NotifyData; inline;
begin
  MUINotifyData := PMUI_NotifyData(@P__dummyAreaData__(Obj)^.mnd);
end;

function MUIAreaData(Obj: APTR): PMUI_AreaData; inline;
begin
  MUIAreaData := PMUI_AreaData(@P__dummyAreaData__(Obj)^.mad);
end;

function MUIGlobalInfo(Obj: APTR): PMUI_GlobalInfo; inline;
begin
  MUIGlobalInfo := PMUI_GlobalInfo(P__dummyAreaData__(Obj)^.mnd.mnd_GlobalInfo);
end;

function MUIUserData(Obj: APTR): Pointer; inline;
begin
  MUIUserData := Pointer(p__dummyAreaData__(obj)^.mnd.mnd_GlobalInfo);
end;

function MUIRenderInfo(Obj: APTR): PMUI_RenderInfo; inline;
begin
  MUIRenderInfo := PMUI_RenderInfo(P__dummyAreaData__(obj)^.mad.mad_RenderInfo);
end;

function MUIPen(Pen: LongInt): LongInt; inline;
begin
  MUIPen := LongInt(Pen and MUIPEN_Mask);
end;

function OBJ_App(Obj: APTR): PObject_; inline;
begin
  OBJ_App := MUIGlobalInfo(Obj)^.mgi_ApplicationObject;
end;

function OBJ_Win(Obj: APTR): PObject_; inline;
begin
  OBJ_Win := MUIRenderInfo(Obj)^.mri_WindowObject;
end;

function OBJ_Dri(Obj: APTR): PDrawInfo; inline;
begin
  OBJ_Dri := MUIRenderInfo(obj)^.mri_DrawInfo;
end;

function OBJ_Screen(Obj: APTR): PScreen; inline;
begin
  OBJ_Screen := MUIRenderInfo(Obj)^.mri_Screen;
end;

function OBJ_Pens(Obj: APTR): PWord; inline;
begin
  OBJ_Pens := MUIRenderInfo(obj)^.mri_Pens;
end;

function OBJ_Window(Obj: APTR): PWindow; inline;
begin
  OBJ_Window := MUIRenderInfo(obj)^.mri_Window;
end;

function OBJ_Rp(Obj: APTR): PRastPort; inline;
begin
  OBJ_Rp := MUIRenderInfo(Obj)^.mri_RastPort;
end;

function OBJ_Left(Obj: APTR): SmallInt; inline;
begin
  OBJ_Left := MUIAreaData(Obj)^.mad_Box.Left;
end;

function OBJ_Top(Obj: APTR): SmallInt; inline;
begin
  OBJ_Top := MUIAreaData(Obj)^.mad_Box.Top;
end;

function OBJ_Width(Obj: APTR): SmallInt; inline;
begin
  OBJ_Width := MUIAreaData(Obj)^.mad_Box.Width;
end;

function OBJ_Height(Obj: APTR): SmallInt; inline;
begin
  OBJ_Height := MUIAreaData(Obj)^.mad_Box.Height;
end;

function OBJ_Right(Obj: APTR): SmallInt; inline;
begin
  OBJ_Right := OBJ_Left(Obj) + OBJ_Width(Obj) - 1;
end;

function OBJ_Bottom(Obj: APTR): SmallInt; inline;
begin
  OBJ_Bottom := OBJ_Top(Obj) + OBJ_Height(Obj) - 1;
end;

function OBJ_AddLeft(Obj: APTR): SmallInt; inline;
begin
  OBJ_AddLeft := MUIAreaData(obj)^.mad_AddLeft;
end;

function OBJ_AddTop(Obj: APTR): SmallInt; inline;
begin
  OBJ_AddTop := MUIAreaData(obj)^.mad_AddTop;
end;

function OBJ_SubWidth(Obj: APTR): SmallInt; inline;
begin
  OBJ_SubWidth := MUIAreaData(obj)^.mad_SubWidth;
end;

function OBJ_SubHeight(Obj: APTR): SmallInt; inline;
begin
  OBJ_SubHeight := MUIAreaData(obj)^.mad_SubHeight;
end;

function OBJ_MLeft(Obj: APTR): SmallInt; inline;
begin
  OBJ_MLeft := OBJ_Left(obj) + OBJ_AddLeft(obj);
end;

function OBJ_MTop(Obj: APTR): SmallInt; inline;
begin
  OBJ_MTop := OBJ_Top(obj) + OBJ_AddTop(obj);
end;

function OBJ_MWidth(Obj: APTR): SmallInt; inline;
begin
  OBJ_MWidth := OBJ_Width(obj) - OBJ_SubWidth(obj);
end;

function OBJ_MHeight(Obj: APTR): SmallInt; inline;
begin
  OBJ_MHeight := OBJ_Height(obj) - OBJ_SubHeight(obj);
end;

function OBJ_MRight(Obj: APTR): SmallInt; inline;
begin
  OBJ_MRight := OBJ_MLeft(obj) + OBJ_MWidth(obj) - 1;
end;

function OBJ_MBottom(Obj: APTR): SmallInt; inline;
begin
  OBJ_MBottom := OBJ_MTop(obj) + OBJ_MHeight(obj) - 1;
end;

function OBJ_Font(Obj: APTR): PTextFont; inline;
begin
  OBJ_Font := MUIAreaData(obj)^.mad_Font;
end;

function OBJ_MinWidth(Obj: APTR): LongWord; inline;
begin
  OBJ_MinWidth := MUIAreaData(obj)^.mad_MinMax.MinWidth;
end;

function OBJ_MinHeight(Obj: APTR): LongWord; inline;
begin
  OBJ_MinHeight := MUIAreaData(obj)^.mad_MinMax.MinHeight;
end;

function OBJ_MaxWidth(Obj: APTR): LongWord; inline;
begin
  OBJ_maxWidth := MUIAreaData(obj)^.mad_MinMax.MaxWidth;
end;

function OBJ_MaxHeight(Obj: APTR): LongWord; inline;
begin
  OBJ_maxHeight := MUIAreaData(obj)^.mad_MinMax.MaxHeight;
end;

function OBJ_DefWidth(Obj: APTR): LongWord; inline;
begin
  OBJ_DefWidth := MUIAreaData(obj)^.mad_MinMax.DefWidth;
end;

function OBJ_DefHeight(Obj: APTR): LongWord; inline;
begin
  OBJ_DefHeight := MUIAreaData(obj)^.mad_MinMax.DefHeight;
end;

function OBJ_Flags(Obj: APTR): LongWord; inline;
begin
  OBJ_Flags := MUIAreaData(obj)^.mad_Flags;
end;

// 2 useful procedures for testing if some coordinates are inside your object (converted from the ones in class3.c. So look there how to use... )

function OBJ_Between(a,x,b : SmallInt): boolean; inline;
begin
  OBJ_Between := (x >= a) and (x <= b);
end;

function OBJ_IsInObject(x, y: SmallInt; Obj: PObject_): boolean; inline;
begin
  OBJ_IsInObject := OBJ_Between(OBJ_MLeft(obj), x, OBJ_MRight(obj)) and OBJ_Between(OBJ_MTop(obj), y, OBJ_MBottom(obj));
end;

function MUIV_Window_AltHeight_MinMax(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltHeight_MinMax := 0 - p;
end;

function MUIV_Window_AltHeight_Visible(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltHeight_Visible := -100 - p;
end;

function MUIV_Window_AltHeight_Screen(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltHeight_Screen := -200 - p;
end;

function MUIV_Window_AltTopEdge_Delta(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltTopEdge_Delta := -3 - p;
end;

function MUIV_Window_AltWidth_MinMax(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltWidth_MinMax := 0 - p;
end;

function MUIV_Window_AltWidth_Visible(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltWidth_Visible := -100 - p;
end;

function MUIV_Window_AltWidth_Screen(p: LongInt): LongInt; inline;
begin
  MUIV_Window_AltWidth_Screen := -200 - p;
end;

function MUIV_Window_Height_MinMax(p: LongInt): LongInt; inline;
begin
  MUIV_Window_Height_MinMax := 0 - p;
end;

function MUIV_Window_Height_Visible(p: LongInt): LongInt; inline;
begin
  MUIV_Window_Height_Visible := -100 - p;
end;

function MUIV_Window_Height_Screen(p: LongInt): LongInt; inline;
begin
  MUIV_Window_Height_Screen := -200 - p;
end;

function MUIV_Window_TopEdge_Delta(p: LongInt): LongInt; inline;
begin
  MUIV_Window_TopEdge_Delta := -3 - p;
end;

function MUIV_Window_Width_MinMax(p: LongInt): LongInt; inline;
begin
  MUIV_Window_Width_MinMax := 0 - p;
end;

function MUIV_Window_Width_Visible(p: LongInt): LongInt; inline;
begin
  MUIV_Window_Width_Visible := -100 - p;
end;

function MUIV_Window_Width_Screen(p : LongInt) : LongInt; inline;
begin
  MUIV_Window_Width_Screen := (-200 - (p));
end;

// Functions and procedures with array of const go here

function MUI_AllocAslRequestTags(ReqTyp : longword; const Tags: array of PtrUInt) : Pointer; inline;
begin
  MUI_AllocAslRequestTags := MUI_AllocAslRequest(ReqTyp, @Tags);
end;

function MUI_AslRequestTags(Req: Pointer; const Tags: array of PtrUInt) : LongBool; inline;
begin
  MUI_AslRequestTags := MUI_AslRequest(Req, @Tags);
end;

function MUI_MakeObject(_Type : LongInt; const Params: array of PtrUInt): PObject_; inline;
begin
  MUI_MakeObject := MUI_MakeObjectA(_Type, @Params);
end;

function MUI_NewObject(a0arg: PChar; const Tags: array of PtrUInt): PObject_; inline;
begin
  MUI_NewObject := MUI_NewObjectA(a0arg , @Tags);
end;

function MUI_Request(App: Pointer; win: Pointer; Flags: LongWord; Title: PChar; Gadgets: PChar; Format: PChar; const Params: Array Of PtrUInt): LongInt;
begin
  MUI_Request := MUI_RequestA(App, Win, Flags, Title, Gadgets, Format, @Params);
end;

const
  // Change LIBVERSION to proper values
  LIBVERSION: LongWord = 0;

initialization
  MUIMasterBase := OpenLibrary(MUIMASTER_NAME, LIBVERSION);
  if Assigned(MUIMasterBase) then
    IMui := GetInterface(MUIMasterBase, 'main', 1, nil);
finalization
  if Assigned(IMui) then
    DropInterface(IMui);
  if Assigned(MUIMasterBase) then
    CloseLibrary(MUIMasterBase);
end.
