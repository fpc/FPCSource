{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    intuition.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Intuition;

{$mode objfpc}

{$define INTUI_V36_NAMES_ONLY}

interface

uses
  Exec, Utility, AGraphics, InputEvent, Timer, Layers, Keymap;


type
{ IntuiText is a series of strings that start with a screen location
  (always relative to the upper-left corner of something) and then the
  text of the string.  The text is null-terminated.}
  PIntuiText = ^TIntuiText;
  TIntuiText = record
    FrontPen,
    BackPen: Byte;         // the pen numbers for the rendering
    DrawMode: Byte;        // the mode for rendering the text
    LeftEdge: SmallInt;    // relative start location for the text
    TopEdge: SmallInt;     // relative start location for the text
    ITextFont: PTextAttr;  // if nil, you accept the default
    IText: PChar;          // pointer to null-terminated text
    NextText: PIntuiText;  // continuation to TxWrite another text
  end;

  PStringExtend = ^TStringExtend;
  TStringExtend = record
    // display specifications
    Font: PTextFont;                   // must be an open Font (not TextAttr)
    Pens: array[0..1] of Byte;         // color of text/background
    ActivePens: array[0..1] of Byte;   // colors when gadget is active
    // edit specifications
    InitialModes: LongWord;            // initial mode flags, below
    EditHook: PHook;                   // IF non-nil, must supply WorkBuffer
    WorkBuffer: STRPTR;                // must be as large as StringInfo.Buffer
    Reserved: array[0..3] of LongWord; // set to 0
  end;
  
{ Data type Border, used for drawing a series of lines which is intended for use as a border drawing, but which may, in fact, be used to render any
  arbitrary vector shape. The routine DrawBorder sets up the RastPort with the appropriate variables, then does a Move to the first coordinate, then does Draws
  to the subsequent coordinates. After all the Draws are done, if NextBorder is non-zero we call DrawBorder recursively}
type
  PBorder = ^TBorder;
  TBorder = record
    LeftEdge,
    TopEdge: SmallInt;   // initial offsets from the origin
    FrontPen,
    BackPen: Byte;       // pens numbers for rendering
    DrawMode: Byte;      // mode for rendering
    Count: ShortInt;     // number of XY pairs
    XY: PSmallInt;       // vector coordinate pairs rel to LeftTop
    NextBorder: PBorder; // pointer to any other Border too
  end;

type
  PMenuItem = ^TMenuItem;
  TMenuItem = record
    NextItem: PMenuItem;    // pointer to next in chained list
    LeftEdge,
    TopEdge: SmallInt;      // position of the select box
    Width,
    Height: SmallInt;       // dimensions of the select box
    Flags: Word;            // see the defines below
    MutualExclude: LongInt; // set bits mean this item excludes that
    ItemFill: APTR;         // points to Image, IntuiText, or nil
    SelectFill: APTR;       // points to Image, IntuiText, or nil when this item is pointed to by the cursor and the items highlight mode HIGHIMAGE is selected, this alternate image will be displayed
    Command: Char;          // only if appliprog sets the COMMSEQ flag
    SubItem: PMenuItem;     // if non-nil, DrawMenu shows "->"
    NextSelect: Word;       // The NextSelect field represents the menu number of next selected item (when user has drag-selected several items)
  end;
const
  // Flags set by the Application
  CHECKIT       = 1 shl 0;  // whether to check this item if selected
  ITEMTEXT      = 1 shl 1;  // set if textual, clear if graphical item
  COMMSEQ       = 1 shl 2;  // set if there's an command sequence
  MENUTOGGLE    = 1 shl 3;  // set to toggle the check of a menu item
  ITEMENABLED   = 1 shl 4;  // set if this item is enabled
  // these are the SPECIAL HIGHLIGHT FLAG state meanings
  HIGHIMAGE     = $0000;    // use the user's "select image"
  HIGHCOMP      = 1 shl 6;  // highlight by complementing the selectbox
  HIGHBOX       = 1 shl 7;  // highlight by "boxing" the selectbox
  HIGHFLAGS     = $00C0;    // see definitions below for these bits
  HIGHNONE      = $00C0;    // don't highlight
  // Flags set by both Application and intuition
  Checked       = 1 shl 8;  // if CHECKIT, then set this when selected
  // Flags set by Intuition (Read Only)
  ISDRAWN       = 1 shl 12; // this item's subs are currently drawn
  HIGHITEM      = 1 shl 13; // this item is currently highlighted
  MENUTOGGLED   = 1 shl 14; // this item was already toggled

  NOMENU   = $001F;
  NOITEM   = $003F;
  NOSUB    = $001F;
  MENUNULL = $FFFF;

{ these defines are for the COMMSEQ and CHECKIT menu stuff.  If CHECKIT,
  I'll use a generic Width (for all resolutions) for the CheckMark.
  If COMMSEQ, likewise I'll use this generic stuff}
  CHECKWIDTH    = 19;
  LOWCHECKWIDTH = 13;
  COMMWIDTH     = 27; 
  LOWCOMMWIDTH  = 16;

type
  PMenu = ^TMenu;
  TMenu = record
    NextMenu: PMenu;      // same level
    LeftEdge,
    TopEdge: SmallInt;    // position of the select box
    Width,
    Height: SmallInt;     // dimensions of the select box
    Flags: Word;          // see flag definitions below (MENUENABLED, MIDRAWN)
    MenuName: PChar;      // text for this Menu Header 
    FirstItem: PMenuItem; // pointer to first in chain 
    JazzX,                // these mysteriously-named variables are for internal use only
    JazzY,
    BeatX,
    BeatY: SmallInt;
  end;

const
  // FLAGS SET BY BOTH THE APPLIPROG AND INTUITION
  MENUENABLED = 1 shl 0; // whether or not this menu is enabled
  // FLAGS SET BY INTUITION }
  MIDRAWN     = 1 shl 8; // this menu's items are currently drawn

type
  PGadget = ^TGadget;
  TGadget = record
    NextGadget: PGadget;  // next gadget in the list

    LeftEdge,
    TopEdge: SmallInt;    // "hit box" of gadget
    Width,
    Height: SmallInt;     // "hit box" of gadget

    Flags: Word;          // see below for list of defines GFLG_*
    Activation: Word;     // see below for list of defines GACT_*
    GadgetType: Word;     // see below for defines GTYP_*

    GadgetRender: APTR;     // appliprog can specify that the Gadget be rendered as either as Border
                            // or an Image.  This variable points to which (or equals nil if there's nothing to be rendered about this Gadget)
    SelectRender: APTR;     // appliprog can specify "highlighted" imagery rather than algorithmic this can point to either Border or Image data
    GadgetText: PIntuiText; // text for this gadget

    MutualExclude: IPTR;    // obsolete
    
    SpecialInfo: APTR;      // pointer to a structure of special data required by Proportional, String and LongInt Gadgets
    GadgetID: Word;         // user-definable ID field
    UserData: APTR;         // ptr to general purpose User data (ignored by In)
  end;

  PExtGadget = ^TExtGadget;
  TExtGadget = record
    // The first fields match struct Gadget exactly
    NextGadget: PExtGadget;  // Matches struct Gadget
    LeftEdge, TopEdge,       // Matches struct Gadget 
    Width, Height: SmallInt; // Matches struct Gadget
    Flags,                   // Matches struct Gadget
    Activation,              // Matches struct Gadget
    GadgetType: Word;        // Matches struct Gadget
    GadgetRender,            // Matches struct Gadget
    SelectRender: APTR;      // Matches struct Gadget
    GadgetText: PIntuiText;  // Matches struct Gadget
    MutualExclude: IPTR;     // Matches struct Gadget
    SpecialInfo: APTR;       // Matches struct Gadget
    GadgetID: Word;          // Matches struct Gadget
    UserData: APTR;          // Matches struct Gadget

    // These fields only exist if GFLG_EXTENDED is set
    MoreFlags: LongWord;     // see GMORE_* flags below
    BoundsLeftEdge,          // Bounding extent for gadget, valid
    BoundsTopEdge,           // only if GMORE_BOUNDS is set.  The
    BoundsWidth,             // GFLG_RELxxx flags affect these
    BoundsHeight: SmallInt;  // coordinates as well.
  end;
const
// Gadget.Flags values
  // combinations in these bits describe the highlight technique to be used
  
  GFLG_GADGHCOMP    = $0000;    // Complement the select box
  GFLG_GADGHBOX     = 1 shl 0;  // Draw a box around the image
  GFLG_GADGHIMAGE   = 1 shl 1;  // Blast in this alternate image
  GFLG_GADGHNONE    = $0003;    // don't highlight
  GFLG_GADGHIGHBITS = $0003;
  GFLG_GADGIMAGE    = 1 shl 2;  // set IF GadgetRender AND SelectRender point to an Image structure, clear if they point to Border structures
  { combinations in these next two bits specify to which corner the gadget's Left & Top coordinates are relative.  If relative to Top/Left,
    these are "normal" coordinates (everything is relative to something in this universe).
    Gadget positions and dimensions are relative to the window or requester which contains the gadget}
  GFLG_RELBOTTOM    = 1 shl 3;  // vert. pos. is relative to bottom edge
  GFLG_RELRIGHT     = 1 shl 4;  // horiz. pos. is relative to right edge
  GFLG_RELWIDTH     = 1 shl 5;  // width is relative to req/window
  GFLG_RELHEIGHT    = 1 shl 6;  // height is relative to req/window
  GFLG_SELECTED     = 1 shl 7;  // you may initialize AND look at this
  GFLG_DISABLED     = 1 shl 8;  // the GFLG_DISABLED flag is initialized by you and later set by Intuition according to your calls to On/OffGadget(). It specifies whether or not this Gadget is currently disabled from being selected
  GFLG_TABCYCLE     = 1 shl 9;  // (string OR custom) gadget participates in cycling activation with Tab or Shift-Tab
  GFLG_STRINGEXTEND = 1 shl 10; // this String Gadget has StringExtend
  GFLG_IMAGEDISABLE = 1 shl 11; // Gadget's image knows how to do disabled rendering
  GFLG_LABELITEXT   = $0000;    // GadgetText points to IntuiText
  GFLG_LABELSTRING  = 1 shl 12; // GadgetText points to (PChar)
  GFLG_LABELIMAGE   = 1 shl 13; // GadgetText points to Image (object)
  GFLG_LABELMASK    = $3000;
  GFLG_RELSPECIAL   = 1 shl 14; // custom gadget has special relativity. Gadget box values are absolutes, but can be changed via the GM_LAYOUT method.
  GFLG_EXTENDED     = 1 shl 15; // Gadget is extended
  
{GFLG_RELSPECIAL allows custom gadget implementors to
  make gadgets whose position and size depend in an arbitrary way
  on their window's dimensions.  The GM_LAYOUT method will be invoked
  for such a gadget (or any other GREL_xxx gadget) at suitable times,
  such as when the window opens or the window's size changes.
  
 GFLG_STRINGEXTEND.  We discovered that V34 doesn't properly
  ignore the value we had chosen for the Gadget->Activation flag
  GACT_STRINGEXTEND.  NEVER SET THAT FLAG WHEN RUNNING UNDER V34.
  The Gadget->Flags bit GFLG_STRINGEXTEND is provided as a synonym which is
  safe under V34, and equivalent to GACT_STRINGEXTEND under V37.
  (Note that the two flags are not numerically equal)
  
 GFLG_IMAGEDISABLE.  This flag is automatically set if
  the custom image of this gadget knows how to do disabled rendering
  (more specifically, if its IA_SupportsDisable attribute is TRUE).
  Intuition uses this to defer the ghosting to the image-class,
  instead of doing it itself (the old compatible way).
  Do not set this flag yourself - Intuition will do it for you.
  
 GFLG_EXTENDED. If set, this bit means that the Gadget is actually
  a struct ExtGadget, with new fields and flags.  All V39 boopsi
  gadgets are ExtGadgets.  Never ever attempt to read the extended
  fields of a gadget if this flag is not set.  
  
 GACT_FOLLOWMOUSE flag, when set, specifies that you want to receive
  reports on mouse movements while this gadget is active.
  You probably want to set the GACT_IMMEDIATE flag when using
  GACT_FOLLOWMOUSE, since that's the only reasonable way you have of
  learning why Intuition is suddenly sending you a stream of mouse
  movement events.  If you don't set GACT_RELVERIFY, you'll get at
  least one Mouse Position event. 
  }

// Gadget.Activation flag values
  GACT_RELVERIFY    = 1 shl 0; // Set if you want to verify that the pointer was still over the gadget when the select button was released.  Will cause an IDCMP_GADGETUP message to be sent if so.
  GACT_IMMEDIATE    = 1 shl 1; // when set, informs the caller that the gadget was activated when it was activated.  This flag works in conjunction with the GACT_RELVERIFY flag
  GACT_ENDGADGET    = 1 shl 2; // when set, tells the system that this gadget, when selected, causes the Requester to be ended.  Requesters that are ended are erased and unlinked from the system.
  GACT_FOLLOWMOUSE  = 1 shl 3;
  // if any of the BORDER flags are set in a Gadget that's included in the Gadget list when a Window is opened, the corresponding Border will be adjusted to make room for the Gadget
  GACT_RIGHTBORDER  = 1 shl 4;
  GACT_LEFTBORDER   = 1 shl 5;
  GACT_TOPBORDER    = 1 shl 6;
  GACT_BOTTOMBORDER = 1 shl 7;
  GACT_TOGGLESELECT = 1 shl 8; // this bit for toggle-select mode
  // should properly be in StringInfo, but aren't 
  GACT_STRINGLEFT   = $0000;   // NOTE WELL: that this has value zero
  GACT_STRINGCENTER = 1 shl 9;
  GACT_STRINGRIGHT  = 1 shl 10;
  GACT_LONGINT      = 1 shl 11;  // this String Gadget is for Long Ints
  GACT_ALTKEYMAP    = 1 shl 12;  // this String has an alternate keymap
  GACT_STRINGEXTEND = 1 shl 13;  // this String Gadget has StringExtend NOTE: NEVER SET GACT_STRINGEXTEND IF YOU ARE RUNNING ON LESS THAN V36! SEE GFLG_STRINGEXTEND (ABOVE) INSTEAD  
  GACT_BOOLEXTEND   = 1 shl 13;  // this Boolean Gadget has a BoolInfo
  GACT_ACTIVEGADGET = 1 shl 14;  // this gadget is "active".  This flag is maintained by Intuition, and you cannot count on its value persisting
                                 // while you do something on your program's task.  It can only be trusted by people implementing custom gadgets
  GACT_BORDERSNIFF  = 1 shl 15;  // neither set nor rely on this bit

// GADGET TYPES
 {These are the Gadget type definitions for the variable GadgetType
  gadget number type MUST start from one.  NO TYPES OF ZERO ALLOWED.
  first comes the mask for Gadget flags reserved for Gadget typing}
  GTYP_GADGETTYPE  = $FC00;  // all Gadget Global type flags (padded)
  GTYP_SYSTYPEMASK = $00F0;  
  // system gadgets
  GTYP_SIZING      = $0010;
  GTYP_WDRAGGING   = $0020;
  GTYP_SDRAGGING   = $0030;
  GTYP_WDEPTH      = $0040;
  GTYP_SDEPTH      = $0050;
  GTYP_WZOOM       = $0060;
  GTYP_SUNUSED     = $0070;
  GTYP_CLOSE       = $0080;
  // application gadgets
  GTYP_REQGADGET    = 1 shl 12;  // 1 = this is a Requester Gadget
  GTYP_GZZGADGET    = 1 shl 13;  // 1 = for WFLG_GIMMEZEROZERO borders
  GTYP_SCRGADGET    = 1 shl 14;  // 1 = ScreenGadget, 0 = WindowGadget
  GTYP_SYSGADGET    = 1 shl 15;  // 1 = Allocated by the system, 0 = by app.
  GTYP_BOOLGADGET   = $0001;
  GTYP_GADGET0002   = $0002;
  GTYP_PROPGADGET   = $0003;
  GTYP_STRGADGET    = $0004;
  GTYP_CUSTOMGADGET = $0005;
  GTYP_GTYPEMASK    = $0007; //mask you can apply to tell what class of gadget this is.  The possible classes follow.

{ Gadgets which have the GFLG_EXTENDED flag set are
 * actually ExtGadgets, which have more flags.  The GMORE_xxx
 * identifiers describe those flags.  For GMORE_SCROLLRASTER, see
 * important information in the ScrollWindowRaster() autodoc.
 * NB: GMORE_SCROLLRASTER must be set before the gadget is
 * added to a window.
 }
  GMORE_BOUNDS       = 1 shl 0; // ExtGadget has valid Bounds
  GMORE_GADGETHELP   = 1 shl 1; // This gadget responds to gadget help
  GMORE_SCROLLRASTER = 1 shl 2; // This (custom) gadget uses ScrollRaster
  GMORE_BOOPSIGADGET = 1 shl 3; // some internal boopsi classes changes the gadget type during execution (ie propgclass), so GTYP_CUSTOMGADGET doesn´t work (dariusb)

type
  // Bool Gadget This is the special data needed by an Extended Boolean Gadget Typically this structure will be pointed to by the Gadget field SpecialInfo
  PBoolInfo = ^TBoolInfo;
  TBoolInfo = record
    Flags: Word;        // defined below (BOOLMASK)
    Mask: PWord;        // bit mask for highlighting and selecting mask must follow the same rules as an Image
                        // plane.  It's width and height are determined by the width and height of the gadget's select box. (i.e. Gadget.Width and .Height).
    Reserved: LongWord; // set to 0 
  end;
const
// set BoolInfo.Flags to this flag bit. in the future, additional bits might mean more stuff hanging  off of BoolInfo.Reserved.
  BOOLMASK   = 1 shl 0;  // extension is for masked gadget
type
  // Proportional Gadget this is the special data required by the proportional Gadget typically, this data will be pointed to by the Gadget variable SpecialInfo
  PPropInfo = ^TPropInfo;
  TPropInfo = record
    Flags: Word;        // general purpose flag bits (see defines below)

    { You initialize the Pot variables before the Gadget is added to
      the system.  Then you can look here for the current settings
      any time, even while User is playing with this Gadget.  To
      adjust these after the Gadget is added to the System, use
      ModifyProp();  The Pots are the actual proportional settings,
      where a value of zero means zero and a value of MAXPOT means
      that the Gadget is set to its maximum setting.}
    HorizPot: Word;     // 16-bit FixedPoint horizontal quantity percentage
    VertPot: Word;      // 16-bit FixedPoint vertical quantity percentage
    { the 16-bit FixedPoint Body variables describe what percentage of
      the entire body of stuff referred to by this Gadget is actually
      shown at one time.  This is used with the AUTOKNOB routines,
      to adjust the size of the AUTOKNOB according to how much of
      the data can be seen.  This is also used to decide how far
      to advance the Pots when User hits the Container of the Gadget.
      For instance, if you were controlling the display of a 5-line
      Window of text with this Gadget, and there was a total of 15
      lines that could be displayed, you would set the VertBody value to
          (MAXBODY / (TotalLines / DisplayLines)) = MAXBODY / 3.
      Therefore, the AUTOKNOB would fill 1/3 of the container, and
      if User hits the Cotainer outside of the knob, the pot would
      advance 1/3 (plus or minus) If there's no body to show, or
      the total amount of displayable info is less than the display area,
      set the Body variables to the MAX.  To adjust these after the
      Gadget is added to the System, use ModifyProp();}
    HorizBody: Word;    // horizontal Body
    VertBody: Word;     // vertical Body
    // these are the variables that Intuition sets and maintains
    CWidth: Word;       // Container width (with any relativity absoluted)
    CHeight: Word;      // Container height (with any relativity absoluted)
    HPotRes,
    VPotRes: Word;      // pot increments
    LeftBorder: Word;   // Container borders
    TopBorder: Word;    // Container borders
  end;

const
// Flags BITS
  AUTOKNOB       = 1 shl 0; // this flag sez:  gimme that old auto-knob NOTE: if you do not use an AUTOKNOB for a proportional gadget,
                            // you are currently limited to using a single Image of your own design: Intuition won't handle a linked list
                            // of images as a proportional gadget knob.
  FREEHORIZ      = 1 shl 1; // IF set, the knob can move horizontally
  FREEVERT       = 1 shl 2; // IF set, the knob can move vertically
  PROPBORDERLESS = 1 shl 3; // IF set, no border will be rendered
  KNOBHIT        = 1 shl 8; // set when this Knob is hit
  PROPNEWLOOK    = 1 shl 4; // set this IF you want to get the new look

  KNOBHMIN = 6;     // minimum horizontal size of the Knob
  KNOBVMIN = 4;     // minimum vertical size of the Knob
  MAXBODY  = $FFFF; // maximum body value
  MAXPOT   = $FFFF; // maximum pot value

type
  // StringInfo this is the special data required by the string Gadget typically, this data will be pointed to by the Gadget variable SpecialInfo
  PStringInfo = ^TStringInfo;
  TStringInfo = record
    // you initialize these variables, and then Intuition maintains them
    Buffer: PChar;       // the buffer containing the start and final string
    UndoBuffer: PChar;   // optional buffer for undoing current entry
    BufferPos: SmallInt; // character position in Buffer
    MaxChars: SmallInt;  // max number of chars in Buffer (including #0)
    DispPos: SmallInt;   // Buffer position of first displayed character
    // Intuition initializes and maintains these variables for you
    UndoPos: SmallInt;   // character position in the undo buffer
    NumChars: SmallInt;  // number of characters currently in Buffer
    DispCount: SmallInt; // number of whole characters visible in Container
    CLeft,
    CTop: SmallInt;      // topleft offset of the container
    { you can initialize this variable before the gadget is submitted to
      Intuition, and then examine it later to discover what LongInt
      the user has entered (if the user never plays with the gadget,
      the value will be unchanged from your initial setting)}
    Extension: PStringExtend;
    _LongInt: LongInt;
    { If you want this Gadget to use your own Console keymapping, you
      set the ALTKEYMAP bit in the Activation flags of the Gadget, and then
      set this variable to point to your keymap.  If you don't set the
      ALTKEYMAP, you'll get the standard ASCII keymapping.}
    AltKeyMap: PKeymap;
  end;

type
  PImage = ^TImage;
  PWindow = ^TWindow;
  PScreen = ^TScreen;
// Requesters The following struct is used for standard intuition requesters (not to be mixed up with asl or easy requesters). See intuition.library/Request() for more information. 
  PRequester = ^TRequester;
  TRequester = record
    // the ClipRect and BitMap and used for rendering the requester
    OlderRequest: PRequester;
    LeftEdge,
    TopEdge: SmallInt;        // dimensions of the entire box
    Width,
    Height: SmallInt;         // dimensions of the entire box
    RelLeft,
    RelTop: SmallInt;         // for Pointer relativity offsets

    ReqGadget: PGadget;       // First gadget of the requester
    ReqBorder: PBorder;       // First border of the requester
    ReqText: PIntuiText;      // First intuitext of the requester
    
    Flags: Word;              // see definitions below 
    BackFill: Byte;           // pen number for back-plane fill before draws

    ReqLayer: PLayer;         // Layer in place of clip rect

    ReqPad1: array [0..31] of Byte;  // Private

    { If the BitMap plane pointers are non-zero, this tells the system
      that the image comes pre-drawn (if the appliprog wants to define
      it's own box, in any shape or size it wants!);  this is OK by
      Intuition as long as there's a good correspondence between
      the image and the specified Gadgets}
    ImageBMap: PBitMap;    // you may use this to fill the requester with your own image
    RWindow: PWindow;      // window, which the requester belongs to
    ReqImage: PImage;      // corresponds to USEREQIMAGE (see below)
    ReqPad2: array[0..31] of ShortInt; // PRIVATE
  end;
  
{ The Timage.PlanePick and PlaneOnOff variables work much the same way as the equivalent GELS Bob variables.  It's a space-saving
  mechanism for image data.  Rather than defining the image data for every plane of the RastPort, you need define data only
  for the planes that are not entirely zero or one.  As you define your Imagery, you will often find that most of the planes
  ARE just as color selectors.  For instance, if you're designing a two-color Gadget to use colors two and three, and the Gadget
  will reside in a five-plane display, bit plane zero of your imagery would be all ones, bit plane one would have data that
  describes the imagery, and bit planes two through four would be all zeroes.  Using these flags allows you to avoid wasting all
  that memory in this way:  first, you specify which planes you want your data to appear in using the PlanePick variable.  For
  each bit set in the variable, the next "plane" of your image data is blitted to the display.  For each bit clear in this
  variable, the corresponding bit in PlaneOnOff is examined. If that bit is clear, a "plane" of zeroes will be used.
  If the bit is set, ones will go out instead.  So, for our example:
    Gadget.PlanePick = $02;
    Gadget.PlaneOnOff = $01;
  Note that this also allows for generic Gadgets, like the System Gadgets, which will work in any number of bit planes.
  Note also that if you want an Image that is only a filled rectangle, you can get this by setting PlanePick to zero
  (pick no planes of data) and set PlaneOnOff to describe the pen color of the rectangle.}

// This is a brief image structure for very simple transfers of image data to a RastPort
  TImage = record
    LeftEdge: SmallInt; // starting offset relative to some origin
    TopEdge: SmallInt;  // starting offsets relative to some origin
    Width: SmallInt;    // pixel size (though data is Word-aligned)
    Height: SmallInt;
    Depth: SmallInt;    // pixel sizes
    ImageData: PWord;   // pointer to the actual Word-aligned bits
    PlanePick,
    PlaneOnOff: Byte;
    NextImage: PImage;   // if not nil, Intuition presumes that it points to another Image structure with another Image to be rendered
  end;

{ If your window sets WA_TabletMessages to TRUE, then it will receive extended IntuiMessages (struct ExtIntuiMessage) whose eim_TabletData
  field points at a TabletData structure.  This structure contains additional information about the input event.}
  PTabletData = ^TTabletData;
  TTabletData = record
    td_XFraction,         // Sub-pixel position of tablet, in screen coordinates, scaled to fill a Word fraction:
    td_YFraction: Word; 
    td_TabletX,
    td_TabletY: LongWord; // Current tablet coordinates along each axis
    td_RangeX,
    td_RangeY: LongWord;  // Tablet range along each axis.  For example, if td_TabletX can take values 0-999, td_RangeX should be 1000.
    td_TagList: PTagItem; // Pointer to tag-list of additional tablet attributes. TABLETA_*
  end;

{ If a tablet driver supplies a hook for ient_CallBack, it will be invoked in the standard hook manner.  A0 will point to the Hook
  itself, A2 will point to the InputEvent that was sent, and A1 will point to a TabletHookData structure.  The InputEvent's
  ie_EventAddress field points at the IENewTablet structure that the driver supplied.
  Based on the thd_Screen, thd_Width, and thd_Height fields, the driver should scale the ient_TabletX and ient_TabletY fields and store the
  result in ient_ScaledX, ient_ScaledY, ient_ScaledXFraction, and ient_ScaledYFraction.
  The tablet hook must currently return NULL.  This is the only acceptable return-value.}
  PTabletHookData = ^TTabletHookData;
  TTabletHookData = record  
    thd_Screen: PScreen;        // Pointer to the active screen: Note: if there are no open screens, thd_Screen will be nil.
    thd_Width,                  // thd_Width and thd_Height will then describe an NTSC 64$400 screen.  Please scale accordingly.
    thd_Height: LongWord;       // The width and height (measured in pixels of the active screen)that your are to scale to:
    thd_ScreenChanged: LongInt; // Non-zero if the screen or something about the screen  changed since the last time you were invoked:
  end;

  PIntuiMessage = ^TIntuiMessage;
  TIntuiMessage = record
    ExecMessage: TMessage;
    IClass: LongWord;      // the Class bits correspond directly with the IDCMP Flags, except for the special bit LONELYMESSAGE (defined below)
    Code: Word;            // the Code field is for special values like MENU number
    Qualifier: Word;       // the Qualifier field is a copy of the current InputEvent's Qualifier
    IAddress: APTR;        // IAddress contains particular addresses for Intuition functions, like the pointer to the Gadget or the Screen

    MouseX,            // when getting mouse movement reports, any event you get will have the the mouse coordinates in these variables. 
    MouseY: SmallInt;  // the coordinates are relative to the upper-left corner of your Window (GIMMEZEROZERO notwithstanding)
    Seconds,           // the time values are copies of the current system clock time.
    Micros: LongWord;  // Micros are in units of microseconds, Seconds in seconds.

    IDCMPWindow: PWindow; // the IDCMPWindow variable will always have the Pointer of the Window of this IDCMP
    SpecialLink: PIntuiMessage; // system-use variable
  end;

{ All IntuiMessages are now slightly extended.  The ExtIntuiMessage
  structure has an additional field for tablet data, which is usually
  nil.  If a tablet driver which is sending IESUBCLASS_NEWTABLET
  events is installed in the system, windows with the WA_TabletMessages
  property set will find that eim_TabletData points to the TabletData
  structure.  Applications must first check that this field is non-NULL;
  it will be nil for certain kinds of message, including mouse activity
  generated from other than the tablet (i.e. the keyboard equivalents
  or the mouse itself).
  NOTE: This structure is subject to grow in the future.  Making
  assumptions about its size is A BAD IDEA.}

  PExtIntuiMessage = ^TExtIntuiMessage;
  TExtIntuiMessage = record
    eim_IntuiMessage: TIntuiMessage;
    eim_TabletData: PTabletData;
  end;

  // A data structure common in Intuition processing
  PIBox = ^TIBox;
  TIBox = record
    Left,
    Top,
    Width,
    Height: SmallInt;
  end;

// Window
  TWindow = record
    NextWindow: PWindow;   // for the linked list in a screen
    
    LeftEdge,
    TopEdge: SmallInt;     // screen dimensions of window
    Width,
    Height: SmallInt;      // screen dimensions of window
{$ifdef AROS_FLAVOUR_BINCOMPAT}
    MouseY,
    MouseX: SmallInt;      // relative to upper-left of window
{$else}
    MouseX,
    MouseY: SmallInt;      // relative to upper-left of window
{$endif}
    MinWidth,
    MinHeight: SmallInt;   // minimum sizes
    MaxWidth,
    MaxHeight: Word;       // maximum sizes

    Flags: LongWord;       // see below for defines
    MenuStrip: PMenu;      // the strip of Menu headers
    Title: PChar;          // the title text for this window
    FirstRequest: PRequester; // all active Requesters
    DMRequest: PRequester;    // double-click Requester
    ReqCount: SmallInt;    // count of reqs blocking Window
    WScreen: PScreen;      // this Window's Screen
    RPort: PRastPort;  { this Window's very own RastPort }
      { the border variables describe the window border.   If you specify
        GIMMEZEROZERO when you open the window, then the upper-left of the
        ClipRect for this window will be upper-left of the BitMap (with correct
        offsets when in SuperBitMap mode; you MUST select GIMMEZEROZERO when
        using SuperBitMap).  If you don't specify ZeroZero, then you save
        memory (no allocation of RastPort, Layer, ClipRect and associated
        Bitmaps), but you also must offset all your writes by BorderTop,
        BorderLeft and do your own mini-clipping to prevent writing over the
        system gadgets}
    BorderLeft,
    BorderTop,
    BorderRight,
    BorderBottom: ShortInt;
    BorderRPort: PRastPort;
    { You supply a linked-list of Gadgets for your Window.
      This list DOES NOT include system gadgets.  You get the standard
      window system gadgets by setting flag-bits in the variable Flags (see
      the bit definitions below)}
    FirstGadget: PGadget;
    Parent,
    Descendant: PWindow; // these are for opening/closing the windows

    // sprite data iformation for your own Pointer set these AFTER you Open the Window by calling SetPointer()
    _Pointer: PWord;      // sprite data
    PtrHeight: ShortInt;  // sprite height (not including sprite padding)
    PtrWidth: ShortInt;   // sprite width (must be less than or equal to 16)
    XOffset,
    YOffset: ShortInt;    // sprite offsets

    // the IDCMP Flags and User's and Intuition's Message Ports
    IDCMPFlags: LongWord; // User-selected flags
    UserPort,
    WindowPort: PMsgPort;
    MessageKey: PIntuiMessage;

    DetailPen,
    BlockPen: Byte;       // for bar/border/gadget rendering
    CheckMark: PImage;    // the CheckMark is a pointer to the imagery that will be used when  rendering MenuItems of this Window that want to be checkmarked if this is equal to NULL, you'll get the default imagery
    ScreenTitle: PChar;   // if non-nil, Screen title when Window is active

    { These variables have the mouse coordinates relative to the
      inner-Window of GIMMEZEROZERO Windows.  This is compared with the
      MouseX and MouseY variables, which contain the mouse coordinates
      relative to the upper-left corner of the Window, GIMMEZEROZERO
      notwithstanding}
    GZZMouseX: SmallInt;
    GZZMouseY: SmallInt;
    GZZWidth: SmallInt;  // these variables contain the width and height of the inner-Window of GIMMEZEROZERO Windows
    GZZHeight: SmallInt;

    ExtData: PByte;
    UserData: PSmallInt; // general-purpose pointer to User data extension
    
    WLayer: PLayer;
    IFont: PTextFont;

    MoreFlags: LongWord;

    RelLeftEdge: SmallInt; // relative coordinates of the window to its parent window. If it is 
    RelTopEdge: SmallInt;  // a window on the screen then these are the same as LeftEdge and TopEdge.
    
    FirstChild: PWindow;   // pointer to first child
    PrevChild: PWindow;    // if window is a child of a window
    NextChild: PWindow;    // then they are concatenated here.
    Parent2: PWindow;      // parent of this window
    // Data beyond this point are Intuition Private.  DO NOT USE
  end;

  TScreen = record
    NextScreen: PScreen;  // linked list of screens
    FirstWindow: PWindow; // linked list Screen's Windows

    LeftEdge,
    TopEdge: SmallInt;    // parameters of the screen
    Width,
    Height: SmallInt;     // parameters of the screen
    
{$ifdef AROS_FLAVOUR_BINCOMPAT}
    MouseY,
    MouseX: SmallInt;     // position relative to upper-left
{$else}
    MouseX,
    MouseY: SmallInt;     // position relative to upper-left
{$endif}

    Flags: Word;          // see definitions below
    Title: PChar;         // null-terminated Title text
    DefaultTitle: PChar;  // for Windows without ScreenTitle

    // Bar sizes for this Screen and all Window's in this Screen
    BarHeight,
    BarVBorder,
    BarHBorder,
    MenuVBorder,
    MenuHBorder: ShortInt;
    WBorTop,
    WBorLeft,
    WBorRight,
    WBorBottom: ShortInt;

    Font: PTextAttr;       // this screen's default font

    // the display data structures for this Screen (note the prefix S)
    ViewPort: TViewPort;    // describing the Screen's display
    RastPort: TRastPort;    // describing Screen rendering
    BitMap: TBitMap;        // extra copy of RastPort BitMap obsolete
    LayerInfo: TLayer_Info; // each screen gets a LayerInfo

  // You supply a linked-list of Gadgets for your Screen. This list DOES NOT include system Gadgets. You get the standard system Screen Gadgets by default
    FirstGadget: PGadget;

    DetailPen,
    BlockPen: Byte;         // for bar/border/gadget rendering

  // the following variable(s) are maintained by Intuition to support the DisplayBeep() color flashing technique
    SaveColor0: Word;
    BarLayer: PLayer;       // This layer is for the Screen and Menu bars
    ExtData: Pointer;
    UserData: Pointer;
  { general-purpose pointer to User data extension 
    **** Data below this point are SYSTEM PRIVATE ****}
  end;
  
const
// IDCMP Classes
  // Please refer to the Autodoc for OpenWindow() and to the Rom Kernel  Manual for full details on the IDCMP classes.
  IDCMP_SIZEVERIFY     = 1 shl 0;
  IDCMP_NEWSIZE        = 1 shl 1;
  IDCMP_REFRESHWINDOW  = 1 shl 2;
  IDCMP_MOUSEBUTTONS   = 1 shl 3;
  IDCMP_MOUSEMOVE      = 1 shl 4;
  IDCMP_GADGETDOWN     = 1 shl 5;
  IDCMP_GADGETUP       = 1 shl 6;
  IDCMP_REQSET         = 1 shl 7;
  IDCMP_MENUPICK       = 1 shl 8;
  IDCMP_CLOSEWINDOW    = 1 shl 9;
  IDCMP_RAWKEY         = 1 shl 10;
  IDCMP_REQVERIFY      = 1 shl 11;
  IDCMP_REQCLEAR       = 1 shl 12;
  IDCMP_MENUVERIFY     = 1 shl 13;
  IDCMP_NEWPREFS       = 1 shl 14;
  IDCMP_DISKINSERTED   = 1 shl 15;
  IDCMP_DISKREMOVED    = 1 shl 16;
  IDCMP_WBENCHMESSAGE  = 1 shl 17;  // System use only
  IDCMP_ACTIVEWINDOW   = 1 shl 18;
  IDCMP_INACTIVEWINDOW = 1 shl 19;
  IDCMP_DELTAMOVE      = 1 shl 20;
  IDCMP_VANILLAKEY     = 1 shl 21;
  IDCMP_INTUITICKS     = 1 shl 22;
  IDCMP_IDCMPUPDATE    = 1 shl 23;  // for notifications from "boopsi" gadgets
  IDCMP_MENUHELP       = 1 shl 24;  // for getting help key report during menu session
  IDCMP_CHANGEWINDOW   = 1 shl 25;  // for notification of any move/size/zoom/change window
  IDCMP_GADGETHELP     = 1 shl 26;  
  IDCMP_LONELYMESSAGE  = 1 shl 31; { the IDCMP Flags do not use this special bit, which is cleared when
    Intuition sends its special message to the Task, and set when Intuition
    gets its Message back from the Task.  Therefore, I can check here to
    find out fast whether or not this Message is available for me to send }

// IDCMP Codes
  // This group of codes is for the IDCMP_CHANGEWINDOW message
  CWCODE_MOVESIZE = 0;  // Window was moved and/or sized
  CWCODE_DEPTH    = 1;  // Window was depth-arranged
  // This group of codes is for the IDCMP_MENUVERIFY function
  MENUHOT         = 1; // IntuiWants verification OR MENUCANCEL
  MENUCANCEL      = 2; // HOT Reply of this cancels Menu operation
  MENUWAITING     = 3; // Intuition simply wants a ReplyMsg() ASAP
  // These are internal tokens to represent state of verification attempts shown here as a clue.
  OKOK            = 1; // guy didn't care
  OKABORT         = 4; // window rendered question moot
  OKCANCEL        = 2; // window sent cancel reply
  // This group of codes is for the IDCMP_WBENCHMESSAGE messages
  WBENCHOPEN      = 1;
  WBENCHCLOSE     = 2;

type
  PNewWindow = ^TNewWindow;
  TNewWindow = record
    LeftEdge,
    TopEdge: SmallInt;        // screen dimensions of window
    Width,
    Height: SmallInt;         // screen dimensions of window
    
    DetailPen,
    BlockPen: Byte;           // for bar/border/gadget rendering 

    IDCMPFlags: LongWord;     // User-selected IDCMP flags
    Flags: LongWord;          // see Window struct for defines

    FirstGadget: PGadget; { You supply a linked-list of Gadgets for your Window.
                             This list DOES NOT include system Gadgets.  You get the standard
                             system Window Gadgets by setting flag-bits in the variable Flags (see
                             the bit definitions under the Window structure definition)}
    CheckMark: PImage;    { the CheckMark is a pointer to the imagery that will be used when
                             rendering MenuItems of this Window that want to be checkmarked
                             if this is equal to NULL, you'll get the default imagery}
    Title: PChar;         // the title text for this window
    Screen: PScreen;      { the Screen pointer is used only if you've defined a CUSTOMSCREEN and
                             want this Window to open in it.  If so, you pass the Pointer of the
                             Custom Screen structure in this variable.  Otherwise, this variable
                             is ignored and doesn't have to be initialized.}
    BitMap: PBitMap;      { SUPER_BITMAP Window?  If so, put the Pointer of your BitMap structure
                             in this variable.  If not, this variable is ignored and doesn't have
                             to be initialized}
    { the values describe the minimum and maximum sizes of your Windows.
      these matter only if you've chosen the WINDOWSIZING Gadget option,
      which means that you want to let the User to change the size of
      this Window.  You describe the minimum and maximum sizes that the
      Window can grow by setting these variables.  You can initialize
      any one these to zero, which will mean that you want to duplicate
      the setting for that dimension (if MinWidth == 0, MinWidth will be
      set to the opening Width of the Window).
      You can change these settings later using SetWindowLimits().
      If you haven't asked for a SIZING Gadget, you don't have to
      initialize any of these variables.}
    MinWidth,
    MinHeight: SmallInt;   // minimums
    MaxWidth,
    MaxHeight: Word;       // maximums

    WType: Word;           { the type variable describes the Screen in which you want this Window to
                              open.  The type value can either be CUSTOMSCREEN or one of the
                              system standard Screen Types such as WBENCHSCREEN.  See the
                              type definitions under the Screen structure}
  end;


{ The following structure is the future NewWindow.  Compatibility
  issues require that the size of NewWindow not change.
  Data in the common part (NewWindow) indicates the the extension
  fields are being used.
  NOTE WELL: This structure may be subject to future extension.
  Writing code depending on its size is not allowed.
 }
   PExtNewWindow = ^TExtNewWindow;
   TExtNewWindow = record
    LeftEdge,
    TopEdge: SmallInt;
    Width,
    Height: SmallInt;

    DetailPen,
    BlockPen: Byte;
    
    IDCMPFlags: LongWord;
    Flags: LongWord;
    
    FirstGadget: PGadget;
    CheckMark: PImage;
    Title: PChar;
    WScreen: PScreen;
    WBitMap: PBitMap;

    MinWidth,
    MinHeight: SmallInt;
    MaxWidth,
    MaxHeight: Word;

    { the type variable describes the Screen in which you want this Window to
      open.  The type value can either be CUSTOMSCREEN or one of the
      system standard Screen Types such as WBENCHSCREEN.  See the
      type definitions under the Screen structure.
      A new possible value for this field is PUBLICSCREEN, which
      defines the window as a 'visitor' window.  See below for
      additional information provided.}
    WType: Word;

    { if the NewWindow Flag value WFLG_NW_EXTENDED is set, then
       this field is assumed to point to an array ( or chain of arrays)
       of TagItem structures.  See also ExtNewScreen for another
       use of TagItems to pass optional data.
       see below for tag values and the corresponding data.}
    Extension: PTagItem;
  end;

const  
  TAG_DONE = 0; { terminates array of TagItems. ti_Data unused }
  TAG_END = TAG_DONE;

{ The TagItem ID's (ti_Tag values) for OpenWindowTagList() follow.
  They are values in a TagItem array passed as extension/replacement
  values for the data in NewWindow.  OpenWindowTagList() can actually
  work well with a nil NewWindow pointer. }
  WA_Dummy        = TAG_USER + 99; { $80000063   }
  // these tags simply override NewWindow parameters
  WA_Left         = WA_Dummy + 1;
  WA_Top          = WA_Dummy + 2;
  WA_Width        = WA_Dummy + 3;
  WA_Height       = WA_Dummy + 4;
  WA_DetailPen    = WA_Dummy + 5;
  WA_BlockPen     = WA_Dummy + 6;
  WA_IDCMP        = WA_Dummy + 7;
  WA_Flags        = WA_Dummy + 8;  // "bulk" initialization of NewWindow.Flags
  WA_Gadgets      = WA_Dummy + 9;
  WA_Checkmark    = WA_Dummy + 10;
  WA_Title        = WA_Dummy + 11; // means you don't have to call SetWindowTitles after you open your window
  WA_ScreenTitle  = WA_Dummy + 12;
  WA_CustomScreen = WA_Dummy + 13;
  WA_SuperBitMap  = WA_Dummy + 14;
  // also implies WFLG_SUPER_BITMAP property
  WA_MinWidth     = WA_Dummy + 15;
  WA_MinHeight    = WA_Dummy + 16;
  WA_MaxWidth     = WA_Dummy + 17;
  WA_MaxHeight    = WA_Dummy + 18;
  WA_InnerWidth   = WA_Dummy + 19;
  WA_InnerHeight  = WA_Dummy + 20; { You can specify the dimensions of the interior region of your window, independent of what the border widths will be.  You probably want
                                     to also specify WA_AutoAdjust to allow Intuition to move your window or even shrink it so that it is completely on screen.}
  WA_PubScreenName = WA_Dummy + 21; // declares that you want the window to open as a visitor on the public screen whose name is pointed to by (PChar) ti_Data}
  WA_PubScreen     = WA_Dummy + 22; { open as a visitor window on the public screen whose Pointer is in (PScreen) ti_Data. To ensure that this screen remains open, you
                                      should either be the screen's owner, have a window open on the screen, or use LockPubScreen().}
  WA_PubScreenFallBack = WA_Dummy + 23; { A Boolean, specifies whether a visitor window should "fall back" to the default public screen
                                          (or Workbench) if the named public screen isn't available}
  WA_WindowName    = WA_Dummy + 24;
  WA_Colors        = WA_Dummy + 25 unimplemented;
                                    { a ColorSpec array for colors to be set when this window is active.  This is not implemented, and may not be, since the default
                                     values to restore would be hard to track. We'd like to at least support per-window colors for the mouse pointer sprite.}
  WA_Zoom          = WA_Dummy + 26; { ti_Data points to an array of four Word's, the initial Left/Top/Width/Height values of the "alternate" zoom position/dimensions.
                                     It also specifies that you want a Zoom gadget for your window, whether or not you have a sizing gadget.}
  WA_MouseQueue    = WA_Dummy + 27; // ti_Data contains initial value for the mouse message backlog limit for this window
  WA_BackFill      = WA_Dummy + 28; // provides a "backfill hook" for your window's layer.
  WA_RptQueue      = WA_Dummy + 29; // initial value of repeat key backlog limit
  WA_SizeGadget    = WA_Dummy + 30; // These Boolean tag items are alternatives to the NewWindow.Flags boolean flags with similar names.
  WA_DragBar       = WA_Dummy + 31;
  WA_DepthGadget   = WA_Dummy + 32;
  WA_CloseGadget   = WA_Dummy + 33;
  WA_Backdrop      = WA_Dummy + 34;
  WA_ReportMouse   = WA_Dummy + 35;
  WA_NoCareRefresh = WA_Dummy + 36;
  WA_Borderless    = WA_Dummy + 37;
  WA_Activate      = WA_Dummy + 38;
  WA_RMBTrap       = WA_Dummy + 39;
  WA_WBenchWindow  = WA_Dummy + 40; // PRIVATE!!
  WA_SimpleRefresh = WA_Dummy + 41; // only specify if True
  WA_SmartRefresh  = WA_Dummy + 42; // only specify if True 
  WA_SizeBRight    = WA_Dummy + 43;
  WA_SizeBBottom   = WA_Dummy + 44;
  WA_AutoAdjust    = WA_Dummy + 45; // shift or squeeze the window's position and dimensions to fit it on screen.
  WA_GimmeZeroZero = WA_Dummy + 46; // equiv. to NewWindow.Flags WFLG_GIMMEZEROZERO
  WA_MenuHelp      = WA_Dummy + 47; // Enables IDCMP_MENUHELP:  Pressing HELP during menus will return IDCMP_MENUHELP message.
  WA_NewLookMenus  = WA_Dummy + 48; // Set to True if you want NewLook menus
  WA_AmigaKey      = WA_Dummy + 49; // Pointer to image for Amiga-key equiv in menus
  WA_NotifyDepth   = WA_Dummy + 50; // Requests IDCMP_CHANGEWINDOW message when window is depth arranged (imsg^.Code = CWCODE_DEPTH)
  // WA_Dummy + 51 is obsolete
  WA_Pointer       = WA_Dummy + 52; { Allows you to specify a custom pointer for your window. ti_Data points to a pointer object you obtained via
                                      "pointerclass". NULL signifies the default pointer. This tag may be passed to OpenWindowTags() or SetWindowPointer().}
  WA_BusyPointer   = WA_Dummy + 53; { ti_Data is boolean.  Set to True to request the standard busy pointer. This tag may be passed to OpenWindowTags() or SetWindowPointer().}
  WA_PointerDelay  = WA_Dummy + 54; { ti_Data is boolean.  Set to True to request that the changing of the pointer be slightly delayed.  The change
                                      will be called off if you call NewSetPointer() before the delay expires.  This allows you to post a busy-pointer even if you think
                                      the busy-time may be very Word, without fear of a flashing pointer. This tag may be passed to OpenWindowTags() or SetWindowPointer().}
  WA_TabletMessages = WA_Dummy + 55; { ti_Data is a boolean.  Set to True to request that tablet information be included in IntuiMessages sent to your window.
                                       Requires that something (i.e. a tablet driver) feed IESUBCLASS_NEWTABLET InputEvents into the system.  For a pointer to the TabletData,
                                       examine the ExtIntuiMessage^.eim_TabletData field.  It is UNSAFE to check this field when running on pre-V39 systems.  It's always
                                       safe to check this field under V39 and up, though it may be nil.}
  WA_HelpGroup     = WA_Dummy + 56; { When the active window has gadget help enabled, other windows of the same HelpGroup number will also get GadgetHelp.  This allows GadgetHelp
                                       to work for multi-windowed applications. Use GetGroupID() to get an ID number.  Pass this number as ti_Data to all your windows. See also the HelpControl() function.}
  WA_HelpGroupWindow = WA_Dummy + 57; { When the active window has gadget help enabled, other windows of the same HelpGroup will also get GadgetHelp.  This allows GadgetHelp to work
                                         for multi-windowed applications.  As an alternative to WA_HelpGroup, you can pass a pointer to any other window of the same group to join its help
                                         group.  Defaults to NULL, which has no effect. See also the HelpControl() function.}
  WA_ToolBox       = WA_Dummy + 58; // (LongBool) Make this window a Toolbox window

  // AROS specific tags
  WA_Priority 	    = WA_Dummy + 100;
  WA_Parent   	    = WA_Dummy + 101; // (PWindow) Make the window a child of the parent Window
  WA_InFrontOf	    = WA_Dummy + 102;
  WA_Behind   	    = WA_Dummy + 103;
  WA_Visible  	    = WA_Dummy + 104; // (LongBool) Make window visible. default True
  WA_Shape    	    = WA_Dummy + 105; // (PRegion)
  WA_ShapeHook	    = WA_Dummy + 106; // (PHook)

// --- Flags requested at OpenWindow() time by the application 
  WFLG_SIZEGADGET  = 1 shl 0; // include sizing system-gadget
  WFLG_DRAGBAR     = 1 shl 1; // include dragging system-gadget
  WFLG_DEPTHGADGET = 1 shl 2; // include depth arrangement gadget
  WFLG_CLOSEGADGET = 1 shl 3; // include close-box system-gadget
  WFLG_SIZEBRIGHT  = 1 shl 4; // size gadget uses right border
  WFLG_SIZEBBOTTOM = 1 shl 5; // size gadget uses bottom border
  // refresh modes combinations of the WFLG_REFRESHBITS select the refresh type
  WFLG_SMART_REFRESH  = 0;
  WFLG_SIMPLE_REFRESH = 1 shl 6;
  WFLG_SUPER_BITMAP   = 1 shl 7;
  WFLG_OTHER_REFRESH  = (1 shl 6) or (1 shl 7);
  WFLG_REFRESHBITS    = WFLG_OTHER_REFRESH;
  
  WFLG_BACKDROP      = 1 shl 8;  // this is a backdrop window
  WFLG_REPORTMOUSE   = 1 shl 9;  // to hear about every mouse move
  WFLG_GIMMEZEROZERO = 1 shl 10; // a GimmeZeroZero window
  WFLG_BORDERLESS    = 1 shl 11; // to get a Window sans border
  WFLG_ACTIVATE      = 1 shl 12; // when Window opens, it's Active
  // Private
  WFLG_WINDOWACTIVE  = 1 shl 13; // this window is the active one
  WFLG_INREQUEST     = 1 shl 14; // this window is in request mode
  WFLG_MENUSTATE     = 1 shl 15; // Window is active with Menus on
  // Other User Flags
  WFLG_RMBTRAP       = 1 shl 16; // Catch RMB events for your own
  WFLG_NOCAREREFRESH = 1 shl 17; // not to be bothered with REFRESH
  WFLG_NW_EXTENDED   = 1 shl 18; // extension data provided see struct ExtNewWindow
  
  WFLG_NEWLOOKMENUS  = 1 shl 21; // window has NewLook menus

  // These flags are set only by Intuition.  YOU MAY NOT SET THEM YOURSELF!
  WFLG_WINDOWREFRESH = 1 shl 24; // Window is currently refreshing
  WFLG_WBENCHWINDOW  = 1 shl 25; // WorkBench tool ONLY Window
  WFLG_WINDOWTICKED  = 1 shl 26; // only one timer tick at a time
  WFLG_VISITOR       = 1 shl 27; // visitor window
  WFLG_ZOOMED        = 1 shl 28; // identifies "zoom state"
  WFLG_HASZOOM       = 1 shl 29; // windowhas a zoom gadget
  WFLG_TOOLBOX       = 1 shl 30; 
  // Other Window Values
  DEFAULTMOUSEQUEUE = 5; // no more mouse messages
  // see struct IntuiMessage for the IDCMP Flag definitions
  
  // HelpControl() flags: HC_GADGETHELP - Set this flag to enable Gadget-Help for one or more windows.
  HC_GADGETHELP = 1;

const
// Flags for TRequester.Flags
  // set by Application
  POINTREL      = 1 shl 0; // if POINTREL set, TopLeft is relative to pointer to the coordinates of either the pointer or the window
  PREDRAWN      = 1 shl 1; // If set, ImageBMap points to a custom bitmap
  NOISYREQ      = 1 shl 2; // Requester doesn't filter input
  SIMPLEREQ     = 1 shl 4; // If set, a SIMPLEREFRESH layer is used
  USEREQIMAGE   = 1 shl 5; // render linked list ReqImage after BackFill but before gadgets and text
  NOREQBACKFILL = 1 shl 6; // don't bother filling requester with Requester.BackFill pen
  //  Read only Flags set by Intuition
  REQOFFWINDOW = 1 shl 12; // part of one of the Gadgets was offwindow
  REQACTIVE    = 1 shl 13; // this requester is active
  SYSREQUEST   = 1 shl 14; // this requester caused by system
  DEFERREFRESH = 1 shl 15; // this Requester stops a Refresh broadcast


{ Intuition supports the IESUBCLASS_NEWTABLET subclass of the IECLASS_NEWPOINTERPOS event.  The ie_EventAddress of such an event points to a TabletData structure (see below).
  The TabletData structure contains certain elements including a taglist.The taglist can be used for special tablet parameters.  A tablet driver
  should include only those tag-items the tablet supports.  An application can listen for any tag-items that interest it.  Note: an application
  must set the WA_TabletMessages attribute to TRUE to receive this extended information in its IntuiMessages.
  The definitions given here MUST be followed.  Pay careful attention
  to normalization and the interpretation of signs.
      Note: a stylus that supports tilt should use the TABLETA_AngleX
      and TABLETA_AngleY attributes.  Tilting the stylus so the tip
      points towards increasing or decreasing X is actually a rotation
      around the Y-axis.  Thus, if the stylus tip points towards
      positive X, then that tilt is represented as a negative
      TABLETA_AngleY.  Likewise, if the stylus tip points towards
      positive Y, that tilt is represented by positive TABLETA_AngleX.}
const
  TABLETA_Dummy          = TAG_USER + $3A000;
  TABLETA_TabletZ        = TABLETA_Dummy + $01; // the current value of the tablet in the Z direction. This unsigned value should typically be in the natural units of the
                                                //  tablet. You should also provide TABLETA_RangeZ.
  TABLETA_RangeZ         = TABLETA_Dummy + $02; // the maximum value of the tablet in the Z direction. Normally specified along with TABLETA_TabletZ, this allows the
                                                //  application to scale the actual Z value across its range.
  TABLETA_AngleX         = TABLETA_Dummy + $03; // the angle of rotation or tilt about the X-axis. This number should be normalized to fill a signed long LongInt.  Positive
                                                // values imply a clockwise rotation about the X-axis when viewing from +X towards the origin.
  TABLETA_AngleY         = TABLETA_Dummy + $04; // the angle of rotation or tilt about the Y-axis. This number should be normalized to fill a signed long LongInt.  Positive
                                                // values imply a clockwise rotation about the Y-axis when viewing from +Y towards the origin.
  TABLETA_AngleZ         = TABLETA_Dummy + $05; // the angle of rotation or tilt about the Z axis. This number should be normalized to fill a signed long LongInt.  Positive
                                                // values imply a clockwise rotation about the Z-axis when viewing from +Z towards the origin.
  TABLETA_Pressure       = TABLETA_Dummy + $06; {  the pressure reading of the stylus.  The pressure should be normalized to fill a signed long LongInt.  Typical devices
                                                   won't generate negative pressure, but the possibility is not precluded. The pressure threshold which is considered to cause a button-click is
                                                   expected to be set in a Preferences program supplied by the tablet vendor.  The tablet driver would send IECODE_LBUTTON-type events as
                                                   the pressure crossed that threshold.}
  TABLETA_ButtonBits     = TABLETA_Dummy + $07; // ti_Data is a long LongInt whose bits are to be interpreted at the state of the first 32 buttons of the tablet.
  TABLETA_InProximity    = TABLETA_Dummy + $08; { ti_Data is a boolean.  For tablets that support proximity, they should send the (TABLETA_InProximity,FALSE) tag item
                                                  when the stylus is out of proximity.  One possible use we can forsee is a mouse-blanking commodity which keys off this to blank the
                                                  mouse.  When this tag is absent, the stylus is assumed to be in proximity.}
  TABLETA_ResolutionX    = TABLETA_Dummy + $09; // ti_Data is an unsigned long LongInt which is the x-axis resolution in dots per inch.
  TABLETA_ResolutionY    = TABLETA_Dummy + $0A; // ti_Data is an unsigned long LongInt which is the y-axis resolution in dots per inch.

type
  // this structure is used for remembering what memory has been allocated to date by a given routine,
  // so that a premature abort or systematic exit can deallocate memory cleanly, easily, and completely
  PRemember = ^TRemember;
  TRemember = record
    NextRemember: PRemember;
    RememberSize: LongWord;
    Memory: PByte;
  end;

// How to tell Intuition about RGB values for a color table entry. }
  PColorSpec = ^TColorSpec;
  TColorSpec = record
    ColorIndex: SmallInt;   // -1 terminates an array of ColorSpec
    Red: Word;              // only the _bottom_ 4 bits recognized
    Green: Word;            // only the _bottom_ 4 bits recognized
    Blue: Word;             // only the _bottom_ 4 bits recognized
  end;

// Easy Requester Specification see also autodocs for EasyRequest and BuildEasyRequest NOTE: This structure may grow in size in the future
  PEasyStruct = ^TEasyStruct;
  TEasyStruct = record
    es_StructSize: LongWord; // should be sizeof (TEasyStruct) Note that this size may change, if you update the includes! Do not use absolute values as the size of pointers may vary on different platforms!
    es_Flags: LongWord;      // should be 0 for now
    es_Title: PChar;         // title of requester window
    es_TextFormat: PChar;    // 'printf' style formatting string
    es_GadgetFormat: PChar;  // Text of the gadgets, separated by |'s 
  end;

const
// These are the AlertNumber defines.  if you are calling DisplayAlert() the AlertNumber you supply must have the ALERT_TYPE bits set to one of these patterns
  ALERT_TYPE     = $80000000;
  RECOVERY_ALERT = $00000000;   // the system can recover from this
  DEADEND_ALERT  = $80000000;   // no recovery possible, this is it
  
{ When you're defining IntuiText for the Positive and Negative Gadgets created by a call to AutoRequest(), these defines will get you
  reasonable-looking text.  The only field without a define is the IText field; you decide what text goes with the Gadget}
  AUTOFRONTPEN  = 0;
  AUTOBACKPEN   = 1;
  AUTODRAWMODE  = JAM2;
  AUTOLEFTEDGE  = 6;
  AUTOTOPEDGE   = 3;
  AUTOITEXTFONT = nil;
  AUTONEXTTEXT  = nil;

// RAWMOUSE Codes and Qualifiers (Console OR <IDCMP)
  SELECTDOWN = IECODE_LBUTTON;
  SELECTUP   = IECODE_LBUTTON or IECODE_UP_PREFIX;
  MENUDOWN   = IECODE_RBUTTON;
  MENUUP     = IECODE_RBUTTON or IECODE_UP_PREFIX;
  MIDDLEDOWN = IECODE_MBUTTON;
  MIDDLEUP   = IECODE_MBUTTON or IECODE_UP_PREFIX;
  ALTLEFT    = IEQUALIFIER_LALT;
  ALTRIGHT   = IEQUALIFIER_RALT;
  AMIGALEFT  = IEQUALIFIER_LCOMMAND;
  AMIGARIGHT = IEQUALIFIER_RCOMMAND;
  AMIGAKEYS  = AMIGALEFT or AMIGARIGHT;

  CURSORUP        = $4C;
  CURSORDOWN      = $4D;
  CURSORRIGHT     = $4E;
  CURSORLEFT      = $4F;
  
  KEYCODE_Q       = $10;
  KEYCODE_Z       = $31;
  KEYCODE_X       = $32;
  KEYCODE_V       = $34;
  KEYCODE_B       = $35;
  KEYCODE_N       = $36;
  KEYCODE_M       = $37;
  KEYCODE_LESS    = $38;
  KEYCODE_GREATER = $39;

const
// these are the display modes for which we have corresponding parameter settings in the config arrays
  HIRESPICK  = $0000;
  LOWRESPICK = $0001;
  DMODECOUNT = $0002;  // how many modes there are
  // these are the system Gadget defines
  HIRESGADGET  = 0;
  LOWRESGADGET = 1;
  RESCOUNT     = 2;  

  UPFRONTGADGET   = 0;
  DOWNBACKGADGET  = 1;
  SIZEGADGET      = 2;
  CLOSEGADGET     = 3;
  DRAGGADGET      = 4;
  SUPFRONTGADGET  = 5;
  SDOWNBACKGADGET = 6;
  SDRAGGADGET     = 7;
  GADGETCOUNT     = 8;
  
  EVENTMAX = 10;  // size of event array

type
  // This is a packet of information for graphics rendering.  It originates with a Screen, and is gotten using GetScreenDrawInfo( screen );
  PDrawInfo = ^TDrawInfo;
  TDrawInfo = record
    dri_Version: Word;   // will be  DRI_VERSION
    dri_NumPens: Word;   // guaranteed to be >= numDrIPens
    dri_Pens: PWord;     // pointer to pen array
    dri_Font: PTextFont; // screen default font
    dri_Depth: Word;     // (initial) depth of screen bitmap

    dri_Resolution: record      
      x: Word;           // from DisplayInfo database for initial display mode
      y: Word;
    end;

    dri_Flags: LongWord; // defined below (DIRF_*)
    
    dri_CheckMark: PImage; // pointer to scaled checkmark image Will be nil if DRI_VERSION < 2
    dri_AmigaKey: PImage;  // pointer to scaled Amiga-key image Will be NULL if DRI_VERSION < 2

    dri_Reserved: array[0..4] of LongWord; // avoid recompilation ;^)
  end;

const
  
  // If you find dri_Version >= DRI_VERSION, you know this structure has at least the fields defined in this version of the include file
  DRI_VERSION = 2;
  // dri_Flags
  DRIF_NEWLOOK     = 1 shl 0; // specified SA_Pens, full treatment
  DRIF_DIRECTCOLOR = 1 shl 1;
  // rendering pen number indexes into DrawInfo.dri_Pens[]
  DETAILPEN        = 0;  // compatible Intuition rendering pens
  BLOCKPEN         = 1;  // compatible Intuition rendering pens
  TEXTPEN          = 2;  // text on background
  SHINEPEN         = 3;  // bright edge on 3D objects
  SHADOWPEN        = 4;  // dark edge on 3D objects
  FILLPEN          = 5;  // active-window/selected-gadget fill
  FILLTEXTPEN      = 6;  // text over FILLPEN
  BACKGROUNDPEN    = 7;  // always color 0
  HIGHLIGHTTEXTPEN = 8;  // special color text, on background
  // Only present if DRI_VERSION >= 2
  BARDETAILPEN     = 9;  // text/detail in screen-bar/menus
  BARBLOCKPEN      = 10; // screen-bar/menus fill
  BARTRIMPEN       = 11; // trim under screen-bar
  NUMDRIPENS       = 12;

//It is sometimes useful to specify that a pen value is to be the complement of color zero to three.  The "magic" numbers serve that purpose:
  PEN_C3 = $FEFC; // Complement of color 3
  PEN_C2 = $FEFD; // Complement of color 2
  PEN_C1 = $FEFE; // Complement of color 1
  PEN_C0 = $FEFF; // Complement of color 0
  
// values for ChangeDecoration ID param
  DECORATION_SET     = $8001;
  DECORATION_DEFAULT = $8000;
  
// OpenScreen error codes, which are returned in the (optional) LongIng pointed to by ti_Data for the SA_ErrorCode tag item
  OSERR_NOMONITOR    = 1; // named monitor spec not available
  OSERR_NOCHIPS      = 2; // you need newer custom chips
  OSERR_NOMEM        = 3; // couldn't get normal memory
  OSERR_NOCHIPMEM    = 4; // couldn't get chipmem
  OSERR_PUBNOTUNIQUE = 5; // public screen name already used
  OSERR_UNKNOWNMODE  = 6; // don't recognize mode asked for
  OSERR_TOODEEP      = 7;
  OSERR_ATTACHFAIL   = 8;
  OSERR_NOTAVAILABLE = 9;
  
// The SCREENTYPE bits are reserved for describing various Screen types available under Intuition.
  // The screen flags have the suffix "_f" added to avoid conflicts with routine names.
  // Screen^.Flags and (Ext)NewScreen^.Type
  WBENCHSCREEN_f = 1 shl 0;   // Ta Da!  The Workbench
  PUBLICSCREEN_f = 1 shl 1;
  CUSTOMSCREEN_f = $000F;     // for that special look
  SCREENTYPE_f   = $000F;     // all the screens types available
  // Screen^.Flags 
  SHOWTITLE_f    = 1 shl 4;   // this gets set by a call to ShowTitle()
  BEEPING_f      = 1 shl 5;   // set when Screen is beeping
  CUSTOMBITMAP_f = 1 shl 6;   // if you are supplying your own BitMap
  SCREENBEHIND_f = 1 shl 7;   // if you want your screen to open behind already open screens
  SCREENQUIET_f  = 1 shl 8;   // if you do not want Intuition to render into your screen (gadgets, title)
  SCREENHIRES    = 1 shl 9;   // do no use lowres gadgets (private)
  PENSHARED      = 1 shl 10;  // Screen opener set (SA_SharePens,True)
  NS_EXTENDED    = 1 shl 12;  // ExtNewScreen.Extension is valid
  AUTOSCROLL     = 1 shl 14;  // screen is to autoscoll

  STDSCREENHEIGHT = -1; // supply in NewScreen.Height
  STDSCREENWIDTH  = -1; // supply in NewScreen.Width

const
  // Screen attribute tag ID's.  These are used in the ti_Tag field of TagItem arrays passed to OpenScreenTagList() (or in the ExtNewScreen.Extension field).
  SA_Dummy       = TAG_USER + 32;
  // these items specify items equivalent to fields in NewScreen
  SA_Left        = SA_Dummy + 1;
  SA_Top         = SA_Dummy + 2;
  SA_Width       = SA_Dummy + 3;
  SA_Height      = SA_Dummy + 4;  // traditional screen positions and dimensions
  SA_Depth       = SA_Dummy + 5;  // screen bitmap depth
  SA_DetailPen   = SA_Dummy + 6;  //serves as default for windows, too
  SA_BlockPen    = SA_Dummy + 7;
  SA_Title       = SA_Dummy + 8;  // default screen title
  SA_Colors      = SA_Dummy + 9;  // ti_Data is an array of struct ColorSpec, terminated by ColorIndex = -1.  Specifies initial screen palette colors.
  SA_ErrorCode   = SA_Dummy + 10; // ti_Data points to LONG error code (values below)
  SA_Font        = SA_Dummy + 11; // equiv. to NewScreen.Font
  SA_SysFont     = SA_Dummy + 12; // Selects one of the preferences system fonts: 0 - old DefaultFont, fixed-width 1 - WB Screen preferred font
  SA_Type        = SA_Dummy + 13; // equiv. to NewScreen.type
  SA_BitMap      = SA_Dummy + 14; // ti_Data is pointer to custom BitMap.  This implies type of CUSTOMBITMAP
  SA_PubName     = SA_Dummy + 15; // presence of this tag means that the screen is to be a public screen. Please specify BEFORE the two tags below
  SA_PubSig      = SA_Dummy + 16;
  SA_PubTask     = SA_Dummy + 17; // Task ID and signal for being notified that the last window has closed on a public screen.
  SA_DisplayID   = SA_Dummy + 18; // ti_Data is new extended display ID 
  SA_DClip       = SA_Dummy + 19; // ti_Data points to a rectangle which defines screen display clip region
  SA_Overscan    = SA_Dummy + 20; { was S_STDDCLIP.  Set to one of the OSCAN_* specifiers below to get a system standard
                                       overscan region for your display clip, screen dimensions (unless otherwise specified),
                                       and automatically centered position (partial support only so far).
                                       If you use this, you shouldn't specify SA_DClip.  SA_Overscan is for "standard"
                                       overscan dimensions, SA_DClip is for your custom numeric specifications.}
  SA_ShowTitle     = SA_Dummy + 22; // boolean equivalent to flag SHOWTITLE
  SA_Behind        = SA_Dummy + 23; // boolean equivalent to flag SCREENBEHIND
  SA_Quiet         = SA_Dummy + 24; // boolean equivalent to flag SCREENQUIET
  SA_AutoScroll    = SA_Dummy + 25; // boolean equivalent to flag AUTOSCROLL
  SA_Pens          = SA_Dummy + 26; // pointer to ~0 terminated UWORD array, as found in struct DrawInfo
  SA_FullPalette   = SA_Dummy + 27; // boolean: initialize color table to entire preferences palette, rather than compatible pens 0-3, 17-19, with remaining palette as returned by GetColorMap()
  SA_ColorMapEntries = SA_Dummy + 28; {Allows you to override the number of entries in the ColorMap for your screen.  Intuition
                                            normally allocates (1 shl depth) or 32, whichever is more, but you may require even more if you
                                            use certain graphics.library features (eg. palette-banking).}
  SA_Parent        = SA_Dummy + 29; // Pointer to a "parent" screen to attach this one to.  Attached screens slide and depth-arrange together.
  SA_Draggable     = SA_Dummy + 30; // Boolean tag allowing non-draggable screens. Do not use without good reason! (Defaults to True).
  SA_Exclusive     = SA_Dummy + 31; { Boolean tag allowing screens that won't share the display.  Use sparingly!  Starting with 3.01,
                                         attached screens may be SA_Exclusive.  Setting SA_Exclusive for each screen will produce an
                                          exclusive family.   (Defaults to FALSE).}
  SA_SharePens     = SA_Dummy + 32; { For those pens in the screen's DrawInfo^.dri_Pens, Intuition obtains them in shared mode (see
                                         graphics.library/ObtainPen()).  For compatibility, Intuition obtains the other pens of a public
                                         screen as PEN_EXCLUSIVE.  Screens that wish to manage the pens themselves should generally set
                                         this tag to True.  This instructs Intuition to leave the other pens unallocated.}
  SA_BackFill      = SA_Dummy + 33; // provides a "backfill hook" for your screen's Layer_Info. See layers.library/InstallLayerInfoHook()
  SA_Interleaved   = SA_Dummy + 34; // Boolean tag requesting that the bitmap allocated for you be interleaved. (Defaults to False).
  SA_Colors32      = SA_Dummy + 35; { Tag to set the screen's initial palette colors at 32 bits-per-gun.  ti_Data is a pointer
                                         to a table to be passed to the graphics.library/LoadRGB32() function. This format supports both runs of color
                                         registers and sparse registers.  See the autodoc for that function for full details. Any color set here has
                                         precedence over the same register set by SA_Colors.}
  SA_VideoControl  = SA_Dummy + 36; // Pointer to a taglist that Intuition will pass to graphics.library/VideoControl(), upon opening the screen.
  SA_FrontChild    = SA_Dummy + 37; // Pointer to an already open screen that is to be the child of the screen being opened. The child screen will be moved to the front of its family.
  SA_BackChild     = SA_Dummy + 38; // Pointer to an already open screen that is to be the child of the screen being opened. The child screen will be moved to the back of its family.
  SA_LikeWorkbench = SA_Dummy + 39; // 1  = request a screen which is just like the Workbench.  This gives you the same screen mode, depth, size, colors, etc., as the Workbench screen.
  SA_Reserved      = SA_Dummy + 40; // Reserved for private Intuition use

  SA_MinimizeISG   = SA_Dummy + 41; { For compatibility, Intuition always ensures that the inter-screen gap is at least three non-interlaced lines.  If your application
                                           would look best with the smallest possible inter-screen gap, set ti_Data to True. If you use the new graphics VideoControl()
                                           VC_NoColorPaletteLoad tag for your screen's ViewPort, you should also set this tag.}
  SA_ID            = SA_Dummy + 42;
// this is an obsolete tag included only for compatibility with V35 interim release for the A2024 and Viking monitors
  NSTAG_EXT_VPMODE = TAG_USER + 1;

  // SA_Overscan
  OSCAN_TEXT     = 1; // entirely visible
  OSCAN_STANDARD = 2; // just past edges
  OSCAN_MAX      = 3; // as much as possible
  OSCAN_VIDEO    = 4; // even more than is possible

type
  PNewScreen = ^TNewScreen;
  TNewScreen = record
    LeftEdge,
    TopEdge,
    Width,
    Height,
    Depth: SmallInt;        // screen dimensions

    DetailPen,
    BlockPen: Byte;         // for bar/border/gadget rendering

    ViewModes: Word;        // the Modes for the ViewPort (and View)
    SType: Word;            // the Screen type (see defines above) (Type in C-Include)
    
    Font: PTextAttr;        // this Screen's default text attributes
    DefaultTitle: PChar;    // the default title for this Screen
    Gadgets: PGadget;       // your own Gadgets for this Screen

  { if you are opening a CUSTOMSCREEN and already have a BitMap
    that you want used for your Screen, you set the flags CUSTOMBITMAP in
    the type field and you set this variable to point to your BitMap
    structure.  The structure will be copied into your Screen structure,
    after which you may discard your own BitMap if you want}
    CustomBitMap: PBitMap;
  end;

  PExtNewScreen = ^TExtNewScreen;
  TExtNewScreen = record
    LeftEdge,
    TopEdge,
    Width,
    Height,
    Depth: SmallInt;
    DetailPen,
    BlockPen: Byte;
    ViewModes: Word;
    ens_Type: Word;     { type in C-Includes }
    Font: PTextAttr;
    DefaultTitle: PChar;
    Gadgets: PGadget;
    CustomBitMap: PBitMap;
    Extension: PTagItem; // ExtNewScreen specific extension SA_*
  end;
  
// Public Shared Screen Node
{ This is the representative of a public shared screen.
  This is an internal data structure, but some functions may
  present a copy of it to the calling application.  In that case,
  be aware that the screen pointer of the structure can NOT be
  used safely, since there is no guarantee that the referenced
  screen will remain open and a valid data structure.
  Never change one of these.}
  PPubScreenNode = ^TPubScreenNode;
  TPubScreenNode = record
    psn_Node: TNode;            // ln_Name is screen name
    psn_Screen: PScreen;
    psn_Flags: Word;            // below (PSNF_*)
    psn_Size: SmallInt;         // includes name buffer
    psn_VisitorCount: SmallInt; // how many visitor windows
    psn_SigTask: PTask;         // who to signal when visitors gone
    psn_SigBit: Byte;           // which signal
  end;

const
  // psn_Flags
  PSNF_PRIVATE = 1 shl 0;
  // Maximum length of public screen names. The buffers containing these strings must have a length of MAXPUBSCREENNAME+1.
  MAXPUBSCREENNAME = 139;   // names no longer, please
  // pub screen modes
  SHANGHAI     = 1 shl 0; // put workbench windows on pub screen
  POPPUBSCREEN = 1 shl 1; // pop pub screen to front when visitor opens

{Intuition has new screen depth-arrangement and movement
  functions called ScreenDepth() and ScreenPosition() respectively.
  These functions permit the old behavior of ScreenToFront(),
  ScreenToBack(), and MoveScreen().  ScreenDepth() also allows
  independent depth control of attached screens.  ScreenPosition()
  optionally allows positioning screens even though they were opened
  (SA_Draggable, False).
 For ScreenDepth(), specify one of SDEPTH_TOFRONT or SDEPTH_TOBACK,
  and optionally also SDEPTH_INFAMILY.
  NOTE: ONLY THE OWNER OF THE SCREEN should ever specify
  SDEPTH_INFAMILY.  Commodities, "input helper" programs,
  or any other program that did not open a screen should never
  use that flag.  (Note that this is a style-behavior
  requirement;  there is no technical requirement that the
  task calling this function need be the task which opened
  the screen).}
  SDEPTH_TOFRONT  = 0; // Bring screen to front
  SDEPTH_TOBACK   = 1; // Send screen to back
  SDEPTH_INFAMILY = 2; // Move an attached screen with respect to other screens of its family


{ For ScreenPosition(), specify one of SPOS_RELATIVE, SPOS_ABSOLUTE, or SPOS_MAKEVISIBLE to describe the kind of screen positioning you
  wish to perform. You may additionally set SPOS_FORCEDRAG along with any of the above.  Set this if you wish to reposition an (SA_Draggable,False)
  screen that you opened.
  NOTE: ONLY THE OWNER OF THE SCREEN should ever specify SPOS_FORCEDRAG.  Commodities, "input helper" programs, or any other program that did not
  open a screen should never use that flag.
 }

  SPOS_RELATIVE    = 0;       // The x1 and y1 parameters to ScreenPosition() describe the offset in coordinates you wish to move the screen by. Coordinates are relative 
  SPOS_ABSOLUTE    = 1 shl 0; // The x1 and y1 parameters to ScreenPosition() describe the absolute coordinates you wish to move the screen to. Coordinates are expressed as absolutes, not relatives.
  SPOS_MAKEVISIBLE = 1 shl 1; // Coordinates describe a box on the screen you wish to be made visible by autoscrolling
                              // (x1,y1)-(x2,y2) describes a rectangle on the screen which you would like autoscrolled into view.
  SPOS_FORCEDRAG   = 1 shl 2; // Move non-draggable screen, You may additionally set SPOS_FORCEDRAG along with any of the above.
                              // Set this if you wish to reposition an (SA_Draggable, False) screen that you opened.

{ Intuition supports double-buffering in screens, with friendly interaction with menus and certain gadgets.
  For each buffer, you need to get one of these structures from the AllocScreenBuffer() call.  Never allocate your own ScreenBuffer structures!
  The sb_DBufInfo field is for your use.  See the graphics.library AllocDBufInfo() autodoc for details.}
type
  PScreenBuffer = ^TScreenBuffer;
  TScreenBuffer = record
    sb_BitMap: PBitMap;      // BitMap of this buffer
    sb_DBufInfo: PDBufInfo;  // DBufInfo for this buffer
  end;

const
// These are the flags that may be passed to AllocScreenBuffer().
  SB_SCREEN_BITMAP = 1;
  SB_COPY_BITMAP   = 2;

const

  // these are the definitions for the printer configurations
  FILENAME_SIZE = 30; // Filename size
  DEVNAME_SIZE  = 16;
  POINTERSIZE = (1 + 16 + 1) * 2; // Size of Pointer data buffer
  
{ These defines are for the default font size.   These actually describe the
  height of the defaults fonts.  The default font type is the topaz
  font, which is a fixed width font that can be used in either
  eighty-column or sixty-column mode.  The Preferences structure reflects
  which is currently selected by the value found in the variable FontSize,
  which may have either of the values defined below.  These values actually
  are used to select the height of the default font.  By changing the
  height, the resolution of the font changes as well.}
  TOPAZ_EIGHTY = 8;
  TOPAZ_SIXTY = 9;
type
  PPreferences = ^TPreferences;
  TPreferences = record
    FontHeight: ShortInt;      // height for system default font  
    PrinterPort: Byte;         // printer port connection constant describing what's hooked up to the port 
    BaudRate: Word;            // baud rate for the serial port
    
  // various timing rates
    KeyRptSpeed: Ttimeval;     // repeat speed for keyboard
    KeyRptDelay: Ttimeval;     // Delay before keys repeat
    DoubleClick: Ttimeval;     // Interval allowed between clicks
    
  // Intuition Mouse-Pointer data  
    PointerMatrix: array[0..POINTERSIZE - 1] of Word; // Definition of pointer sprite
    XOffset: ShortInt;   // X-Offset for active 'bit'
    YOffset: ShortInt;   // Y-Offset for active 'bit'
    color17: Word;       //
    color18: Word;       // Colours for sprite pointer
    color19: Word;       //
    PointerTicks: Word;  // Sensitivity of the pointer

   // Workbench Screen colors
    color0: Word;        //
    color1: Word;        // Standard default colours
    color2: Word;        // Used in the Workbench
    color3: Word;        //

   // positioning data for the Intuition View
    ViewXOffset: ShortInt; // Offset for top lefthand corner
    ViewYOffset: ShortInt; // X and Y dimensions
    ViewInitX,
    ViewInitY: SmallInt;   // View initial offset values

    EnableCLI: LongBool;   // CLI availability switch

  // printer configurations
    PrinterType: Word;     // printer type
    PrinterFilename: array[0..FILENAME_SIZE - 1] of char; // file for printer 

  // print format and quality configurations
    PrintPitch: Word;         // print pitch
    PrintQuality: Word;       // print quality
    PrintSpacing: Word;       // number of lines per inch
    PrintLeftMargin: Word;    // left margin in characters
    PrintRightMargin: Word;   // right margin in characters
    PrintImage: Word;         // positive or negative
    PrintAspect: Word;        // horizontal or vertical
    PrintShade: Word;         // b&w, half-tone, or color
    PrintThreshold: SmallInt; // darkness ctrl for b/w dumps

  // print paper descriptors
    PaperSize: Word;          // paper size
    PaperLength: Word;        // paper length in number of lines
    PaperType: Word;          // continuous or single sheet

  // Serial device settings: These are six nibble-fields in three bytes (these look a little strange so the defaults will map out to zero)
    SerRWBits: Byte;  // upper nibble = (8-number of read bits), lower nibble = (8-number of write bits)
    SerStopBuf: Byte; // upper nibble = (number of stop bits - 1), lower nibble = (table value for BufSize)
    SerParShk: Byte;  // upper nibble = (value for Parity setting), lower nibble = (value for Handshake mode)
    LaceWB: Byte;     // if workbench is to be interlaced 

  // temp file for printer
    Pad: array[0..11] of Byte;
    PrtDevName: array [0..DEVNAME_SIZE - 1] of char; // Device used by printer.device (leave out the ".device")
  
    DefaultPrtUnit: Byte; // Default unit opened by printer.device
    DefaultSerUnit: Byte; // Default serial unit

    RowSizeChange: ShortInt;    // Affect NormalDisplayRows/Columns
    ColumnSizeChange: ShortInt;

    PrintFlags: Word;     // user preference flags
    PrintMaxWidth: Word;  // max width of printed picture in 10ths/inch
    PrintMaxHeight: Word; // max height of printed picture in 10ths/inch
    PrintDensity: Byte;   // print density
    PrintXOffset: Byte;   // offset of printed picture in 10ths/inch

    wb_Width: Word;       // override default workbench width
    wb_Height: Word;      // override default workbench height
    wb_Depth: Byte;       // override default workbench depth

    ext_size: Byte;       // extension information -- do not touch!
    // extension size in blocks of 64 bytes DO NOT TOUCH
  end;

const
  // Workbench Interlace (use one bit)
  LACEWB      = 1 shl 0;
  LW_RESERVED = 1;        // internal use only
  SCREEN_DRAG = 1 shl 14;
  MOUSE_ACCEL = 1 shl 15;
  // PrinterPort
  PARALLEL_PRINTER = $00;
  SERIAL_PRINTER   = $01;
  // BaudRate
  BAUD_110   = $00;
  BAUD_300   = $01;
  BAUD_1200  = $02;
  BAUD_2400  = $03;
  BAUD_4800  = $04;
  BAUD_9600  = $05;
  BAUD_19200 = $06;
  BAUD_MIDI  = $07;
  // PaperType
  ptFANFOLD = $00;
  ptSingle  = $80;
  // PrintPitch
  PICA  = $000;
  ELITE = $400;
  FINE  = $800;
  // PrintQuality
  DRAFT  = $000;
  LETTER = $100;
  // PrintSpacing
  SIX_LPI   = $000;
  EIGHT_LPI = $200;
  // Print Image
  IMAGE_POSITIVE = $00;
  IMAGE_NEGATIVE = $01;
  // PrintAspect
  ASPECT_HORIZ = $00;
  ASPECT_VERT  = $01;
  // PrintShade
  SHADE_BW        = $00;
  SHADE_GREYSCALE = $01;
  SHADE_COLOR     = $02;
  // PaperSize
  US_LETTER    = $00;
  US_LEGAL     = $10;
  N_TRACTOR    = $20;
  W_TRACTOR    = $30;
  CUSTOM_PAPER = $40;
  // European sizes
  EURO_A0 = $50; // European size A0: 841 x 1189
  EURO_A1 = $60; // European size A1: 594 x 841
  EURO_A2 = $70; // European size A2: 420 x 594
  EURO_A3 = $80; // European size A3: 297 x 420
  EURO_A4 = $90; // European size A4: 210 x 297
  EURO_A5 = $A0; // European size A5: 148 x 210
  EURO_A6 = $B0; // European size A6: 105 x 148
  EURO_A7 = $C0; // European size A7: 74 x 105
  EURO_A8 = $D0; // European size A8: 52 x 74
  // PrinterType
  CUSTOM_NAME      = $00;
  ALPHA_P_101      = $01;
  BROTHER_15XL     = $02;
  CBM_MPS1000      = $03;
  DIAB_630         = $04;
  DIAB_ADV_D25     = $05;
  DIAB_C_150       = $06;
  EPSON            = $07;
  EPSON_JX_80      = $08;
  OKIMATE_20       = $09;
  QUME_LP_20       = $0A;
  HP_LASERJET      = $0B;
  HP_LASERJET_PLUS = $0C;
  // Serial Input Buffer Sizes
  SBUF_512   = $00;
  SBUF_1024  = $01;
  SBUF_2048  = $02;
  SBUF_4096  = $03;
  SBUF_8000  = $04;
  SBUF_16000 = $05;
  // Serial Bit Masks
  SREAD_BITS  = $F0; // for SerRWBits
  SWRITE_BITS = $0F;

  SSTOP_BITS    = $F0; // for SerStopBuf
  SBUFSIZE_BITS = $0F;

  SPARITY_BITS = $F0; // for SerParShk
  SHSHAKE_BITS = $0F;
  // Serial Parity (upper nibble, after being shifted by macro SPARNUM())
  SPARITY_NONE  = 0;
  SPARITY_EVEN  = 1;
  SPARITY_ODD   = 2;
  SPARITY_MARK  = 3;
  SPARITY_SPACE = 4;
  // Serial Handshake Mode (lower nibble, after masking using macro SHANKNUM())
  SHSHAKE_XON  = 0;
  SHSHAKE_RTS  = 1;
  SHSHAKE_NONE = 2;
  // new defines for PrintFlags
  CORRECT_RED   = $0001; // color correct red shades
  CORRECT_GREEN = $0002; // color correct green shades
  CORRECT_BLUE  = $0004; // color correct blue shades

  CENTER_IMAGE = $0008; // center image on paper

  IGNORE_DIMENSIONS   = $0000; // ignore max width/height settings
  BOUNDED_DIMENSIONS  = $0010; // use max width/height as boundaries
  ABSOLUTE_DIMENSIONS = $0020; // use max width/height as absolutes
  PIXEL_DIMENSIONS    = $0040; // use max width/height as prt pixels
  MULTIPLY_DIMENSIONS = $0080; // use max width/height as multipliers

  INTEGER_SCALING = $0100; // force integer scaling

  ORDERED_DITHERING  = $0000; // ordered dithering
  HALFTONE_DITHERING = $0200; // halftone dithering
  FLOYD_DITHERING    = $0400; // Floyd-Steinberg dithering

  ANTI_ALIAS  = $0800; // anti-alias image
  GREY_SCALE2 = $1000; // for use with hi-res monitor

  // masks used for checking bits
  CORRECT_RGB_MASK = CORRECT_RED or CORRECT_GREEN or CORRECT_BLUE;
  DIMENSIONS_MASK = BOUNDED_DIMENSIONS or ABSOLUTE_DIMENSIONS or PIXEL_DIMENSIONS or MULTIPLY_DIMENSIONS;
  DITHERING_MASK = HALFTONE_DITHERING or FLOYD_DITHERING;

{ Be sure to protect yourself against someone modifying these data as you look at them.  This is done by calling:
  lock = LockIBase(0), which returns an Integer.  When done call UnlockIBase(lock) where lock is what LockIBase() returned.}
type
  // IntuitionBase should never be directly modified by programs
  PIntuitionBase = ^TIntuitionBase;
  TIntuitionBase = record
    LibNode: TLibrary;

    ViewLord: TView;

    ActiveWindow: PWindow;
    ActiveScreen: PScreen;
    FirstScreen: PScreen; { for linked list of all screens, the FirstScreen variable points to the frontmost Screen.
                            Screens are then maintained in a front to back order using Screen.NextScreen  }
    Flags: LongWord;      // see definitions below
{$ifdef AROS_FLAVOUR_BINCOMPAT}
    MouseX,
    MouseY: SmallInt;      // mouse position relative to View
{$else}
    MouseY,
    MouseX: SmallInt;      // mouse position relative to View
{$endif}

    Seconds: LongWord;     // timestamp of most current input event
    Micros: LongWord;      // timestamp of most current input event

    // I told you this was private. The data beyond this point has changed, is changing, and will continue to change.
  end;

{ Package of information passed to custom and 'boopsi'
  gadget 'hook' functions.  This structure is READ ONLY.}
type
  PGadgetInfo = ^TGadgetInfo;

  TGadgetInfo = record
    gi_Screen: PScreen;       // ScreenPtr
    gi_Window: PWindow;       // nil for screen gadgets
    gi_Requester: PRequester; // nil IF not GTYP_REQGADGET
    gi_RastPort: PRastPort;   // rendering information: don't use these without cloning/locking. Official way is to call ObtainRPort()
    gi_Layer: PLayer;         // LayerPtr
    { copy of dimensions of screen/window/g00/req(/group)
      that gadget resides in.  Left/Top of this box is
      offset from window mouse coordinates to gadget coordinates
               screen gadgets:                 0,0 (from screen coords)
       window gadgets (no g00):                0,0
       GTYP_GZZGADGETs (borderlayer):          0,0
       GZZ innerlayer gadget:          borderleft, bordertop
       Requester gadgets:              reqleft, reqtop}
    gi_Domain: TIBox;
    gi_Pens: record
      DetailPen: Byte;
      BlockPen: Byte;
    end;
    gi_DrInfo: PDrawInfo;                 // the Detail and Block pens in gi_DrInfo^.dri_Pens[] are for the screen.  Use the above for window-sensitive colors.
    gi_Reserved: array[0..5] of LongWord; // reserved space: this structure is extensible anyway, but using these saves some recompilation
  end;

{ system private data structure for now
   prop gadget extra info}
  PPGX = ^TPGX;
  TPGX = record
    pgx_Container: TIBox;
    pgx_NewKnob: TIBox;
  end;

{** User visible handles on objects, classes, messages **}
type
  Object_ = LongWord;
  PObject_ = ^Object_;
  PPObject_ = ^PObject_; 
  ClassID = STRPTR;

{ you can use this type to point to a 'generic' message,
  in the object-oriented programming parlance.  Based on
  the value of 'MethodID', you dispatch to processing
  for the various message types.  The meaningful parameter
  packet structure definitions are defined below.}
  PMsg = ^TMsg;
  TMsg = record
    MethodID: LongWord;
  end;

{ Class id strings for Intuition classes.
  There's no real reason to use the uppercase constants
  over the lowercase strings, but this makes a good place
  to list the names of the built-in classes.}
const
  ROOTCLASS: ClassID     = 'rootclass';          // classusr.h   
  IMAGECLASS: ClassID    = 'imageclass';        // imageclass.h 
  FRAMEICLASS: ClassID   = 'frameiclass';
  SYSICLASS: ClassID     = 'sysiclass';
  FILLRECTCLASS: ClassID = 'fillrectclass';
  GADGETCLASS: ClassID   = 'gadgetclass';      // gadgetclass.h
  PROPGCLASS: ClassID    = 'propgclass';
  STRGCLASS: ClassID     = 'strgclass';
  BUTTONGCLASS: ClassID  = 'buttongclass';
  FRBUTTONCLASS: ClassID = 'frbuttonclass';
  GROUPGCLASS: ClassID   = 'groupgclass';
  ICCLASS: ClassID       = 'icclass';               // icclass.h
  MODELCLASS: ClassID    = 'modelclass';
  ITEXTICLASS: ClassID   = 'itexticlass';
  POINTERCLASS: ClassID  = 'pointerclass';     // pointerclass.h

// public classes existing only in AROS but not AmigaOS
  MENUBARLABELCLASS: PChar = 'menubarlabelclass';
  WINDECORCLASS: PChar     = 'windecorclass';
  SCRDECORCLASS: PChar     = 'scrdecorclass';
  MENUDECORCLASS: PChar    = 'menudecorclass';

{ Dispatched method ID's
  NOTE: Applications should use Intuition entry points, not direct
  DoMethod() calls, for NewObject, DisposeObject, SetAttrs,
  SetGadgetAttrs, and GetAttr}
  OM_Dummy     = $100;
  OM_NEW       = OM_Dummy + 1;  // 'object' parameter is 'true class'
  OM_DISPOSE   = OM_Dummy + 2;  // delete self (no parameters)
  OM_SET       = OM_Dummy + 3;  // set attributes (in tag list)
  OM_GET       = OM_Dummy + 4;  // return single attribute value
  OM_ADDTAIL   = OM_Dummy + 5;  // add self to a List (let root do it)
  OM_REMOVE    = OM_Dummy + 6;  // remove self from list
  OM_NOTIFY    = OM_Dummy + 7;  // send to self: notify dependents
  OM_UPDATE    = OM_Dummy + 8;  // notification message from somebody
  OM_ADDMEMBER = OM_Dummy + 9;  // used by various classes with lists
  OM_REMMEMBER = OM_Dummy + 10; // used by various classes with lists

// Parameter 'Messages' passed to methods
type
  // OM_NEW and OM_SET
  PopSet = ^TopSet;
  TopSet = record
    MethodID: LongWord;
    ops_AttrList: PTagItem; // new attributes
    ops_GInfo: PGadgetInfo; // always there for gadgets, when SetGadgetAttrs() is used, but will be nil for OM_NEW
  end;
  
  // OM_GET
  PopGet = ^TopGet;
  TopGet = record
    MethodID,
    opg_AttrID: LongWord;
    opg_Storage: Pointer;   // may be other types, but 'int' types are all LongWord
  end;
  
  // OM_ADDTAIL

  PopAddTail = ^TopAddTail;
  TopAddTail = record
    MethodID: LongWord;
    opat_List: PList;
  end;

  // OM_ADDMEMBER, OM_REMMEMBER
  PopMember = ^TopMember;
  TopMember = record
    MethodID: LongWord;
    opam_Object: PObject_;
  end;
  TopAddMember = TopMember;
  PopAddMember = ^TopAddMember;
  
  // OM_NOTIFY, and OM_UPDATE
  PopUpdate = ^TopUpdate;
  TopUpdate = record
    MethodID: LongWord;
    opu_AttrList: PTagItem; // new attributes
    opu_GInfo: PGadgetInfo; // non-nil when SetGadgetAttrs OR notification resulting from gadget input occurs.
    opu_Flags: LongWord;    // defined below (OPUF_*)
  end;

{ this flag means that the update message is being issued from
  something like an active gadget, a la GACT_FOLLOWMOUSE.  When
  the gadget goes inactive, it will issue a final update
  message with this bit cleared.  Examples of use are for
  GACT_FOLLOWMOUSE equivalents for propgadclass, and repeat strobes
  for buttons.}
const
  OPUF_INTERIM = 1; // opu_Flags

{ This structure is READ-ONLY, and allocated only by Intuition }
type
  PIClass = ^TIClass;
  TIClass = record          // also used as Class
    cl_Dispatcher: THook;
    cl_Reserved: LongWord;      // must be 0
    cl_Super: PIClass;          // Super - Class
    cl_ID: ClassID;
    cl_InstOffset: Word;        // where within an object is the instance data for this class?
    cl_InstSize: Word;

    cl_UserData: IPTR;          // per-class data of your choice, application specific
    cl_SubclassCount: LongWord; // # of direct suclasses
    cl_ObjectCount: LongWord;   // # of objects, made from this class must be 0, if the class is to be deleted
    cl_Flags: LongWord;         // Flags (CLF_INLIST)
    cl_ObjectSize: LongWord;    // cl_InstOffset + cl_InstSize + SizeOf(_Object)
    cl_MemoryPool: APTR;
  end;

const
  CLF_INLIST = 1 shl 0;     // class is in public class list (cl_Flags)

{ We have this, the instance data of the root class, PRECEDING
  the 'object'.  This is so that Gadget objects are Gadget pointers,
  and so on.  If this structure grows, it will always have o_Class
  at the end, so the macro OCLASS(o) will always have the same
  offset back from the pointer returned from NewObject().}
type
  P_Object = ^T_Object;
  T_Object = record
    o_Node: TMinNode;
    o_Class: PIClass;
  end;

{ BOOPSI class libraries should use this structure as the base for their
  library data.  This allows developers to obtain the class pointer for
  performing object-less inquiries. }
  PClassLibrary = ^TClassLibrary;
  TClassLibrary = record
    cl_Lib: TLibrary;    // Embedded library
    cl_Pad: Word;        // Align the structure
    cl_Class: PIClass;   // Class pointer
  end;

{ GadgetClass Attributes
  Most subclasses of GadgetClass implement a subset of these attributes. Note
  also that even if an attribute is signed as settable, some subclasses may
  ignore this or even behave strange, if such an attribute is set, after they
  were added to a window. Read the class documentation of the subclasses to
  learn about such caveats.

  Many of these attributes correspond directly to a field of the Gadget
  structure or to one flag for this structure. }
const
  GA_Dummy = (TAG_USER + $30000);
// Gadget placing (in pixels). Of course, all GA_Rel attributes are mutually
// exclusive with their non relative equivalents.
  GA_Left      = (GA_Dummy + $0001); // [ISG] (LONG) Left edge of gadget.
  GA_RelRight  = (GA_Dummy + $0002); // [ISG] (LONG) Left edge of gadget, depending on right window border: Left=Win^.Width-this-1
  GA_Top       = (GA_Dummy + $0003); // [ISG] (LONG) Top edge of gadget.
  GA_RelBottom = (GA_Dummy + $0004); // [ISG] (LONG) Top edge of gadget, depending on bottom window border: Top=Win^.Height-this-1
  GA_Width     = (GA_Dummy + $0005); // [ISG] (LONG) Width of gadget.
  GA_RelWidth  = (GA_Dummy + $0006); // [ISG] (LONG) Width of gadget, depending on window width: Width=Win^.Width-this
  GA_Height    = (GA_Dummy + $0007); // [ISG] (LONG) Height of gadget.
  GA_RelHeight = (GA_Dummy + $0008); // [ISG] (LONG) Height of gadget, depending on window height: Height=Win^.Height-this
  
// Gadget rendering
  GA_Text         = (GA_Dummy + $0009); // [IS.] (PChar) Label text. This is mutually exclusive with GA_IntuiText and GA_LabelImage.
  // The next two attributes are mutually exclusive.
  GA_Image        = (GA_Dummy + $000A); // (PImage) Gadget imagry is an image 
  GA_Border       = (GA_Dummy + $000B); // (PBorder) Gadget imagry is a border
  GA_SelectRender = (GA_Dummy + $000C); { [IS.] (PImage) Gadgets' image in selected state. Note that if
     this is nil and GA_Image is in fact an image object, GA_Image may be
     tried to be drawn with IDS_SELECTED. So you do not need to fill this in,
     if you wish to have a special selected image and GA_Image is an image
     object that supports the selected state.}
  GA_Highlight    = (GA_Dummy + $000D); // [IS.] (LongWord) Takes GFLG_GADGH* flags as argument. Used to specify the highlighting technique.
  GA_Disabled     = (GA_Dummy + $000E); // [ISG] (LongBool) If this is set to true, the gadget is not selectable. Often this is visually represented by using a special disabled pattern.
  
// Additional information.
  GA_GZZGadget   = (GA_Dummy + $000F); // [IS.] (LongBool) The Gadget is a GimmeZeroZero gadget. Default = False
  GA_ID          = (GA_Dummy + $0010); // (LongInt) Gadget ID assigned by the application (prevent double numbers)
  GA_UserData    = (GA_Dummy + $0011); // [ISG] (IPTR) Fill with whatever you want to. This field is ignored by the system.
  GA_SpecialInfo = (GA_Dummy + $0012); { [IS.] (APTR) Pointer to additional information, needed by some gadgets
     (like string or integer gadgets). This field should generally only be set
     by subclasses of GadgetClass. Applications should keep their hands off it.}
     
// Gadget activation.
  GA_Selected     = (GA_Dummy + $0013); // [ISG] (LongBool) Indicate whether the gadget is selected or not. Default = False
  GA_EndGadget    = (GA_Dummy + $0014); // [IS.] (LongBool) Only used for requester gadgets. This tells intuition that the requester is to be closed, when the gadget is released. Default = False
  GA_Immediate    = (GA_Dummy + $0015); // [IS.] (LongBool) If set the gadget responds immediatly, when the gadget is selected. Default = False
  GA_RelVerify    = (GA_Dummy + $0016); // [IS.] (LongBool) If set the gadget responds, when it is released from selected state. Default = False
  GA_FollowMouse  = (GA_Dummy + $0017); // [IS.] (LongBool) If this is set, the gadget receives information about the movement of the mouse as long as it is activated. Default = False
  GA_RightBorder  = (GA_Dummy + $0018); // [IS.] (LongBool) Indicate whether the gadget is in the right border or not. Default = False.
  GA_LeftBorder   = (GA_Dummy + $0019); // [IS.] (LongBool) Indicate whether the gadget is in the left border or not. Default = False.
  GA_TopBorder    = (GA_Dummy + $001A); // [IS.] (LongBool) Indicate whether the gadget is in the top border or not. Default = False.
  GA_BottomBorder = (GA_Dummy + $001B); // [IS.] (LongBool) Indicate whether the gadget is in the bottom border or not. Default = False.
  GA_ToggleSelect = (GA_Dummy + $001C); // [IS.] (LongBool) Indicate whether the gadget is toggle-selected or not.  Default = False. 
  // The following two attributes are PRIVATE!
  GA_SysGadget    = (GA_Dummy + $001D); // [IS.] (LongBool) Set, if gadget is a system-gadget e.g. a standard window border gadget. Default = False.
  GA_SysGType     = (GA_Dummy + $001E); // [IS.] (LongWord) Reserved for system use to indicate the gadget type.

// Gadget linking.
  GA_Previous = (GA_Dummy + $001F); { [I..] (PGadget) Pointer to previous gadget. This is used to link
     the current gadget into a gadget list, before this list is used. It can
     not be used to add a gadget to a list of an open window or requester!}
  GA_Next = (GA_Dummy + $0020); // [I..] (PGadget) Next gadget in the linked list. Currently not implemented.
  GA_DrawInfo = (GA_Dummy + $0021); { [I..] (PDrawInfo) Some gadgets need a DrawInfo structure
     to be able to perform correct rendering. Read the documentation of the
     subclasses to learn, which need this attribute. To be on the safe side,
     you can always supply it.}
  // You should use at most ONE of GA_Text, GA_IntuiText, and GA_LabelImage 
  GA_IntuiText = (GA_Dummy + $0022); // [IS.] (PIntuiText) Label is an IntuiText.
  GA_LabelImage = (GA_Dummy + $0023); // [IS.] (PObject_) Label is an image object. 
  GA_TabCycle = (GA_Dummy + $0024);  // [IS.] (LongBool) If set to true that gadget participates in TAB handling, i.e. if tab is pressed, the next gadget is activated.
  GA_GadgetHelp = (GA_Dummy + $0025); // [..G] (LongBool) If this is set by the gadget, the sends GADGETHELP messages.
  GA_Bounds = (GA_Dummy + $0026); // [IS.] (PIBox) Bounds to be copied into the ExtGadget structure.
  GA_RelSpecial = (GA_Dummy + $0027); { [IS.] (Long) This attribute should only be set by subclasses of
     GadgetClass. Applications should keep their hands away!
     If set this means, that GM_LAYOUT is called, when the window it is in is
     opened or its size changes. This allows gadgets to make their own size
     dependent on the size of the window. }
  GA_TextAttr = GA_Dummy + 40; // [IS.] (PTextAttr) Indicate the font to use for the gadget.
  GA_ReadOnly = GA_Dummy + 41; // (LongBool) Indicate that the gadget is read-only (non-selectable). Default = False
  GA_Underscore = GA_Dummy + 42; // (Char) Underscore/escape character for keyboard shortcuts. Defaults = '_'
  GA_ActivateKey = GA_Dummy + 43; // (PChar) Set/Get the gadgets shortcut/activation key(s) Default = nil
  GA_BackFill = GA_Dummy + 44; // (PHook) Backfill pattern hook. Defaults to nil.
  GA_GadgetHelpText = GA_Dummy + 45; // (PChar) RESERVERD/PRIVATE DO NOT USE. Default = nil
  GA_UserInput = GA_Dummy + 46; // (LongBool) Notification tag indicates this notification is from the activite gadget receiving user input - an attempt to make IDCMPUPDATE more efficient.
// Aros Specifics
  GA_LabelPlace = GA_Dummy + 100; { [I..] (LongInt) Choose the placing of the label. GadgetClass does not support
     this directly. Its subclasses have to take care of that. For possible values see GV_* .}
     
// Placetext values for GA_LabelPlace.
  GV_LabelPlace_In    = 1;
  GV_LabelPlace_Left  = 2;
  GV_LabelPlace_Right = 3;
  GV_LabelPlace_Above = 4;
  GV_LabelPlace_Below = 5;

//*** PropGClass
  // This class defines a standard proportional gadget.
  PGA_Dummy       = TAG_USER + $31000;
  PGA_Freedom     = PGA_Dummy + 1;  // [IS.] (LongWord) Define in which the direction gadget should stretch. Possible values are FREEVERT and FREEHORIZ
  PGA_Borderless  = PGA_Dummy + 2;  // [IS.] (LongBool) If set, no border will be rendered.
  // The following four attributes should not be used with PGA_Total, PGA_Visible and PGA_Top. 
  PGA_HorizPot    = PGA_Dummy + 3;  // [ISG] (Word)
  PGA_HorizBody   = PGA_Dummy + 4;  // [ISG] (Word)
  PGA_VertPot     = PGA_Dummy + 5;  // [ISG] (Word)
  PGA_VertBody    = PGA_Dummy + 6;  // [ISG] (Word)
  // The following three attributes should not be used with the PGA_*Pot and PGA_*Body attributes.
  PGA_Total       = PGA_Dummy + 7;  // [IS.] (Word) The total number of positions in the gadget.
  PGA_Visible     = PGA_Dummy + 8;  // [IS.] (Word) The number of visible positions in the gadget.
  PGA_Top         = PGA_Dummy + 9;  // [ISG] (Word) The first visible position.
  PGA_NewLook     = PGA_Dummy + 10; // [IS.] (LongBool) If set, this indicated that the new look should be used for rendering.
  PGA_DisplayHook = PGA_Dummy + 11; // [I.G] (PHook) Use this Hook to render the Gadget visuals
  PGA_NotifyBehaviour = PGA_Dummy + 30; { [I..] (Word) If set to PG_BEHAVIOUR_NICE OM_NOTIFY messages are sent
    also during OM_SET/OM_UPDATE, not just when user drags the knob, which is the default behaviour (PG_BEHAVIOUR_COMPATIBLE)}
  PGA_RenderBehaviour = PGA_Dummy + 31; { [I..] (Word) If set to PG_BEHAVIOUR_NICE the gadget is re-rendered
    during OM_SET/OM_UPDATE even when being a subclass of propgclass.
    The default behaviour (PG_BEHAVIOUR_COMPATIBLE) is that subclasses
    of propgclass don't render in OM_SET/OM_UPDATE }
  // Flags for PGA_*Behaviour
  PG_BEHAVIOUR_COMPATIBLE = 0;
  PG_BEHAVIOUR_NICE       = 1;

// StrGClass StringGClass is just a normal "string" gadget.
  STRINGA_Dummy          = TAG_USER + $32000;
  STRINGA_MaxChars       = STRINGA_Dummy + 1;  // [I..] (SmallInt) Maximum number of characters the string gadget accepts. Default SG_DEFAULTMAXCHARS.
  STRINGA_Buffer         = STRINGA_Dummy + 2;  // [I..] (STRPTR) Buffer for storing the current string of the gadget.
  STRINGA_UndoBuffer     = STRINGA_Dummy + 3;  // [I..] (STRPTR) Buffer for storing the old (undo) string of the gadget.
  STRINGA_WorkBuffer     = STRINGA_Dummy + 4;  // [I..] (STRPTR) Buffer for the class to work with.
  STRINGA_BufferPos      = STRINGA_Dummy + 5;  // [IS.] (SmallInt) Current position of cursor (relative to the beginning of the buffer).
  STRINGA_DispPos        = STRINGA_Dummy + 6;  // [IS.] (SmallInt) FIXME
  STRINGA_AltKeyMap      = STRINGA_Dummy + 7;  // [IS.] (PKeyMap) KeyMap to use
  STRINGA_Font           = STRINGA_Dummy + 8;  // [IS.] (PTextFont) Font to use for displaying the string
  STRINGA_Pens           = STRINGA_Dummy + 9;  // [IS.] (LongInt) The lower 16 bits specify the background-pen, the upper 16 bits the foreground-pen. The gadget is rendered, using these pens, if the gadget is inactive
  STRINGA_ActivePens     = STRINGA_Dummy + 10; // [IS.] (LongInt) Like STRINGA_Pens. These pens are used, if the gadget is active. 
  STRINGA_EditHook       = STRINGA_Dummy + 11; // [I..] (PHook) FIXME
  STRINGA_EditModes      = STRINGA_Dummy + 12; // [IS.] (LongWord) FIXME
  STRINGA_ReplaceMode    = STRINGA_Dummy + 13; // [IS.] (BOOL) If this is TRUE, the current character is overwritten, if the use presses a key. Otherwise, the new character is inserted.
  STRINGA_FixedFieldMode = STRINGA_Dummy + 14; // [IS.] (LongBool) FIXME
  STRINGA_NoFilterMode   = STRINGA_Dummy + 15; // [IS.] (LongBool) FIXME
  STRINGA_Justification  = STRINGA_Dummy + 16; // [IS.] (UWORD) Where should the text be justified? Use one of GACT_STRINGCENTER, GACT_STRINGLEFT and GACT_STRINGRIGHT
  STRINGA_LongVal        = STRINGA_Dummy + 17; // [ISG] (LONG) If this is set, the string gadget will only accept numeric values. Argument is the number, the string gadget is to be set to. When getting this attribute, this number is returned.
  STRINGA_TextVal        = STRINGA_Dummy + 18; // [ISG] (STRPTR) If this is set, the string gadget will accept strings. Argument is a string that is to be copied into the string gadget and its buffer.
  STRINGA_ExitHelp       = STRINGA_Dummy + 19; // [IS.] (BOOL) If this is set, pressing the "Help" key, while the gadget is active, will unselect the gadget.

  SG_DEFAULTMAXCHARS = 128; //  Default, if STRINGA_MaxChars is not set.

  // Gadget Layout related attributes
  LAYOUTA_Dummy          = TAG_USER + $38000;
  LAYOUTA_LayoutObj      = LAYOUTA_Dummy + 1;
  LAYOUTA_Spacing        = LAYOUTA_Dummy + 2;
  LAYOUTA_Orientation    = LAYOUTA_Dummy + 3; // Orientation LORIENT_*
  LAYOUTA_ChildMaxWidth  = LAYOUTA_Dummy + 4; // (BOOL) Child objects are of equal width. Should default to True for gadgets with a horizontal orientation.
  LAYOUTA_ChildMaxHeight = LAYOUTA_Dummy + 5; // (BOOL) Child objects are of equal height. Should default to True for gadgets with a vertical orientation.
  // orientation values for LAYOUTA_Orientation 
  LORIENT_NONE  = 0;
  LORIENT_HORIZ = 1;
  LORIENT_VERT  = 2;

// Gadget Method ID's
  GM_Dummy       = -1; // not used for anything
  GM_HITTEST     = 0;  // return GMR_GADGETHIT IF you are clicked on (whether or not you are disabled).
  GM_RENDER      = 1;  // draw yourself, in the appropriate state
  GM_GOACTIVE    = 2;  // you are now going to be fed input
  GM_HANDLEINPUT = 3;  // handle that input
  GM_GOINACTIVE  = 4;  // whether or not by choice, you are done
  GM_HELPTEST    = 5;  // Will you send gadget help if the mouse is at the specified coordinates?  See below for possible GMR_ values.                              }
  GM_LAYOUT      = 6;  // re-evaluate your size based on the GadgetInfo Domain.  Do NOT re-render yourself yet, you will be called when it is time...
  GM_DOMAIN      = 7;  // This method is invoked to learn about the sizing requirements of your class, before an object is created. 
  
// Parameter "Messages" passed to gadget class methods
type
  // GM_HITTEST
  { This method is used to test, if a mouse-click hit the gadget. You return
   GMR_GADGETHIT (see below), if you were hit and 0 otherwise. Note that you
   have to test, if you were hit, no matter if you are disabled or not.}
  PgpHitTest = ^TgpHitTest;
  TgpHitTest = record
    MethodID: LongWord;       // GM_HITEST or GM_HELPTEST
    gpht_GInfo: PGadgetInfo;
    gpht_Mouse: record         // These values are relative to the gadget select box for GM_HITTEST. For
      x: SmallInt;             // GM_HELPTEST they are relative to the bounding box (which is often
      y: SmallInt;             // equal to the select box).
    end;
  end;
const
  GMR_GADGETHIT = $00000004; // GM_HITTEST hit
  GMR_NOHELPHIT = $00000000; // GM_HELPTEST didn't hit
  GMR_HELPHIT   = $FFFFFFFF; // GM_HELPTEST hit, The gadget was hit. The lower word of the Code field of the IntuiMessage will be set to -1.
  GMR_HELPCODE  = $00010000; // GM_HELPTEST hit, return low Word as code. The gadget was hit. Pass the lower word, returned by this method to the application by using the Code field of the IntuiMessage.

type
  // GM_RENDER   This method is invoked to draw the gadget into a rastport.
  PgpRender = ^TgpRender;
  TgpRender = record
    MethodID: LongWord;       // GM_RENDER
    gpr_GInfo: PGadgetInfo;   // gadget context
    gpr_RPort: PRastPort;     // all ready for use
    gpr_Redraw: LongInt;      // might be a "highlight pass" (GREDRAW_*)
  end;
const
  // gpr_Redraw. Not all of these values make sense for all gadgets.
  GREDRAW_UPDATE = 2; // incremental update. Some data (e.g. the level of a slider) was updated. Just redraw the necessary parts.
  GREDRAW_REDRAW = 1; // redraw the whole gadget
  GREDRAW_TOGGLE = 0; // toggle highlight, IF applicable 

type
  // GM_GOACTIVE, GM_HANDLEINPUT
  {GM_GOACTIVE tells the gadget that it has become active and will receive
   input from now on from the method GM_HANDLEINPUT. This is stopped by using
   GM_GOINACTIVE (see below).
   GM_GOACTIVE and GM_HANDLEINPUT both use the same structure and return the
   same values, as defined below.}
  PgpInput = ^TgpInput;
  TgpInput = record
    MethodID: LongWord;       // GM_GOACTIVE or GM_HANDLEINPUT
    gpi_GInfo: PGadgetInfo;   //  gadget context
    gpi_IEvent: PInputEvent;  // Pointer to the InputEvent that caused the method to be invoked.
    gpi_Termination: Pointer; { Pointer to a variable that is to be set by the gadget class, if
         GMR_VERIFY is returned. The lower 16 bits of this value are returned
         in the Code field of the IntuiMessage  passed back to the application}
    gpi_Mouse: record         // This struct defines the current mouse position, relative to the gadgets' bounding box.
      x: SmallInt;
      y: SmallInt;
    end;
    gpi_TabletData: PTabletData; { Pointer to TabletData structure or nil,
         if this input event did not originate from a tablet that is capable of
         sending IESUBCLASS_NEWTABLET events.}
  end;
const
  // GM_HANDLEINPUT and GM_GOACTIVE  return code flags. These are actually flags and may be or'ed.
  GMR_MEACTIVE   = 0;       // Gadget is still alive.
  GMR_NOREUSE    = 1 shl 1; // Gadget has become inactive, but the input event may not be used again.
  GMR_REUSE      = 1 shl 2; // Gadget has become inactive, and the input event may be reused by intuition.
  GMR_VERIFY     = 1 shl 3; // Gadget was selected. Generate IDCMP_GADGETUP message. gpi_Termination must be set.
  { If one of the following two flags is returned, the gadget has become
    inactive, but the next or previous gadget, which has the GFLG_TABCYCLE flag
    set is to be activated.}
  GMR_NEXTACTIVE = 1 shl 4; // Activate next gadget.
  GMR_PREVACTIVE = 1 shl 5; // Activate previous gadget.
  
type
  // GM_GOINACTIVE see GM_GOACTIVE for explanation
  PgpGoInactive = ^TgpGoInactive;
  TgpGoInactive = record
    MethodID: LongWord;         // GM_GOINACTIVE
    gpgi_GInfo: PGadgetInfo;
    gpgi_Abort: LongWord; { Boolean field to indicate, who wanted the gadget to go inactive. If
         this is 1 this method was sent, because intution wants the gadget to
         go inactive, if it is 0, it was the gadget itself that wanted it.}
  end;

type
  //  GM_LAYOUT
  { This method is called by intuition, if on of the GFLG_REL flags or one of
    the GA_Rel attributes is set and the window size changes or you are added to
    a window. In this method you should re-evaluate the size of yourself. You
    are not allowed to do any rendering operation during this method!}
  PgpLayout = ^TgpLayout;
  TgpLayout = record
    MethodID: LongWord;       // GM_LAYOUT
    gpl_GInfo: PGadgetInfo;
    gpl_Initial: LongWord; {Boolean that indicated, if this method was invoked, when you are added
         to a window (True) or if it is called, because the window was resized
         (False).}
  end;

  // GM_DOMAIN
  { This method is invoked to learn about the sizing requirements of your class,
   before an object is created. This is AROS specific.}
  PgpDomain = ^TgpDomain;
  TgpDomain = record
    MethodID: LongWord;      // GM_DOMAIN
    gpd_GInfo: PGadgetInfo;
    gpd_RPort: PRastPort;    // RastPort to calculate dimensions for.
    gpd_Which: LONG;         // what to calculate (GDOMAIN_*)
    gpd_Domain: TIBox;       // Resulting domain
    gpd_Attrs: PTagItem;     // Additional attributes. None defines yet
  end;
const
  // gpd_Which 
  GDOMAIN_MINIMUM = 0; // Calculate minimum size.
  GDOMAIN_NOMINAL = 1; // Calculate nominal size.
  GDOMAIN_MAXIMUM = 2; // Calculate maximum size.
  
const
  ICM_Dummy     = $0401;          // used for nothing
  // no parameters for ICM_SETLOOP, ICM_CLEARLOOP, ICM_CHECKLOOP
  ICM_SETLOOP   = ICM_Dummy + 1;  // set/increment loop counter
  ICM_CLEARLOOP = ICM_Dummy + 2;  // clear/decrement loop counter
  ICM_CHECKLOOP = ICM_Dummy + 3;  // set/increment loop
  // interconnection attributes used by icclass, modelclass, and gadgetclass
  ICA_Dummy      = TAG_USER + $40000;
  ICA_TARGET     = ICA_Dummy + 1; // interconnection target
  ICA_MAP        = ICA_Dummy + 2; // interconnection map tagitem list
  ICSPECIAL_CODE = ICA_Dummy + 3; // a "pseudo-attribute", see below.

{ Normally, the value for ICA_TARGET is some object pointer,
  but if you specify the special value ICTARGET_IDCMP, notification
  will be send as an IDCMP_IDCMPUPDATE message to the appropriate window's
  IDCMP port.  See the definition of IDCMP_IDCMPUPDATE.
  When you specify ICTARGET_IDCMP for ICA_TARGET, the map you
  specify will be applied to derive the attribute list that is
  sent with the IDCMP_IDCMPUPDATE message.  If you specify a map list
  which results in the attribute tag id ICSPECIAL_CODE, the
  lower sixteen bits of the corresponding ti_Data value will
  be copied into the Code field of the IDCMP_IDCMPUPDATE IntuiMessage.}
  ICTARGET_IDCMP = not 0;

const
  // if image.Depth is this, it's a new Image class object
  CUSTOMIMAGEDEPTH = -1;
  
  // Attributes for IMAGECLASS
  IA_Dummy        = TAG_USER + $20000;
  IA_Left         = IA_Dummy + $01;
  IA_Top          = IA_Dummy + $02;
  IA_Width        = IA_Dummy + $03;
  IA_Height       = IA_Dummy + $04;
  IA_FGPen        = IA_Dummy + $05; // IA_FGPen also means "PlanePick"
  IA_BGPen        = IA_Dummy + $06; // IA_BGPen also means "PlaneOnOff"
  IA_Data         = IA_Dummy + $07; // bitplanes, for classic image, other image classes may use it for other things
  IA_LineWidth    = IA_Dummy + $08;
  IA_Pens         = IA_Dummy + $0E; // pointer to UWord pens[], ala DrawInfo.Pens, MUST be terminated by (not 0). Some classes can choose to have this, or SYSIA_DrawInfo, or both.
  IA_Resolution   = IA_Dummy + $0F; // packed uwords for x/y resolution into a LongWord ala DrawInfo.Resolution
  // see class documentation to learn which classes recognize these
  IA_APattern     = IA_Dummy + $10;
  IA_APatSize     = IA_Dummy + $11;
  IA_Mode         = IA_Dummy + $12;
  IA_Font         = IA_Dummy + $13;
  IA_Outline      = IA_Dummy + $14;
  IA_Recessed     = IA_Dummy + $15;
  IA_DoubleEmboss = IA_Dummy + $16;
  IA_EdgesOnly    = IA_Dummy + $17; // to specify that the interior of a frame should not be cleared
  // SYSICLASS attributes
  SYSIA_Size          = IA_Dummy + $0B; // see below SYSISIZE_*
  SYSIA_Depth         = IA_Dummy + $0C;
  SYSIA_Which         = IA_Dummy + $0D; // see below 
  SYSIA_UserBuffer    = IA_Dummy + $20; // Only for system Images
  SYSIA_DrawInfo      = IA_Dummy + $18; // Must be specified
  SYSIA_ReferenceFont = IA_Dummy + $19; // Font to use as reference for scaling certain sysiclass images
  IA_SupportsDisable  = IA_Dummy + $1a; // Tell intuition to use IDS_*DISABLED instead of own code
  IA_FrameType        = IA_Dummy + $1b; // Starting with V39, FrameIClass recognizes several standard types of frame.  Use one of the FRAME_* specifiers below.  Default = FRAME_DEFAULT.
  // Private AROS sysiclass tags and defines
  SYSIA_WithBorder    = IA_FGPen; // default: True
  SYSIA_Style         = IA_BGPen;	// default: SYSISTYLE_NORMAL
  
  SYSISTYLE_NORMAL   = 0;
  SYSISTYLE_GADTOOLS = 1;	// to get arrow images in gadtools look

  // data values for SYSIA_Size
  SYSISIZE_MEDRES = 0;
  SYSISIZE_LOWRES = 1;
  SYSISIZE_HIRES  = 2;

// SYSIA_Which tag data values: Specifies which system gadget you want an image for. Some numbers correspond to internal Intuition
  DEPTHIMAGE  = $00; // Window depth gadget image
  ZOOMIMAGE   = $01; // Window zoom gadget image
  SIZEIMAGE   = $02; // Window sizing gadget image
  CLOSEIMAGE  = $03; // Window close gadget image
  SDEPTHIMAGE = $05; // Screen depth gadget image
  LEFTIMAGE   = $0A; // Left-arrow gadget image
  UPIMAGE     = $0B; // Up-arrow gadget image
  RIGHTIMAGE  = $0C; // Right-arrow gadget image
  DOWNIMAGE   = $0D; // Down-arrow gadget image
  CHECKIMAGE  = $0E; // GadTools checkbox image
  MXIMAGE     = $0F; // GadTools mutual exclude "button" image
  MENUCHECK   = $10; // Menu checkmark image
  AMIGAKEY    = $11; // Menu Amiga-key image

  // Data values for IA_FrameType (recognized by FrameIClass)
  FRAME_DEFAULT     = 0; // The standard V37-type frame, which has thin edges.
  FRAME_BUTTON      = 1; // Standard button gadget frames, having thicker sides and nicely edged corners.
  FRAME_RIDGE       = 2; // A ridge such as used by standard string gadgets. You can recess the ridge to get a groove image.
  FRAME_ICONDROPBOX = 3; // A broad ridge which is the standard imagery for areas in AppWindows where icons may be dropped.

  // image message id's
  IM_DRAW        = $202; // draw yourself, with "state"
  IM_HITTEST     = $203; // return True if click hits image
  IM_ERASE       = $204; // erase yourself
  IM_MOVE        = $205; // draw new and erase old, smoothly
  IM_DRAWFRAME   = $206; // draw with specified dimensions
  IM_FRAMEBOX    = $207; // get recommended frame around some box
  IM_HITFRAME    = $208; // hittest with dimensions
  IM_ERASEFRAME  = $209; // hittest with dimensions

  // image draw states or styles, for IM_DRAW
  IDS_NORMAL           = 0;
  IDS_SELECTED         = 1; // for selected gadgets
  IDS_DISABLED         = 2; // for disabled gadgets
  IDS_BUSY             = 3; // for future functionality
  IDS_INDETERMINATE    = 4; // for future functionality
  IDS_INACTIVENORMAL   = 5; // normal, in inactive window border
  IDS_INACTIVESELECTED = 6; // selected, in inactive border
  IDS_INACTIVEDISABLED = 7; // disabled, in inactive border
  IDS_SELECTEDDISABLED = 8; // disabled and selected
  
type
  // IM_FRAMEBOX
  PimpFrameBox = ^TimpFrameBox;
  TimpFrameBox = record
    MethodID: LongWord;
    imp_ContentsBox: PIBox;   // input: relative box of contents
    imp_FrameBox: PIBox;      // output: rel. box of encl frame
    imp_DrInfo: PDrawInfo;    // may be nil
    imp_FrameFlags: LongWord;
  end;

const
  FRAMEF_SPECIFY = 1 shl 0; // Make do with the dimensions of FrameBox provided.

type
  PimpPos = ^TimpPos;
  TimpPos = record
    X: SmallInt;
    Y: SmallInt;
  end;

  PimpSize = ^TimpSize;
  TimpSize = record
    Width: SmallInt;
    Height: SmallInt;
  end;

  // IM_DRAW, IM_DRAWFRAME
  PimpDraw = ^TimpDraw;
  TimpDraw = record
    MethodID: LongWord;
    imp_RPort: PRastPort;
    imp_Offset: TimpPos;
    imp_State: LongWord;
    imp_DrInfo: PDrawInfo;    // May be nil
    imp_Dimensions: TimpSize; // Only valid for IM_DRAWFRAME
  end;

  // IM_ERASE, IM_ERASEFRAME NOTE: This is a subset of TimpDraw
  PimpErase = ^TimpErase;
  TimpErase = record
    MethodID: LongWord;
    imp_RPort: PRastPort;
    imp_Offset: TimpPos;
    imp_Dimensions: TimpSize; // // Only valid for IM_DRAWFRAME
  end;

  // IM_HITTEST, IM_HITFRAME
  PimpHitTest = ^TimpHitTest;
  TimpHitTest = record
    MethodID: LongWord;
    imp_Point: TimpPos;
    imp_Dimensions: TimpSize; // only valid for IM_HITFRAME
  end;

// 'boopsi' pointer class interface
const
  // The following tags are recognized at NewObject() time by pointerclass
  POINTERA_Dummy       = TAG_USER + $39000;
  POINTERA_BitMap      = POINTERA_Dummy + $01; // (PBitmap) Pointer to bitmap to get pointer imagery from.  Bitplane data need not be in chip RAM.
  POINTERA_XOffset     = POINTERA_Dummy + $02; // (LongInt) - X-offset of the pointer hotspot.
  POINTERA_YOffset     = POINTERA_Dummy + $03; // (LongInt) - Y-offset of the pointer hotspot.
  POINTERA_WordWidth   = POINTERA_Dummy + $04; // (LongWord) - designed width of the pointer in words
  POINTERA_XResolution = POINTERA_Dummy + $05; // (LongWord) - one of the POINTERXRESN_ flags below
  POINTERA_YResolution = POINTERA_Dummy + $06; // (LongWord) - one of the POINTERYRESN_ flags below

  // These are the choices for the POINTERA_XResolution attribute which
  // will determine what resolution pixels are used for this pointer.
  POINTERXRESN_DEFAULT   = 0; // (ECS-compatible pointer width) = 70 ns if SUPERHIRES-type mode, 140 ns if not
  POINTERXRESN_140NS     = 1; // (pointer always in 140 ns pixels) = 140 ns always
  POINTERXRESN_70NS      = 2; // (pointer always in 70 ns pixels) = 70 ns always
  POINTERXRESN_35NS      = 3; // (pointer always in 35 ns pixels) = 35 ns always
  POINTERXRESN_SCREENRES = 4; // Same as pixel speed of screen
  POINTERXRESN_LORES     = 5; // (pointer always in lores-like pixels) = 140 ns in 15kHz modes, 70 ns in 31kHz modes
  POINTERXRESN_HIRES     = 6; // (pointer always in hires-like pixels) = 70 ns in 15kHz modes, 35 ns in 31kHz modes

{ These are the choices for the POINTERA_YResolution attribute which
  will determine what vertical resolution is used for this pointer.
 
  POINTERYRESN_DEFAULT
       = In 15 kHz modes, the pointer resolution will be the same
         as a non-interlaced screen.  In 31 kHz modes, the pointer
         will be doubled vertically.  This means there will be about
         200-256 pointer lines per screen.
 
  POINTERYRESN_HIGH
  POINTERYRESN_HIGHASPECT
       = Where the hardware/software supports it, the pointer resolution
         will be high.  This means there will be about 400-480 pointer
         lines per screen.  POINTERYRESN_HIGHASPECT also means that
         when the pointer comes out double-height due to hardware/software
         restrictions, its width would be doubled as well, if possible
         (to preserve aspect).
 
  POINTERYRESN_SCREENRES
  POINTERYRESN_SCREENRESASPECT
       = Will attempt to match the vertical resolution of the pointer
         to the screen's vertical resolution.  POINTERYRESN_SCREENASPECT also
         means that when the pointer comes out double-height due to
         hardware/software restrictions, its width would be doubled as well,
         if possible (to preserve aspect).}

  POINTERYRESN_DEFAULT         = 0;
  POINTERYRESN_HIGH            = 2;
  POINTERYRESN_HIGHASPECT      = 3;
  POINTERYRESN_SCREENRES       = 4;
  POINTERYRESN_SCREENRESASPECT = 5;
type
  PSGWork = ^TSGWork;
  TSGWork = record
    // set up when gadget is first activated    }
    Gadget: PGadget;         // the contestant itself
    StringInfo: PStringInfo; // easy access to sinfo
    WorkBuffer: STRPTR;      // intuition's planned result
    PrevBuffer: STRPTR;      // what was there before
    Modes: LongWord;         // current mode
    // modified for each input event
    IEvent: PInputEvent;     // actual event: do not change
    Code: Word;              // character code, IF one Byte
    BufferPos: SmallInt;     // cursor position
    NumChars: SmallInt;
    Actions: LongWord;       // what Intuition will do
    LongInt_: LongInt;       // temp storage for LongInt
    GadgetInfo: PGadgetInfo;
    EditOp: Word;            // from constants below
  end;

{ TSGWork.EditOp - These values indicate what basic type of operation the global editing hook has performed on the string before your gadget's custom
  editing hook gets called.  You do not have to be concerned with the value your custom hook leaves in the EditOp field, only if you write a global editing hook.
  For most of these general edit operations, you'll want to compare the BufferPos and NumChars of the StringInfo (before global editing) and SGWork (after global editing).}
const
  EO_NOOP        = $0001; // did nothing
  EO_DELBACKWARD = $0002; // deleted some chars (maybe 0).
  EO_DELFORWARD  = $0003; // deleted some characters under and in front of the cursor
  EO_MOVECURSOR  = $0004; // moved the cursor
  EO_ENTER       = $0005; // "enter" or "return" key, terminate
  EO_RESET       = $0006; // current Intuition-style undo
  EO_REPLACECHAR = $0007; // replaced one character and (maybe) advanced cursor
  EO_INSERTCHAR  = $0008; // inserted one char into string or added one at end
  EO_BADFORMAT   = $0009; // didn't like the text data, e.g., Bad LONGINT
  EO_BIGCHANGE   = $000A; // unused by Intuition complete or major change to the text, e.g. new string
  EO_UNDO        = $000B; // unused by Intuition  some other style of undo
  EO_CLEAR       = $000C; // clear the string
  EO_SPECIAL     = $000D; // unused by Intuition some operation that doesn't fit into the categories here

  // Mode Flags definitions (ONLY first group allowed as InitialModes)
  SGM_REPLACE    = 1 shl 0; // replace mode please initialize StringInfo with in-range value of BufferPos if you are using SGM_REPLACE mode.
  SGM_FIXEDFIELD = 1 shl 1; // fixed length buffer always set SGM_REPLACE, too
  SGM_NOFILTER   = 1 shl 2; // don't filter control chars
  SGM_EXITHELP   = 1 shl 7; // exit with code = $5F IF HELP hit
  // These Mode Flags are for internal use only
  SGM_NOCHANGE   = 1 shl 3; // no edit changes yet
  SGM_NOWORKB    = 1 shl 4; // Buffer = PrevBuffer
  SGM_CONTROL    = 1 shl 5; // control char escape mode
  SGM_LONGINT    = 1 shl 6; // an intuition LongInt gadget
  // String Gadget Action Flags (put in SGWork.Actions by EditHook)
  SGA_USE        = $1;  // use contents of SGWork
  SGA_END        = $2;  // terminate gadget, code in Code field
  SGA_BEEP       = $4;  // flash the screen for the user
  SGA_REUSE      = $8;  // reuse input event
  SGA_REDISPLAY  = $10; // gadget visuals changed
  SGA_NEXTACTIVE = $20; // Make next possible gadget active.
  SGA_PREVACTIVE = $40; // Make previous possible gadget active.
  // function id for only existing custom string gadget edit hook
  SGH_KEY   = 1; // process editing keystroke
  SGH_CLICK = 2; // process mouse click cursor position 

{ Here's a brief summary of how the custom string gadget edit hook works:
    You provide a hook in StringInfo.Extension.EditHook. The hook is called in the standard way with the 'object' a pointer to SGWork,
    and the 'message' a pointer to a command block, starting either with (LongWord) SGH_KEY, SGH_CLICK, or something new.
    You return 0 if you don't understand the command (SGH_KEY is required and assumed).  Return non-zero if you implement the command.
    SGH_KEY:
       There are no parameters following the command LongWord. Intuition will put its idea of proper values in the SGWork
       before calling you, and if you leave SGA_USE set in the SGWork.Actions field, Intuition will use the values
       found in SGWork fields WorkBuffer, NumChars, BufferPos, and LongInt, copying the WorkBuffer back to the StringInfo
       Buffer. 
       
       NOTE WELL: You may NOT change other SGWork fields.
       If you clear SGA_USE, the string gadget will be unchanged.
       
       If you set SGA_END, Intuition will terminate the activation of the string gadget.  If you also set SGA_REUSE, Intuition
       will reuse the input event after it deactivates your gadget. In this case, Intuition will put the value found in SGWork.Code
       into the IntuiMessage.Code field of the IDCMP_GADGETUP message it sends to the application.

       If you set SGA_BEEP, Intuition will call DisplayBeep(); use this if the user has typed in error, or buffer is full.
 
       Set SGA_REDISPLAY if the changes to the gadget warrant a gadget redisplay.  Note: cursor movement requires a redisplay.
       Starting in V37, you may set SGA_PREVACTIVE or SGA_NEXTACTIVE when you set SGA_END.  This tells Intuition that you want
       the next or previous gadget with GFLG_TABCYCLE to be activated.
 
    SGH_CLICK:
       This hook command is called when Intuition wants to position the cursor in response to a mouse click in the string gadget.
       Again, here are no parameters following the command LongWord. This time, Intuition has already calculated the mouse position
       character cell and put it in SGWork.BufferPos.  The previous BufferPos value remains in the SGWork.StringInfo.BufferPos.

       Intuition will again use the SGWork fields listed above for SGH_KEY.  One restriction is that you are NOT allowed to set
       SGA_END or SGA_REUSE for this command.  Intuition will not stand for a gadget which goes inactive when you click in it.
 
       You should always leave the SGA_REDISPLAY flag set, since Intuition uses this processing when activating a string gadget.}
type
{ NewDecorator structure used by ChangeDecoration the three Objects (nd_Window, nd_Screen and nd_Menu must be installed and point to decorator objects
  the port is used for different issues and will be filled up with DecoratorMessages}
  PNewDecorator = ^TNewDecorator;
  TNewDecorator = record
    nd_Node: TNode;
    nd_Port: PMsgPort;
    nd_cnt: Word;
    nd_Pattern: STRPTR;
    nd_IntPattern: STRPTR; // Private, transformated Pattern be dos/ParsePattern()
    nd_Window: PObject_;
    nd_Screen: PObject_;
    nd_Menu: PObject_;
  end;
  
  PDecoratorMessage = ^TDecoratorMessage;
  TDecoratorMessage = record
    dm_Message: TMagicMessage;
    dm_Class: LongWord;
    dm_Code: LongWord;
    dm_Flags: LongWord;
    dm_Object: IPTR;
  end;
  
  PScreenNotifyMessage = ^TScreenNotifyMessage;
  TScreenNotifyMessage = record
    snm_Message: PMagicMessage;
    snm_Class: LongWord;  // Notification Class ID same as SNA_Notify
    snm_Code: LongWord;   // Code only supported for ScreenDepth() and will put the Flags in
    snm_Object: IPTR;     // Pointer to the Object that caused this message      
    snm_UserData: IPTR;   // will be filled with SNA_UserData
  end;
  
const
  DECORATOR_VERSION    = 0;
  SCREENNOTIFY_VERSION = 0;
{ there is only one Message in the initial decoration system it will be sent to the decorator port to signal that it´ll not be used any longer
  and may be destroyed, in that case the dm_Object contains the NewDecorator struct Intuition does not touch anything, the decorator have to
  destroy all objects as well as the NewDecorator struct.} 
  DM_CLASS_DESTROYDECORATOR = $8001;
  // Tags for Screen notify message
  SNA_PubName  = TAG_USER + $01; // public screen name of nil for all screens
  SNA_Notify   = TAG_USER + $02; // Flags to look for see below
  SNA_UserData = TAG_USER + $03; // this tag will be passed to the screennotify message
  SNA_SigTask  = TAG_USER + $04; // if port = nil, a sigbit will be set for this task
  SNA_SigBit   = TAG_USER + $05; // signal bit to set if port = nil
  SNA_MsgPort  = TAG_USER + $06; // if <> nil post mesage to this port
  SNA_Priority = TAG_USER + $07;
  SNA_Hook     = TAG_USER + $08;

  // SNA_Notify (all unassigned bits are reserved for system use)
  SNOTIFY_AFTER_OPENSCREEN       = 1 shl 0;  // screen has been opened
  SNOTIFY_BEFORE_CLOSESCREEN     = 1 shl 1;  // going to close screen
  SNOTIFY_AFTER_OPENWB           = 1 shl 2;  // Workbench is open
  SNOTIFY_BEFORE_CLOSEWB         = 1 shl 3;  // Workbench is going to be closed
  SNOTIFY_AFTER_OPENWINDOW       = 1 shl 4;  // new window
  SNOTIFY_BEFORE_CLOSEWINDOW     = 1 shl 5;  // window is going to be closed
  SNOTIFY_PUBSCREENSTATE         = 1 shl 6;  // PubScreenState()
  SNOTIFY_LOCKPUBSCREEN          = 1 shl 7;  // LockPubScreen()
  SNOTIFY_SCREENDEPTH            = 1 shl 8;  // ScreenDepth()
  SNOTIFY_AFTER_CLOSESCREEN      = 1 shl 9;	 // notify after CloseScreen()
  SNOTIFY_AFTER_CLOSEWINDOW      = 1 shl 10; // dto. CloseWindow()
  SNOTIFY_BEFORE_OPENSCREEN      = 1 shl 11; // notify before OpenScreen()
  SNOTIFY_BEFORE_OPENWINDOW      = 1 shl 12; // dto. OpenWindow()
  SNOTIFY_BEFORE_OPENWB          = 1 shl 13; // like OPENSCREEN
  SNOTIFY_AFTER_CLOSEWB          = 1 shl 14; // like CLOSESCREEN
  SNOTIFY_WAIT_REPLY             = 1 shl 15; // wait for reply before taking action
  SNOTIFY_UNLOCKPUBSCREEN        = 1 shl 16; // UnlockPubScreen()
  SNOTIFY_BEFORE_UPDATEINTUITION = 1 shl 17; // Intuition is going to be updated
  SNOTIFY_AFTER_UPDATEINTUITION  = 1 shl 18; // Intuition is updated
  
  // Attributes for MENUDECORCLASS
  MDA_Dummy         = TAG_USER + $22000;
  MDA_DrawInfo      = MDA_Dummy + 1; // [I.G]
  MDA_Screen        = MDA_Dummy + 2; // [I.G]
  MDA_TrueColorOnly = MDA_Dummy + 3; // [..G]
  MDA_UserBuffer    = MDA_Dummy + 4; // [I.G]
  // Methods for MENUDECORCLASS
  MDM_Dummy               = MDA_Dummy + 500;
  MDM_GETDEFSIZE_SYSIMAGE = MDM_Dummy + 1;
  MDM_DRAW_SYSIMAGE       = MDM_Dummy + 2;
  MDM_GETMENUSPACES       = MDM_Dummy + 3;
  MDM_DRAWBACKGROUND      = MDM_Dummy + 4;
  MDM_INITMENU            = MDM_Dummy + 5;
  MDM_EXITMENU            = MDM_Dummy + 6;

type
  PmdpGetDefSizeSysImage = ^TmdpGetDefSizeSysImage;
  TmdpGetDefSizeSysImage = record
    MethodID: LongWord;
    mdp_TrueColor: ShortInt;
    mdp_Dri: PDrawInfo;
    mdp_ReferenceFont: PTextFont; // In:
    mdp_Which: LongWord;          // In: One of CLOSEIMAGE, SIZEIMAGE, ... 
    mdp_SysiSize: LongWord;       // In: lowres/medres/highres
    mdp_Width: PLongWord;         // Out
    mdp_Height: PLongWord;        // Out
    mdp_Flags: LongWord;
  end;

  PmdpDrawSysImage = ^TmdpDrawSysImage;
  TmdpDrawSysImage = record
    MethodID: LongWord;
    mdp_TrueColor: ShortInt;
    mdp_Dri: PDrawInfo;
    mdp_RPort: PRastPort;
    mdp_X: LongInt;
    mdp_Y: LongInt;
    mdp_Width: LongInt;
    mdp_Height: LongInt;
    mdp_Which: LongWord;
    mdp_State: LongWord;
    mdp_Flags: LongWord;
    mdp_UserBuffer: IPTR;
  end;

  PmdpGetMenuSpaces = ^TmdpGetMenuSpaces;
  TmdpGetMenuSpaces = record
    MethodID: LongInt;
    mdp_TrueColor: ShortInt;
    mdp_InnerLeft: LongInt;      // Out
    mdp_InnerTop: LongInt;       // Out
    mdp_InnerRight: LongInt;
    mdp_InnerBottom: LongInt;
    mdp_ItemInnerLeft: LongInt;
    mdp_ItemInnerTop: LongInt;
    mdp_ItemInnerRight: LongInt;
    mdp_ItemInnerBottom: LongInt;
    mdp_MinWidth: LongInt;
    mdp_MinHeight: LongInt;
  end;

  PmdpDrawBackground = ^TmdpDrawBackground;
  TmdpDrawBackground = record
    MethodID: Longword;
    mdp_TrueColor: ShortInt;
    mdp_RPort: PRastPort;
    mdp_X: LongInt;
    mdp_Y: LongInt;
    mdp_Width: LongInt;
    mdp_Height: LongInt;
    mdp_ItemLeft: LongInt;
    mdp_ItemTop: LongInt;
    mdp_ItemWidth: LongInt;
    mdp_ItemHeight: LongInt;
    mdp_Flags: Word;
    mdp_UserBuffer: IPTR;
    mdp_MenuDecorFlags: LongWord;
  end;

  PmdpInitMenu = ^TmdpInitMenu;
  TmdpInitMenu = record
    MethodID: LongInt;
    mdp_TrueColor: SmallInt;
    mdp_RPort: PRastPort;
    mdp_Screen: PScreen;
    mdp_Left: LongWord;
    mdp_Top: LongWord;
    mdp_Width: LongWord;
    mdp_Height: LongInt;
    mdp_UserBuffer: IPTR;
    mdp_ScreenUserBuffer: IPTR;
    mdp_MenuDecorFlags: LongWord;
  end;

  PmdpExitMenu = ^TmdpExitMenu;
  TmdpExitMenu = record
    MethodID: LongInt;
    mdp_TrueColor: ShortInt;
    mdp_UserBuffer: IPTR;
  end;

const
  MDP_STATE_NORMAL   = 0;
  MDP_STATE_SELECTED = 1;
  MDP_STATE_DISABLED = 2;

  MDP_MDF_MENU             = 1 shl 0;
  MDP_MDF_ITEM             = 1 shl 1;
  MDP_MDF_SUBITEM          = 1 shl 2;
  MDP_MDF_MENUS_UNDERMOUSE = 1 shl 7;
  
  // Length of array returned by MA_PixelFormats
  MONITOR_MAXPIXELFORMATS = 14;

  // Attributes
  MA_Dummy               = TAG_USER;
  MA_MonitorName         = MA_Dummy + 1;	// [..G] STRPTR Monitor name
  MA_Manufacturer        = MA_Dummy + 2;	// [..G] STRPTR Hardware manufacturer string
  MA_ManufacturerID      = MA_Dummy + 3;	// [..G] LongWord
  MA_ProductID           = MA_Dummy + 4;	// [..G] LongWord
  MA_MemorySize          = MA_Dummy + 5;	// [..G] LongWord Video card memory size
  MA_PixelFormats        = MA_Dummy + 6;  // [..G] PLongWord Pixelformat support flags
  MA_TopLeftMonitor      = MA_Dummy + 7;	// [.SG] PObject_  Monitor placed in a position relative to the current one
  MA_TopMiddleMonitor    = MA_Dummy + 8;  // [.SG] PObject_
  MA_TopRightMonitor     = MA_Dummy + 9;  // [.SG] PObject_
  MA_MiddleLeftMonitor   = MA_Dummy + 10; // [.SG] PObject_
  MA_MiddleRightMonitor  = MA_Dummy + 11; // [.SG] PObject_
  MA_BottomLeftMonitor   = MA_Dummy + 12; // [.SG] PObject_
  MA_BottomMiddleMonitor = MA_Dummy + 13; // [.SG] PObject_
  MA_BottomRightMonitor  = MA_Dummy + 14; // [.SG] PObject_ 
  MA_GammaControl        = MA_Dummy + 15; // [..G] LongBool Whether gamma control is supported
  MA_PointerType         = MA_Dummy + 16; // [..G] LongWord Supported pointer types
  MA_DriverName          = MA_Dummy + 17; // [..G] STRPTR Driver name
  MA_MemoryClock         = MA_Dummy + 18; // [..G] LongWord Video memory clock in Hz, 0 if unknown

  //* Pointer type flags */
  PointerType_3Plus1 = $0001; // color 0 transparent, 1-3 visible
  PointerType_2Plus1 = $0002; // color 0 transparent, 2-3 visible, 1 undefined/clear/inverse
  PointerType_ARGB   = $0004; // Direct color alpha-blended bitmap pointer

  // Methods
  MM_GetRootBitMap	       = $401; // Reserved
  MM_Query3DSupport        = $402; // Ask for 3D acceleration support for given pixelformat
  MM_GetDefaultGammaTables = $403; // Get default gamma correction table
  MM_GetDefaultPixelFormat = $404; // Ask for preferred pixelformat for given depth (-1 = unsupported depth)
  MM_GetPointerBounds      = $405; // Ask for maximum supported mouse pointer size
  MM_RunBlanker            = $406; // Start screensaver for this monitor
  MM_EnterPowerSaveMode    = $407; // Start power saving mode
  MM_ExitBlanker           = $408; // Stop screensaver or power saving mode

  // AROS-specific attributes
  MA_AROS      = TAG_USER + $00010000;
  MA_Windowed  = MA_AROS + 1; // [G..] BOOL A display is a window on hosted OS 
  // AROS-specific methods
  MM_SetDefaultGammaTables = $1401; // Set default gamma correction table

type
  PmsGetRootBitMap = ^TmsGetRootBitMap;
  TmsGetRootBitMap = record
    MethodID: LongWord;
    PixelFormat: LongWord;
    Store: ^PBitMap;
  end;

  PmsQuery3DSupport = ^TmsQuery3DSupport;
  TmsQuery3DSupport = record
    MethodID: LongWord;
    PixelFormat: LongWord;
    Store: PLongWord;
  end;
const
  MSQUERY3D_UNKNOWN  = 0; // Unsupported pixelformat or other error
  MSQUERY3D_NODRIVER = 1; // No 3D support available
  MSQUERY3D_SWDRIVER = 2; // Software 3D support available
  MSQUERY3D_HWDRIVER = 3; // Hardware accelerated 3D available
type
  PmsGetDefaultGammaTables = ^TmsGetDefaultGammaTables;
  TmsGetDefaultGammaTables = record
    MethodID: LongWord;
    Red: PByte;		      // Optional pointers to 256-byte arrays to fill in
    Green: PByte;
    Blue: PByte;
  end;

  PmsGetDefaultPixelFormat = ^TmsGetDefaultPixelFormat;
  TmsGetDefaultPixelFormat = record
    MethodID: LongWord;
    Depth: LongWord;
    Store: PLongWord;
  end;

  PmsGetPointerBounds = ^TmsGetPointerBounds;
  TmsGetPointerBounds = record
    MethodID: LongWord;
    PointerType: LongWord;
    Width: PLongWord;
    Height: PLongWord;
  end;

  PmsSetDefaultGammaTables = PmsGetDefaultGammaTables;
  TmsSetDefaultGammaTables = TmsGetDefaultGammaTables;
const
  // Attributes for SCRDECORCLASS
  SDA_Dummy = TAG_USER + $22100;
  SDA_DrawInfo      = SDA_Dummy + 1; // [I.G]
  SDA_Screen        = SDA_Dummy + 2; // [I.G]
  SDA_TrueColorOnly	= SDA_Dummy + 3; // [..G]
  SDA_UserBuffer    = SDA_Dummy + 4; // [I.G]
  // Methods for SCRDECORCLASS */
  SDM_Dummy                = SDA_Dummy + 500;
  SDM_SETUP                = SDM_Dummy + 1;
  SDM_CLEANUP              = SDM_Dummy + 2;
  SDM_GETDEFSIZE_SYSIMAGE  = SDM_Dummy + 3;
  SDM_DRAW_SYSIMAGE        = SDM_Dummy + 4;
  SDM_DRAW_SCREENBAR       = SDM_Dummy + 5;
  SDM_LAYOUT_SCREENGADGETS = SDM_Dummy + 6;
  SDM_INITSCREEN           = SDM_Dummy + 7;
  SDM_EXITSCREEN           = SDM_Dummy + 8;
type
  PsdpGetDefSizeSysImage = ^TsdpGetDefSizeSysImage;
  TsdpGetDefSizeSysImage = record
    MethodID: LongWord;
    sdp_TrueColor: ShortInt;
    sdp_Dri: PDrawInfo;
    sdp_ReferenceFont: PTextFont; // In:
    sdp_Which: LongWord;  	      // In: SDEPTHIMAGE
    sdp_SysiSize: LongWord;	      // In: lowres/medres/highres
    sdp_Width: PLongWord;  	      // Out
    sdp_Height: PLongWord; 	      // Out
    sdp_Flags: LongWord;
    sdp_UserBuffer: LongWord;
  end;
  
  PsdpDrawSysImage = ^TsdpDrawSysImage;
  TsdpDrawSysImage = record
    MethodID: LongInt;
    sdp_TrueColor: ShortInt;
    sdp_Dri: PDrawInfo;
    sdp_RPort: PRastPort;
    sdp_X: LongInt;
    sdp_Y: LongInt;
    sdp_Width: LongInt;
    sdp_Height: LongInt;
    sdp_Which: LongWord;
    sdp_State: LongWord;
    sdp_Flags: LongWord;
    sdp_UserBuffer: IPTR;
  end;

  PsdpDrawScreenBar = ^TsdpDrawScreenBar;
  TsdpDrawScreenBar = record
    MethodID: LongWord;
    sdp_TrueColor: ShortInt;
    sdp_Dri: PDrawInfo;
    sdp_Layer: PLayer;
    sdp_RPort: PRastPort;
    sdp_Screen: PScreen;
    sdp_Flags: LongWord;
    sdp_UserBuffer: IPTR;
  end;

  PsdpLayoutScreenGadgets = ^TsdpLayoutScreenGadgets;
  TsdpLayoutScreenGadgets = record
    MethodID: LongWord;
    sdp_TrueColor: ShortInt;
    sdp_Dri: PDrawInfo;
    sdp_Layer: PLayer;
    sdp_Gadgets: PGadget;
    sdp_Flags: LongWord;
    sdp_UserBuffer: IPTR;
  end;

  PsdpInitScreen = ^TsdpInitScreen;
  TsdpInitScreen = record
    MethodID: LongWord;
    sdp_TrueColor: ShortInt;
    sdp_Dri: PDrawInfo;
    sdp_Screen: PScreen;
    sdp_FontHeight: LongWord;
    sdp_TitleHack: LongInt;
    sdp_BarHeight: LongWord;
    sdp_BarVBorder: LongWord;
    sdp_BarHBorder: LongWord;
    sdp_MenuVBorder: LongWord;
    spd_MenuHBorder: LongWord;
    sdp_WBorTop: ShortInt;
    sdp_WBorLeft: ShortInt;
    sdp_WBorRight: ShortInt;
    sdp_WBorBottom: ShortInt;
    sdp_UserBuffer: IPTR;
  end;

  PsdpExitScreen = ^TsdpExitScreen;
  TsdpExitScreen = record
    MethodID: LongWord;
    sdp_TrueColor: ShortInt;
    sdp_UserBuffer: IPTR;
  end;
const
// ScrDecor LayoutScreenGadgets Flags
  SDF_LSG_INITIAL      = 1; // First time = During OpenScreen
  SDF_LSG_SYSTEMGADGET = 2; // Is a system gadget (sdepth)
  SDF_LSG_INGADLIST    = 4; // Gadget is already in screen gadget list
  SDF_LSG_MULTIPLE     = 8; // There may be multiple gadgets (linked together through NextGadget. Follow it)

// Attributes for WINDECORCLASS 
  WDA_Dummy         = TAG_USER + $22000;
  WDA_DrawInfo      = WDA_Dummy + 1; 	// [I.G]
  WDA_Screen        = WDA_Dummy + 2; 	// [I.G]
  WDA_TrueColorOnly = WDA_Dummy + 3; 	// [..G]
  WDA_UserBuffer    = WDA_Dummy + 4;  // [I.G]

// Methods for WINDECORCLASS
  WDM_Dummy                = WDA_Dummy + 500;
  WDM_SETUP                = WDM_Dummy + 1;
  WDM_CLEANUP              = WDM_Dummy + 2;
  WDM_GETDEFSIZE_SYSIMAGE  = WDM_Dummy + 3;
  WDM_DRAW_SYSIMAGE        = WDM_Dummy + 4;
  WDM_DRAW_WINBORDER       = WDM_Dummy + 5;
  WDM_LAYOUT_BORDERGADGETS = WDM_Dummy + 6;
  WDM_DRAW_BORDERPROPBACK  = WDM_Dummy + 7;
  WDM_DRAW_BORDERPROPKNOB  = WDM_Dummy + 8;
  WDM_INITWINDOW           = WDM_Dummy + 9;
  WDM_EXITWINDOW           = WDM_Dummy + 10;
  WDM_WINDOWSHAPE          = WDM_Dummy + 11;

type
  PwdpGetDefSizeSysImage = ^TwdpGetDefSizeSysImage;
  TwdpGetDefSizeSysImage = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_ReferenceFont: PTextFont; // In:
    wdp_Which: LongWord;          // In: One of CLOSEIMAGE, SIZEIMAGE, ...
    wdp_SysiSize: LongWord;       // In: lowres/medres/highres
    wdp_Width: PLongWord;         // Out
    wdp_Height: PLongWord;        // Out
    wdp_Flags: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpDrawSysImage = ^TwdpDrawSysImage;
  TwdpDrawSysImage = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_RPort: PRastPort;
    wdp_X: LongInt;
    wdp_Y: LongInt;
    wdp_Width: LongInt;
    wdp_Height: LongInt;
    wdp_Which: LongWord;
    wdp_State: LongWord;
    wdp_Flags: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpDrawWinBorder = ^TwdpDrawWinBorder;
  TwdpDrawWinBorder = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_Window: PWindow;
    wdp_RPort: PRastPort;
    wdp_Flags: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpLayoutBorderGadgets = ^TwdpLayoutBorderGadgets;
  TwdpLayoutBorderGadgets = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_Window: PWindow;
    wdp_Gadgets: PGadget;
    wdp_Flags: LongWord;
    wdp_ExtraButtons: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpDrawBorderPropBack = ^TwdpDrawBorderPropBack;
  TwdpDrawBorderPropBack = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_Window: PWindow;
    wdp_RPort: PRastPort;
    wdp_Gadget: PGadget;
    wdp_RenderRect: PRectangle;
    wdp_PropRect: PRectangle;
    wdp_KnobRect: Prectangle;
    wdp_Flags: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpDrawBorderPropKnob = ^TwdpDrawBorderPropKnob;
  TwdpDrawBorderPropKnob = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Dri: PDrawInfo;
    wdp_Window: PWindow;
    wdp_RPort: PRastPort;
    wdp_Gadget: PGadget;
    wdp_RenderRect: PRectangle;
    wdp_PropRect: PRectangle;
    wdp_Flags: LongWord;
    wdp_UserBuffer: IPTR;
  end;

  PwdpInitWindow = ^TwdpInitWindow;
  TwdpInitWindow = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_UserBuffer: IPTR;
    wdp_Screen: PScreen;
    wdp_ScreenUserBuffer: IPTR;
  end;

  PwdpExitWindow = ^TwdpExitWindow;
  TwdpExitWindow = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_UserBuffer: IPTR;
  end;

  PwdpWindowShape = ^TwdpWindowShape;
  TwdpWindowShape = record
    MethodID: LongWord;
    wdp_TrueColor: ShortInt;
    wdp_Window: PWindow;
    wdp_Width: LongInt;
    wdp_Height: LongInt;
    wdp_UserBuffer: IPTR;
  end;
const
// WinDecor DrawWindowBorder Flags
   WDF_DWB_TOP_ONLY = 1;   // Draw top border only
// WinDecor DrawWinTitle Title Align 
  WD_DWTA_LEFT    = 0;
  WD_DWTA_RIGHT   = 1;
  WD_DWTA_CENTER  = 2;
// WinDecor LayourBorderGadgets Flags
  WDF_LBG_INITIAL      = 1; // First time == During OpenWindow
  WDF_LBG_SYSTEMGADGET = 2; // Is a system gadget (close/depth/zoom)
  WDF_LBG_INGADLIST    = 4; // Gadget is already in window gadget list
  WDF_LBG_MULTIPLE     = 8; // There may be multiple gadgets (linked together through NextGadget. Follow it)
// WinDecor DrawBorderPropKnob Flags
  WDF_DBPK_HIT = 1; // Knob is hit / in use by user


// extensions:
const

// Sysiclass  SYSIA_Which
  ICONIFYIMAGE    = $12;
  LOCKIMAGE       = $13;
  MUIIMAGE        = $14;
  POPUPIMAGE      = $15;
  SNAPSHOTIMAGE   = $16;
  JUMPIMAGE       = $17;
  MENUTOGGLEIMAGE = $19;
  SUBMENUIMAGE    = $1A;
  
// Window attributes
  WA_ExtraTitlebarGadgets = WA_Dummy + 151;
  WA_ExtraGadgetsStartID  = WA_Dummy + 152;
  WA_ExtraGadget_Iconify  = WA_Dummy + 153;
  WA_ExtraGadget_Lock     = WA_Dummy + 154;
  WA_ExtraGadget_MUI      = WA_Dummy + 155;
  WA_ExtraGadget_PopUp    = WA_Dummy + 156;
  WA_ExtraGadget_Snapshot = WA_Dummy + 157;
  WA_ExtraGadget_Jump     = WA_Dummy + 158;

// WA_ExtraTitlebarGadgets
  // Flags 
  ETG_ICONIFY  = $01;
  ETG_LOCK     = $02;
  ETG_MUI      = $04;
  ETG_POPUP    = $08;
  ETG_SNAPSHOT = $10;
  ETG_JUMP     = $20;

  // Gadget ID offsets
  ETD_Iconify  = 0;
  ETD_Lock     = 1;
  ETD_MUI      = 2;
  ETD_PopUp    = 3;
  ETD_Snapshot = 4;
  ETD_Jump     = 5;

  // Gadget IDs
  ETI_Dummy    = $FFD0;
  ETI_Iconify  = ETI_Dummy + ETD_Iconify;
  ETI_Lock     = ETI_Dummy + ETD_Lock;
  ETI_MUI      = ETI_Dummy + ETD_MUI;
  ETI_PopUp    = ETI_Dummy + ETD_PopUp;
  ETI_Snapshot = ETI_Dummy + ETD_Snapshot;
  ETI_Jump     = ETI_Dummy + ETD_Jump;

// Defines for WindowAction()
  // Commands
  WAC_BASE                = $0001;
  WAC_HIDEWINDOW          = WAC_BASE + 0 unimplemented;
  WAC_SHOWWINDOW          = WAC_BASE + 1 unimplemented;
  WAC_SENDIDCMPCLOSE      = WAC_BASE + 2;
  WAC_MOVEWINDOW          = WAC_BASE + 3 unimplemented;
  WAC_SIZEWINDOW          = WAC_BASE + 4 unimplemented;
  WAC_CHANGEWINDOWBOX     = WAC_BASE + 5 unimplemented;
  WAC_WINDOWTOFRONT       = WAC_BASE + 6 unimplemented;
  WAC_WINDOWTOBACK        = WAC_BASE + 7 unimplemented;
  WAC_ZIPWINDOW           = WAC_BASE + 8 unimplemented;
  WAC_MOVEWINDOWINFRONTOF = WAC_BASE + 9 unimplemented;
  WAC_ACTIVATEWINDOW      = WAC_BASE + 10 unimplemented;

  // Tags
  WAT_BASE = TAG_USER;
  // WAC_MOVEWINDOW
  WAT_MOVEWINDOWX       = WAT_BASE + 1;
  WAT_MOVEWINDOWY       = WAT_BASE + 2;
  // WAC_SIZEWINDOW
  WAT_SIZEWINDOWX       = WAT_BASE + 3;
  WAT_SIZEWINDOWY       = WAT_BASE + 4;
  // WAC_CHANGEWINDOWBOX
  WAT_WINDOWBOXLEFT     = WAT_BASE + 5;
  WAT_WINDOWBOXTOP      = WAT_BASE + 6;
  WAT_WINDOWBOXWIDTH    = WAT_BASE + 7;
  WAT_WINDOWBOXHEIGHT   = WAT_BASE + 8;
  // WAC_MOVEWINDOWINFRONTOF
  WAT_MOVEWBEHINDWINDOW = WAT_BASE + 9;

{$ifndef INTUI_V36_NAMES_ONLY}

//* Gadget Type names: */

  GTYPEMASK    = GTYP_GTYPEMASK;
  CUSTOMGADGET = GTYP_CUSTOMGADGET;
  STRGADGET    = GTYP_STRGADGET;
  PROPGADGET   = GTYP_PROPGADGET;
  GADGET0002   = GTYP_GADGET0002;
  BOOLGADGET   = GTYP_BOOLGADGET;
  IntuiCLOSE   = GTYP_CLOSE;
  //SDOWNBACK    = GTYP_SDOWNBACK; // not in the official AROS includes
  //WDOWNBACK    = GTYP_WDOWNBACK;
  //SUPFRONT     = GTYP_SUPFRONT;
  //WUPFRONT     = GTYP_WUPFRONT;
  SDRAGGING    = GTYP_SDRAGGING;
  WDRAGGING    = GTYP_WDRAGGING;
  SIZING       = GTYP_SIZING;
  REQGADGET    = GTYP_REQGADGET;
  GZZGADGET    = GTYP_GZZGADGET;
  SCRGADGET    = GTYP_SCRGADGET;
  SYSGADGET    = GTYP_SYSGADGET;
  GADGETTYPE   = GTYP_GADGETTYPE;
// Gadget Flags names:
  LABELIMAGE   = GFLG_LABELIMAGE;
  LABELSTRING  = GFLG_LABELSTRING;
  LABELITEXT   = GFLG_LABELITEXT;
  LABELMASK    = GFLG_LABELMASK;
  GADGDISABLED = GFLG_DISABLED;
  SELECTED     = GFLG_SELECTED;
  GRELHEIGHT   = GFLG_RELHEIGHT;
  GRELWIDTH    = GFLG_RELWIDTH;
  GRELRIGHT    = GFLG_RELRIGHT;
  GRELBOTTOM   = GFLG_RELBOTTOM;
  GADGIMAGE    = GFLG_GADGIMAGE;
  GADGHNONE    = GFLG_GADGHNONE;
  GADGHIMAGE   = GFLG_GADGHIMAGE;
  GADGHBOX     = GFLG_GADGHBOX;
  GADGHCOMP    = GFLG_GADGHCOMP;
  GADGHIGHBITS = GFLG_GADGHIGHBITS;
// Gadget Activation flag names:
  ACTIVEGADGET  = GACT_ACTIVEGADGET;
  STRINGEXTEND  = GACT_STRINGEXTEND;
  ALTKEYMAP     = GACT_ALTKEYMAP;
  IntuiLONGINT  = GACT_LONGINT;
  STRINGRIGHT   = GACT_STRINGRIGHT;
  STRINGCENTER  = GACT_STRINGCENTER;
  STRINGLEFT    = GACT_STRINGLEFT;
  BOOLEXTEND    = GACT_BOOLEXTEND;
  TOGGLESELECT  = GACT_TOGGLESELECT;
  BORDERSNIFF   = GACT_BORDERSNIFF;
  BOTTOMBORDER  = GACT_BOTTOMBORDER;
  TOPBORDER     = GACT_TOPBORDER;
  LEFTBORDER    = GACT_LEFTBORDER;
  RIGHTBORDER   = GACT_RIGHTBORDER;
  FOLLOWMOUSE   = GACT_FOLLOWMOUSE;
  ENDGADGET     = GACT_ENDGADGET;
  GADGIMMEDIATE = GACT_IMMEDIATE;
  RELVERIFY     = GACT_RELVERIFY;
// Window Flags names:
  HASZOOM       = WFLG_HASZOOM;
  ZOOMED        = WFLG_ZOOMED;
  VISITOR       = WFLG_VISITOR;
  NW_EXTENDED   = WFLG_NW_EXTENDED;
  WINDOWTICKED  = WFLG_WINDOWTICKED;
  WBENCHWINDOW  = WFLG_WBENCHWINDOW;
  WINDOWREFRESH = WFLG_WINDOWREFRESH;
  NOCAREREFRESH = WFLG_NOCAREREFRESH;
  RMBTRAP       = WFLG_RMBTRAP;
  MENUSTATE     = WFLG_MENUSTATE;
  INREQUEST     = WFLG_INREQUEST;
  WINDOWACTIVE  = WFLG_WINDOWACTIVE;
  ACTIVATE      = WFLG_ACTIVATE;
  BORDERLESS    = WFLG_BORDERLESS;
  GIMMEZEROZERO = WFLG_GIMMEZEROZERO;
  IntuiREPORTMOUSE   = WFLG_REPORTMOUSE;
  BACKDROP      = WFLG_BACKDROP;
  OTHER_REFRESH = WFLG_OTHER_REFRESH;
  SUPER_BITMAP  = WFLG_SUPER_BITMAP;
  SIMPLE_REFRESH= WFLG_SIMPLE_REFRESH;
  SMART_REFRESH = WFLG_SMART_REFRESH;
  REFRESHBITS   = WFLG_REFRESHBITS;
  SIZEBBOTTOM   = WFLG_SIZEBBOTTOM;
  SIZEBRIGHT    = WFLG_SIZEBRIGHT;
  WINDOWCLOSE   = WFLG_CLOSEGADGET;
  WINDOWDEPTH   = WFLG_DEPTHGADGET;
  WINDOWDRAG    = WFLG_DRAGBAR;
  WINDOWSIZING  = WFLG_SIZEGADGET;
// IDCMP class names: 
  LONELYMESSAGE  = IDCMP_LONELYMESSAGE;
  CHANGEWINDOW   = IDCMP_CHANGEWINDOW;
  MENUHELP       = IDCMP_MENUHELP;
  IDCMPUPDATE    = IDCMP_IDCMPUPDATE;
  INTUITICKS     = IDCMP_INTUITICKS;
  VANILLAKEY     = IDCMP_VANILLAKEY;
  DELTAMOVE      = IDCMP_DELTAMOVE;
  INACTIVEWINDOW = IDCMP_INACTIVEWINDOW;
  ACTIVEWINDOW   = IDCMP_ACTIVEWINDOW;
  WBENCHMESSAGE  = IDCMP_WBENCHMESSAGE;
  DISKREMOVED    = IDCMP_DISKREMOVED;
  DISKINSERTED   = IDCMP_DISKINSERTED;
  NEWPREFS       = IDCMP_NEWPREFS;
  MENUVERIFY     = IDCMP_MENUVERIFY;
  REQCLEAR       = IDCMP_REQCLEAR;
  REQVERIFY      = IDCMP_REQVERIFY;
  RAWKEY         = IDCMP_RAWKEY;
  IntuiCLOSEWINDOW = IDCMP_CLOSEWINDOW;
  MENUPICK       = IDCMP_MENUPICK;
  REQSET         = IDCMP_REQSET;
  GADGETUP       = IDCMP_GADGETUP;
  GADGETDOWN     = IDCMP_GADGETDOWN;
  MOUSEMOVE      = IDCMP_MOUSEMOVE;
  MOUSEBUTTONS   = IDCMP_MOUSEBUTTONS;
  REFRESHWINDOW  = IDCMP_REFRESHWINDOW;
  NEWSIZE        = IDCMP_NEWSIZE;
  SIZEVERIFY     = IDCMP_SIZEVERIFY;
{$endif} // not INTUI_V36_NAMES_ONLY

// Tags for GetMonitorList
const
  GMLA_Dummy     = TAG_USER + $4000;
  GMLA_DisplayID = GMLA_Dummy + 1;

var
  IntuitionBase: PIntuitionBase;


function ActivateGadget(Gadget: PGadget; Window: PWindow; Requester: PRequester): LongBool; syscall IntuitionBase 77;
procedure ActivateWindow(Window: PWindow); syscall IntuitionBase 75;
procedure AddClass(ClassPtr: PIClass); syscall IntuitionBase 114;
function AddGadget(Window: PWindow; Gadget: PGadget; Position: LongWord): Word; syscall IntuitionBase 7;
function AddGList(Window: PWindow; Gadget: PGadget; Position: LongWord; NumGad: LongInt; Requester: PRequester): Word; syscall IntuitionBase 73;
function AllocIntuiMessage(Window: PWindow): PIntuiMessage; syscall IntuitionBase 148;
function AllocRemember(var RememberKey: PRemember; Size: LongWord; Flags: LongWord): APTR; syscall IntuitionBase 66;
function AllocScreenBuffer(Screen: PScreen; Bitmap: PBitMap; Flags: LongWord): PScreenBuffer; syscall IntuitionBase 128;
procedure AlohaWorkbench(MsgPort: PMsgPort); syscall IntuitionBase 67;
function AutoRequest(Window: PWindow; Body: PIntuiText; PosText: PIntuiText; NegText: PIntuiText; PFlag: LongWord; NFlag: LongWord; Width: LongWord; Height: LongWord): LongBool; syscall IntuitionBase 58;
procedure BeginRefresh(Window: PWindow); syscall IntuitionBase 59;
function BuildEasyRequestArgs(Window: PWindow; EasyStruct: PEasyStruct; IDCMP: LongWord; Args: APTR): PWindow; syscall IntuitionBase 99;
function BuildSysRequest(Window: PWindow; Body: PIntuiText; PosText: PIntuiText; NegText: PIntuiText; Flags: LongWord; Width: LongWord; Height: LongWord): PWindow; syscall IntuitionBase 60;
function ChangeScreenBuffer(Screen: PScreen; ScreenBuffer: PScreenBuffer): LongWord; syscall IntuitionBase 130;
procedure ChangeWindowBox(Window: PWindow; Left: LongInt; Top: LongInt; Width: LongInt; Height: LongInt); syscall IntuitionBase 81;
function ClearDMRequest(Window: PWindow): LongBool; syscall IntuitionBase 8;
procedure ClearMenuStrip(Window: PWindow); syscall IntuitionBase 9;
procedure ClearPointer(Window: PWindow); syscall IntuitionBase 10;
function CloseScreen(Screen: PScreen): LongBool; syscall IntuitionBase 11;
procedure CloseWindow(Window: PWindow); syscall IntuitionBase 12;
function CloseWorkBench: LongInt; syscall IntuitionBase 13;
procedure ChangeDecoration(ID: LongWord; Decor: PNewDecorator); syscall IntuitionBase 153;
function ChangeWindowShape(Window: PWindow; NewShape: PRegion; CallBack: PHook): PRegion; syscall IntuitionBase 143; unimplemented;
procedure CurrentTime(var Seconds: LongWord; var Micros: LongWord); syscall IntuitionBase 14;
function DisplayAlert(AlertNumber: LongWord; String_: PChar; Height: Word): LongBool; syscall IntuitionBase 15; deprecated;
procedure DisplayBeep(Screen: PScreen); syscall IntuitionBase 16;
procedure DisposeObject(Object_: APTR); syscall IntuitionBase 107;
function DoGadgetMethodA(Gad: PGadget; Win: PWindow; Req: PRequester; Msg: TMsg): IPTR; syscall IntuitionBase 135;
function DoNotify(Cl: PIClass; O: PObject_; Ic: Pointer; Msg: TopUpdate): Pointer; syscall IntuitionBase 145;
function DoubleClick(SSeconds: LongWord; SMicros: LongWord; CSeconds: LongWord; CMicros: LongWord): LongBool; syscall IntuitionBase 17;
procedure DrawBorder(Rp: PRastPort; Border: PBorder; LeftOffset: LongInt; TopOffset: LongInt); syscall IntuitionBase 18;
procedure DrawImage(Rp: PRastPort; Image: PImage; LeftOffset: LongInt; TopOffset: LongInt); syscall IntuitionBase 19;
procedure DrawImageState(Rp: PRastPort; Image: PImage; LeftOffset: LongInt; TopOffset: LongInt; state: LongWord; DrawInfo: PDrawInfo); syscall IntuitionBase 103;
function EasyRequestArgs(Window: PWindow; EasyStruct: PEasyStruct; IDCMP_Ptr: PLongWord; Args: APTR): LongInt; syscall IntuitionBase 98;
procedure EndRefresh(Window: PWindow; Complete: LongBool); syscall IntuitionBase 61;
procedure EndRequest(Requester: PRequester; Window: PWindow); syscall IntuitionBase 20;
function EndScreenNotify(Notify: IPTR): LongBool; syscall IntuitionBase 162;
procedure EraseImage(Rp: PRastPort; Image: PImage; LeftOffset: LongInt; TopOffset: LongInt); syscall IntuitionBase 105;
function FindClass(ClassID: ClassID): PIClass; syscall IntuitionBase 112;
function FreeClass(IClass: PIClass): LongBool; syscall IntuitionBase 119;
procedure FreeICData(ICData: Pointer); syscall IntuitionBase 146;
procedure FreeIntuiMessage(IMsg: PIntuiMessage); syscall IntuitionBase 149;
procedure FreeMonitorList(Obj: PPObject_); syscall IntuitionBase 164;
procedure FreeRemember(var RememberKey: PRemember; ReallyForget: LongInt); syscall IntuitionBase 68;
procedure FreeScreenBuffer(Screen: PScreen; ScreenBuffer: PScreenBuffer); syscall IntuitionBase 129;
procedure FreeScreenDrawInfo(Screen: PScreen; DrawInfo: PDrawInfo); syscall IntuitionBase 116;
procedure FreeSysRequest(Window: PWindow); syscall IntuitionBase 62;
procedure GadgetMouse(Gadget: PGadget; GInfo: PGadgetInfo; var MousePoint: SmallInt); syscall IntuitionBase 95;
function GetAttr(AttrID: LongWord; Object_: PObject_; StoragePtr: PIPTR): LongWord; syscall IntuitionBase 109;
function GetDefaultPubScreen(NameBuffer: PChar): PScreen; syscall IntuitionBase 97;
function GetDefPrefs(Preferences: PPreferences; Size: SmallInt): PPreferences; syscall IntuitionBase 21;
function GetMonitorList(Tags: PTagItem): PPObject_; syscall IntuitionBase 163; 
function GetPrefs(Preferences: PPreferences; Size: SmallInt): PPreferences; syscall IntuitionBase 22;
function GetScreenData(Buffer: APTR; Size: LongWord; Type_: LongWord; Screen: PScreen): LongInt; syscall IntuitionBase 71;
function GetScreenDrawInfo(Screen: PScreen): PDrawInfo; syscall IntuitionBase 115;
procedure HelpControl(Window: PWindow; Flags: LongWord); syscall IntuitionBase 138;
procedure HideWindow(Window: PWindow); syscall IntuitionBase 141;
procedure InitRequester(Requester: PRequester); syscall IntuitionBase 23; deprecated;
function IntuiTextLength(iText: PIntuiText): LongInt; syscall IntuitionBase 55;
function ItemAddress(MenuStrip: PMenu; MenuNumber: Word): PMenuItem; syscall IntuitionBase 24;
function IsWindowVisible(Window: PWindow): LongWord; syscall IntuitionBase 139;
procedure LendMenus(FromWindow: PWindow; ToWindow: PWindow); syscall IntuitionBase 134;
function LockIBase(LockNumber: LongWord): LongWord; syscall IntuitionBase 69;
function LockPubScreen(const Name: STRPTR): PScreen; syscall IntuitionBase 85;
function LockPubScreenList: PList; syscall IntuitionBase 87;
function MakeClass(ClassID: ClassID; SuperClassID: ClassID; SuperClassPtr: PIClass; InstanceSize: LongWord; Flags: LongWord): PIClass; syscall IntuitionBase 113;
function MakeScreen(Screen: PScreen): LongInt; syscall IntuitionBase 63;
function ModifyIDCMP(Window: PWindow; Flags: LongWord): LongBool; syscall IntuitionBase 25;
procedure ModifyProp(Gadget: PGadget; Window: PWindow; Requester: PRequester; Flags: LongWord; HorizPot: LongWord; VertPot: LongWord; HorizBody: LongWord; VertBody: LongWord); syscall IntuitionBase 26;
procedure MoveScreen(Screen: PScreen; Dx: LongInt; Dy: LongInt); syscall IntuitionBase 27;
procedure MoveWindow(Window: PWindow; Dx: LongInt; Dy: LongInt); syscall IntuitionBase 28;
procedure MoveWindowInFrontOf(Window: PWindow; BehindWindow: PWindow); syscall IntuitionBase 80;
procedure NewModifyProp(Gadget: PGadget; Window: PWindow; Requester: PRequester; Flags: LongWord; HorizPot: LongWord; VertPot: LongWord; HorizBody: LongWord; VertBody: LongWord; NumGad: LongInt); syscall IntuitionBase 78;
function NewObjectA(ClassPtr: PIClass; ClassID: PChar; TagList: PTagItem): APTR; syscall IntuitionBase 106;
function NextObject(ObjectPtrPtr: APTR): APTR; syscall IntuitionBase  111;
function NextPubScreen(Screen: PScreen; Namebuf: PChar): PChar; syscall IntuitionBase 89;
function ObtainGIRPort(GInfo: PGadgetInfo): PRastPort; syscall IntuitionBase 93;
procedure OffGadget(Gadget: PGadget; Window: PWindow; Requester: PRequester); syscall IntuitionBase 29;
procedure OffMenu(Window: PWindow; MenuNumber: Word); syscall IntuitionBase 30;
procedure OnGadget(Gadget: PGadget; Window: PWindow; Requester: PRequester); syscall IntuitionBase 31;
procedure OnMenu(Window: PWindow; MenuNumber: Word); syscall IntuitionBase 32;
function OpenScreen(NewScreen: PNewScreen): PScreen; syscall IntuitionBase 33;
function OpenScreenTagList(NewScreen: PNewScreen; TagList: PTagItem): PScreen; syscall IntuitionBase 102;
function OpenWindow(NewWindow: PNewWindow): PWindow; syscall IntuitionBase 34;
function OpenWindowTagList(NewWindow: PNewWindow; TagList: PTagItem): PWindow; syscall IntuitionBase 101;
function OpenWorkBench: IPTR; syscall IntuitionBase 35;
function PointInImage(Point: LongWord; Image: PImage): LongBool; syscall IntuitionBase 104;
procedure PrintIText(Rp: PRastPort; IText: PIntuiText; Left: LongInt; Top: LongInt); syscall IntuitionBase 36;
function PubScreenStatus(Screen: PScreen; StatusFlags: Word): Word; syscall IntuitionBase 92;
function QueryOverscan(DisplayID: LongWord; Rect: PRectangle; OScanType: SmallInt): LongInt; syscall IntuitionBase 79;
procedure RefreshGadgets(Gadgets: PGadget; Window: PWindow; Requester: PRequester); syscall IntuitionBase 37;
procedure RefreshGList(Gadgets: PGadget; Window: PWindow; Requester: PRequester; NumGad: LongInt); syscall IntuitionBase 72;
procedure RefreshWindowFrame(Window: PWindow); syscall IntuitionBase 76;
procedure ReleaseGIRPort(Rp: PRastPort); syscall IntuitionBase 94;
function RemakeDisplay: LongInt; syscall IntuitionBase 64;
procedure RemoveClass(ClassPtr: PIClass); syscall IntuitionBase 118;
function RemoveGadget(Window: PWindow; Gadget: PGadget): Word; syscall IntuitionBase 38;
function RemoveGList(RemPtr: PWindow; Gadget: PGadget; NumGad: LongInt): Word; syscall IntuitionBase 74;
procedure ReportMouse(Flag: LongInt; Window: PWindow); syscall IntuitionBase 39;
function Request(Requester: PRequester; Window: PWindow): LongBool; syscall IntuitionBase 40;
function ResetMenuStrip(Window: PWindow; Menu: PMenu): LongBool; syscall IntuitionBase 117;
function RethinkDisplay: LongInt; syscall IntuitionBase 65;
procedure ScreenDepth(Screen: PScreen; Flags: LongWord; Reserved: APTR); syscall IntuitionBase 131;
procedure ScreenPosition(Screen: PScreen; Flags: LongWord; X1: LongInt; Y1: LongInt; X2: LongInt; Y2: LongInt); syscall IntuitionBase 132;
procedure ScreenToBack(Screen: PScreen); syscall IntuitionBase 41;
procedure ScreenToFront(Screen: PScreen); syscall IntuitionBase 42;
procedure ScrollWindowRaster(Win: PWindow; Dx: SmallInt; Dy: SmallInt; XMin: SmallInt; YMin: SmallInt; XMax: SmallInt; YMax: SmallInt); syscall IntuitionBase 133;
procedure ScrollWindowRasterNoFill(Window: PWindow; Dx, Dy, XMin, YMin, XMax, YMax: Word); syscall IntuitionBase 159;
procedure SendIntuiMessage(Window: PWindow; IMsg: PIntuiMessage); syscall IntuitionBase 151;
function SetAttrsA(Object_: APTR; TagList: PTagItem): IPTR; syscall IntuitionBase 108;
procedure SetDefaultPubScreen(Name: PChar); syscall IntuitionBase 90;
procedure SetDefaultScreenFont(TextFont: PTextFont); syscall IntuitionBase 144;
function SetDMRequest(Window: PWindow; Requester: PRequester): LongBool; syscall IntuitionBase 43;
function SetEditHook(Hook: PHook): PHook; syscall IntuitionBase 82;
function SetGadgetAttrsA(Gadget: PGadget; Window: PWindow; Requester: PRequester; TagList: PTagItem): IPTR; syscall IntuitionBase 110;
function SetIPrefs(Data: Pointer; Length: LongWord; Typ: LongWord): LongWord; syscall IntuitionBase 96;
function SetMenuStrip(Window: PWindow; Menu: PMenu): LongBool; syscall IntuitionBase 44;
function SetMouseQueue(Window: PWindow; QueueLength: LongWord): LongInt; syscall IntuitionBase 83;
procedure SetPointer(Window: PWindow; Pointer_: PWord; Height: LongInt; Width: LongInt; XOffset: LongInt; YOffset: LongInt); syscall IntuitionBase 45;
function SetPointerBounds(Screen: PScreen; Rect: TRectangle; Reserved: LongWord; Tags: PTagItem): LongWord; syscall IntuitionBase 160;
function SetPrefs(PrefBuffer: PPreferences; Size: LongInt; Inform: LongBool): PPreferences; syscall IntuitionBase 54;
function SetPubScreenModes(Modes: Word): Word; syscall IntuitionBase 91;
procedure SetWindowPointerA(Win: PWindow; Taglist: PTagItem); syscall IntuitionBase 136;
procedure SetWindowTitles(Window: PWindow; const WindowTitle: PChar; const ScreenTitle: PChar); syscall IntuitionBase 46;
procedure ShowTitle(Screen: PScreen; ShowIt: LongBool); syscall IntuitionBase 47;
procedure ShowWindow(Window: PWindow); syscall IntuitionBase 140;
procedure SizeWindow(Window: PWindow; Dx: LongInt; Dy: LongInt); syscall IntuitionBase 48;
function StartScreenNotifyTagList(Tags: PTagItem): IPTR; syscall IntuitionBase 161;
function SysReqHandler(Window: PWindow; IDCMPFlagsPtr: PLongWord; WaitInput: LongBool): LongInt; syscall IntuitionBase 100;
function TimedDisplayAlert(AlertNumber: LongWord; String_: PChar; Height: Word; Time: LongWord): LongBool; syscall IntuitionBase 137;
procedure UnlockIBase(LockNumber: LongWord); syscall IntuitionBase 70;
procedure UnlockPubScreen(Name: PChar; Screen: PScreen); syscall IntuitionBase 86;
procedure UnlockPubScreenList; syscall IntuitionBase 88;
function ViewAddress: PView; syscall IntuitionBase 49;
function ViewPortAddress(Window: PWindow): PViewPort; syscall IntuitionBase 50;
function WBenchToBack: LongBool; syscall IntuitionBase 56;
function WBenchToFront: LongBool; syscall IntuitionBase 57;
procedure WindowAction(Window: PWindow; Action: LongWord; Tags: PTagItem); syscall IntuitionBase 157;
function WindowLimits(Window: PWindow; WidthMin: SmallInt; HeightMin: SmallInt; WidthMax: Word; HeightMax: Word): LongBool; syscall IntuitionBase 53;
procedure WindowToBack(Window: PWindow); syscall IntuitionBase 51;
procedure WindowToFront(Window: PWindow); syscall IntuitionBase 52;
procedure ZipWindow(Window: PWindow); syscall IntuitionBase 84;

// VarArgs Versions
function SetAttrs(Obj: APTR; const Tags: array of const): LongWord;
function NewObject(ClassPtr: PIClass; ClassID: PChar; const Tags: array of const): APTR;
function BuildEasyRequest(Window: PWindow; EasyStruct: PEasyStruct; IDCMP: LongWord; const Args: array of const): PWindow;
function DoGadgetMethod(Gad: PGadget; Win: PWindow; Req: PRequester; const Args: array of const): IPTR;
function EasyRequest(Window: PWindow; EasyStruct: PEasyStruct; IDCMP_Ptr: PLongWord; const Args: array of const): LongInt; 
function OpenScreenTags(NewScreen: PNewScreen; const Tags: array of const): PScreen;
function OpenWindowTags(NewWindow: PNewWindow; const Tags: array of const): PWindow;
function SetGadgetAttrs(Gadget: PGadget; Window: PWindow; Requester: PRequester; const Tags: array of const): IPTR;
procedure SetWindowPointer(Win: PWindow; const Tags: array of const);

// Function wrapper
function SetSuperAttrsA(cl: PIClass; Obj: PObject_; TagList: PTagItem): IPTR;
function SetSuperAttrs(cl: PIClass; Obj: PObject_; Tags: array of const): IPTR;
function DoMethodA(Obj: PObject_; Message: APTR): IPTR;
function DoMethod(Obj: PObject_; MethodID: LongWord; Args: array of const): IPTR;
function CoerceMethodA(cl: PIClass; Obj: PObject_; Message: APTR): IPTR;
function CoerceMethod(cl: PIClass; Obj: PObject_; MethodID: LongWord; const Args: array of const): IPTR;
function DoSuperMethodA(cl: PIClass; Obj: PObject_; Message: APTR): IPTR;
function DoSuperMethod(cl: PIClass; Obj: PObject_; Args: array of const): IPTR;

function Has_Children(Win: PWindow): Boolean;
function Is_Children(Win: PWindow): Boolean;

{ Intuition macros }

function INST_DATA(Cl: PIClass; O: P_Object): Pointer;
function SIZEOF_INSTANCE(Cl: PIClass): LongInt;
function BASEOBJECT(O: P_Object): Pointer;
function _OBJ(O: Pointer): P_Object;
function __OBJECT(O: Pointer): P_Object;
function OCLASS(O: Pointer): PIClass;
function SHIFTITEM(N: SmallInt): Word;
function SHIFTMENU(N: SmallInt): Word;
function SHIFTSUB(N: SmallInt): Word;
function FULLMENUNUM(Menu, Item, Sub: SmallInt): Word;
function IM_BGPEN(Im: PImage): Byte;
function IM_BOX(Im: PImage): PIBox;
function IM_FGPEN(Im: PImage): Byte;
function GADGET_BOX(G: PGadget): PIBox;
function CUSTOM_HOOK(Gadget: PGadget): PHook;
function ITEMNUM(N: Word): Word;
function MENUNUM(N: Word): Word;
function SUBNUM(N: Word): Word;
function IAM_Resolution(x, y: Word): LongWord; // Pack two Words into one LongWord
function SRBNUM(x: Word): Word;
function SWBNUM(x: Word): Word;
function SSBNUM(x: Word): Word;
function SPARNUM(x: Word): Word;
function SHAKNUM(x: Word): Word;

implementation

uses
  tagsarray, longarray;
  
function SetAttrs(Obj: APTR; const Tags: array of const): LongWord;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := SetAttrsA(Obj, GetTagPtr(TagList));
end;

function NewObject(ClassPtr: PIClass; ClassID: PChar; const Tags: array of const): APTR;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := NewObjectA(ClassPtr, ClassID, GetTagPtr(TagList));
end;

function BuildEasyRequest(Window: PWindow; EasyStruct: PEasyStruct; IDCMP: LongWord; const Args: array of const): PWindow;
var
  ArgList: TArgList;
begin
  AddArguments(ArgList, Args);
  Result := BuildEasyRequestArgs(Window, EasyStruct, IDCMP, GetArgPtr(ArgList));
end;

function DoGadgetMethod(Gad: PGadget; Win: PWindow; Req: PRequester; const Args: array of const): IPTR;
var
  ArgList: TArgList;
begin
  AddArguments(ArgList, Args);
  Result := DoGadgetMethodA(Gad, Win, Req, TMsg(ArgList));
end;

function EasyRequest(Window: PWindow; EasyStruct: PEasyStruct; IDCMP_Ptr: PLongWord; const Args: array of const): LongInt;
var
  ArgList: TArgList;
begin
  AddArguments(ArgList, Args);
  Result := EasyRequestArgs(Window, EasyStruct, IDCMP_Ptr, @(ArgList[0])); 
end;

function OpenScreenTags(NewScreen: PNewScreen; const Tags: array of const): PScreen;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := OpenScreenTagList(NewScreen, GetTagPtr(TagList));
end;

function OpenWindowTags(NewWindow: PNewWindow; const Tags: array of const): PWindow;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := OpenWindowTagList(NewWindow, GetTagPtr(TagList));
end;

function SetGadgetAttrs(Gadget: PGadget; Window: PWindow; Requester: PRequester; const Tags: array of const): IPTR;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := SetGadgetAttrsA(Gadget, Window, Requester, GetTagPtr(TagList));
end;

procedure SetWindowPointer(Win: PWindow; const Tags: array of const);
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  SetWindowPointerA(Win, GetTagPtr(TagList));
end;

// Functions wrapper

function DoMethodA(Obj: PObject_; Message: APTR): IPTR;
begin
  Result := 0;
  if Obj = nil then
    Exit;
  Result := CALLHOOKPKT_(PHook(OCLASS(Obj)), Obj, Message);
end;

function DoMethod(Obj: PObject_; MethodID: LongWord; Args: array of const): IPTR;
var
  ArgList: TArgList;
begin
  Result := 0;
  if obj = nil then
    Exit;
  AddArguments(ArgList, [MethodID]);  
  AddArguments(ArgList, Args);  
  Result := CALLHOOKPKT_(PHook(OCLASS(Obj)), Obj, @(ArgList[0]));
end;

function DoSuperMethodA(cl: PIClass; Obj: PObject_; Message: APTR): IPTR;
begin
  Result := 0;
  if (cl = nil) or (obj = nil) then
    Exit;
  Result := CALLHOOKPKT_(PHook(cl^.cl_Super), Obj, Message);
end;

function DoSuperMethod(cl: PIClass; Obj: PObject_; Args: array of const): IPTR;
var
  ArgList: TArgList;
begin
  Result := 0;
  if (cl = nil) or (obj = nil) then
    Exit;
  AddArguments(ArgList, Args);  
  Result := CALLHOOKPKT_(PHook(cl^.cl_Super), Obj, @(ArgList[0]));
end;

function CoerceMethodA(cl: PIClass; Obj: PObject_; Message: APTR): IPTR;
begin
  Result := 0;
  if (cl = nil) or (obj = nil) then
    Exit;
  Result := CALLHOOKPKT_(PHook(cl), Obj, Message);
end;

function CoerceMethod(cl: PIClass; Obj: PObject_; MethodID: LongWord; const Args: array of const): IPTR;
var
  ArgList: TArgList;
begin
  AddArguments(ArgList,[MethodID]);
  AddArguments(ArgList, Args);
  Result := CoerceMethodA(cl, Obj, @(ArgList[0])); 
end;


function SetSuperAttrs(cl: PIClass; Obj: PObject_; Tags: array of const): IPTR;
var
  TagList: TTagsList;
  ops: TopSet;
begin
  AddTags(TagList, Tags);
  ops.MethodID := OM_SET;
  ops.ops_AttrList := GetTagPtr(TagList);
  ops.ops_GInfo := nil;
  Result := DoSuperMethodA(cl, obj, @ops);
end;

function SetSuperAttrsA(cl: PIClass; Obj: PObject_; TagList: PTagItem): IPTR;
var
  ops: TopSet;
begin
  ops.MethodID := OM_SET;
  ops.ops_AttrList := TagList;
  ops.ops_GInfo := nil;
  Result := DoSuperMethodA(cl, obj, @ops);
end;


function INST_DATA(Cl: PIClass; O: P_Object): Pointer;
begin
  INST_DATA := Pointer(PtrUInt(O) + Cl^.cl_InstOffset);
end;

function SIZEOF_INSTANCE(Cl: PIClass): LongInt;
begin
  SIZEOF_INSTANCE := Cl^.cl_InstOffset + Cl^.cl_InstSize + SizeOf(T_Object);
end;

function BASEOBJECT(O: P_Object): Pointer;
begin
  BASEOBJECT := Pointer(PtrUInt(O) + SizeOf(T_Object));
end;

function _OBJ(O: Pointer): P_Object;
begin
 _OBJ := P_Object(O);
end;

function __OBJECT(O: Pointer): P_Object;
begin
  __OBJECT := P_Object(PtrUInt(O) - SizeOf(T_Object))
end;

function OCLASS(O: Pointer): PIClass;
var
  Obj: P_Object;
begin
  Obj := P_Object(PtrUInt(O) - SizeOf(T_Object));
  OCLASS := Obj^.o_Class;
end;

function SHIFTITEM(N: SmallInt): Word;
begin
  SHIFTITEM := (N and $3f) shl 5
end;

function SHIFTMENU(N: SmallInt): Word;
begin
  SHIFTMENU := N and $1f
end;

function SHIFTSUB(N: SmallInt): Word;
begin
  SHIFTSUB := (N and $1f) shl 11
end;

function FULLMENUNUM(Menu, Item, Sub: SmallInt): Word;
begin
  FULLMENUNUM := ((Sub and $1f) shl 11) or ((Item and $3f) shl 5) or (Menu and $1f);
end;

{ The next functons _BGPEN AND _FGPEN aren't a full replacement of the
  C macros because the C preprocessor makes it possible to set the
  A/BPen values of the Image class objects as well. This can't work
  in pascal, of course!
}

function IM_BGPEN(Im: PImage): Byte;
begin
  IM_BGPEN := Im^.PlaneOnOff;
end;

function IM_BOX(Im: PImage): PIBox;
begin
  IM_BOX := PIBox(@Im^.LeftEdge);
END;

function IM_FGPEN (Im: PImage): Byte;
begin
  IM_FGPEN := Im^.PlanePick;
end;

function GADGET_BOX(G: PGadget): PIBox;
begin
  GADGET_BOX := PIBox(@G^.LeftEdge);
end;

function CUSTOM_HOOK (Gadget: PGadget): PHook;
begin
    CUSTOM_HOOK := PHook(Gadget^.MutualExclude);
end;

function ITEMNUM(N: Word): Word;
begin
  ITEMNUM := (N shr 5) and $3F
end;

function MENUNUM(N: Word): Word;
begin
 MENUNUM := N and $1f
end;

function SUBNUM(N: Word): Word;
begin
  SUBNUM := (N shr 11) and $1f
end;

function IAM_Resolution(x, y: Word): LongWord;
begin
  Result := (x shl 16) or y;
end;

function SRBNUM(x: Word): Word;
begin
  SRBNUM := $08 - (x shr 4);
end;

function SWBNUM(x: Word): Word;
begin
  SWBNUM := $08 - (x and $0f);
end;

function SSBNUM(x: Word): Word;
begin
  SSBNUM := $01 + (x shr 4);
end;

function SPARNUM(x: Word): Word;
begin
  SPARNUM := x shr 4;
end;

function SHAKNUM(x: Word): Word;
begin
  SHAKNUM := x and $0f;
end;

function Has_Children(Win: PWindow): Boolean;
begin
  Result := Assigned(Win^.FirstChild);
end;

function Is_Children(Win: PWindow): Boolean;
begin
  Result := Assigned(Win^.Parent2);
end;

initialization
  IntuitionBase := PIntuitionBase(OpenLibrary('intuition.library', 36));

finalization
  CloseLibrary(PLibrary(IntuitionBase));

end.
