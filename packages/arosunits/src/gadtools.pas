{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    gadtools.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit gadtools;

interface

uses
  exec, intuition, agraphics, utility;

{------------------------------------------------------------------------}

{  The kinds (almost classes) of gadgets in the toolkit.  Use these
    identifiers when calling CreateGadgetA() }

CONST
 GENERIC_KIND  =  0;
 BUTTON_KIND   =  1;
 CHECKBOX_KIND =  2;
 INTEGER_KIND  =  3;
 LISTVIEW_KIND =  4;
 MX_KIND       =  5;
 NUMBER_KIND   =  6;
 CYCLE_KIND    =  7;
 PALETTE_KIND  =  8;
 SCROLLER_KIND =  9;
{ Kind number 10 is reserved }
 SLIDER_KIND   =  11;
 STRING_KIND   =  12;
 TEXT_KIND     =  13;

 NUM_KINDS     =  14;

 GADTOOLSNAME   : PChar = 'gadtools.library';


{------------------------------------------------------------------------}

{  'Or' the appropriate set together for your Window IDCMPFlags: }

 ARROWIDCMP    =  (IDCMP_GADGETUP + IDCMP_GADGETDOWN +
                   IDCMP_INTUITICKS + IDCMP_MOUSEBUTTONS);

 BUTTONIDCMP   =  (IDCMP_GADGETUP);
 CHECKBOXIDCMP =  (IDCMP_GADGETUP);
 INTEGERIDCMP  =  (IDCMP_GADGETUP);
 LISTVIEWIDCMP =  (IDCMP_GADGETUP + IDCMP_GADGETDOWN +
                   IDCMP_MOUSEMOVE + ARROWIDCMP);

 MXIDCMP       =  (IDCMP_GADGETDOWN);
 NUMBERIDCMP   =  0;
 CYCLEIDCMP    =  (IDCMP_GADGETUP);
 PALETTEIDCMP  =  (IDCMP_GADGETUP);

{  Use ARROWIDCMP+SCROLLERIDCMP if your scrollers have arrows: }
 SCROLLERIDCMP =  (IDCMP_GADGETUP + IDCMP_GADGETDOWN + IDCMP_MOUSEMOVE);
 SLIDERIDCMP   =  (IDCMP_GADGETUP + IDCMP_GADGETDOWN + IDCMP_MOUSEMOVE);
 STRINGIDCMP   =  (IDCMP_GADGETUP);

 TEXTIDCMP     =  0;


{------------------------------------------------------------------------}

{  Generic NewGadget used by several of the gadget classes: }

Type
   PNewGadget = ^tNewGadget;
   tNewGadget = record
    ng_LeftEdge, ng_TopEdge : SmallInt;     {  gadget position }
    ng_Width, ng_Height     : SmallInt;     {  gadget size }
    ng_GadgetText           : STRPTR;      {  gadget label }
    ng_TextAttr             : PTextAttr;   {  desired font for gadget label }
    ng_GadgetID             : Word;        {  gadget ID }
    ng_Flags                : LongWord;       {  see below }
    ng_VisualInfo           : Pointer;     {  Set to retval of GetVisualInfo() }
    ng_UserData             : Pointer;     {  gadget UserData }
   END;


{  ng_Flags control certain aspects of the gadget.  The first five control
    the placement of the descriptive text.  All larger groups supply a
    default: }

CONST
 PLACETEXT_LEFT  = $0001;  { Right-align text on left side }
 PLACETEXT_RIGHT = $0002;  { Left-align text on right side }
 PLACETEXT_ABOVE = $0004;  { Center text above }
 PLACETEXT_BELOW = $0008;  { Center text below }
 PLACETEXT_IN    = $0010;  { Center text on }

 NG_HIGHLABEL    = $0020;  { Highlight the label }

{------------------------------------------------------------------------}

{ Fill out an array of these and pass that to CreateMenus(): }

Type
   pNewMenu = ^tNewMenu;
   tNewMenu = record
    nm_Type           : Byte;              {  See below }
    nm_Label          : STRPTR;            {  Menu's label }
    nm_CommKey        : STRPTR;            {  MenuItem Command Key Equiv }
    nm_Flags          : Word;              {  Menu OR MenuItem flags (see note) }
    nm_MutualExclude  : Longint;           {  MenuItem MutualExclude word }
    nm_UserData       : Pointer;           {  For your own use, see note }
   END;

const
{ Needed only by inside IM_ definitions below }
 MENU_IMAGE     = 128;

{ nm_Type determines what each NewMenu structure corresponds to.
 * for the NM_TITLE, NM_ITEM, and NM_SUB values, nm_Label should
 * be a text string to use for that menu title, item, or sub-item.
 * For IM_ITEM or IM_SUB, set nm_Label to point at the Image structure
 * you wish to use for this item or sub-item.
 * NOTE: At present, you may only use conventional images.
 * Custom images created from Intuition image-classes do not work.
 }
 NM_TITLE      =  1;       { Menu header }
 NM_ITEM       =  2;       { Textual menu item }
 NM_SUB        =  3;       { Textual menu sub-item }

 IM_ITEM       =  (NM_ITEM OR MENU_IMAGE);    { Graphical menu item }
 IM_SUB        =  (NM_SUB OR MENU_IMAGE);     { Graphical menu sub-item }

{ The NewMenu array should be terminated with a NewMenu whose
 * nm_Type equals NM_END.
 }
 NM_END        =  0;       { End of NewMenu array }

{ Starting with V39, GadTools will skip any NewMenu entries whose
 * nm_Type field has the NM_IGNORE bit set.
 }
 NM_IGNORE     =  64;


{ nm_Label should be a text string for textual items, a pointer
 * to an Image structure for graphical menu items, or the special
 * constant NM_BARLABEL, to get a separator bar.
 }
 NM_BARLABEL   = -1;

{ The nm_Flags field is used to fill out either the Menu->Flags or
 * MenuItem->Flags field.  Note that the sense of the MENUENABLED or
 * ITEMENABLED bit is inverted between this use and Intuition's use,
 * in other words, NewMenus are enabled by default.  The following
 * labels are provided to disable them:
 }
 NM_MENUDISABLED = MENUENABLED;
 NM_ITEMDISABLED = ITEMENABLED;

{ New for V39:  NM_COMMANDSTRING.  For a textual MenuItem or SubItem,
 * point nm_CommKey at an arbitrary string, and set the NM_COMMANDSTRING
 * flag.
 }
 NM_COMMANDSTRING = COMMSEQ;

{ The following are pre-cleared (COMMSEQ, ITEMTEXT, and HIGHxxx are set
 * later as appropriate):
 * Under V39, the COMMSEQ flag bit is not cleared, since it now has
 * meaning.
 }
 NM_FLAGMASK    = not (COMMSEQ or ITEMTEXT or HIGHFLAGS);
 NM_FLAGMASK_V39 = not (ITEMTEXT or HIGHFLAGS);

{ You may choose among CHECKIT, MENUTOGGLE, and CHECKED.
 * Toggle-select menuitems are of type CHECKIT|MENUTOGGLE, along
 * with CHECKED if currently selected.  Mutually exclusive ones
 * are of type CHECKIT, and possibly CHECKED too.  The nm_MutualExclude
 * is a bit-wise representation of the items excluded by this one,
 * so in the simplest case (choose 1 among n), these flags would be
 * ~1, ~2, ~4, ~8, ~16, etc.  See the Intuition Menus chapter.
 }

{ A UserData pointer can be associated with each Menu and MenuItem structure.
 * The CreateMenus() call allocates space for a UserData after each
 * Menu or MenuItem (header, item or sub-item).  You should use the
 * GTMENU_USERDATA() or GTMENUITEM_USERDATA() macro to extract it.
 }

const
{ These return codes can be obtained through the GTMN_SecondaryError tag }
 GTMENU_TRIMMED = $00000001;      { Too many menus, items, or subitems,
                                         * menu has been trimmed down
                                         }
 GTMENU_INVALID = $00000002;      { Invalid NewMenu array }
 GTMENU_NOMEM   = $00000003;      { Out of memory }

{------------------------------------------------------------------------}

{ Starting with V39, checkboxes and mx gadgets can be scaled to your
 * specified gadget width/height.  Use the new GTCB_Scaled or GTMX_Scaled
 * tags, respectively.  Under V37, and by default in V39, the imagery
 * is of the following fixed size:
 }

{ MX gadget default dimensions: }
 MX_WIDTH      =  17;
 MX_HEIGHT     =  9;

{ Checkbox default dimensions: }
 CHECKBOX_WIDTH  = 26;
 CHECKBOX_HEIGHT = 11;

{------------------------------------------------------------------------}


{------------------------------------------------------------------------}

{  Tags for GadTools functions: }
CONST
 GT_TagBase        =   TAG_USER + $80000;

 GTVI_NewWindow    =   GT_TagBase+1;  { Unused }
 GTVI_NWTags       =   GT_TagBase+2;  { Unused }

 GT_Private0       =   GT_TagBase+3;  { (private) }

 GTCB_Checked      =   GT_TagBase+4;  { State of checkbox }

 GTLV_Top          =   GT_TagBase+5;  { Top visible one in listview }
 GTLV_Labels       =   GT_TagBase+6;  { List to display in listview }
 GTLV_ReadOnly     =   GT_TagBase+7;  { TRUE IF listview is to be
                                              read-only }
 GTLV_ScrollWidth  =   GT_TagBase+8;  { Width of scrollbar }

 GTMX_Labels       =   GT_TagBase+9;  { NULL-terminated array of labels }
 GTMX_Active       =   GT_TagBase+10; { Active one in mx gadget }

 GTTX_Text         =   GT_TagBase+11; { Text to display }
 GTTX_CopyText     =   GT_TagBase+12; { Copy text label instead of
                                              referencing it }

 GTNM_Number       =   GT_TagBase+13; { Number to display }

 GTCY_Labels       =   GT_TagBase+14; { NULL-terminated array of labels }
 GTCY_Active       =   GT_TagBase+15; { The active one in the cycle gad }

 GTPA_Depth        =   GT_TagBase+16; { Number of bitplanes in palette }
 GTPA_Color        =   GT_TagBase+17; { Palette color }
 GTPA_ColorOffset  =   GT_TagBase+18; { First color to use in palette }
 GTPA_IndicatorWidth = GT_TagBase+19; { Width of current-color indicator }
 GTPA_IndicatorHeight = GT_TagBase+20; { Height of current-color indicator }

 GTSC_Top          =   GT_TagBase+21; { Top visible in scroller }
 GTSC_Total        =   GT_TagBase+22; { Total in scroller area }
 GTSC_Visible      =   GT_TagBase+23; { Number visible in scroller }
 GTSC_Overlap      =   GT_TagBase+24; { Unused }

{  GT_TagBase+25 through GT_TagBase+37 are reserved }

 GTSL_Min          =   GT_TagBase+38; { Slider min value }
 GTSL_Max          =   GT_TagBase+39; { Slider max value }
 GTSL_Level        =   GT_TagBase+40; { Slider level }
 GTSL_MaxLevelLen  =   GT_TagBase+41; { Max length of printed level }
 GTSL_LevelFormat  =   GT_TagBase+42; { Format string for level }
 GTSL_LevelPlace   =   GT_TagBase+43; { Where level should be placed }
 GTSL_DispFunc     =   GT_TagBase+44; { Callback for number calculation
                                              before display }

 GTST_String       =   GT_TagBase+45; { String gadget's displayed string }
 GTST_MaxChars     =   GT_TagBase+46; { Max length of string }

 GTIN_Number       =   GT_TagBase+47; { Number in integer gadget }
 GTIN_MaxChars     =   GT_TagBase+48; { Max number of digits }

 GTMN_TextAttr     =   GT_TagBase+49; { MenuItem font TextAttr }
 GTMN_FrontPen     =   GT_TagBase+50; { MenuItem text pen color }

 GTBB_Recessed     =   GT_TagBase+51; { Make BevelBox recessed }

 GT_VisualInfo     =   GT_TagBase+52; { result of VisualInfo call }

 GTLV_ShowSelected =   GT_TagBase+53; { show selected entry beneath
                listview, set tag data = NULL for display-only, or pointer
                to a string gadget you've created }
 GTLV_Selected     =   GT_TagBase+54; { Set ordinal number of selected
                                              entry in the list }
 GT_Reserved0      =   GT_TagBase+55; { Reserved }
 GT_Reserved1      =   GT_TagBase+56; { Reserved for future use }

 GTTX_Border       =   GT_TagBase+57; { Put a border around
                                              Text-display gadgets }
 GTNM_Border       =   GT_TagBase+58; { Put a border around
                                              Number-display gadgets }

 GTSC_Arrows       =   GT_TagBase+59; { Specify size of arrows for
                                              scroller }

 GTMN_Menu         =   GT_TagBase+60; { Pointer to Menu for use by
                                              LayoutMenuItems() }
 GTMX_Spacing      =   GT_TagBase+61; { Added to font height to
                figure spacing between mx choices.  Use this instead
                of LAYOUTA_SPACING for mx gadgets. }

{ New to V37 GadTools.  Ignored by GadTools V36 }
 GTMN_FullMenu     =   GT_TagBase+62; { Asks CreateMenus() to
                validate that this is a complete menu structure }
 GTMN_SecondaryError =  GT_TagBase+63; { ti_Data is a pointer
                to a LongWord to receive error reports from CreateMenus() }
 GT_Underscore     =   GT_TagBase+64; { ti_Data points to the symbol
                that preceeds the character you'd like to underline in a
                gadget label }

{ New to V39 GadTools.  Ignored by GadTools V36 and V37 }
 GTMN_Checkmark     =  GT_TagBase+65; { ti_Data is checkmark img to use }
 GTMN_AmigaKey      =  GT_TagBase+66; { ti_Data is Amiga-key img to use }
 GTMN_NewLookMenus  =  GT_TagBase+67; { ti_Data is boolean }

{ New to V39 GadTools.  Ignored by GadTools V36 and V37.
 * Set to TRUE if you want the checkbox or mx image scaled to
 * the gadget width/height you specify.  Defaults to FALSE,
 * for compatibility.
 }
 GTCB_Scaled         = GT_TagBase+68; { ti_Data is boolean }
 GTMX_Scaled         = GT_TagBase+69; { ti_Data is boolean }

 GTPA_NumColors      = GT_TagBase+70; { Number of colors in palette }

 GTMX_TitlePlace     = GT_TagBase+71; { Where to put the title }

 GTTX_FrontPen       = GT_TagBase+72; { Text color in TEXT_KIND gad }
 GTTX_BackPen        = GT_TagBase+73; { Bgrnd color in TEXT_KIND gad }
 GTTX_Justification  = GT_TagBase+74; { See GTJ_#? constants }

 GTNM_FrontPen       = GT_TagBase+72; { Text color in NUMBER_KIND gad }
 GTNM_BackPen        = GT_TagBase+73; { Bgrnd color in NUMBER_KIND gad }
 GTNM_Justification  = GT_TagBase+74; { See GTJ_#? constants }
 GTNM_Format         = GT_TagBase+75; { Formatting string for number }
 GTNM_MaxNumberLen   = GT_TagBase+76; { Maximum length of number }

 GTBB_FrameType      = GT_TagBase+77; { defines what kind of boxes
                                            * DrawBevelBox() renders. See
                                            * the BBFT_#? constants for
                                            * possible values
                                            }

 GTLV_MakeVisible    = GT_TagBase+78; { Make this item visible }
 GTLV_ItemHeight     = GT_TagBase+79; { Height of an individual item }

 GTSL_MaxPixelLen    = GT_TagBase+80; { Max pixel size of level display }
 GTSL_Justification  = GT_TagBase+81; { how should the level be displayed }

 GTPA_ColorTable     = GT_TagBase+82; { colors to use in palette }

 GTLV_CallBack       = GT_TagBase+83; { general-purpose listview call back }
 GTLV_MaxPen         = GT_TagBase+84; { maximum pen number used by call back }

 GTTX_Clipped        = GT_TagBase+85; { make a TEXT_KIND clip text }
 GTNM_Clipped        = GT_TagBase+85; { make a NUMBER_KIND clip text }


{------------------------------------------------------------------------}

{ Justification types for GTTX_Justification and GTNM_Justification tags }
 GTJ_LEFT   = 0;
 GTJ_RIGHT  = 1;
 GTJ_CENTER = 2;

{------------------------------------------------------------------------}

{ Bevel box frame types for GTBB_FrameType tag }
 BBFT_BUTTON      = 1;  { Standard button gadget box }
 BBFT_RIDGE       = 2;  { Standard string gadget box }
 BBFT_ICONDROPBOX = 3;  { Standard icon drop box     }

{------------------------------------------------------------------------}

{ Typical suggested spacing between "elements": }
 INTERWIDTH    =  8;
 INTERHEIGHT   =  4;

{------------------------------------------------------------------------}


{  "NWay" is an old synonym for cycle gadgets }
 NWAY_KIND    =   CYCLE_KIND;
 NWAYIDCMP    =   CYCLEIDCMP;
 GTNW_Labels  =   GTCY_Labels;
 GTNW_Active  =   GTCY_Active;

{------------------------------------------------------------------------}

{ These two definitions are obsolete, but are here for backwards
 * compatibility.  You never need to worry about these:
 }
 GADTOOLBIT    =  ($8000);
{ Use this mask to isolate the user part: }
 GADTOOLMASK   =  not (GADTOOLBIT);

{------------------------------------------------------------------------}

{ These definitions are for the GTLV_CallBack tag }

{ The different types of messages that a listview callback hook can see }
 LV_DRAW     =  $202;    { draw yourself, with state }

{ Possible return values from a callback hook }
 LVCB_OK      = 0;         { callback understands this message type    }
 LVCB_UNKNOWN = 1;         { callback does not understand this message }

{ states for LVDrawMsg.lvdm_State }
 LVR_NORMAL           = 0; { the usual                 }
 LVR_SELECTED         = 1; { for selected gadgets      }
 LVR_NORMALDISABLED   = 2;         { for disabled gadgets      }
 LVR_SELECTEDDISABLED = 8;         { disabled and selected     }

Type
{ structure of LV_DRAW messages, object is a (struct Node *) }
 pLVDrawMsg = ^tLVDrawMsg;
 tLVDrawMsg = record
    lvdm_MethodID       : LongWord;       { LV_DRAW                   }
    lvdm_RastPort       : PRastPort;   { where to render to        }
    lvdm_DrawInfo       : PDrawInfo;   { useful to have around     }
    lvdm_Bounds         : TRectangle;  { limits of where to render }
    lvdm_State          : LongWord;     { how to render     }
 end;


var
  GadToolsBase: PLibrary;

function CreateContext(GListPtr: PGadget): PGadget; syscall GadToolsBase 19;
function CreateGadgetA(Kind: LongWord; Gad: PGadget; const ng: PNewGadget; const TagList: PTagItem): PGadget; syscall GadToolsBase 5;
function CreateMenusA(const NewMenu: PNewMenu; const TagList: PTagItem): PMenu; syscall GadToolsBase 8;
procedure DrawBevelBoxA(RPort: PRastPort; Left: LongInt; Top: LongInt; Width: LongInt; Height: LongInt; const TagList: PTagItem); syscall GadToolsBase 20;
procedure FreeGadgets(Gad: PGadget); syscall GadToolsBase 6;
procedure FreeMenus(Menu: PMenu); syscall GadToolsBase 9;
procedure FreeVisualInfo(vi: Pointer); syscall GadToolsBase 22;
function GetVisualInfoA(Screen: PScreen; const TagList: PTagItem): Pointer; syscall GadToolsBase 21;
procedure GT_BeginRefresh(Win: PWindow); syscall GadToolsBase 15;
procedure GT_EndRefresh(Win: PWindow; Complete: LongInt); syscall GadToolsBase 16;
function GT_FilterIMsg(const IMsg: PIntuiMessage): PIntuiMessage; syscall GadToolsBase 17;

FUNCTION GT_GetIMsg(IPort: PMsgPort): PIntuiMessage; syscall GadToolsBase 12;
FUNCTION GT_PostFilterIMsg(IMsg: PIntuiMessage): PIntuiMessage; syscall GadToolsBase 18;
procedure GT_RefreshWindow(Win: PWindow; Req: PRequester); syscall GadToolsBase 14;
procedure GT_ReplyIMsg(IMsg: PIntuiMessage); syscall GadToolsBase 13;
function GT_GetGadgetAttrsA(Gad: PGadget; Win: PWindow; Req: PRequester; const TagList: PTagItem): LongInt; syscall GadToolsBase 29;
procedure GT_SetGadgetAttrsA(Gad: PGadget; Win: PWindow; Req: PRequester; const TagList: PTagItem); syscall GadToolsBase 7;
function LayoutMenuItemsA(FirstItem: PMenuItem; vi: Pointer; const TagList: PTagItem): LongBool; syscall GadToolsBase 10;
function LayoutMenusA(FirstMenu: PMenu; vi: Pointer; const TagList: PTagItem): LongBool; syscall GadToolsBase 11;

function GTMENUITEM_USERDATA(MenuItem: PMenuItem): Pointer;
function GTMENU_USERDATA(Menu: PMenu): Pointer;


implementation

function GTMENUITEM_USERDATA(menuitem : pMenuItem): pointer;
begin
  GTMENUITEM_USERDATA := Pointer((PMenuItem(MenuItem) + 1));
end;

function GTMENU_USERDATA(Menu: PMenu): Pointer;
begin
  GTMENU_USERDATA := Pointer((PMenu(Menu) + 1));
end;

initialization
  GadToolsBase := OpenLibrary('gadtools.library', 36);
finalization
  CloseLibrary(GadToolsBase);
end. 




