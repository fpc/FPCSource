{$mode objfpc}
unit newt;
interface
{
  Automatically converted by H2Pas 0.99.15 from newt.h
  The following command line parameters were used:
    newt.h
}
{$PACKRECORDS C}
{$LINKLIB slang}

Const
  newtlib = 'newt';

const
  NEWT_COLORSET_ROOT          = 2;
  NEWT_COLORSET_BORDER        = 3;
  NEWT_COLORSET_WINDOW        = 4;
  NEWT_COLORSET_SHADOW        = 5;
  NEWT_COLORSET_TITLE         = 6;
  NEWT_COLORSET_BUTTON        = 7;
  NEWT_COLORSET_ACTBUTTON     = 8;
  NEWT_COLORSET_CHECKBOX      = 9;
  NEWT_COLORSET_ACTCHECKBOX   = 10;
  NEWT_COLORSET_ENTRY         = 11;
  NEWT_COLORSET_LABEL         = 12;
  NEWT_COLORSET_LISTBOX       = 13;
  NEWT_COLORSET_ACTLISTBOX    = 14;
  NEWT_COLORSET_TEXTBOX       = 15;
  NEWT_COLORSET_ACTTEXTBOX    = 16;
  NEWT_COLORSET_HELPLINE      = 17;
  NEWT_COLORSET_ROOTTEXT      = 18;
  NEWT_COLORSET_EMPTYSCALE    = 19;
  NEWT_COLORSET_FULLSCALE     = 20;
  NEWT_COLORSET_DISENTRY      = 21;
  NEWT_COLORSET_COMPACTBUTTON = 22;
  NEWT_COLORSET_ACTSELLISTBOX = 23;
  NEWT_COLORSET_SELLISTBOX    = 24;
  NEWT_ARG_LAST               = -(100000);
  NEWT_ARG_APPEND             = -(1);

type
  newtColors = record
    rootFg : ^char;
    rootBg : ^char;
    borderFg : ^char;
    borderBg : ^char;
    windowFg : ^char;
    windowBg : ^char;
    shadowFg : ^char;
    shadowBg : ^char;
    titleFg : ^char;
    titleBg : ^char;
    buttonFg : ^char;
    buttonBg : ^char;
    actButtonFg : ^char;
    actButtonBg : ^char;
    checkboxFg : ^char;
    checkboxBg : ^char;
    actCheckboxFg : ^char;
    actCheckboxBg : ^char;
    entryFg : ^char;
    entryBg : ^char;
    labelFg : ^char;
    labelBg : ^char;
    listboxFg : ^char;
    listboxBg : ^char;
    actListboxFg : ^char;
    actListboxBg : ^char;
    textboxFg : ^char;
    textboxBg : ^char;
    actTextboxFg : ^char;
    actTextboxBg : ^char;
    helpLineFg : ^char;
    helpLineBg : ^char;
    rootTextFg : ^char;
    rootTextBg : ^char;
    emptyScale : ^char;
    fullScale : ^char;
    disabledEntryFg : ^char;
    disabledEntryBg : ^char;
    compactButtonFg : ^char;
    compactButtonBg : ^char;
    actSelListboxFg : ^char;
    actSelListboxBg : ^char;
    selListboxFg : ^char;
    selListboxBg : ^char;
  end;
  newtFlagsSense = (NEWT_FLAGS_SET,NEWT_FLAGS_RESET,NEWT_FLAGS_TOGGLE );

const
  NEWT_FLAG_RETURNEXIT  = 1 shl 0;
  NEWT_FLAG_HIDDEN      = 1 shl 1;
  NEWT_FLAG_SCROLL      = 1 shl 2;
  NEWT_FLAG_DISABLED    = 1 shl 3;
  NEWT_FLAG_BORDER      = 1 shl 5;
  NEWT_FLAG_WRAP        = 1 shl 6;
  NEWT_FLAG_NOF12       = 1 shl 7;
  NEWT_FLAG_MULTIPLE    = 1 shl 8;
  NEWT_FLAG_SELECTED    = 1 shl 9;
  NEWT_FLAG_CHECKBOX    = 1 shl 10;

  NEWT_FD_READ          = 1 shl 0;
  NEWT_FD_WRITE         = 1 shl 1;

  NEWT_LISTBOX_RETURNEXIT = NEWT_FLAG_RETURNEXIT;
  NEWT_ENTRY_SCROLL       = NEWT_FLAG_SCROLL;
  NEWT_ENTRY_HIDDEN       = NEWT_FLAG_HIDDEN;
  NEWT_ENTRY_RETURNEXIT   = NEWT_FLAG_RETURNEXIT;
  NEWT_ENTRY_DISABLED     = NEWT_FLAG_DISABLED;
  NEWT_TEXTBOX_WRAP       = NEWT_FLAG_WRAP;
  NEWT_TEXTBOX_SCROLL     = NEWT_FLAG_SCROLL;
  NEWT_FORM_NOF12         = NEWT_FLAG_NOF12;

  NEWT_KEY_TAB            = #8;
  NEWT_KEY_ENTER          = #13;
  NEWT_KEY_SUSPEND        = #26;
  NEWT_KEY_RETURN         = NEWT_KEY_ENTER;
  NEWT_KEY_EXTRA_BASE     = $8000;

  NEWT_KEY_UP           = NEWT_KEY_EXTRA_BASE + 1;
  NEWT_KEY_DOWN         = NEWT_KEY_EXTRA_BASE + 2;
  NEWT_KEY_LEFT         = NEWT_KEY_EXTRA_BASE + 4;
  NEWT_KEY_RIGHT        = NEWT_KEY_EXTRA_BASE + 5;
  NEWT_KEY_BKSPC        = NEWT_KEY_EXTRA_BASE + 6;
  NEWT_KEY_DELETE       = NEWT_KEY_EXTRA_BASE + 7;
  NEWT_KEY_HOME         = NEWT_KEY_EXTRA_BASE + 8;
  NEWT_KEY_END          = NEWT_KEY_EXTRA_BASE + 9;
  NEWT_KEY_UNTAB        = NEWT_KEY_EXTRA_BASE + 10;
  NEWT_KEY_PGUP         = NEWT_KEY_EXTRA_BASE + 11;
  NEWT_KEY_PGDN         = NEWT_KEY_EXTRA_BASE + 12;
  NEWT_KEY_INSERT       = NEWT_KEY_EXTRA_BASE + 13;
  NEWT_KEY_F1           = NEWT_KEY_EXTRA_BASE + 101;
  NEWT_KEY_F2           = NEWT_KEY_EXTRA_BASE + 102;
  NEWT_KEY_F3           = NEWT_KEY_EXTRA_BASE + 103;
  NEWT_KEY_F4           = NEWT_KEY_EXTRA_BASE + 104;
  NEWT_KEY_F5           = NEWT_KEY_EXTRA_BASE + 105;
  NEWT_KEY_F6           = NEWT_KEY_EXTRA_BASE + 106;
  NEWT_KEY_F7           = NEWT_KEY_EXTRA_BASE + 107;
  NEWT_KEY_F8           = NEWT_KEY_EXTRA_BASE + 108;
  NEWT_KEY_F9           = NEWT_KEY_EXTRA_BASE + 109;
  NEWT_KEY_F10          = NEWT_KEY_EXTRA_BASE + 110;
  NEWT_KEY_F11          = NEWT_KEY_EXTRA_BASE + 111;
  NEWT_KEY_F12          = NEWT_KEY_EXTRA_BASE + 112;
 { not really a key, but newtGetKey returns it  }

  NEWT_KEY_RESIZE       = NEWT_KEY_EXTRA_BASE + 113;
  NEWT_ANCHOR_LEFT = 1 shl 0;
  NEWT_ANCHOR_RIGHT = 1 shl 1;
  NEWT_ANCHOR_TOP = 1 shl 2;
  NEWT_ANCHOR_BOTTOM = 1 shl 3;
  NEWT_GRID_FLAG_GROWX = 1 shl 0;
  NEWT_GRID_FLAG_GROWY = 1 shl 1;

// was alias in C:     newtListboxAddEntry = newtListboxAppendEntry;
// Was C alias      newtGridDestroy = Pointer; // ^newtGridFree;

type
  newtComponent  = Pointer;  // Opaque, was : ^newtComponent_struct;
  pnewtComponent = ^newtComponent;

  newtCallback = procedure (_para1:newtComponent; _para2:pointer);cdecl;
  newtSuspendCallback = procedure (data:pointer);cdecl;
  treason = (NEWT_EXIT_HOTKEY,NEWT_EXIT_COMPONENT, NEWT_EXIT_FDREADY,NEWT_EXIT_TIMER);
  newtExitStruct = record
    reason : treason;
    u : record
    case longint of
       0 : ( key : longint );
       1 : ( co : newtComponent );
    end;
  end;
  PnewtExitStruct = ^newtExitStruct;   (* Const before type ignored *)

  newtEntryFilter = function (entry:newtComponent; data:pointer; ch:longint; cursor:longint):longint;cdecl;
  newtGrid = pointer;
  newtGridElement = (NEWT_GRID_EMPTY := 0,NEWT_GRID_COMPONENT, NEWT_GRID_SUBGRID);

  newtWinEntry = record
    text : pchar;
    value : ppchar;
    flags : longint;
  end;
  PnewtWinEntry = ^newtWinEntry;

var
  newtDefaultColorPalette : newtColors;cvar;external;

function newtInit:longint;cdecl; external newtlib;
function newtFinished:longint; cdecl; external newtlib;
procedure newtCls;  cdecl; external newtlib;
procedure newtResizeScreen(redraw:longint);  cdecl; external newtlib;
procedure newtWaitForKey; cdecl; external newtlib;
procedure newtClearKeyBuffer; cdecl; external newtlib;
procedure newtDelay(usecs:longint); cdecl; external newtlib;
function newtOpenWindow(left:longint; top:longint; width:longint; height:longint; title:Pchar):longint;cdecl; external newtlib;
function newtCenteredWindow(width:longint; height:longint; title:Pchar):longint; cdecl; external newtlib;
procedure newtPopWindow; cdecl; external newtlib;
procedure newtSetColors(colors:newtColors); cdecl; external newtlib;
procedure newtRefresh; cdecl; external newtlib;
procedure newtSuspend; cdecl; external newtlib;
procedure newtSetSuspendCallback(cb:newtSuspendCallback; data:pointer);cdecl; external newtlib;
procedure newtResume;cdecl; external newtlib;
procedure newtPushHelpLine(text:Pchar); cdecl; external newtlib;
procedure newtRedrawHelpLine; cdecl; external newtlib;
procedure newtPopHelpLine; cdecl; external newtlib;
procedure newtDrawRootText(col:longint; row:longint; text:Pchar); cdecl; external newtlib;
procedure newtBell; cdecl; external newtlib;
function newtCompactButton(left:longint; top:longint; text:Pchar):newtComponent; cdecl; external newtlib;
function newtButton(left:longint; top:longint; text:Pchar):newtComponent; cdecl; external newtlib;
function newtCheckbox(left:longint; top:longint; text:Pchar; defValue:char; seq:Pchar;  aresult:Pchar):newtComponent;  cdecl; external newtlib;
function newtCheckboxGetValue(co:newtComponent):char; cdecl; external newtlib;
procedure newtCheckboxSetValue(co:newtComponent; value:char); cdecl; external newtlib;
procedure newtCheckboxSetFlags(co:newtComponent; flags:longint; sense:newtFlagsSense); cdecl; external newtlib;
function newtRadiobutton(left:longint; top:longint; text:Pchar; isDefault:longint; prevButton:newtComponent):newtComponent; cdecl; external newtlib;
function newtRadioGetCurrent(setMember:newtComponent):newtComponent; cdecl; external newtlib;
function newtListitem(left:longint; top:longint; text:Pchar; isDefault:longint; prevItem:newtComponent; data:pointer; flags:longint):newtComponent;cdecl; external newtlib;
procedure newtListitemSet(co:newtComponent; text:Pchar); cdecl; external newtlib;
function newtListitemGetData(co:newtComponent):pointer; cdecl; external newtlib;
procedure newtGetScreenSize(cols:Plongint; rows:Plongint); cdecl; external newtlib;
function newtLabel(left:longint; top:longint; text:Pchar):newtComponent; cdecl; external newtlib;
procedure newtLabelSetText(co:newtComponent; text:Pchar); cdecl; external newtlib;
function newtVerticalScrollbar(left:longint; top:longint; height:longint; normalColorset:longint; thumbColorset:longint):newtComponent; cdecl; external newtlib;
procedure newtScrollbarSet(co:newtComponent; where:longint; total:longint); cdecl; external newtlib;
function newtListbox(left:longint; top:longint; height:longint; flags:longint):newtComponent; cdecl; external newtlib;
function newtListboxGetCurrent(co:newtComponent):pointer; cdecl; external newtlib;
procedure newtListboxSetCurrent(co:newtComponent; num:longint); cdecl; external newtlib;
procedure newtListboxSetCurrentByKey(co:newtComponent; key:pointer); cdecl; external newtlib;
procedure newtListboxSetEntry(co:newtComponent; num:longint; text:Pchar); cdecl; external newtlib;
procedure newtListboxSetWidth(co:newtComponent; width:longint); cdecl; external newtlib;
procedure newtListboxSetData(co:newtComponent; num:longint; data:pointer); cdecl; external newtlib;
function newtListboxAppendEntry(co:newtComponent; text:Pchar; data:pointer):longint; cdecl; external newtlib;
function newtListboxInsertEntry(co:newtComponent; text:Pchar; data:pointer; key:pointer):longint; cdecl; external newtlib;
function newtListboxDeleteEntry(co:newtComponent; data:pointer):longint; cdecl; external newtlib;
procedure newtListboxClear(co:newtComponent); cdecl; external newtlib;
procedure newtListboxGetEntry(co:newtComponent; num:longint; text:PPchar; data:Ppointer); cdecl; external newtlib;
function newtListboxGetSelection(co:newtComponent; numitems:Plongint): ppointer; cdecl; external newtlib;
procedure newtListboxClearSelection(co:newtComponent); cdecl; external newtlib;
procedure newtListboxSelectItem(co:newtComponent; key:pointer; sense:newtFlagsSense); cdecl; external newtlib;
function newtCheckboxTree(left:longint; top:longint; height:longint; flags:longint):newtComponent; cdecl; external newtlib;
function newtCheckboxTreeMulti(left:longint; top:longint; height:longint; seq:Pchar; flags:longint):newtComponent; cdecl; external newtlib;
function newtCheckboxTreeGetSelection(co:newtComponent; numitems:Plongint): ppointer; cdecl; external newtlib;
function newtCheckboxTreeGetCurrent(co:newtComponent):pointer; cdecl; external newtlib;
function newtCheckboxTreeGetMultiSelection(co:newtComponent; numitems:Plongint; seqnum:char): ppointer; cdecl; external newtlib;
function newtCheckboxTreeAddItem(co:newtComponent; text:Pchar; data:pointer; flags:longint; index:longint):longint; cdecl; varargs; external newtlib;
function newtCheckboxTreeAddArray(co:newtComponent; text:Pchar; data:pointer; flags:longint; indexes:Plongint):longint;  cdecl; external newtlib;
function newtCheckboxTreeFindItem(co:newtComponent; data:pointer):plongint;  cdecl; external newtlib;
function newtTextboxReflowed(left:longint; top:longint; text:Pchar; width:longint; flexDown:longint; flexUp:longint; flags:longint):newtComponent;  cdecl; external newtlib;
function newtTextbox(left:longint; top:longint; width:longint; height:longint; flags:longint):newtComponent;  cdecl; external newtlib;
procedure newtTextboxSetText(co:newtComponent; text:Pchar);  cdecl; external newtlib;
procedure newtTextboxSetHeight(co:newtComponent; height:longint);  cdecl; external newtlib;
function newtTextboxGetNumLines(co:newtComponent):longint;  cdecl; external newtlib;
function newtReflowText(text:Pchar; width:longint; flexDown:longint; flexUp:longint; actualWidth:Plongint; actualHeight:Plongint):pchar;  cdecl; external newtlib;
function newtForm(vertBar:newtComponent; help:Pchar; flags:longint):newtComponent;  cdecl; external newtlib;
procedure newtFormSetTimer(form:newtComponent; millisecs:longint); cdecl; external newtlib;
procedure newtFormWatchFd(form:newtComponent; fd:longint; fdFlags:longint);  cdecl; external newtlib;
procedure newtFormSetSize(co:newtComponent);  cdecl; external newtlib;
function newtFormGetCurrent(co:newtComponent):newtComponent;  cdecl; external newtlib;
procedure newtFormSetBackground(co:newtComponent; color:longint);   cdecl; external newtlib;
procedure newtFormSetCurrent(co:newtComponent; subco:newtComponent);   cdecl; external newtlib;
procedure newtFormAddComponent(form:newtComponent; co:newtComponent);  cdecl; external newtlib;
procedure newtFormAddComponents(form:newtComponent);  cdecl; varargs; external newtlib;
procedure newtFormSetHeight(co:newtComponent; height:longint);   cdecl; external newtlib;
procedure newtFormSetWidth(co:newtComponent; width:longint);   cdecl; external newtlib;
function newtRunForm(form:newtComponent):newtComponent;  cdecl; external newtlib; { obsolete  }
procedure newtFormRun(co:newtComponent; es:PnewtExitStruct);   cdecl; external newtlib;
procedure newtDrawForm(form:newtComponent);   cdecl; external newtlib;
procedure newtFormAddHotKey(co:newtComponent; key:longint); cdecl; external newtlib;
function newtEntry(left:longint; top:longint; initialValue:Pchar; width:longint; resultPtr:PPchar; flags:longint):newtComponent; cdecl; external newtlib;
procedure newtEntrySet(co:newtComponent; value:Pchar; cursorAtEnd:longint);   cdecl; external newtlib;
procedure newtEntrySetFilter(co:newtComponent; filter:newtEntryFilter; data:pointer);   cdecl; external newtlib;
function newtEntryGetValue(co:newtComponent):pchar;   cdecl; external newtlib;
procedure newtEntrySetFlags(co:newtComponent; flags:longint; sense:newtFlagsSense);   cdecl; external newtlib;
function newtScale(left:longint; top:longint; width:longint; fullValue:int64):newtComponent; cdecl; external newtlib;
procedure newtScaleSet(co:newtComponent; amount:qword); cdecl; external newtlib;
procedure newtComponentAddCallback(co:newtComponent; f:newtCallback; data:pointer); cdecl; external newtlib;
procedure newtComponentTakesFocus(co:newtComponent; val:longint); cdecl; external newtlib;
procedure newtFormDestroy(form:newtComponent); cdecl; external newtlib;
function newtCreateGrid(cols:longint; rows:longint):newtGrid;cdecl; external newtlib;
function newtGridVStacked(_type:newtGridElement; what:pointer):newtGrid;cdecl; varargs; external newtlib;
function newtGridVCloseStacked(_type:newtGridElement; what:pointer):newtGrid;cdecl; varargs; external newtlib;
function newtGridHStacked(type1:newtGridElement; what1:pointer):newtGrid;cdecl; varargs; external newtlib;
function newtGridHCloseStacked(type1:newtGridElement; what1:pointer):newtGrid;cdecl; varargs; external newtlib;
function newtGridBasicWindow(text:newtComponent; middle:newtGrid; buttons:newtGrid):newtGrid;cdecl; external newtlib;
function newtGridSimpleWindow(text:newtComponent; middle:newtComponent; buttons:newtGrid):newtGrid;cdecl; external newtlib;
procedure newtGridSetField(grid:newtGrid; col:longint; row:longint; _type:newtGridElement; val:pointer;
                padLeft:longint; padTop:longint; padRight:longint; padBottom:longint; anchor:longint;
                flags:longint);cdecl; external newtlib;
procedure newtGridPlace(grid:newtGrid; left:longint; top:longint);cdecl; external newtlib;
procedure newtGridFree(grid:newtGrid; recurse:longint);cdecl; external newtlib;
procedure newtGridGetSize(grid:newtGrid; width:Plongint; height:Plongint);cdecl; external newtlib;
procedure newtGridWrappedWindow(grid:newtGrid; title:Pchar);cdecl; external newtlib;
procedure newtGridWrappedWindowAt(grid:newtGrid; title:Pchar; left:longint; top:longint);cdecl; external newtlib;
procedure newtGridAddComponentsToForm(grid:newtGrid; form:newtComponent; recurse:longint);cdecl; external newtlib;
function newtButtonBarv(button1:Pchar; b1comp:PnewtComponent):newtGrid;cdecl;varargs; external newtlib;
function newtButtonBar(button1:Pchar; b1comp:PnewtComponent):newtGrid;cdecl;varargs; external newtlib;
procedure newtWinMessage(title:Pchar; buttonText:Pchar; text:Pchar);cdecl;varargs; external newtlib;
procedure newtWinMessagev(title:Pchar; buttonText:Pchar; text:Pchar);cdecl;varargs; external newtlib;
function newtWinChoice(title:Pchar; button1:Pchar; button2:Pchar; text:Pchar):longint;cdecl; varargs; external newtlib;
function newtWinTernary(title:Pchar; button1:Pchar; button2:Pchar; button3:Pchar; message:Pchar):longint;cdecl;varargs; external newtlib;
function newtWinMenu(title:Pchar; text:Pchar; suggestedWidth:longint; flexDown:longint; flexUp:longint;  maxListHeight:longint; items:PPchar; listItem:Plongint; button1:Pchar):longint;cdecl; varargs; external newtlib;
function newtWinEntries(title:Pchar; text:Pchar; suggestedWidth:longint; flexDown:longint; flexUp:longint;
               dataWidth:longint; items:PnewtWinEntry; button1:Pchar):longint;cdecl;varargs; external newtlib;

implementation

end.
