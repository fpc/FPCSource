{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    keymap.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit keymap;

interface

uses
  exec, inputevent;

type
  PKeyMap = ^TKeyMap;
  TKeyMap = record
    km_LoKeyMapTypes: PByte;
    km_LoKeyMap: PLongWord;
    km_LoCapsable: PByte;
    km_LoRepeatable: PByte;
    km_HiKeyMapTypes: PByte;
    km_HiKeyMap: PLongWord;
    km_HiCapsable: PByte;
    km_HiRepeatable: PByte;
  end;

  PKeymapNode = ^TKeyMapNode;
  TKeyMapNode = record
    kn_Node: TNode;       // including name of keymap
    kn_KeyMap: TKeyMap;
  end;

// the structure of keymap.resource
  PKeyMapResource = ^TKeyMapResource;
  TKeyMapResource = record
    kr_Node: TNode;
    kr_List: TList;       // a list of KeyMapNodes
  end;

const
// Key Map Types
  KC_NOQUAL           = 0;
  KC_VANILLA          = 7;   // note that SHIFT+ALT+CTRL is VANILLA
  KCB_SHIFT           = 0;
  KCF_SHIFT           = $01;
  KCB_ALT             = 1;
  KCF_ALT             = $02;
  KCB_CONTROL         = 2;
  KCF_CONTROL         = $04;
  KCB_DOWNUP          = 3;
  KCF_DOWNUP          = $08;

  KCB_DEAD            = 5;    // may be dead or modified by dead key:
  KCF_DEAD            = $20;  //   use dead prefix bytes

  KCB_STRING          = 6;
  KCF_STRING          = $40;

  KCB_NOP             = 7;
  KCF_NOP             = $80;

// Dead Prefix Bytes
  DPB_MOD             = 0;
  DPF_MOD             = $01;
  DPB_DEAD            = 3;
  DPF_DEAD            = $08;

  DP_2DINDEXMASK      = $0f;  // mask for index for 1st of two dead keys
  DP_2DFACSHIFT       = 4;    // shift for factor for 1st of two dead keys

// Some useful definitions for rawkey codes which are assumed
// to be the same on all keyboards so no call of MapRawKey()
//is necessary. These are the keydown codes only.
  RAWKEY_SPACE     = $40;
  RAWKEY_BACKSPACE = $41;
  RAWKEY_TAB       = $42;
  RAWKEY_ENTER     = $43; // Numeric pad
  RAWKEY_RETURN    = $44;
  RAWKEY_ESC       = $45;
  RAWKEY_DEL       = $46;
  RAWKEY_INSERT    = $47; // Not on classic keyboards
  RAWKEY_PAGEUP    = $48; // Not on classic keyboards
  RAWKEY_PAGEDOWN  = $49; // Not on classic keyboards
  RAWKEY_F11       = $4B; // Not on classic keyboards
  RAWKEY_CRSRUP    = $4C;
  RAWKEY_CRSRDOWN  = $4D;
  RAWKEY_CRSRRIGHT = $4E;
  RAWKEY_CRSRLEFT  = $4F;
  RAWKEY_F1        = $50;
  RAWKEY_F2        = $51;
  RAWKEY_F3        = $52;
  RAWKEY_F4        = $53;
  RAWKEY_F5        = $54;
  RAWKEY_F6        = $55;
  RAWKEY_F7        = $56;
  RAWKEY_F8        = $57;
  RAWKEY_F9        = $58;
  RAWKEY_F10       = $59;
  RAWKEY_HELP      = $5F;
  RAWKEY_LSHIFT    = $60;
  RAWKEY_RSHIFT    = $61;
  RAWKEY_CAPSLOCK  = $62;
  RAWKEY_LCTRL     = $63; // Right Ctrl is the same for now
  RAWKEY_LALT      = $64;
  RAWKEY_RALT      = $65;
  RAWKEY_LCOMMAND  = $66; // LAmiga|LWin|LApple|LMeta
  RAWKEY_RCOMMAND  = $67; // RAmiga|RWin|RApple|RMeta
  RAWKEY_MENU      = $6B; // Not on classic keyboards
                              // Menu|Win|Compose
                              // Dont use, its reserved
  RAWKEY_PRINTSCR  = $6D; // Not on classic keyboards
  RAWKEY_BREAK     = $6E; // Not on classic keyboards
                              // Pause/Break
  RAWKEY_F12       = $6F; // Not on classic keyboards
  RAWKEY_HOME      = $70; // Not on classic keyboards
  RAWKEY_END       = $71; // Not on classic keyboards

{ The following keys can exist on CDTV, CD32 and "multimedia" keyboards:

 Rawkey         |CD32 color&key     |CDTV key  |Comment
 ---------------+-------------------+----------+-----------
 = $72 Stop      |Blue     Stop      |Stop      |
 = $73 Play/Pause|Grey     Play/Pause|Play/Pause|
 = $74 Prev Track|Charcoal Reverse   |<< REW    |
 = $75 Next Track|Charcoal Forward   |>> FF     |
 = $76 Shuffle   |Green    Shuffle   |          |Random Play
 = $77 Repeat    |Yellow   Repeat    |          |}

  RAWKEY_MEDIA_STOP       = $72;
  RAWKEY_MEDIA_PLAY_PAUSE = $73;
  RAWKEY_MEDIA_PREV_TRACK = $74;
  RAWKEY_MEDIA_NEXT_TRACK = $75;
  RAWKEY_MEDIA_SHUFFLE    = $76;
  RAWKEY_MEDIA_REPEAT     = $77;

// Extended raw keys (via IECLASS_EXTENDEDRAWKEY)
  RAWKEY_F13  = $0103;
  RAWKEY_F14  = $0104;
  RAWKEY_F15  = $0105;

// Tags for keymap.library/ObtainKeyMapInfo()
  KEYMAPINFO_KEYMAPNODE         = TAG_USER + 0; // (struct KeyMapNode *)
  KEYMAPINFO_GETCLASSICKEYBOARD = TAG_USER + 1; // Private, dont use
  KEYMAPINFO_SETCLASSICKEYBOARD = TAG_USER + 2; // Private, dont use
// The following tags were added in V51
  KEYMAPINFO_INFOTEXT_ENGLISH   = TAG_USER + 3; // (^STRPTR)
  KEYMAPINFO_INFOTEXT_LOCAL     = TAG_USER + 4; // (^STRPTR)
  KEYMAPINFO_INFOTEXT_CHARSET   = TAG_USER + 5; // (PLongWord)
  KEYMAPINFO_CLASSIC_ONLY       = TAG_USER + 6; // (PLongWord)
  KEYMAPINFO_PC_ONLY            = TAG_USER + 7; // (PLongWord)
  KEYMAPINFO_SETCHARSET         = TAG_USER + 8; // (LongWord)

// Tags for keymap.library/ObtainRawKeyInfo() (V51.7)
  RKI_SET_TYPE       = TAG_USER + 0; // (LongWord)
  RKI_SET_VALUE      = TAG_USER + 1; // (LongWord)
  RKI_GET_RAWKEY     = TAG_USER + 2; // (PLongWord)
  RKI_GET_EXT_RAWKEY = TAG_USER + 3; // (PLongWord)
  RKI_GET_PS2_SET1   = TAG_USER + 4; // (PLongWord)
  RKI_GET_PS2_SET2   = TAG_USER + 5; // (PLongWord)
  RKI_GET_USB        = TAG_USER + 6; // (PLongWord)
  RKI_GET_FLAGS      = TAG_USER + 7; // (PLongWord)
  RKI_GET_NAME       = TAG_USER + 8; // (^STRPTR)

// Types for RKI_SET_TYPE
  RKITYPE_RAWKEY     = 1; // Amiga 8bit rawkey code
  RKITYPE_EXT_RAWKEY = 2; // Amiga 16bit extended rawkey code
  RKITYPE_PS2_SET1   = 3; // PS/2 Set1 make or break code
  RKITYPE_PS2_SET2   = 4; // PS/2 Set2 make or break code
  RKITYPE_USB        = 5; // USB HID Usage page and ID code (down)
  RKITYPE_USB_UPCODE = 6; // USB HID Usage page and ID code (up)

// Tags for keymap.library/KeyMapControlTagList() (V53.8)
  KMCTRL_SetAltAmigaSwap = TAG_USER + 0; // (BOOL)
  KMCTRL_GetAltAmigaSwap = TAG_USER + 1; // (PBOOL)

var
  KeymapBase: PLibrary = nil;

const
  KEYMAPNAME: PChar = 'keymap.library';
  IKeymap: PInterface = nil;

function KeymapObtain(): LongWord; syscall IKeymap 60;
function KeymapRelease(): LongWord; syscall IKeymap 64;
procedure KeymapExpunge(); syscall IKeymap 68;
function KeymapClone(): PInterface; syscall IKeymap 72;
procedure SetKeyMapDefault(KeyMap: PKeyMap); syscall IKeymap 76;
function AskKeyMapDefault: PKeyMap; syscall IKeymap 80;
function MapRawKey(Event: PInputEvent; Buffer: PChar; Length: LongInt; KeyMap: PKeyMap): SmallInt; syscall IKeymap 84;
function MapANSI(TheString: PChar; Count: LongInt; Buffer: PChar; Length: LongInt; KeyMap: PKeyMap): LongInt; syscall IKeymap 88;
function OpenKeyMapHandleA(const Filename: STRPTR; const TagList: PTagItem): APTR; syscall IKeymap 92;
// 96 OpenKeyMapHandle
procedure CloseKeyMapHandle(Handle: APTR); syscall IKeymap 100;
function ObtainKeyMapInfoA(Handle: APTR; const TagList: PTagItem): APTR; syscall IKeymap 104;
// 108 ObtainKeyMapInfo
function ReleaseKeyMapInfoA(Handle: APTR; const TagList: PTagItem): APTR; syscall IKeymap 112;
// 116 ReleaseKeyMapInfoA
function ObtainRawKeyInfoA(const TagList: PTagItem): APTR; syscall IKeymap 120;
// 124 ObtainRawKeyInfo
function KeyMapControlTagList(const TagList: PTagItem): APTR; syscall IKeymap 128;
// KeyMapControlTags

implementation

const
  { Change VERSION and LIBVERSION to proper values }
  LIBVERSION: LongWord = 0;

initialization
  KeymapBase := OpenLibrary(KEYMAPNAME,LIBVERSION);
  if Assigned(KeymapBase) then
    IKeymap := GetInterface(PLibrary(KeymapBase), 'main', 1, nil);
finalization
  if Assigned(IKeymap) then
    DropInterface(IKeymap);
  if Assigned(KeymapBase) then
    CloseLibrary(KeymapBase);
end.



