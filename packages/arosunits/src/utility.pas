{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    utility.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utility;

{$mode objfpc}{$H+}
{$PACKRECORDS C}

interface

uses
  Exec;

type
  PClockData = ^TClockData;
  TClockData = record
    Sec: Word;
    Min: Word;
    Hour: Word;
    MDay: Word;
    Month: Word;
    Year: Word;
    WDay: Word;
  end;
  
// Use CALLHOOKPKT to call a hook
  PHook = ^THook;
  THookFunctionProc = function(Hook: PHook; Object_: APTR; Message: APTR): IPTR; cdecl;
  
  THook = record
    h_MinNode: TMinNode;
    h_Entry: IPTR;    // Main Entry point THookFunctionProc
    h_SubEntry: IPTR; // Secondary entry point
    h_Data: Pointer;     // owner specific
  end;
  
// The named object structure
  PNamedObject = ^TNamedObject;
  TNamedObject = record
    no_Object: APTR;  // Your pointer, for whatever you want
  END;

const
// Tags for AllocNamedObject()
  ANO_NameSpace  = 4000; // Tag to define namespace
  ANO_UserSpace  = 4001; // tag to define userspace
  ANO_Priority   = 4002; // tag to define priority
  ANO_Flags      = 4003; // tag to define flags (NSF_*)

// Flags for tag ANO_Flags
  NSB_NODUPS     = 0;
  NSF_NODUPS     = 1 shl 0; // Default allow duplicates
  NSB_CASE       = 1;
  NSF_CASE       = 1 shl 1; // Default to caseless...

//   Control attributes for Pack/UnpackStructureTags()
{ PackTable definition:
 
  The PackTable is a simple array of LONGWORDS that are evaluated by
  PackStructureTags() and UnpackStructureTags().
 
  The table contains compressed information such as the tag offset from
  the base tag. The tag offset has a limited range so the base tag is
  defined in the first longword.
 
  After the first longword, the fields look as follows:
 
       +--------- 1 = signed, 0 = unsigned (for bits, 1=inverted boolean)
       |
       |  +------ 00 = Pack/Unpack, 10 = Pack, 01 = Unpack, 11 = special
       | / \
       | | |  +-- 00 = Byte, 01 = Integer, 10 = Long, 11 = Bit
       | | | / \
       | | | | | /----- For bit operations: 1 = TAG_EXISTS is TRUE
       | | | | | |
       | | | | | | /-------------------- Tag offset from base tag value
       | | | | | | |                 \
       m n n o o p q q q q q q q q q q r r r s s s s s s s s s s s s s
                                       \   | |               |
       Bit offset (for bit operations) ----/ |               |
                                             \                       |
       Offset into data structure -----------------------------------/
 
  A -1 longword signifies that the next longword will be a new base tag
 
  A 0 longword signifies that it is the end of the pack table.
 
  What this implies is that there are only 13-bits of address offset
  and 10 bits for tag offsets from the base tag.  For most uses this
  should be enough, but when this is not, either multiple pack tables
  or a pack table with extra base tags would be able to do the trick.
  The goal here was to make the tables small and yet flexible enough to
  handle most cases.}
const
  PSTB_EXISTS = 26;       // Tag exists bit true flag hack...
  PSTF_EXISTS = 1 shl 26;
  PSTB_PACK   = 29;       // Note that these are active low...
  PSTF_PACK   = 1 shl 29;
  PSTB_UNPACK = 30;       // Note that these are active low...
  PSTF_UNPACK = 1 shl 30;  
  PSTB_SIGNED = 31;
  PSTF_SIGNED = 1 shl 31;

  PKCTRL_UBYTE      = $00000000;
  PKCTRL_BYTE       = $80000000;
  PKCTRL_UWORD      = $08000000;
  PKCTRL_WORD       = $88000000;
  PKCTRL_LongWord   = $10000000;
  PKCTRL_LONG       = $90000000;
  PKCTRL_PACKUNPACK = $00000000;
  PKCTRL_UNPACKONLY = $20000000;
  PKCTRL_PACKONLY   = $40000000;
  PKCTRL_BIT        = $18000000;
  PKCTRL_FLIPBIT    = $98000000;

{ Some handy dandy macros to easily create pack tables
 *
 * Use PACK_STARTTABLE() at the start of a pack table. You pass it the
 * base tag value that will be handled in the following chunk of the pack
 * table.
 *
 * PACK_ENDTABLE() is used to mark the end of a pack table.
 *
 * PACK_NEWOFFSET() lets you change the base tag value used for subsequent
 * entries in the table
 *
 * PACK_ENTRY() lets you define an entry in the pack table. You pass it the
 * base tag value, the tag of interest, the type of the structure to use,
 * the field name in the structure to affect and control bits (combinations of
 * the various PKCTRL_XXX bits)
 *
 * PACK_BYTEBIT() lets you define a bit-control entry in the pack table. You
 * pass it the same data as PACK_ENTRY, plus the flag bit pattern this tag
 * affects. This macro should be used when the field being affected is byte
 * sized.
 *
 * PACK_WORDBIT() lets you define a bit-control entry in the pack table. You
 * pass it the same data as PACK_ENTRY, plus the flag bit pattern this tag
 * affects. This macro should be used when the field being affected is Integer
 * sized.
 *
 * PACK_LONGBIT() lets you define a bit-control entry in the pack table. You
 * pass it the same data as PACK_ENTRY, plus the flag bit pattern this tag
 * affects. This macro should be used when the field being affected is longword
 * sized.
 *
 * EXAMPLE:
 *
 *    LongWord packTable[] =
 *    (
 *         PACK_STARTTABLE(GA_Dummy),
 *         PACK_ENTRY(GA_Dummy,GA_Left,Gadget,LeftEdge,PKCTRL_WORD|PKCTRL_PACKUNPACK),
 *         PACK_ENTRY(GA_Dummy,GA_Top,Gadget,TopEdge,PKCTRL_WORD|PKCTRL_PACKUNPACK),
 *         PACK_ENTRY(GA_Dummy,GA_Width,Gadget,Width,PKCTRL_UWORD|PKCTRL_PACKUNPACK),
 *         PACK_ENTRY(GA_Dummy,GA_Height,Gadget,Height,PKCTRL_UWORD|PKCTRL_PACKUNPACK),
 *         PACK_WORDBIT(GA_Dummy,GA_RelVerify,Gadget,Activation,PKCTRL_BIT|PKCTRL_PACKUNPACK,GACT_RELVERIFY)
 *         PACK_ENDTABLE
 *    );
 }

// TagItem, Tag, TAG_USER moved to Exec needed there already for dome definitions


const
// system tag values Tag.ti_Tag
  TAG_DONE   = 0;  // terminates array of TagItems. ti_Data unused
  TAG_END    = TAG_DONE;
  TAG_IGNORE = 1;  // ignore this item, not END of array
  TAG_MORE   = 2;  // ti_Data is pointer to another array of TagItems note that this tag terminates the current array
  TAG_SKIP   = 3;  // skip this AND the next ti_Data items
// What separates user tags from system tags
  // TAG_USER = 1 shl 31; // see exec
  TAG_OS = 16;  // The first tag used by the OS
// Tag-Offsets for the OS
  DOS_TAGBASE       = TAG_OS; // Reserve 16k tags for DOS
  INTUITION_TAGBASE = TAG_OS or $2000; // Reserve 16k tags for Intuition

{ If the TAG_USER bit is set in a tag number, it tells utility.library that
  the tag is not a control tag (like TAG_DONE, TAG_IGNORE, TAG_MORE) and is
  instead an application tag. "USER" means a client of utility.library in
  general, including system code like Intuition or ASL, it has nothing to do
  with user code.}
// Tag filter logic specifiers for use with FilterTagItems()
  TAGFILTER_AND = 0; // exclude everything but filter hits
  TAGFILTER_NOT = 1; // exclude only filter hits

// Mapping types for use with MapTags()
  MAP_REMOVE_NOT_FOUND = 0; // remove tags that aren't in mapList
  MAP_KEEP_NOT_FOUND   = 1; // keep tags that aren't in mapList

  UTILITYNAME	= 'utility.library';

type
  PUtilityBase = ^TUtilityBase;
  TUtilityBase = record
    ub_LibNode: TLibrary;
    ub_Language: Byte;
    ub_Reserved: Byte;
  end;

function AddNamedObject(NameSpace, Object_: PNamedObject): LongBool; syscall AOS_UtilityBase 37;
function AllocateTagItems(Num: LongWord): PTagItem; syscall AOS_UtilityBase 11;
function AllocNamedObjectA(const Name: STRPTR; TagList: PTagItem): PNamedObject; syscall AOS_UtilityBase 38;
procedure Amiga2Date(Seconds: LongWord; Resultat: PClockData); syscall AOS_UtilityBase 20;
procedure ApplyTagChanges(List: PTagItem; ChangeList: PTagItem); syscall AOS_UtilityBase 31;
function AttemptRemNamedObject(Object_: PNamedObject): LongInt; syscall AOS_UtilityBase 39;
function CallHookPkt(Hook: PHook; Object_, ParamPaket: APTR): IPTR; syscall AOS_UtilityBase 17;
function CheckDate(Date: PClockData): LongWord; syscall AOS_UtilityBase 22;
function CloneTagItems(const TagList: PTagItem): PTagItem; syscall AOS_UtilityBase 12;
function Date2Amiga(Date: PClockData): LongWord; syscall AOS_UtilityBase 21;
procedure FilterTagChanges(ChangeList: PTagItem; const Oldvalues: PTagItem; Apply: LongBool); syscall AOS_UtilityBase 9;
function FilterTagItems(TagList: PTagItem; FilterArray: PTag; Logic: LongWord): LongWord; syscall AOS_UtilityBase 16;
function FindNamedObject(NameSpace: PNamedObject; const Name: STRPTR; LastObject: PNamedObject): PNamedObject; syscall AOS_UtilityBase 40;
function FindTagItem(TagValue: Tag; const TagList: PTagItem): PTagItem; syscall AOS_UtilityBase 5;
procedure FreeNamedObject(Object_: PNamedObject); syscall AOS_UtilityBase 41;
procedure FreeTagItems(TagList: PTagItem); syscall AOS_UtilityBase 13;
function GetTagData(TagValue: Tag; Default: IPTR; const TagList: PTagItem): IPTR; syscall AOS_UtilityBase 6;
function GetUniqueID: LongWord; syscall AOS_UtilityBase 45;
procedure MapTags(TagList: PTagItem; const MapList: PTagItem; MapType: LongWord); syscall AOS_UtilityBase 10;
function NamedObjectName(Object_: PNamedObject): STRPTR; syscall AOS_UtilityBase 42;
function NextTagItem(var Item: PTagItem): PTagItem; syscall AOS_UtilityBase 8;
function PackBoolTags(InitialFlags: LongWord; const TagList, BoolMap: PTagItem): IPTR; syscall AOS_UtilityBase 7;
function PackStructureTags(Pack: APTR; PackTable: PLongWord; TagList: PTagItem): LongWord; syscall AOS_UtilityBase 35;
procedure RefreshTagItemClones(Clone: PTagItem; const Original: PTagItem); syscall AOS_UtilityBase 14;
procedure ReleaseNamedObject(Object_: PNamedObject); syscall AOS_UtilityBase 43;
procedure RemNamedObject(Object_: PNamedObject; Message: PMessage); syscall AOS_UtilityBase 44;
function SDivMod32(Dividend, Divisor: LongInt): Int64; syscall AOS_UtilityBase 25;
function SMult32(Arg1, Arg2: LongInt): LongInt; syscall AOS_UtilityBase 23;
function SMult64(Arg1, Arg2: LongInt): Int64; syscall AOS_UtilityBase 33;
function Stricmp(const Str1: STRPTR; const Str2: STRPTR): LongInt; syscall AOS_UtilityBase 27;
function Strnicmp(const Str1: STRPTR; const Str2 : STRPTR; Length_: LongInt): LongInt; syscall AOS_UtilityBase 28;
function TagInArray(TagValue: Tag; TagArray: PTag): LongBool; syscall AOS_UtilityBase 15;
function ToLower(c: LongWord): Char; syscall AOS_UtilityBase 30;
function ToUpper(c: LongWord): Char; syscall AOS_UtilityBase 29;
function UDivMod32(Dividend, Divisor: LongWord): LongWord; syscall AOS_UtilityBase 26;
function UMult32(Arg1, Arg2: LongWord): LongWord; syscall AOS_UtilityBase 24;
function UMult64(Arg1, Arg2: LongWord): QWord; syscall AOS_UtilityBase 34;
function UnpackStructureTags(Pack: APTR; PackTable: PLongWord; TagList: PTagItem): LongWord; syscall AOS_UtilityBase 36;

// Macros
function CALLHOOKPKT_(Hook: PHook; Object_: APTR; Message: APTR): IPTR; inline;
function TAGLIST(var Args: array of const): PTagItem; // NOT threadsafe! Better use AddTags/GetTagPtr

// VarArgs Versions
function AllocNamedObject(const Name: STRPTR; const Tags: array of const): PNamedObject;
function CallHook(Hook: PHook; Object_: APTR; const Params: array of const): IPTR;

implementation

uses
  tagsarray,longarray;

function AllocNamedObject(const Name: STRPTR; const Tags: array of const): PNamedObject;
var
  TagList: TTagsList;
begin
  AddTags(TagList, Tags);
  Result := AllocNamedObjectA(Name, GetTagPtr(TagList));
end;
  
function TAGLIST(var Args: array of const): PTagItem;
begin
  Result := ReadInTags(Args);
end;

function CallHook(Hook: PHook; Object_: APTR; const Params: array of const): IPTR;
begin
  CallHook := CallHookPkt(Hook, Object_ , ReadInLongs(Params));
end;

function CALLHOOKPKT_(Hook: PHook; Object_: APTR; Message: APTR): IPTR;
var
  FuncPtr: THookFunctionProc; 
begin
  Result := 0;
  if (Hook = nil) or (Object_ = nil) or (Message = nil) then
    Exit;
  if (Hook^.h_Entry = 0) then
    Exit;
  FuncPtr := THookFunctionProc(Hook^.h_Entry);
  Result := FuncPtr(Hook, Object_, Message);
end;

end.
