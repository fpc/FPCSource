{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    utility.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$PACKRECORDS 2}

unit utility;

interface

uses
  exec;

type
  PClockData = ^TClockData;
  TClockData = record
    sec: Word;   // 0..59
    min: Word;   // 0..59
    hour: Word;  // 0..23
    mday: Word;  // 1..31
    month: Word; // 1..12
    year: Word;  // 1978..
    wday: Word;  // 0..6; 0 = Sunday
  end;

// PHook/THook relocated to Exec

// Namespace definitions
type
// The named object structure
  PNamedObject = ^TNamedObject;
  TNamedObject = record
    no_Object: APTR;  // Your pointer, for whatever you want
  end;

const
// Tags for AllocNamedObject()

// enAllocNamedObjectTags
  ANO_NameSpace  = 4000; // Tag to define namespace
  ANO_UserSpace  = 4001; // tag to define userspace
  ANO_Priority   = 4002; // tag to define priority
  ANO_Flags      = 4003; // tag to define flags

// Flags for tag ANO_Flags
// enANOFlagBits
  NSB_NODUPS     = 0;
  NSB_CASE       = 1;
// enANOFlags
  NSF_NODUPS     = 1 shl NSB_NODUPS; // Default allow duplicates
  NSF_CASE       = 1 shl NSB_CASE;   // Default to caseless...


   {    Control attributes for Pack/UnpackStructureTags() }


{ PackTable definition:
 *
 * The PackTable is a simple array of LONGWORDS that are evaluated by
 * PackStructureTags() and UnpackStructureTags().
 *
 * The table contains compressed information such as the tag offset from
 * the base tag. The tag offset has a limited range so the base tag is
 * defined in the first longword.
 *
 * After the first longword, the fields look as follows:
 *
 *      +--------- 1 = signed, 0 = unsigned (for bits, 1=inverted boolean)
 *      |
 *      |  +------ 00 = Pack/Unpack, 10 = Pack, 01 = Unpack, 11 = special
 *      | / \
 *      | | |  +-- 00 = Byte, 01 = Integer, 10 = Long, 11 = Bit
 *      | | | / \
 *      | | | | | /----- For bit operations: 1 = TAG_EXISTS is TRUE
 *      | | | | | |
 *      | | | | | | /-------------------- Tag offset from base tag value
 *      | | | | | | |                 \
 *      m n n o o p q q q q q q q q q q r r r s s s s s s s s s s s s s
 *                                      \   | |               |
 *      Bit offset (for bit operations) ----/ |               |
 *                                            \                       |
 *      Offset into data structure -----------------------------------/
 *
 * A -1 longword signifies that the next longword will be a new base tag
 *
 * A 0 longword signifies that it is the end of the pack table.
 *
 * What this implies is that there are only 13-bits of address offset
 * and 10 bits for tag offsets from the base tag.  For most uses this
 * should be enough, but when this is not, either multiple pack tables
 * or a pack table with extra base tags would be able to do the trick.
 * The goal here was to make the tables small and yet flexible enough to
 * handle most cases.
 }

const
  PSTB_SIGNED = 31;
  PSTB_UNPACK = 30; // Note that these are active low...
  PSTB_PACK   = 29; // Note that these are active low...
  PSTB_EXISTS = 26; // Tag exists bit true flag hack...

  PSTF_SIGNED = 1 shl PSTB_SIGNED;
  PSTF_UNPACK = 1 shl PSTB_UNPACK;
  PSTF_PACK   = 1 shl PSTB_PACK;

  PSTF_EXISTS = 1 shl PSTB_EXISTS;


// *********************************************************************

 PKCTRL_PACKUNPACK = $00000000;
 PKCTRL_PACKONLY   = $40000000;
 PKCTRL_UNPACKONLY = $20000000;

 PKCTRL_BYTE       = $80000000;
 PKCTRL_WORD       = $88000000;
 PKCTRL_LONG       = $90000000;

 PKCTRL_UBYTE      = $00000000;
 PKCTRL_UWORD      = $08000000;
 PKCTRL_ULONG      = $10000000;

 PKCTRL_BIT        = $18000000;
 PKCTRL_FLIPBIT    = $98000000;


// *********************************************************************
// Tags are a general mechanism of extensible data arrays for parameter
// specification and property inquiry. In practice, tags are used in arrays,
// or chain of arrays.

// PTagItem/TTagItem/TAG relocated to Exec

// constants for Tag.ti_Tag, control tag values
const
  TAG_DONE   = 0; // terminates array of TagItems. ti_Data unused
  TAG_END    = 0; // synonym for TAG_DONE
  TAG_IGNORE = 1; // ignore this item, not end of array
  TAG_MORE   = 2; // ti_Data is pointer to another array of TagItems note that this tag terminates the current array
  TAG_SKIP   = 3; // skip this and the next ti_Data items

// differentiates user tags from control tags
  TAG_USER = $80000000;    { differentiates user tags from system tags}

{ If the TAG_USER bit is set in a tag number, it tells utility.library that
  the tag is not a control tag (like TAG_DONE, TAG_IGNORE, TAG_MORE) and is
  instead an application tag. "USER" means a client of utility.library in
  general, including system code like Intuition or ASL, it has nothing to do
  with user code.}

// Tag filter logic specifiers for use with FilterTagItems()
// enTagLogic
  TAGFILTER_AND = 0; // exclude everything but filter hits
  TAGFILTER_NOT = 1; // exclude only filter hits

// Mapping types for use with MapTags()
// enTagMap
 MAP_REMOVE_NOT_FOUND = 0; // remove tags that aren't in mapList
 MAP_KEEP_NOT_FOUND   = 1; // keep tags that aren't in mapList

type
  PUtilityBase = ^TUtilityBase;
  TUtilityBase = record
    ub_LibNode: TLibrary;
    ub_Language: Byte;    // Private, for lowlevel.library
    ub_Reserved: Byte;
  end;

//**********************************************************************
// Flags used by the UTF-8 functions. */
const
  UTF_INVALID_SUBST_FFFD = 1 shl 0;
  //  Do not abort decoding when an invalid UTF-8 sequence is encountered,
  //  instead, substitute the invalid byte sequence with the special
  //  $FFFD character. (inverse ? on square background)

type
// *********************************************************************
// Lists with probabilistic balancing
  PSkipList = ^TSkipList;
  TSkipList = record
    sl_Error: LongInt; // If an insertion fails, here is why
  end;

  PSkipNode = ^TSkipNode;
  TSkipNode = record
    sn_Reserved: APTR;
    sn_Key: APTR;      // Unique key associated with this node
  end;

// *********************************************************************
// Self-organizing binary trees
  PSplayTree = ^TSplayTree;
  TSplayTree = record
    st_Error: LongInt; // If an insertion fails, here is why
  end;

  PSplayNode = ^TSplayNode;
  TSplayNode = record
    sn_UserData: APTR; // Points to user data area for this node
  end;

const
// *********************************************************************
// Error codes that may be returned by the insertion functions.
// enErrorCodes
  INSERTNODE_OUT_OF_MEMORY = 1; // Not enough memory
  INSERTNODE_DUPLICATE_KEY = 2; // Key is not unique
  INSERTNODE_TOO_SHORT     = 3; // Node size must be at least as large as sizeof(TSkipNode)

// *********************************************************************
// Context information to be passed around between the different SHA-1
//   calculation routines. When the digest has been calculated, you fill
//   find it stored in the 'mdsha_Code' member (all 160 bits of it).
type
  PMessageDigest_SHA = ^TMessageDigest_SHA;
  TMessageDigest_SHA = record
    mdsha_Code: array[0..19] of Byte;
    mdsha_Reserved: array[0..327] of Byte;
  end;

  PRandomState = ^TRandomState;
  TRandomState = record
    rs_High: LongInt;
    rs_Low: LongInt;
  end;

var
  UtilityBase: pUtilityBase;

function UtilityObtain(): LongWord; syscall IUtility 60;
function UtilityRelease(): LongWord; syscall IUtility 64;
procedure UtilityExpunge(); syscall IUtility 68;
function UtilityClone(): PInterface; syscall IUtility 72;
function AllocateTagItems(NumTags: LongWord): PTagItem; syscall IUtility 76;
procedure ApplyTagChanges(List: PTagItem; const ChangeList: PTagItem); syscall IUtility 80;
function CloneTagItems(const Original: PTagItem): PTagItem; syscall IUtility 84;
procedure FilterTagChanges(Changelist: PTagItem; OriginalList: PTagItem; Apply: LongWord); syscall IUtility 88;
function FilterTagItems(Taglist: PTagItem; const FilterArray: PLongWord; Logic: LongWord): LongWord; syscall IUtility 92;
function FindTagItem(TagValue: Tag; const TagList: PTagItem): PTagItem; syscall IUtility 96;
procedure FreeTagItems(TagList: PTagItem); syscall IUtility 100;
function GetTagData(TagValue: Tag; DefaultValue: LongWord; const TagList: PTagItem): LongWord; syscall IUtility 104;
procedure MapTags(TagList: PTagItem; const Maplist: PTagItem; MapType: LongWord); syscall IUtility 108;
function NextTagItem(ItemPtr: PPTagItem): PTagItem; overload; syscall IUtility 112;
function NextTagItem(var Item: PTagItem): PTagItem; overload; syscall IUtility 112;
function PackBoolTags(InitialFlags: LongWord; const TagList: PTagItem; const Boolmap: PTagItem) : LongWord; syscall IUtility 116;
procedure RefreshTagItemClones(Clone: PTagItem; const OriginalTagItems : PTagItem); syscall IUtility 120;
function TagInArray(TagValue: Tag; const TagArray: PLongWord): LongBool; syscall IUtility 124;
function CallHookPkt(Hook: PHook; Obj: APTR; Message_: APTR): LongWord; syscall IUtility 128;
// 132 CallHook
function AddNamedObject(NameSpace: PNamedObject; Obj: PNamedObject): LongBool; syscall IUtility 136;
function AllocNamedObjectA(const Name: STRPTR; const TagList: PTagItem): PNamedObject; syscall IUtility 140;
// 144 AllocNamedObject
function AttemptRemNamedObject(Obj: PNamedObject): LongInt; syscall IUtility 148;
function FindNamedObject(NameSpace: PNamedObject; const Name: STRPTR; PreviousObject: PNamedObject): PNamedObject; syscall IUtility 152;
procedure FreeNamedObject(Obj: PNamedObject); syscall IUtility 156;
function NamedObjectName(Obj: PNamedObject): STRPTR; syscall IUtility 160;
procedure ReleaseNamedObject(Obj: PNamedObject); syscall IUtility 164;
procedure RemNamedObject(Obj: PNamedObject; Message_: PMessage); syscall IUtility 168;
procedure Amiga2Date(DateAmiga: LongWord; Cd: PClockData); syscall IUtility 172;
function CheckDate(const Date: PClockData): LongWord; syscall IUtility 176;
function Date2Amiga(const Date: PClockData): LongWord; syscall IUtility 180;
function CreateSkipList(Hook: PHook; MaxLevels: LongInt): PSkipList; syscall IUtility 184;
procedure DeleteSkipList(SkipList: PSkipList); syscall IUtility 188;
function FindSkipNode(SkipList: PSkipList; Key: APTR): PSkipList; syscall IUtility 192;
function GetFirstSkipNode(SkipList: PSkipList): PSkipList; syscall IUtility 196;
function GetNextSkipNode(SkipList: PSkipList): PSkipList; syscall IUtility 200;
function InsertSkipNode(SkipList: PSkipList): PSkipList; syscall IUtility 204;
function RemoveSkipNode(SkipList: PSkipList; Key: APTR): LongBool; syscall IUtility 208;
function CreateSplayTree(CompareHook: PHook): PSplayTree; syscall IUtility 212;
procedure DeleteSplayTree(SplayTree: PSplayTree); syscall IUtility 216;
function FindSplayNode(SplayTree: PSplayTree; Key: APTR): PSplayTree; syscall IUtility 220;
function InsertSplayNode(SplayTree: PSplayTree; Key: APTR; DateSize: LongWord): PSplayTree; syscall IUtility 224;
function RemoveSplayNode(SplayTree: PSplayTree; Key: APTR): PSplayTree; syscall IUtility 228;
function FindNameNC(List: PList; const Name: PChar): PNode; syscall IUtility 232;
function GetUniqueID : LongWord; syscall IUtility 236;
procedure MessageDigest_SHA_Final(SHAs: PMessageDigest_SHA); syscall IUtility 240;
procedure MessageDigest_SHA_Init(SHAs: PMessageDigest_SHA); syscall IUtility 244;
procedure MessageDigest_SHA_Update(SHAs: PMessageDigest_SHA; Data: APTR; NumBytes: LongInt); syscall IUtility 248;
function PackStructureTags(Pack: APTR; const PackTable: PLongWord; const TagList: PTagItem): LongWord; syscall IUtility 252;
function UnpackStructureTags(const Pack: APTR; const PackTable: PLongWord; TagList: PTagItem): LongWord; syscall IUtility 256;
function Random(State: PRandomState): LongWord; syscall IUtility 260;
function SetMem(Destination: APTR; FillChar: Byte; Length: LongInt): APTR; syscall IUtility 264;
function Stricmp(const S1: STRPTR; const S2: STRPTR): LongInt; syscall IUtility 268;
function Strlcpy(Dst: STRPTR; const Src: STRPTR; Size: LongInt): LongInt; syscall IUtility 272;
function Strlcat(Dst: STRPTR; const Src: STRPTR; Size: LongInt): LongInt; syscall IUtility 276;
function Strnicmp(const S1: STRPTR; const S2: STRPTR; n: LongInt): LongInt; syscall IUtility 280;
function ToLower(c: Char): LongWord; syscall IUtility 284;
function ToUpper(c: Char): LongWord; syscall IUtility 288;
function VASPrintf(const Fmt: STRPTR; Args: APTR): STRPTR; syscall IUtility 292;
// 296 ASPrintf
function VSNPrintf(Buffer: STRPTR; BufferSize: LongInt; const Fmt: STRPTR; Args: APTR): STRPTR; syscall IUtility 300;
// 304 SNPrintf
procedure ClearMem(Destination: APTR; Size: LongWord); syscall IUtility 308;
procedure MoveMem(const Source: APTR; Destination: APTR; Size: LongWord); syscall IUtility 312;
function Strlen(const String_: STRPTR): LongWord; syscall IUtility 316;
function UTF8toUCS4(const UTF8Source: STRPTR; UCS4Destination: PLongInt; UCS4DestinationSize: LongInt; Flags: LongWord): LongInt; syscall IUtility 320;
function UCS4toUTF8(const UCS4Source: PLongInt; UTF8Destination: STRPTR;  UTF8DestinationSize: LongInt; Flags: LongWord): LongInt; syscall IUtility 324;
function UTF8Strnicmp(const UTF8String1: STRPTR; const UTF8String2: STRPTR; MaxUCSchars: LongInt): LongInt; syscall IUtility 328;
function UTF8Stricmp(const UTF8String1: STRPTR; const UTF8String2: STRPTR): LongInt; syscall IUtility 332;
function UTF8Count(const UTF8String: STRPTR; Validate: LongInt): LongInt; syscall IUtility 336;
function UCS4Count(const UCS4String: PLongInt; Validate: LongInt): LongInt; syscall IUtility 340;
function UTF8Encode(UCS4: LongInt; Buffer: STRPTR; BufSize: LongInt; Flags: LongWord): LongInt; syscall IUtility 344;
function UTF8Decode(const UTF8PPTR: PSTRPTR; Flags: LongWord): LongInt; syscall IUtility 348;
function UCS4ToLower(UCS4In: LongInt): LongInt; syscall IUtility 352;
function UCS4ToUpper(UCS4In: LongInt): LongInt; syscall IUtility 356;
function UCS4Valid(UCS4: LongInt): LongInt; syscall IUtility 360;


function AllocNamedObject(Name: STRPTR; const Tags: array of PtrUInt): PNamedObject; inline;

function TAG_(value: pointer): PtrUInt; overload; inline;
function TAG_(value: PChar): PtrUInt; overload; inline;
function TAG_(value: boolean): PtrUInt; overload; inline;
function TAG_(value: integer): PtrUInt; overload; inline;

function AsTag(value: pointer): PtrUInt; overload; inline;
function AsTag(value: PChar): PtrUInt; overload; inline;
function AsTag(value: boolean): PtrUInt; overload; inline;
function AsTag(value: integer): PtrUInt; overload; inline;

implementation

function AllocNamedObject(Name: STRPTR; const Tags: array of PtrUInt): PNamedObject; inline;
begin
  AllocNamedObject := AllocNamedObjectA(Name, @Tags);
end;

function TAG_(value: pointer): PtrUInt; inline;
begin
  TAG_:=PtrUInt(value);
end;

function TAG_(value: PChar): PtrUInt; inline;
begin
  TAG_:=PtrUInt(value);
end;

function TAG_(value: boolean): PtrUInt; inline;
begin
  if value then
    TAG_ := LTrue
  else
    TAG_ := LFalse;
end;

function TAG_(value: integer): PtrUInt; inline;
begin
  TAG_:=PtrUInt(value);
end;

function AsTag(value: pointer): PtrUInt; inline;
begin
  AsTag:=PtrUInt(value);
end;

function AsTag(value: PChar): PtrUInt; inline;
begin
  AsTag:=PtrUInt(value);
end;

function AsTag(value: boolean): PtrUInt; inline;
begin
  if value then
    AsTag := LTrue
  else
    AsTag := LFalse;
end;

function AsTag(value: integer): PtrUInt; inline;
begin
  AsTag:=PtrUInt(value);
end;

initialization
  UtilityBase := _UtilityBase;
end.
