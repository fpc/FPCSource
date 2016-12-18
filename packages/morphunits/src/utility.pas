{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    utility.library interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$inline on}
unit utility;

interface

uses
  exec;

var
  UtilityBase: Pointer;

// utility.library date defines
type
  PClockData = ^TClockData;
  TClockData = packed record
    Sec: Word;
    Min: Word;
    Hour: Word;
    MDay: Word;
    Month: Word;
    Year: Word;
    WDay: Word;
  end;


// utility.library tagitem defines
type
  Tag = LongWord;
  PTag = ^Tag;

  PPTagItem = ^PTagItem;
  PTagItem = ^TTagItem;
  TTagItem = packed record
    ti_Tag : Tag;
    ti_Data: LongWord;
  end;


const
  TAG_DONE   = 0;
  TAG_END    = 0;
  TAG_IGNORE = 1;
  TAG_MORE   = 2;
  TAG_SKIP   = 3;

  TAG_USER   = 1 Shl 31;

  TAGFILTER_AND = 0;
  TAGFILTER_NOT = 1;

  MAP_REMOVE_NOT_FOUND = 0;
  MAP_KEEP_NOT_FOUND   = 1;

// utility.library namespace defines
type
  PNamedObject = ^TNamedObject;
  TNamedObject = packed record
    no_Object: APTR;
  end;

const
  ANO_NameSpace  = 4000;
  ANO_UserSpace  = 4001;
  ANO_Priority   = 4002;
  ANO_Flags      = 4003;

  NSB_NODUPS = 0;
  NSB_CASE   = 1;

  NSF_NODUPS = 1 Shl NSB_NODUPS;
  NSF_CASE   = 1 Shl NSB_CASE;

// utility.library pack attributes and macros
const
  PSTB_SIGNED = 31;
  PSTB_UNPACK = 30;
  PSTB_PACK   = 29;
  PSTB_EXISTS = 26;

  PSTF_SIGNED = 1 Shl PSTB_SIGNED;
  PSTF_UNPACK = 1 Shl PSTB_UNPACK;
  PSTF_PACK   = 1 Shl PSTB_PACK;
  PSTF_EXISTS = 1 Shl PSTB_EXISTS;

const
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

{$WARNING FIX ME!!! Some macros to convert}
{
  PK_BITNUM1(flg)            ((flg) == 0x01 ? 0 : (flg) == 0x02 ? 1 : (flg) == 0x04 ? 2 : (flg) == 0x08 ? 3 : (flg) == 0x10 ? 4 : (flg) == 0x20 ? 5 : (flg) == 0x40 ? 6 : 7)
  PK_BITNUM2(flg)            ((flg < 0x100 ? PK_BITNUM1(flg) : 8 + PK_BITNUM1(flg >> 8)))
  PK_BITNUM(flg)             ((flg < 0x10000 ? PK_BITNUM2(flg) : 16 + PK_BITNUM2(flg >> 16)))
  PK_WORDOFFSET(flg)         ((flg) < 0x100 ? 1 : 0)
  PK_LONGOFFSET(flg)         ((flg) < 0x100  ? 3 : (flg) < 0x10000 ? 2 : (flg) < 0x1000000 ? 1 : 0)
  PK_CALCOFFSET(type,field)  ((ULONG)(&((struct type *)0)->field))


  PACK_STARTTABLE(tagbase)                           (tagbase)
  PACK_NEWOFFSET(tagbase)                            (-1L),(tagbase)
  PACK_ENDTABLE                                      0
  PACK_ENTRY(tagbase,tag,type,field,control)         (control | ((tag-tagbase) << 16L) | PK_CALCOFFSET(type,field))
  PACK_BYTEBIT(tagbase,tag,type,field,control,flags) (control | ((tag-tagbase) << 16L) | PK_CALCOFFSET(type,field) | (PK_BITNUM(flags) << 13L))
  PACK_WORDBIT(tagbase,tag,type,field,control,flags) (control | ((tag-tagbase) << 16L) | (PK_CALCOFFSET(type,field) + PK_WORDOFFSET(flags)) | ((PK_BITNUM(flags) & 7) << 13L))
  PACK_LONGBIT(tagbase,tag,type,field,control,flags) (control | ((tag-tagbase) << 16L) | (PK_CALCOFFSET(type,field) + PK_LONGOFFSET(flags)) | ((PK_BITNUM(flags) & 7) << 13L))
}

// utility.library include
const
  UtilityName = 'utility.library';


type
  PUtilityBase = ^TUtilityName;
  TUtilityName = packed record
    ub_LibNode : TLibrary;
    ub_Language: Byte;
    ub_Reserved: Byte;
  end;

// utility.library hook defines
type
  PHook = ^THook;
  THook = packed record
    h_MinNode : TMinNode;
    h_Entry   : Pointer;
    h_SubEntry: Pointer;
    h_Data    : APTR;
  end;


function FindTagItem(TagVal: Tag location 'd0'; TagList: PTagItem location 'a0'): PTagItem; SysCall MOS_UtilityBase 030;
function GetTagData(TagValue: Tag location 'd0'; DefaultVal: LongWord location 'd1'; TagList: PTagItem location 'a0'): LongWord; SysCall MOS_UtilityBase 036;
function PackBoolTags(InitialFlags: LongWord location 'd0'; TagList: PTagItem location 'a0'; BoolMap: PTagItem location 'a1'): LongWord; SysCall MOS_UtilityBase 042;
function NextTagItem(TagListPtr: PPTagItem location 'a0'): PTagItem; overload; SysCall MOS_UtilityBase 048;
function NextTagItem(var TagList: PTagItem location 'a0'): PTagItem; overload; SysCall MOS_UtilityBase 048;
procedure FilterTagChanges(ChangeList: PTagItem location 'a0'; OriginalList: PTagItem location 'a1'; Apply: LongWord location 'd0'); SysCall MOS_UtilityBase 054;
procedure MapTags(TagList: PTagItem location 'a0'; MapList: PTagItem location 'a1'; MapType: Cardinal location 'd0'); SysCall MOS_UtilityBase 060;
function AllocateTagItems(NumTags: Cardinal location 'd0'): PTagItem; SysCall MOS_UtilityBase 066;
function CloneTagItems(TagList: PTagItem location 'a0'): PTagItem; SysCall MOS_UtilityBase 072;
procedure FreeTagItems(TagList: PTagItem location 'a0'); SysCall MOS_UtilityBase 078;
procedure RefreshTagItemClones(Clone: PTagItem location 'a0'; Original: PTagItem location 'a1'); SysCall MOS_UtilityBase 084;
function TagInArray(TagValue: Tag location 'd0'; TagArray: PTag location 'a0'): LongBool; SysCall MOS_UtilityBase 090;
function FilterTagItems(TagList: PTagItem location 'a0'; FilterArray: PTag location 'a1'; Logic: LongWord location 'd0'): LongWord; SysCall MOS_UtilityBase 096;

function CallHookPkt(Hook: PHook location 'a0'; HObject: APTR location 'a2'; ParamPacket: APTR location 'a1'): LongWord; SysCall MOS_UtilityBase 102;

procedure Amiga2Date(Seconds: LongWord location 'd0'; Result: PClockData location 'a0'); SysCall MOS_UtilityBase 120;
function Date2Amiga(Date: PClockData location 'a0'): LongWord; SysCall MOS_UtilityBase 126;
function CheckDate(Date: PClockData location 'a0'): LongWord; SysCall MOS_UtilityBase 132;

function SMult32(Arg1: LongInt location 'd0'; Arg2: LongInt location 'd1'): LongInt; SysCall MOS_UtilityBase 138;
function UMult32(Arg1: LongWord location 'd0'; Arg2: LongWord location 'd1'): LongWord; SysCall MOS_UtilityBase 144;

function SDivMod32(Dividend: LongInt location 'd0'; Divisor: LongInt location 'd1'): LongInt; SysCall MOS_UtilityBase 150;
function UDivMod32(Dividend: LongWord location 'd0'; Divisor: LongWord location 'd1'): LongWord; SysCall MOS_UtilityBase 156;

function Stricmp(String1: STRPTR location 'a0'; String2: STRPTR location 'a1'): LongInt; SysCall MOS_UtilityBase 162;
function Strnicmp(String1: STRPTR location 'a0'; String2: STRPTR location 'a1'; Length: LongInt location 'd0'): LongInt; SysCall MOS_UtilityBase 168;
function ToUpper(Character: LongWord location 'd0'): Char; SysCall MOS_UtilityBase 174;
function ToLower(character: LongWord location 'd0'): Char; SysCall MOS_UtilityBase 180;

procedure ApplyTagChanges(List: PTagItem location 'a0'; ChangeList: PTagItem location 'a1'); SysCall MOS_UtilityBase 186;

function SMult64(Arg1: LongInt location 'd0'; Arg2: LongInt location 'd1'): LongInt; SysCall MOS_UtilityBase 198;
function UMult64(Arg1: LongWord location 'd0'; Arg2: LongWord location 'd1'): LongWord; SysCall MOS_UtilityBase 204;

function PackStructureTags(Pack: APTR location 'a0'; PackTable: PLongWord location 'a1'; TagList: PTagItem location 'a2'): LongWord; SysCall MOS_UtilityBase 210;
function UnpackStructureTags(Pack: APTR location 'a0'; PackTable: PLongWord location 'a1'; TagList: PTagItem location 'a2'): LongWord; SysCall MOS_UtilityBase 216;

function AddNamedObject(NameSpace: PNamedObject location 'a0'; NObject: PNamedObject location 'a1'): LongBool; SysCall MOS_UtilityBase 222;
function AllocNamedObjectA(Name: STRPTR location 'a0'; TagList: PTagItem location 'a1'): PNamedObject; SysCall MOS_UtilityBase 228;
function AttemptRemNamedObject(NObject: PNamedObject location 'a0'): LongInt; SysCall MOS_UtilityBase 234;
function FindNamedObject(NameSpace: PNamedObject location 'a0'; Name: STRPTR location 'a1'; LastObject: PNamedObject location 'a2'): PNamedObject; SysCall MOS_UtilityBase 240;
procedure FreeNamedObject(NObject: PNamedObject location 'a0'); SysCall MOS_UtilityBase 246;
function NamedObjectName(NObject: PNamedObject location 'a0'): STRPTR; SysCall MOS_UtilityBase 252;
procedure ReleaseNamedObject(NObject: PNamedObject location 'a0'); SysCall MOS_UtilityBase 258;
procedure RemNamedObject(NObject: PNamedObject location 'a0'; Message: PMessage location 'a1'); SysCall MOS_UtilityBase 264;

function GetUniqueID: LongWord; SysCall MOS_UtilityBase 270;

// varargs version
function AllocNamedObject(Name: STRPTR; const Tags: array of PtrUInt): PNamedObject; inline;

function TAG_(Value: Pointer): PtrUInt; overload; inline;
function TAG_(Value: PChar): PtrUInt; overload; inline;
function TAG_(Value: Boolean): PtrUInt; overload; inline;
function TAG_(Value: LongInt): PtrUInt; overload; inline;
function TAG_(Value: LongWord): PtrUInt; overload; inline;

function AsTag(Value: Pointer): PtrUInt; overload; inline;
function AsTag(Value: PChar): PtrUInt; overload; inline;
function AsTag(Value: Boolean): PtrUInt; overload; inline;
function AsTag(Value: LongInt): PtrUInt; overload; inline;
function AsTag(Value: LongWord): PtrUInt; overload; inline;

implementation

function AllocNamedObject(Name: STRPTR; const Tags: array of PtrUInt): PNamedObject; inline;
begin
  AllocNamedObject := AllocNamedObjectA(Name, @Tags);
end;

function TAG_(Value: Pointer): PtrUInt; inline;
begin
  TAG_ := PtrUInt(Value);
end;

function TAG_(Value: PChar): PtrUInt; inline;
begin
  TAG_ := PtrUInt(Value);
end;

function TAG_(Value: Boolean): PtrUInt; inline;
begin
  if Value then
    TAG_ := LTrue
  else
    TAG_ := LFalse;
end;

function TAG_(Value: LongInt): PtrUInt; inline;
begin
  TAG_ := PtrUInt(Value);
end;

function TAG_(Value: LongWord): PtrUInt; inline;
begin
  TAG_ := PtrUInt(Value);
end;

function AsTag(Value: Pointer): LongWord; inline;
begin
  AsTag := LongWord(Value);
end;

function AsTag(Value: PChar): PtrUInt; inline;
begin
  AsTag := PtrUInt(Value);
end;

function AsTag(Value: Boolean): PtrUInt; inline;
begin
  if Value then
    AsTag := LTrue
  else
    AsTag := LFalse;
end;

function AsTag(Value: LongInt): PtrUInt; inline;
begin
  AsTag := PtrUInt(Value);
end;

function AsTag(Value: LongWord): PtrUInt; inline;
begin
  AsTag := PtrUInt(Value);
end;

begin
  UtilityBase := MOS_UtilityBase;
end.
