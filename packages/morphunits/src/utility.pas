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


{ * utility.library date defines
  *********************************************************************
  * }


type
  PClockData = ^TClockData;
  TClockData = packed record
    sec  : Word;
    min  : Word;
    hour : Word;
    mday : Word;
    month: Word;
    year : Word;
    wday : Word;
  end;



{ * utility.library tagitem defines
  *********************************************************************
  * }


type
  Tag = Cardinal;

type
  PPTagItem = ^PTagItem;
  PTagItem = ^TTagItem;
  TTagItem = packed record
    ti_Tag : Tag;
    ti_Data: Cardinal;
  end;


const
  TAG_DONE   = 0;
  TAG_END    = 0;
  TAG_IGNORE = 1;
  TAG_MORE   = 2;
  TAG_SKIP   = 3;

const
  TAG_USER   = 1 Shl 31;

const
  TAGFILTER_AND = 0;
  TAGFILTER_NOT = 1;

const
  MAP_REMOVE_NOT_FOUND = 0;
  MAP_KEEP_NOT_FOUND   = 1;



{ * utility.library namespace defines
  *********************************************************************
  * }


type
  PNamedObject = ^TNamedObject;
  TNamedObject = packed record
    no_Object: Pointer;
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



{ * utility.library pack attributes and macros
  *********************************************************************
  * }


const
  PSTB_SIGNED = 31;
  PSTB_UNPACK = 30;
  PSTB_PACK   = 29;
  PSTB_EXISTS = 26;

  PSTF_SIGNED = (1 Shl PSTB_SIGNED);
  PSTF_UNPACK = (1 Shl PSTB_UNPACK);
  PSTF_PACK   = (1 Shl PSTB_PACK);
  PSTF_EXISTS = (1 Shl PSTB_EXISTS);

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

{ * utility.library include
  *********************************************************************
  * }


const
  UtilityName = 'utility.library';


type
  PUtilityBase = ^TUtilityName;
  TUtilityName = packed record
    ub_LibNode : TLibrary;
    ub_Language: Byte;
    ub_Reserved: Byte;
  end;



{ * utility.library hook defines
  *********************************************************************
  * }


type
  PHook = ^THook;
  THook = packed record
    h_MinNode : TMinNode;
    h_Entry   : Pointer;
    h_SubEntry: Pointer;
    h_Data    : Pointer;
  end;


function FindTagItem(tagVal : Cardinal location 'd0';
                     tagList: PTagItem location 'a0'): PTagItem;
SysCall MOS_UtilityBase 030;

function GetTagData(tagValue  : Cardinal location 'd0';
                    defaultVal: Cardinal location 'd1';
                    tagList   : PTagItem location 'a0'): Cardinal;
SysCall MOS_UtilityBase 036;

function PackBoolTags(initialFlags: Cardinal location 'd0';
                      tagList     : PTagItem location 'a0';
                      boolMap     : PTagItem location 'a1'): Cardinal;
SysCall MOS_UtilityBase 042;

function NextTagItem(tagListPtr: pPTagItem location 'a0'): PTagItem;
SysCall MOS_UtilityBase 048;

procedure FilterTagChanges(changeList  : PTagItem location 'a0';
                           originalList: PTagItem location 'a1';
                           apply       : Cardinal location 'd0');
SysCall MOS_UtilityBase 054;

procedure MapTags(tagList: PTagItem location 'a0';
                  mapList: PTagItem location 'a1';
                  mapType: Cardinal location 'd0');
SysCall MOS_UtilityBase 060;

function AllocateTagItems(numTags: Cardinal location 'd0'): PTagItem;
SysCall MOS_UtilityBase 066;

function CloneTagItems(tagList: PTagItem location 'a0'): PTagItem;
SysCall MOS_UtilityBase 072;

procedure FreeTagItems(tagList: PTagItem location 'a0');
SysCall MOS_UtilityBase 078;

procedure RefreshTagItemClones(clone   : PTagItem location 'a0';
                               original: PTagItem location 'a1');
SysCall MOS_UtilityBase 084;

function TagInArray(tagValue    : Cardinal location 'd0';
                    var tagArray: Cardinal location 'a0'): LongBool;
SysCall MOS_UtilityBase 090;

function FilterTagItems(tagList        : PTagItem location 'a0';
                        var filterArray: Cardinal location 'a1';
                        logic          : Cardinal location 'd0'): Cardinal;
SysCall MOS_UtilityBase 096;

function CallHookPkt(hook       : PHook   location 'a0';
                     hobject    : Pointer location 'a2';
                     paramPacket: Pointer location 'a1'): Cardinal;
SysCall MOS_UtilityBase 102;

procedure Amiga2Date(seconds: Cardinal   location 'd0';
                     result : PClockData location 'a0');
SysCall MOS_UtilityBase 120;

function Date2Amiga(date: PClockData location 'a0'): Cardinal;
SysCall MOS_UtilityBase 126;

function CheckDate(date: PClockData location 'a0'): Cardinal;
SysCall MOS_UtilityBase 132;

function SMult32(arg1: LongInt location 'd0';
                 arg2: LongInt location 'd1'): LongInt;
SysCall MOS_UtilityBase 138;

function UMult32(arg1: Cardinal location 'd0';
                 arg2: Cardinal location 'd1'): Cardinal;
SysCall MOS_UtilityBase 144;

function SDivMod32(dividend: LongInt location 'd0';
                    divisor: LongInt location 'd1'): LongInt;
SysCall MOS_UtilityBase 150;

function UDivMod32(dividend: Cardinal location 'd0';
                   divisor : Cardinal location 'd1'): Cardinal;
SysCall MOS_UtilityBase 156;

function Stricmp(string1: PChar location 'a0';
                 string2: PChar location 'a1'): LongInt;
SysCall MOS_UtilityBase 162;

function Strnicmp(string1: PChar   location 'a0';
                  string2: PChar   location 'a1';
                  length : LongInt location 'd0'): LongInt;
SysCall MOS_UtilityBase 168;

function ToUpper(character: Cardinal location 'd0'): Char;
SysCall MOS_UtilityBase 174;

function ToLower(character: Cardinal location 'd0'): Char;
SysCall MOS_UtilityBase 180;

procedure ApplyTagChanges(list      : PTagItem location 'a0';
                          changeList: PTagItem location 'a1');
SysCall MOS_UtilityBase 186;

function SMult64(arg1: LongInt location 'd0';
                 arg2: LongInt location 'd1'): LongInt;
SysCall MOS_UtilityBase 198;

function UMult64(arg1: Cardinal location 'd0';
                 arg2: Cardinal location 'd1'): Cardinal;
SysCall MOS_UtilityBase 204;

function PackStructureTags(pack         : Pointer  location 'a0';
                           var packTable: Cardinal location 'a1';
                           tagList      : PTagItem location 'a2'): Cardinal;
SysCall MOS_UtilityBase 210;

function UnpackStructureTags(pack         : Pointer  location 'a0';
                             var packTable: Cardinal location 'a1';
                             tagList      : PTagItem location 'a2'): Cardinal;
SysCall MOS_UtilityBase 216;

function AddNamedObject(nameSpace: PNamedObject location 'a0';
                        nobject  : PNamedObject location 'a1'): LongBool;
SysCall MOS_UtilityBase 222;

function AllocNamedObjectA(name   : PChar    location 'a0';
                           tagList: PTagItem location 'a1'): PNamedObject;
SysCall MOS_UtilityBase 228;

function AttemptRemNamedObject(nobject: PNamedObject location 'a0'): LongInt;
SysCall MOS_UtilityBase 234;

function FindNamedObject(nameSpace : PNamedObject location 'a0';
                         name      : PChar        location 'a1';
                         lastObject: PNamedObject location 'a2'): PNamedObject;
SysCall MOS_UtilityBase 240;

procedure FreeNamedObject(nobject: PNamedObject location 'a0');
SysCall MOS_UtilityBase 246;

function NamedObjectName(nobject: PNamedObject location 'a0'): PChar;
SysCall MOS_UtilityBase 252;

procedure ReleaseNamedObject(nobject: pNamedObject location 'a0');
SysCall MOS_UtilityBase 258;

procedure RemNamedObject(nobject: PNamedObject location 'a0';
                         message: PMessage     location 'a1');
SysCall MOS_UtilityBase 264;

function GetUniqueID: Cardinal;
SysCall MOS_UtilityBase 270;


function TAG_(value: pointer): longword; inline;
function TAG_(value: pchar): longword; inline;


implementation

function TAG_(value: pointer): longword; inline;
begin
  TAG_:=longword(value);
end;

function TAG_(value: pchar): longword; inline;
begin
  TAG_:=longword(value);
end;


begin
  UtilityBase:=MOS_UtilityBase;
end.
