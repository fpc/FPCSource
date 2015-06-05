{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    History:

    Added functions and procedures with array of const.
    For use with fpc 1.0.7. Thay are in systemvartags.
    11 Nov 2002.


    Added the define use_amiga_smartlink.
    13 Jan 2003.

    Update for AmigaOS 3.9.
    Added a few overlays.
    06 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se
}
{$PACKRECORDS 2}

unit utility;

INTERFACE
uses exec;


Type
      pClockData = ^tClockData;
      tClockData = record
        sec   : Word;
        min   : Word;
        hour  : Word;
        mday  : Word;
        month : Word;
        year  : Word;
        wday  : Word;
      END;

      pHook = ^tHook;
      tHook = record
        h_MinNode  : tMinNode;
        h_Entry    : Pointer;   { assembler entry point        }
        h_SubEntry : Pointer;   { often HLL entry point        }
        h_Data     : Pointer;   { owner specific               }
      END;

{
 * Hook calling conventions:
 *      A0 - pointer to hook data structure itself
 *      A1 - pointer to parameter structure ("message") typically
 *           beginning with a longword command code, which makes
 *           sense in the context in which the hook is being used.
 *      A2 - Hook specific address data ("object," e.g, GadgetInfo)
 *
 * Control will be passed to the routine h_Entry.  For many
 * High-Level Languages (HLL), this will be an assembly language
 * stub which pushes registers on the stack, does other setup,
 * and then calls the function at h_SubEntry.
 *
 * The C standard receiving code is:
 * CDispatcher( hook, object, message )
 *     struct Hook      *hook;
 *     APTR             object;
 *     APTR             message;
 *
 * NOTE that register natural order differs from this convention
 * for C parameter order, which is A0,A2,A1.
 *
 * The assembly language stub for "vanilla" C parameter conventions
 * could be:

 _hookEntry:
        move.l  a1,-(sp)                ; push message packet pointer
        move.l  a2,-(sp)                ; push object pointer
        move.l  a0,-(sp)                ; push hook pointer
        move.l  h_SubEntry(a0),a0       ; fetch C entry point ...
        jsr     (a0)                    ; ... and call it
        lea     12(sp),sp               ; fix stack
        rts

 * with this function as your interface stub, you can write
 * a Hook setup function as:

 SetupHook( hook, c_function, userdata )
 struct Hook    *hook;
 ULONG          (*c_function)();
 VOID           *userdata;

        ULONG   (*hookEntry)();

        hook->h_Entry =         hookEntry;
        hook->h_SubEntry =      c_function;
        hook->h_Data =                  userdata;


 * with Lattice C pragmas, you can put the C function in the
 * h_Entry field directly if you declare the function:

ULONG __saveds __asm
CDispatcher(    register __a0 struct Hook       *hook,
                register __a2 VOID              *object,
                register __a1 ULONG             *message );
 *
 ***}

 {      Namespace definitions      }


Type
{ The named object structure }
 pNamedObject = ^tNamedObject;
 tNamedObject = record
    no_Object  : Pointer;       { Your pointer, for whatever you want }
 END;

const
{ Tags for AllocNamedObject() }
 ANO_NameSpace  = 4000;    { Tag to define namespace      }
 ANO_UserSpace  = 4001;    { tag to define userspace      }
 ANO_Priority   = 4002;    { tag to define priority       }
 ANO_Flags      = 4003;    { tag to define flags          }

{ Flags for tag ANO_Flags }
 NSB_NODUPS     = 0;
 NSB_CASE       = 1;

 NSF_NODUPS     = 1;      { Default allow duplicates }
 NSF_CASE       = 2;      { Default to caseless... }


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
 PSTB_SIGNED =31;
 PSTB_UNPACK =30;    { Note that these are active low... }
 PSTB_PACK   =29;    { Note that these are active low... }
 PSTB_EXISTS =26;    { Tag exists bit true flag hack...  }

 PSTF_SIGNED = $80000000;
 PSTF_UNPACK = $40000000;
 PSTF_PACK   = $20000000;

 PSTF_EXISTS = $4000000;


{***************************************************************************}


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


{***************************************************************************}


{ Macros used by the next batch of macros below. Normally, you don't use
 * this batch directly. Then again, some folks are wierd
 }



{***************************************************************************}


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
 *    ULONG packTable[] =
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


{ ======================================================================= }
{ ==== TagItem ========================================================== }
{ ======================================================================= }
{ This data type may propagate through the system for more general use.
 * In the meantime, it is used as a general mechanism of extensible data
 * arrays for parameter specification and property inquiry (coming soon
 * to a display controller near you).
 *
 * In practice, an array (or chain of arrays) of TagItems is used.
 }
Type
    Tag = LongInt;
    pTag = ^Tag;

    pTagItem = ^tTagItem;
    tTagItem = record
     ti_Tag  : Tag;
     ti_Data : LongInt;
    END;

    ppTagItem = ^pTagItem;

{ ---- system tag values ----------------------------- }
CONST
 TAG_DONE          = 0; { terminates array of TagItems. ti_Data unused }
 TAG_END           = TAG_DONE;
 TAG_IGNORE        = 1; { ignore this item, not END of array           }
 TAG_MORE          = 2; { ti_Data is pointer to another array of TagItems
                         * note that this tag terminates the current array
                         }
 TAG_SKIP          = 3; { skip this AND the next ti_Data items         }

{ differentiates user tags from control tags }
 TAG_USER          = $80000000;    { differentiates user tags from system tags}

{* If the TAG_USER bit is set in a tag number, it tells utility.library that
 * the tag is not a control tag (like TAG_DONE, TAG_IGNORE, TAG_MORE) and is
 * instead an application tag. "USER" means a client of utility.library in
 * general, including system code like Intuition or ASL, it has nothing to do
 * with user code.
 *}


{ Tag filter logic specifiers for use with FilterTagItems() }
 TAGFILTER_AND     = 0;       { exclude everything but filter hits   }
 TAGFILTER_NOT     = 1;       { exclude only filter hits             }

{ Mapping types for use with MapTags() }
 MAP_REMOVE_NOT_FOUND = 0;  { remove tags that aren't in mapList }
 MAP_KEEP_NOT_FOUND   = 1;  { keep tags that aren't in mapList   }



Type
 pUtilityBase = ^tUtilityBase;
 tUtilityBase = record
    ub_LibNode   : tLibrary;
    ub_Language  : Byte;
    ub_Reserved  : Byte;
 END;

function AddNamedObject(nameSpace : pNamedObject location 'a0';obj : pNamedObject location 'a1') : LongBool; syscall _UtilityBase 222;
function AllocateTagItems(num : ULONG location 'd0') : pTagItem; syscall _UtilityBase 066;
function AllocNamedObjectA(const name : STRPTR location 'a0';const TagList : pTagItem location 'a1') : pNamedObject; syscall _UtilityBase 228;
procedure Amiga2Date(amigatime : ULONG location 'd0';resultat : pClockData location 'a0'); syscall _UtilityBase 120;
procedure ApplyTagChanges(TagList : pTagItem location 'a0'; const ChangeList : pTagItem location 'a1'); syscall _UtilityBase 186;
function AttemptRemNamedObject(obj : pNamedObject location 'a0') : LongInt; syscall _UtilityBase 234;
function CallHookPkt(h : pHook location 'a0';obj: APTR location 'a2'; paramPkt : APTR location 'a1') : ULONG; syscall _UtilityBase 102;
function CheckDate(const date : pClockData location 'a0') : ULONG; syscall _UtilityBase 132;
function CloneTagItems(const tagList : pTagItem location 'a0') : pTagItem; syscall _UtilityBase 072;
function Date2Amiga(const date : pClockData location 'a0') : ULONG; syscall _UtilityBase 126;
procedure FilterTagChanges(changelist: PTagItem location 'a0'; oldvalues : pTagItem location 'a1';apply : ULONG location 'd0'); syscall _UtilityBase 054;
function FilterTagItems(taglist : pTagItem  location 'a0';const tagArray : pULONG location 'a1';logic : ULONG location 'd0') : ULONG; syscall _UtilityBase 096;
function FindNamedObject(nameSpace : pNamedObject location 'a0';const name : STRPTR location 'a1';lastobject: pNamedObject location 'a2') : pNamedObject; syscall _UtilityBase 240;
function FindTagItem(TagVal : Tag location 'd0';const TagList : pTagItem location 'a0') : pTagItem; syscall _UtilityBase 030;
procedure FreeNamedObject(Obj : pNamedObject location 'a0'); syscall _UtilityBase 246;
procedure FreeTagItems(TagList : pTagItem location 'a0'); syscall _UtilityBase 078;
function GetTagData(tagval : Tag location 'd0';default : ULONG location 'd1';const TagList : pTagItem location 'a0') : ULONG; syscall _UtilityBase 036;
function GetUniqueID : ULONG; syscall _UtilityBase 270;
procedure MapTags(TagList : pTagItem location 'a0';const maplist : pTagItem location 'a1';IncludeMiss : ULONG location 'd0'); syscall _UtilityBase 060;
function NamedObjectName(Obj : pNamedObject location 'a0') : STRPTR; syscall _UtilityBase 252;
function NextTagItem(Item : ppTagItem location 'a0') : pTagItem; syscall _UtilityBase 048;
function PackBoolTags(InitialFlags : ULONG location 'd0';const TagList: PTagItem location 'a0'; const boolmap : pTagItem location 'a1') : ULONG; syscall _UtilityBase 042;
function PackStructureTags(packk: APTR location 'a0';const packTable : pULONG location 'a1';const TagList : pTagItem location 'a2') : ULONG; syscall _UtilityBase 210;
procedure RefreshTagItemClones(cloneTagItem : pTagItem location 'a0'; const OriginalTagItems : pTagItem location 'a1'); syscall _UtilityBase 084;
procedure ReleaseNamedObject(Obj : pNamedObject location 'a0'); syscall _UtilityBase 258;
procedure RemNamedObject(Obj : pNamedObject location 'a0';Msg : pointer location 'a1'); syscall _UtilityBase 264;
function SDivMod32( dividend: LongInt location 'd0'; divisor : LongInt location 'd1') : LongInt; syscall _UtilityBase 150;
function SMult32(Arg1: LongInt location 'd0'; Arg2 : LongInt location 'd1') : LongInt; syscall _UtilityBase 138;
function SMult64(Arg1: LongInt location 'd0'; Arg2 : LongInt location 'd1') : LongInt; syscall _UtilityBase 198;
function Stricmp(const Str1: STRPTR location 'a0';const Str2 : STRPTR location 'a1') : LongInt; syscall _UtilityBase 162;
function Strnicmp(const Str1: STRPTR location 'a0';const Str2 : STRPTR location 'a1';len : LongInt location 'd0') : LongInt; syscall _UtilityBase 168;
function TagInArray(t : Tag location 'd0';const TagArray : pULONG location 'a0') : LongBool; syscall _UtilityBase 090;
function ToLower(c : ULONG location 'd0') : Char; syscall _UtilityBase 180;
function ToUpper(c : ULONG location 'd0') : Char; syscall _UtilityBase 174;
function UDivMod32( dividend: ULONG location 'd0'; divisor : ULONG location 'd1') : ULONG; syscall _UtilityBase 156;
function UMult32(Arg1: ULONG location 'd0'; Arg2 : ULONG location 'd1') : ULONG; syscall _UtilityBase 144;
function UMult64(Arg1: ULONG location 'd0'; Arg2 : ULONG location 'd1') : ULONG; syscall _UtilityBase 204;
function UnpackStructureTags(const pac: APTR location 'a0';const packTable: pULONG location 'a1';TagList : pTagItem location 'a2') : ULONG; syscall _UtilityBase 216;

function AllocNamedObjectA(const name : string;const TagList : pTagItem) : pNamedObject;
FUNCTION FindNamedObject(nameSpace : pNamedObject; CONST name : string; lastObject : pNamedObject) : pNamedObject;
FUNCTION Stricmp(CONST string1 : string; CONST string2 : pCHAR) : LONGINT;
FUNCTION Stricmp(CONST string1 : pCHAR; CONST string2 : string) : LONGINT;
FUNCTION Stricmp(CONST string1 : string; CONST string2 : string) : LONGINT;
FUNCTION Strnicmp(CONST string1 : string; CONST string2 : pCHAR; length : LONGINT) : LONGINT;
FUNCTION Strnicmp(CONST string1 : pCHAR; CONST string2 : string; length : LONGINT) : LONGINT;
FUNCTION Strnicmp(CONST string1 : string; CONST string2 : string; length : LONGINT) : LONGINT;


IMPLEMENTATION


function AllocNamedObjectA(const name : string;const TagList : pTagItem) : pNamedObject;
begin
       AllocNamedObjectA := AllocNamedObjectA(PChar(RawByteString(name)),TagList);
end;

FUNCTION FindNamedObject(nameSpace : pNamedObject; CONST name : string; lastObject : pNamedObject) : pNamedObject;
begin
       FindNamedObject := FindNamedObject(nameSpace,PChar(RawByteString(name)),lastObject);
end;

FUNCTION Stricmp(CONST string1 : string; CONST string2 : pCHAR) : LONGINT;
begin
       Stricmp := Stricmp(PChar(RawbyteString(string1)),string2);
end;

FUNCTION Stricmp(CONST string1 : pCHAR; CONST string2 : string) : LONGINT;
begin
       Stricmp := Stricmp(string1,PChar(RawbyteString(string2)));
end;

FUNCTION Stricmp(CONST string1 : string; CONST string2 : string) : LONGINT;
begin
       Stricmp := Stricmp(PChar(RawbyteString(string1)),PChar(RawbyteString(string2)));
end;

FUNCTION Strnicmp(CONST string1 : string; CONST string2 : pCHAR; length : LONGINT) : LONGINT;
begin
       Strnicmp := Strnicmp(PChar(RawbyteString(string1)),string2,length);
end;

FUNCTION Strnicmp(CONST string1 : pCHAR; CONST string2 : string; length : LONGINT) : LONGINT;
begin
       Strnicmp := Strnicmp(string1,PChar(RawbyteString(string2)),length);
end;

FUNCTION Strnicmp(CONST string1 : string; CONST string2 : string; length : LONGINT) : LONGINT;
begin
       Strnicmp := Strnicmp(PChar(RawbyteString(string1)),PChar(RawbyteString(string2)),length);
end;


end.
