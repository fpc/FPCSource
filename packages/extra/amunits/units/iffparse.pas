{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.
    
    Added function Make_ID.
    14 Jan 2003.
    
    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

unit iffparse;

INTERFACE

uses exec, clipboard, utility;


const

    IFFPARSENAME  : PChar = 'iffparse.library';

{
 * Struct associated with an active IFF stream.
 * "iff_Stream" is a value used by the client's read/write/seek functions -
 * it will not be accessed by the library itself and can have any value
 * (could even be a pointer or a BPTR).
 }
Type
       pIFFHandle = ^tIFFHandle;
       tIFFHandle = record
        iff_Stream,
        iff_Flags   : ULONG;
        iff_Depth   : LONGINT;      {  Depth of context stack.  }
        {  There are private fields hiding here.  }
       END;

{
 * Bit masks for "iff_Flags" field.
 }
CONST
 IFFF_READ     =  0;                      { read mode - default }
 IFFF_WRITE    =  1;                      { write mode }
 IFFF_RWBITS   =  (IFFF_READ + IFFF_WRITE);        { read/write bits }
 IFFF_FSEEK    =  2;                 { forward seek only }
 IFFF_RSEEK    =  4;                 { random seek }
 IFFF_RESERVED =  $FFFF0000;             { Don't touch these bits. }

{
 * When the library calls your stream handler, you'll be passed a pointer
 * to this structure as the "message packet".
 }
Type
       pIFFStreamCmd = ^tIFFStreamCmd;
       tIFFStreamCmd = record
        sc_Command    : Longint;     {  Operation to be performed (IFFCMD_) }
        sc_Buf        : Pointer;     {  Pointer to data buffer              }
        sc_NBytes     : Longint;     {  Number of bytes to be affected      }
       END;
{
 * A node associated with a context on the iff_Stack.  Each node
 * represents a chunk, the stack representing the current nesting
 * of chunks in the open IFF file.  Each context node has associated
 * local context items in the (private) LocalItems list.  The ID, type,
 * size and scan values describe the chunk associated with this node.
 }
       pContextNode = ^tContextNode;
       tContextNode = record
        cn_Node         : tMinNode;
        cn_ID,
        cn_Type,
        cn_Size,        {  Size of this chunk             }
        cn_Scan  : Longint;        {  # of bytes read/written so far }
        {  There are private fields hiding here.  }
       END;

{
 * Local context items live in the ContextNode's.  Each class is identified
 * by its lci_Ident code and has a (private) purge vector for when the
 * parent context node is popped.
 }
       pLocalContextItem = ^tLocalContextItem;
       tLocalContextItem = record
        lci_Node        : tMinNode;
        lci_ID,
        lci_Type,
        lci_Ident       : ULONG;
        {  There are private fields hiding here.  }
       END;

{
 * StoredProperty: a local context item containing the data stored
 * from a previously encountered property chunk.
 }
       pStoredProperty = ^tStoredProperty;
       tStoredProperty = Record
        sp_Size  : Longint;
        sp_Data  : Pointer;
       END;

{
 * Collection Item: the actual node in the collection list at which
 * client will look.  The next pointers cross context boundaries so
 * that the complete list is accessable.
 }
       pCollectionItem = ^tCollectionItem;
       tCollectionItem = record
        ci_Next                 : pCollectionItem;
        ci_Size                 : Longint;
        ci_Data                 : Pointer;
       END;

{
 * Structure returned by OpenClipboard().  You may do CMD_POSTs and such
 * using this structure.  However, once you call OpenIFF(), you may not
 * do any more of your own I/O to the clipboard until you call CloseIFF().
 }
       pClipboardHandle = ^tClipBoardHandle;
       tClipboardHandle = record
        cbh_Req                 : tIOClipReq;
        cbh_CBport,
        cbh_SatisfyPort         : tMsgPort;
       END;

{
 * IFF return codes.  Most functions return either zero for success or
 * one of these codes.  The exceptions are the read/write functions which
 * return positive values for number of bytes or records read or written,
 * or a negative error code.  Some of these codes are not errors per sae,
 * but valid conditions such as EOF or EOC (End of Chunk).
 }
CONST
 IFFERR_EOF            =  -1 ;    {  Reached logical END of file }
 IFFERR_EOC            =  -2 ;    {  About to leave context      }
 IFFERR_NOSCOPE        =  -3 ;    {  No valid scope for property }
 IFFERR_NOMEM          =  -4 ;    {  Internal memory alloc failed}
 IFFERR_READ           =  -5 ;    {  Stream read error           }
 IFFERR_WRITE          =  -6 ;    {  Stream write error          }
 IFFERR_SEEK           =  -7 ;    {  Stream seek error           }
 IFFERR_MANGLED        =  -8 ;    {  Data in file is corrupt     }
 IFFERR_SYNTAX         =  -9 ;    {  IFF syntax error            }
 IFFERR_NOTIFF         =  -10;    {  Not an IFF file             }
 IFFERR_NOHOOK         =  -11;    {  No call-back hook provided  }
 IFF_RETURN2CLIENT     =  -12;    {  Client handler normal return}

{
 MAKE_ID(a,b,c,d)        \
        ((ULONG) (a)<<24 | (ULONG) (b)<<16 | (ULONG) (c)<<8 | (ULONG) (d))
     }
{
 * Universal IFF identifiers.
 }
 ID_FORM = 1179603533;
 ID_LIST = 1279873876;
 ID_CAT  = 1128354848;
 ID_PROP = 1347571536;
 ID_NULL = 538976288;

{
 * Ident codes for universally recognized local context items.
 }
 IFFLCI_PROP         = 1886547824;
 IFFLCI_COLLECTION   = 1668246636;
 IFFLCI_ENTRYHANDLER = 1701734500;
 IFFLCI_EXITHANDLER  = 1702389860;


{
 * Control modes for ParseIFF() function.
 }
 IFFPARSE_SCAN         =  0;
 IFFPARSE_STEP         =  1;
 IFFPARSE_RAWSTEP      =  2;

{
 * Control modes for StoreLocalItem().
 }
 IFFSLI_ROOT           =  1;      {  Store in default context       }
 IFFSLI_TOP            =  2;      {  Store in current context       }
 IFFSLI_PROP           =  3;      {  Store in topmost FORM OR LIST  }

{
 * "Flag" for writing functions.  If you pass this value in as a size
 * to PushChunk() when writing a file, the parser will figure out the
 * size of the chunk for you.  (Chunk sizes >= 2**31 are forbidden by the
 * IFF specification, so this works.)
 }
 IFFSIZE_UNKNOWN       =  -1;

{
 * Possible call-back command values.  (Using 0 as the value for IFFCMD_INIT
 * was, in retrospect, probably a bad idea.)
 }
 IFFCMD_INIT    = 0;       {  Prepare the stream for a session    }
 IFFCMD_CLEANUP = 1;       {  Terminate stream session            }
 IFFCMD_READ    = 2;       {  Read bytes from stream              }
 IFFCMD_WRITE   = 3;       {  Write bytes to stream               }
 IFFCMD_SEEK    = 4;       {  Seek on stream                      }
 IFFCMD_ENTRY   = 5;       {  You just entered a new context      }
 IFFCMD_EXIT    = 6;       {  You're about to leave a context     }
 IFFCMD_PURGELCI= 7;       {  Purge a LocalContextItem            }

{  Backward compatibility.  Don't use these in new code.  }
 IFFSCC_INIT    = IFFCMD_INIT;
 IFFSCC_CLEANUP = IFFCMD_CLEANUP;
 IFFSCC_READ    = IFFCMD_READ;
 IFFSCC_WRITE   = IFFCMD_WRITE;
 IFFSCC_SEEK    = IFFCMD_SEEK;

VAR IFFParseBase : pLibrary;

FUNCTION AllocIFF : pIFFHandle;
FUNCTION AllocLocalItem(typ : LONGINT; id : LONGINT; ident : LONGINT; dataSize : LONGINT) : pLocalContextItem;
PROCEDURE CloseClipboard(clipHandle : pClipboardHandle);
PROCEDURE CloseIFF(iff : pIFFHandle);
FUNCTION CollectionChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
FUNCTION CollectionChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
FUNCTION CurrentChunk(iff : pIFFHandle) : pContextNode;
FUNCTION EntryHandler(iff : pIFFHandle; typ : LONGINT; id : LONGINT; position : LONGINT; handler : pHook; obj : POINTER) : LONGINT;
FUNCTION ExitHandler(iff : pIFFHandle; typ : LONGINT; id : LONGINT; position : LONGINT; handler : pHook; obj : POINTER) : LONGINT;
FUNCTION FindCollection(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : pCollectionItem;
FUNCTION FindLocalItem(iff : pIFFHandle; typ : LONGINT; id : LONGINT; ident : LONGINT) : pLocalContextItem;
FUNCTION FindProp(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : pStoredProperty;
FUNCTION FindPropContext(iff : pIFFHandle) : pContextNode;
PROCEDURE FreeIFF(iff : pIFFHandle);
PROCEDURE FreeLocalItem(localItem : pLocalContextItem);
FUNCTION GoodID(id : LONGINT) : LONGINT;
FUNCTION GoodType(typ : LONGINT) : LONGINT;
FUNCTION IDtoStr(id : LONGINT; buf : pCHAR) : pCHAR;
PROCEDURE InitIFF(iff : pIFFHandle; flags : LONGINT; streamHook : pHook);
PROCEDURE InitIFFasClip(iff : pIFFHandle);
PROCEDURE InitIFFasDOS(iff : pIFFHandle);
FUNCTION LocalItemData(localItem : pLocalContextItem) : POINTER;
FUNCTION OpenClipboard(unitNumber : LONGINT) : pClipboardHandle;
FUNCTION OpenIFF(iff : pIFFHandle; rwMode : LONGINT) : LONGINT;
FUNCTION ParentChunk(contextNode : pContextNode) : pContextNode;
FUNCTION ParseIFF(iff : pIFFHandle; control : LONGINT) : LONGINT;
FUNCTION PopChunk(iff : pIFFHandle) : LONGINT;
FUNCTION PropChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
FUNCTION PropChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
FUNCTION PushChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT; size : LONGINT) : LONGINT;
FUNCTION ReadChunkBytes(iff : pIFFHandle; buf : POINTER; numBytes : LONGINT) : LONGINT;
FUNCTION ReadChunkRecords(iff : pIFFHandle; buf : POINTER; bytesPerRecord : LONGINT; numRecords : LONGINT) : LONGINT;
PROCEDURE SetLocalItemPurge(localItem : pLocalContextItem; purgeHook : pHook);
FUNCTION StopChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
FUNCTION StopChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
FUNCTION StopOnExit(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
PROCEDURE StoreItemInContext(iff : pIFFHandle; localItem : pLocalContextItem; contextNode : pContextNode);
FUNCTION StoreLocalItem(iff : pIFFHandle; localItem : pLocalContextItem; position : LONGINT) : LONGINT;
FUNCTION WriteChunkBytes(iff : pIFFHandle; buf : POINTER; numBytes : LONGINT) : LONGINT;
FUNCTION WriteChunkRecords(iff : pIFFHandle; buf : POINTER; bytesPerRecord : LONGINT; numRecords : LONGINT) : LONGINT;

Function Make_ID(str : String) : LONGINT;

IMPLEMENTATION

uses msgbox;

FUNCTION AllocIFF : pIFFHandle;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L IFFParseBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocLocalItem(typ : LONGINT; id : LONGINT; ident : LONGINT; dataSize : LONGINT) : pLocalContextItem;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVE.L  ident,D2
    MOVE.L  dataSize,D3
    MOVEA.L IFFParseBase,A6
    JSR -186(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE CloseClipboard(clipHandle : pClipboardHandle);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L clipHandle,A0
    MOVEA.L IFFParseBase,A6
    JSR -252(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CloseIFF(iff : pIFFHandle);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -048(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION CollectionChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -138(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION CollectionChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L propArray,A1
    MOVE.L  numPairs,D0
    MOVEA.L IFFParseBase,A6
    JSR -144(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION CurrentChunk(iff : pIFFHandle) : pContextNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -174(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION EntryHandler(iff : pIFFHandle; typ : LONGINT; id : LONGINT; position : LONGINT; handler : pHook; obj : POINTER) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVE.L  position,D2
    MOVEA.L handler,A1
    MOVEA.L obj,A2
    MOVEA.L IFFParseBase,A6
    JSR -102(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ExitHandler(iff : pIFFHandle; typ : LONGINT; id : LONGINT; position : LONGINT; handler : pHook; obj : POINTER) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVE.L  position,D2
    MOVEA.L handler,A1
    MOVEA.L obj,A2
    MOVEA.L IFFParseBase,A6
    JSR -108(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindCollection(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : pCollectionItem;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -162(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindLocalItem(iff : pIFFHandle; typ : LONGINT; id : LONGINT; ident : LONGINT) : pLocalContextItem;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVE.L  ident,D2
    MOVEA.L IFFParseBase,A6
    JSR -210(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindProp(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : pStoredProperty;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -156(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindPropContext(iff : pIFFHandle) : pContextNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -168(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE FreeIFF(iff : pIFFHandle);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -054(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeLocalItem(localItem : pLocalContextItem);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L localItem,A0
    MOVEA.L IFFParseBase,A6
    JSR -204(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION GoodID(id : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  id,D0
    MOVEA.L IFFParseBase,A6
    JSR -258(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GoodType(typ : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  typ,D0
    MOVEA.L IFFParseBase,A6
    JSR -264(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION IDtoStr(id : LONGINT; buf : pCHAR) : pCHAR;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  id,D0
    MOVEA.L buf,A0
    MOVEA.L IFFParseBase,A6
    JSR -270(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE InitIFF(iff : pIFFHandle; flags : LONGINT; streamHook : pHook);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  flags,D0
    MOVEA.L streamHook,A1
    MOVEA.L IFFParseBase,A6
    JSR -228(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE InitIFFasClip(iff : pIFFHandle);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -240(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE InitIFFasDOS(iff : pIFFHandle);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -234(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION LocalItemData(localItem : pLocalContextItem) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L localItem,A0
    MOVEA.L IFFParseBase,A6
    JSR -192(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenClipboard(unitNumber : LONGINT) : pClipboardHandle;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  unitNumber,D0
    MOVEA.L IFFParseBase,A6
    JSR -246(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenIFF(iff : pIFFHandle; rwMode : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  rwMode,D0
    MOVEA.L IFFParseBase,A6
    JSR -036(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ParentChunk(contextNode : pContextNode) : pContextNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L contextNode,A0
    MOVEA.L IFFParseBase,A6
    JSR -180(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ParseIFF(iff : pIFFHandle; control : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  control,D0
    MOVEA.L IFFParseBase,A6
    JSR -042(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION PopChunk(iff : pIFFHandle) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L IFFParseBase,A6
    JSR -090(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION PropChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -114(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION PropChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L propArray,A1
    MOVE.L  numPairs,D0
    MOVEA.L IFFParseBase,A6
    JSR -120(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION PushChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT; size : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVE.L  size,D2
    MOVEA.L IFFParseBase,A6
    JSR -084(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ReadChunkBytes(iff : pIFFHandle; buf : POINTER; numBytes : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L buf,A1
    MOVE.L  numBytes,D0
    MOVEA.L IFFParseBase,A6
    JSR -060(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ReadChunkRecords(iff : pIFFHandle; buf : POINTER; bytesPerRecord : LONGINT; numRecords : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L buf,A1
    MOVE.L  bytesPerRecord,D0
    MOVE.L  numRecords,D1
    MOVEA.L IFFParseBase,A6
    JSR -072(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE SetLocalItemPurge(localItem : pLocalContextItem; purgeHook : pHook);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L localItem,A0
    MOVEA.L purgeHook,A1
    MOVEA.L IFFParseBase,A6
    JSR -198(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION StopChunk(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -126(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION StopChunks(iff : pIFFHandle; propArray : LONGINT; numPairs : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L propArray,A1
    MOVE.L  numPairs,D0
    MOVEA.L IFFParseBase,A6
    JSR -132(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION StopOnExit(iff : pIFFHandle; typ : LONGINT; id : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVE.L  typ,D0
    MOVE.L  id,D1
    MOVEA.L IFFParseBase,A6
    JSR -150(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE StoreItemInContext(iff : pIFFHandle; localItem : pLocalContextItem; contextNode : pContextNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L localItem,A1
    MOVEA.L contextNode,A2
    MOVEA.L IFFParseBase,A6
    JSR -222(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION StoreLocalItem(iff : pIFFHandle; localItem : pLocalContextItem; position : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L localItem,A1
    MOVE.L  position,D0
    MOVEA.L IFFParseBase,A6
    JSR -216(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION WriteChunkBytes(iff : pIFFHandle; buf : POINTER; numBytes : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L buf,A1
    MOVE.L  numBytes,D0
    MOVEA.L IFFParseBase,A6
    JSR -066(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION WriteChunkRecords(iff : pIFFHandle; buf : POINTER; bytesPerRecord : LONGINT; numRecords : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iff,A0
    MOVEA.L buf,A1
    MOVE.L  bytesPerRecord,D0
    MOVE.L  numRecords,D1
    MOVEA.L IFFParseBase,A6
    JSR -078(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

Function Make_ID(str : String) : LONGINT;
begin
	Make_ID := (LONGINT(Ord(Str[1])) shl 24) or
	          (LONGINT(Ord(Str[2])) shl 16 ) or
		  (LONGINT(Ord(Str[3])) shl 8 ) or (LONGINT(Ord(Str[4])));
end;

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of iffparse.library}

var
    iffparse_exit : Pointer;

procedure CloseiffparseLibrary;
begin
    ExitProc := iffparse_exit;
    if IFFParseBase <> nil then begin
        CloseLibrary(IFFParseBase);
        IFFParseBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    IFFParseBase := nil;
    IFFParseBase := OpenLibrary(IFFPARSENAME,LIBVERSION);
    if IFFParseBase <> nil then begin
        iffparse_exit := ExitProc;
        ExitProc := @CloseiffparseLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open iffparse.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of iffparse.library compiled}
   {$Info Make sure you open iffparse.library yourself}
{$endif use_auto_openlib}


END. (* UNIT IFFPARSE *)


{
  $Log$
  Revision 1.4  2003-01-19 14:57:50  nils
  * added function make_id

  Revision 1.3  2003/01/14 18:46:04  nils
  * added defines use_amia_smartlink and use_auto_openlib

  * implemented autoopening of library

  Revision 1.2  2002/11/18 20:54:56  nils
    * missing semicolon

}

  

