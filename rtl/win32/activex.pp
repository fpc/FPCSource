Unit ActiveX;

//+-------------------------------------------------------------------------
//
//  Microsoft Windows
//  Copyright (c) Microsoft Corporation. All rights reserved.
//
//  File: objidl.idl
//
//  Header translation by Marco van de Voort for Free Pascal Platform
//  SDK dl'ed January 2002
//
//--------------------------------------------------------------------------

Interface
{$Mode Delphi}
Uses Windows;

{$ifndef DO_NO_IMPORTS}
//import "unknwn.idl";
//import "wtypes.idl";
{$ENDIF}

{Glue types, should be linked to the proper windows unit types}

TYPE Size_t              = DWord;       {??, probably, like Unix, typecastable to pointer?!?}
     OleChar             = WChar;
     LPOLESTR            = ^OLECHAR;
     polestr             = Pwidechar;


     // bit flags for IExternalConnection
CONST
    EXTCONN_STRONG              = $0001;   // strong connection
    EXTCONN_WEAK                = $0002;  // weak connection (table, container)
    EXTCONN_CALLABLE            = $0004;  // table .vs. callable
     {Bind Flags}
    BIND_MAYBOTHERUSER          = 1;
    BIND_JUSTTESTEXISTENCE      = 2;


    MKSYS_NONE                  = 0;
    MKSYS_GENERICCOMPOSITE      = 1;
    MKSYS_FILEMONIKER           = 2;
    MKSYS_ANTIMONIKER           = 3;
    MKSYS_ITEMMONIKER           = 4;
    MKSYS_POINTERMONIKER        = 5;
//  MKSYS_URLMONIKER            = 6;
    MKSYS_CLASSMONIKER          = 7;
    MKSYS_OBJREFMONIKER         = 8;
    MKSYS_SESSIONMONIKER        = 9;

    // system moniker types; returned from IsSystemMoniker.

    MKRREDUCE_ONE               = 3 SHL 16;
    MKRREDUCE_TOUSER            = 2 SHL 16;
    MKRREDUCE_THROUGHUSER       = 1 SHL 16;
    MKRREDUCE_ALL               = 0;

    // Storage element types
    STGTY_STORAGE               = 1;
    STGTY_STREAM                = 2;
    STGTY_LOCKBYTES             = 3;
    STGTY_PROPERTY              = 4;

    STREAM_SEEK_SET             = 0;
    STREAM_SEEK_CUR             = 1;
    STREAM_SEEK_END             = 2;

    LOCK_WRITE                  = 1;
    LOCK_EXCLUSIVE              = 2;
    LOCK_ONLYONCE               = 4;

    //Advise Flags
    ADVF_NODATA                 = 1;
    ADVF_PRIMEFIRST             = 2;
    ADVF_ONLYONCE               = 4;
    ADVF_DATAONSTOP             = 64;
    ADVFCACHE_NOHANDLER         = 8;
    ADVFCACHE_FORCEBUILTIN      = 16;
    ADVFCACHE_ONSAVE            = 32;


//****************************************************************************
//*  Notification Interfaces
//****************************************************************************/

    TYMED_HGLOBAL               = 1;
    TYMED_FILE                  = 2;
    TYMED_ISTREAM               = 4;
    TYMED_ISTORAGE              = 8;
    TYMED_GDI                   = 16;
    TYMED_MFPICT                = 32;
    TYMED_ENHMF                 = 64;
    TYMED_NULL                  = 0;

// Object Definitions for EnumObjects()
    OBJ_PEN                     = 1;
    OBJ_BRUSH                   = 2;
    OBJ_DC                      = 3;
    OBJ_METADC                  = 4;
    OBJ_PAL                     = 5;
    OBJ_FONT                    = 6;
    OBJ_BITMAP                  = 7;
    OBJ_REGION                  = 8;
    OBJ_METAFILE                = 9;
    OBJ_MEMDC                   = 10;
    OBJ_EXTPEN                  = 11;
    OBJ_ENHMETADC               = 12;
    OBJ_ENHMETAFILE             = 13;
    DATADIR_GET                 = 1;
    DATADIR_SET                 = 2;

// call type used by IMessageFilter::HandleIncomingMessage

    CALLTYPE_TOPLEVEL           = 1;      // toplevel call - no outgoing call
    CALLTYPE_NESTED             = 2;      // callback on behalf of previous outgoing call - should always handle
    CALLTYPE_ASYNC              = 3;      // aysnchronous call - can NOT be rejected
    CALLTYPE_TOPLEVEL_CALLPENDING = 4;  // new toplevel call with new LID
    CALLTYPE_ASYNC_CALLPENDING  = 5;   // async call - can NOT be rejected

// status of server call - returned by IMessageFilter::HandleIncomingCall
// and passed to  IMessageFilter::RetryRejectedCall

    SERVERCALL_ISHANDLED        = 0;
    SERVERCALL_REJECTED         = 1;
    SERVERCALL_RETRYLATER       = 2;

// Pending type indicates the level of nesting

    PENDINGTYPE_TOPLEVEL        = 1; // toplevel call
    PENDINGTYPE_NESTED          = 2;  // nested call

// return values of MessagePending

    PENDINGMSG_CANCELCALL       = 0; // cancel the outgoing call
    PENDINGMSG_WAITNOPROCESS    = 1; // wait for the return and don't dispatch the message
    PENDINGMSG_WAITDEFPROCESS   = 2;  // wait and dispatch the message

    EOAC_NONE                   = $0;
    EOAC_MUTUAL_AUTH            = $1;
    EOAC_STATIC_CLOAKING        = $20;
    EOAC_DYNAMIC_CLOAKING       = $40;
    EOAC_ANY_AUTHORITY          = $80;
    EOAC_MAKE_FULLSIC           = $100;
    EOAC_DEFAULT                = $800;

    // These are only valid for CoInitializeSecurity
    EOAC_SECURE_REFS            = $2;
    EOAC_ACCESS_CONTROL         = $4;
    EOAC_APPID                  = $8;
    EOAC_DYNAMIC                = $10;
    EOAC_REQUIRE_FULLSIC        = $200;
    EOAC_AUTO_IMPERSONATE       = $400;
    EOAC_NO_CUSTOM_MARSHAL      = $2000;
    EOAC_DISABLE_AAA            = $1000;

//****************************************************************************
//* ICOMThreadingInfo and enums
//****************************************************************************/

    APTTYPE_CURRENT             = -1;
    APTTYPE_STA                 = 0;
    APTTYPE_MTA                 = 1;
    APTTYPE_NA                  = 2;
    APTTYPE_MAINSTA             = 3;

    THDTYPE_BLOCKMESSAGES       = 0;
    THDTYPE_PROCESSMESSAGES     = 1;

    DCOM_NONE                   = $0;
    DCOM_CALL_COMPLETE          = $1;
    DCOM_CALL_CANCELED          = $2;

    COMBND_RPCTIMEOUT           = $1;  // Rpc transport-specific timeout.

//************************* Misc Enums wtypes.h ***********************************/

// Common typdefs used in API paramaters, gleamed from compobj.h

// memory context values; passed to CoGetMalloc

Const
    MEMCTX_TASK                 = 1;          // task (private) memory
    MEMCTX_SHARED               = 2;          // shared memory (between processes)
    MEMCTX_MACSYSTEM            = 3;          // on the mac, the system heap
    // these are mostly for internal use...
    MEMCTX_UNKNOWN              = -1;         // unknown context (when asked about it)
    MEMCTX_SAME                 = -2;         // same context (as some other pointer)


// For IRunningObjectTable::Register
    ROTFLAGS_REGISTRATIONKEEPSALIVE  = $1;
    ROTFLAGS_ALLOWANYCLIENT          = $2;

// Maximum size of comparison buffer for IROTData::GetComparisonData
    ROT_COMPARE_MAX                  = 2048;


// class context: used to determine what scope and kind of class object to use
// NOTE: this is a bitwise enum

    CLSCTX_INPROC_SERVER        = $0001;     // server dll (runs in same process as caller)
    CLSCTX_INPROC_HANDLER       = $0002;     // handler dll (runs in same process as caller)
    CLSCTX_LOCAL_SERVER         = $0004;     // server exe (runs on same machine; diff proc)
    CLSCTX_INPROC_SERVER16      = $0008;     // 16-bit server dll (runs in same process as caller)
    CLSCTX_REMOTE_SERVER        = $0010;     // remote server exe (runs on different machine)
    CLSCTX_INPROC_HANDLER16     = $0020;     // 16-bit handler dll (runs in same process as caller)
    CLSCTX_INPROC_SERVERX86     = $0040;     // Wx86 server dll (runs in same process as caller)
    CLSCTX_INPROC_HANDLERX86    = $0080;     // Wx86 handler dll (runs in same process as caller)
    CLSCTX_ESERVER_HANDLER      = $0100;     // handler dll (runs in the server process)
    CLSCTX_RESERVED =$0200;                  // reserved
    CLSCTX_NO_CODE_DOWNLOAD     = $0400;     // disallow code download from the Directory Service (if any) or the internet   -rahulth
    CLSCTX_NO_WX86_TRANSLATION  = $0800;
    CLSCTX_NO_CUSTOM_MARSHAL    = $1000;
    CLSCTX_ENABLE_CODE_DOWNLOAD = $2000;     // allow code download from the Directory Service (if any) or the internet
    CLSCTX_NO_FAILURE_LOG       = $04000;    // do not log messages about activation failure (should one occur) to Event Log
    CLSCTX_DISABLE_AAA          = $08000;    // Disable EOAC_DISABLE_AAA capability for this activation only
    CLSCTX_ENABLE_AAA           = $10000;    // Enable EOAC_DISABLE_AAA capability for this activation only
    CLSCTX_FROM_DEFAULT_CONTEXT = $20000;    // Begin this activation from the default context of the current apartment


// marshaling flags; passed to CoMarshalInterface
    MSHLFLAGS_NORMAL            = 0;   // normal marshaling via proxy/stub
    MSHLFLAGS_TABLESTRONG       = 1;   // keep object alive; must explicitly release
    MSHLFLAGS_TABLEWEAK         = 2;   // doesn't hold object alive; still must release
    MSHLFLAGS_NOPING            = 4;   // remote clients dont 'ping' to keep objects alive
    MSHLFLAGS_RESERVED1         = 8;   // reserved
    MSHLFLAGS_RESERVED2         = 16;  // reserved
    MSHLFLAGS_RESERVED3         = 32;  // reserved
    MSHLFLAGS_RESERVED4         = 64;  // reserved

// marshal context: determines the destination context of the marshal operation

    MSHCTX_LOCAL                = 0;   // unmarshal context is local (eg.shared memory)
    MSHCTX_NOSHAREDMEM          = 1;   // unmarshal context has no shared memory access
    MSHCTX_DIFFERENTMACHINE     = 2;   // unmarshal context is on a different machine
    MSHCTX_INPROC               = 3;   // unmarshal context is on different thread
    MSHCTX_CROSSCTX             = 4;   // unmarshal context is on different context



// #########################################################################
//
//  VARTYPE
//
// #########################################################################


{
    VARENUM usage key,

    * [V] - may appear in a VARIANT
    * [T] - may appear in a TYPEDESC
    * [P] - may appear in an OLE property set
    * [S] - may appear in a Safe Array


     VT_EMPTY            [V]   [P]     nothing
     VT_NULL             [V]   [P]     SQL style Null
     VT_I2               [V][T][P][S]  2 byte signed int
     VT_I4               [V][T][P][S]  4 byte signed int
     VT_R4               [V][T][P][S]  4 byte real
     VT_R8               [V][T][P][S]  8 byte real
     VT_CY               [V][T][P][S]  currency
     VT_DATE             [V][T][P][S]  date
     VT_BSTR             [V][T][P][S]  OLE Automation string
     VT_DISPATCH         [V][T]   [S]  IDispatch *
     VT_ERROR            [V][T][P][S]  SCODE
     VT_BOOL             [V][T][P][S]  True=-1, False=0
     VT_VARIANT          [V][T][P][S]  VARIANT *
     VT_UNKNOWN          [V][T]   [S]  IUnknown *
     VT_DECIMAL          [V][T]   [S]  16 byte fixed point
     VT_RECORD           [V]   [P][S]  user defined type
     VT_I1               [V][T][P][s]  signed char
     VT_UI1              [V][T][P][S]  unsigned char
     VT_UI2              [V][T][P][S]  unsigned short
     VT_UI4              [V][T][P][S]  unsigned long
     VT_I8                  [T][P]     signed 64-bit int
     VT_UI8                 [T][P]     unsigned 64-bit int
     VT_INT              [V][T][P][S]  signed machine int
     VT_UINT             [V][T]   [S]  unsigned machine int
     VT_INT_PTR             [T]        signed machine register size width
     VT_UINT_PTR            [T]        unsigned machine register size width
     VT_VOID                [T]        C style void
     VT_HRESULT             [T]        Standard return type
     VT_PTR                 [T]        pointer type
     VT_SAFEARRAY           [T]        (use VT_ARRAY in VARIANT)
     VT_CARRAY              [T]        C style array
     VT_USERDEFINED         [T]        user defined type
     VT_LPSTR               [T][P]     null terminated string
     VT_LPWSTR              [T][P]     wide null terminated string
     VT_FILETIME               [P]     FILETIME
     VT_BLOB                   [P]     Length prefixed bytes
     VT_STREAM                 [P]     Name of the stream follows
     VT_STORAGE                [P]     Name of the storage follows
     VT_STREAMED_OBJECT        [P]     Stream contains an object
     VT_STORED_OBJECT          [P]     Storage contains an object
     VT_VERSIONED_STREAM       [P]     Stream with a GUID version
     VT_BLOB_OBJECT            [P]     Blob contains an object
     VT_CF                     [P]     Clipboard format
     VT_CLSID                  [P]     A Class ID
     VT_VECTOR                 [P]     simple counted array
     VT_ARRAY            [V]           SAFEARRAY*
     VT_BYREF            [V]           void* for local use
     VT_BSTR_BLOB                      Reserved for system use
}

// VARENUM's

    VT_EMPTY                    = 0;
    VT_NULL                     = 1;
    VT_I2                       = 2;
    VT_I4                       = 3;
    VT_R4                       = 4;
    VT_R8                       = 5;
    VT_CY                       = 6;
    VT_DATE                     = 7;
    VT_BSTR                     = 8;
    VT_DISPATCH                 = 9;
    VT_ERROR                    = 10;
    VT_BOOL                     = 11;
    VT_VARIANT                  = 12;
    VT_UNKNOWN                  = 13;
    VT_DECIMAL                  = 14;
// VBA reserves 15 for future use
    VT_I1                       = 16;
    VT_UI1                      = 17;
    VT_UI2                      = 18;
    VT_UI4                      = 19;
    VT_I8                       = 20;
    VT_UI8                      = 21;
    VT_INT                      = 22;
    VT_UINT                     = 23;
    VT_VOID                     = 24;
    VT_HRESULT                  = 25;
    VT_PTR                      = 26;
    VT_SAFEARRAY                = 27;
    VT_CARRAY                   = 28;
    VT_USERDEFINED              = 29;
    VT_LPSTR                    = 30;
    VT_LPWSTR                   = 31;
// VBA reserves 32-35 for future use
    VT_RECORD                   = 36;
    VT_INT_PTR                  = 37;
    VT_UINT_PTR                 = 38;

    VT_FILETIME                 = 64;
    VT_BLOB                     = 65;
    VT_STREAM                   = 66;
    VT_STORAGE                  = 67;
    VT_STREAMED_OBJECT          = 68;
    VT_STORED_OBJECT            = 69;
    VT_BLOB_OBJECT              = 70;
    VT_CF                       = 71;
    VT_CLSID                    = 72;
    VT_VERSIONED_STREAM         = 73;

    VT_BSTR_BLOB                = $0fff;

    VT_VECTOR                   = $1000;
    VT_ARRAY                    = $2000;
    VT_BYREF                    = $4000;
    VT_RESERVED                 = $8000;

    VT_ILLEGAL                  = $ffff;
    VT_ILLEGALMASKED            = $0fff;
    VT_TYPEMASK                 = $0fff;



//
// Common typedefs for paramaters used in data view API's, gleamed
// from dvobj.h
//

// Data/View aspect; specifies the desired aspect of the object when
// drawing or getting data.

    DVASPECT_CONTENT            = 1;
    DVASPECT_THUMBNAIL          = 2;
    DVASPECT_ICON               = 4;
    DVASPECT_DOCPRINT           = 8;

//****** Storage types *************************************************


// Storage commit types

    STGC_DEFAULT                = 0;
    STGC_OVERWRITE              = 1;
    STGC_ONLYIFCURRENT          = 2;
    STGC_DANGEROUSLYCOMMITMERELYTODISKCACHE = 4;
    STGC_CONSOLIDATE            = 8;

    STGMOVE_MOVE                = 0;
    STGMOVE_COPY                = 1;
    STGMOVE_SHALLOWCOPY         = 2;

    STATFLAG_DEFAULT            = 0;
    STATFLAG_NONAME             = 1;
    STATFLAG_NOOPEN             = 2;

// #########################################################################
//
//   Constants for the call context
//

    WDT_INPROC_CALL             = ULONG($48746457);
    WDT_REMOTE_CALL             = ULONG($52746457);
    WDT_INPROC64_CALL           = ULONG($50746457);

    COLE_DEFAULT_PRINCIPAL {: pOleStr?} = pOleStr(-1);
    COLE_DEFAULT_AUTHINFO  {: pointer?} = pointer(-1);

TYPE
    VARTYPE     = USHORT;

    PCOAUTHIDENTITY    = ^TCOAUTHIDENTITY;
    _COAUTHIDENTITY    = Record
                          User           : PUSHORT;
                          UserLength     : ULONG;
                          Domain         : PUSHORT;
                          DomainLength   : ULong;
                          Password       : PUSHORT;
                          PassWordLength : ULong;
                          Flags          : ULong;
                          End;

   COAUTHIDENTITY      = _COAUTHIDENTITY;
   TCOAUTHIDENTITY     = _COAUTHIDENTITY;

   PCOAUTHINFO         = ^TCOAUTHINFO;
   COAuthInfo          = Record
                          AuthnSvc              : DWord;
                          AuthzSvc              : DWord;
                          ServerPrincName       : LPWSTR;
                          AuthnLevel            : DWord;
                          ImpersonationLevel    : DWord;
                          AuthIdentityData      : PCOAUTHIDENTITY;
                          Capabilities          : DWord;
                          END;
   TCOAUTHINFO         = COAUTHINFO;

   PCOSERVERINFO       = ^TCOSERVERINFO;
   _COSERVERINFO       = Record
                          dwReserved1 : DWord;
                          pwszName    : LPWSTR;
                          pAuthInfo   : PCoAuthInfo;
                          dwReserved2 : DWord;
                          end;
   TCOSERVERINFO       = _COSERVERINFO;
   PMultiQI            = ^Multi_QI;
   tagMULTI_QI         = Record
                          iid: piid;                   // pass this one in
                          itf: IUnknown;                // get these out (you must set to NULL before calling)
                          hr : Hresult;
                          END;
   MULTI_QI            = TagMULTI_QI;


   HContext            = Pointer;
   ApartmentID         = DWord;


//****** Critical Section Wrappers ***********************************

//   LCID                    = WORD;
//   LANGID                  = USHORT;

// #######################################################################
//
//  User marshal support for Windows data types.

//
//  Frequently used helpers: sized blobs
//
//      Never put [user_marshal] or [wire_marshal] on the helpers directly.
//

// Flagged blobs.

   _FLAGGED_BYTE_BLOB           = Record
                                   fFlags :  ULong;
                                   clSize :  ULong;
                                   abdata :  array[0..0] of byte;
                                   End;
   FLAGGED_BYTE_BLOB            = _FLAGGED_BYTE_BLOB;
   UP_FLAGGED_BYTE_BLOB         = ^FLAGGED_BYTE_BLOB;

   _FLAGGED_WORD_BLOB           = Record
                                   fFlags :  ULong;
                                   clSize :  ULong;
                                   abdata :  array[0..0] of USHORT;
                                   End;
   FLAGGED_WORD_BLOB            = _FLAGGED_WORD_BLOB;
   UP_FLAGGED_WORD_BLOB         = ^FLAGGED_WORD_BLOB;

   _FLAGGED_DWORD_BLOB          = Record
                                   fFlags :  ULong;
                                   clSize :  ULong;
                                   abdata :  array[0..0] of ULONG;
                                   End;
   FLAGGED_DWORD_BLOB           = _FLAGGED_DWORD_BLOB;
   FLAGGED_UP_DWORD_BLOB        = ^FLAGGED_DWORD_BLOB;

// Simple blobs.

   _BYTE_BLOB                   = Record
                                   clSize :  ULong;
                                   abdata :  array[0..0] of byte;
                                   End;
   BYTE_BLOB                    = _BYTE_BLOB;
   UP_BYTE_BLOB                 = ^BYTE_BLOB;

   _WORD_BLOB                   = Record
                                   clSize :  ULong;
                                   abdata :  array[0..0] of USHORT;
                                   End;
   WORD_BLOB                    = _WORD_BLOB;
   UP_WORD_BLOB                 = ^WORD_BLOB;

   _DWORD_BLOB                  = Record
                                   clSize :  ULong;
                                   abdata :  array[0..0] of ULONG;
                                   End;
   DWORD_BLOB                   = _DWORD_BLOB;
   UP_DWORD_BLOB                = ^DWORD_BLOB;

// Frequently used helpers with sized pointers.

   _BYTE_SIZEDARR               = Record
                                   clsize : ULong;
                                   Data   : PByte;
                                   End;
   BYTE_SIZEDARR                = _BYTE_SIZEDARR;

   _SHORT_SIZEDARR              = Record
                                   clsize : ULong;
                                   Data   : PSHORT;
                                   End;
   SHORT_SIZEDARR               = _SHORT_SIZEDARR;

   _LONG_SIZEDARR               = Record
                                   clsize : ULong;
                                   Data   : PLONG;
                                   End;
   LONG_SIZEDARR                = _LONG_SIZEDARR;
   HYPER                        = LONGLONG;
   PHYPER                       = ^HYPER;
   _HYPER_SIZEDARR              = Record
                                   clsize : ULong;
                                   Data   : PHYPER;
                                   End;
   HYPER_SIZEDARR               = _HYPER_SIZEDARR;


// #########################################################################
//
//  CLIPFORMAT
//

   userCLIPFORMAT               = Record
                                    FContext : Long;
                                    CASE INTEGER OF
                                      0 : (dwvalue : DWORD);
                                      1 : (szName : poleStr);
                                      End;

   wireCLIPFORMAT               = ^userCLIPFORMAT;


// #########################################################################
//
//  Good for most of the gdi handles.

   _GDI_NONREMOTE               = Record
                                    FContext : Long;
                                    Case Integer Of
                                     0 : (HInProc : Long);
                                     1 : (HRemote : DWORD_BLOB);
                                     END;
   GDI_NONREMOTE                = _GDI_NONREMOTE;

// #########################################################################
//
//  HGLOBAL
//
// A global may be Null or may be non-NULL with 0 length.

   _userHGLOBAL                 = Record
                                   FContext : Long;
                                   CASE Integer OF
                                      0 : (hInproc : Long);
                                      1 : (hRemote : UP_FLAGGED_BYTE_BLOB);
                                      2 : (hInproc64: int64);
                                      End;
   userHGlobal                  = _userHGLOBAL;
   wireHGLOBAL                  = ^userHGLOBAL;

// #########################################################################
//
//  HMETAFILE
//
   _userHMETAFILE               = Record
                                   fContext : Long;
                                   Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : up_byte_blob);
                                      2 : (hInProc64 : Int64);
                                      End;
   userHMETAFILE                = _userHMETAFILE;
   puserHMETAFILE               = ^userHMETAFILE;

// #########################################################################
//
//  HMETAFILEPICT
//

   _remoteMETAFILEPICT          = Record
                                    mm   : Long;
                                    xExt : Long;
                                    yExt : Long;
                                    mgf  : puserHMETAFILE;
                                    End;

   remoteMETAFILEPICT           = _remoteMETAFILEPICT;
   premoteMETAFILEPICT          = ^remoteMETAFILEPICT;

   _userHMETAFILEPICT           = Record
                                    fContext : Long;
                                    Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : premoteMETAFILEPICT);
                                      2 : (hInProc64 : Int64);
                                      End;
   userHMETAFILEPICT            = _userHMETAFILEPICT;


// #########################################################################
//
//  HENHMETAFILE
//

   _userHENHMETAFILE            = Record
                                    fContext : Long;
                                    Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : up_byte_blob);
                                      2 : (hInProc64 : Int64);
                                      End;
   userHENHMETAFILE             = _userHENHMETAFILE;
   puserHENHMETAFILE            = ^userHENHMETAFILE;

// #########################################################################
//
//  HBITMAP
//

// RemHBITMAP was just a byte blob, but the whole bitmap structure was copied
// at the beginning of the buffer.

// So, we take BITMAP fields from wingdi.x


   _userBITMAP                  = Record
                                   bmType,
                                   bmWidth,
                                   bmHeight,
                                   bmWidthBytes : Long;
                                   bmPlanes,
                                   bmBitsPixel  : Word;
                                   cvsize       : ULONG;
                                   buffer       : pbyte;
                                   End;

   userBITMAP                   = _userBITMAP;
   puserBITMAP                  = ^userBITMAP;

   _userHBITMAP                 = Record
                                    fContext : Long;
                                    Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : puserBITMAP);
                                      2 : (hInProc64 : Int64);
                                      End;
   userHBITMAP                  = _userHBITMAP;
   puserHBITMAP                 = ^userHBITMAP;


// #########################################################################
//
//  HPALETTE
//

// PALETTEENTRY is in wingdi.x, it is a struct with 4 bytes.
// LOGPALETTE   is in wingdi.x, it is a conf struct with paletteentries and
//                                    a version field

   _userHpalette                = Record
                                    fContext : Long;
                                    Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : logpalette);
                                      2 : (hInProc64 : Int64);
                                  End;
   userHpalette                 = _userHpalette;
   puserHpalette                = ^userHpalette;

// #########################################################################
//
//  Handles passed locally as longs.
//

   _RemotableHandle             = Record
                                    fContext : Long;
                                    Case Integer OF
                                      0 : (hInproc   : Long);
                                      1 : (hRemote   : Long);
                                      End;
   RemotableHandle              = _RemotableHandle;


   wireHWND                     = ^RemotableHandle;
   wireHMENU                    = ^RemotableHandle;
   wireHACCEL                   = ^RemotableHandle;
   wireHBRUSH                   = ^RemotableHandle;
   wireHFONT                    = ^RemotableHandle;
   wireHDC                      = ^RemotableHandle;
   wireHICON                    = ^RemotableHandle;
   HCursor                      = HICON;


   tagTEXTMETRICW               = Record
                                    tmHeight,
                                    tmAscent,
                                    tmDescent,
                                    tmInternalLeading,
                                    tmExternalLeading,
                                    tmAveCharWidth,
                                    tmMaxCharWidth,
                                    tmWeight,
                                    tmOverhang,
                                    tmDigitizedAspectX,
                                    tmDigitizedAspectY   : Long;
                                    tmFirstChar,
                                    tmLastChar,
                                    tmDefaultChar,
                                    tmBreakChar          : WCHAR;
                                    tmItalic,
                                    tmUnderlined,
                                    tmStruckOut,
                                    tmPitchAndFamily,
                                    tmCharSet            : BYTE;
                                  End;

   TEXTMETRICW                  = tagTEXTMETRICW;
   PTEXTMETRICW                 = ^TEXTMETRICW;
   LPTEXTMETRICW                = PTEXTMETRICW;
   wireHBITMAP                  = ^userHBITMAP;
   wireHPALETTE                 = ^userHPALETTE;
   wireHENHMETAFILE             = ^userHENHMETAFILE;
   wireHMETAFILE                = ^userHMETAFILE;
   wireHMETAFILEPICT            = ^userHMETAFILEPICT;
   HMetaFilePict                = Pointer;
   HLOCAL                       = HGLOBAL;
//  Date              = Double;

{****************************************************************************
 *  Binding Interfaces
 ****************************************************************************}

   tagBIND_OPTS                 = Record
                                    cvStruct,          //  sizeof(BIND_OPTS)
                                    grfFlags,
                                    grfMode,
                                    dwTickCountDeadline : DWord;
                                    End;
   TBind_Opts                   = tagBIND_OPTS;
   PBind_Opts                   = ^TBind_Opts;

   tagBIND_OPTS2_CPP            = Record
                                    dwTrackFlags,
                                    dwClassContext  : Dword;
                                    Locale          : LCID;
                                    ServerInfo      : pCoServerInfo;
                                    End;

   TBind_Opts2_CPP              = tagBIND_OPTS2_CPP;
   PBind_Opts2_CPP              = ^TBind_Opts2_CPP;


   tagBind_OPTS2                = Record
                                    cvStruct,          //  sizeof(BIND_OPTS)
                                    grfFlags,
                                    grfMode,
                                    dwTickCountDeadline : DWord;
                                    dwTrackFlags,
                                    dwClassContext      : DWord;
                                    Locale              : LCID;
                                    ServerInfo          : pCoServerInfo;
                                    End;

   TBind_Opts2                  = tagBIND_OPTS2;
   PBind_Opts2                  = ^TBind_Opts2;

// ****************************************************************************
// *  Structured Storage Interfaces
// ****************************************************************************



   tagSTATSTG                   = record
                                    pwcsName      : POleStr;
                                    dwType        : DWord;
                                    cbSize        : ULarge_integer;
                                    mtime         : TFileTime;
                                    ctime         : TFileTime;
                                    atime         : TFileTime;
                                    grfMode       : DWord;
                                    grfLocksSupported : DWord;
                                    clsid         : TCLSID;
                                    grfStateBits  : DWord;
                                    reserved      : DWord;
                                    end;

   TStatStg                     = tagSTATSTG;
   PStatStg                     = ^TStatStg;
   STATSTG                      = TStatStg;

{    TagRemSNB = Record
                   ulCntStr  : ULong;
                   ulCntChar : ULong;
                   [size_is(ulCntChar)] OLECHAR rgString[];
                 End;
        RemSNB=TagRemSNB
        WireSNB=^RemSNB}
   SNB                          = ^PoleStr;
   tagDVTARGETDEVICE            = Record
                                    tdSize                     : DWord;
                                    tdDriverNameOffset,
                                    tdDeviceNameOffset,
                                    tdPortNameOffset,
                                    tdExtDevmodeOffset         : Word;
                                    Data                       : Record End;
                                    End;

   DVTARGETDEVICE               = TagDVTARGETDEVICE;
   PDVTARGETDEVICE              = ^tagDVTARGETDEVICE;
   LPCLIPFORMAT                 = ^TCLIPFORMAT;
   TCLIPFORMAT                  = Word;
   CLIPFORMAT                   = TCLIPFORMAT;

   tagFORMATETC                 = Record
                                    CfFormat :  Word {TCLIPFORMAT};
                                    Ptd      : PDVTARGETDEVICE;
                                    dwAspect : DWORD;
                                    lindex   : Long;
                                    tymed    : DWORD;
                                    End;
   FORMATETC                    = TagFORMATETC;
   TFORMATETC                   = FORMATETC;
   LPFORMATETC                  = ^FORMATETC;

    // Stats for data; used by several enumerations and by at least one
    // implementation of IDataAdviseHolder; if a field is not used, it
    // will be NULL.


   tagRemSTGMEDIUM              = Record
                                    tymed                     : DWord;
                                    dwHandleType              : DWord;
                                    pData,
                                    pUnkForRelease,
                                    cbData                    : ULong;
                                    Data                      : Record end;
                                    End;

   RemSTGMEDIUM                 = TagRemSTGMedium;

   TagSTGMEDIUM                 = Record
                                    Tymed : DWord;
                                    Case Integer Of
                                      0 : (HBITMAP             : hBitmap;       PUnkForRelease :  Pointer {IUnknown});
                                      1 : (HMETAFILEPICT       : hMetaFilePict );
                                      2 : (HENHMETAFILE        : hEnhMetaFile  );
                                      3 : (HGLOBAL             : hGlobal       );
                                      4 : (lpszFileName        : LPOLESTR    );
                                      5 : (pstm                : Pointer{IStream}  );
                                      6 : (pstg                : Pointer{IStorage} );
                                      End;
   USTGMEDIUM                   = TagSTGMEDIUM;
   STGMEDIUM                    = USTGMEDIUM;

//
//  wireSTGMEDIUM
//
// These flags are #defined (not enumerated) in wingdi.
// We need to repeat #defines to avoid conflict in the generated file.
//

   _GDI_OBJECT                  = Record
                                    ObjectType : DWord;
                                    Case Integer Of
                                      0 : (HBitmap : WireHBITMAP);
                                      1 : (hPalette: wireHPALETTE);
                                      2 : (hGeneric: wireHGLOBAL);
                                      END;
   GDI_OBJECT                   = _GDI_OBJECT;


   _userSTGMEDIUM               = Record
                                    tymed : DWORD;
                                    Case Integer OF
                                      0 : (hMetaFilePict : wireHMETAFILEPICT;punkforrelease:Pointer {IUnknown});
                                      1 : (hHEnhMetaFile : wireHENHMETAFILE);
                                      2 : (hGdiHandle    : ^GDI_OBJECT);
                                      3 : (HGlobal       : wireHGLOBAL);
                                      4 : (lpszFileName  : LPOLESTR);
                                      5 : (pstm          : ^BYTE_BLOB);
                                      6 : (pstg          : ^BYTE_BLOB);
                                      END;

   userSTGMEDIUM                = _userSTGMEDIUM;


   LPSTGMEDIUM                  = ^STGMEDIUM;

   _userFLAG_STGMEDIUM          = Record
                                    ContextFlags,
                                    fPassOwnership  : Long;
                                    stgmed          : userSTGMEDIUM;
                                    End;

   userFLAG_STGMEDIUM           = _userFLAG_STGMEDIUM;

   wireFLAG_STGMEDIUM           = ^userFLAG_STGMEDIUM;


   _FLAG_STGMEDIUM              = Record
                                    ContextFlags,
                                    fPassOwnership   : Long;
                                    Stgmed           : STGMEDIUM;
                                    End;
   FLAG_STGMEDIUM               = _FLAG_STGMEDIUM;


// additional interface information about the incoming call
   tagINTERFACEINFO             = Record
                                    Unk     : IUnknown;   // the pointer to the object
                                    IID     : Tiid;       // interface id
                                    wMethod : WORD;        // interface method
                                    End;

   INTERFACEINFO                = tagINTERFACEINFO;
   LPINTERFACEINFO              = ^INTERFACEINFO;
   RPCOLEDATAREP                = ULong;
   tagRPCOLEMESSAGE             = Record
                                    Reserved1          : Pointer;
                                    DataRepresentation : RPCOLEDATAREP;
                                    Buffer             : Pointer;
                                    cbBuffer,
                                    IMethod            : ULong;
                                    Reserved2          : Array[0..4] Of Pointer;
                                    rpcFlags           : ULong;
                                    End;

   RPCOLEMESSAGE                = tagRPCOLEMESSAGE;
   PRPCOLEMESSAGE               = ^RPCOLEMESSAGE;

   tagStorageLayout             = Record
                                    LayoutType       : Dword;
                                    pwcsElementName  : POleStr;
                                    cOffset,
                                    cBytes           : Large_Integer;
                                    End;

   StorageLayout                = tagStorageLayout;

   tagSTATDATA                  = Record
                                                                // field used by:
                                    FORMATETC   : Tformatetc;   // EnumAdvise, EnumData (cache), EnumFormats
                                    advf        : DWord;        // EnumAdvise, EnumData (cache)
                                    padvSink    : Pointer {IAdviseSink};  // EnumAdvise
                                    dwConnection: DWord;        // EnumAdvise
                                    End;
   STATDATA                     = TagStatData;
   LPStatData                   = ^StatData;


// Forward interfaces.

   IStream             = Interface;
   IMoniker            = Interface;
   IEnumMoniker        = Interface;
   IEnumString         = Interface;
   IRunningObjectTable = Interface;
   IStorage            = Interface;
   IEnumSTATSTG        = Interface;
   IAdviseSink         = Interface;
   IBindCtx            = Interface;
   IAsyncManager       = Interface;
   ICallFactory        = Interface;
   ISynchronize        = Interface;


{****************************************************************************
 *  Component Object Interfaces
 ****************************************************************************}


     IMarshal = Interface(IUnknown)
        ['{00000003-0000-0000-C000-000000000046}']
        Function GetUnmarshalClass ( Const riid: TIID; pv:Pointer; Const dwDestContext:DWord;
                    pvDestContext:Pointer; Const mshlflags:DWORD;out LCid : TCLSID ):HResult;Stdcall;
        Function GetMarshalSizeMax ( Const Riid: TIID; {in, unique} pv:Pointer; Const dwDestContext : DWord;
                   {in, unique} pvDestContext:Pointer; Const mshlflags : DWord; out pSize : PDWord ): HResult;Stdcall;
        Function MarshalInterface ( Const {in, unique} pStm: IStream; Const riid: TIID; {in, unique} pv:Pointer;
                   Const dwDestContext:DWord; {in, unique} pvDestContext:Pointer; Const mshlflags:DWord ): HRESULT;Stdcall;
        Function UnmarshalInterface ( {[in, unique]} Const pStm:IStream; Const riid: TIID;
                   out ppv ): HResult;Stdcall;
        Function ReleaseMarshalData ( {[in, unique]} Const Strm: IStream ):HResult;Stdcall;
        Function DisconnectObject ( Const dwReserved:DWord ):HRESULT;Stdcall;
        END;


     IMarshal2 = Interface(IMarshal)
        ['{000001cf-0000-0000-C000-000000000046}']
        End;

     IMalloc   = Interface(IUnknown)
        ['{00000002-0000-0000-C000-000000000046}']
        Function  Alloc(cb :size_t):Pointer; Stdcall;
        Function  Realloc (pv :pointer;cb:size_t):Pointer;stdcall;
        Procedure Free({[in]} pv: pointer); Stdcall;
        Function  GetSize(pv:pointer):size_t;stdcall;
        Function  DidAlloc(pv:pointer):Longint;stdcall;
        procedure HeapMinimize; stdcall;
        End;

     IMallocSpy = Interface(IUnknown)
        ['{0000001d-0000-0000-C000-000000000046}']

        Function  PreAlloc(cbrequest:Size_t):Longint; StdCall;
        function  PostAlloc(Pactual:Pointer):Pointer;StdCall;
        Function  PreFree(pRequest:Pointer;fSpyed:bool):pointer;StdCall;
        Procedure PostFree(fspyed:Bool);Stdcall;
        Function  PreRealloc(pRequest:Pointer;cbRequest:Size_t;Out ppNewRequest:Pointer;
                        fSpyed:Bool):Size_t;Stdcall;
        Function  PostRealloc(pactual:Pointer;fspyed:Bool):pointer;Stdcall;
        Function  PreGetSize(pRequest:pointer;fSpyed:Bool):Pointer;StdCall;
        Function  PostGetSize(cbactual:Size_t;fSpyed:Bool):Size_t;StdCall;
        Function  PreDidAlloc(pRequest:pointer;fSpyed:Bool):pointer;stdcall;
        Function  PostDidAlloc(pRequest:pointer;fSpyed:Bool;Factual:Longint):pointer;stdcall;
        Procedure PreHeapMinimize;StdCall;
        Procedure PostHeapMinimize;StdCall;
        End;

     IStdMarshalInfo = Interface(IUnknown)
       ['{00000018-0000-0000-C000-000000000046}']
       Function GetClassForHandler (dwDestContext : DWord;pvDestContext:pointer;out Clsid : Pclsid ):HResult;Stdcall;
       End;


     IExternalConnection = Interface(IUnknown)
       ['{00000019-0000-0000-C000-000000000046}']
       Function AddConnection    (ExtConn: DWord; Reserved: DWord):DWord;Stdcall;
       Function ReleaseConnection(extconn: DWord; Reserved: Dword;FLastReleaseCloses: Bool):DWord;StdCall;
      End;


      IMultiQI = Interface(IUnknown)
        ['{00000020-0000-0000-C000-000000000046}']
//if (__midl >= 500)
//    async_uuid(000e0020-0000-0000-C000-000000000046)
//endif
        Function QueryMultipleInterfaces(cMQIs:Ulong;pMQIs:pMultiQI):HResult;StdCall;
        END;

     IInternalUnknown=Interface(IUnknown)
        ['{00000021-0000-0000-C000-000000000046}']
        Function QueryInternalInterface(riid:TIID;Out ppv:Pointer):HResult;StdCall;
        END;


     IEnumUnknown = Interface(IUnknown)
        ['{00000100-0000-0000-C000-000000000046}']
        //    pointer_default(unique)
     Function Next(Celt:Ulong;out rgelt:IUnknown;out pCeltFetched:pulong):HRESULT;StdCall;
//    HRESULT RemoteNext(        [in] ULONG celt,        [out, size_is(celt), length_is(*pceltFetched)]        IUnknown **rgelt,        [out] ULONG *pceltFetched);
     Function Skip(Celt:Ulong):HResult;StdCall;
     Function Reset():HResult;
     Function Close(Out ppenum: IEnumUnknown):HResult;
     END;


    IBindCtx = Interface (IUnknown)
       ['{0000000e-0000-0000-C000-000000000046}']
       Function RegisterObjectBound(Const punk:IUnknown):HResult; stdCall;
       Function RevokeObjectBound (Const Punk:IUnknown):HResult;  stdCall;
       Function ReleaseBoundObjects :HResult;  StdCall;
       Function SetBindOptions(Const bindOpts:TBind_Opts):HResult;  stdCall;
//       Function RemoteSetBindOptions(Const bind_opts: TBind_Opts2):HResult;StdCall;
       Function GetBindOptions(var BindOpts:TBind_Opts):HResult;  stdCall;
//       Function RemoteGetBindOptions(Var bind_opts: TBind_Opts2):HResult;StdCall;
       Function GetRunningObjectTable(Out rot : IRunningObjectTable):Hresult; StdCall;
       Function RegisterObjectParam(Const pszkey:LPOleStr;const punk:IUnknown):HResult;
       Function GetObjectParam(Const pszkey:LPOleStr; out punk: IUnknown):HResult; StdCall;
       Function EnumObjectParam (out enum:IEnumString):Hresult;StdCall;
       Function RevokeObjectParam(pszKey:LPOleStr):HResult;StdCall;
       End;


    IEnumMoniker = Interface (IUnknown)
       ['{00000102-0000-0000-C000-000000000046}']
       Function Next(celt:ULong; out Elt;out celftfetched: ULong):HResult; StdCall;
//     Function RemoteNext(Celt:ULong; Out rgelt;out celtfetched :ULong):Hresult; StdCall;
       Function Skip(celt:Ulong):HResult; StdCall;
       Function Reset:HResult; StdCall;
       Function Close(out penum:IEnumMoniker):HResult;StdCall;
       End;


    IRunnableObject = Interface(IUnknown)
    ['{00000126-0000-0000-C000-000000000046}']
       Function GetRunningClass(Out clsid:Tclsid):Hresult; StdCall;
       Function Run(Const pb: IBindCtx):HResult; StdCall;
       Function IsRunning:Bool; StdCall;
//     Function RemoteIsRunning:Bool; StdCall;
       Function LockRunning(FLock,FLastUnlockClose:BOOL):HResult; StdCall;
       Function SetContainedObject(fContained:Bool):Hresult;Stdcall;
       End;

    IRunningObjectTable = Interface (IUnknown)
       ['{00000010-0000-0000-C000-000000000046}']
       Function Register  (grfFlags :DWord;const unkobject:IUnknown;Const mkObjectName:IMoniker;Out dwregister:DWord):HResult;StdCall;
       Function Revoke    (dwRegister:DWord):HResult; StdCall;
       Function IsRunning (Const mkObjectName: IMoniker):HResult;StdCall;
       Function GetObject (Const mkObjectName: IMoniker; Out punkObject:IUnknown):HResult; StdCall;
       Function NoteChangeTime(dwRegister :DWord;Const FileTime: TFileTime):HResult;StdCall;
       Function GetTimeOfLastChange(Const mkObjectName:IMoniker;Out filetime:TFileTime):HResult; StdCall;
       Function EnumRunning (Out enumMoniker: IEnumMoniker):HResult; StdCall;
       End;

    IPersist = Interface (IUnknown)
       ['{0000010c-0000-0000-C000-000000000046}']
       Function GetClassId(clsid:TClsId):HResult; StdCall;
       End;

    IPersistStream = Interface(IPersist)
       ['{00000109-0000-0000-C000-000000000046}']
       Function IsDirty:HResult; StdCall;
       Function Load(Const stm: IStream):HResult; StdCall;
       Function Save(Const stm: IStream;fClearDirty:Bool):HResult;StdCall;
       Function GetSizeMax(Out cbSize:ULarge_Integer):HResult; StdCall;
       End;

    PIMoniker = ^IMoniker;
    IMoniker = Interface (IPersistStream)
      ['{0000000f-0000-0000-C000-000000000046}']
      Function BindToObject (const pbc:IBindCtx;const mktoleft:IMoniker; RiidResult:TIID;Out vresult):HResult;StdCall;
//    Function RemoteBindToObject (const pbc:IBindCtx;const mktoleft:IMoniker;RiidResult:TIID;Out vresult):HResult;StdCall;
      Function BindToStorage(Const Pbc:IBindCtx;Const mktoLeft:IMoniker; Riid:TIID;Out vobj):HResult; StdCall;
//    Function RemoteBindToStorage(Const Pbc:IBindCtx;Const mktoLeft:IMoniker; Riid:TIID;Out vobj):HResult; StdCall;
      Function Reduce (const pbc:IBindCtx; dwReduceHowFar:DWord; mktoLeft: PIMoniker; Out mkReduced:IMoniker):HResult; StdCall;
      Function ComposeWith(Const MkRight:IMoniker;fOnlyIfNotGeneric:BOOL; OUT mkComposite:IMoniker):HResult; StdCall;
      Function Enum(fForward:Bool;Out enumMoniker:IEnumMoniker):HResult;StdCall;
      Function IsEqual(Const mkOtherMoniker:IMoniker):HResult;StdCall;
      Function Hash   (Out dwHash:Dword):HResult;StdCall;
      Function IsRunning(Const bc:IBindCtx;Const MkToLeft:IMoniker;Const mknewlyRunning:IMoniker):HResult;StdCall;
      Function GetTimeOfLastChange(Const bc:IBindCtx;Const mkToLeft:IMoniker; out ft : FileTime):HResult; StdCall;
      Function Inverse(out mk : IMoniker):HResult; StdCall;
      Function CommonPrefixWith (Const mkOther:IMoniker):HResult; StdCall;
      Function RelativePathTo(Const mkother:IMoniker; Out mkRelPath : IMoniker):HResult;StdCall;
      Function GetDisplayName(Const bc:IMoniker;const mktoleft:IMoniker;Out szDisplayName: pOleStr):HResult; StdCall;
      Function ParseDisplayName(Const bc:IBindCtx;Const mkToLeft:IMoniker;szDisplayName:POleStr;out cheaten:ULong;out mkOut:IMoniker):HResult; StdCall;
      Function IsSystemMonitor(Out dwMkSys:DWord):HResult;StdCall;
      End;

    IROTData = Interface (IUnknown)
       ['{f29f6bc0-5021-11ce-aa15-00006901293f}']
       Function GetComparisonData(out data; cbMax:ULong;out cbData:ULong):HResult;StdCall;
       End;


    IEnumString = Interface (IUnknown)
       ['{00000101-0000-0000-C000-000000000046}']
       Function Next(Celt:ULong;Out xcelt;Out Celtfetched:ULong):HResult; StdCall;
//     Function RemoteNext(Celt:ULong; Out celt;Out Celtfetched:ULong):HResult; StdCall;
       Function Skip (Celt:ULong):Hresult;StdCall;
       Function Reset:HResult;StdCall;
       Function Clone(Out penum:IEnumString):HResult;StdCall;
       End;


       ISequentialStream = interface(IUnknown)
          ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
          function Read(pv : Pointer;cb : DWord;pcbRead : PDWord) : HRESULT;stdcall;
          function Write(pv : Pointer;cb : DWord;pcbWritten : PDWord): HRESULT;stdcall;
       end;

       IStream = interface(ISequentialStream)
          ['{0000000C-0000-0000-C000-000000000046}']
          function Seek(dlibMove : Large_integer; dwOrigin: Longint;
            out libNewPosition : Large_Integer): HResult; stdcall;
          function SetSize(libNewSize : Large_Integer) : HRESULT;stdcall;
          function CopyTo(stm: IStream;cb : Large_Integer;out cbRead : Large_Integer;
            out cbWritten: Large_Integer) : HRESULT;stdcall;
          function Commit(grfCommitFlags : Longint) : HRESULT; stdcall;
          function Revert : HRESULT; stdcall;
          function LockRegion(libOffset : Large_Integer;cb : Large_Integer;
            dwLockType: Longint) : HRESULT;stdcall;
          function UnlockRegion(libOffset: Large_Integer;cb: Large_Integer;
            dwLockType: Longint) : HRESULT;stdcall;
          function Stat(out statstg : TStatStg; grfStatFlag: Longint): HRESULT;stdcall;
          function Clone(out stm : IStream) : HRESULT; stdcall;
       end;



    IEnumSTATSTG = Interface (IUnknown)
       ['{0000000d-0000-0000-C000-000000000046}']
        Function Next (Celt:ULong;Out xcelt;pceltfetched : PUlong):HResult; StdCall;
//     Function RemoteNext(Celt:Ulong; Out Celt;pceltfetched : PUlong);
       Function Skip(Celt:ULong):HResult; StdCall;
       Function Reset:HResult; StdCall;
       Function Clone(Out penum:IEnumStatSTG):HResult; StdCall;
       End;




    IStorage = Interface (IUnknown)
       ['{0000000b-0000-0000-C000-000000000046}']
       Function CreateStream(pwcsname:POleStr;GrfMode,Reserved1,Reserved2 : DWord; Out stm : IStream):HResult; StdCall;
       Function OpenStream(pwcsname:POleStr;Reserved1:Pointer;GrfMode,Reserved2 : DWord; Out stm : IStream):HResult; StdCall;
//       Function RemouteOpenStream(pwcsname:POleStr;cbReserved1:ULong;reserved1:pbyte;GrfMode,Reserved2 : DWord; Out stm : IStream):HResult; StdCall;
       Function CreateStorage(pwcsname:POleStr;GrfMode,Reserved1,Reserved2 : DWord; Out stm : IStorage):HResult; StdCall;
       Function OpenStorage(pwcsname:POleStr;Const stgPriority:IStorage;grfmode : DWord;Const SNBExclude :SNB;reserved:DWord;Out stm : IStorage):HResult; StdCall;
       Function CopyTo(ciidExclude:DWord; rgiidexclude:piid; const snbexclude:SNB;const pstg : IStorage):HResult;StdCall;
       Function MoveElementTo(wcsName:POleStr;Const pstgDest : IStorage;
                                wcvsNewName:POleStr; GrfFlags:DWord):Hresult; StdCall;
       Function Commit(grfCommitFlags:Dword):Hresult; StdCall;
       Function Revert:HResult; StdCall;
       Function EnumElements(Reserved1 :Dword;Reserved2:Pointer;Reserved3:DWord;Out penum:IEnumStatStg):HResult;StdCall;
       Function RemoteEnumElements(Reserved1 :Dword;cbReserved2:ULong;Reserved2:pbyte;reserved3:DWord;Out penum:IEnumStatStg):HResult;StdCall;
       Function DestroyElement(wcsName: POleStr):HResult;StdCall;
       Function RenameElement(wcsoldName: POleStr;wcsnewName: POleStr):HResult;StdCall;
       Function SetElementTimes(wcsName:POleStr; Const pctime,patime,pmtime : FileTime):HResult;StdCall;
       Function SetClass(Const ClasId: TClsID):HResult;StdCall;
       Function SetStateBits(grfStateBits:DWord;grfMask:DWord):HResult;StdCall;
       Function Stat(Out pStatStg:StatStg;grfStatFlag:DWord):HResult;StdCall;
       End;

    IPersistFile = Interface (IPersist)
       ['{0000010b-0000-0000-C000-000000000046}']
       Function IsDirty:HResult;StdCall;
       Function Load(FileName:POleStr;dwMode:DWord):HResult;StdCall;
       Function Save(FileName:POleStr;fremember:Bool):HResult;StdCall;
       Function SaveCompleted(FileName:POleStr):HResult;StdCall;
       Function GetCurFIle(Out FileName:POleStr):HResult;StdCall;
       End;


    IPersistStorage = Interface (IPersist)
       ['{0000010a-0000-0000-C000-000000000046}']
       Function IsDirty:HResult;StdCall;
       Function InitNew(const pstg:IStorage):HResult;StdCall;
       Function Load(const pstg:IStorage):HResult;StdCall;
       Function Save(const pstg:IStorage;FSameAsLoad:Boolean):HResult;StdCall;
       Function SaveCompleted(const pstg:IStorage):HResult;StdCall;
       Function HandsOffStorage:HResult;StdCall;
       End;

    ILockBytes = Interface (IUnknown)
       ['{0000000a-0000-0000-C000-000000000046}']
       Function ReadAt(ulOffset:ULarge_Integer;pv:Pointer;cb:Ulong; Out pcbRead:ULong):HResult; StdCall;
//       Function RemoteReadAt(ulOffset:ULarge_Integer;pv:Pointer;cb:Ulong; Out pcbRead:ULong):HResult; StdCall;
       Function WriteAt(ulOffset:ULarge_Integer;pv:Pointer;cb:Ulong; Out pcbWritten:ULong):HResult; StdCall;
//       Function RemoteWriteAt(ulOffset:ULarge_Integer;pv:Pointer;cb:Ulong; Out pcbWritten:ULong):HResult; StdCall;
       Function Flush:HResult;StdCall;
       Function SetSize(cb:ULarge_Integer):HResult;StdCall;
       Function LockRegion(LibOffSet:ULarge_Integer;cb:ULarge_Integer;dwLockType:DWord):HResult;StdCall;
       Function UnlockRegion(LibOffSet:ULarge_Integer;cb:ULarge_Integer;dwLockType:DWord):HResult;StdCall;
       Function Stat(Out pstatstg:STATSTG;grfstatFlag:DWord):HResult;StdCall;
       End;


   IEnumFORMATETC = Interface (IUnknown)
     ['{00000103-0000-0000-C000-000000000046}']
     Function Next(Celt:ULong;Out Rgelt:FormatEtc;Out pceltFetched:ULong):HResult; StdCall;
//     Function RemoteNext(Celt:ULong;Out Rgelt:FormatEtc;Out pceltFetched:ULong):HResult; StdCall;
     Function Skip(Celt:ULong):HResult;StdCall;
     Function Reset:HResult;StdCall;
     Function Clone(out penum:IEnumFORMATETC):HResult;StdCall;
     End;

    IEnumSTATDATA = Interface (IUnknown)
        ['{00000105-0000-0000-C000-000000000046}']
        Function Next(Celt:ULong;Out Rgelt:statdata;Out pceltFetched:ULong):HResult; StdCall;
//      Function RemoteNext(Celt:ULong;Out Rgelt:statdata;Out pceltFetched:ULong):HResult; StdCall;
        Function Skip(Celt:ULong):HResult;StdCall;
        Function Reset:HResult;StdCall;
        Function Clone(out penum:IEnumstatdata):HResult;StdCall;
        End;



    IRootStorage = Interface (IUnknown)
        ['{00000012-0000-0000-C000-000000000046}']
        Function SwitchToFile(pszfile:PoleStr):HResult;StdCall;
        End;



    IAdviseSink = Interface (IUnknown)
        ['{0000010f-0000-0000-C000-000000000046}']
    {$ifdef midl500} ['{00000150-0000-0000-C000-000000000046}'] {$endif}
        Procedure OnDataChange (Const pformatetc : Formatetc;const pstgmed : STGMEDIUM); StdCall;
        Procedure OnViewChange (dwAspect : DWord; lindex : Long); StdCall;
        Procedure OnRename (Const pmk : IMoniker); StdCall;
        Procedure OnSave; StdCall;
        Procedure OnClose; StdCall;
        End;

    IAdviseSink2 = Interface (IAdviseSink)
       ['{00000125-0000-0000-C000-000000000046}']
        Procedure OnLinkSrcChange(Const Pmk: IMoniker); StdCall;
        End;


    IDataObject = Interface (IUnknown)
       ['{0000010e-0000-0000-C000-000000000046}']
       Function GetData(Const formatetcIn : FORMATETC;Out medium : STGMEDIUM):HRESULT; STDCALL;
       Function GetDataHere(CONST pformatetc : FormatETC; Out medium : STGMEDIUM):HRESULT; STDCALL;
       Function QueryGetData(const pformatetc : FORMATETC):HRESULT; STDCALL;
       Function GetCanonicalFormatTEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult; STDCALl;
       Function SetData (Const pformatetc : FORMATETC;const medium:STGMEDIUM;FRelease : BOOL):HRESULT; StdCall;
       Function EnumFormatEtc(dwDirection : DWord; OUT enumformatetc : IENUMFORMATETC):HRESULT; StdCall;
       Function DAdvise(const formatetc : FORMATETC;advf :DWORD; CONST AdvSink : IAdviseSink;OUT dwConnection:DWORD):HRESULT;StdCall;
       Function DUnadvise(dwconnection :DWord) :HRESULT;StdCall;
       Function EnumDAvise(Out enumAdvise : IEnumStatData):HResult;StdCall;
       End;


    IDataAdviseHolder = Interface (IUnknown)
       ['{00000110-0000-0000-C000-000000000046}']
       Function Advise    (CONST pdataObject : IDataObject;CONST fetc:FORMATETC;advf : DWORD;Const pAdvise:IAdviseSink;Out DwConnection:DWord):HResult; StdCall;
       Function Unadvise  (dwConnection:Dword):HResult; StdCall;
       Function EnumAdvise(out penumAdvise : IEnumStatData):HResult;StdCall;
       Function SendOnDataChange(const pDataObject :IDataObject;DwReserved,advf : DWord):HResult; StdCall;
       End;




    IMessageFilter = Interface (IUnknown)
       ['{00000016-0000-0000-C000-000000000046}']
       Function HandleInComingCall(dwCallType :DWord;htaskCaller : HTASK; dwTickCount: DWORD;CONST sinterfaceinfo:InterfaceInfo):DWord; StdCall;
       Function RetryRejectedCall (htaskCallee:HTASK; dwTickCount : DWord; dwRejectType : Dword):DWord; StdCall;
       Function MessagePending    (htaskCallee:HTASK; dwTickCount : DWord; dwPendingType : Dword):DWord; StdCall;
       End;

//****************************************************************************
//*  Object Remoting Interfaces
//****************************************************************************



    IRpcChannelBuffer = Interface (IUnknown)
       ['{D5F56B60-593B-101A-B569-08002B2DBF7A}']
       Function GetBuffer (Const pMesasge : RPCOLEMESSAGE;Const riid :TIId):HResult; StdCall;
       Function SendReceive(Var pMessage : RPCOLEMESSAGE; Out PStatus : ULong):HResult; StdCall;
       Function FreeBuffer(Const pMessage : RPCOLEMESSAGE):HResult; StdCall;
       Function GetDestCTX(Out dwDestContext : DWord;Out pvDestContext : Pointer):HResult; StdCall;
       Function IsConnected:HResult; StdCall;
       End;

    IRpcChannelBuffer2 = Interface (IRpcChannelBuffer)
       ['{594f31d0-7f19-11d0-b194-00a0c90dc8bf}']
       Function GetProtocolVersion(Var dwVersion : DWord):HResult; StdCall;
       End;


    IAsyncRpcChannelBuffer = Interface (IRpcChannelBuffer2)
       ['{a5029fb6-3c34-11d1-9c99-00c04fb998aa}']
       Function Send(Var Msg: RPCOLEMESSAGE;Const pSync : ISynchronize;Out PulStatus : ULong):HResult; StdCall;
       Function Receive(Var Msg: RPCOLEMESSAGE;Out PulStatus : ULong):HResult; StdCall;
       Function GetDestCTXEx(Out MSG : RPCOLEMESSAGE;Out vDestContext : DWord;Out pvDestContext : Pointer ):HResult StdCall;
       End;

    IRpcChannelBuffer3 = Interface (IRpcChannelBuffer2)
       ['{25B15600-0115-11d0-BF0D-00AA00B8DFD2}']
       Function Send(Var msg : RPCOLEMESSAGE;Out ulStatus : ULONG):HResult; StdCall;
       Function Receive(Var msg : RPCOLEMESSAGE;ulSize : ULong;Out ulStatus : ULONG):HResult; StdCall;
       Function Cancel (Const msg : RPCOLEMESSAGE):HResult; StdCall;
       Function GetCallContext(Const msg : RPCOLEMESSAGE; Const riid : TIID; Out pInterface : Pointer):HResult; StdCall;
       Function GetDestCTXEx(Const Msg : RPCOLEMESSAGE;Out vDestContext : DWord;Out pvDestContext : Pointer ):HResult StdCall;
       Function GetState(Const Msg : RPCOLEMESSAGE;Out State: DWord):HResult StdCall;
       Function RegisterAsync(Const Msg : RPCOLEMESSAGE;Const asyncmgr : IAsyncManager):HResult StdCall;
       End;

    IRpcSyntaxNegotiate = Interface (IUnknown)
       ['{58a08519-24c8-4935-b482-3fd823333a4f}']
       Function NegotiateSyntax ( Var msg :  RPCOLEMESSAGE):HResult; StdCall;
       End;




    IRpcProxyBuffer = Interface (IUnknown)
       ['{D5F56A34-593B-101A-B569-08002B2DBF7A}']
       Function Connect(Const rpcchannelbuffer : IRpcChannelBuffer):HResult; StdCall;
       Procedure Disconnect;
       End;

    IRpcStubBuffer = Interface (IUnknown)
       ['{D5F56AFC-593B-101A-B569-08002B2DBF7A}']
       Function COnnect ( Const UnkServer : IUnknown):HResult; StdCall;
       Procedure Disconnect; StdCall;
       Function Invoke(Const rpcmsg : RPCOLEMESSAGE;Const RpcChanBuf : IRpcChannelBuffer):HResult; StdCall;
       Function IsIIDSupported (Const riid : TIID):Pointer {IRpcStubBuffer}; StdCall;
       Function CountRefs :ULong; StdCall;
       Function DebugServerQueryInterface(CONST pv : Pointer):HResult; StdCall;
       Procedure DebugServerRelease (pv : Pointer); StdCall;
       End;

    IPSFactoryBuffer = Interface (IUnknown)
       ['{D5F569D0-593B-101A-B569-08002B2DBF7A}']
       Function CreateProxy(Const UnkOuter : IUnknown;const riid : TIID; Out proxy: IRpcProxyBuffer; Out Pv :Pointer):HResult; StdCall;
       Function CreateStub (Const riid : TIID; Const UnkServer : IUnknown; Out pstub : IRpcStubBuffer):HResult; StdCall;
       End;

{$ifdef NT4_greater_Or_DCOM}
// This interface is only valid on Windows NT 4.0

// This structure contains additional data for hooks.  As a backward
// compatability hack, the entire structure is passed in place of the
// RIID parameter on all hook methods.  Thus the IID must be the first
// parameter.  As a forward compatability hack the second field is the
// current size of the structure.

    SChannelHookCallInfo= Record;
                            IID               : iid;
                            cbSize            : Dword;
                            uCausality        : GUID;
                            dwServerPid,
                            iMethod           : DWord;
                            pObject           : Pointer;
                            End;



  IChannelHook = Interface (IUnknown)
     ['{1008c4a0-7613-11cf-9af1-0020af6e72f4}']
     Procedure ClientGetSize(Const uExtent : TGuid; CONST riid : TIID; Out datasize :ULong); StdCall;
     Procedure ClientFillBuffer(Const uExtent : TGuid; CONST riid : TIID; Var datasize :ULong;Buffer :Pointer); StdCall;
     Procedure ClientNotify(Const uExtent : TGuid; CONST riid : TIID; datasize :ULong;Buffer :Pointer;hrfault:HResult); StdCall;
     Procedure ServerNotify(Const uExtent : TGuid; CONST riid : TIID; datasize :ULong;Buffer :Pointer;DataRep:DWord); StdCall;
     Procedure ServerGetSize(Const uExtent : TGuid; CONST riid : TIID;hrFault :HResult; Out datasize :ULong); StdCall;
     Procedure ServerFillBuffer(Const uExtent : TGuid; CONST riid : TIID; Var datasize :ULong;Buffer :Pointer;HrFault:HResult); StdCall;
     End;
{$Endif}


// Well-known Property Set Format IDs
//FMTID_SummaryInformation              = {CONST} FMTID;
//FMTID_DocSummaryInformation           = {CONST} FMTID;
//FMTID_UserDefinedProperties           = {CONST} FMTID;
//FMTID_DiscardableInformation          = {CONST} FMTID;
//FMTID_ImageSummaryInformation         = {CONST} FMTID;
//FMTID_AudioSummaryInformation         = {CONST} FMTID;
//FMTID_VideoSummaryInformation         = {CONST} FMTID;
//FMTID_MediaFileSummaryInformation     = {CONST} FMTID;


//****************************************************************************
// *  Connection Point Interfaces
// ****************************************************************************/

//#ifdef __INCLUDE_CPIFS
     IConnectionPointContainer = Interface;
//interface IConnectionPoint;
//interface IEnumConnections;
     IEnumConnectionPoints = Interface;
     IEnumConnections      = Interface;


    IConnectionPoint = Interface (IUnknown)
       ['{B196B286-BAB4-101A-B69C-00AA00341D07}']
       Function GetConnectionInterface(out piid : TIID):HResult;StdCall;
       Function GetConnectionPointContainer(CPC : IConnectionPointContainer):HResult;StdCall;
       Function Advise(unkSink : IUnknown;Out dwCookie : DWord):HResult;StdCall;
       Function UnAdvise(dwCookie : DWord):HResult;StdCall;
       Function EnumConnection(out pEnum : IEnumConnections):HResult;stdCall;
      End;

    IConnectionPointContainer = Interface (IUnknown)
       ['{B196B284-BAB4-101A-B69C-00AA00341D07}']
       Function EnumConnectionPoints(out pEnum : IEnumConnectionPoints):HResult;StdCall;
       Function FindConnectionPoint(Const RIID : TIID;Out ppcp : IConnectionPoint):HResult;StdCall;
       End;

    tagCONNECTDATA = Record
                      unk        : Pointer; {IUnknown}
                      dwCookie   : DWord;
                      End;
    ConnectData    = tagCONNECTDATA;

    IEnumConnections = Interface (IUnknown)
       ['{B196B287-BAB4-101A-B69C-00AA00341D07}']
       Function Next(cConnections : ULong; Out rgcd : ConnectData;Out lpcFetched : ULong):HResult;StdCall;
       Function Skip(cConnections : ULong):HResult;StdCall;
       Function Reset:HResult;StdCall;
       Function Clone(Out pEnum : IEnumConnections):HResult; StdCall;
       End;


    IEnumConnectionPoints = Interface (IUnknown)
       ['{B196B285-BAB4-101A-B69C-00AA00341D07}']
       Function Next(cConnections : ULong; Out rgpcm : IConnectionPoint;Out lpcFetched : ULong):HResult;StdCall;
       Function Skip(cConnections : ULong):HResult;StdCall;
       Function Reset:HResult;StdCall;
       Function Clone(Out pEnum : IEnumConnectionPoints):HResult;StdCall;
       End;



    tagSOLE_AUTHENTICATION_SERVICE = Record
                                       dwAuthnSvc,
                                       dwAuthzSvc     : DWord;
                                       pPrincipalName : POleStr;
                                       hr             : HResult;
                                       End;
    SOLE_AUTHENTICATION_SERVICE   = tagSOLE_AUTHENTICATION_SERVICE;
    PSOLE_AUTHENTICATION_SERVICE  = ^SOLE_AUTHENTICATION_SERVICE;

    tagSOLE_AUTHENTICATION_INFO   = Record
                                       dwAuthnSvc,
                                       dwAuthzSvc     : DWord;
                                       AuthInfo       : Pointer;
                                       End;
    SOLE_AUTHENTICATION_INFO      = tagSOLE_AUTHENTICATION_INFO;
    PSOLE_AUTHENTICATION_INFO     = ^SOLE_AUTHENTICATION_INFO;

    tagSOLE_AUTHENTICATION_LIST   = Record
                                       cAuthInfo      : DWord;
                                       AuthInfo       : PSOLE_AUTHENTICATION_INFO;
                                       End;
    SOLE_AUTHENTICATION_LIST      = tagSOLE_AUTHENTICATION_LIST;
    PSOLE_AUTHENTICATION_LIST     = ^SOLE_AUTHENTICATION_LIST;

{$ifdef WINNT_DCOM}

    IClientSecurity = Interface (IUnknown)
        ['{0000013D-0000-0000-C000-000000000046}']
        Function QueryBlanket (Proxy : IUnknown;Out AuthnSvc,AuthzSvc : Dword;Out ServerPrincName:pOleStr;Out AuthnLevel,ImpLevel:Dword; Out AuthInfo : Pointer; Out Capabilities : Dword):HResult;StdCall;
        Function SetBlanket   (Proxy : IUnknown;AuthnSvc,AuthzSvc : Dword;ServerPrincName:pOleStr;AuthnLevel,ImpLevel:Dword;AuthInfo : Pointer;Capabilities : Dword):HResult;StdCall;
        Function CopyProxy    (Proxy : IUnknown;Out pcopy:IUnknown):HResult;StdCall;
        End;

    IServerSecurity = Interface (IUnknown)
       ['{0000013E-0000-0000-C000-000000000046}']
       Function QueryBlanket ( out authnSvc,AuthzSvc : DWord; Out pServerPrincName : pOleStr; Out AuthnLevel, ImpLevel; :DWord; out Privs : Pointer; Var Capabilities :DWord):HResult;StdCall;
       Function ImpersonateClient:HResult;StdCall;
       Function RevertToSelf:HResult;StdCall;
       Function IsImpersonating:Bool;StdCall;
    End;

    IClassActivator = Interface (IUnknown)
       ['{00000140-0000-0000-C000-000000000046}']
       Function GetClassObject(Const rclsif : TClsID; ClassContext : DWord; locale : LCID; Const ridd : TIID; Out pv : Pointer):HResult;StdCall;
       End;


    IRpcOptions = Interface (IUnknown)
       ['{00000144-0000-0000-C000-000000000046}']
       Function xSet (prx : IUnknown;dwProperty : DWord; dwValue:ULONG_PTR):HResult; StdCall;
       Function Query (prx : IUnknown;dwProperty:Dword; dwValue:ULONG_PTR):HResult; StdCall;
       End;

{$endif} {DCOM}

    IFillLockBytes = Interface (IUnknown)
       ['{99caf010-415e-11cf-8814-00aa00b569f5}']
       Function FillAppend(const pv : Pointer;cb:ULong; Out PcbWritten : ULong):HResult;StdCall;
       Function FillAt(ulOffset : ULarge_INTEGER;Const pv : Pointer;cb :ULong; Out pcbWritten:ULong):HResult;StdCall;
       Function SetFillSize ( ulSize :ULarge_Integer):HResult;StdCall;
       Function Terminate (bCanceled :Bool):HResult;StdCall;
    End;

    IProgressNotify = Interface (IUnknown)
       ['{a9d758a0-4617-11cf-95fc-00aa00680db4}']
       Function OnProgress (ProgressCurrent,ProgressMaximum :Dword; FAccurate,Fowner : Bool):HResult;StdCall;
       End;

    ILayoutStorage = Interface (IUnknown)
       ['{0e6d4d90-6738-11cf-9608-00aa00680db4}']
       {The methods in this interface all had "__stdcall" as modifier, while the other classes don't. ?!?!?}
       Function LayoutScript ( xStorageLayout : StorageLayout;nEntries,glfInterleaveFlag : Dword) :HResult; StdCall;
       Function BeginMonitor:HResult;StdCall;
       Function EndMonitor:HResult;StdCall;
       Function ReLayourDocFile(pwcsNewDFName :pOleStr):HResult;StdCall;
       Function ReLayoutDocfileOnILockBytes(LockBytes : ILockBytes):Hresult;StdCall;
       End;

    IBlockingLock = Interface (IUnknown)
       ['{30f3d47a-6447-11d1-8e3c-00c04fb9386d}']
       Function Lock (dwTimeOut : DWord) : HResult;Stdcall;
       Function Unlock : HResult;Stdcall;
       End;

    ITimeAndNoticeControl = Interface (IUnknown)
       ['{bc0bf6ae-8878-11d1-83e9-00c04fc2c6d4}']
       Function SuppressChanges(res1,res2 : Dword):HResult;StdCall;
       End;

    IOplockStorage = Interface (IUnknown)
       ['{8d19c834-8879-11d1-83e9-00c04fc2c6d4}']
       Function CreateStorageEx(wcsName : LPCWSTR;grfMode,StgFmt,GrfAtrrs :Dword;Const riid :Tiid; Out ppstgOpen : Pointer):HResult;StdCall;
       Function OpenStorageEx(wcsName : LPCWSTR;grfMode,StgFmt,GrfAtrrs :Dword;Const riid :Tiid; Out ppstgOpen : Pointer):HResult;StdCall;
       End;

    ISurrogate = Interface (IUnknown)
       ['{00000022-0000-0000-C000-000000000046}']
       Function LoadDllServer (Const ClsId : TClsId):HResult;StdCall;
       Function FreeSurrogate:HResult;StdCall;
       End;

    IGlobalInterfaceTable = Interface (IUnknown)
       ['{00000146-0000-0000-C000-000000000046}']
       Function RegisterInterfaceInGlobal(unk :IUnknown;Const riid : TIID; Out dwcookie :DWord):HResult;StdCall;
       Function RevokeInterfaceFromGlobal (dwCookie :DWord):HResult;StdCall;
       Function GetInterfaceFromGlobal (dwCookie :DWord;Const riid : TIID;out pv : Pointer):HResult;StdCall;
       End;

    IDirectWriterLock = Interface (IUnknown)
       ['{0e6d4d92-6738-11cf-9608-00aa00680db4}']
       Function WaitForWriteAccess (dwTimeOut : DWORD):HResult;StdCall;
       Function ReleaseWriteAccess:HResult;StdCall;
       Function HaveWriteAccess:HResult;StdCall;
       End;

    ISynchronize = Interface (IUnknown)
       ['{00000030-0000-0000-C000-000000000046}']
       Function Wait (dwFlags : DWord; dwMilliSeconds : DWord):HResult;StdCall;
       Function Signal : HResult;StdCall;
       Function Reset : HResult;StdCall;
       End;

    ISynchronizeHandle = Interface (IUnknown)
       ['{00000031-0000-0000-C000-000000000046}']
       Function GetHandle(Out ph : Handle):HResult;StdCall;
       End;

    ISynchronizeEvent = Interface (ISynchronizeHandle)
       ['{00000032-0000-0000-C000-000000000046}']
       Function SetEventHandle (Const ph : Handle):HResult; StdCall;
       End;

    ISynchronizeContainer = Interface (IUnknown)
       ['{00000033-0000-0000-C000-000000000046}']
       Function AddSynchronize(pSync : ISynchronize):HResult; StdCall;
       Function WaitMultiple(dwFlags : Dword; dwTimeOut : Dword; Out pSync : ISynchronize):HResult;StdCall;
       End;

    ISynchronizeMutex = Interface (ISynchronize)
       ['{00000025-0000-0000-C000-000000000046}']
       Function ReleaseMutex:HResult; StdCall;
       End;

    ICancelMethodCalls = Interface (IUnknown)
       ['{00000029-0000-0000-C000-000000000046}']
       Function Cancel(ulSeconds : ULong):HResult; StdCall;
       Function TestCancel:HResult;StdCall;
       End;

    IAsyncManager = Interface (IUnknown)
       ['{0000002A-0000-0000-C000-000000000046}']
       Function CompleteCall (xResult : HResult):HResult;StdCall;
       Function GetCallContext(Const iid :TIID; Out pInterface : Pointer):HResult;StdCall;
       Function GetState(Out pulStateFlags : ULong):HResult;StdCall;
       End;

    ICallFactory = Interface (IUnknown)
       ['{1c733a30-2a1c-11ce-ade5-00aa0044773d}']
       Function CreateCall(Const riid:TIID;CtrUnk : IUnknown;Const Riid2:TIID;Out Unknown : IUnknown):HResult;StdCall;
       End;

    IRpcHelper = Interface (IUnknown)
       ['{00000149-0000-0000-C000-000000000046}']
       Function GetDCOMProtocolVersion(Out ComVersion :DWord):HResult;StdCall;
       Function GettIIDFromOBJREF(ObjRef : Pointer;Out xIID : piid):HResult;StdCall;
       End;

    IReleaseMarshalBuffers = Interface (IUnknown)
       ['{eb0cb9e8-7996-11d2-872e-0000f8080859}']
       Function ReleaseMarshalBuffer(const pnsg : RPCOLEMESSAGE;dwFlags:DWord;Const pchn : IUnknown):HResult; StdCall;
       End;

    IWaitMultiple = Interface (IUnknown)
       ['{0000002B-0000-0000-C000-000000000046}']
       Function WaitMulitple(TImeout :DWord;out psync : ISynchronize):HResult; StdCall;
       Function AddSynchronize (const psync : ISynchronize):HResult;StdCall;
       End;

    IUrlMon = Interface (IUnknown)
       ['{00000026-0000-0000-C000-000000000046}']
       Function AsyncGetClassBits(CONST rclsif : TClsID; psztype,pzext : lpcwstr; dwfileversionMS,dwFileVersionLS : DWord; pzcodebase : LPCWSTR; Const pbc : IBindCTX; dwclasscontext : DWord; const Riid:TIID; flags :DWORD):HResult; StdCall;
       End;

    IForegroundTransfer = Interface (IUnknown)
       ['{00000145-0000-0000-C000-000000000046}']
       Function AllowForegroundTransfer(lpvReserved:Pointer):HResult; StdCall;
       End;

    IAddrTrackingControl = Interface (IUnknown)
       ['{00000147-0000-0000-C000-000000000046}']
       Function EnableCOMDynamicAddrTracking:HResult; StdCall;
       Function DisableCOMDynamicAddrTracking:HResult; StdCall;
       End;

    IAddrExclusionControl = Interface (IUnknown)
       ['{00000148-0000-0000-C000-000000000046}']
       Function GetCurrentAddrExclusionList(Const riid : TIID;out Enumerator : Pointer):HResult;StdCall;
       Function UpdateAddrExclusionList(Enumerator : IUnknown):HResult;StdCall;
       End;

//****************************************************************************
//* Pipe interfaces
//****************************************************************************/

// Doesn't look translatable. See objidl.idl

//****************************************************************************
//* Thumbnail generator interface
//****************************************************************************/

   IThumbnailExtractor = Interface (IUnknown)
      ['{969dc708-5c76-11d1-8d86-0000f804b057}']
       Function ExtractThumbnail (pStg : IStorage; uLength,UHeight : ULong; Out uloutputlength,Height :ULong; Out OutputBitmap : HBITMAP): HResult; StdCall;
       Function OnFileUpdated (pStg : IStorage):HResult;
       End;

//****************************************************************************
//* Dummy Interface to force inclusion of HICON and HDC in proxy/stub code....
//****************************************************************************/

    IDummyHICONIncluder = Interface (IUnknown)
       ['{947990de-cc28-11d2-a0f7-00805f858fb1}']
       Function Dummy (h1 : HICON; H2 :HDC):HResult;
       End;

    IComThreadingInfo = Interface (IUnknown)
       ['{000001ce-0000-0000-C000-000000000046}']
       Function GetCurrentApartmentType(out pAptType : DWord {APTTTYPE}):HResult;
       Function GetCurrentThreadType(Out ThreadType : Dword {THDTTYPE}):HResult;StdCall;
       Function GetCurrentLogicalThreadID(Out guidlogicalThreadId : TGUID):HResult;StdCall;
       Function SetCurrentLogicalThreadID(Const guidlogicalThreadId : TGUID):HResult;StdCall;
       End;

   IProcessInitControl = Interface (IUnknown)
       ['{72380d55-8d2b-43a3-8513-2b6ef31434e9}']
       Function ResetInitializerTimeout(dwSecondsRemaining:DWord):HResult; StdCall;
       End;

implementation
end.

{
  $Log$
  Revision 1.2  2002-02-26 10:30:01  marco
   * Merged objidl.idl translation. Most of wtypes.idl also included. Size slightly 	increased.

  Revision 1.1  2001/08/19 21:02:02  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

}
