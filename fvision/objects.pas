{ $Id$ }
{**********************************************************}
{                                                          }
{    System independent clone of OBJECTS.PAS               }
{                                                          }
{    Interface Copyright (c) 1992 Borland International    }
{                                                          }
{    Parts Copyright (c) 1992,96 by Florian Klaempfl       }
{    fnklaemp@cip.ft.uni-erlangen.de                       }
{                                                          }
{    Parts Copyright (c) 1996 by Frank ZAGO                }
{    zago@ecoledoc.ipc.fr                                  }
{                                                          }
{    Parts Copyright (c) 1995 by MH Spiegel                }
{                                                          }
{    Parts Copyright (c) 1996, 1997, 1998, 1999, 2000      }
{    ldeboer@attglobal.net  - primary e-mail address       }
{    ldeboer@projectent.com.au - backup e-mail address     }
{                                                          }
{****************[ THIS CODE IS FREEWARE ]*****************}
{                                                          }
{     This sourcecode is released for the purpose to       }
{   promote the pascal language on all platforms. You may  }
{   redistribute it and/or modify with the following       }
{   DISCLAIMER.                                            }
{                                                          }
{     This SOURCE CODE is distributed "AS IS" WITHOUT      }
{   WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR     }
{   ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.     }
{                                                          }
{*****************[ SUPPORTED PLATFORMS ]******************}
{     16 and 32 Bit compilers                              }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{        DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                 - Speed Pascal 1.0+       (32 Bit)       }
{                 - C'T patch to BP         (16 Bit)       }
{                                                          }
{*****************[ REVISION HISTORY ]*********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     12 Jun 96   First multi platform release       }
{  1.01     20 Jun 96   Fixes to TCollection               }
{  1.02     07 Aug 96   Fixed TStringCollection.Compare    }
{  1.10     18 Jul 97   Windows 95 support added.          }
{  1.11     21 Aug 97   FPC pascal 0.92 implemented        }
{  1.15     26 Aug 97   TXMSStream compatability added     }
{                       TEMSStream compatability added     }
{  1.30     29 Aug 97   Platform.inc sort added.           }
{  1.32     02 Sep 97   RegisterTypes completed.           }
{  1.37     04 Sep 97   TStream.Get & Put completed.       }
{  1.40     04 Sep 97   LongMul & LongDiv added.           }
{  1.45     04 Sep 97   Refined and passed all tests.      }
{                       FPC - bugged on register records!  }
{  1.50     05 May 98   Fixed DOS Access to files, one     }
{                       version for all intel platforms    }
{                       (CEC)                              }
{  1.60     22 Oct 97   Delphi3 32 bit code added.         }
{  1.70     05 Feb 98   Speed pascal code added.           }
{  1.80     05 May 98   Virtual pascal 2.0 compiler added. }
{  1.85     10 Sep 98   Checks run & commenting added.     }
{  1.90     03 Nov 98   Fixed for FPC version 0.998        }
{                       Only Go32v2 supported no Go32v1    }
{  1.95     02 Feb 99   Moved some stuff to common.pas     }
{  1.97     28 May 99   Bug fix to TCollection.AtInsert    }
{  1.98     07 Jul 99   Speedsoft SYBIL 2.0 code added.    }
{  1.99     08 Jul 99   Fixed TCollection FirstThat etc.   }
{  2.00     27 Oct 99   All stream read/writes checked.    }
{                       Delphi3+ memory code to COMMON.PAS }
{  2.01     03 Nov 99   FPC windows support added.         }
{  2.02     14 Nov 00   Fixed XMS/EMS Stream read/writes.  }
{**********************************************************}

UNIT Objects;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC} { FPC doesn't support these switches }
  {$F+} { Force far calls - Used because of the Foreach, FirstThat etc...}
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$E+} { Emulation is on }
  {$N-} { No 80x87 code generation }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     {$IFNDEF PPC_SPEED}                              { NON SPEED COMPILER }
       {$IFDEF PPC_FPC}                               { FPC WINDOWS COMPILER }
       Windows,                                       { Standard unit }
       {$ELSE}                                        { OTHER COMPILERS }
       WinTypes, WinProcs,                            { Stardard units }
       {$ENDIF}
     {$ELSE}                                          { SPEEDSOFT COMPILER }
     WinBase, WinUser,                                { Standard unit }
     {$ENDIF}
   {$ENDIF}

   Common, FileIO;                                    { GFV standard units }

{***************************************************************************}
{                             PUBLIC CONSTANTS                              }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                          STREAM ERROR STATE MASKS                         }
{---------------------------------------------------------------------------}
CONST
   stOk         =  0;                                 { No stream error }
   stError      = -1;                                 { Access error }
   stInitError  = -2;                                 { Initialize error }
   stReadError  = -3;                                 { Stream read error }
   stWriteError = -4;                                 { Stream write error }
   stGetError   = -5;                                 { Get object error }
   stPutError   = -6;                                 { Put object error }
   stSeekError  = -7;                                 { Seek error in stream }
   stOpenError  = -8;                                 { Error opening stream }

{---------------------------------------------------------------------------}
{                        STREAM ACCESS MODE CONSTANTS                       }
{---------------------------------------------------------------------------}
CONST
   stCreate    = fa_Create;                           { Create new file }
   stOpenRead  = fa_OpenRead;                         { Read access only }
   stOpenWrite = fa_OpenWrite;                        { Write access only }
   stOpen      = fa_Open;                             { Read/write access }

{---------------------------------------------------------------------------}
{                          TCollection ERROR CODES                          }
{---------------------------------------------------------------------------}
CONST
   coIndexError = -1;                                 { Index out of range }
   coOverflow   = -2;                                 { Overflow }

{---------------------------------------------------------------------------}
{         VMT HEADER CONSTANT - HOPEFULLY WE CAN DROP THIS LATER            }
{---------------------------------------------------------------------------}
{$IFDEF PPC_Virtual}                                  { Virtual is different }
CONST
   vmtHeaderSize = 12;                                { VMT header size }
{$ELSE}
CONST
   vmtHeaderSize = 8;                                 { VMT header size }
{$ENDIF}

{---------------------------------------------------------------------------}
{                            MAXIUM DATA SIZES                              }
{---------------------------------------------------------------------------}
CONST
   MaxCollectionSize = 65520 DIV SizeOf(Pointer);     { Max collection size }

{***************************************************************************}
{                          PTBMIC TYPE DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                               CHARACTER SET                               }
{---------------------------------------------------------------------------}
TYPE
   TCharSet = SET Of Char;                            { Character set }
   PCharSet = ^TCharSet;                              { Character set ptr }

{---------------------------------------------------------------------------}
{                             POINTER TO STRING                             }
{---------------------------------------------------------------------------}
TYPE
   PString = ^String;                                 { String pointer }

{---------------------------------------------------------------------------}
{                            DOS FILENAME STRING                            }
{---------------------------------------------------------------------------}
TYPE
{$IFDEF OS_DOS}                                       { DOS/DPMI DEFINE }
   FNameStr = String[79];                             { DOS filename }
{$ENDIF}
{$IFDEF OS_WINDOWS}                                   { WIN/NT DEFINE }
   FNameStr = PChar;                                  { Windows filename }
{$ENDIF}
{$IFDEF OS_OS2}                                       { OS2 DEFINE }
   FNameStr = String;                                 { OS2 filename }
{$ENDIF}
{$IFDEF OS_LINUX}                                     { LINUX DEFINE }
   FNameStr = String;                                 { Linux filename }
{$ENDIF}
{$IFDEF OS_AMIGA}                                     { AMIGA DEFINE }
   FNameStr = String;                                 { Amiga filename }
{$ENDIF}
{$IFDEF OS_ATARI}                                     { ATARI DEFINE }
   FNameStr = String[79];                             { Atari filename }
{$ENDIF}
{$IFDEF OS_MAC}                                       { MACINTOSH DEFINE }
    FNameStr = String;                                { Mac filename }
{$ENDIF}

{***************************************************************************}
{                        PUBLIC RECORD DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                          TYPE CONVERSION RECORDS                          }
{---------------------------------------------------------------------------}
TYPE
   WordRec = PACKED RECORD
     Lo, Hi: Byte;                                    { Word to bytes }
   END;

   LongRec = PACKED RECORD
     Lo, Hi: Word;                                    { LongInt to words }
   END;

   PtrRec = PACKED RECORD
     Ofs, Seg: Word;                                  { Pointer to words }
   END;

{---------------------------------------------------------------------------}
{                  TStreamRec RECORD - STREAM OBJECT RECORD                 }
{---------------------------------------------------------------------------}
TYPE
   PStreamRec = ^TStreamRec;                          { Stream record ptr }
   TStreamRec = PACKED RECORD
      ObjType: Word;                                  { Object type id }
      {$IFDEF BP_VmtLink}
      VmtLink: Sw_Word;                               { VMT link like BP }
      {$ELSE}
      VmtLink: Pointer;                               { Delphi3/FPC like VMT }
      {$ENDIF}
      Load : Pointer;                                 { Object load code }
      Store: Pointer;                                 { Object store code }
      Next : PStreamRec;                              { Next stream record }
   END;

{***************************************************************************}
{                        PUBLIC OBJECT DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                        TPoint OBJECT - POINT OBJECT                       }
{---------------------------------------------------------------------------}
TYPE
   TPoint = OBJECT
      X, Y: Integer;
   END;
   PPoint = ^TPoint;

{---------------------------------------------------------------------------}
{                      TRect OBJECT - RECTANGLE OBJECT                      }
{---------------------------------------------------------------------------}
   TRect = OBJECT
         A, B: TPoint;                                { Corner points }
      FUNCTION Empty: Boolean;
      FUNCTION Equals (R: TRect): Boolean;
      FUNCTION Contains (P: TPoint): Boolean;
      PROCEDURE Copy (R: TRect);
      PROCEDURE Union (R: TRect);
      PROCEDURE Intersect (R: TRect);
      PROCEDURE Move (ADX, ADY: Integer);
      PROCEDURE Grow (ADX, ADY: Integer);
      PROCEDURE Assign (XA, YA, XB, YB: Integer);
   END;
   PRect = ^TRect;

{---------------------------------------------------------------------------}
{                  TObject OBJECT - BASE ANCESTOR OBJECT                    }
{---------------------------------------------------------------------------}
TYPE
   TObject = OBJECT
      CONSTRUCTOR Init;
      PROCEDURE Free;
      DESTRUCTOR Done;                                               Virtual;
   END;
   PObject = ^TObject;

{ ******************************* REMARK ****************************** }
{  Two new virtual methods have been added to the object in the form of }
{  Close and Open. The main use here is in the Disk Based Descendants   }
{  the calls open and close the given file so these objects can be      }
{  used like standard files. Two new fields have also been added to     }
{  speed up seeks on descendants. All existing code will compile and    }
{  work completely normally oblivious to these new methods and fields.  }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }

{---------------------------------------------------------------------------}
{                 TStream OBJECT - STREAM ANCESTOR OBJECT                   }
{---------------------------------------------------------------------------}
TYPE
   TStream = OBJECT (TObject)
         Status    : Integer;                         { Stream status }
         ErrorInfo : Integer;                         { Stream error info }
         StreamSize: LongInt;                         { Stream current size }
         Position  : LongInt;                         { Current position }
      FUNCTION Get: PObject;
      FUNCTION StrRead: PChar;
      FUNCTION GetPos: LongInt; Virtual;
      FUNCTION GetSize: LongInt; Virtual;
      FUNCTION ReadStr: PString;
      PROCEDURE Open (OpenMode: Word); Virtual;
      PROCEDURE Close; Virtual;
      PROCEDURE Reset;
      PROCEDURE Flush; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Put (P: PObject);
      PROCEDURE StrWrite (P: PChar);
      PROCEDURE WriteStr (P: PString);
      PROCEDURE Seek (Pos: LongInt); Virtual;
      PROCEDURE Error (Code, Info: Integer); Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
      PROCEDURE CopyFrom (Var S: TStream; Count: LongInt);
   END;
   PStream = ^TStream;

{ ******************************* REMARK ****************************** }
{   A few minor changes to this object and an extra field added called  }
{  FName which holds an AsciiZ array of the filename this allows the    }
{  streams file to be opened and closed like a normal text file. All    }
{  existing code should work without any changes.                       }
{ ****************************** END REMARK *** Leon de Boer, 19May96 * }

{---------------------------------------------------------------------------}
{                TDosStream OBJECT - DOS FILE STREAM OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
   TDosStream = OBJECT (TStream)
         Handle: THandle;                             { DOS file handle }
         FName : AsciiZ;                              { AsciiZ filename }
      CONSTRUCTOR Init (FileName: FNameStr; Mode: Word);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Close; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Seek (Pos: LongInt); Virtual;
      PROCEDURE Open (OpenMode: Word); Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
   END;
   PDosStream = ^TDosStream;

{ ******************************* REMARK ****************************** }
{   A few minor changes to this object and an extra field added called  }
{  lastmode which holds the read or write condition last using the      }
{  speed up buffer which helps speed up the flush, position and size    }
{  functions. All existing code should work without any changes.        }
{ ****************************** END REMARK *** Leon de Boer, 19May96 * }

{---------------------------------------------------------------------------}
{                TBufStream OBJECT - BUFFERED DOS FILE STREAM               }
{---------------------------------------------------------------------------}
TYPE
   TBufStream = OBJECT (TDosStream)
         LastMode: Byte;                              { Last buffer mode }
         BufSize : Word;                              { Buffer size }
         BufPtr  : Word;                              { Buffer start }
         BufEnd  : Word;                              { Buffer end }
         Buffer  : PByteArray;                        { Buffer allocated }
      CONSTRUCTOR Init (FileName: FNameStr; Mode, Size: Word);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Close; Virtual;
      PROCEDURE Flush; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Seek (Pos: LongInt); Virtual;
      PROCEDURE Open (OpenMode: Word); Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
   END;
   PBufStream = ^TBufStream;

{ ******************************* REMARK ****************************** }
{  All the changes here should be completely transparent to existing    }
{  code. Basically the memory blocks do not have to be base segments    }
{  but this means our list becomes memory blocks rather than segments.  }
{  The stream will also expand like the other standard streams!!        }
{ ****************************** END REMARK *** Leon dd Boer, 19May96 * }

{---------------------------------------------------------------------------}
{               TMemoryStream OBJECT - MEMORY STREAM OBJECT                 }
{---------------------------------------------------------------------------}
TYPE
   TMemoryStream = OBJECT (TStream)
         BlkCount: Word;                              { Number of segments }
         BlkSize : Word;                              { Memory block size }
         MemSize : LongInt;                           { Memory alloc size }
         BlkList : PPointerArray;                     { Memory block list }
      CONSTRUCTOR Init (ALimit: LongInt; ABlockSize: Word);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
      PRIVATE
      FUNCTION ChangeListSize (ALimit: Word): Boolean;
   END;
   PMemoryStream = ^TMemoryStream;

{ ******************************* REMARK ****************************** }
{  This object under all but real mode DOS is simple a TMemoryStream    }
{  by another name. Under real mode DOS programs it copies the standard }
{  standard EMS stream object as per Borland's original unit.           }
{ ****************************** END REMARK *** Leon de Boer, 14Aug98 * }

{---------------------------------------------------------------------------}
{                  TEmsStream OBJECT - EMS STREAM OBJECT                    }
{---------------------------------------------------------------------------}
TYPE
{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
   TEmsStream = OBJECT (TStream)
         Handle   : Word;                             { EMS handle }
         PageCount: Word;                             { Pages allocated }
         MemSize  : LongInt;                          { EMS alloc size }
      CONSTRUCTOR Init (MinSize, MaxSize: LongInt);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
   END;
{$ELSE}                                               { DPMI/WIN/OS2 CODE }
   TEmsStream = OBJECT (TMemoryStream)                { Memory stream object }
      CONSTRUCTOR Init (MinSize, MaxSize: LongInt);
   END;
{$ENDIF}
   PEmsStream = ^TEmsStream;                          { EMS stream pointer }

{ ******************************* REMARK ****************************** }
{  This object under all but real mode DOS is simple a TMemoryStream    }
{  by another name. Under real mode DOS programs it is a copy of the    }
{  EMS stream object but using XMS, it can replace use of TEMSStream.   }
{ ****************************** END REMARK *** Leon de Boer, 14Aug98 * }

{---------------------------------------------------------------------------}
{                  TXmsStream OBJECT - XMS STREAM OBJECT                    }
{---------------------------------------------------------------------------}
TYPE
{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
   TXmsStream = OBJECT (TStream)
         Handle    : Word;                            { XMS handle number }
         BlocksUsed: Word;                            { XMS blocks in use }
         MemSize   : LongInt;                         { XMS alloc size }
      CONSTRUCTOR Init (MinSize, MaxSize: LongInt);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Truncate; Virtual;
      PROCEDURE Read (Var Buf; Count: Word); Virtual;
      PROCEDURE Write (Var Buf; Count: Word); Virtual;
   END;
{$ELSE}                                               { DPMI/WIN/NT/OS2 CODE }
   TXmsStream = OBJECT (TMemoryStream)                { Memory stream object }
      CONSTRUCTOR Init (MinSize, MaxSize: LongInt);
   END;
{$ENDIF}
   PXmsStream = ^TXmsStream;                          { XMS stream pointer }

TYPE
  TItemList = Array [0..MaxCollectionSize - 1] Of Pointer;
  PItemList = ^TItemList;

{ ******************************* REMARK ****************************** }
{    The changes here look worse than they are. The Sw_Integer simply   }
{  switches between Integers and LongInts if switched between 16 and 32 }
{  bit code. All existing code will compile without any changes.        }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{---------------------------------------------------------------------------}
{              TCollection OBJECT - COLLECTION ANCESTOR OBJECT              }
{---------------------------------------------------------------------------}
   TCollection = OBJECT (TObject)
         Items: PItemList;                            { Item list pointer }
         Count: Integer;                              { Item count }
         Limit: Integer;                              { Item limit count }
         Delta: Integer;                              { Inc delta size }
      CONSTRUCTOR Init (ALimit, ADelta: Integer);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION At (Index: Integer): Pointer;
      FUNCTION IndexOf (Item: Pointer): Integer; Virtual;
      FUNCTION GetItem (Var S: TStream): Pointer; Virtual;
      FUNCTION LastThat (Test: Pointer): Pointer;
      FUNCTION FirstThat (Test: Pointer): Pointer;
      PROCEDURE Pack;
      PROCEDURE FreeAll;
      PROCEDURE DeleteAll;
      PROCEDURE Free (Item: Pointer);
      PROCEDURE Insert (Item: Pointer); Virtual;
      PROCEDURE Delete (Item: Pointer);
      PROCEDURE AtFree (Index: Integer);
      PROCEDURE FreeItem (Item: Pointer); Virtual;
      PROCEDURE AtDelete (Index: Integer);
      PROCEDURE ForEach (Action: Pointer);
      PROCEDURE SetLimit (ALimit: Integer); Virtual;
      PROCEDURE Error (Code, Info: Integer); Virtual;
      PROCEDURE AtPut (Index: Integer; Item: Pointer);
      PROCEDURE AtInsert (Index: Integer; Item: Pointer);
      PROCEDURE Store (Var S: TStream);
      PROCEDURE PutItem (Var S: TStream; Item: Pointer); Virtual;
   END;
   PCollection = ^TCollection;

{---------------------------------------------------------------------------}
{          TSortedCollection OBJECT - SORTED COLLECTION ANCESTOR            }
{---------------------------------------------------------------------------}
TYPE
   TSortedCollection = OBJECT (TCollection)
         Duplicates: Boolean;                         { Duplicates flag }
      CONSTRUCTOR Init (ALimit, ADelta: Integer);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION KeyOf (Item: Pointer): Pointer; Virtual;
      FUNCTION IndexOf (Item: Pointer): Integer; Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Integer; Virtual;
      FUNCTION Search (Key: Pointer; Var Index: Integer): Boolean; Virtual;
      PROCEDURE Insert (Item: Pointer); Virtual;
      PROCEDURE Store (Var S: TStream);
   END;
   PSortedCollection = ^TSortedCollection;

{---------------------------------------------------------------------------}
{           TStringCollection OBJECT - STRING COLLECTION OBJECT             }
{---------------------------------------------------------------------------}
TYPE
   TStringCollection = OBJECT (TSortedCollection)
      FUNCTION GetItem (Var S: TStream): Pointer; Virtual;
      FUNCTION Compare (Key1, Key2: Pointer): Integer; Virtual;
      PROCEDURE FreeItem (Item: Pointer); Virtual;
      PROCEDURE PutItem (Var S: TStream; Item: Pointer); Virtual;
   END;
   PStringCollection = ^TStringCollection;

{---------------------------------------------------------------------------}
{             TStrCollection OBJECT - STRING COLLECTION OBJECT              }
{---------------------------------------------------------------------------}
TYPE
   TStrCollection = OBJECT (TSortedCollection)
      FUNCTION Compare (Key1, Key2: Pointer): Integer; Virtual;
      FUNCTION GetItem (Var S: TStream): Pointer; Virtual;
      PROCEDURE FreeItem (Item: Pointer); Virtual;
      PROCEDURE PutItem (Var S: TStream; Item: Pointer); Virtual;
   END;
   PStrCollection = ^TStrCollection;

{ ******************************* REMARK ****************************** }
{    This is a completely >> NEW << object which holds a collection of  }
{  strings but does not alphabetically sort them. It is a very useful   }
{  object for insert ordered list boxes!                                }
{ ****************************** END REMARK *** Leon de Boer, 15May96 * }

{---------------------------------------------------------------------------}
{        TUnSortedStrCollection - UNSORTED STRING COLLECTION OBJECT         }
{---------------------------------------------------------------------------}
TYPE
   TUnSortedStrCollection = OBJECT (TStringCollection)
      PROCEDURE Insert (Item: Pointer); Virtual;
   END;
   PUnSortedStrCollection = ^TUnSortedStrCollection;

{---------------------------------------------------------------------------}
{         TResourceCollection OBJECT - RESOURCE COLLECTION OBJECT           }
{---------------------------------------------------------------------------}
TYPE
   TResourceCollection = OBJECT (TStringCollection)
      FUNCTION KeyOf (Item: Pointer): Pointer; Virtual;
      FUNCTION GetItem (Var S: TStream): Pointer; Virtual;
      PROCEDURE FreeItem (Item: Pointer); Virtual;
      PROCEDURE PutItem (Var S: TStream; Item: Pointer); Virtual;
   END;
   PResourceCollection = ^TResourceCollection;

{---------------------------------------------------------------------------}
{                 TResourceFile OBJECT - RESOURCE FILE OBJECT               }
{---------------------------------------------------------------------------}
TYPE
   TResourceFile = OBJECT (TObject)
         Stream  : PStream;                           { File as a stream }
         Modified: Boolean;                           { Modified flag }
      CONSTRUCTOR Init (AStream: PStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION Count: Integer;
      FUNCTION KeyAt (I: Integer): String;
      FUNCTION Get (Key: String): PObject;
      FUNCTION SwitchTo (AStream: PStream; Pack: Boolean): PStream;
      PROCEDURE Flush;
      PROCEDURE Delete (Key: String);
      PROCEDURE Put (Item: PObject; Key: String);
      PRIVATE
         BasePos: LongInt;                            { Base position }
         IndexPos: LongInt;                           { Index position }
         Index: TResourceCollection;                  { Index collection }
   END;
   PResourceFile = ^TResourceFile;

TYPE
   TStrIndexRec = PACKED RECORD Key, Count, Offset: Word; END;

   TStrIndex = Array [0..9999] Of TStrIndexRec;
   PStrIndex = ^TStrIndex;

{---------------------------------------------------------------------------}
{                 TStringList OBJECT - STRING LIST OBJECT                   }
{---------------------------------------------------------------------------}
   TStringList = OBJECT (TObject)
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION Get (Key: Word): String;
      PRIVATE
         Stream   : PStream;
         BasePos  : LongInt;
         IndexSize: Sw_Word;
         Index    : PStrIndex;
      PROCEDURE ReadStr (Var S: String; Offset, Skip: Word);
   END;
   PStringList = ^TStringList;

{---------------------------------------------------------------------------}
{                 TStrListMaker OBJECT - RESOURCE FILE OBJECT               }
{---------------------------------------------------------------------------}
TYPE
   TStrListMaker = OBJECT (TObject)
      CONSTRUCTOR Init (AStrSize, AIndexSize: Word);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Put (Key: Word; S: String);
      PROCEDURE Store (Var S: TStream);
      PRIVATE
         StrPos   : Sw_Word;
         StrSize  : Sw_Word;
         Strings  : PByteArray;
         IndexPos : Sw_Word;
         IndexSize: Sw_Word;
         Index    : PStrIndex;
         Cur      : TStrIndexRec;
      PROCEDURE CloseCurrent;
   END;
   PStrLisuMaker = ^TStrListMaker;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        STREAM INTERFACE ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-Abstract-----------------------------------------------------------
Terminates program with a run-time error 211. When implementing
an abstract object type, call Abstract in those virtual methods that
must be overridden in descendant types. This ensures that any
attempt to use instances of the abstract object type will fail.
12Jun96 LdB
---------------------------------------------------------------------}
PROCEDURE Abstract;

{-RegisterObjects----------------------------------------------------
Registers the three standard objects TCollection, TStringCollection
and TStrCollection.
02Sep97 LdB
---------------------------------------------------------------------}
PROCEDURE RegisterObjects;

{-RegisterType-------------------------------------------------------
Registers the given object type with Free Vision's streams, creating
a list of known objects. Streams can only store and return these known
object types. Each registered object needs a unique stream registration
record, of type TStreamRec.
02Sep97 LdB
---------------------------------------------------------------------}
PROCEDURE RegisterType (Var S: TStreamRec);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    GENERAL FUNCTION INTERFACE ROUTINES                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-LongMul------------------------------------------------------------
Returns the long integer value of X * Y integer values.
10Feb98 LdB
---------------------------------------------------------------------}
FUNCTION LongMul (X, Y: Integer): LongInt;

{-LongDiv------------------------------------------------------------
Returns the integer value of long integer X divided by integer Y.
10Feb98 LdB
---------------------------------------------------------------------}
FUNCTION LongDiv (X: LongInt; Y: Integer): Integer;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    DYNAMIC STRING INTERFACE ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewStr-------------------------------------------------------------
Allocates a dynamic string into memory. If S is nil, NewStr returns
a nil pointer, otherwise NewStr allocates Mength(S)+1 bytes of memory
containing a copy of S, and returns a pointer to the string.
12Jun96 LdB
---------------------------------------------------------------------}
FUNCTION NewStr (S: String): PString;

{-DisposeStr---------------------------------------------------------
Disposes of a PString allocated by the function NewStr.
12Jun96 LdB
---------------------------------------------------------------------}
PROCEDURE DisposeStr (P: PString);

{***************************************************************************}
{                         PUBLIC INITIALIZED VARIABLES                      }
{***************************************************************************}

{---------------------------------------------------------------------------}
{              INITIALIZED DOS/DPMI/WIN/NT/OS2 PUBLIC VARIABLES             }
{---------------------------------------------------------------------------}
CONST
   StreamError : Pointer = Nil;                       { Stream error ptr }

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        STREAM REGISTRATION RECORDS                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{                      TCollection STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RCollection: TStreamRec = (
     ObjType: 50;                                     { Register id = 50 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TCollection)^);              { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TCollection);                    { Alt style VMT link }
     {$ENDIF}
     Load: @TCollection.Load;                         { Object load method }
     Store: @TCollection.Store);                      { Object store method }

{---------------------------------------------------------------------------}
{                   TStringCollection STREAM REGISTRATION                   }
{---------------------------------------------------------------------------}
CONST
   RStringCollection: TStreamRec = (
     ObjType: 51;                                     { Register id = 51 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TStringCollection)^);        { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TStringCollection);              { Alt style VMT link }
     {$ENDIF}
     Load: @TStringCollection.Load;                   { Object load method }
     Store: @TStringCollection.Store);                { Object store method }

{---------------------------------------------------------------------------}
{                     TStrCollection STREAM REGISTRATION                    }
{---------------------------------------------------------------------------}
CONST
   RStrCollection: TStreamRec = (
     ObjType: 69;                                     { Register id = 69 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TStrCollection)^);           { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TStrCollection);                 { Alt style VMT link }
     {$ENDIF}
     Load:    @TStrCollection.Load;                   { Object load method }
     Store:   @TStrCollection.Store);                 { Object store method }

{---------------------------------------------------------------------------}
{                      TStringList STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RStringList: TStreamRec = (
     ObjType: 52;                                     { Register id = 52 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TStringList)^);              { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TStringList);                    { Alt style VMT link }
     {$ENDIF}
     Load: @TStringList.Load;                         { Object load method }
     Store: Nil);                                     { No store method }

{---------------------------------------------------------------------------}
{                     TStrListMaker STREAM REGISTRATION                     }
{---------------------------------------------------------------------------}
CONST
   RStrListMaker: TStreamRec = (
     ObjType: 52;                                     { Register id = 52 }
     {$IFDEF BP_VMTLink}
     VmtLink: Ofs(TypeOf(TStrListMaker)^);            { BP style VMT link }
     {$ELSE}
     VmtLink: TypeOf(TStrListMaker);                  { Alt style VMT link }
     {$ENDIF}
     Load: Nil;                                       { No load method }
     Store: @TStrListMaker.Store);                    { Object store method }

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
USES XMSUnit, EMSUnit;                                { Needs these units }
{$ENDIF}

{$IFNDEF PROC_Real}                                   { NOT DOS REAL CODE }
  {$DEFINE NewExeFormat}                              { New format EXE }
{$ENDIF}

{$IFDEF OS_OS2}                                       { OS2 COMPILERS }

  {$IFDEF PPC_Virtual}                                { VIRTUAL PASCAL UNITS }
  USES OS2Base;                                       { Standard unit }
  {$ENDIF}

  {$IFDEF PPC_Speed}                                  { SPEED PASCAL UNITS }
  USES BseDos, Os2Def;                                { Standard units }
  {$ENDIF}

  {$IFDEF PPC_BPOS2}                                  { C'T PATCH TO BP UNITS }
  USES DosTypes, DosProcs;                            { Standard units }

  TYPE FILEFINDBUF = TFILEFINDBUF;                    { Type correction }
  {$ENDIF}
{$ENDIF}

{***************************************************************************}
{                         PRIVATE TYPE DEFINITIONS                          }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                      FRAME POINTER SIZE SWITCH TYPE                       }
{---------------------------------------------------------------------------}
TYPE
   FramePointer = sw_Word;                            { Frame pointer }

{ ******************************* REMARK ****************************** }
{  This TYPECAST is serverely COMPILER SPECIFIC if you have a different }
{  compiler you will probably have to work this out.                    }
{ ****************************** END REMARK *** Leon de Boer, 08Jul99 * }

{---------------------------------------------------------------------------}
{                 POINTER LOCAl FUNCTION DEFINITION SWITCH                  }
{---------------------------------------------------------------------------}
TYPE
   {$IFDEF PPC_VIRTUAL}                               { VIRTUAL PASCAL }
     FuncCallPtr = FUNCTION (Param1: Pointer): Boolean;
   {$ELSE}                                            { OTHER COMPILERS }
     {$IFNDEF PPC_FPC}                                { NON FPC COMPILERS }
     FuncCallPtr = FUNCTION (Param1: Pointer; _EBP: FramePointer): Boolean;
     {$ELSE}                                          { FPC COMPILER }
     FuncCallPtr = FUNCTION (_EBP: FramePointer; Param1: Pointer): Boolean;
     {$ENDIF}
   {$ENDIF}

{***************************************************************************}
{                      PRIVATE INITIALIZED VARIABLES                        }
{***************************************************************************}

{---------------------------------------------------------------------------}
{              INITIALIZED DOS/DPMI/WIN/NT/OS2 PRIVATE VARIABLES            }
{---------------------------------------------------------------------------}
CONST
   StreamTypes: PStreamRec = Nil;                     { Stream types reg }

{***************************************************************************}
{                          PRIVATE INTERNAL ROUTINES                        }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{              PRIVATE INTERNAL DOS/DPMI/WIN/NT/OS2 ROUTINES                }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{ ******************************* REMARK ****************************** }
{  This routine is serverely COMPILER SPECIFIC if you have a different  }
{  compiler you will probably have to work this out.                    }
{ ****************************** END REMARK *** Leon de Boer, 08Jul99 * }

{---------------------------------------------------------------------------}
{   PrevFramePtr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB     }
{---------------------------------------------------------------------------}
FUNCTION PrevFramePtr: FramePointer; ASSEMBLER;
{$IFNDEF PPC_FPC}                                     { NON FPC COMPILER }
ASM
   {$IFDEF BIT_16}                                    { 16 BIT CODE }
     MOV AX, [BP];                                    { Load AX from BP }
     {$IFDEF OS_WINDOWS}                              { WIN 16 BIT CODE }
     AND AL, 0FEH;                                    { Windows make even }
     {$ENDIF}
     {$IFDEF OS_OS2}                                  { OS2 16 BIT CODE }
     AND AL, 0FEH;                                    { OS2 make even }
     {$ENDIF}
   {$ENDIF}
   {$IFDEF BIT_32}                                    { 32 BIT CODE }
   MOV EAX, [EBP];                                    { Get previous frame }
   {$ENDIF}
END;
{$ELSE}                                               { FPC COMPILER }
ASM
   {$IFDEF i386}                                      { 80x PROCESSOR }
   MOVL (%EBP), %EAX;                                 { Get previous frame }
   {$ENDIF}
   {$IFDEF m68k}                                      { 68x PROCESSOR }
   MOVE.L A6, D0;                                     { Get previous frame }
   {$ENDIF}
END ['EAX'];
{$ENDIF}

{ ******************************* REMARK ****************************** }
{  This routine is serverely COMPILER SPECIFIC if you have a different  }
{  compiler you will probably have to work this out.                    }
{ ****************************** END REMARK *** Leon de Boer, 08Jul99 * }

{---------------------------------------------------------------------------}
{   CallPointerLocal -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB }
{---------------------------------------------------------------------------}
{$IFDEF PPC_VIRTUAL}                                  { VIRTUAL COMPILER }
FUNCTION CallTestLocal (Func: Pointer; Param1: Pointer): Boolean;
BEGIN
   CallTestLocal := FuncCallPtr(Func)(Param1);        { Function call to ptr }
END;
{$ELSE}                                               { OTHER COMPILERS }
FUNCTION CallTestLocal (Func: Pointer; Frame: FramePointer;
Param1: Pointer): Boolean;
BEGIN
   {$IFNDEF PPC_FPC}                                  { NON FPC COMPILERS }
   CallTestLocal := FuncCallPtr(Func)(Param1, Frame); { Function call to ptr }
   {$ELSE}                                            { FPC COMPILER }
   CallTestLocal := FuncCallPtr(Func)(Frame, Param1); { Function call to ptr }
   {$ENDIF}
END;
{$ENDIF}

{---------------------------------------------------------------------------}
{  RegisterError -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Jun96 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE RegisterError;
BEGIN
   RunError(212);                                     { Register error }
END;

{***************************************************************************}
{                               OBJECT METHODS                              }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TRect OBJECT METHODS                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
PROCEDURE CheckEmpty (Var Rect: TRect);
BEGIN
   With Rect Do Begin
     If (A.X >= B.X) OR (A.Y >= B.Y) Then Begin       { Zero or reversed }
       A.X := 0;                                      { Clear a.x }
       A.Y := 0;                                      { Clear a.y }
       B.X := 0;                                      { Clear b.x }
       B.Y := 0;                                      { Clear b.y }
     End;
   End;
END;

{--TRect--------------------------------------------------------------------}
{  Empty -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TRect.Empty: Boolean;
BEGIN
   Empty := (A.X >= B.X) OR (A.Y >= B.Y);             { Empty result }
END;

{--TRect--------------------------------------------------------------------}
{  Equals -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TRect.Equals (R: TRect): Boolean;
BEGIN
   Equals := (A.X = R.A.X) AND (A.Y = R.A.Y) AND
     (B.X = R.B.X) AND (B.Y = R.B.Y);                 { Equals result }
END;

{--TRect--------------------------------------------------------------------}
{  Contains -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TRect.Contains (P: TPoint): Boolean;
BEGIN
   Contains := (P.X >= A.X) AND (P.X <= B.X) AND
     (P.Y >= A.Y) AND (P.Y <= B.Y);                   { Contains result }
END;

{--TRect--------------------------------------------------------------------}
{  Copy -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Copy (R: TRect);
BEGIN
   A := R.A;                                          { Copy point a }
   B := R.B;                                          { Copy point b }
END;

{--TRect--------------------------------------------------------------------}
{  Union -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Union (R: TRect);
BEGIN
   If (R.A.X < A.X) Then A.X := R.A.X;                { Take if smaller }
   If (R.A.Y < A.Y) Then A.Y := R.A.Y;                { Take if smaller }
   If (R.B.X > B.X) Then B.X := R.B.X;                { Take if larger }
   If (R.B.Y > B.Y) Then B.Y := R.B.Y;                { Take if larger }
END;

{--TRect--------------------------------------------------------------------}
{  Intersect -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Intersect (R: TRect);
BEGIN
   If (R.A.X > A.X) Then A.X := R.A.X;                { Take if larger }
   If (R.A.Y > A.Y) Then A.Y := R.A.Y;                { Take if larger }
   If (R.B.X < B.X) Then B.X := R.B.X;                { Take if smaller }
   If (R.B.Y < B.Y) Then B.Y := R.B.Y;                { Take if smaller }
   CheckEmpty(Self);                                  { Check if empty }
END;

{--TRect--------------------------------------------------------------------}
{  Move -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Move (ADX, ADY: Integer);
BEGIN
   Inc(A.X, ADX);                                     { Adjust A.X }
   Inc(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
END;

{--TRect--------------------------------------------------------------------}
{  Grow -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Grow (ADX, ADY: Integer);
BEGIN
   Dec(A.X, ADX);                                     { Adjust A.X }
   Dec(A.Y, ADY);                                     { Adjust A.Y }
   Inc(B.X, ADX);                                     { Adjust B.X }
   Inc(B.Y, ADY);                                     { Adjust B.Y }
   CheckEmpty(Self);                                  { Check if empty }
END;

{--TRect--------------------------------------------------------------------}
{  Assign -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TRect.Assign (XA, YA, XB, YB: Integer);
BEGIN
   A.X := XA;                                         { Hold A.X value }
   A.Y := YA;                                         { Hold A.Y value }
   B.X := XB;                                         { Hold B.X value }
   B.Y := YB;                                         { Hold B.Y value }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TObject OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

TYPE
   DummyObject = OBJECT (TObject)                     { Internal object }
     Data: RECORD END;                                { Helps size VMT link }
   END;

{ ******************************* REMARK ****************************** }
{ I Prefer this code because it self sizes VMT link rather than using a }
{ fixed record structure thus it should work on all compilers without a }
{ specific record to match each compiler.                               }
{ ****************************** END REMARK *** Leon de Boer, 10May96 * }

{--TObject------------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TObject.Init;
VAR LinkSize: LongInt; Dummy: DummyObject; P: Pointer;
BEGIN
   LinkSize := LongInt(@Dummy.Data)-LongInt(@Dummy);  { Calc VMT link size }
   P := Pointer(LongInt(@Self)+LinkSize);             { Pointer to data }
   FillChar(P^, SizeOf(Self)-LinkSize, #0);           { Clear data fields }
END;

{--TObject------------------------------------------------------------------}
{  Free -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TObject.Free;
BEGIN
   Dispose(PObject(@Self), Done);                     { Dispose of self }
END;

{--TObject------------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TObject.Done;
BEGIN                                                 { Abstract method }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TStream OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStream------------------------------------------------------------------}
{  Get -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14Aug98 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TStream.Get: PObject;

TYPE LoadPtr = FUNCTION (Var S: TStream; Link: Sw_Word; Iv: Pointer): PObject;

VAR ObjType: Word; P: PStreamRec;
BEGIN
   ObjType := 0;                                      { Zero the value }
   Read(ObjType, 2);                                  { Read object type }
   If (ObjType <> 0) Then Begin                       { Object registered }
     P := StreamTypes;                                { Current reg list }
     While (P <> Nil) AND (P^.ObjType <> ObjType)     { Find object type OR }
       Do P := P^.Next;                               { Find end of chain }
     If (P = Nil) Then Begin                          { Not registered }
       Error(stGetError, ObjType);                    { Obj not registered }
       Get := Nil;                                    { Return nil pointer }
     End Else
       {$IFDEF BP_VMTLink}                            { BP like VMT link }
       Get := LoadPtr(P^.Load)(Self, P^.VMTLink, Nil) { Call constructor }
       {$ELSE}                                        { FPC/DELPHI3 VMT link }
       Get := LoadPtr(P^.Load)(Self,
         Sw_Word(P^.VMTLink^), Nil)                   { Call constructor }
       {$ENDIF}
   End Else Get := Nil;                               { Return nil pointer }
END;

{--TStream------------------------------------------------------------------}
{  StrRead -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStream.StrRead: PChar;
VAR W: Word; P: PChar;
BEGIN
   W := 0;                                            { Zero the value }
   Read(W, 2);                                        { Read string length }
   If (W = 0) Then StrRead := Nil Else Begin          { Check for empty }
     If (MaxAvail >= (W+1)) Then Begin                { Check avail memory }
       GetMem(P, W + 1);                              { Allocate memory }
       Read(P[0], W);                                 { Read the data }
       P[W] := #0;                                    { Terminate with #0 }
     End Else P := Nil;                               { Not enough memory }
     StrRead := P;                                    { PChar returned }
   End;
END;

{--TStream------------------------------------------------------------------}
{  ReadStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 06Aug98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStream.ReadStr: PString;
VAR B: Byte; P: PString;
BEGIN
   Read(B, 1);                                        { Read string length }
   If (B > 0) AND (MaxAvail >= (B+1)) Then Begin      { Check enough memory }
     GetMem(P, B + 1);                                { Allocate memory }
     {$IFDEF PPC_DELPHI3}                             { DELPHI 3+ COMPILER }
     SetLength(P^, B);                                { Hold new length }
     {$ELSE}                                          { OTHER COMPILERS }
     P^[0] := Chr(B);                                 { Hold new length }
     {$ENDIF}
     Read(P^[1], B);                                  { Read string data }
     ReadStr := P;                                    { Return string ptr }
   End Else ReadStr := Nil;
END;

{--TStream------------------------------------------------------------------}
{  GetPos -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TStream.GetPos: LongInt;
BEGIN
   If (Status = stOk) Then GetPos := Position         { Return position }
     Else GetPos := -1;                               { Stream in error }
END;

{--TStream------------------------------------------------------------------}
{  GetSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStream.GetSize: LongInt;
BEGIN
   If (Status = stOk) Then GetSize := StreamSize      { Return stream size }
     Else GetSize := -1;                              { Stream in error }
END;

{--TStream------------------------------------------------------------------}
{  Close -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Close;
BEGIN                                                 { Abstract method }
END;

{--TStream------------------------------------------------------------------}
{  Reset -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Reset;
BEGIN
   Status := 0;                                       { Clear status }
   ErrorInfo := 0;                                    { Clear error info }
END;

{--TStream------------------------------------------------------------------}
{  Flush -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Flush;
BEGIN                                                 { Abstract method }
END;

{--TStream------------------------------------------------------------------}
{  Truncate -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Truncate;
BEGIN
   Abstract;                                          { Abstract error }
END;

{--TStream------------------------------------------------------------------}
{  Get -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14Aug98 LdB               }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Put (P: PObject);
TYPE StorePtr = PROCEDURE (Var S: TStream; AnObject: PObject);

VAR ObjType: Word; Link: Sw_Word; Q: PStreamRec; VmtPtr: ^Sw_Word;
BEGIN
   ObjType := 0;                                      { Set objtype to zero }
   If (P <> Nil) Then Begin                           { Non nil object }
     VmtPtr := Pointer(P);                            { Xfer object to ptr }
     Link := VmtPtr^;                                 { VMT link }
     If (Link <> 0) Then Begin                        { We have a VMT link }
       Q := StreamTypes;                              { Current reg list }
       {$IFDEF BP_VMTLink}                            { BP like VMT link }
       While (Q <> Nil) AND (Q^.VMTLink <> Link)      { Find link match OR }
       {$ELSE}                                        { FPC/DELHI3 VMT link }
       While (Q <> Nil) AND (Sw_Word(Q^.VMTLink^) <>
         Link)                                        { Find link match OR }
       {$ENDIF}
         Do Q := Q^.Next;                             { Find end of chain }
       If (Q = Nil) Then Begin                        { End of chain found }
         Error(stPutError, 0);                        { Not registered error }
         Exit;                                        { Now exit }
       End Else ObjType := Q^.ObjType;                { Update object type }
     End;
   End;
   Write(ObjType, 2);                                 { Write object type }
   If (ObjType <> 0) Then                             { Registered object }
     StorePtr(Q^.Store)(Self, P);                     { Store object }
END;

{--TStream------------------------------------------------------------------}
{  Seek -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Seek (Pos: LongInt);
BEGIN
   If (Status = stOk) Then Begin                      { Check status }
     If (Pos < 0) Then Pos := 0;                      { Remove negatives }
     If (Pos <= StreamSize) Then Position := Pos      { If valid set pos }
       Else Error(stSeekError, Pos);                  { Position error }
   End;
END;

{--TStream------------------------------------------------------------------}
{  StrWrite -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStream.StrWrite (P: PChar);
VAR W: Word; Q: PByteArray;
BEGIN
   W := 0;                                            { Preset zero size }
   Q := PByteArray(P);                                { Transfer type }
   If (Q <> Nil) Then While (Q^[W] <> 0) Do Inc(W);   { PChar length }
   Write(W, SizeOf(W));                               { Store length }
   If (P <> Nil) Then Write(P[0], W);                 { Write data }
END;

{--TStream------------------------------------------------------------------}
{  WriteStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStream.WriteStr (P: PString);
CONST Empty: String[1] = '';
BEGIN
   If (P <> Nil) Then Write(P^, Length(P^) + 1)       { Write string }
     Else Write(Empty, 1);                            { Write empty string }
END;

{--TStream------------------------------------------------------------------}
{  Open -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Open (OpenMode: Word);
BEGIN                                                 { Abstract method }
END;

{--TStream------------------------------------------------------------------}
{  Error -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Error (Code, Info: Integer);
TYPE TErrorProc = Procedure (Var S: TStream);
BEGIN
   Status := Code;                                    { Hold error code }
   ErrorInfo := Info;                                 { Hold error info }
   If (StreamError <> Nil) Then
     TErrorProc(StreamError)(Self);                   { Call error ptr }
END;

{--TStream------------------------------------------------------------------}
{  Read -> Platforms DOS/DPMI/WIN/NT/OS2 , Updated 10May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Read (Var Buf; Count: Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

{--TStream------------------------------------------------------------------}
{  Write -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStream.Write (Var Buf; Count: Word);
BEGIN
   Abstract;                                          { Abstract error }
END;

{--TStream------------------------------------------------------------------}
{  CopyFrom -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStream.CopyFrom (Var S: TStream; Count: LongInt);
VAR W: Word; Buffer: Array[0..1023] of Byte;
BEGIN
   While (Count > 0) Do Begin
     If (Count > SizeOf(Buffer)) Then                 { To much data }
       W := SizeOf(Buffer) Else W := Count;           { Size to transfer }
     S.Read(Buffer, W);                               { Read from stream }
     Write(Buffer, W);                                { Write to stream }
     Dec(Count, W);                                   { Dec write count }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TDosStream OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TDosStream---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TDosStream.Init (FileName: FNameStr; Mode: Word);
VAR Success: Integer; {$IFDEF OS_OS2} Info: FILEFINDBUF; {$ENDIF}
BEGIN
   Inherited Init;                                    { Call ancestor }
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     {$IFDEF BIT_16}                                  { 16 BIT WINDOWS CODE }
     AnsiToOEM(FileName, FName);                      { Ansi to OEM }
     {$ENDIF}
     {$IFDEF BIT_32}                                  { 32 BIT WINDOWS CODE }
       {$IFNDEF PPC_SPEED}                            { NON SPEED COMPILER }
       CharToOEM(FileName, FName);                    { Ansi to OEM }
       {$ELSE}                                        { SPEEDSOFT SYBIL 2+ }
       CharToOEM(CString(FileName), CString(FName));  { Ansi to OEM }
       {$ENDIF}
     {$ENDIF}
   {$ELSE}                                            { DOS/DPMI/OS2 CODE }
     FileName := FileName+#0;                         { Make asciiz }
     Move(FileName[1], FName, Length(FileName));      { Create asciiz name }
   {$ENDIF}
   Handle := FileOpen(FName, Mode);                   { Open the file }
   If (Handle <> 0) Then Begin                        { Handle valid }
     Success := SetFilePos(Handle, 0, 2, StreamSize); { Locate end of file }
     If (Success = 0) Then
       Success := SetFilePos(Handle, 0, 0, Position); { Reset to file start }
   End Else Success := 103;                           { Open file failed }
   If (Handle = 0) OR (Success <> 0) Then Begin       { Open failed }
     Handle := -1;                                    { Reset invalid handle }
     Error(stInitError, Success);                     { Call stream error }
   End;
END;

{--TDosStream---------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 16May96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TDosStream.Done;
BEGIN
   If (Handle <> -1) Then FileClose(Handle);          { Close the file }
   Inherited Done;                                    { Call ancestor }
END;

{--TDosStream---------------------------------------------------------------}
{  Close -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 16May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Close;
BEGIN
   If (Handle <> -1) Then FileClose(Handle);          { Close the file }
   Position := 0;                                     { Zero the position }
   Handle := -1;                                      { Handle now invalid }
END;

{--TDosStream---------------------------------------------------------------}
{  Truncate -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 16May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Truncate;
VAR Success: Integer;
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     Success := SetFileSize(Handle, Position);        { Truncate file }
     If (Success = 0) Then StreamSize := Position     { Adjust size }
       Else Error(stError, Success);                  { Identify error }
   End;
END;

{--TDosStream---------------------------------------------------------------}
{  Seek -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 16May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Seek (Pos: LongInt);
VAR Success: Integer; Li: LongInt;
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Pos < 0) Then Pos := 0;                      { Negatives removed }
     If (Handle = -1) Then Success := 103 Else        { File not open }
       Success := SetFilePos(Handle, Pos, 0, Li);     { Set file position }
     If ((Success = -1) OR (Li <> Pos)) Then Begin    { We have an error }
       If (Success = -1) Then Error(stSeekError, 0)   { General seek error }
         Else Error(stSeekError, Success);            { Specific seek error }
     End Else Position := Li;                         { Adjust position }
   End;
END;

{--TDosStream---------------------------------------------------------------}
{  Open -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 16May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Open (OpenMode: Word);
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Handle = -1) Then Begin                      { File not open }
       Handle := FileOpen(FName, OpenMode);           { Open the file }
       Position := 0;                                 { Reset position }
       If (Handle = 0) Then Begin                     { File open failed }
         Handle := -1;                                { Reset handle }
         Error(stOpenError, 103);                     { Call stream error }
       End;
     End Else Error(stOpenError, 104);                { File already open }
   End;
END;

{--TDosStream---------------------------------------------------------------}
{  Read -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Read (Var Buf; Count: Word);
VAR Success: Integer; Ri, W: Word; Moved: Sw_Word; P: PByteArray;
BEGIN
   If (Position + Count > StreamSize) Then            { Insufficient data }
     Error(stReadError, 0);                           { Read beyond end!!! }
   If (Handle = -1) Then Error(stReadError, 103);     { File not open }
   P := @Buf;                                         { Transfer address }
   Ri := 0;                                           { Zero read index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := FileRead(Handle, P^[Ri], W, Moved);   { Read from file }
     If ((Success <> 0) OR (Moved <> W))              { Error was detected }
     Then Begin
       Moved := 0;                                    { Clear bytes moved }
       If (Success <> 0) Then
         Error(stReadError, Success)                  { Specific read error }
         Else Error(stReadError, 0);                  { Non specific error }
     End;
     Inc(Position, Moved);                            { Adjust position }
     Inc(Ri, Moved);                                  { Adjust read index }
     Dec(Count, Moved);                               { Adjust count left }
   End;
   If (Count <> 0) Then FillChar(P^[Ri], Count, #0);  { Error clear buffer }
END;

{--TDosStream---------------------------------------------------------------}
{  Write -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TDosStream.Write (Var Buf; Count: Word);
VAR Success: Integer; W, Wi: Word; Moved: Sw_Word; P: PByteArray;
BEGIN
   If (Handle = -1) Then Error(stWriteError, 103);    { File not open }
   P := @Buf;                                         { Transfer address }
   Wi := 0;                                           { Zero write index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := FileWrite(Handle, P^[Wi], W, Moved);  { Write to file }
     If ((Success <> 0) OR (Moved <> W))              { Error was detected }
     Then Begin
       Moved := 0;                                    { Clear bytes moved }
       If (Success <> 0) Then
         Error(stWriteError, Success)                 { Specific write error }
         Else Error(stWriteError, 0);                 { Non specific error }
     End;
     Inc(Position, Moved);                            { Adjust position }
     Inc(Wi, Moved);                                  { Adjust write index }
     Dec(Count, Moved);                               { Adjust count left }
     If (Position > StreamSize) Then                  { File expanded }
       StreamSize := Position;                        { Adjust stream size }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TBufStream OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TBufStream---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TBufStream.Init (FileName: FNameStr; Mode, Size: Word);
BEGIN
   Inherited Init(FileName, Mode);                    { Call ancestor }
   If (Size <> 0) AND (MaxAvail >= Size) Then Begin
     GetMem(Buffer, Size);                            { Allocate buffer }
     BufSize := Size;                                 { Hold buffer size }
   End;
   If (Buffer = Nil) Then Error(stInitError, 0);      { Buffer allocate fail }
END;

{--TBufStream---------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TBufStream.Done;
BEGIN
   Flush;                                             { Flush the file }
   Inherited Done;                                    { Call ancestor }
   If (Buffer <> Nil) Then FreeMem(Buffer, BufSize);  { Release buffer }
END;

{--TBufStream---------------------------------------------------------------}
{  Close -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Close;
BEGIN
   Flush;                                             { Flush the buffer }
   Inherited Close;                                   { Call ancestor }
END;

{--TBufStream---------------------------------------------------------------}
{  Flush -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Flush;
VAR Success: Integer; W: Sw_Word;
BEGIN
   If (LastMode = 2) AND (BufPtr <> 0) Then Begin     { Must update file }
     If (Handle = -1) Then Success := 103             { File is not open }
       Else Success := FileWrite(Handle, Buffer^,
         BufPtr, W);                                  { Write to file }
     If (Success <> 0) OR (W <> BufPtr) Then          { We have an error }
       If (Success = 0) Then Error(stWriteError, 0)   { Unknown write error }
         Else Error(stError, Success);                { Specific write error }
   End;
   BufPtr := 0;                                       { Reset buffer ptr }
   BufEnd := 0;                                       { Reset buffer end }
END;

{--TBufStream---------------------------------------------------------------}
{  Truncate -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Truncate;
BEGIN
   Flush;                                             { Flush buffer }
   Inherited Truncate;                                { Truncate file }
END;

{--TBufStream---------------------------------------------------------------}
{  Seek -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Seek (Pos: LongInt);
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Position <> Pos) Then Begin                  { Move required }
       Flush;                                         { Flush the buffer }
       Inherited Seek(Pos);                           { Call ancestor }
     End;
   End;
END;

{--TBufStream---------------------------------------------------------------}
{  Open -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 17May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Open (OpenMode: Word);
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     BufPtr := 0;                                     { Clear buffer start }
     BufEnd := 0;                                     { Clear buffer end }
     Inherited Open(OpenMode);                        { Call ancestor }
   End;
END;

{--TBufStream---------------------------------------------------------------}
{  Read -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Read (Var Buf; Count: Word);
VAR Success: Integer; W, Bw, Ri: Word; Br: Sw_Word; P: PByteArray;
BEGIN
   If (Position + Count > StreamSize) Then            { Read pas stream end }
     Error(stReadError, 0);                           { Call stream error }
   If (Handle = -1) Then Error(stReadError, 103);     { File not open }
   P := @Buf;                                         { Transfer address }
   Ri := 0;                                           { Zero read index }
   If (LastMode = 2) Then Flush;                      { Flush write buffer }
   LastMode := 1;                                     { Now set read mode }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     If (BufPtr = BufEnd) Then Begin                  { Buffer is empty }
       If (Position + BufSize > StreamSize) Then
         Bw := StreamSize - Position                  { Amount of file left }
         Else Bw := BufSize;                          { Full buffer size }
       Success := FileRead(Handle, Buffer^, Bw, Br);  { Read from file }
       If ((Success <> 0) OR (Bw <> Br)) Then Begin   { Error was detected }
         If (Success <> 0) Then
           Error(stReadError, Success)                { Specific read error }
           Else Error(stReadError, 0);                { Non specific error }
       End Else Begin
         BufPtr := 0;                                 { Reset BufPtr }
         BufEnd := Bw;                                { End of buffer }
       End;
     End;
     If (Status = stOk) Then Begin                    { Status still okay }
       W := BufEnd - BufPtr;                          { Space in buffer }
       If (Count < W) Then W := Count;                { Set transfer size }
       Move(Buffer^[BufPtr], P^[Ri], W);              { Data from buffer }
       Dec(Count, W);                                 { Reduce count }
       Inc(BufPtr, W);                                { Advance buffer ptr }
       Inc(Ri, W);                                    { Increase read index }
       Inc(Position, W);                              { Advance position }
     End;
   End;
   If (Status <> stOk) AND (Count > 0) Then
     FillChar(P^[Ri], Count, #0);                     { Error clear buffer }
END;

{--TBufStream---------------------------------------------------------------}
{  Write -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 27Oct99 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TBufStream.Write (Var Buf; Count: Word);
VAR Success: Integer; W, Wi: Word; Bw: Sw_Word; P: PByteArray;
BEGIN
   If (Handle = -1) Then Error(stWriteError, 103);    { File not open }
   If (LastMode = 1) Then Flush;                      { Flush read buffer }
   LastMode := 2;                                     { Now set write mode }
   P := @Buf;                                         { Transfer address }
   Wi := 0;                                           { Zero write index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     If (BufPtr = BufSize) Then Begin                 { Buffer is full }
       Success := FileWrite(Handle, Buffer^, BufSize,
         Bw);                                         { Write to file }
       If (Success <> 0) OR (Bw <> BufSize) Then      { We have an error }
         If (Success=0) Then Error(stWriteError, 0)   { Unknown write error }
           Else Error(stError, Success);              { Specific write error }
       BufPtr := 0;                                   { Reset BufPtr }
     End;
     If (Status = stOk) Then Begin                    { Status still okay }
       W := BufSize - BufPtr;                         { Space in buffer }
       If (Count < W) Then W := Count;                { Transfer size }
       Move(P^[Wi], Buffer^[BufPtr], W);              { Data to buffer }
       Dec(Count, W);                                 { Reduce count }
       Inc(BufPtr, W);                                { Advance buffer ptr }
       Inc(Wi, W);                                    { Advance write index }
       Inc(Position, W);                              { Advance position }
       If (Position > StreamSize) Then                { File has expanded }
         StreamSize := Position;                      { Update new size }
     End;
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TMemoryStream OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMemoryStream------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMemoryStream.Init (ALimit: LongInt; ABlockSize: Word);
VAR W: Word;
BEGIN
   Inherited Init;                                    { Call ancestor }
   If (ABlockSize = 0) Then BlkSize := 8192 Else      { Default blocksize }
     BlkSize := ABlockSize;                           { Set blocksize }
   If (ALimit = 0) Then W := 1 Else                   { At least 1 block }
     W := (ALimit + BlkSize - 1) DIV BlkSize;         { Blocks needed }
   If NOT ChangeListSize(W) Then                      { Try allocate blocks }
      Error(stInitError, 0);                          { Initialize error }
END;

{--TMemoryStream------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TMemoryStream.Done;
BEGIN
   ChangeListSize(0);                                 { Release all memory }
   Inherited Done;                                    { Call ancestor }
END;

{--TMemoryStream------------------------------------------------------------}
{  Truncate -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TMemoryStream.Truncate;
VAR W: Word;
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Position = 0) Then W := 1 Else               { At least one block }
       W := (Position + BlkSize - 1) DIV BlkSize;     { Blocks needed }
     If ChangeListSize(W) Then StreamSize := Position { Set stream size }
       Else Error(stError, 0);                        { Error truncating }
   End;
END;

{--TMemoryStream------------------------------------------------------------}
{  Read -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TMemoryStream.Read (Var Buf; Count: Word);
VAR W, CurBlock, BlockPos, Op: Word; Li: LongInt; P, Q: PByteArray;
BEGIN
   If (Position + Count > StreamSize) Then            { Insufficient data }
     Error(stReadError, 0);                           { Read beyond end!!! }
   P := @Buf;                                         { Transfer address }
   Op := 0;                                           { Zero offset position }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     CurBlock := Position DIV BlkSize;                { Current block }
     { * REMARK * - Do not shorten this, result can be > 64K }
     Li := CurBlock;                                  { Transfer current block }
     Li := Li * BlkSize;                              { Current position }
     { * REMARK END * - Leon de Boer }
     BlockPos := Position - Li;                       { Current position }
     W := BlkSize - BlockPos;                         { Current block space }
     If (W > Count) Then W := Count;                  { Adjust read size }
     Q := BlkList^[CurBlock];                         { Calc pointer }
     Move(Q^[BlockPos], P^[Op], W);                   { Move data to buffer }
     Inc(Position, W);                                { Adjust position }
     Inc(Op, W);                                      { Increase offset }
     Dec(Count, W);                                   { Adjust count left }
   End;
   If (Count <> 0) Then FillChar(P^[Op], Count, #0);  { Error clear buffer }
END;

{--TMemoryStream------------------------------------------------------------}
{  Write -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TMemoryStream.Write (Var Buf; Count: Word);
VAR W, CurBlock, BlockPos, Op: Word; Li: LongInt; P, Q: PByteArray;
BEGIN
   If (Position + Count > MemSize) Then Begin         { Expansion needed }
     If (Position + Count = 0) Then W := 1 Else       { At least 1 block }
       W := (Position+Count+BlkSize-1) DIV BlkSize;   { Blocks needed }
     If NOT ChangeListSize(W) Then
       Error(stWriteError, 0);                        { Expansion failed!!! }
   End;
   P := @Buf;                                         { Transfer address }
   Op := 0;                                           { Zero offset position }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     CurBlock := Position DIV BlkSize;                { Current segment }
     { * REMARK * - Do not shorten this, result can be > 64K }
     Li := CurBlock;                                  { Transfer current block }
     Li := Li * BlkSize;                              { Current position }
     { * REMARK END * - Leon de Boer }
     BlockPos := Position - Li;                       { Current position }
     W := BlkSize - BlockPos;                         { Current block space }
     If (W > Count) Then W := Count;                  { Adjust write size }
     Q := BlkList^[CurBlock];                         { Calc pointer }
     Move(P^[Op], Q^[BlockPos], W);                   { Transfer data }
     Inc(Position, W);                                { Adjust position }
     Inc(Op, W);                                      { Increase offset }
     Dec(Count, W);                                   { Adjust count left }
     If (Position > StreamSize) Then                  { File expanded }
       StreamSize := Position;                        { Adjust stream size }
   End;
END;

{***************************************************************************}
{                      TMemoryStream PRIVATE METHODS                        }
{***************************************************************************}

{--TMemoryStream------------------------------------------------------------}
{  ChangeListSize -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 19May96 LdB    }
{---------------------------------------------------------------------------}
FUNCTION TMemoryStream.ChangeListSize (ALimit: Word): Boolean;
VAR I, W, Bas: Word; P: PPointerArray;
BEGIN
   If (ALimit <> BlkCount) Then Begin                 { Change is needed }
     ChangeListSize := False;                         { Preset failure }
     If (ALimit > MaxPtrs) Then Exit;                 { To many blocks req }
     If (ALimit <> 0) Then Begin                      { Create segment list }
       Bas := ALimit * SizeOf(Pointer);               { Block array size }
       If (MaxAvail > Bas) Then Begin
         GetMem(P, Bas);                              { Allocate memory }
         FillChar(P^, Bas, #0);                       { Clear the memory }
       End Else Exit;                                 { Insufficient memory }
       If (BlkCount <> 0) AND (BlkList <> Nil) Then   { Current list valid }
         If (BlkCount <= ALimit) Then Move(BlkList^,
           P^, BlkCount * SizeOf(Pointer)) Else       { Move whole old list }
           Move(BlkList^, P^, Bas);                   { Move partial list }
     End Else P := Nil;                               { No new block list }
     If (ALimit < BlkCount) Then                      { Shrink stream size }
       For W := BlkCount-1 DownTo ALimit Do
         FreeMem(BlkList^[W], BlkSize);               { Release memory block }
     If (P <> Nil) AND (ALimit > BlkCount) Then Begin { Expand stream size }
       For W := BlkCount To ALimit-1 Do Begin
         If (MaxAvail < BlkSize) Then Begin           { Check enough memory }
           For I := BlkCount To W-1 Do
             FreeMem(P^[I], BlkSize);                 { Free mem allocated }
           FreeMem(P, Bas);                           { Release memory }
           Exit;                                      { Now exit }
         End Else GetMem(P^[W], BlkSize);             { Allocate memory }
       End;
     End;
     If (BlkCount <> 0) AND (BlkList<>Nil) Then
       FreeMem(BlkList, BlkCount * SizeOf(Pointer));  { Release old list }
     BlkList := P;                                    { Hold new block list }
     BlkCount := ALimit;                              { Hold new count }
     { * REMARK * - Do not shorten this, result can be > 64K }
     MemSize := BlkCount;                             { Block count }
     MemSize := MemSize * BlkSize;                    { Current position }
     { * REMARK END * - Leon de Boer }
   End;
   ChangeListSize := True;                            { Successful }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TEmsStream OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TEmsStream---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Feb97 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TEmsStream.Init (MinSize, MaxSize: LongInt);
{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
VAR Success: Integer; MinPg, MaxPg: Word;
BEGIN
   Inherited Init;                                    { Call ancestor }
   If (EMS_MemAvail >= MaxSize) Then Begin            { Sufficient memory }
     If (MaxSize = 0) Then MaxPg := 1 Else            { At least one page }
       MaxPg := (MaxSize + 16383) DIV 16384;          { Max pages needed }
     If (MinSize = 0) Then MinPg := 1 Else            { At least one page }
       MinPg := (MinSize + 16383) DIV 16384;          { Min pages needed }
     Handle := EMS_GetMem(MaxPg);                     { Allocate EMS pages }
     If (Handle <> 0) Then Begin
       Success := 0;                                  { Preset success }
       PageCount := MaxPg;                            { Pages used }
       If (MaxPg <> MinPg) Then                       { Sizes differ }
         If (EMS_ResizeMem(MinPg, Handle)=0)          { Resize to minimum }
           Then PageCount := MinPg;                   { Hold new page count }
       { * REMARK * - Do not shorten this, result can be > 64K }
       MemSize := PageCount;
       MemSize := MemSize * 16384;
       { * REMARK END * - Leon de Boer }
     End Else Success := 403;                         { Failed to allocate }
   End Else Success := 400;                           { Insufficent EMS }
   If (Handle = 0) OR (Success <> 0) Then             { EMS failed }
     Error(stInitError, Success);                     { Call stream error }
END;
{$ELSE}                                               { ALL OTHER OS SYSTEMS }
BEGIN
   Inherited Init(MaxSize, 16384);                    { For compatability }
END;
{$ENDIF}

{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
{***************************************************************************}
{                    TEMSStream DOS REAL MODE ONLY METHODS                  }
{***************************************************************************}

{--TEmsStream---------------------------------------------------------------}
{  Done -> Platforms DOS REAL MODE - Updated!28Feb97 LdB                    }
{---------------------------------------------------------------------------}
DESTRUCTOR TEmsStream.Done;
BEGIN
   If (Handle <> 0) Then EMS_FreeMem(Handle);         { Release EMS blocks }
   Inherited Done;                                    { Call ancestor }
END;

{--TEmsStream---------------------------------------------------------------}
{  Truncate -> Platforms DOS REAL MODE - Updated 28Feb97 LdB                }
{---------------------------------------------------------------------------}
PROCEDURE TEmsStream.Truncate;
VAR Success: Integer; W: Word;
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Position = 0) Then W := 1 Else               { At least one page }
       W := (Position + 16383) DIV 16384;             { Pages to use }
     Success := 0;                                    { Preset success }
     If (W <> PageCount) Then                         { Sizes differ }
       If (EMS_ResizeMem(W, Handle)=0) Then           { Resize to this }
         PageCount := W Else Success := 401;          { Adjust blocks used }
     If (Success = 0) Then StreamSize := Position     { Adjust size }
       Else Error(stError, Success);                  { Identify error }
   End;
END;

{--TEmsStream---------------------------------------------------------------}
{  Read -> Platforms DOS REAL MODE - Updated 14Nov00 LdB                    }
{---------------------------------------------------------------------------}
PROCEDURE TEmsStream.Read (Var Buf; Count: Word);
VAR Success: Integer; W, Ri: Word; P: PByteArray;
BEGIN
   If (Position + Count > StreamSize) Then            { Insufficient data }
     Error(stReadError, 0);                           { Read beyond end!!! }
   If (Handle = 0) Then Error(stReadError, 403);      { EMS not available }
   P := @Buf;                                         { Transfer address }
   Ri := 0;                                           { Zero read index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := EMS_MoveMem(LongInt(@P^[Ri]), 0,
       Position, Handle, W);                          { Move the data }
     If (Success <> 0) Then Begin                     { Error was detected }
       W := 0;                                        { Clear bytes moved }
       Error(stReadError, Success)                    { Specific read esror }
     End;
     Inc(Position, W);                                { Adjust position }
     Inc(Ri, W);                                      { Adjust read index }
     Dec(Count, W);                                   { Adjust count left }
   End;
   If (Count <> 0) Then FillChar(P^[Ri], Count, #0);  { Error clear buffer }
END;

{--TEmsStream---------------------------------------------------------------}
{  Write -> Platforms DOS REAL MODE - Updated 14Nov00 LdB                   }
{---------------------------------------------------------------------------}
PROCEDURE TEmsStream.Write (Var Buf; Count: Word);
VAR Success: Integer; W, Wi: Word; P: PByteArray;
BEGIN
   If (Position + Count > MemSize) Then Begin         { Expansion needed }
     If (Position + Count = 0) Then W := 1 Else       { At least one page }
       W := (Position+Count + 16383) DIV 16384;       { Pages needed }
     If (EMS_ResizeMem(W, Handle)=0) Then Begin       { Resize memory }
       PageCount := W;                                { Adjust page count }
       { * REMARK * - Do not shorten this, result can be > 64K }
       MemSize := PageCount;
       MemSize := MemSize * 1024;                     { New memory size }
       { * REMARK END * - Leon de Boer }
     End Else Error(stWriteError, 0);                 { We have an error }
   End;
   If (Handle = 0) Then Error(stWriteError, 403);     { EMS not available }
   P := @Buf;                                         { Transfer address }
   Wi := 0;                                           { Zero write index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := EMS_MoveMem(Position, Handle,
       LongInt(@P^[Wi]), 0, W);                       { Move the memory }
     If (Success <> 0) Then Begin                     { Error was detected }
       W := 0;                                        { Clear bytes moved }
       Error(stWriteError, Success);                  { Specific write error }
     End;
     Inc(Position, W);                                { Adjust position }
     Inc(Wi, W);                                      { Adjust write index }
     Dec(Count, W);                                   { Adjust count left }
     If (Position > StreamSize) Then                  { File expanded }
       StreamSize := Position;                        { Adjust stream size }
   End;
END;

{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TXmsStream OBJECT ANCESTOR                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TXmsStream---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28Feb97 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TXmsStream.Init (MinSize, MaxSize: LongInt);
{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
VAR Success: Integer; MinBlk, MaxBlk: Word;
BEGIN
   Inherited Init;                                    { Call ancestor }
   If (XMS_MemAvail >= MaxSize) Then Begin            { Sufficient memory }
     If (MaxSize = 0) Then MaxBlk := 1 Else           { At least one block }
       MaxBlk := (MaxSize + 1023) DIV 1024;           { Max blocks needed }
     If (MinSize = 0) Then MinBlk := 1 Else           { At least one block }
       MinBlk := (MinSize + 1023) DIV 1024;           { Min blocks needed }
     Handle := XMS_GetMem(MaxBlk);                    { Allocate XMS blocks }
     If (Handle <> 0) Then Begin
       Success := 0;                                  { Preset success }
       BlocksUsed := MaxBlk;                          { Blocks used }
       If (MaxBlk <> MinBlk) Then                     { Sizes differ }
         If (XMS_ResizeMem(MaxBlk, MinBlk, Handle)=0) { Resize to minimum }
           Then BlocksUsed := MinBlk;                 { Hold block size }
       { * REMARK * - Do not shorten this, result can be > 64K }
       MemSize := BlocksUsed;
       MemSize := MemSize * 1024;
       { * REMARK END * - Leon de Boer }
     End Else Success := 303;                         { Failed to allocate }
   End Else Success := 300;                           { Insufficent XMS }
   If (Handle = 0) OR (Success <> 0) Then             { XMS failed }
     Error(stInitError, Success);                     { Call stream error }
END;
{$ELSE}                                               { ALL OTHER OP SYSTEMS }
BEGIN
   Inherited Init(MaxSize, 16384);                    { For compatability }
END;
{$ENDIF}

{$IFDEF PROC_Real}                                    { DOS REAL MODE CODE }
{***************************************************************************}
{                    TXMSStream DOS REAL MODE ONLY METHODS                  }
{***************************************************************************}

{--TXmsStream---------------------------------------------------------------}
{  Done -> Platforms DOS REAL MODE - Updated 28Feb97 LdB                    }
{---------------------------------------------------------------------------}
DESTRUCTOR TXmsStream.Done;
BEGIN
   If (Handle <> 0) Then XMS_FreeMem(Handle);         { Release XMS blocks }
   Inherited Done;                                    { Call ancestor }
END;

{--TXmsStream---------------------------------------------------------------}
{  Truncate -> Platforms DOS REAL MODE - Updated 28Feb97 LdB                }
{---------------------------------------------------------------------------}
PROCEDURE TXmsStream.Truncate;
VAR Success: Integer; W: Word;
BEGIN
   If (Status = stOk) Then Begin                      { Check status okay }
     If (Position = 0) Then W := 1 Else               { At least 1 block }
       W := (Position + 1023) DIV 1024;               { Blocks to use }
     Success := 0;                                    { Preset success }
     If (W <> BlocksUsed) Then                        { Sizes differ }
       If (XMS_ResizeMem(BlocksUsed, W, Handle)=0)    { Resize to this }
       Then Begin
         BlocksUsed := W;                             { Adjust blocks used }
         { * REMARK * - Do not shorten this, result can be > 64K }
         MemSize := BlocksUsed;                       { Blocks used }
         MemSize := MemSize * 1024;                   { Mult by block size }
         { * REMARK END * - Leon de Boer }
       End Else Success := 301;                       { Resize failed }
     If (Success = 0) Then StreamSize := Position     { Adjust size }
       Else Error(stError, Success);                  { Identify error }
   End;
END;

{--TXmsStream---------------------------------------------------------------}
{  Read -> Platforms DOS REAL MODE - Updated 14Nov00 LdB                    }
{---------------------------------------------------------------------------}
PROCEDURE TXmsStream.Read (Var Buf; Count: Word);
VAR Success: Integer; W, Ri: Word; P: PByteArray;
BEGIN
   If (Position + Count > StreamSize) Then            { Insufficient data }
     Error(stReadError, 0);                           { Read beyond end!!! }
   If (Handle = 0) Then Error(stReadError, 303);      { XMS not available }
   P := @Buf;                                         { Transfer address }
   Ri := 0;                                           { Zero read index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := XMS_MoveMem(LongInt(@P^[Ri]), 0,
       Position, Handle, W);                          { Move the data }
     If (Success <> 0) Then Begin                     { Error was detected }
       W := 0;                                        { Clear bytes moved }
       Error(stReadError, Success)                    { Specific read error }
     End;
     Inc(Position, W);                                { Adjust position }
     Inc(Ri, W);                                      { Adjust read index }
     Dec(Count, W);                                   { Adjust count left }
   End;
   If (Count <> 0) Then FillChar(P^[Ri], Count, #0);  { Error clear buffer }
END;

{--TXmsStream---------------------------------------------------------------}
{  Write -> Platforms DOS REAL MODE - Updated 14Nov00 LdB                   }
{---------------------------------------------------------------------------}
PROCEDURE TXmsStream.Write (Var Buf; Count: Sw_Word);
VAR Success: Integer; W, Wi: Word; P: PByteArray;
BEGIN
   { * REMARK * - Because XMS must move even bytes we expand if within  }
   {              one byte of allocated size so we can read/write the   }
   {              last byte with an even access using a dummy end byte. }
   { * REMARK * - Leon de Boer                                          }
   If (Position + Count > (MemSize-1)) Then Begin     { Expansion needed }
     If (Position + Count = 0) Then W := 1 Else Begin { At least one }
       W := (Position + Count + 1023) DIV 1024;       { Blocks needed }
       If ((Position + Count) MOD 1024 = 0) Then
         Inc(W);                                      { Fix for even access }
     End;
     If (XMS_ResizeMem(BlocksUsed, W, Handle)=0)      { Resize memory }
     Then Begin
       BlocksUsed := W;                               { Adjust block count }
       { * REMARK * - Do not shorten this, result can be > 64K }
       MemSize := BlocksUsed;
       MemSize := MemSize * 1024;                     { New memory size }
       { * REMARK END * - Leon de Boer }
     End Else Error(stWriteError, 0);                 { We have an error }
   End;
   If (Handle = 0) Then Error(stWriteError, 303);     { XMS not available }
   P := @Buf;                                         { Transfer address }
   Wi := 0;                                           { Zero write index }
   While (Count > 0) AND (Status = stOk) Do Begin     { Check status & count }
     W := Count;                                      { Transfer read size }
     If (Count > $FFFE) Then W := $FFFE;              { Cant read >64K bytes }
     Success := XMS_MoveMem(Position, Handle,
       LongInt(@P^[Wi]), 0, W);                       { Move the memory }
     If (Success <> 0) Then Begin                     { Error was detected }
       W := 0;                                        { Clear bytes moved }
       Error(stWriteError, Success);                  { Specific write error }
     End;
     Inc(Position, W);                                { Adjust position }
     Inc(Wi, W);                                      { Adjust write index }
     Dec(Count, W);                                   { Adjust count left }
     If (Position > StreamSize) Then                  { File expanded }
       StreamSize := Position;                        { Adjust stream size }
   End;
END;

{$ENDIF}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       TCollection OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TCollection--------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TCollection.Init (ALimit, ADelta: Integer);
BEGIN
   Inherited Init;                                    { Call ancestor }
   Delta := ADelta;                                   { Set increment }
   SetLimit(ALimit);                                  { Set limit }
END;

{--TCollection--------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TCollection.Load (Var S: TStream);
VAR C, I: Integer;
BEGIN
   S.Read(Count, 2);                                  { Read count }
   S.Read(Limit, 2);                                  { Read limit }
   S.Read(Delta, 2);                                  { Read delta }
   Items := Nil;                                      { Clear item pointer }
   C := Count;                                        { Hold count }
   I := Limit;                                        { Hold limit }
   Count := 0;                                        { Clear count }
   Limit := 0;                                        { Clear limit }
   SetLimit(I);                                       { Set requested limit }
   Count := C;                                        { Set count }
   For I := 0 To C-1 Do AtPut(I, GetItem(S));         { Get each item }
END;

{--TCollection--------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TCollection.Done;
BEGIN
   FreeAll;                                           { Free all items }
   SetLimit(0);                                       { Release all memory }
END;

{--TCollection--------------------------------------------------------------}
{  At -> Platforms DOS/DPMI/WIN/NT/OS2 -Updated 22May96 LdB                 }
{---------------------------------------------------------------------------}
FUNCTION TCollection.At (Index: Integer): Pointer;
BEGIN
   If (Index < 0) OR (Index >= Count) Then Begin      { Invalid index }
     Error(coIndexError, Index);                      { Call error }
     At := Nil;                                       { Return nil }
   End Else At := Items^[Index];                      { Return item }
END;

{--TCollection--------------------------------------------------------------}
{  IndexOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TCollection.IndexOf (Item: Pointer): Integer;
VAR I: Integer;
BEGIN
   If (Count > 0) Then Begin                          { Count is positive }
     For I := 0 To Count-1 Do                         { For each item }
       If (Items^[I] = Item) Then Begin               { Look for match }
         IndexOf := I;                                { Return index }
         Exit;                                        { Now exit }
       End;
   End;
   IndexOf := -1;                                     { Return index }
END;

{--TCollection--------------------------------------------------------------}
{  GetItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.Get;                                  { Item off stream }
END;

{--TCollection--------------------------------------------------------------}
{  LastThat -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TCollection.LastThat (Test: Pointer): Pointer;
VAR I: Integer;
BEGIN
   For I := Count DownTo 1 Do Begin                   { Down from last item }
     {$IFDEF PPC_VIRTUAL}                             { VIRTUAL COMPILER }
     If CallTestLocal(Test, Items^[I-1])              { Test each item }
     {$ELSE}                                          { OTHER COMPILERS }
     If CallTestLocal(Test, PrevFramePtr, Items^[I-1]){ Test each item }
     {$ENDIF}
     Then Begin                                       { Test each item }
       LastThat := Items^[I-1];                       { Return successful }
       Exit;                                          { Now exit }
     End;
    End;
   LastThat := Nil;                                   { None passed test }
END;

{--TCollection--------------------------------------------------------------}
{  FirstThat -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB         }
{---------------------------------------------------------------------------}
FUNCTION TCollection.FirstThat (Test: Pointer): Pointer;
VAR I: Integer;
BEGIN
   For I := 1 To Count Do Begin                       { Up from first item }
     {$IFDEF PPC_VIRTUAL}                             { VIRTUAL COMPILER }
     If CallTestLocal(Test, Items^[I-1])              { Test each item }
     {$ELSE}                                          { OTHER COMPILERS }
     If CallTestLocal(Test, PrevFramePtr, Items^[I-1]){ Test each item }
     {$ENDIF}
     Then Begin                                       { Test each item }
       FirstThat := Items^[I-1];                      { Return successful }
       Exit;                                          { Now exit }
     End;
   End;
   FirstThat := Nil;                                  { None passed test }
END;

{--TCollection--------------------------------------------------------------}
{  Pack -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Pack;
VAR I, J: Integer;
BEGIN
   I := 0;                                            { Initialize dest }
   J := 0;                                            { Intialize test }
   While (I < Count) AND (J < Limit) Do Begin         { Check fully packed }
     If (Items^[J] <> Nil) Then Begin                 { Found a valid item }
       If (I <> J) Then Begin
         Items^[I] := Items^[J];                      { Transfer item }
         Items^[J] := Nil;                            { Now clear old item }
       End;
       Inc(I);                                        { One item packed }
     End;
     Inc(J);                                          { Next item to test }
   End;
   If (I < Count) Then Count := I;                    { New packed count }
END;

{--TCollection--------------------------------------------------------------}
{  FreeAll -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated!22May96 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.FreeAll;
VAR I: Integer;
BEGIN
   For I := 0 To Count-1 Do FreeItem(At(I));          { Release each item }
   Count := 0;                                        { Clear item count }
END;

{--TCollection--------------------------------------------------------------}
{  DeleteAll -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.DeleteAll;
BEGIN
   Count := 0;                                        { Clear item count }
END;

{--TCollection--------------------------------------------------------------}
{  Free -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Free (Item: Pointer);
BEGIN
   Delete(Item);                                      { Delete from list }
   FreeItem(Item);                                    { Free the item }
END;

{--TCollection--------------------------------------------------------------}
{  Insert -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { Insert item }
END;

{--TCollection--------------------------------------------------------------}
{  Delete -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Delete (Item: Pointer);
BEGIN
   AtDelete(IndexOf(Item));                           { Delete from list }
END;

{--TCollection--------------------------------------------------------------}
{  AtFree -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.AtFree (Index: Integer);
VAR Item: Pointer;
BEGIN
   Item := At(Index);                                 { Retreive item ptr }
   AtDelete(Index);                                   { Delete item }
   FreeItem(Item);                                    { Free the item }
END;

{--TCollection--------------------------------------------------------------}
{  FreeItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.FreeItem (Item: Pointer);
VAR P: PObject;
BEGIN
   P := PObject(Item);                                { Convert pointer }
   If (P <> Nil) Then Dispose(P, Done);               { Dispose of object }
END;

{--TCollection--------------------------------------------------------------}
{  AtDelete -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.AtDelete (Index: Integer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then Begin     { Valid index }
     Dec(Count);                                      { One less item }
     If (Count > Index) Then Move(Items^[Index+1],
      Items^[Index], (Count-Index)*Sizeof(Pointer));  { Shuffle items down }
   End Else Error(coIndexError, Index);               { Index error }
END;

{--TCollection--------------------------------------------------------------}
{  ForEach -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08Jul99 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.ForEach (Action: Pointer);
VAR I: Integer;
BEGIN
   For I := 1 To Count Do                             { Up from first item }
    {$IFDEF PPC_VIRTUAL}                              { VIRTUAL COMPILER }
    CallTestLocal(Action, Items^[I-1]);               { Call with each item }
    {$ELSE}                                           { OTHER COMPILERS }
    CallTestLocal(Action, PrevFramePtr, Items^[I-1]); { Call with each item }
    {$ENDIF}
END;

{--TCollection--------------------------------------------------------------}
{  SetLimit -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.SetLimit (ALimit: Integer);
VAR AItems: PItemList;
BEGIN
   If (ALimit < Count) Then ALimit := Count;          { Stop underflow }
   If (ALimit > MaxCollectionSize) Then
     ALimit := MaxCollectionSize;                     { Stop overflow }
   {$IFNDEF PPC_SPEED}                                { NON SPEED COMPILERS }
   If (MaxAvail < (ALimit*SizeOf(Pointer))) Then      { Check enough memory }
     ALimit := Limit;                                 { Insufficient memory }
   {$ENDIF}
   If (ALimit <> Limit) Then Begin                    { Limits differ }
     If (ALimit = 0) Then AItems := Nil Else Begin    { Alimit=0 nil entry }
       GetMem(AItems, ALimit * SizeOf(Pointer));      { Allocate memory }
       If (AItems <> Nil) Then FillChar(AItems^,
         ALimit * SizeOf(Pointer), #0);               { Clear the memory }
     End;
     If (AItems <> Nil) OR (ALimit = 0) Then Begin    { Check success }
       If (AItems <> Nil) AND (Items <> Nil) Then     { Check both valid }
         Move(Items^, AItems^, Count*SizeOf(Pointer));{ Move existing items }
       If (Limit <> 0) AND (Items <> Nil) Then        { Check old allocation }
         FreeMem(Items, Limit * SizeOf(Pointer));     { Release memory }
       Items := AItems;                               { Update items }
       Limit := ALimit;                               { Set limits }
     End;
   End;
END;

{--TCollection--------------------------------------------------------------}
{  Error -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Error (Code, Info: Integer);
BEGIN
   RunError(212 - Code);                              { Run error }
END;

{--TCollection--------------------------------------------------------------}
{  AtPut -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.AtPut (Index: Integer; Item: Pointer);
BEGIN
   If (Index >= 0) AND (Index < Count) Then           { Index valid }
     Items^[Index] := Item                            { Put item in index }
     Else Error(coIndexError, Index);                 { Index error }
END;

{--TCollection--------------------------------------------------------------}
{  AtInsert -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 28May99 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.AtInsert (Index: Integer; Item: Pointer);
VAR I: Integer;
BEGIN
   If (Index >= 0) AND (Index <= Count) Then Begin    { Valid index }
     If (Count=Limit) Then SetLimit(Limit+Delta);     { Expand size if able }
     If (Limit>Count) Then Begin
       If (Index < Count) Then Begin                  { Not last item }
         For I := Count-1 DownTo Index Do             { Start from back }
           Items^[I+1] := Items^[I];                  { Move each item }
       End;
       Items^[Index] := Item;                         { Put item in list }
       Inc(Count);                                    { Inc count }
     End Else Error(coOverflow, Index);               { Expand failed }
   End Else Error(coIndexError, Index);               { Index error }
END;

{--TCollection--------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.Store (Var S: TStream);

   PROCEDURE DoPutItem (P: Pointer); {$IFNDEF FPC} FAR;{$ENDIF}
   BEGIN
     PutItem(S, P);                                   { Put item on stream }
   END;

BEGIN
   S.Write(Count, 2);                                 { Write count }
   S.Write(Limit, 2);                                 { Write limit }
   S.Write(Delta, 2);                                 { Write delta }
   ForEach(@DoPutItem);                               { Each item to stream }
END;

{--TCollection--------------------------------------------------------------}
{  PutItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.Put(Item);                                       { Put item on stream }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       TSortedCollection OBJECT METHODS                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TSortedCollection--------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TSortedCollection.Init (ALimit, ADelta: Integer);
BEGIN
   Inherited Init(ALimit, ADelta);                    { Call ancestor }
   Duplicates := False;                               { Clear flag }
END;

{--TSortedCollection--------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TSortedCollection.Load (Var S: TStream);
BEGIN
   Inherited Load(S);                                 { Call ancestor }
   S.Read(Duplicates, 1);                             { Read duplicate flag }
END;

{--TSortedCollection--------------------------------------------------------}
{  KeyOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TSortedCollection.KeyOf (Item: Pointer): Pointer;
BEGIN
   KeyOf := Item;                                     { Return item as key }
END;

{--TSortedCollection--------------------------------------------------------}
{  IndexOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TSortedCollection.IndexOf (Item: Pointer): Integer;
VAR I, J: Integer;
BEGIN
   J := -1;                                           { Preset result }
   If Search(KeyOf(Item), I) Then Begin               { Search for item }
     If Duplicates Then                               { Duplicates allowed }
       While (I < Count) AND (Item <> Items^[I]) Do
         Inc(I);                                      { Count duplicates }
     If (I < Count) Then J := I;                      { Index result }
   End;
   IndexOf := J;                                      { Return result }
END;

{--TSortedCollection--------------------------------------------------------}
{  Compare -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TSortedCollection.Compare (Key1, Key2: Pointer): Integer;
BEGIN
   Abstract;                                          { Abstract method }
END;

{--TSortedCollection--------------------------------------------------------}
{  Search -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TSortedCollection.Search (Key: Pointer; Var Index: Integer): Boolean;
VAR L, H, I, C: Integer;
BEGIN
   Search := False;                                   { Preset failure }
   L := 0;                                            { Start count }
   H := Count - 1;                                    { End count }
   While (L <= H) Do Begin
     I := (L + H) SHR 1;                              { Mid point }
     C := Compare(KeyOf(Items^[I]), Key);             { Compare with key }
     If (C < 0) Then L := I + 1 Else Begin            { Item to left }
       H := I - 1;                                    { Item to right }
       If C = 0 Then Begin                            { Item match found }
         Search := True;                              { Result true }
         If NOT Duplicates Then L := I;               { Force kick out }
       End;
     End;
   End;
   Index := L;                                        { Return result }
END;

{--TSortedCollection--------------------------------------------------------}
{  Insert -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TSortedCollection.Insert (Item: Pointer);
VAR I: Integer;
BEGIN
   If NOT Search(KeyOf(Item), I) OR Duplicates Then   { Item valid }
     AtInsert(I, Item);                               { Insert the item }
END;

{--TSortedCollection--------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TSortedCollection.Store (Var S: TStream);
BEGIN
   TCollection.Store(S);                              { Call ancestor }
   S.Write(Duplicates, 1);                            { Write duplicate flag }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                     TStringCollection OBJECT METHODS                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStringCollection--------------------------------------------------------}
{  GetItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStringCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.ReadStr;                              { Get new item }
END;

{--TStringCollection--------------------------------------------------------}
{  Compare -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 21Aug97 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStringCollection.Compare (Key1, Key2: Pointer): Integer;
VAR I, J: Integer; P1, P2: PString;
BEGIN
   P1 := PString(Key1);                               { String 1 pointer }
   P2 := PString(Key2);                               { String 2 pointer }
   If (Length(P1^)<Length(P2^)) Then J := Length(P1^)
     Else J := Length(P2^);                           { Shortest length }
   I := 1;                                            { First character }
   While (I<J) AND (P1^[I]=P2^[I]) Do Inc(I);         { Scan till fail }
   If (I=J) Then Begin                                { Possible match }
   { * REMARK * - Bug fix   21 August 1997 }
     If (P1^[I]<P2^[I]) Then Compare := -1 Else       { String1 < String2 }
       If (P1^[I]>P2^[I]) Then Compare := 1 Else      { String1 > String2 }
       If (Length(P1^)>Length(P2^)) Then Compare := 1 { String1 > String2 }
         Else If (Length(P1^)<Length(P2^)) Then       { String1 < String2 }
           Compare := -1 Else Compare := 0;           { String1 = String2 }
   { * REMARK END * - Leon de Boer }
   End Else If (P1^[I]<P2^[I]) Then Compare := -1     { String1 < String2 }
     Else Compare := 1;                               { String1 > String2 }
END;

{--TStringCollection--------------------------------------------------------}
{  FreeItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStringCollection.FreeItem (Item: Pointer);
BEGIN
   DisposeStr(Item);                                  { Dispose item }
END;

{--TStringCollection--------------------------------------------------------}
{  PutItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 22May96 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TStringCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.WriteStr(Item);                                  { Write string }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       TStrCollection OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStrCollection-----------------------------------------------------------}
{  Compare -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStrCollection.Compare (Key1, Key2: Pointer): Integer;
VAR I, J: Integer; P1, P2: PByteArray;
BEGIN
   P1 := PByteArray(Key1);                            { PChar 1 pointer }
   P2 := PByteArray(Key2);                            { PChar 2 pointer }
   I := 0;                                            { Preset no size }
   If (P1 <> Nil) Then While (P1^[I] <> 0) Do Inc(I); { PChar 1 length }
   J := 0;                                            { Preset no size }
   If (P2 <> Nil) Then While (P2^[J] <> 0) Do Inc(J); { PChar 2 length }
   If (I < J) Then J := I;                            { Shortest length }
   I := 0;                                            { First character }
   While (I < J) AND (P1^[I] = P2^[I]) Do Inc(I);     { Scan till fail }
   If (P1^[I] = P2^[I]) Then Compare := 0 Else        { Strings matched }
     If (P1^[I] < P2^[I]) Then Compare := -1 Else     { String1 < String2 }
        Compare := 1;                                 { String1 > String2 }
END;

{--TStrCollection-----------------------------------------------------------}
{  GetItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TStrCollection.GetItem (Var S: TStream): Pointer;
BEGIN
   GetItem := S.StrRead;                              { Get string item }
END;

{--TStrCollection-----------------------------------------------------------}
{  FreeItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TStrCollection.FreeItem (Item: Pointer);
VAR I: Integer; P: PByteArray;
BEGIN
   If (Item <> Nil) Then Begin                        { Item is valid }
     P := PByteArray(Item);                           { Create byte pointer }
     I := 0;                                          { Preset no size }
     While (P^[I] <> 0) Do Inc(I);                    { Find PChar end }
     FreeMem(Item, I+1);                              { Release memory }
   End;
END;

{--TStrCollection-----------------------------------------------------------}
{  PutItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May96 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TStrCollection.PutItem (Var S: TStream; Item: Pointer);
BEGIN
   S.StrWrite(Item);                                  { Write the string }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                   TUnSortedStrCollection OBJECT METHODS                   }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TUnSortedCollection------------------------------------------------------}
{  Insert -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 23May96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TUnSortedStrCollection.Insert (Item: Pointer);
BEGIN
   AtInsert(Count, Item);                             { Insert - NO sorting }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           TResourceItem RECORD                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
TYPE
   TResourceItem = PACKED RECORD
      Posn: LongInt;                                  { Resource position }
      Size: LongInt;                                  { Resource size }
      Key : String;                                   { Resource key }
   End;
   PResourceItem = ^TResourceItem;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    TResourceCollection OBJECT METHODS                     }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TResourceCollection------------------------------------------------------}
{  KeyOf -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24May96 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TResourceCollection.KeyOf (Item: Pointer): Pointer;
BEGIN
   KeyOf := @PResourceItem(Item)^.Key;                { Pointer to key }
END;

{--TResourceCollection------------------------------------------------------}
{  GetItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24May96 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TResourceCollection.GetItem (Var S: TStream): Pointer;
VAR B: Byte; Pos, Size: LongInt; P: PResourceItem; Ts: String;
BEGIN
   S.Read(Pos, 4);                                    { Read position }
   S.Read(Size, 4);                                   { Read size }
   S.Read(B, 1);                                      { Read key length }
   If (MaxAvail > (SizeOf(TResourceItem)-SizeOf(Ts)))
   Then Begin
     GetMem(P, B + (SizeOf(TResourceItem) -
       SizeOf(Ts) + 1));                              { Allocate min memory }
     P^.Posn := Pos;                                  { Xfer position }
     P^.Size := Size;                                 { Xfer size }
     {$IFDEF PPC_DELPHI3}                             { DELPHI 3+ COMPILER }
     SetLength(P^.Key, B);                            { Xfer string length }
     {$ELSE}                                          { OTHER COMPILERS }
     P^.Key[0] := Chr(B);                             { Xfer string length }
     {$ENDIF}
     S.Read(P^.Key[1], B);                            { Xfer string data }
   End Else P := Nil;                                 { Insufficient memory }
   GetItem := P;                                      { Return pointer }
END;

{--TResourceCollection------------------------------------------------------}
{  FreeItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24May96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE TResourceCollection.FreeItem (Item: Pointer);
VAR S: String;
BEGIN
   If (Item <> Nil) Then FreeMem(Item,
     SizeOf(TResourceItem) - SizeOf(S) +
     Length(PResourceItem(Item)^.Key) + 1);           { Release memory }
END;

{--TResourceCollection------------------------------------------------------}
{  PutItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 24May96 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TResourceCollection.PutItem (Var S: TStream; Item: Pointer);
VAR Ts: String;
BEGIN
   If (Item <> Nil) Then S.Write(PResourceItem(Item)^,
    SizeOf(TResourceItem) - SizeOf(Ts) +
    Length(PResourceItem(Item)^.Key) + 1);            { Write to stream }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                  PRIVATE RESOURCE MANAGER CONSTANTS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
CONST
   RStreamMagic: LongInt = $52504246;                 { 'FBPR' }
   RStreamBackLink: LongInt = $4C424246;              { 'FBBL' }

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    PRIVATE RESOURCE MANAGER TYPES                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
TYPE
{$IFDEF NewExeFormat}                                 { New EXE format }
   TExeHeader = PACKED RECORD
     eHdrSize:   Word;
     eMinAbove:  Word;
     eMaxAbove:  Word;
     eInitSS:    Word;
     eInitSP:    Word;
     eCheckSum:  Word;
     eInitPC:    Word;
     eInitCS:    Word;
     eRelocOfs:  Word;
     eOvlyNum:   Word;
     eRelocTab:  Word;
     eSpace:     Array[1..30] of Byte;
     eNewHeader: Word;
   END;
{$ENDIF}

   THeader = PACKED RECORD
     Signature: Word;
     Case Integer Of
       0: (
         LastCount: Word;
         PageCount: Word;
         ReloCount: Word);
       1: (
         InfoType: Word;
         InfoSize: Longint);
   End;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                       TResourceFile OBJECT METHODS                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TResourceFile------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TResourceFile.Init(AStream: PStream);
VAR Found, Stop: Boolean; Header: THeader;
    {$IFDEF NewExeFormat} ExeHeader: TExeHeader; {$ENDIF}
BEGIN
   TObject.Init;                                      { Initialize object }
   Found := False;                                    { Preset false }
   If (AStream <> Nil) Then Begin
     Stream := AStream;                               { Hold stream }
     BasePos := Stream^.GetPos;                       { Get position }
     Repeat
       Stop := True;                                  { Preset stop }
       If (BasePos <= Stream^.GetSize-SizeOf(THeader))
       Then Begin                                     { Valid file header }
         Stream^.Seek(BasePos);                       { Seek to position }
         Stream^.Read(Header, SizeOf(THeader));       { Read header }
         Case Header.Signature Of
         {$IFDEF NewExeFormat}                        { New format file }
           $5A4D: Begin
             Stream^.Read(ExeHeader, SizeOf(TExeHeader));
             BasePos := ExeHeader.eNewHeader;         { Hold position }
             Stop := False;                           { Clear stop flag }
           End;
           $454E: Begin
             BasePos := Stream^.GetSize - 8;          { Hold position }
             Stop := False;                           { Clear stop flag }
           End;
           $4246: Begin
             Stop := False;                           { Clear stop flag }
             Case Header.Infotype Of
               $5250: Begin                           { Found Resource }
                   Found := True;                     { Found flag is true }
                   Stop := True;                      { Set stop flag }
                 End;
               $4C42: Dec(BasePos, Header.InfoSize-8);{ Found BackLink }
               $4648: Dec(BasePos, SizeOf(THeader)*2);{ Found HelpFile }
               Else Stop := True;                     { Set stop flag }
             End;
           End;
           $424E: If Header.InfoType = $3230          { Found Debug Info }
           Then Begin
             Dec(BasePos, Header.InfoSize);           { Adjust position }
             Stop := False;                           { Clear stop flag }
           End;
         {$ELSE}                                      { Old EXE format }
           $5A4D: Begin
             Inc(BasePos, LongInt(Header.PageCount)*512
               - (-Header.LastCount AND 511));        { Calc position }
             Stop := False;                           { Clear stop flag }
           End;
           $4246: If Header.InfoType = $5250 Then     { Header was found }
             Found := True Else Begin
               Inc(BasePos, Header.InfoSize + 8);     { Adjust position }
               Stop := False;                         { Clear stop flag }
             End;
         {$ENDIF}
         End;
       End;
     Until Stop;                                      { Until flag is set }
   End;
   If Found Then Begin                                { Resource was found }
     Stream^.Seek(BasePos + SizeOf(LongInt) * 2);     { Seek to position }
     Stream^.Read(IndexPos, SizeOf(LongInt));         { Read index position }
     Stream^.Seek(BasePos + IndexPos);                { Seek to resource }
     Index.Load(Stream^);                             { Load resource }
   End Else Begin
     IndexPos := SizeOf(LongInt) * 3;                 { Set index position }
     Index.Init(0, 8);                                { Set index }
   End;
END;


{--TResourceFile------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TResourceFile.Done;
BEGIN
   Flush;                                             { Flush the file }
   Index.Done;                                        { Dispose of index }
   If (Stream <> Nil) Then Dispose(Stream, Done);     { Dispose of stream }
END;

{--TResourceFile------------------------------------------------------------}
{  Count -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TResourceFile.Count: Integer;
BEGIN
   Count := Index.Count;                              { Return index count }
END;

{--TResourceFile------------------------------------------------------------}
{  KeyAt -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB             }
{---------------------------------------------------------------------------}
FUNCTION TResourceFile.KeyAt (I: Integer): String;
BEGIN
   KeyAt := PResourceItem(Index.At(I))^.Key;          { Return key }
END;

{--TResourceFile------------------------------------------------------------}
{  Get -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TResourceFile.Get (Key: String): PObject;
VAR I: Integer;
BEGIN
   If (Stream = Nil) OR (NOT Index.Search(@Key, I))   { No match on key }
   Then Get := Nil Else Begin
     Stream^.Seek(BasePos +
       PResourceItem(Index.At(I))^.Posn);             { Seek to position }
     Get := Stream^.Get;                              { Get item }
   End;
END;

{--TResourceFile------------------------------------------------------------}
{  SwitchTo -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TResourceFile.SwitchTo (AStream: PStream; Pack: Boolean): PStream;
VAR NewBasePos: LongInt;

   PROCEDURE DoCopyResource (Item: PResourceItem); {$IFNDEF FPC} FAR; {$ENDIF}
   BEGIN
     Stream^.Seek(BasePos + Item^.Posn);              { Move stream position }
     Item^.Posn := AStream^.GetPos - NewBasePos;      { Hold new position }
     AStream^.CopyFrom(Stream^, Item^.Size);          { Copy the item }
   END;

BEGIN
   SwitchTo := Stream;                                { Preset return }
   If (AStream <> Nil) AND (Stream <> Nil) Then Begin { Both streams valid }
     NewBasePos := AStream^.GetPos;                   { Get position }
     If Pack Then Begin
       AStream^.Seek(NewBasePos + SizeOf(LongInt)*3); { Seek to position }
       Index.ForEach(@DoCopyResource);                { Copy each resource }
       IndexPos := AStream^.GetPos - NewBasePos;      { Hold index position }
     End Else Begin
       Stream^.Seek(BasePos);                         { Seek to position }
       AStream^.CopyFrom(Stream^, IndexPos);          { Copy the resource }
     End;
     Stream := AStream;                               { Hold new stream }
     BasePos := NewBasePos;                           { New base position }
     Modified := True;                                { Set modified flag }
   End;
END;

{--TResourceFile------------------------------------------------------------}
{  Flush -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TResourceFile.Flush;
VAR ResSize: LongInt; LinkSize: LongInt;
BEGIN
   If (Modified) AND (Stream <> Nil) Then Begin       { We have modification }
     Stream^.Seek(BasePos + IndexPos);                { Seek to position }
     Index.Store(Stream^);                            { Store the item }
     ResSize := Stream^.GetPos - BasePos;             { Hold position }
     LinkSize := ResSize + SizeOf(LongInt) * 2;       { Hold link size }
     Stream^.Write(RStreamBackLink, SizeOf(LongInt)); { Write link back }
     Stream^.Write(LinkSize, SizeOf(LongInt));        { Write link size }
     Stream^.Seek(BasePos);                           { Move stream position }
     Stream^.Write(RStreamMagic, SizeOf(LongInt));    { Write number }
     Stream^.Write(ResSize, SizeOf(LongInt));         { Write record size }
     Stream^.Write(IndexPos, SizeOf(LongInt));        { Write index position }
     Stream^.Flush;                                   { Flush the stream }
   End;
   Modified := False;                                 { Clear modified flag }
END;

{--TResourceFile------------------------------------------------------------}
{  Delete -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TResourceFile.Delete (Key: String);
VAR I: Integer;
BEGIN
   If Index.Search(@Key, I) Then Begin                { Search for key }
     Index.Free(Index.At(I));                         { Delete from index }
     Modified := True;                                { Set modified flag }
   End;
END;

{--TResourceFile------------------------------------------------------------}
{  Put -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 18Jun96 LdB               }
{---------------------------------------------------------------------------}
PROCEDURE TResourceFile.Put (Item: PObject; Key: String);
VAR I: Integer; P: PResourceItem;
BEGIN
   If (Stream = Nil) Then Exit;                       { Stream not valid }
   If Index.Search(@Key, I) Then P := Index.At(I)     { Search for item }
   Else Begin
     If (MaxAvail > SizeOf(TResourceItem)-SizeOf(Key)){ Check free memory }
     Then Begin
       GetMem(P, Length(Key) + (SizeOf(TResourceItem)
         - SizeOf(Key) + 1));                         { Allocate memory }
       P^.Key := Key;                                 { Store key }
       Index.AtInsert(I, P);                          { Insert item }
     End Else P := Nil;                               { Insufficient memory }
   End;
   If (P <> Nil) Then Begin                           { Allocate worked }
     P^.Posn := IndexPos;                             { Set index position }
     Stream^.Seek(BasePos + IndexPos);                { Seek file position }
     Stream^.Put(Item);                               { Put item on stream }
     IndexPos := Stream^.GetPos - BasePos;            { Hold index position }
     P^.Size := IndexPos - P^.Posn;                   { Calc size }
     Modified := True;                                { Set modified flag }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TStringList OBJECT METHODS                       }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStringList--------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStringList.Load (Var S: TStream);
VAR Size: Word;
BEGIN
   Stream := @S;                                      { Hold stream pointer }
   S.Read(Size, SizeOf(Word));                        { Read size }
   BasePos := S.GetPos;                               { Hold position }
   S.Seek(BasePos + Size);                            { Seek to position }
   S.Read(IndexSize, SizeOf(Integer));                { Read index size }
   If (MaxAvail >= IndexSize * SizeOf(TStrIndexRec))  { Check free memory }
   Then Begin
     GetMem(Index, IndexSize * SizeOf(TStrIndexRec)); { Allocate memory }
     S.Read(Index^, IndexSize * SizeOf(TStrIndexRec));{ Read indexes }
   End Else IndexSize := 0;                           { Insufficient memory }
END;

{--TStringList--------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TStringList.Done;
BEGIN
   FreeMem(Index, IndexSize * SizeOf(TStrIndexRec));  { Release memory }
END;

{--TStringList--------------------------------------------------------------}
{  Get -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB               }
{---------------------------------------------------------------------------}
FUNCTION TStringList.Get (Key: Word): String;
VAR I: Word; S: String;
BEGIN
   S := '';                                           { Preset empty string }
   If (IndexSize > 0) Then Begin                      { We must have strings }
     I := 0;                                          { First entry }
     While (I < IndexSize) AND (S = '') Do Begin
       If ((Key - Index^[I].Key) < Index^[I].Count)   { Diff less than count }
         Then ReadStr(S, Index^[I].Offset,
           Key-Index^[I].Key);                        { Read the string }
       Inc(I);                                        { Next entry }
     End;
   End;
   Get := S;                                          { Return empty string }
END;

{***************************************************************************}
{                       TStringList PRIVATE METHODS                         }
{***************************************************************************}

{--TStringList--------------------------------------------------------------}
{  ReadStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB           }
{---------------------------------------------------------------------------}
PROCEDURE TStringList.ReadStr (Var S: String; Offset, Skip: Word);
VAR B: Byte;
BEGIN
   Stream^.Seek(BasePos + Offset);                    { Seek to position }
   Inc(Skip);                                         { Adjust skip }
   Repeat
     Stream^.Read(B, 1);                              { Read string size }
     {$IFDEF PPC_DELPHI3}                             { DELPHI 3+ COMPILER }
     SetLength(S, B);                                 { Xfer string length }
     {$ELSE}                                          { OTHER COMPILERS }
     S[0] := Chr(B);                                  { Xfer string size }
     {$ENDIF}
     Stream^.Read(S[1], B);                           { Read string data }
     Dec(Skip);                                       { One string read }
   Until (Skip = 0);
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                         TStrListMaker OBJECT METHODS                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStrListMaker------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStrListMaker.Init (AStrSize, AIndexSize: Word);
BEGIN
   Inherited Init;                                    { Call ancestor }
   StrSize := AStrSize;                               { Hold size }
   If (MaxAvail >= AStrSize) Then
     GetMem(Strings, AStrSize);                       { Allocate memory }
   If (MaxAvail >= AIndexSize * SizeOf(TStrIndexRec)) { Check free memory }
   Then Begin
     IndexSize := AIndexSize;                         { Hold index size }
     GetMem(Index, AIndexSize * SizeOf(TStrIndexRec));{ Allocate memory }
   End;
END;

{--TStrListMaker------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TStrListMaker.Done;
BEGIN
   FreeMem(Index, IndexSize * SizeOf(TStrIndexRec));  { Free index memory }
   FreeMem(Strings, StrSize);                         { Free data memory }
END;

{--TStrListMaker------------------------------------------------------------}
{  Put -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB               }
{---------------------------------------------------------------------------}
PROCEDURE TStrListMaker.Put (Key: Word; S: String);
BEGIN
   If (Cur.Count = 16) OR (Key <> Cur.Key + Cur.Count)
     Then CloseCurrent;                               { Close current }
   If (Cur.Count = 0) Then Begin
     Cur.Key := Key;                                  { Set key }
     Cur.Offset := StrPos;                            { Set offset }
   End;
   Inc(Cur.Count);                                    { Inc count }
   Move(S, Strings^[StrPos], Length(S) + 1);          { Move string data }
   Inc(StrPos, Length(S) + 1);                        { Adjust position }
END;

{--TStrListMaker------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStrListMaker.Store (Var S: TStream);
BEGIN
   CloseCurrent;                                      { Close all current }
   S.Write(StrPos, SizeOf(Word));                     { Write position }
   S.Write(Strings^, StrPos);                         { Write string data }
   S.Write(IndexPos, SizeOf(Word));                   { Write index position }
   S.Write(Index^, IndexPos * SizeOf(TStrIndexRec));  { Write indexes }
END;

{***************************************************************************}
{                      TStrListMaker PRIVATE METHODS                        }
{***************************************************************************}

{--TStrListMaker------------------------------------------------------------}
{  CloseCurrent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Jun97 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TStrListMaker.CloseCurrent;
BEGIN
   If (Cur.Count <> 0) Then Begin
     Index^[IndexPos] := Cur;                         { Hold index position }
     Inc(IndexPos);                                   { Next index }
     Cur.Count := 0;                                  { Adjust count }
   End;
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        STREAM INTERFACE ROUTINES                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  Abstract -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Jun96 LdB          }
{---------------------------------------------------------------------------}
PROCEDURE Abstract;
BEGIN
   RunError(211);                                     { Abstract error }
END;

{---------------------------------------------------------------------------}
{  RegisterObjects -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 02Sep97 LdB   }
{---------------------------------------------------------------------------}
PROCEDURE RegisterObjects;
BEGIN
   RegisterType(RCollection);                         { Register object }
   RegisterType(RStringCollection);                   { Register object }
   RegisterType(RStrCollection);                      { Register object }
END;

{---------------------------------------------------------------------------}
{  RegisterType -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 02Sep97 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE RegisterType (Var S: TStreamRec);
VAR P: PStreamRec;
BEGIN
   P := StreamTypes;                                  { Current reg list }
   While (P <> Nil) AND (P^.ObjType <> S.ObjType)
     Do P := P^.Next;                                 { Find end of chain }
   If (P = Nil) AND (S.ObjType <> 0) Then Begin       { Valid end found }
     S.Next := StreamTypes;                           { Chain the list }
     StreamTypes := @S;                               { We are now first }
   End Else RegisterError;                            { Register the error }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    GENERAL FUNCTION INTERFACE ROUTINES                    }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  LongMul -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Feb98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION LongMul (X, Y: Integer): LongInt;
BEGIN
    LongMul := LongInt(X*Y);                          { Multiply integers }
END;

{---------------------------------------------------------------------------}
{  LongDiv -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 10Feb98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION LongDiv (X: LongInt; Y: Integer): Integer;
BEGIN
   LongDiv := Integer(X DIV Y);                       { Divid longint }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                    DYNAMIC STRING INTERFACE ROUTINES                      }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Jun96 LdB            }
{---------------------------------------------------------------------------}
FUNCTION NewStr (S: String): PString;
VAR P: PString;
BEGIN
   If (S = '') Then P := Nil Else Begin               { Empty returns nil }
     If (MaxAvail > Length(S)) Then Begin             { Check free memory }
       GetMem(P, Length(S) + 1);                      { Allocate memory }
       If (P <> Nil) Then P^ := S;                    { Transfer string }
     End Else P := Nil;                               { Insufficient memory }
   End;
   NewStr := P;                                       { Return result }
END;

{---------------------------------------------------------------------------}
{  DisposeStr -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 12Jun96 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE DisposeStr (P: PString);
BEGIN
   If (P <> Nil) Then FreeMem(P, Length(P^) + 1);     { Release memory }
END;

END.

{
 $Log$
 Revision 1.4  2001-04-10 21:57:55  pierre
  + first adds for Use_API define

 Revision 1.3  2001/04/10 21:29:55  pierre
  * import of Leon de Boer's files

 Revision 1.2  2000/08/24 12:00:22  marco
  * CVS log and ID tags


}