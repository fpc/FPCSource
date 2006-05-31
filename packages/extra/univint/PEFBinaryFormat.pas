{
     File:       PEFBinaryFormat.p
 
     Contains:   PEF Types and Macros
 
     Version:    Technology: Master Interfaces
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit PEFBinaryFormat;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes;


{$ALIGN MAC68K}


{ -------------------------------------------------------------------------------------------- }
{ Almost all types are padded for natural alignment.  However the PEFExportedSymbol type is    }
{ 10 bytes long, containing two 32 bit fields and one 16 bit field.  Arrays of it must be      }
{ packed, so it requires "68K" alignment.  Setting this globally to 68K should also help       }
{ ensure consistent treatment across compilers.                                                }

{ ======================================================================================== }
{ Overall Structure }
{ ================= }

{ -------------------------------------------------------------------------------------------- }
{ This header contains a complete set of types and macros for dealing with the PEF executable  }
{ format.  While some description is provided, this header is not meant as a primary source    }
{ of documentation on PEF.  An excellent specification of PEF can be found in the Macintosh    }
{ Runtime Architectures book.  This header is primarily a physical format description.  Thus   }
{ it depends on as few other headers as possible and structure fields have obvious sizes.      }
{                                                                                              }
{ The physical storage for a PEF executable is known as a "container".  This refers to just    }
{ the executable itself, not the file etc.  E.g. if five DLLs are packaged in a single file's  }
{ data fork, that one data fork has five containers within it.                                 }
{                                                                                              }
{ A PEF container consists of an overall header, followed by one or more section headers,      }
{ followed by the section name table, followed by the contents for the sections.  Some kinds   }
{ of sections have specific internal representation.  The "loader" section is the most common  }
{ of these special sections.  It contains information on the exports, imports, and runtime     }
{ relocations required to prepare the executable.  PEF containers are self contained, all      }
{ portions are located via relative offsets.                                                   }
{                                                                                              }
{                                                                                              }
{          +-------------------------------+                                                   }
{          |       Container Header        |   40 bytes                                        }
{          +-------------------------------+                                                   }
{          |       Section 0 header        |   28 bytes each                                   }
{          |...............................|                                                   }
{          |           - - - -             |                                                   }
{          |...............................|                                                   }
{          |       Section n-1 header      |                                                   }
{          +-------------------------------+                                                   }
{          |       Section Name Table      |                                                   }
{          +-------------------------------+                                                   }
{          |       Section x raw data      |                                                   }
{          +-------------------------------+                                                   }
{          |           - - - -             |                                                   }
{          +-------------------------------+                                                   }
{          |       Section y raw data      |                                                   }
{          +-------------------------------+                                                   }
{                                                                                              }
{                                                                                              }
{ The sections are implicitly numbered from 0 to n according to the order of their headers.    }
{ The headers of the instantiated sections must precede those of the non-instantiated          }
{ sections.  The ordering of the raw data is independent of the section header ordering.       }
{ Each section header contains the offset for that section's raw data.                         }

{ =========================================================================================== }
{ Container Header }
{ ================ }


type
	PEFContainerHeaderPtr = ^PEFContainerHeader;
	PEFContainerHeader = record
		tag1:					OSType;									{  Must contain 'Joy!'. }
		tag2:					OSType;									{  Must contain 'peff'.  (Yes, with two 'f's.) }
		architecture:			OSType;									{  The ISA for code sections.  Constants in CodeFragments.h. }
		formatVersion:			UInt32;									{  The physical format version. }
		dateTimeStamp:			UInt32;									{  Macintosh format creation/modification stamp. }
		oldDefVersion:			UInt32;									{  Old definition version number for the code fragment. }
		oldImpVersion:			UInt32;									{  Old implementation version number for the code fragment. }
		currentVersion:			UInt32;									{  Current version number for the code fragment. }
		sectionCount:			UInt16;									{  Total number of section headers that follow. }
		instSectionCount:		UInt16;									{  Number of instantiated sections. }
		reservedA:				UInt32;									{  Reserved, must be written as zero. }
	end;


const
	kPEFTag1					= $4A6F7921 (* 'Joy!' *);						{  For non-Apple compilers: 0x4A6F7921. }
	kPEFTag2					= $70656666 (* 'peff' *);						{  For non-Apple compilers: 0x70656666. }
	kPEFVersion					= $00000001;


	kPEFFirstSectionHeaderOffset = 40;


	{	 =========================================================================================== 	}
	{	 Section Headers 	}
	{	 =============== 	}


type
	PEFSectionHeaderPtr = ^PEFSectionHeader;
	PEFSectionHeader = record
		nameOffset:				SInt32;									{  Offset of name within the section name table, -1 => none. }
		defaultAddress:			UInt32;									{  Default address, affects relocations. }
		totalLength:			UInt32;									{  Fully expanded size in bytes of the section contents. }
		unpackedLength:			UInt32;									{  Size in bytes of the "initialized" part of the contents. }
		containerLength:		UInt32;									{  Size in bytes of the raw data in the container. }
		containerOffset:		UInt32;									{  Offset of section's raw data. }
		sectionKind:			SInt8;									{  Kind of section contents/usage. }
		shareKind:				SInt8;									{  Sharing level, if a writeable section. }
		alignment:				SInt8;									{  Preferred alignment, expressed as log 2. }
		reservedA:				SInt8;									{  Reserved, must be zero. }
	end;


const
																{  Values for the sectionKind field. }
																{     Section kind values for instantiated sections. }
	kPEFCodeSection				= 0;							{  Code, presumed pure & position independent. }
	kPEFUnpackedDataSection		= 1;							{  Unpacked writeable data. }
	kPEFPackedDataSection		= 2;							{  Packed writeable data. }
	kPEFConstantSection			= 3;							{  Read-only data. }
	kPEFExecDataSection			= 6;							{  Intermixed code and writeable data. }
																{  Section kind values for non-instantiated sections. }
	kPEFLoaderSection			= 4;							{  Loader tables. }
	kPEFDebugSection			= 5;							{  Reserved for future use. }
	kPEFExceptionSection		= 7;							{  Reserved for future use. }
	kPEFTracebackSection		= 8;							{  Reserved for future use. }


																{  Values for the shareKind field. }
	kPEFProcessShare			= 1;							{  Shared within a single process. }
	kPEFGlobalShare				= 4;							{  Shared across the entire system. }
	kPEFProtectedShare			= 5;							{  Readable across the entire system, writeable only to privileged code. }


	{	 =========================================================================================== 	}
	{	 Packed Data Contents 	}
	{	 ==================== 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The raw contents of a packed data section are a sequence of byte codes.  The basic format    	}
	{	 has a 3 bit opcode followed by a 5 bit count.  Additional bytes might be used to contain     	}
	{	 counts larger than 31, and to contain a second or third count.  Further additional bytes     	}
	{	 contain actual data values to transfer.                                                      	}
	{	                                                                                              	}
	{	 All counts are represented in a variable length manner.  A zero in the initial 5 bit count   	}
	{	 indicates the actual value follows.  In this case, and for the second and third counts, the  	}
	{	 count is represented as a variable length sequence of bytes.  The bytes are stored in big    	}
	{	 endian manner, most significant part first.  The high order bit is set in all but the last   	}
	{	 byte.  The value is accumulated by shifting the current value up 7 bits and adding in the    	}
	{	 low order 7 bits of the next byte.                                                           	}

																{  The packed data opcodes. }
	kPEFPkDataZero				= 0;							{  Zero fill "count" bytes. }
	kPEFPkDataBlock				= 1;							{  Block copy "count" bytes. }
	kPEFPkDataRepeat			= 2;							{  Repeat "count" bytes "count2"+1 times. }
	kPEFPkDataRepeatBlock		= 3;							{  Interleaved repeated and unique data. }
	kPEFPkDataRepeatZero		= 4;							{  Interleaved zero and unique data. }


	kPEFPkDataOpcodeShift		= 5;
	kPEFPkDataCount5Mask		= $1F;
	kPEFPkDataMaxCount5			= 31;
	kPEFPkDataVCountShift		= 7;
	kPEFPkDataVCountMask		= $7F;
	kPEFPkDataVCountEndMask		= $80;


	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The following code snippet can be used to input a variable length count.                     	}
	{	                                                                                              	}
	{	      count = 0;                                                                              	}
	{	      do (                                                                                    	}
	{	          byte = *bytePtr++;                                                                  	}
	{	          count = (count << kPEFPkDataVCountShift) | (byte & kPEFPkDataVCountMask);           	}
	{	      ) while ( (byte & kPEFPkDataVCountEndMask) != 0 );                                      	}
	{	                                                                                              	}
	{	 The following code snippet can be used to output a variable length count to a byte array.    	}
	{	 This is more complex than the input code because the chunks are output in big endian order.  	}
	{	 Think about handling values like 0 or 0x030000.                                              	}
	{	                                                                                              	}
	{	      count = 1;.                                                                             	}
	{	      tempValue = value >> kPEFPkDataCountShift;                                              	}
	{	      while ( tempValue != 0 ) (                                                              	}
	{	          count += 1;                                                                         	}
	{	          tempValue = tempValue >> kPEFPkDataCountShift;                                      	}
	{	      )                                                                                       	}
	{	                                                                                              	}
	{	      bytePtr += count;                                                                       	}
	{	      tempPtr = bytePtr - 1;                                                                  	}
	{	      *tempPtr-- = value;     // ! No need to mask, only the low order byte is stored.        	}
	{	      for ( count -= 1; count != 0; count -= 1 ) (                                            	}
	{	          value = value >> kPEFPkDataCountShift;                                              	}
	{	          *tempPtr-- = value | kPEFPkDataCountEndMask;                                        	}
	{	      )                                                                                       	}

	{	 =========================================================================================== 	}
	{	 Loader Section 	}
	{	 ============== 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The loader section contains information needed to prepare the code fragment for execution.   	}
	{	 This includes this fragment's exports, the import libraries and the imported symbols from    	}
	{	 each library, and the relocations for the writeable sections.                                	}
	{	                                                                                              	}
	{	          +-----------------------------------+               <-- containerOffset --------+   	}
	{	          |       Loader Info Header          |   56 bytes                                |   	}
	{	          |-----------------------------------|                                           |   	}
	{	          |       Imported Library 0          |   24 bytes each                           |   	}
	{	          |...................................|                                           |   	}
	{	          |           - - -                   |                                           |   	}
	{	          |...................................|                                           |   	}
	{	          |       Imported Library l-1        |                                           |   	}
	{	          |-----------------------------------|                                           |   	}
	{	          |       Imported Symbol 0           |   4 bytes each                            |   	}
	{	          |...................................|                                           |   	}
	{	          |           - - -                   |                                           |   	}
	{	          |...................................|                                           |   	}
	{	          |       Imported Symbol i-1         |                                           |   	}
	{	          |-----------------------------------|                                           |   	}
	{	          |       Relocation Header 0         |   12 bytes each                           |   	}
	{	          |...................................|                                           |   	}
	{	          |           - - -                   |                                           |   	}
	{	          |...................................|                                           |   	}
	{	          |       Relocation Header r-1       |                                           |   	}
	{	          |-----------------------------------|               <-- + relocInstrOffset -----|   	}
	{	          |       Relocation Instructions     |                                           |   	}
	{	          |-----------------------------------|               <-- + loaderStringsOffset --|   	}
	{	          |       Loader String Table         |                                           |   	}
	{	          |-----------------------------------|               <-- + exportHashOffset -----+   	}
	{	          |       Export Hash Slot 0          |   4 bytes each                                	}
	{	          |...................................|                                               	}
	{	          |           - - -                   |                                               	}
	{	          |...................................|                                               	}
	{	          |       Export Hash Slot h-1        |                                               	}
	{	          |-----------------------------------|                                               	}
	{	          |       Export Symbol Key 0         |   4 bytes each                                	}
	{	          |...................................|                                               	}
	{	          |           - - -                   |                                               	}
	{	          |...................................|                                               	}
	{	          |       Export Symbol Key e-1       |                                               	}
	{	          |-----------------------------------|                                               	}
	{	          |       Export Symbol 0             |   10 bytes each                               	}
	{	          |...................................|                                               	}
	{	          |           - - -                   |                                               	}
	{	          |...................................|                                               	}
	{	          |       Export Symbol e-1           |                                               	}
	{	          +-----------------------------------+                                               	}


type
	PEFLoaderInfoHeaderPtr = ^PEFLoaderInfoHeader;
	PEFLoaderInfoHeader = record
		mainSection:			SInt32;									{  Section containing the main symbol, -1 => none. }
		mainOffset:				UInt32;									{  Offset of main symbol. }
		initSection:			SInt32;									{  Section containing the init routine's TVector, -1 => none. }
		initOffset:				UInt32;									{  Offset of the init routine's TVector. }
		termSection:			SInt32;									{  Section containing the term routine's TVector, -1 => none. }
		termOffset:				UInt32;									{  Offset of the term routine's TVector. }
		importedLibraryCount:	UInt32;									{  Number of imported libraries.  ('l') }
		totalImportedSymbolCount: UInt32;								{  Total number of imported symbols.  ('i') }
		relocSectionCount:		UInt32;									{  Number of sections with relocations.  ('r') }
		relocInstrOffset:		UInt32;									{  Offset of the relocation instructions. }
		loaderStringsOffset:	UInt32;									{  Offset of the loader string table. }
		exportHashOffset:		UInt32;									{  Offset of the export hash table. }
		exportHashTablePower:	UInt32;									{  Export hash table size as log 2.  (Log2('h')) }
		exportedSymbolCount:	UInt32;									{  Number of exported symbols.  ('e') }
	end;

	{	 =========================================================================================== 	}
	{	 Imported Libraries 	}
	{	 ------------------ 	}
	PEFImportedLibraryPtr = ^PEFImportedLibrary;
	PEFImportedLibrary = record
		nameOffset:				UInt32;									{  Loader string table offset of library's name. }
		oldImpVersion:			UInt32;									{  Oldest compatible implementation version. }
		currentVersion:			UInt32;									{  Current version at build time. }
		importedSymbolCount:	UInt32;									{  Imported symbol count for this library. }
		firstImportedSymbol:	UInt32;									{  Index of first imported symbol from this library. }
		options:				SInt8;									{  Option bits for this library. }
		reservedA:				SInt8;									{  Reserved, must be zero. }
		reservedB:				UInt16;									{  Reserved, must be zero. }
	end;


const
																{  Bits for the PEFImportedLibrary options field. }
	kPEFWeakImportLibMask		= $40;							{  The imported library is allowed to be missing. }
	kPEFInitLibBeforeMask		= $80;							{  The imported library must be initialized first. }


	{	 =========================================================================================== 	}
	{	 Imported Symbols 	}
	{	 ---------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The PEFImportedSymbol type has the following bit field layout.                               	}
	{	                                                                                              	}
	{	                                                                     3                        	}
	{	       0             7 8                                             1                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      | symbol class  | offset of symbol name in loader string table  |                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      |<-- 8 bits --->|<-- 24 bits ---------------------------------->|                       	}


type
	PEFImportedSymbolPtr = ^PEFImportedSymbol;
	PEFImportedSymbol = record
		classAndName:			UInt32;
	end;


const
	kPEFImpSymClassShift		= 24;
	kPEFImpSymNameOffsetMask	= $00FFFFFF;
	kPEFImpSymMaxNameOffset		= $00FFFFFF;					{  16,777,215 }


																{  Imported and exported symbol classes. }
	kPEFCodeSymbol				= $00;
	kPEFDataSymbol				= $01;
	kPEFTVectorSymbol			= $02;
	kPEFTOCSymbol				= $03;
	kPEFGlueSymbol				= $04;
	kPEFUndefinedSymbol			= $0F;
	kPEFWeakImportSymMask		= $80;


	{	 =========================================================================================== 	}
	{	 Exported Symbol Hash Table 	}
	{	 -------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 Exported symbols are described in four parts, optimized for speed of lookup.  These parts    	}
	{	 are the "export hash table", the "export key table", the "export symbol table", and the      	}
	{	 "export name table".  Overall they contain a flattened representation of a fairly normal     	}
	{	 hashed symbol table.                                                                         	}
	{	                                                                                              	}
	{	 The export hash table is an array of small fixed size elements.  The number of elements is   	}
	{	 a power of 2.  A 32 bit hash word for a symbol is converted into an index into this array.   	}
	{	 Each hash slot contains a count of the number of exported symbols that map to this slot and  	}
	{	 the index of the first of those symbols in the key and symbol tables.  Of course some hash   	}
	{	 slots will have a zero count.                                                                	}
	{	                                                                                              	}
	{	 The key and symbol tables are also arrays of fixed size elements, one for each exported      	}
	{	 symbol.  Their entries are grouped by hash slot, those elements mapping to the same hash     	}
	{	 slot are contiguous.  The key table contains just the full 32 bit hash word for each         	}
	{	 exported symbol.  The symbol table contains the offset of the symbol's name in the string    	}
	{	 table and other information about the exported symbol.                                       	}
	{	                                                                                              	}
	{	 To look up an export you take the hashword and compute the hash slot index.  You then scan   	}
	{	 the indicated portion of the key table for matching hashwords.  If a hashword matches, you   	}
	{	 look at the corresponding symbol table entry to find the full symbol name.  If the names     	}
	{	 match the symbol is found.                                                                   	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The following function may be used to compute the hash table size.  Signed values are used   	}
	{	 just to avoid potential code generation overhead for unsigned division.                      	}
	{	                                                                                              	}
	{	      UInt8   PEFComputeHashTableExponent ( SInt32    exportCount )                           	}
	{	      (                                                                                       	}
	{	          SInt32  exponent;                                                                   	}
	{	                                                                                              	}
	{	          const SInt32    kExponentLimit      = 16;   // Arbitrary, but must not exceed 30.   	}
	{	          const SInt32    kAverageChainLimit  = 10;   // Arbitrary, for space/time tradeoff.  	}
	{	                                                                                              	}
	{	          for ( exponent = 0; exponent < kExponentLimit; exponent += 1 ) (                    	}
	{	              if ( (exportCount / (1 << exponent)) < kAverageChainLimit ) break;              	}
	{	          )                                                                                   	}
	{	                                                                                              	}
	{	          return exponent;                                                                    	}
	{	                                                                                              	}
	{	      )   // PEFComputeHashTableExponent ()                                                   	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The PEFExportedSymbolHashSlot type has the following bit field layout.                       	}
	{	                                                                                              	}
	{	                                 1 1                                 3                        	}
	{	       0                         3 4                                 1                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      | symbol count              | index of first export key         |                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      |<-- 14 bits -------------->|<-- 18 bits ---------------------->|                       	}


type
	PEFExportedSymbolHashSlotPtr = ^PEFExportedSymbolHashSlot;
	PEFExportedSymbolHashSlot = record
		countAndStart:			UInt32;
	end;


const
	kPEFHashSlotSymCountShift	= 18;
	kPEFHashSlotFirstKeyMask	= $0003FFFF;
	kPEFHashSlotMaxSymbolCount	= $00003FFF;					{   16,383 }
	kPEFHashSlotMaxKeyIndex		= $0003FFFF;					{  262,143 }


	{	 =========================================================================================== 	}
	{	 Exported Symbol Hash Key 	}
	{	 ------------------------ 	}


type
	PEFSplitHashWordPtr = ^PEFSplitHashWord;
	PEFSplitHashWord = record
		nameLength:				UInt16;
		hashValue:				UInt16;
	end;

	PEFExportedSymbolKeyPtr = ^PEFExportedSymbolKey;
	PEFExportedSymbolKey = record
		case SInt16 of
		0: (
			fullHashWord:		UInt32;
			);
		1: (
			splitHashWord:		PEFSplitHashWord;
			);
	end;


const
	kPEFHashLengthShift			= 16;
	kPEFHashValueMask			= $0000FFFF;
	kPEFHashMaxLength			= $0000FFFF;					{  65,535 }


	{	 ---------------------------------------------------------------------------------------------------- 	}
	{	 The following function computes the full 32 bit hash word.                                           	}
	{	                                                                                                      	}
	{	      UInt32  PEFComputeHashWord  ( BytePtr   nameText,       // ! First "letter", not length byte.   	}
	{	                                    UInt32    nameLength )    // ! The text may be zero terminated.   	}
	{	      (                                                                                               	}
	{	          BytePtr charPtr     = nameText;                                                             	}
	{	          SInt32  hashValue   = 0;        // ! Signed to match old published algorithm.               	}
	{	          UInt32  length      = 0;                                                                    	}
	{	          UInt32  limit;                                                                              	}
	{	          UInt32  result;                                                                             	}
	{	          UInt8   currChar;                                                                           	}
	{	                                                                                                      	}
	{	          #define PseudoRotate(x)  ( ( (x) << 1 ) - ( (x) >> 16 ) )                                   	}
	{	                                                                                                      	}
	{	          for ( limit = nameLength; limit > 0; limit -= 1 ) (                                         	}
	{	              currChar = *charPtr++;                                                                  	}
	{	              if ( currChar == NULL ) break;                                                          	}
	{	              length += 1;                                                                            	}
	{	              hashValue = PseudoRotate ( hashValue ) ^ currChar;                                      	}
	{	          )                                                                                           	}
	{	                                                                                                      	}
	{	          result  = (length << kPEFHashLengthShift) |                                                 	}
	{	                    ((UInt16) ((hashValue ^ (hashValue >> 16)) & kPEFHashValueMask));                 	}
	{	                                                                                                      	}
	{	          return result;                                                                              	}
	{	                                                                                                      	}
	{	      )   // PEFComputeHashWord ()                                                                    	}

	{	 =========================================================================================== 	}
	{	 Exported Symbols 	}
	{	 ---------------- 	}


type
	PEFExportedSymbolPtr = ^PEFExportedSymbol;
	PEFExportedSymbol = record
																		{  ! This structure is 10 bytes long and arrays are packed. }
		classAndName:			UInt32;									{  A combination of class and name offset. }
		symbolValue:			UInt32;									{  Typically the symbol's offset within a section. }
		sectionIndex:			SInt16;									{  The index of the section, or pseudo-section, for the symbol. }
	end;

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The classAndName field of the PEFExportedSymbol type has the following bit field layout.     	}
	{	                                                                                              	}
	{	                                                                     3                        	}
	{	       0             7 8                                             1                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      | symbol class  | offset of symbol name in loader string table  |                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                       	}
	{	      |<-- 8 bits --->|<-- 24 bits ---------------------------------->|                       	}


const
	kPEFExpSymClassShift		= 24;
	kPEFExpSymNameOffsetMask	= $00FFFFFF;
	kPEFExpSymMaxNameOffset		= $00FFFFFF;					{  16,777,215 }


																{  Negative section indices indicate pseudo-sections. }
	kPEFAbsoluteExport			= -2;							{  The symbol value is an absolute address. }
	kPEFReexportedImport		= -3;							{  The symbol value is the index of a reexported import. }


	{	 =========================================================================================== 	}
	{	 Loader Relocations 	}
	{	 ================== 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The relocations for a section are defined by a sequence of instructions for an abstract      	}
	{	 machine that is specifically geared to performing relocations commonly needed for the "CFM"  	}
	{	 code generation model.  These instructions occur in 16 bit chunks.  Most instructions have   	}
	{	 just a single chunk.  Instructions that are larger than 16 bits have an opcode and some of   	}
	{	 the operands in the first chunk, with other operands in following chunks.                    	}
	{	                                                                                              	}
	{	 ! Note that the multi-chunk relocations have separate "Compose" macros for each chunk.  The  	}
	{	 ! macros have the same basic name with a positional suffix of "_1st", "_2nd", etc.           	}


type
	PEFRelocChunk						= UInt16;
	PEFLoaderRelocationHeaderPtr = ^PEFLoaderRelocationHeader;
	PEFLoaderRelocationHeader = record
		sectionIndex:			UInt16;									{  Index of the section to be fixed up. }
		reservedA:				UInt16;									{  Reserved, must be zero. }
		relocCount:				UInt32;									{  Number of 16 bit relocation chunks. }
		firstRelocOffset:		UInt32;									{  Offset of first relocation instruction. }
	end;

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 ! Note that the relocCount field is the number of 16 bit relocation chunks, i.e. 1/2 the     	}
	{	 ! total number of bytes of relocation instructions.  While most relocation instructions are  	}
	{	 ! 16 bits long, some are longer so the number of complete relocation instructions may be     	}
	{	 ! less than the relocCount value.                                                            	}

	{	 ------------------------------------------------------------------------------------ 	}
	{	 The PEFRelocField macro is a utility for extracting relocation instruction fields.   	}

	{	 =========================================================================================== 	}
	{	 Basic Relocation Opcodes 	}
	{	 ------------------------ 	}
	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The number of opcode bits varies from 2 to 7.  The enumeration and switch table given here   	}
	{	 are defined in terms of the most significant 7 bits of the first instruction chunk.  An      	}
	{	 instruction is decoded by using the most significant 7 bits as an index into the opcode      	}
	{	 table, which in turn contains appropriately masked forms of the most significant 7 bits.     	}
	{	 The macro PEFRelocBasicOpcode assumes a declaration of the form.                             	}
	{	                                                                                              	}
	{	      UInt8 kPEFRelocBasicOpcodes [kPEFRelocBasicOpcodeRange] = ( PEFMaskedBasicOpcodes );    	}


const
	kPEFRelocBasicOpcodeRange	= 128;


	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The relocation opcodes, clustered by major and minor groups.  The instructions within a      	}
	{	 cluster all have the same bit field layout.  The enumeration values use the high order 7     	}
	{	 bits of the relocation instruction.  Unused low order bits are set to zero.                  	}
	kPEFRelocBySectDWithSkip	= $00;							{  Binary: 00x_xxxx }
	kPEFRelocBySectC			= $20;							{  Binary: 010_0000, group is "RelocRun" }
	kPEFRelocBySectD			= $21;							{  Binary: 010_0001 }
	kPEFRelocTVector12			= $22;							{  Binary: 010_0010 }
	kPEFRelocTVector8			= $23;							{  Binary: 010_0011 }
	kPEFRelocVTable8			= $24;							{  Binary: 010_0100 }
	kPEFRelocImportRun			= $25;							{  Binary: 010_0101 }
	kPEFRelocSmByImport			= $30;							{  Binary: 011_0000, group is "RelocSmIndex" }
	kPEFRelocSmSetSectC			= $31;							{  Binary: 011_0001 }
	kPEFRelocSmSetSectD			= $32;							{  Binary: 011_0010 }
	kPEFRelocSmBySection		= $33;							{  Binary: 011_0011 }
	kPEFRelocIncrPosition		= $40;							{  Binary: 100_0xxx }
	kPEFRelocSmRepeat			= $48;							{  Binary: 100_1xxx }
	kPEFRelocSetPosition		= $50;							{  Binary: 101_000x }
	kPEFRelocLgByImport			= $52;							{  Binary: 101_001x }
	kPEFRelocLgRepeat			= $58;							{  Binary: 101_100x }
	kPEFRelocLgSetOrBySection	= $5A;							{  Binary: 101_101x }
	kPEFRelocUndefinedOpcode	= $FF;							{  Used in masking table for all undefined values. }


	{	 ---------------------------------------------------------------------------- 	}
	{	 The RelocLgSetOrBySection instruction has an additional 4 bits of subopcode  	}
	{	 beyond the 7 used by the dispatch table.  To be precise it has 6 plus 4 but  	}
	{	 the dispatch table ignores the 7th bit, so the subdispatch is on all 4 extra 	}
	{	 subopcode bits.                                                              	}
	kPEFRelocLgBySectionSubopcode = $00;						{  Binary: 0000 }
	kPEFRelocLgSetSectCSubopcode = $01;							{  Binary: 0001 }
	kPEFRelocLgSetSectDSubopcode = $02;							{  Binary: 0010 }


	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The initial values for the opcode "masking" table.  This has the enumeration values from     	}
	{	 above with appropriate replications for "don't care" bits.  It is almost certainly shorter   	}
	{	 and faster to look up the masked value in a table than to use a branch tree.                 	}

	{	 =========================================================================================== 	}
	{	 RelocBySectDWithSkip Instruction (DDAT) 	}
	{	 --------------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocBySectDWithSkip" (DDAT) instruction has the following bit field layout.            	}
	{	                                                                                              	}
	{	                           1         1                                                        	}
	{	       0 1 2             9 0         5                                                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |0 0| skip count    | rel count |                                                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      | 2 |<-- 8 bits --->|<--  6 --->|                                                       	}
	{	                                                                                              	}
	{	 ! Note that the stored skip count and reloc count are the actual values!                     	}
	kPEFRelocWithSkipMaxSkipCount = 255;
	kPEFRelocWithSkipMaxRelocCount = 63;


	{	 =========================================================================================== 	}
	{	 RelocRun Group (CODE, DATA, DESC, DSC2, VTBL, SYMR) 	}
	{	 --------------------------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocRun" group includes the "RelocBySectC" (CODE), "RelocBySectD" (DATA),              	}
	{	 "RelocTVector12" (DESC), "RelocTVector8" (DSC2), "RelocVTable8" (VTBL), and                  	}
	{	 "RelocImportRun" (SYMR) instructions.  This group has the following bit field layout.        	}
	{	                                                                                              	}
	{	                                     1                                                        	}
	{	       0   2 3     6 7               5                                                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |0 1 0| subop.| run length      |                                                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |  3  |<- 4 ->|<-- 9 bits ----->|                                                       	}
	{	                                                                                              	}
	{	 ! Note that the stored run length is the actual value minus 1, but the macros deal with the  	}
	{	 ! actual value!                                                                              	}
	kPEFRelocRunMaxRunLength	= 512;


	{	 =========================================================================================== 	}
	{	 RelocSmIndex Group (SYMB, CDIS, DTIS, SECN) 	}
	{	 ------------------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocSmIndex" group includes the "RelocSmByImport" (SYMB), "RelocSmSetSectC" (CDIS),    	}
	{	 "RelocSmSetSectD" (DTIS) and "RelocSmBySection" (SECN) instructions.  This group has the     	}
	{	 following bit field layout.                                                                  	}
	{	                                                                                              	}
	{	                                     1                                                        	}
	{	       0   2 3     6 7               5                                                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |0 1 1| subop.| index           |                                                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |  3  |<- 4 ->|<-- 9 bits ----->|                                                       	}
	{	                                                                                              	}
	{	 ! Note that the stored index is the actual value!                                            	}
	kPEFRelocSmIndexMaxIndex	= 511;


	{	 =========================================================================================== 	}
	{	 RelocIncrPosition Instruction (DELT) 	}
	{	 ------------------------------------ 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocIncrPosition" (DELT) instruction has the following bit field layout.               	}
	{	                                                                                              	}
	{	                                     1                                                        	}
	{	       0     3 4                     5                                                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |1 0 0 0| offset                |                                                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |<- 4 ->|<-- 12 bits ---------->|                                                       	}
	{	                                                                                              	}
	{	 ! Note that the stored offset is the actual value minus 1, but the macros deal with the      	}
	{	 ! actual value!                                                                              	}
	kPEFRelocIncrPositionMaxOffset = 4096;


	{	 =========================================================================================== 	}
	{	 RelocSmRepeat Instruction (RPT) 	}
	{	 ------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocSmRepeat" (RPT) instruction has the following bit field layout.                    	}
	{	                                                                                              	}
	{	                                     1                                                        	}
	{	       0     3 4     7 8             5                                                        	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |1 0 0 1| chnks | repeat count  |                                                       	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                                                       	}
	{	      |<- 4 ->|<- 4 ->|<-- 8 bits --->|                                                       	}
	{	                                                                                              	}
	{	 ! Note that the stored chunk count and repeat count are the actual values minus 1, but the   	}
	{	 ! macros deal with the actual values!                                                        	}
	kPEFRelocSmRepeatMaxChunkCount = 16;
	kPEFRelocSmRepeatMaxRepeatCount = 256;


	{	 =========================================================================================== 	}
	{	 RelocSetPosition Instruction (LABS) 	}
	{	 ----------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocSetPosition" (LABS) instruction has the following bit field layout.                	}
	{	                                                                                              	}
	{	                                     1                                   1                    	}
	{	       0         5 6                 5     0                             5                    	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |1 0 1 0 0 0| offset (high)     |   | offset (low)                  |                   	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |<-- 6 ---->|<-- 10 bits ------>|   |<-- 16 bits ------------------>|                   	}
	{	                                                                                              	}
	{	 ! Note that the stored offset is the actual value!                                           	}
	kPEFRelocSetPosMaxOffset	= $03FFFFFF;					{  67,108,863 }


	{	 =========================================================================================== 	}
	{	 RelocLgByImport Instruction (LSYM) 	}
	{	 ---------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocLgByImport" (LSYM) instruction has the following bit field layout.                 	}
	{	                                                                                              	}
	{	                                     1                                   1                    	}
	{	       0         5 6                 5     0                             5                    	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |1 0 1 0 0 1| index (high)      |   | index (low)                   |                   	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |<-- 6 ---->|<-- 10 bits ------>|   |<-- 16 bits ------------------>|                   	}
	{	                                                                                              	}
	{	 ! Note that the stored offset is the actual value!                                           	}
	kPEFRelocLgByImportMaxIndex	= $03FFFFFF;					{  67,108,863 }


	{	 =========================================================================================== 	}
	{	 RelocLgRepeat Instruction (LRPT) 	}
	{	 -------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocLgRepeat" (LRPT) instruction has the following bit field layout.                   	}
	{	                                                                                              	}
	{	                           1         1                                   1                    	}
	{	       0         5 6     9 0         5     0                             5                    	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |1 0 1 1 0 0| chnks | rpt (high)|   | repeat count (low)            |                   	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |<--  6 --->|<- 4 ->|<--  6 --->|   |<-- 16 bits ------------------>|                   	}
	{	                                                                                              	}
	{	 ! Note that the stored chunk count is the actual value minus 1, but the macros deal with     	}
	{	 ! the actual value!  The stored repeat count is the actual value!                            	}
	kPEFRelocLgRepeatMaxChunkCount = 16;
	kPEFRelocLgRepeatMaxRepeatCount = $003FFFFF;				{  4,194,303 }


	{	 =========================================================================================== 	}
	{	 RelocLgSetOrBySection Group (LSEC) 	}
	{	 ---------------------------------- 	}

	{	 -------------------------------------------------------------------------------------------- 	}
	{	 The "RelocLgSetOrBySection" (LSEC) instruction is a group including the "RelocLgBySection",  	}
	{	 "RelocLgSetSectC" and "RelocLgSetSectD" instructions.  This group has the following bit      	}
	{	 field layout.                                                                                	}
	{	                                                                                              	}
	{	                           1         1                                   1                    	}
	{	       0         5 6     9 0         5     0                             5                    	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |1 0 1 1 0 1| subop | idx (high)|   | index (low)                   |                   	}
	{	      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+                   	}
	{	      |<--  6 --->|<- 4 ->|<--  6 --->|   |<-- 16 bits ------------------>|                   	}
	{	                                                                                              	}
	{	 ! Note that the stored index is the actual value!                                            	}
	kPEFRelocLgSetOrBySectionMaxIndex = $003FFFFF;				{  4,194,303 }


{$ALIGN MAC68K}


end.
