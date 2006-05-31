{
     File:       WorldScript.p
 
     Contains:   WorldScript I Interfaces.
 
     Version:    Technology: System 7.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit WorldScript;
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
uses MacTypes,QuickdrawText;


{$ALIGN MAC68K}


type
	WSIOffset							= UInt16;
	WSIByteCount						= UInt8;
	WSIByteIndex						= UInt8;
	{	 offset from start of sub-table to row in state table 	}
	WSIStateOffset						= UInt16;
	WSITableOffset						= UInt32;
	WSISubtableOffset					= UInt16;
	WSIGlyphcode						= UInt16;
	WSITableIdentifiers					= UInt32;

const
	kScriptSettingsTag			= $696E666F (* 'info' *);
	kMetamorphosisTag			= $6D6F7274 (* 'mort' *);
	kGlyphExpansionTag			= $67326723 (* 'g2g#' *);
	kPropertiesTag				= $70726F70 (* 'prop' *);
	kJustificationTag			= $6B617368 (* 'kash' *);
	kCharToGlyphTag				= $636D6170 (* 'cmap' *);
	kGlyphToCharTag				= $70616D63 (* 'pamc' *);
	kFindScriptRunTag			= $66737462 (* 'fstb' *);


	{	***           L O O K U P    T A B L E    T Y P E S       ***	}
	WSILookupSimpleArray		= 0;							{  a simple array indexed by glyph code  }
	WSILookupSegmentSingle		= 2;							{  segment mapping to single value  }
	WSILookupSegmentArray		= 4;							{  segment mapping to lookup array  }
	WSILookupSingleTable		= 6;							{  sorted list of glyph, value pairs  }
	WSILookupTrimmedArray		= 8;							{  a simple trimmed array indexed by glyph code  }


type
	WSILookupTableFormat				= UInt16;
	WSILookupValue						= UInt16;
	{	 An offset from the beginning of the lookup table 	}
	WSILookupOffset						= UInt16;
	{	  FORMAT SPECIFIC DEFINITIONS 	}
	{	
	        lookupSimpleArray:
	        
	        This is a simple array which maps all glyphs in the font
	        to lookup values.
	    	}
	WSILookupArrayHeaderPtr = ^WSILookupArrayHeader;
	WSILookupArrayHeader = record
		lookupValues:			array [0..0] of WSILookupValue;			{  The array of values indexed by glyph code  }
	end;

	{	
	        lookupTrimmedArray:
	        
	        This is a single trimmed array which maps a single range
	        of glyhs in the font to lookup values.
	    	}
	WSILookupTrimmedArrayHeaderPtr = ^WSILookupTrimmedArrayHeader;
	WSILookupTrimmedArrayHeader = record
		firstGlyph:				WSIGlyphcode;
		limitGlyph:				WSIGlyphcode;
		valueArray:				array [0..0] of WSILookupValue;
	end;

	{	 The format specific part of the subtable header 	}
	WSILookupFormatSpecificHeaderPtr = ^WSILookupFormatSpecificHeader;
	WSILookupFormatSpecificHeader = record
		case SInt16 of
		0: (
			simpleArray:		WSILookupArrayHeader;					{  rename lookupArray as simpleArray <9>  }
			);
		1: (
			trimmedArray:		WSILookupTrimmedArrayHeader;
			);
	end;

	{	 The overall subtable header 	}
	WSILookupTableHeaderPtr = ^WSILookupTableHeader;
	WSILookupTableHeader = record
		format:					WSILookupTableFormat;					{  table format  }
		fsHeader:				WSILookupFormatSpecificHeader;			{  format specific header  }
	end;


	{	***       G L Y P H    E X P A N S I O N    ***	}

const
																{  fixed 1.0  }
	kCurrentGlyphExpansionVersion = $00010000;


type
	GlyphExpansionFormats				= UInt16;

const
	GlyphExpansionLookupFormat	= 1;
	GlyphExpansionContextualFormat = 2;


type
	ExpandedGlyphClusterPtr = ^ExpandedGlyphCluster;
	ExpandedGlyphCluster = packed record
		numGlyphs:				WSIByteCount;
		bestGlyph:				WSIByteIndex;
		glyphs:					array [0..0] of WSIGlyphcode;
	end;

	ExpandedGlyphOffsetPtr = ^ExpandedGlyphOffset;
	ExpandedGlyphOffset = record
		glyph:					WSIGlyphcode;
		offset:					WSIOffset;								{  offset to ExpandedGlyphCluster  }
	end;

	GlyphExpansionStateTablePtr = ^GlyphExpansionStateTable;
	GlyphExpansionStateTable = record
		stateTableOffset:		WSISubtableOffset;
		classTableOffset:		WSISubtableOffset;
		actionTableOffset:		WSISubtableOffset;						{  state, class and actions tables follow here...  }
	end;

	GlyphExpansionTablePtr = ^GlyphExpansionTable;
	GlyphExpansionTable = record
		version:				Fixed;
		format:					SInt16;
		expansionNumer:			SInt16;
		expansionDenom:			SInt16;								{  num/denom ratio for expansion <2>  }
		case SInt16 of
		0: (
			stateTable:			GlyphExpansionStateTable;
			);
		1: (
			lookup:				WSILookupTableHeader;					{  expanded glyph clusters follow here...  }
			);
	end;


	{	 Glyph-to-Character constants and types  	}

const
	kCurrentGlyphToCharVersion	= $00010100;


type
	GlyphToCharLookupFormats			= UInt16;

const
	kGlyphToCharLookup8Format	= 1;
	kGlyphToCharLookup16Format	= 2;
	kGlyphToCharLookup32Format	= 3;


type
	GlyphToCharFontIndex				= UInt8;
	QDGlyphcode							= UInt8;
	GlyphToCharActionTablePtr = ^GlyphToCharActionTable;
	GlyphToCharActionTable = record
		fontNameOffset:			WSISubtableOffset;						{  offset relative to this table  }
		actions:				WSILookupTableHeader;					{  only support lookupSimpleArray format for now  }
	end;

	GlyphToCharActionHeaderPtr = ^GlyphToCharActionHeader;
	GlyphToCharActionHeader = record
		numTables:				SInt16;								{  0..n  }
		offsets:				array [0..0] of WSISubtableOffset;		{  offsets from start of action table header  }
	end;

	GlyphToCharHeaderPtr = ^GlyphToCharHeader;
	GlyphToCharHeader = record
		version:				Fixed;
		actionOffset:			WSISubtableOffset;						{  offset to GlyphToCharActionHeader  }
		format:					SInt16;								{  size of font mask  }
		mappingTable:			WSILookupTableHeader;
	end;


	{	 JUSTIFICATION TYPES
	    WorldScript supports justification of text using insertion. The justification
	    table specifies a insertion string to insert between 2 specified glyphs.
	    Each combination of inter-glyph boundary can be assigned a justification priority,
	    the higher the priority the more justification strings inserted at that position.
	    
	    The priorities for each inter-glyph boundary are specified by the justification table's
	    state table.
	    
	    Special handling is done for scripts which use spaces to justify, because the width of 
	    a space varies depending on the setting of SpaceExtra. This is why the number of spaces
	    per inserting string is specified in the justification table.
	
		}

const
																{  1.0 not supported  }
	kCurrentJustificationVersion = $0200;

	kJustificationStateTableFormat = 1;

																{  WSI's internal limitation <12>  }
	kMaxJustificationStringLength = 13;


type
	WSIJustificationPriority			= UInt8;

const
	WSIJustificationSetMarkMask	= $80;


type
	WSIJustificationStateEntryPtr = ^WSIJustificationStateEntry;
	WSIJustificationStateEntry = packed record
		markPriority:			WSIJustificationPriority;				{  non-zero priorities means insertion  }
		priority:				WSIJustificationPriority;
		newState:				WSIStateOffset;
	end;

	WSIJustificationClasses				= UInt16;

const
	wsiJustEndOfLineClass		= 0;
	wsiJustEndOfRunClass		= 1;
	wsiJustDeletedGlyphClass	= 2;
	wsiJustUserDefinedClass		= 3;


type
	WSIJustificationStates				= UInt16;

const
	wsiStartOfLineState			= 0;							{  pre-defined states  }
	wsiStartOfRunState			= 1;
	wsiUserDefinedState			= 2;

	{	 pre-multiplied: class# * sizeof(WSIJustificationStateEntry) 	}

type
	WSIJustificationClassOffset			= UInt8;
	WSIJustificationStateTablePtr = ^WSIJustificationStateTable;
	WSIJustificationStateTable = record
		maxPriorities:			SInt16;
		rowWidth:				UInt16;									{  width of a state table row in bytes  }
		classTableOffset:		SInt16;
		stateTableOffset:		SInt16;
	end;

	{	
	            Last two fields of above structure - someday?
	            WSIJustificationClassOffset classes[up to 64 classes supported];
	            WSIJustificationStateEntry  states[up to your heart's desire];
	        	}
	WSIJustificationHeaderPtr = ^WSIJustificationHeader;
	WSIJustificationHeader = record
		version:				SInt16;
		format:					SInt16;
		scaling:				Point;									{  numer/denom scaling of priority weights <7>  }
		spacesPerInsertion:		UInt16;									{  # of $20 chars in justification insertion string <12>  }
		justStringOffset:		UInt16;									{  offset to justification string  }
		stateTable:				WSIJustificationStateTable;				{  long-aligned boundary aligned w/ spacesPerInsertion field - justification string follows  }
	end;


	{	 Line Layout's Property table version <11> 	}

const
																{  v1.0  }
	currentPropsTableVersion	= $00010000;

																{  version is octal 0100 or hex 0x40 (#64)  }
	kCharToGlyphCurrentVersion	= $40;

	{	 pass as priorityWeight to JustifyWSILayout to use script's current just setting 	}
	kScriptsDefaultJustWeight	= -1;


	{	 feature selectors used in FindScriptRun and itl5 configuration tables <9> 	}

type
	WSIFeatureType						= UInt16;
	WSIFeatureSelector					= UInt16;
	WSIFeaturePtr = ^WSIFeature;
	WSIFeature = record
		featureType:			WSIFeatureType;
		featureSelector:		WSIFeatureSelector;
	end;

{$ALIGN MAC68K}


end.
