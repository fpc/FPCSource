{
     File:       AIFF.p
 
     Contains:   Definition of AIFF file format components.
 
     Version:    Technology: System 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit AIFF;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

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


const
	AIFFID						= FourCharCode('AIFF');
	AIFCID						= FourCharCode('AIFC');
	FormatVersionID				= FourCharCode('FVER');
	CommonID					= FourCharCode('COMM');
	FORMID						= FourCharCode('FORM');
	SoundDataID					= FourCharCode('SSND');
	MarkerID					= FourCharCode('MARK');
	InstrumentID				= FourCharCode('INST');
	MIDIDataID					= FourCharCode('MIDI');
	AudioRecordingID			= FourCharCode('AESD');
	ApplicationSpecificID		= FourCharCode('APPL');
	CommentID					= FourCharCode('COMT');
	NameID						= FourCharCode('NAME');
	AuthorID					= FourCharCode('AUTH');
	CopyrightID					= FourCharCode('(c) ');
	AnnotationID				= FourCharCode('ANNO');

	NoLooping					= 0;
	ForwardLooping				= 1;
	ForwardBackwardLooping		= 2;

																{  AIFF-C Versions  }
	AIFCVersion1				= $A2805140;

	{	 Compression Names 	}
	NoneName					= 'not compressed';
	ACE2to1Name					= 'ACE 2-to-1';
	ACE8to3Name					= 'ACE 8-to-3';
	MACE3to1Name				= 'MACE 3-to-1';
	MACE6to1Name				= 'MACE 6-to-1';
																{  Compression Types  }
	NoneType					= FourCharCode('NONE');
	ACE2Type					= FourCharCode('ACE2');
	ACE8Type					= FourCharCode('ACE8');
	MACE3Type					= FourCharCode('MAC3');
	MACE6Type					= FourCharCode('MAC6');


type
{ changed from ID to ChunkID, as ID is used in objc.pas (sorry) }
	ChunkID									= UInt32; {ID}
	MarkerIdType						= SInt16;
	ChunkHeaderPtr = ^ChunkHeader;
	ChunkHeader = record
		ckID:					ChunkID;
		ckSize:					SInt32;
	end;

	ContainerChunkPtr = ^ContainerChunk;
	ContainerChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		formType:				ChunkID;
	end;

	FormatVersionChunkPtr = ^FormatVersionChunk;
	FormatVersionChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		timestamp:				UInt32;
	end;

	CommonChunkPtr = ^CommonChunk;
	CommonChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		numChannels:			SInt16;
		numSampleFrames:		UInt32;
		sampleSize:				SInt16;
		sampleRate:				extended80;
	end;

	ExtCommonChunkPtr = ^ExtCommonChunk;
	ExtCommonChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		numChannels:			SInt16;
		numSampleFrames:		UInt32;
		sampleSize:				SInt16;
		sampleRate:				extended80;
		compressionType:		ChunkID;
		compressionName:		SInt8;									{  variable length array, Pascal string  }
	end;

	SoundDataChunkPtr = ^SoundDataChunk;
	SoundDataChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		offset:					UInt32;
		blockSize:				UInt32;
	end;

	MarkerPtr = ^Marker;
	Marker = record
		id:						MarkerIdType;
		position:				UInt32;
		markerName:				Str255;
	end;

	MarkerChunkPtr = ^MarkerChunk;
	MarkerChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		numMarkers:				UInt16;
		Markers:				array [0..0] of Marker;					{  variable length array  }
	end;

	AIFFLoopPtr = ^AIFFLoop;
	AIFFLoop = record
		playMode:				SInt16;
		beginLoop:				MarkerIdType;
		endLoop:				MarkerIdType;
	end;

	InstrumentChunkPtr = ^InstrumentChunk;
	InstrumentChunk = packed record
		ckID:					ChunkID;
		ckSize:					SInt32;
		baseFrequency:			UInt8;
		detune:					UInt8;
		lowFrequency:			UInt8;
		highFrequency:			UInt8;
		lowVelocity:			UInt8;
		highVelocity:			UInt8;
		gain:					SInt16;
		sustainLoop:			AIFFLoop;
		releaseLoop:			AIFFLoop;
	end;

	MIDIDataChunkPtr = ^MIDIDataChunk;
	MIDIDataChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		MIDIdata:				SInt8;									{  variable length array  }
	end;

	AudioRecordingChunkPtr = ^AudioRecordingChunk;
	AudioRecordingChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		AESChannelStatus:		packed array [0..23] of UInt8;
	end;

	ApplicationSpecificChunkPtr = ^ApplicationSpecificChunk;
	ApplicationSpecificChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		applicationSignature:	OSType;
		data:					SInt8;									{  variable length array  }
	end;

	CommentPtr = ^Comment;
	Comment = record
		timeStamp:				UInt32;
		marker:					MarkerIdType;
		count:					UInt16;
		text:					SInt8;									{  variable length array, Pascal string  }
	end;

	CommentsChunkPtr = ^CommentsChunk;
	CommentsChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		numComments:			UInt16;
		comments:				array [0..0] of Comment;				{  variable length array  }
	end;

	TextChunkPtr = ^TextChunk;
	TextChunk = record
		ckID:					ChunkID;
		ckSize:					SInt32;
		text:					SInt8;									{  variable length array, Pascal string  }
	end;

{$ALIGN MAC68K}


end.
