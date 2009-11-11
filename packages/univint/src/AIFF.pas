{
     File:       CarbonCore/AIFF.h
 
     Contains:   Definition of AIFF file format components.
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1989-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit AIFF;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$endc} {not MACOSALLINCLUDE}



{$ALIGN MAC68K}

const
	AIFFID = FourCharCode('AIFF');
	AIFCID = FourCharCode('AIFC');
	FormatVersionID = FourCharCode('FVER');
	CommonID = FourCharCode('COMM');
	FORMID = FourCharCode('FORM');
	SoundDataID = FourCharCode('SSND');
	MarkerID = FourCharCode('MARK');
	InstrumentID = FourCharCode('INST');
	MIDIDataID = FourCharCode('MIDI');
	AudioRecordingID = FourCharCode('AESD');
	ApplicationSpecificID = FourCharCode('APPL');
	CommentID = FourCharCode('COMT');
	NameID = FourCharCode('NAME');
	AuthorID = FourCharCode('AUTH');
	CopyrightID = FourCharCode('(c) ');
	AnnotationID = FourCharCode('ANNO');

const
	NoLooping = 0;
	ForwardLooping = 1;
	ForwardBackwardLooping = 2;


const
{ AIFF-C Versions }
	AIFCVersion1 = $A2805140;

{ Compression Names }
const
	NoneName = 'not compressed';
const
	ACE2to1Name = 'ACE 2-to-1';
const
	ACE8to3Name = 'ACE 8-to-3';
const
	MACE3to1Name = 'MACE 3-to-1';
const
	MACE6to1Name = 'MACE 6-to-1';
const
{ Compression Types }
	NoneType = FourCharCode('NONE');
	ACE2Type = FourCharCode('ACE2');
	ACE8Type = FourCharCode('ACE8');
	MACE3Type = FourCharCode('MAC3');
	MACE6Type = FourCharCode('MAC6');

{
    AIFF.h use to define a type, ID, which causes conflicts with other headers and application which want to use
    this pretty common name as their own type.  If you were previously relying on this being defined here, you 
    should either define it yourself or change your references to it into a UInt32.
    
    typedef UInt32 ID;
}
type
	MarkerIdType = SInt16;
	ChunkHeaderPtr = ^ChunkHeader;
	ChunkHeader = record
		ckID: UInt32;
		ckSize: SInt32;
	end;
type
	ContainerChunkPtr = ^ContainerChunk;
	ContainerChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		formType: UInt32;
	end;
type
	FormatVersionChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		timestamp: UInt32;
	end;
	FormatVersionChunkPtr = ^FormatVersionChunk;
type
	CommonChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		numChannels: SInt16;
		numSampleFrames: UInt32;
		sampleSize: SInt16;
		sampleRate: extended80;
	end;
	CommonChunkPtr = ^CommonChunk;
type
	ExtCommonChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		numChannels: SInt16;
		numSampleFrames: UInt32;
		sampleSize: SInt16;
		sampleRate: extended80;
		compressionType: UInt32;
		compressionName: SInt8;     { variable length array, Pascal string }
	end;
	ExtCommonChunkPtr = ^ExtCommonChunk;
type
	SoundDataChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		offset: UInt32;
		blockSize: UInt32;
	end;
	SoundDataChunkPtr = ^SoundDataChunk;
type
	MarkerPtr = ^Marker;
	Marker = record
		id: MarkerIdType;
		position: UInt32;
		markerName: Str255;
	end;
type
	MarkerChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		numMarkers: UInt16;
		Markers: array [0..0] of Marker;             { variable length array }
	end;
	MarkerChunkPtr = ^MarkerChunk;
type
	AIFFLoopPtr = ^AIFFLoop;
	AIFFLoop = record
		playMode: SInt16;
		beginLoop: MarkerIdType;
		endLoop: MarkerIdType;
	end;
type
	InstrumentChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		baseFrequency: UInt8;
		detune: UInt8;
		lowFrequency: UInt8;
		highFrequency: UInt8;
		lowVelocity: UInt8;
		highVelocity: UInt8;
		gain: SInt16;
		sustainLoop: AIFFLoop;
		releaseLoop: AIFFLoop;
	end;
	InstrumentChunkPtr = ^InstrumentChunk;
type
	MIDIDataChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		MIDIdata: SInt8;            { variable length array }
	end;
	MIDIDataChunkPtr = ^MIDIDataChunk;
type
	AudioRecordingChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		AESChannelStatus:		packed array [0..23] of UInt8;
	end;
	AudioRecordingChunkPtr = ^AudioRecordingChunk;
type
	ApplicationSpecificChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		applicationSignature: OSType;
		data: UInt8;                { variable length array }
	end;
	ApplicationSpecificChunkPtr = ^ApplicationSpecificChunk;
type
	CommentPtr = ^Comment;
	Comment = record
		timeStamp: UInt32;
		marker: MarkerIdType;
		count: UInt16;
		text: SInt8;                { variable length array, Pascal string }
	end;
	Comment_fix = Comment;
type
	CommentsChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		numComments: UInt16;
		Comment: array [0..0] of Comment_fix;            { variable length array }
	end;
	CommentsChunkPtr = ^CommentsChunk;
type
	TextChunk = record
		ckID: UInt32;
		ckSize: SInt32;
		text: SInt8;                { variable length array, Pascal string }
	end;
	TextChunkPtr = ^TextChunk;

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
