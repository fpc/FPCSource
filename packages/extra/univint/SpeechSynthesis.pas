{
     File:       SpeechSynthesis.p
 
     Contains:   Speech Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit SpeechSynthesis;
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
uses MacTypes,MixedMode,Files;

{$ALIGN MAC68K}


const
	kTextToSpeechSynthType		= $74747363 (* 'ttsc' *);
	kTextToSpeechVoiceType		= $74747664 (* 'ttvd' *);
	kTextToSpeechVoiceFileType	= $74747666 (* 'ttvf' *);
	kTextToSpeechVoiceBundleType = $74747662 (* 'ttvb' *);

	kNoEndingProsody			= 1;
	kNoSpeechInterrupt			= 2;
	kPreflightThenPause			= 4;

	kImmediate					= 0;
	kEndOfWord					= 1;
	kEndOfSentence				= 2;


	{	------------------------------------------	}
	{	 GetSpeechInfo & SetSpeechInfo selectors  	}
	{	------------------------------------------	}
	soStatus					= $73746174 (* 'stat' *);
	soErrors					= $6572726F (* 'erro' *);
	soInputMode					= $696E7074 (* 'inpt' *);
	soCharacterMode				= $63686172 (* 'char' *);
	soNumberMode				= $6E6D6272 (* 'nmbr' *);
	soRate						= $72617465 (* 'rate' *);
	soPitchBase					= $70626173 (* 'pbas' *);
	soPitchMod					= $706D6F64 (* 'pmod' *);
	soVolume					= $766F6C6D (* 'volm' *);
	soSynthType					= $76657273 (* 'vers' *);
	soRecentSync				= $73796E63 (* 'sync' *);
	soPhonemeSymbols			= $70687379 (* 'phsy' *);
	soCurrentVoice				= $63766F78 (* 'cvox' *);
	soCommandDelimiter			= $646C696D (* 'dlim' *);
	soReset						= $72736574 (* 'rset' *);
	soCurrentA5					= $6D794135 (* 'myA5' *);
	soRefCon					= $72656663 (* 'refc' *);
	soTextDoneCallBack			= $74646362 (* 'tdcb' *);						{  use with SpeechTextDoneProcPtr }
	soSpeechDoneCallBack		= $73646362 (* 'sdcb' *);						{  use with SpeechDoneProcPtr }
	soSyncCallBack				= $73796362 (* 'sycb' *);						{  use with SpeechSyncProcPtr }
	soErrorCallBack				= $65726362 (* 'ercb' *);						{  use with SpeechErrorProcPtr }
	soPhonemeCallBack			= $70686362 (* 'phcb' *);						{  use with SpeechPhonemeProcPtr }
	soWordCallBack				= $77646362 (* 'wdcb' *);
	soSynthExtension			= $78746E64 (* 'xtnd' *);
	soSoundOutput				= $736E646F (* 'sndo' *);


	{	------------------------------------------	}
	{	 Speaking Mode Constants                  	}
	{	------------------------------------------	}
	modeText					= $54455854 (* 'TEXT' *);						{  input mode constants              }
	modePhonemes				= $50484F4E (* 'PHON' *);
	modeNormal					= $4E4F524D (* 'NORM' *);						{  character mode and number mode constants  }
	modeLiteral					= $4C54524C (* 'LTRL' *);


	soVoiceDescription			= $696E666F (* 'info' *);
	soVoiceFile					= $66726566 (* 'fref' *);


type
	SpeechChannel    = ^SInt32; { an opaque 32-bit type }
	SpeechChannelPtr = ^SpeechChannel;  { when a var xx:SpeechChannel parameter can be nil, it is changed to xx: SpeechChannelPtr }

	VoiceSpecPtr = ^VoiceSpec;
	VoiceSpec = record
		creator:				OSType;
		id:						OSType;
	end;


const
	kNeuter						= 0;
	kMale						= 1;
	kFemale						= 2;


type
	VoiceDescriptionPtr = ^VoiceDescription;
	VoiceDescription = record
		length:					SInt32;
		voice:					VoiceSpec;
		version:				SInt32;
		name:					Str63;
		comment:				Str255;
		gender:					SInt16;
		age:					SInt16;
		script:					SInt16;
		language:				SInt16;
		region:					SInt16;
		reserved:				array [0..3] of SInt32;
	end;


	VoiceFileInfoPtr = ^VoiceFileInfo;
	VoiceFileInfo = record
		fileSpec:				FSSpec;
		resID:					SInt16;
	end;

	SpeechStatusInfoPtr = ^SpeechStatusInfo;
	SpeechStatusInfo = record
		outputBusy:				boolean;
		outputPaused:			boolean;
		inputBytesLeft:			SInt32;
		phonemeCode:			SInt16;
	end;


	SpeechErrorInfoPtr = ^SpeechErrorInfo;
	SpeechErrorInfo = record
		count:					SInt16;
		oldest:					OSErr;
		oldPos:					SInt32;
		newest:					OSErr;
		newPos:					SInt32;
	end;


	SpeechVersionInfoPtr = ^SpeechVersionInfo;
	SpeechVersionInfo = record
		synthType:				OSType;
		synthSubType:			OSType;
		synthManufacturer:		OSType;
		synthFlags:				SInt32;
		synthVersion:			NumVersion;
	end;


	PhonemeInfoPtr = ^PhonemeInfo;
	PhonemeInfo = record
		opcode:					SInt16;
		phStr:					Str15;
		exampleStr:				Str31;
		hiliteStart:			SInt16;
		hiliteEnd:				SInt16;
	end;


	PhonemeDescriptorPtr = ^PhonemeDescriptor;
	PhonemeDescriptor = record
		phonemeCount:			SInt16;
		thePhonemes:			array [0..0] of PhonemeInfo;
	end;

	SpeechXtndDataPtr = ^SpeechXtndData;
	SpeechXtndData = packed record
		synthCreator:			OSType;
		synthData:				packed array [0..1] of Byte;
	end;


	DelimiterInfoPtr = ^DelimiterInfo;
	DelimiterInfo = packed record
		startDelimiter:			packed array [0..1] of Byte;
		endDelimiter:			packed array [0..1] of Byte;
	end;


{$ifc TYPED_FUNCTION_POINTERS}
	SpeechTextDoneProcPtr = procedure(chan: SpeechChannel; refCon: SInt32; var nextBuf: UnivPtr; var byteLen: UInt32; var controlFlags: SInt32);
{$elsec}
	SpeechTextDoneProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SpeechDoneProcPtr = procedure(chan: SpeechChannel; refCon: SInt32);
{$elsec}
	SpeechDoneProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SpeechSyncProcPtr = procedure(chan: SpeechChannel; refCon: SInt32; syncMessage: OSType);
{$elsec}
	SpeechSyncProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SpeechErrorProcPtr = procedure(chan: SpeechChannel; refCon: SInt32; theError: OSErr; bytePos: SInt32);
{$elsec}
	SpeechErrorProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SpeechPhonemeProcPtr = procedure(chan: SpeechChannel; refCon: SInt32; phonemeOpcode: SInt16);
{$elsec}
	SpeechPhonemeProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SpeechWordProcPtr = procedure(chan: SpeechChannel; refCon: SInt32; wordPos: UInt32; wordLen: UInt16);
{$elsec}
	SpeechWordProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SpeechTextDoneUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechTextDoneUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SpeechDoneUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechDoneUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SpeechSyncUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechSyncUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SpeechErrorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechErrorUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SpeechPhonemeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechPhonemeUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	SpeechWordUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SpeechWordUPP = UniversalProcPtr;
{$endc}	

const
	uppSpeechTextDoneProcInfo = $0000FFC0;
	uppSpeechDoneProcInfo = $000003C0;
	uppSpeechSyncProcInfo = $00000FC0;
	uppSpeechErrorProcInfo = $00003BC0;
	uppSpeechPhonemeProcInfo = $00000BC0;
	uppSpeechWordProcInfo = $00002FC0;
	{
	 *  NewSpeechTextDoneUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSpeechTextDoneUPP(userRoutine: SpeechTextDoneProcPtr): SpeechTextDoneUPP; external name '_NewSpeechTextDoneUPP'; { old name was NewSpeechTextDoneProc }
{
 *  NewSpeechDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechDoneUPP(userRoutine: SpeechDoneProcPtr): SpeechDoneUPP; external name '_NewSpeechDoneUPP'; { old name was NewSpeechDoneProc }
{
 *  NewSpeechSyncUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechSyncUPP(userRoutine: SpeechSyncProcPtr): SpeechSyncUPP; external name '_NewSpeechSyncUPP'; { old name was NewSpeechSyncProc }
{
 *  NewSpeechErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechErrorUPP(userRoutine: SpeechErrorProcPtr): SpeechErrorUPP; external name '_NewSpeechErrorUPP'; { old name was NewSpeechErrorProc }
{
 *  NewSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechPhonemeUPP(userRoutine: SpeechPhonemeProcPtr): SpeechPhonemeUPP; external name '_NewSpeechPhonemeUPP'; { old name was NewSpeechPhonemeProc }
{
 *  NewSpeechWordUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechWordUPP(userRoutine: SpeechWordProcPtr): SpeechWordUPP; external name '_NewSpeechWordUPP'; { old name was NewSpeechWordProc }
{
 *  DisposeSpeechTextDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechTextDoneUPP(userUPP: SpeechTextDoneUPP); external name '_DisposeSpeechTextDoneUPP';
{
 *  DisposeSpeechDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechDoneUPP(userUPP: SpeechDoneUPP); external name '_DisposeSpeechDoneUPP';
{
 *  DisposeSpeechSyncUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechSyncUPP(userUPP: SpeechSyncUPP); external name '_DisposeSpeechSyncUPP';
{
 *  DisposeSpeechErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechErrorUPP(userUPP: SpeechErrorUPP); external name '_DisposeSpeechErrorUPP';
{
 *  DisposeSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechPhonemeUPP(userUPP: SpeechPhonemeUPP); external name '_DisposeSpeechPhonemeUPP';
{
 *  DisposeSpeechWordUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSpeechWordUPP(userUPP: SpeechWordUPP); external name '_DisposeSpeechWordUPP';
{
 *  InvokeSpeechTextDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechTextDoneUPP(chan: SpeechChannel; refCon: SInt32; var nextBuf: UnivPtr; var byteLen: UInt32; var controlFlags: SInt32; userRoutine: SpeechTextDoneUPP); external name '_InvokeSpeechTextDoneUPP'; { old name was CallSpeechTextDoneProc }
{
 *  InvokeSpeechDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechDoneUPP(chan: SpeechChannel; refCon: SInt32; userRoutine: SpeechDoneUPP); external name '_InvokeSpeechDoneUPP'; { old name was CallSpeechDoneProc }
{
 *  InvokeSpeechSyncUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechSyncUPP(chan: SpeechChannel; refCon: SInt32; syncMessage: OSType; userRoutine: SpeechSyncUPP); external name '_InvokeSpeechSyncUPP'; { old name was CallSpeechSyncProc }
{
 *  InvokeSpeechErrorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechErrorUPP(chan: SpeechChannel; refCon: SInt32; theError: OSErr; bytePos: SInt32; userRoutine: SpeechErrorUPP); external name '_InvokeSpeechErrorUPP'; { old name was CallSpeechErrorProc }
{
 *  InvokeSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechPhonemeUPP(chan: SpeechChannel; refCon: SInt32; phonemeOpcode: SInt16; userRoutine: SpeechPhonemeUPP); external name '_InvokeSpeechPhonemeUPP'; { old name was CallSpeechPhonemeProc }
{
 *  InvokeSpeechWordUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSpeechWordUPP(chan: SpeechChannel; refCon: SInt32; wordPos: UInt32; wordLen: UInt16; userRoutine: SpeechWordUPP); external name '_InvokeSpeechWordUPP'; { old name was CallSpeechWordProc }
{
 *  SpeechManagerVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeechManagerVersion: NumVersion; external name '_SpeechManagerVersion';
{
 *  MakeVoiceSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MakeVoiceSpec(creator: OSType; id: OSType; var voice: VoiceSpec): OSErr; external name '_MakeVoiceSpec';
{
 *  CountVoices()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountVoices(var numVoices: SInt16): OSErr; external name '_CountVoices';
{
 *  GetIndVoice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndVoice(index: SInt16; var voice: VoiceSpec): OSErr; external name '_GetIndVoice';
{
 *  GetVoiceDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetVoiceDescription(const (*var*) voice: VoiceSpec; var info: VoiceDescription; infoLength: SInt32): OSErr; external name '_GetVoiceDescription';
{
 *  GetVoiceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetVoiceInfo(const (*var*) voice: VoiceSpec; selector: OSType; voiceInfo: UnivPtr): OSErr; external name '_GetVoiceInfo';
{
 *  NewSpeechChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewSpeechChannel(voice: VoiceSpecPtr; var chan: SpeechChannel): OSErr; external name '_NewSpeechChannel';
{
 *  DisposeSpeechChannel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeSpeechChannel(chan: SpeechChannel): OSErr; external name '_DisposeSpeechChannel';
{
 *  SpeakString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeakString(const (*var*) textToBeSpoken: Str255): OSErr; external name '_SpeakString';
{
 *  SpeakText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeakText(chan: SpeechChannel; textBuf: UnivPtr; textBytes: UInt32): OSErr; external name '_SpeakText';
{
 *  SpeakBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeakBuffer(chan: SpeechChannel; textBuf: UnivPtr; textBytes: UInt32; controlFlags: SInt32): OSErr; external name '_SpeakBuffer';
{
 *  StopSpeech()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StopSpeech(chan: SpeechChannel): OSErr; external name '_StopSpeech';
{
 *  StopSpeechAt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StopSpeechAt(chan: SpeechChannel; whereToStop: SInt32): OSErr; external name '_StopSpeechAt';
{
 *  PauseSpeechAt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PauseSpeechAt(chan: SpeechChannel; whereToPause: SInt32): OSErr; external name '_PauseSpeechAt';
{
 *  ContinueSpeech()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ContinueSpeech(chan: SpeechChannel): OSErr; external name '_ContinueSpeech';
{
 *  SpeechBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeechBusy: SInt16; external name '_SpeechBusy';
{
 *  SpeechBusySystemWide()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SpeechBusySystemWide: SInt16; external name '_SpeechBusySystemWide';
{
 *  SetSpeechRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSpeechRate(chan: SpeechChannel; rate: Fixed): OSErr; external name '_SetSpeechRate';
{
 *  GetSpeechRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSpeechRate(chan: SpeechChannel; var rate: Fixed): OSErr; external name '_GetSpeechRate';
{
 *  SetSpeechPitch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSpeechPitch(chan: SpeechChannel; pitch: Fixed): OSErr; external name '_SetSpeechPitch';
{
 *  GetSpeechPitch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSpeechPitch(chan: SpeechChannel; var pitch: Fixed): OSErr; external name '_GetSpeechPitch';
{
 *  SetSpeechInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSpeechInfo(chan: SpeechChannel; selector: OSType; speechInfo: UnivPtr): OSErr; external name '_SetSpeechInfo';
{
 *  GetSpeechInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSpeechInfo(chan: SpeechChannel; selector: OSType; speechInfo: UnivPtr): OSErr; external name '_GetSpeechInfo';
{
 *  TextToPhonemes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TextToPhonemes(chan: SpeechChannel; textBuf: UnivPtr; textBytes: UInt32; phonemeBuf: Handle; var phonemeBytes: SInt32): OSErr; external name '_TextToPhonemes';
{
 *  UseDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UseDictionary(chan: SpeechChannel; dictionary: Handle): OSErr; external name '_UseDictionary';
{$ALIGN MAC68K}


end.
