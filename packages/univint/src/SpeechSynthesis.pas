{
     File:       SpeechSynthesis/SpeechSynthesis.h
 
     Contains:   Speech Interfaces.
 
     Version:    SpeechSynthesis-3.10.35~1
 
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

unit SpeechSynthesis;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,Files,CFBase,CFDictionary,CFError,CFURL;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

const
	kTextToSpeechSynthType = FourCharCode('ttsc');
	kTextToSpeechVoiceType = FourCharCode('ttvd');
	kTextToSpeechVoiceFileType = FourCharCode('ttvf');
	kTextToSpeechVoiceBundleType = FourCharCode('ttvb');

const
	kNoEndingProsody = 1;
	kNoSpeechInterrupt = 2;
	kPreflightThenPause = 4;

const
	kImmediate = 0;
	kEndOfWord = 1;
	kEndOfSentence = 2;


{------------------------------------------}
{ GetSpeechInfo & SetSpeechInfo selectors  }
{------------------------------------------}
const
	soStatus = FourCharCode('stat');
	soErrors = FourCharCode('erro');
	soInputMode = FourCharCode('inpt');
	soCharacterMode = FourCharCode('char');
	soNumberMode = FourCharCode('nmbr');
	soRate = FourCharCode('rate');
	soPitchBase = FourCharCode('pbas');
	soPitchMod = FourCharCode('pmod');
	soVolume = FourCharCode('volm');
	soSynthType = FourCharCode('vers');
	soRecentSync = FourCharCode('sync');
	soPhonemeSymbols = FourCharCode('phsy');
	soCurrentVoice = FourCharCode('cvox');
	soCommandDelimiter = FourCharCode('dlim');
	soReset = FourCharCode('rset');
	soCurrentA5 = FourCharCode('myA5');
	soRefCon = FourCharCode('refc');
	soTextDoneCallBack = FourCharCode('tdcb'); { use with SpeechTextDoneProcPtr}
	soSpeechDoneCallBack = FourCharCode('sdcb'); { use with SpeechDoneProcPtr}
	soSyncCallBack = FourCharCode('sycb'); { use with SpeechSyncProcPtr}
	soErrorCallBack = FourCharCode('ercb'); { use with SpeechErrorProcPtr}
	soPhonemeCallBack = FourCharCode('phcb'); { use with SpeechPhonemeProcPtr}
	soWordCallBack = FourCharCode('wdcb'); { use with SpeechWordProcPtr}
	soSynthExtension = FourCharCode('xtnd');
	soSoundOutput = FourCharCode('sndo');
	soOutputToFileWithCFURL = FourCharCode('opaf'); { Pass a CFURLRef to write to this file, NULL to generate sound.}
	soOutputToExtAudioFile = FourCharCode('opax'); { Pass a ExtAudioFileRef to write to this file, NULL to generate sound. Available in 10.6 and later.}
	soOutputToAudioDevice = FourCharCode('opad'); { Pass an AudioDeviceID to play to this file, 0 to play to default output}
	soPhonemeOptions = FourCharCode('popt'); { Available in 10.6 and later}


{ Type for stopSpeakingAtBoundary: and pauseSpeakingAtBoundary:}
const
	kSpeechImmediateBoundary = 0;
	kSpeechWordBoundary = 1;
	kSpeechSentenceBoundary = 2;

type
	SpeechBoundary = UInt32;
{------------------------------------------}
{ Speaking Mode Constants                  }
{------------------------------------------}
const
	modeText = FourCharCode('TEXT'); { input mode constants             }
	modePhonemes = FourCharCode('PHON');
	modeNormal = FourCharCode('NORM'); { character mode and number mode constants }
	modeLiteral = FourCharCode('LTRL');


const
	soVoiceDescription = FourCharCode('info');
	soVoiceFile = FourCharCode('fref');

{------------------------------------------}
{ Flags for phoneme generation.            }
{------------------------------------------}
const
	kSpeechGenerateTune = 1;    { Generate detailed "tune" instead of just phonemes  }
	kSpeechRelativePitch = 2;    { Pitch relative to voice baseline             }
	kSpeechRelativeDuration = 4;    { Duration relative to speech rate             }
	kSpeechShowSyllables = 8;     { Show all syllable marks                              }

{------------------------------------------}
{ AudioUnit constants - new in 10.5        }
{------------------------------------------}
const
	kAudioUnitSubType_SpeechSynthesis = FourCharCode('ttsp'); { kAudioUnitType_Generator }
	kAudioUnitProperty_Voice = 3330; { Get/Set (VoiceSpec)      }
	kAudioUnitProperty_SpeechChannel = 3331; { Get (SpeechChannel)      }

{
   The speech manager sources may or may not need SpeechChannelRecord.
   If not, the .i file should be changed to use the opaque mechanism.
}
type
	SpeechChannelRecordPtr = ^SpeechChannelRecord;
	SpeechChannelRecord = record
		data: array [0..1-1] of SIGNEDLONG;
	end;
type
	SpeechChannel = SpeechChannelRecordPtr;
	SpeechChannelPtr = ^SpeechChannel;  { when a var xx:SpeechChannel parameter can be nil, it is changed to xx: SpeechChannelPtr }

type
	VoiceSpecPtr = ^VoiceSpec;
	VoiceSpec = record
		creator: OSType;
		id: OSType;
	end;

const
	kNeuter = 0;
	kMale = 1;
	kFemale = 2;


type
	VoiceDescriptionPtr = ^VoiceDescription;
	VoiceDescription = record
		length: SInt32;
		voice: VoiceSpec;
		version: SInt32;
		name: Str63;
		comment: Str255;
		gender: SInt16;
		age: SInt16;
		script: SInt16;
		language: SInt16;
		region: SInt16;
		reserved: array [0..4-1] of SInt32;
	end;


type
	VoiceFileInfoPtr = ^VoiceFileInfo;
	VoiceFileInfo = record
		fileSpec: FSSpec;
		resID: SInt16;
	end;
type
	SpeechStatusInfoPtr = ^SpeechStatusInfo;
	SpeechStatusInfo = record
		outputBusy: Boolean;
		outputPaused: Boolean;
		inputBytesLeft: SIGNEDLONG;
		phonemeCode: SInt16;
	end;


type
	SpeechErrorInfoPtr = ^SpeechErrorInfo;
	SpeechErrorInfo = record
		count: SInt16;
		oldest: OSErr;
		oldPos: SIGNEDLONG;
		newest: OSErr;
		newPos: SIGNEDLONG;
	end;


type
	SpeechVersionInfoPtr = ^SpeechVersionInfo;
	SpeechVersionInfo = record
		synthType: OSType;
		synthSubType: OSType;
		synthManufacturer: OSType;
		synthFlags: SInt32;
		synthVersion: NumVersion;
	end;


type
	PhonemeInfoPtr = ^PhonemeInfo;
	PhonemeInfo = record
		opcode: SInt16;
		phStr: Str15;
		exampleStr: Str31;
		hiliteStart: SInt16;
		hiliteEnd: SInt16;
	end;

type
	PhonemeDescriptorPtr = ^PhonemeDescriptor;
	PhonemeDescriptor = record
		phonemeCount: SInt16;
		thePhonemes: array [0..1-1] of PhonemeInfo;
	end;
type
	SpeechXtndDataPtr = ^SpeechXtndData;
	SpeechXtndData = record
		synthCreator: OSType;
		synthData: array [0..2-1] of Byte;
	end;

type
	DelimiterInfoPtr = ^DelimiterInfo;
	DelimiterInfo = record
		startDelimiter: array [0..2-1] of Byte;
		endDelimiter: array [0..2-1] of Byte;
	end;
{ Synthesizer Properties }
{
 *  kSpeechStatusProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechStatusProperty: CFStringRef; external name '_kSpeechStatusProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorsProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorsProperty: CFStringRef; external name '_kSpeechErrorsProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechInputModeProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechInputModeProperty: CFStringRef; external name '_kSpeechInputModeProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechCharacterModeProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechCharacterModeProperty: CFStringRef; external name '_kSpeechCharacterModeProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechNumberModeProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechNumberModeProperty: CFStringRef; external name '_kSpeechNumberModeProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechRateProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechRateProperty: CFStringRef; external name '_kSpeechRateProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPitchBaseProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPitchBaseProperty: CFStringRef; external name '_kSpeechPitchBaseProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPitchModProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPitchModProperty: CFStringRef; external name '_kSpeechPitchModProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechVolumeProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechVolumeProperty: CFStringRef; external name '_kSpeechVolumeProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechSynthesizerInfoProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSynthesizerInfoProperty: CFStringRef; external name '_kSpeechSynthesizerInfoProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechRecentSyncProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechRecentSyncProperty: CFStringRef; external name '_kSpeechRecentSyncProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeSymbolsProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeSymbolsProperty: CFStringRef; external name '_kSpeechPhonemeSymbolsProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechCurrentVoiceProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechCurrentVoiceProperty: CFStringRef; external name '_kSpeechCurrentVoiceProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechCommandDelimiterProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechCommandDelimiterProperty: CFStringRef; external name '_kSpeechCommandDelimiterProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechResetProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechResetProperty: CFStringRef; external name '_kSpeechResetProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechOutputToFileURLProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechOutputToFileURLProperty: CFStringRef; external name '_kSpeechOutputToFileURLProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechOutputToExtAudioFileProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechOutputToExtAudioFileProperty: CFStringRef; external name '_kSpeechOutputToExtAudioFileProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
{
 *  kSpeechOutputToAudioDeviceProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechOutputToAudioDeviceProperty: CFStringRef; external name '_kSpeechOutputToAudioDeviceProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
{
 *  kSpeechRefConProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechRefConProperty: CFStringRef; external name '_kSpeechRefConProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechTextDoneCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechTextDoneCallBack: CFStringRef; external name '_kSpeechTextDoneCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechSpeechDoneCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSpeechDoneCallBack: CFStringRef; external name '_kSpeechSpeechDoneCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechSyncCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSyncCallBack: CFStringRef; external name '_kSpeechSyncCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeCallBack: CFStringRef; external name '_kSpeechPhonemeCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorCFCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorCFCallBack: CFStringRef; external name '_kSpeechErrorCFCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechWordCFCallBack
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechWordCFCallBack: CFStringRef; external name '_kSpeechWordCFCallBack'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeOptionsProperty
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeOptionsProperty: CFStringRef; external name '_kSpeechPhonemeOptionsProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)
{ Speaking Modes}
{
 *  kSpeechModeText
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechModeText: CFStringRef; external name '_kSpeechModeText'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechModePhoneme
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechModePhoneme: CFStringRef; external name '_kSpeechModePhoneme'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechModeNormal
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechModeNormal: CFStringRef; external name '_kSpeechModeNormal'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechModeLiteral
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechModeLiteral: CFStringRef; external name '_kSpeechModeLiteral'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys for options parameter in SpeakCFString}
{
 *  kSpeechNoEndingProsody
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechNoEndingProsody: CFStringRef; external name '_kSpeechNoEndingProsody'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechNoSpeechInterrupt
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechNoSpeechInterrupt: CFStringRef; external name '_kSpeechNoSpeechInterrupt'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPreflightThenPause
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPreflightThenPause: CFStringRef; external name '_kSpeechPreflightThenPause'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechStatusProperty}
{
 *  kSpeechStatusOutputBusy
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechStatusOutputBusy: CFStringRef; external name '_kSpeechStatusOutputBusy'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechStatusOutputPaused
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechStatusOutputPaused: CFStringRef; external name '_kSpeechStatusOutputPaused'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechStatusNumberOfCharactersLeft
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechStatusNumberOfCharactersLeft: CFStringRef; external name '_kSpeechStatusNumberOfCharactersLeft'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechStatusPhonemeCode
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechStatusPhonemeCode: CFStringRef; external name '_kSpeechStatusPhonemeCode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechErrorProperty}
{
 *  kSpeechErrorCount
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorCount: CFStringRef; external name '_kSpeechErrorCount'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorOldest
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorOldest: CFStringRef; external name '_kSpeechErrorOldest'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorOldestCharacterOffset
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorOldestCharacterOffset: CFStringRef; external name '_kSpeechErrorOldestCharacterOffset'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorNewest
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorNewest: CFStringRef; external name '_kSpeechErrorNewest'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorNewestCharacterOffset
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorNewestCharacterOffset: CFStringRef; external name '_kSpeechErrorNewestCharacterOffset'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechSynthesizerInfoProperty}
{
 *  kSpeechSynthesizerInfoIdentifier
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSynthesizerInfoIdentifier: CFStringRef; external name '_kSpeechSynthesizerInfoIdentifier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechSynthesizerInfoManufacturer
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSynthesizerInfoManufacturer: CFStringRef; external name '_kSpeechSynthesizerInfoManufacturer'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechSynthesizerInfoVersion
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechSynthesizerInfoVersion: CFStringRef; external name '_kSpeechSynthesizerInfoVersion'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechPhonemeSymbolsProperty}
{
 *  kSpeechPhonemeInfoOpcode
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeInfoOpcode: CFStringRef; external name '_kSpeechPhonemeInfoOpcode'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeInfoSymbol
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeInfoSymbol: CFStringRef; external name '_kSpeechPhonemeInfoSymbol'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeInfoExample
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeInfoExample: CFStringRef; external name '_kSpeechPhonemeInfoExample'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeInfoHiliteStart
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeInfoHiliteStart: CFStringRef; external name '_kSpeechPhonemeInfoHiliteStart'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechPhonemeInfoHiliteEnd
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechPhonemeInfoHiliteEnd: CFStringRef; external name '_kSpeechPhonemeInfoHiliteEnd'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechCurrentVoiceProperty}
{
 *  kSpeechVoiceCreator
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechVoiceCreator: CFStringRef; external name '_kSpeechVoiceCreator'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechVoiceID
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechVoiceID: CFStringRef; external name '_kSpeechVoiceID'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Dictionary keys returned by kSpeechCommandDelimiterProperty}
{
 *  kSpeechCommandPrefix
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechCommandPrefix: CFStringRef; external name '_kSpeechCommandPrefix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechCommandSuffix
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechCommandSuffix: CFStringRef; external name '_kSpeechCommandSuffix'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Use with useSpeechDictionary:}
{
 *  kSpeechDictionaryLocaleIdentifier
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryLocaleIdentifier: CFStringRef; external name '_kSpeechDictionaryLocaleIdentifier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechDictionaryModificationDate
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryModificationDate: CFStringRef; external name '_kSpeechDictionaryModificationDate'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechDictionaryPronunciations
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryPronunciations: CFStringRef; external name '_kSpeechDictionaryPronunciations'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechDictionaryAbbreviations
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryAbbreviations: CFStringRef; external name '_kSpeechDictionaryAbbreviations'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechDictionaryEntrySpelling
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryEntrySpelling: CFStringRef; external name '_kSpeechDictionaryEntrySpelling'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechDictionaryEntryPhonemes
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechDictionaryEntryPhonemes: CFStringRef; external name '_kSpeechDictionaryEntryPhonemes'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{ Error callback user info keys}
{
 *  kSpeechErrorCallbackSpokenString
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorCallbackSpokenString: CFStringRef; external name '_kSpeechErrorCallbackSpokenString'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)
{
 *  kSpeechErrorCallbackCharacterOffset
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
var kSpeechErrorCallbackCharacterOffset: CFStringRef; external name '_kSpeechErrorCallbackCharacterOffset'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)

type
	SpeechTextDoneProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; {const} nextBuf: UnivPtrPtr; var byteLen: UNSIGNEDLONG; var controlFlags: SInt32 );
	SpeechDoneProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon );
	SpeechSyncProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; syncMessage: OSType );
	SpeechErrorProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; theError: OSErr; bytePos: SIGNEDLONG );
	SpeechPhonemeProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; phonemeOpcode: SInt16 );
	SpeechWordProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; wordPos: UNSIGNEDLONG; wordLen: UInt16 );
	SpeechTextDoneUPP = SpeechTextDoneProcPtr;
	SpeechDoneUPP = SpeechDoneProcPtr;
	SpeechSyncUPP = SpeechSyncProcPtr;
	SpeechErrorUPP = SpeechErrorProcPtr;
	SpeechPhonemeUPP = SpeechPhonemeProcPtr;
	SpeechWordUPP = SpeechWordProcPtr;
{
 *  NewSpeechTextDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechTextDoneUPP( userRoutine: SpeechTextDoneProcPtr ): SpeechTextDoneUPP; external name '_NewSpeechTextDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewSpeechDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechDoneUPP( userRoutine: SpeechDoneProcPtr ): SpeechDoneUPP; external name '_NewSpeechDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewSpeechSyncUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechSyncUPP( userRoutine: SpeechSyncProcPtr ): SpeechSyncUPP; external name '_NewSpeechSyncUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewSpeechErrorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechErrorUPP( userRoutine: SpeechErrorProcPtr ): SpeechErrorUPP; external name '_NewSpeechErrorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechPhonemeUPP( userRoutine: SpeechPhonemeProcPtr ): SpeechPhonemeUPP; external name '_NewSpeechPhonemeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewSpeechWordUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSpeechWordUPP( userRoutine: SpeechWordProcPtr ): SpeechWordUPP; external name '_NewSpeechWordUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechTextDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechTextDoneUPP( userUPP: SpeechTextDoneUPP ); external name '_DisposeSpeechTextDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechDoneUPP( userUPP: SpeechDoneUPP ); external name '_DisposeSpeechDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechSyncUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechSyncUPP( userUPP: SpeechSyncUPP ); external name '_DisposeSpeechSyncUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechErrorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechErrorUPP( userUPP: SpeechErrorUPP ); external name '_DisposeSpeechErrorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechPhonemeUPP( userUPP: SpeechPhonemeUPP ); external name '_DisposeSpeechPhonemeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSpeechWordUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSpeechWordUPP( userUPP: SpeechWordUPP ); external name '_DisposeSpeechWordUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechTextDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechTextDoneUPP( chan: SpeechChannel; refCon: SRefCon; {const} var nextBuf: UnivPtr; var byteLen: UNSIGNEDLONG; var controlFlags: SInt32; userUPP: SpeechTextDoneUPP ); external name '_InvokeSpeechTextDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechDoneUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechDoneUPP( chan: SpeechChannel; refCon: SRefCon; userUPP: SpeechDoneUPP ); external name '_InvokeSpeechDoneUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechSyncUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechSyncUPP( chan: SpeechChannel; refCon: SRefCon; syncMessage: OSType; userUPP: SpeechSyncUPP ); external name '_InvokeSpeechSyncUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechErrorUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechErrorUPP( chan: SpeechChannel; refCon: SRefCon; theError: OSErr; bytePos: SIGNEDLONG; userUPP: SpeechErrorUPP ); external name '_InvokeSpeechErrorUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechPhonemeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechPhonemeUPP( chan: SpeechChannel; refCon: SRefCon; phonemeOpcode: SInt16; userUPP: SpeechPhonemeUPP ); external name '_InvokeSpeechPhonemeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSpeechWordUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeSpeechWordUPP( chan: SpeechChannel; refCon: SRefCon; wordPos: UNSIGNEDLONG; wordLen: UInt16; userUPP: SpeechWordUPP ); external name '_InvokeSpeechWordUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

type
	SpeechErrorCFProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; theError: CFErrorRef );
	SpeechWordCFProcPtr = procedure( chan: SpeechChannel; refCon: SRefCon; aString: CFStringRef; wordRange: CFRange );

{
 *  SpeechManagerVersion()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeechManagerVersion: NumVersion; external name '_SpeechManagerVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MakeVoiceSpec()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function MakeVoiceSpec( creator: OSType; id: OSType; var voice: VoiceSpec ): OSErr; external name '_MakeVoiceSpec';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CountVoices()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function CountVoices( var numVoices: SInt16 ): OSErr; external name '_CountVoices';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetIndVoice()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetIndVoice( index: SInt16; var voice: VoiceSpec ): OSErr; external name '_GetIndVoice';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetVoiceDescription()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetVoiceDescription( const (*var*) voice: VoiceSpec; var info: VoiceDescription; infoLength: SIGNEDLONG ): OSErr; external name '_GetVoiceDescription';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetVoiceInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetVoiceInfo( const (*var*) voice: VoiceSpec; selector: OSType; voiceInfo: UnivPtr ): OSErr; external name '_GetVoiceInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  NewSpeechChannel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function NewSpeechChannel( voice: VoiceSpecPtr { can be NULL }; var chan: SpeechChannel ): OSErr; external name '_NewSpeechChannel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DisposeSpeechChannel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function DisposeSpeechChannel( chan: SpeechChannel ): OSErr; external name '_DisposeSpeechChannel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SpeakString()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeakString( const (*var*) textToBeSpoken: Str255 ): OSErr; external name '_SpeakString';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SpeakText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeakText( chan: SpeechChannel; textBuf: {const} UnivPtr; textBytes: UNSIGNEDLONG ): OSErr; external name '_SpeakText';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SpeakBuffer()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeakBuffer( chan: SpeechChannel; textBuf: {const} UnivPtr; textBytes: UNSIGNEDLONG; controlFlags: SInt32 ): OSErr; external name '_SpeakBuffer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  StopSpeech()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function StopSpeech( chan: SpeechChannel ): OSErr; external name '_StopSpeech';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  StopSpeechAt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function StopSpeechAt( chan: SpeechChannel; whereToStop: SInt32 ): OSErr; external name '_StopSpeechAt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  PauseSpeechAt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function PauseSpeechAt( chan: SpeechChannel; whereToPause: SInt32 ): OSErr; external name '_PauseSpeechAt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ContinueSpeech()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function ContinueSpeech( chan: SpeechChannel ): OSErr; external name '_ContinueSpeech';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SpeechBusy()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeechBusy: SInt16; external name '_SpeechBusy';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SpeechBusySystemWide()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SpeechBusySystemWide: SInt16; external name '_SpeechBusySystemWide';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetSpeechRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SetSpeechRate( chan: SpeechChannel; rate: Fixed ): OSErr; external name '_SetSpeechRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetSpeechRate()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetSpeechRate( chan: SpeechChannel; var rate: Fixed ): OSErr; external name '_GetSpeechRate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetSpeechPitch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SetSpeechPitch( chan: SpeechChannel; pitch: Fixed ): OSErr; external name '_SetSpeechPitch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetSpeechPitch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetSpeechPitch( chan: SpeechChannel; var pitch: Fixed ): OSErr; external name '_GetSpeechPitch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetSpeechInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function SetSpeechInfo( chan: SpeechChannel; selector: OSType; speechInfo: {const} UnivPtr ): OSErr; external name '_SetSpeechInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetSpeechInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function GetSpeechInfo( chan: SpeechChannel; selector: OSType; speechInfo: UnivPtr ): OSErr; external name '_GetSpeechInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  TextToPhonemes()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function TextToPhonemes( chan: SpeechChannel; textBuf: {const} UnivPtr; textBytes: UNSIGNEDLONG; phonemeBuf: Handle; var phonemeBytes: SIGNEDLONG ): OSErr; external name '_TextToPhonemes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UseDictionary()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in SpeechLib 1.0 and later
 }
function UseDictionary( chan: SpeechChannel; dictionary: Handle ): OSErr; external name '_UseDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Replaces SpeakBuffer}
{
 *  SpeakCFString()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SpeakCFString( chan: SpeechChannel; aString: CFStringRef; options: CFDictionaryRef ): OSErr; external name '_SpeakCFString';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ Replaces UseDictionary}
{
 *  UseSpeechDictionary()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function UseSpeechDictionary( chan: SpeechChannel; speechDictionary: CFDictionaryRef ): OSErr; external name '_UseSpeechDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ Replaces TextToPhonemes}
{
 *  CopyPhonemesFromText()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CopyPhonemesFromText( chan: SpeechChannel; text: CFStringRef; var phonemes: CFStringRef ): OSErr; external name '_CopyPhonemesFromText';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ Replaces GetSpeechInfo}
{
 *  CopySpeechProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CopySpeechProperty( chan: SpeechChannel; property: CFStringRef; var objct: CFTypeRef ): OSErr; external name '_CopySpeechProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ Replaces SetSpeechInfo}
{
 *  SetSpeechProperty()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SetSpeechProperty( chan: SpeechChannel; property: CFStringRef; objct: CFTypeRef ): OSErr; external name '_SetSpeechProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER *)


{ Support loading and unloading synthesizers and voices from locations other than the standard directories.}
{
 *  SpeechSynthesisRegisterModuleURL()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SpeechSynthesisRegisterModuleURL( url: CFURLRef ): OSErr; external name '_SpeechSynthesisRegisterModuleURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{
 *  SpeechSynthesisUnregisterModuleURL()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.6 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function SpeechSynthesisUnregisterModuleURL( url: CFURLRef ): OSErr; external name '_SpeechSynthesisUnregisterModuleURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
