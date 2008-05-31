{
     File:       SpeechRecognition.p
 
     Contains:   Apple Speech Recognition Toolbox Interfaces.
 
     Version:    Technology: PlainTalk 1.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit SpeechRecognition;
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
uses MacTypes,Files,AEDataModel,MixedMode;

{$ALIGN MAC68K}

{ Error Codes [Speech recognition gets -5100 through -5199] }

const
	kSRNotAvailable				= -5100;						{  the service requested is not avail or applicable  }
	kSRInternalError			= -5101;						{  a system internal or hardware error condition  }
	kSRComponentNotFound		= -5102;						{  a needed system resource was not located  }
	kSROutOfMemory				= -5103;						{  an out of memory error occurred in the toolbox memory space  }
	kSRNotASpeechObject			= -5104;						{  the object specified is no longer or never was valid  }
	kSRBadParameter				= -5105;						{  an invalid parameter was specified  }
	kSRParamOutOfRange			= -5106;						{  when we say 0-100, don't pass in 101.  }
	kSRBadSelector				= -5107;						{  an unrecognized selector was specified  }
	kSRBufferTooSmall			= -5108;						{  returned from attribute access functions  }
	kSRNotARecSystem			= -5109;						{  the object used was not a SRRecognitionSystem  }
	kSRFeedbackNotAvail			= -5110;						{  there is no feedback window associated with SRRecognizer  }
	kSRCantSetProperty			= -5111;						{  a non-settable property was specified  }
	kSRCantGetProperty			= -5112;						{  a non-gettable property was specified  }
	kSRCantSetDuringRecognition	= -5113;						{  the property can't be set while recognition is in progress -- do before or between utterances.  }
	kSRAlreadyListening			= -5114;						{  in response to SRStartListening  }
	kSRNotListeningState		= -5115;						{  in response to SRStopListening  }
	kSRModelMismatch			= -5116;						{  no acoustical models are avail to match request  }
	kSRNoClientLanguageModel	= -5117;						{  trying to access a non-specified SRLanguageModel  }
	kSRNoPendingUtterances		= -5118;						{  nothing to continue search on  }
	kSRRecognitionCanceled		= -5119;						{  an abort error occurred during search  }
	kSRRecognitionDone			= -5120;						{  search has finished, but nothing was recognized  }
	kSROtherRecAlreadyModal		= -5121;						{  another recognizer is modal at the moment, so can't set this recognizer's kSRBlockModally property right now  }
	kSRHasNoSubItems			= -5122;						{  SRCountItems or related routine was called on an object without subelements -- e.g. a word -- rather than phrase, path, or LM.  }
	kSRSubItemNotFound			= -5123;						{  returned when accessing a non-existent sub item of a container  }
	kSRLanguageModelTooBig		= -5124;						{  Cant build language models so big  }
	kSRAlreadyReleased			= -5125;						{  this object has already been released before  }
	kSRAlreadyFinished			= -5126;						{  the language model can't be finished twice  }
	kSRWordNotFound				= -5127;						{  the spelling couldn't be found in lookup(s)  }
	kSRNotFinishedWithRejection	= -5128;						{  property not found because the LMObj is not finished with rejection  }
	kSRExpansionTooDeep			= -5129;						{  Language model is left recursive or is embedded too many levels  }
	kSRTooManyElements			= -5130;						{  Too many elements added to phrase or path or other langauge model object  }
	kSRCantAdd					= -5131;						{  Can't add given type of object to the base SRLanguageObject (e.g.in SRAddLanguageObject)    }
	kSRSndInSourceDisconnected	= -5132;						{  Sound input source is disconnected  }
	kSRCantReadLanguageObject	= -5133;						{  An error while trying to create new Language object from file or pointer -- possibly bad format  }
																{  non-release debugging error codes are included here  }
	kSRNotImplementedYet		= -5199;						{  you'd better wait for this feature in a future release  }


	{	 Type Definitions 	}

type
	SRSpeechObject    = ^SInt32; { an opaque 32-bit type }
	SRSpeechObjectPtr = ^SRSpeechObject;  { when a var xx:SRSpeechObject parameter can be nil, it is changed to xx: SRSpeechObjectPtr }
	SRRecognitionSystem					= SRSpeechObject;
	SRRecognizer						= SRSpeechObject;
	SRSpeechSource						= SRSpeechObject;
	SRRecognitionResult					= SRSpeechSource;
	SRLanguageObject					= SRSpeechObject;
	SRLanguageModel						= SRLanguageObject;
	SRPath								= SRLanguageObject;
	SRPhrase							= SRLanguageObject;
	SRWord								= SRLanguageObject;
	{	 between 0 and 100 	}
	SRSpeedSetting						= UInt16;
	{	 between 0 and 100 	}
	SRRejectionLevel					= UInt16;
	{	 When an event occurs, the user supplied proc will be called with a pointer   	}
	{	  to the param passed in and a flag to indicate conditions such               	}
	{	  as interrupt time or system background time.                                	}
	SRCallBackStructPtr = ^SRCallBackStruct;
	SRCallBackStruct = record
		what:					SInt32;								{  one of notification flags  }
		message:				SInt32;								{  contains SRRecognitionResult id  }
		instance:				SRRecognizer;							{  ID of recognizer being notified  }
		status:					OSErr;									{  result status of last search  }
		flags:					SInt16;								{  non-zero if occurs during interrupt  }
		refCon:					SInt32;								{  user defined - set from SRCallBackParam  }
	end;

	{	 Call back procedure definition 	}
{$ifc TYPED_FUNCTION_POINTERS}
	SRCallBackProcPtr = procedure(var param: SRCallBackStruct);
{$elsec}
	SRCallBackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SRCallBackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SRCallBackUPP = UniversalProcPtr;
{$endc}	

const
	uppSRCallBackProcInfo = $000000C0;
	{
	 *  NewSRCallBackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSRCallBackUPP(userRoutine: SRCallBackProcPtr): SRCallBackUPP; external name '_NewSRCallBackUPP'; { old name was NewSRCallBackProc }
{
 *  DisposeSRCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSRCallBackUPP(userUPP: SRCallBackUPP); external name '_DisposeSRCallBackUPP';
{
 *  InvokeSRCallBackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeSRCallBackUPP(var param: SRCallBackStruct; userRoutine: SRCallBackUPP); external name '_InvokeSRCallBackUPP'; { old name was CallSRCallBackProc }
type
	SRCallBackParamPtr = ^SRCallBackParam;
	SRCallBackParam = record
		callBack:				SRCallBackUPP;
		refCon:					SInt32;
	end;

	{	 Recognition System Types 	}

const
	kSRDefaultRecognitionSystemID = 0;

	{	 Recognition System Properties 	}
	kSRFeedbackAndListeningModes = FourCharCode('fbwn');						{  short: one of kSRNoFeedbackHasListenModes, kSRHasFeedbackHasListenModes, kSRNoFeedbackNoListenModes  }
	kSRRejectedWord				= FourCharCode('rejq');						{  the SRWord used to represent a rejection  }
	kSRCleanupOnClientExit		= FourCharCode('clup');						{  Boolean: Default is true. The rec system and everything it owns is disposed when the client application quits  }

	kSRNoFeedbackNoListenModes	= 0;							{  next allocated recognizer has no feedback window and doesn't use listening modes    }
	kSRHasFeedbackHasListenModes = 1;							{  next allocated recognizer has feedback window and uses listening modes           }
	kSRNoFeedbackHasListenModes	= 2;							{  next allocated recognizer has no feedback window but does use listening modes   }

	{	 Speech Source Types 	}
	kSRDefaultSpeechSource		= 0;
	kSRLiveDesktopSpeechSource	= FourCharCode('dklv');						{  live desktop sound input  }
	kSRCanned22kHzSpeechSource	= FourCharCode('ca22');						{  AIFF file based 16 bit, 22.050 KHz sound input  }

	{	 Notification via Apple Event or Callback 	}
	{	 Notification Flags 	}
	kSRNotifyRecognitionBeginning = $00000001;					{  recognition can begin. client must now call SRContinueRecognition or SRCancelRecognition  }
	kSRNotifyRecognitionDone	= $00000002;					{  recognition has terminated. result (if any) is available.  }

	{	 Apple Event selectors 	}
	{	 AppleEvent message class  	}
	kAESpeechSuite				= FourCharCode('sprc');

	{	 AppleEvent message event ids 	}
	kAESpeechDone				= FourCharCode('srsd');
	kAESpeechDetected			= FourCharCode('srbd');

	{	 AppleEvent Parameter ids 	}
	keySRRecognizer				= FourCharCode('krec');
	keySRSpeechResult			= FourCharCode('kspr');
	keySRSpeechStatus			= FourCharCode('ksst');

	{	 AppleEvent Parameter types 	}
	typeSRRecognizer			= FourCharCode('trec');
	typeSRSpeechResult			= FourCharCode('tspr');


	{	 SRRecognizer Properties 	}
	kSRNotificationParam		= FourCharCode('noti');						{  see notification flags below  }
	kSRCallBackParam			= FourCharCode('call');						{  type SRCallBackParam  }
	kSRSearchStatusParam		= FourCharCode('stat');						{  see status flags below  }
	kSRAutoFinishingParam		= FourCharCode('afin');						{  automatic finishing applied on LM for search  }
	kSRForegroundOnly			= FourCharCode('fgon');						{  Boolean. Default is true. If true, client recognizer only active when in foreground.    }
	kSRBlockBackground			= FourCharCode('blbg');						{  Boolean. Default is false. If true, when client recognizer in foreground, rest of LMs are inactive.     }
	kSRBlockModally				= FourCharCode('blmd');						{  Boolean. Default is false. When true, this client's LM is only active LM; all other LMs are inactive. Be nice, don't be modal for long periods!  }
	kSRWantsResultTextDrawn		= FourCharCode('txfb');						{  Boolean. Default is true. If true, search results are posted to Feedback window  }
	kSRWantsAutoFBGestures		= FourCharCode('dfbr');						{  Boolean. Default is true. If true, client needn't call SRProcessBegin/End to get default feedback behavior  }
	kSRSoundInVolume			= FourCharCode('volu');						{  short in [0..100] log scaled sound input power. Can't set this property  }
	kSRReadAudioFSSpec			= FourCharCode('aurd');						{  *FSSpec. Specify FSSpec where raw audio is to be read (AIFF format) using kSRCanned22kHzSpeechSource. Reads until EOF  }
	kSRCancelOnSoundOut			= FourCharCode('caso');						{  Boolean: Default is true.  If any sound is played out during utterance, recognition is aborted.  }
	kSRSpeedVsAccuracyParam		= FourCharCode('sped');						{  SRSpeedSetting between 0 and 100  }

	{	 0 means more accurate but slower. 	}
	{	 100 means (much) less accurate but faster. 	}
	kSRUseToggleListen			= 0;							{  listen key modes  }
	kSRUsePushToTalk			= 1;

	kSRListenKeyMode			= FourCharCode('lkmd');						{  short: either kSRUseToggleListen or kSRUsePushToTalk  }
	kSRListenKeyCombo			= FourCharCode('lkey');						{  short: Push-To-Talk key combination; high byte is high byte of event->modifiers, the low byte is the keycode from event->message  }
	kSRListenKeyName			= FourCharCode('lnam');						{  Str63: string representing ListenKeyCombo  }
	kSRKeyWord					= FourCharCode('kwrd');						{  Str255: keyword preceding spoken commands in kSRUseToggleListen mode  }
	kSRKeyExpected				= FourCharCode('kexp');						{  Boolean: Must the PTT key be depressed or the key word spoken before recognition can occur?  }

	{	 Operational Status Flags 	}
	kSRIdleRecognizer			= $00000001;					{  engine is not active  }
	kSRSearchInProgress			= $00000002;					{  search is in progress  }
	kSRSearchWaitForAllClients	= $00000004;					{  search is suspended waiting on all clients' input  }
	kSRMustCancelSearch			= $00000008;					{  something has occurred (sound played, non-speech detected) requiring the search to abort  }
	kSRPendingSearch			= $00000010;					{  we're about to start searching  }

	{	 Recognition Result Properties 	}
	kSRTEXTFormat				= FourCharCode('TEXT');						{  raw text in user supplied memory  }
	kSRPhraseFormat				= FourCharCode('lmph');						{  SRPhrase containing result words  }
	kSRPathFormat				= FourCharCode('lmpt');						{  SRPath containing result phrases or words  }
	kSRLanguageModelFormat		= FourCharCode('lmfm');						{  top level SRLanguageModel for post parse  }

	{	 SRLanguageObject Family Properties 	}
	kSRSpelling					= FourCharCode('spel');						{  spelling of a SRWord or SRPhrase or SRPath, or name of a SRLanguageModel  }
	kSRLMObjType				= FourCharCode('lmtp');						{  Returns one of SRLanguageObject Types listed below  }
	kSRRefCon					= FourCharCode('refc');						{  4 bytes of user storage  }
	kSROptional					= FourCharCode('optl');						{  Boolean -- true if SRLanguageObject is optional     }
	kSREnabled					= FourCharCode('enbl');						{  Boolean -- true if SRLanguageObject enabled  }
	kSRRepeatable				= FourCharCode('rptb');						{  Boolean -- true if SRLanguageObject is repeatable  }
	kSRRejectable				= FourCharCode('rjbl');						{  Boolean -- true if SRLanguageObject is rejectable (Recognition System's kSRRejectedWord  }
																{        object can be returned in place of SRLanguageObject with this property)    }
	kSRRejectionLevel			= FourCharCode('rjct');						{  SRRejectionLevel between 0 and 100  }

	{	 LM Object Types -- returned as kSRLMObjType property of language model objects 	}
	kSRLanguageModelType		= FourCharCode('lmob');						{  SRLanguageModel  }
	kSRPathType					= FourCharCode('path');						{  SRPath  }
	kSRPhraseType				= FourCharCode('phra');						{  SRPhrase  }
	kSRWordType					= FourCharCode('word');						{  SRWord  }

	{	 a normal and reasonable rejection level 	}
	kSRDefaultRejectionLevel	= 50;

	{	******************************************************************************	}
	{	                      NOTES ON USING THE API                                  	}
	{	                                                                              	}
	{	      All operations (with the exception of SRGetRecognitionSystem) are       	}
	{	      directed toward an object allocated or begot from New, Get and Read     	}
	{	      type calls.                                                             	}
	{	                                                                              	}
	{	      There is a simple rule in dealing with allocation and disposal:         	}
	{	                                                                              	}
	{	      *   all toolbox allocations are obtained from a SRRecognitionSystem     	}
	{	                                                                              	}
	{	      *   if you obtain an object via New or Get, then you own a reference    	}
	{	          to that object and it must be released via SRReleaseObject when     	}
	{	          you no longer need it                                               	}
	{	                                                                              	}
	{	      *   when you receive a SRRecognitionResult object via AppleEvent or     	}
	{	          callback, it has essentially been created on your behalf and so     	}
	{	          you are responsible for releasing it as above                       	}
	{	                                                                              	}
	{	      *   when you close a SRRecognitionSystem, all remaining objects which       	}
	{	          were allocated with it will be forcefully released and any          	}
	{	          remaining references to those objects will be invalid.              	}
	{	                                                                              	}
	{	      This translates into a very simple guideline:                           	}
	{	          If you allocate it or have it allocated for you, you must release   	}
	{	          it.  If you are only peeking at it, then don't release it.          	}
	{	                                                                              	}
	{	******************************************************************************	}
	{	 Opening and Closing of the SRRecognitionSystem 	}
	{
	 *  SROpenRecognitionSystem()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function SROpenRecognitionSystem(var system: SRRecognitionSystem; systemID: OSType): OSErr; external name '_SROpenRecognitionSystem';
{
 *  SRCloseRecognitionSystem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRCloseRecognitionSystem(system: SRRecognitionSystem): OSErr; external name '_SRCloseRecognitionSystem';
{ Accessing Properties of any Speech Object }
{
 *  SRSetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSetProperty(srObject: SRSpeechObject; selector: OSType; proprty: UnivPtr; propertyLen: Size): OSErr; external name '_SRSetProperty';
{
 *  SRGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRGetProperty(srObject: SRSpeechObject; selector: OSType; proprty: UnivPtr; var propertyLen: Size): OSErr; external name '_SRGetProperty';
{ Any object obtained via New or Get type calls must be released }
{
 *  SRReleaseObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRReleaseObject(srObject: SRSpeechObject): OSErr; external name '_SRReleaseObject';
{
 *  SRGetReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRGetReference(srObject: SRSpeechObject; var newObjectRef: SRSpeechObject): OSErr; external name '_SRGetReference';
{ SRRecognizer Instance Functions }
{
 *  SRNewRecognizer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewRecognizer(system: SRRecognitionSystem; var recognizer: SRRecognizer; sourceID: OSType): OSErr; external name '_SRNewRecognizer';
{
 *  SRStartListening()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRStartListening(recognizer: SRRecognizer): OSErr; external name '_SRStartListening';
{
 *  SRStopListening()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRStopListening(recognizer: SRRecognizer): OSErr; external name '_SRStopListening';
{
 *  SRSetLanguageModel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSetLanguageModel(recognizer: SRRecognizer; languageModel: SRLanguageModel): OSErr; external name '_SRSetLanguageModel';
{
 *  SRGetLanguageModel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRGetLanguageModel(recognizer: SRRecognizer; var languageModel: SRLanguageModel): OSErr; external name '_SRGetLanguageModel';
{
 *  SRContinueRecognition()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRContinueRecognition(recognizer: SRRecognizer): OSErr; external name '_SRContinueRecognition';
{
 *  SRCancelRecognition()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRCancelRecognition(recognizer: SRRecognizer): OSErr; external name '_SRCancelRecognition';
{
 *  SRIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRIdle: OSErr; external name '_SRIdle';
{ Language Model Building and Manipulation Functions }
{
 *  SRNewLanguageModel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewLanguageModel(system: SRRecognitionSystem; var model: SRLanguageModel; name: UnivPtr; nameLength: Size): OSErr; external name '_SRNewLanguageModel';
{
 *  SRNewPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewPath(system: SRRecognitionSystem; var path: SRPath): OSErr; external name '_SRNewPath';
{
 *  SRNewPhrase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewPhrase(system: SRRecognitionSystem; var phrase: SRPhrase; text: UnivPtr; textLength: Size): OSErr; external name '_SRNewPhrase';
{
 *  SRNewWord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewWord(system: SRRecognitionSystem; var word: SRWord; text: UnivPtr; textLength: Size): OSErr; external name '_SRNewWord';
{ Operations on any object of the SRLanguageObject family }
{
 *  SRPutLanguageObjectIntoHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRPutLanguageObjectIntoHandle(languageObject: SRLanguageObject; lobjHandle: Handle): OSErr; external name '_SRPutLanguageObjectIntoHandle';
{
 *  SRPutLanguageObjectIntoDataFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRPutLanguageObjectIntoDataFile(languageObject: SRLanguageObject; fRefNum: SInt16): OSErr; external name '_SRPutLanguageObjectIntoDataFile';
{
 *  SRNewLanguageObjectFromHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewLanguageObjectFromHandle(system: SRRecognitionSystem; var languageObject: SRLanguageObject; lObjHandle: Handle): OSErr; external name '_SRNewLanguageObjectFromHandle';
{
 *  SRNewLanguageObjectFromDataFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRNewLanguageObjectFromDataFile(system: SRRecognitionSystem; var languageObject: SRLanguageObject; fRefNum: SInt16): OSErr; external name '_SRNewLanguageObjectFromDataFile';
{
 *  SREmptyLanguageObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SREmptyLanguageObject(languageObject: SRLanguageObject): OSErr; external name '_SREmptyLanguageObject';
{
 *  SRChangeLanguageObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRChangeLanguageObject(languageObject: SRLanguageObject; text: UnivPtr; textLength: Size): OSErr; external name '_SRChangeLanguageObject';
{
 *  SRAddLanguageObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRAddLanguageObject(base: SRLanguageObject; addon: SRLanguageObject): OSErr; external name '_SRAddLanguageObject';
{
 *  SRAddText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRAddText(base: SRLanguageObject; text: UnivPtr; textLength: Size; refCon: SInt32): OSErr; external name '_SRAddText';
{
 *  SRRemoveLanguageObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRRemoveLanguageObject(base: SRLanguageObject; toRemove: SRLanguageObject): OSErr; external name '_SRRemoveLanguageObject';
{ Traversing SRRecognitionResults or SRLanguageObjects }
{
 *  SRCountItems()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRCountItems(container: SRSpeechObject; var count: SInt32): OSErr; external name '_SRCountItems';
{
 *  SRGetIndexedItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRGetIndexedItem(container: SRSpeechObject; var item: SRSpeechObject; index: SInt32): OSErr; external name '_SRGetIndexedItem';
{
 *  SRSetIndexedItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSetIndexedItem(container: SRSpeechObject; item: SRSpeechObject; index: SInt32): OSErr; external name '_SRSetIndexedItem';
{
 *  SRRemoveIndexedItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRRemoveIndexedItem(container: SRSpeechObject; index: SInt32): OSErr; external name '_SRRemoveIndexedItem';
{ Utilizing the System Feedback Window }
{
 *  SRDrawText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRDrawText(recognizer: SRRecognizer; dispText: UnivPtr; dispLength: Size): OSErr; external name '_SRDrawText';
{
 *  SRDrawRecognizedText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRDrawRecognizedText(recognizer: SRRecognizer; dispText: UnivPtr; dispLength: Size): OSErr; external name '_SRDrawRecognizedText';
{
 *  SRSpeakText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSpeakText(recognizer: SRRecognizer; speakText: UnivPtr; speakLength: Size): OSErr; external name '_SRSpeakText';
{
 *  SRSpeakAndDrawText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSpeakAndDrawText(recognizer: SRRecognizer; text: UnivPtr; textLength: Size): OSErr; external name '_SRSpeakAndDrawText';
{
 *  SRStopSpeech()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRStopSpeech(recognizer: SRRecognizer): OSErr; external name '_SRStopSpeech';
{
 *  SRSpeechBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRSpeechBusy(recognizer: SRRecognizer): boolean; external name '_SRSpeechBusy';
{
 *  SRProcessBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRProcessBegin(recognizer: SRRecognizer; failed: boolean): OSErr; external name '_SRProcessBegin';
{
 *  SRProcessEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in SpeechRecognitionLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SRProcessEnd(recognizer: SRRecognizer; failed: boolean): OSErr; external name '_SRProcessEnd';
{$ALIGN MAC68K}


end.
