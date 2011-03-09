{ Parsed from Appkit.framework NSSpeechSynthesizer.h }

{$ifdef TYPES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_T}
{$define NSSPEECHSYNTHESIZER_PAS_T}

{ Constants }

const
  NSSpeechImmediateBoundary = 0;
  NSSpeechWordBoundary = 1;
  NSSpeechSentenceBoundary = 2;

{ Types }
type
  NSSpeechBoundary = NSUInteger;
  NSSpeechBoundaryPtr = ^NSSpeechBoundary;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_R}
{$define NSSPEECHSYNTHESIZER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_F}
{$define NSSPEECHSYNTHESIZER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_S}
{$define NSSPEECHSYNTHESIZER_PAS_S}

{ External string constants }
var
  NSVoiceName: NSString; cvar; external;
  NSVoiceIdentifier: NSString; cvar; external;
  NSVoiceAge: NSString; cvar; external;
  NSVoiceGender: NSString; cvar; external;
  NSVoiceDemoText: NSString; cvar; external;
  NSVoiceLocaleIdentifier: NSString; cvar; external;
  NSVoiceSupportedCharacters: NSString; cvar; external;
  NSVoiceIndividuallySpokenCharacters: NSString; cvar; external;
  NSVoiceGenderNeuter: NSString; cvar; external;
  NSVoiceGenderMale: NSString; cvar; external;
  NSVoiceGenderFemale: NSString; cvar; external;
  NSVoiceLanguage: NSString deprecated 'in Mac OS X 10.5 and later'; cvar; external;
  NSSpeechStatusProperty: NSString; cvar; external;
  NSSpeechErrorsProperty: NSString; cvar; external;
  NSSpeechInputModeProperty: NSString; cvar; external;
  NSSpeechCharacterModeProperty: NSString; cvar; external;
  NSSpeechNumberModeProperty: NSString; cvar; external;
  NSSpeechRateProperty: NSString; cvar; external;
  NSSpeechPitchBaseProperty: NSString; cvar; external;
  NSSpeechPitchModProperty: NSString; cvar; external;
  NSSpeechVolumeProperty: NSString; cvar; external;
  NSSpeechSynthesizerInfoProperty: NSString; cvar; external;
  NSSpeechRecentSyncProperty: NSString; cvar; external;
  NSSpeechPhonemeSymbolsProperty: NSString; cvar; external;
  NSSpeechCurrentVoiceProperty: NSString; cvar; external;
  NSSpeechCommandDelimiterProperty: NSString; cvar; external;
  NSSpeechResetProperty: NSString; cvar; external;
  NSSpeechOutputToFileURLProperty: NSString; cvar; external;
  NSSpeechModeText: NSString; cvar; external;
  NSSpeechModePhoneme: NSString; cvar; external;
  NSSpeechModeNormal: NSString; cvar; external;
  NSSpeechModeLiteral: NSString; cvar; external;
  NSSpeechStatusOutputBusy: NSString; cvar; external;
  NSSpeechStatusOutputPaused: NSString; cvar; external;
  NSSpeechStatusNumberOfCharactersLeft: NSString; cvar; external;
  NSSpeechStatusPhonemeCode: NSString; cvar; external;
  NSSpeechErrorCount: NSString; cvar; external;
  NSSpeechErrorOldestCode: NSString; cvar; external;
  NSSpeechErrorOldestCharacterOffset: NSString; cvar; external;
  NSSpeechErrorNewestCode: NSString; cvar; external;
  NSSpeechErrorNewestCharacterOffset: NSString; cvar; external;
  NSSpeechSynthesizerInfoIdentifier: NSString; cvar; external;
  NSSpeechSynthesizerInfoVersion: NSString; cvar; external;
  NSSpeechPhonemeInfoOpcode: NSString; cvar; external;
  NSSpeechPhonemeInfoSymbol: NSString; cvar; external;
  NSSpeechPhonemeInfoExample: NSString; cvar; external;
  NSSpeechPhonemeInfoHiliteStart: NSString; cvar; external;
  NSSpeechPhonemeInfoHiliteEnd: NSString; cvar; external;
  NSSpeechCommandPrefix: NSString; cvar; external;
  NSSpeechCommandSuffix: NSString; cvar; external;
  NSSpeechDictionaryLocaleIdentifier: NSString; cvar; external;
  NSSpeechDictionaryModificationDate: NSString; cvar; external;
  NSSpeechDictionaryPronunciations: NSString; cvar; external;
  NSSpeechDictionaryAbbreviations: NSString; cvar; external;
  NSSpeechDictionaryEntrySpelling: NSString; cvar; external;
  NSSpeechDictionaryEntryPhonemes: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpeechSynthesizerDelegateProtocol = objcprotocol;
  NSSpeechSynthesizer = objcclass;
  NSSpeechSynthesizerPointer = ^NSSpeechSynthesizer;
  NSSpeechSynthesizerPtr = NSSpeechSynthesizerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_C}
{$define NSSPEECHSYNTHESIZER_PAS_C}

{ NSSpeechSynthesizer }
  NSSpeechSynthesizer = objcclass external (NSObject)
  private
    _privateNSSpeechSynthesizerVars: id;
    
  public
    function initWithVoice(voice_: NSString): id; message 'initWithVoice:';
    function startSpeakingString(string_: NSString): Boolean; message 'startSpeakingString:';
    function startSpeakingString_toURL(string_: NSString; url: NSURL): Boolean; message 'startSpeakingString:toURL:';
    function isSpeaking: Boolean; message 'isSpeaking';
    procedure stopSpeaking; message 'stopSpeaking';
    procedure stopSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'stopSpeakingAtBoundary:';
    procedure pauseSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'pauseSpeakingAtBoundary:';
    procedure continueSpeaking; message 'continueSpeaking';
    function delegate: NSSpeechSynthesizerDelegateProtocol; message 'delegate';
    procedure setDelegate(anObject: NSSpeechSynthesizerDelegateProtocol); message 'setDelegate:';
    function voice: NSString; message 'voice';
    function setVoice(voice_: NSString): Boolean; message 'setVoice:';
    function rate: single; message 'rate';
    procedure setRate(rate_: single); message 'setRate:';
    function volume: single; message 'volume';
    procedure setVolume(volume_: single); message 'setVolume:';
    function usesFeedbackWindow: Boolean; message 'usesFeedbackWindow';
    procedure setUsesFeedbackWindow(flag: Boolean); message 'setUsesFeedbackWindow:';
    procedure addSpeechDictionary(speechDictionary: NSDictionary); message 'addSpeechDictionary:';
    function phonemesFromText(text: NSString): NSString; message 'phonemesFromText:';
    function objectForProperty_error(property_: NSString; outError: NSErrorPointer): id; message 'objectForProperty:error:';
    function setObject_forProperty_error(object_: id; property_: NSString; outError: NSErrorPointer): Boolean; message 'setObject:forProperty:error:';
    class function isAnyApplicationSpeaking: Boolean; message 'isAnyApplicationSpeaking';
    class function defaultVoice: NSString; message 'defaultVoice';
    class function availableVoices: NSArray; message 'availableVoices';
    class function attributesForVoice(voice_: NSString): NSDictionary; message 'attributesForVoice:';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_P}
{$define NSSPEECHSYNTHESIZER_PAS_P}
  
{ NSSpeechSynthesizerDelegate Protocol }
  NSSpeechSynthesizerDelegateProtocol = objcprotocol external name 'NSSpeechSynthesizerDelegate'
  optional
    procedure speechSynthesizer_didFinishSpeaking(sender: NSSpeechSynthesizer; finishedSpeaking: Boolean); message 'speechSynthesizer:didFinishSpeaking:';
    procedure speechSynthesizer_willSpeakWord_ofString(sender: NSSpeechSynthesizer; characterRange: NSRange; string_: NSString); message 'speechSynthesizer:willSpeakWord:ofString:';
    procedure speechSynthesizer_willSpeakPhoneme(sender: NSSpeechSynthesizer; phonemeOpcode: cshort); message 'speechSynthesizer:willSpeakPhoneme:';
    procedure speechSynthesizer_didEncounterErrorAtIndex_ofString_message(sender: NSSpeechSynthesizer; characterIndex: NSUInteger; string_: NSString; message: NSString); message 'speechSynthesizer:didEncounterErrorAtIndex:ofString:message:';
    procedure speechSynthesizer_didEncounterSyncMessage(sender: NSSpeechSynthesizer; message: NSString); message 'speechSynthesizer:didEncounterSyncMessage:';
  end;
{$endif}
{$endif}
