{ Parsed from AppKit.framework NSSpeechSynthesizer.h }


{$ifdef TYPES}
type
  NSSpeechSynthesizerPtr = ^NSSpeechSynthesizer;
  NSSpeechSynthesizerDelegateProtocolPtr = ^NSSpeechSynthesizerDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSVoiceName: NSString; cvar; external;
  NSVoiceIdentifier: NSString; cvar; external;
  NSVoiceAge: NSString; cvar; external;
  NSVoiceGender: NSString; cvar; external;
  NSVoiceDemoText: NSString; cvar; external;
  NSVoiceLocaleIdentifier: NSString { available in 10_5 }; cvar; external;
  NSVoiceSupportedCharacters: NSString { available in 10_5 }; cvar; external;
  NSVoiceIndividuallySpokenCharacters: NSString { available in 10_5 }; cvar; external;
  NSVoiceGenderNeuter: NSString; cvar; external;
  NSVoiceGenderMale: NSString; cvar; external;
  NSVoiceGenderFemale: NSString; cvar; external;
  NSVoiceLanguage: NSString deprecated 'in 10_0, 10_5'; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSSpeechBoundary = NSUInteger;
  NSSpeechBoundaryPtr = ^NSSpeechBoundary;

const
  NSSpeechImmediateBoundary = 0;
  NSSpeechWordBoundary = 1;
  NSSpeechSentenceBoundary = 2;
{$endif}

{$ifdef CLASSES}

type
  NSSpeechSynthesizer = objcclass external (NSObject)
  private
    _privateNSSpeechSynthesizerVars: id;
  public
    function initWithVoice (voice: NSString): instancetype; message 'initWithVoice:';
    function startSpeakingString (string_: NSString): ObjCBOOL; message 'startSpeakingString:';
    function startSpeakingString_toURL (string_: NSString; url: NSURL): ObjCBOOL; message 'startSpeakingString:toURL:';
    function isSpeaking: ObjCBOOL; message 'isSpeaking';
    procedure stopSpeaking; message 'stopSpeaking';
    procedure stopSpeakingAtBoundary (boundary: NSSpeechBoundary); message 'stopSpeakingAtBoundary:'; { available in 10_5 }
    procedure pauseSpeakingAtBoundary (boundary: NSSpeechBoundary); message 'pauseSpeakingAtBoundary:'; { available in 10_5 }
    procedure continueSpeaking; message 'continueSpeaking'; { available in 10_5 }
    procedure setDelegate(newValue: NSSpeechSynthesizerDelegateProtocol); message 'setDelegate:';
    function delegate: NSSpeechSynthesizerDelegateProtocol; message 'delegate';
    function voice: NSString; message 'voice';
    function setVoice (voice_: NSString): ObjCBOOL; message 'setVoice:';
    procedure setRate(newValue: single); message 'setRate:';
    function rate: single; message 'rate';
    procedure setVolume(newValue: single); message 'setVolume:';
    function volume: single; message 'volume';
    procedure setUsesFeedbackWindow(newValue: ObjCBOOL); message 'setUsesFeedbackWindow:';
    function usesFeedbackWindow: ObjCBOOL; message 'usesFeedbackWindow';
    procedure addSpeechDictionary (speechDictionary: NSDictionary); message 'addSpeechDictionary:'; { available in 10_5 }
    function phonemesFromText (text: NSString): NSString; message 'phonemesFromText:'; { available in 10_5 }
    function objectForProperty_error (property_: NSString; outError: NSErrorPtr): id; message 'objectForProperty:error:'; { available in 10_5 }
    function setObject_forProperty_error (object_: id; property_: NSString; outError: NSErrorPtr): ObjCBOOL; message 'setObject:forProperty:error:'; { available in 10_5 }
    class function isAnyApplicationSpeaking: ObjCBOOL; message 'isAnyApplicationSpeaking';
    class function defaultVoice: NSString; message 'defaultVoice';
    class function availableVoices: NSArray; message 'availableVoices';
    class function attributesForVoice (voice_: NSString): NSDictionary; message 'attributesForVoice:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSpeechSynthesizerDelegateProtocol = objcprotocol external name 'NSSpeechSynthesizerDelegate' (NSObjectProtocol)
  optional
    procedure speechSynthesizer_didFinishSpeaking (sender: NSSpeechSynthesizer; finishedSpeaking: ObjCBOOL); message 'speechSynthesizer:didFinishSpeaking:';
    procedure speechSynthesizer_willSpeakWord_ofString (sender: NSSpeechSynthesizer; characterRange: NSRange; string_: NSString); message 'speechSynthesizer:willSpeakWord:ofString:';
    procedure speechSynthesizer_willSpeakPhoneme (sender: NSSpeechSynthesizer; phonemeOpcode: cshort); message 'speechSynthesizer:willSpeakPhoneme:';
    procedure speechSynthesizer_didEncounterErrorAtIndex_ofString_message (sender: NSSpeechSynthesizer; characterIndex: NSUInteger; string_: NSString; message_: NSString); message 'speechSynthesizer:didEncounterErrorAtIndex:ofString:message:'; { available in 10_5 }
    procedure speechSynthesizer_didEncounterSyncMessage (sender: NSSpeechSynthesizer; message_: NSString); message 'speechSynthesizer:didEncounterSyncMessage:'; { available in 10_5 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSpeechStatusProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorsProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechInputModeProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechCharacterModeProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechNumberModeProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechRateProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechPitchBaseProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechPitchModProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechVolumeProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechSynthesizerInfoProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechRecentSyncProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeSymbolsProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechCurrentVoiceProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechCommandDelimiterProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechResetProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechOutputToFileURLProperty: NSString { available in 10_5 }; cvar; external;
  NSSpeechModeText: NSString { available in 10_5 }; cvar; external;
  NSSpeechModePhoneme: NSString { available in 10_5 }; cvar; external;
  NSSpeechModeNormal: NSString { available in 10_5 }; cvar; external;
  NSSpeechModeLiteral: NSString { available in 10_5 }; cvar; external;
  NSSpeechStatusOutputBusy: NSString { available in 10_5 }; cvar; external;
  NSSpeechStatusOutputPaused: NSString { available in 10_5 }; cvar; external;
  NSSpeechStatusNumberOfCharactersLeft: NSString { available in 10_5 }; cvar; external;
  NSSpeechStatusPhonemeCode: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorCount: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorOldestCode: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorOldestCharacterOffset: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorNewestCode: NSString { available in 10_5 }; cvar; external;
  NSSpeechErrorNewestCharacterOffset: NSString { available in 10_5 }; cvar; external;
  NSSpeechSynthesizerInfoIdentifier: NSString { available in 10_5 }; cvar; external;
  NSSpeechSynthesizerInfoVersion: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeInfoOpcode: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeInfoSymbol: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeInfoExample: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeInfoHiliteStart: NSString { available in 10_5 }; cvar; external;
  NSSpeechPhonemeInfoHiliteEnd: NSString { available in 10_5 }; cvar; external;
  NSSpeechCommandPrefix: NSString { available in 10_5 }; cvar; external;
  NSSpeechCommandSuffix: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryLocaleIdentifier: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryModificationDate: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryPronunciations: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryAbbreviations: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryEntrySpelling: NSString { available in 10_5 }; cvar; external;
  NSSpeechDictionaryEntryPhonemes: NSString { available in 10_5 }; cvar; external;
{$endif}

