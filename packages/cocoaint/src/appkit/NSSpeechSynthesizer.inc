{ Parsed from Appkit.framework NSSpeechSynthesizer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSPEECHSYNTHESIZER_PAS_H}
{$define NSSPEECHSYNTHESIZER_PAS_H}
type
  NSSpeechSynthesizerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_T}
{$define NSSPEECHSYNTHESIZER_PAS_T}

{ Constants }

const
  NSSpeechImmediateBoundary = 0;
  NSSpeechWordBoundary = 0;
  NSSpeechSentenceBoundary = 1;

{ Types }
type
  NSSpeechBoundary = NSUInteger;

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
  NSVoiceName: NSString; external name '_NSVoiceName';
  NSVoiceIdentifier: NSString; external name '_NSVoiceIdentifier';
  NSVoiceAge: NSString; external name '_NSVoiceAge';
  NSVoiceGender: NSString; external name '_NSVoiceGender';
  NSVoiceDemoText: NSString; external name '_NSVoiceDemoText';
  NSVoiceLocaleIdentifier: NSString; external name '_NSVoiceLocaleIdentifier';
  NSVoiceSupportedCharacters: NSString; external name '_NSVoiceSupportedCharacters';
  NSVoiceIndividuallySpokenCharacters: NSString; external name '_NSVoiceIndividuallySpokenCharacters';
  NSVoiceGenderNeuter: NSString; external name '_NSVoiceGenderNeuter';
  NSVoiceGenderMale: NSString; external name '_NSVoiceGenderMale';
  NSVoiceGenderFemale: NSString; external name '_NSVoiceGenderFemale';
  NSSpeechStatusProperty: NSString; external name '_NSSpeechStatusProperty';
  NSSpeechErrorsProperty: NSString; external name '_NSSpeechErrorsProperty';
  NSSpeechInputModeProperty: NSString; external name '_NSSpeechInputModeProperty';
  NSSpeechCharacterModeProperty: NSString; external name '_NSSpeechCharacterModeProperty';
  NSSpeechNumberModeProperty: NSString; external name '_NSSpeechNumberModeProperty';
  NSSpeechRateProperty: NSString; external name '_NSSpeechRateProperty';
  NSSpeechPitchBaseProperty: NSString; external name '_NSSpeechPitchBaseProperty';
  NSSpeechPitchModProperty: NSString; external name '_NSSpeechPitchModProperty';
  NSSpeechVolumeProperty: NSString; external name '_NSSpeechVolumeProperty';
  NSSpeechSynthesizerInfoProperty: NSString; external name '_NSSpeechSynthesizerInfoProperty';
  NSSpeechRecentSyncProperty: NSString; external name '_NSSpeechRecentSyncProperty';
  NSSpeechPhonemeSymbolsProperty: NSString; external name '_NSSpeechPhonemeSymbolsProperty';
  NSSpeechCurrentVoiceProperty: NSString; external name '_NSSpeechCurrentVoiceProperty';
  NSSpeechCommandDelimiterProperty: NSString; external name '_NSSpeechCommandDelimiterProperty';
  NSSpeechResetProperty: NSString; external name '_NSSpeechResetProperty';
  NSSpeechOutputToFileURLProperty: NSString; external name '_NSSpeechOutputToFileURLProperty';
  NSSpeechModeText: NSString; external name '_NSSpeechModeText';
  NSSpeechModePhoneme: NSString; external name '_NSSpeechModePhoneme';
  NSSpeechModeNormal: NSString; external name '_NSSpeechModeNormal';
  NSSpeechModeLiteral: NSString; external name '_NSSpeechModeLiteral';
  NSSpeechStatusOutputBusy: NSString; external name '_NSSpeechStatusOutputBusy';
  NSSpeechStatusOutputPaused: NSString; external name '_NSSpeechStatusOutputPaused';
  NSSpeechStatusNumberOfCharactersLeft: NSString; external name '_NSSpeechStatusNumberOfCharactersLeft';
  NSSpeechStatusPhonemeCode: NSString; external name '_NSSpeechStatusPhonemeCode';
  NSSpeechErrorCount: NSString; external name '_NSSpeechErrorCount';
  NSSpeechErrorOldestCode: NSString; external name '_NSSpeechErrorOldestCode';
  NSSpeechErrorOldestCharacterOffset: NSString; external name '_NSSpeechErrorOldestCharacterOffset';
  NSSpeechErrorNewestCode: NSString; external name '_NSSpeechErrorNewestCode';
  NSSpeechErrorNewestCharacterOffset: NSString; external name '_NSSpeechErrorNewestCharacterOffset';
  NSSpeechSynthesizerInfoIdentifier: NSString; external name '_NSSpeechSynthesizerInfoIdentifier';
  NSSpeechSynthesizerInfoVersion: NSString; external name '_NSSpeechSynthesizerInfoVersion';
  NSSpeechPhonemeInfoOpcode: NSString; external name '_NSSpeechPhonemeInfoOpcode';
  NSSpeechPhonemeInfoSymbol: NSString; external name '_NSSpeechPhonemeInfoSymbol';
  NSSpeechPhonemeInfoExample: NSString; external name '_NSSpeechPhonemeInfoExample';
  NSSpeechPhonemeInfoHiliteStart: NSString; external name '_NSSpeechPhonemeInfoHiliteStart';
  NSSpeechPhonemeInfoHiliteEnd: NSString; external name '_NSSpeechPhonemeInfoHiliteEnd';
  NSSpeechCommandPrefix: NSString; external name '_NSSpeechCommandPrefix';
  NSSpeechCommandSuffix: NSString; external name '_NSSpeechCommandSuffix';
  NSSpeechDictionaryLocaleIdentifier: NSString; external name '_NSSpeechDictionaryLocaleIdentifier';
  NSSpeechDictionaryModificationDate: NSString; external name '_NSSpeechDictionaryModificationDate';
  NSSpeechDictionaryPronunciations: NSString; external name '_NSSpeechDictionaryPronunciations';
  NSSpeechDictionaryAbbreviations: NSString; external name '_NSSpeechDictionaryAbbreviations';
  NSSpeechDictionaryEntrySpelling: NSString; external name '_NSSpeechDictionaryEntrySpelling';
  NSSpeechDictionaryEntryPhonemes: NSString; external name '_NSSpeechDictionaryEntryPhonemes';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpeechSynthesizer = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_C}
{$define NSSPEECHSYNTHESIZER_PAS_C}

{ NSSpeechSynthesizer }
  NSSpeechSynthesizer = objcclass(NSObject)
  private
    __privateNSSpeechSynthesizerVars: id;
    
  public
    class function alloc: NSSpeechSynthesizer; message 'alloc';

    function initWithVoice(voice_: NSString): id; message 'initWithVoice:';
    function startSpeakingString(string_: NSString): Boolean; message 'startSpeakingString:';
    function startSpeakingString_toURL(string_: NSString; url: NSURL): Boolean; message 'startSpeakingString:toURL:';
    function isSpeaking: Boolean; message 'isSpeaking';
    procedure stopSpeaking; message 'stopSpeaking';
    procedure stopSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'stopSpeakingAtBoundary:';
    procedure pauseSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'pauseSpeakingAtBoundary:';
    procedure continueSpeaking; message 'continueSpeaking';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
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
  end; external;

{$endif}
{$endif}
