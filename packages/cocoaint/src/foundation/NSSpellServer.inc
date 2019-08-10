{ Parsed from Foundation.framework NSSpellServer.h }


{$ifdef TYPES}
type
  NSSpellServerPtr = ^NSSpellServer;
  NSSpellServerDelegateProtocolPtr = ^NSSpellServerDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSSpellServer = objcclass external (NSObject)
  private
    _delegate: id;
    _caseSensitive: NSInteger;
    _spellServerConnection: id;
    _dictionaries: id;
    _learnedDictionaries: NSArray;
    __ssFlags: bitpacked record
      case byte of
        0: (_anonBitField___ssFlags0: cuint);
        1: (
          delegateLearnsWords: 0..1;
          delegateForgetsWords: 0..1;
          busy: 0..1;
          _reserved: 0..((1 shl 29)-1);
        );
      end;
    _checker: id;
    _reservedSpellServer: pointer;
  public
    procedure setDelegate(newValue: NSSpellServerDelegateProtocol); message 'setDelegate:';
    function delegate: NSSpellServerDelegateProtocol; message 'delegate';
    function registerLanguage_byVendor (language: NSString; vendor: NSString): ObjCBOOL; message 'registerLanguage:byVendor:';
    function isWordInUserDictionaries_caseSensitive (word: NSString; flag: ObjCBOOL): ObjCBOOL; message 'isWordInUserDictionaries:caseSensitive:';
    procedure run; message 'run';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSpellServerDelegateProtocol = objcprotocol external name 'NSSpellServerDelegate' (NSObjectProtocol)
  optional
    function spellServer_findMisspelledWordInString_language_wordCount_countOnly (sender: NSSpellServer; stringToCheck: NSString; language: NSString; wordCount: NSIntegerPtr; countOnly: ObjCBOOL): NSRange; message 'spellServer:findMisspelledWordInString:language:wordCount:countOnly:';
    function spellServer_suggestGuessesForWord_inLanguage (sender: NSSpellServer; word: NSString; language: NSString): NSArray; message 'spellServer:suggestGuessesForWord:inLanguage:';
    procedure spellServer_didLearnWord_inLanguage (sender: NSSpellServer; word: NSString; language: NSString); message 'spellServer:didLearnWord:inLanguage:';
    procedure spellServer_didForgetWord_inLanguage (sender: NSSpellServer; word: NSString; language: NSString); message 'spellServer:didForgetWord:inLanguage:';
    function spellServer_suggestCompletionsForPartialWordRange_inString_language (sender: NSSpellServer; range: NSRange; string_: NSString; language: NSString): NSArray; message 'spellServer:suggestCompletionsForPartialWordRange:inString:language:';
    function spellServer_checkGrammarInString_language_details (sender: NSSpellServer; stringToCheck: NSString; language: NSString; details: NSArrayPtr): NSRange; message 'spellServer:checkGrammarInString:language:details:'; { available in 10_5, NA }
    function spellServer_checkString_offset_types_options_orthography_wordCount (sender: NSSpellServer; stringToCheck: NSString; offset: NSUInteger; checkingTypes: NSTextCheckingTypes; options: NSDictionary; orthography: NSOrthography; wordCount: NSIntegerPtr): NSArray; message 'spellServer:checkString:offset:types:options:orthography:wordCount:'; { available in 10_6, NA }
    procedure spellServer_recordResponse_toCorrection_forWord_language (sender: NSSpellServer; response: NSUInteger; correction: NSString; word: NSString; language: NSString); message 'spellServer:recordResponse:toCorrection:forWord:language:'; { available in 10_7, NA }
  end;
{$endif}

