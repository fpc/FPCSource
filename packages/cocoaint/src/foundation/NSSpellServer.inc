{ Parsed from Foundation.framework NSSpellServer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSPELLSERVER_PAS_T}
{$define NSSPELLSERVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPELLSERVER_PAS_R}
{$define NSSPELLSERVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPELLSERVER_PAS_F}
{$define NSSPELLSERVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPELLSERVER_PAS_S}
{$define NSSPELLSERVER_PAS_S}

{ External string constants }
var
  NSGrammarRange: NSString; external name '_NSGrammarRange';
  NSGrammarUserDescription: NSString; external name '_NSGrammarUserDescription';
  NSGrammarCorrections: NSString; external name '_NSGrammarCorrections';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpellServer = objcclass;
  NSSpellServerPointer = ^NSSpellServer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPELLSERVER_PAS_C}
{$define NSSPELLSERVER_PAS_C}

{ NSSpellServer }
  NSSpellServer = objcclass(NSObject)
  private
    __delegate: id;
    __caseSensitive: NSInteger;
    __spellServerConnection: id;
    __dictionaries: id;
    __learnedDictionaries: NSArray;
    __ssFlags: bitpacked record
        delegateLearnsWords: 0..1;
        delegateForgetsWords: 0..1;
        busy: 0..1;
        _reserved: 0..((1 shl 29)-1);
      end;
    __reservedSpellServer1: Pointer;
    __reservedSpellServer2: Pointer;
    
  public
    class function alloc: NSSpellServer; message 'alloc';

    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function registerLanguage_byVendor(language: NSString; vendor: NSString): Boolean; message 'registerLanguage:byVendor:';
    function isWordInUserDictionaries_caseSensitive(word: NSString; flag: Boolean): Boolean; message 'isWordInUserDictionaries:caseSensitive:';
    procedure run; message 'run';
  end; external;

{$endif}
{$endif}
