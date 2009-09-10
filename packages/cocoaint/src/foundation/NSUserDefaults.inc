{ Parsed from Foundation.framework NSUserDefaults.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSUSERDEFAULTS_PAS_H}
{$define NSUSERDEFAULTS_PAS_H}
type
  NSUserDefaultsPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSUSERDEFAULTS_PAS_T}
{$define NSUSERDEFAULTS_PAS_T}

{ CFString constants }
var
  NSGlobalDomain: CFStringRef; external name '_NSGlobalDomain';
  NSArgumentDomain: CFStringRef; external name '_NSArgumentDomain';
  NSRegistrationDomain: CFStringRef; external name '_NSRegistrationDomain';
  NSUserDefaultsDidChangeNotification: CFStringRef; external name '_NSUserDefaultsDidChangeNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSUSERDEFAULTS_PAS_R}
{$define NSUSERDEFAULTS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSUSERDEFAULTS_PAS_F}
{$define NSUSERDEFAULTS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSUSERDEFAULTS_PAS_T}
{$define NSUSERDEFAULTS_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSUserDefaults = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSUSERDEFAULTS_PAS_C}
{$define NSUSERDEFAULTS_PAS_C}

{ NSUserDefaults }
  NSUserDefaults = objcclass(NSObject)
  private
    __private: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSUserDefaults; message 'alloc';

    class function standardUserDefaults: NSUserDefaults; message 'standardUserDefaults';
    class procedure resetStandardUserDefaults; message 'resetStandardUserDefaults';
    function init: id; message 'init';
    function initWithUser(username: NSString): id; message 'initWithUser:';
    function objectForKey(defaultName: NSString): id; message 'objectForKey:';
    procedure setObject_forKey(value: id; defaultName: NSString); message 'setObject:forKey:';
    procedure removeObjectForKey(defaultName: NSString); message 'removeObjectForKey:';
    function stringForKey(defaultName: NSString): NSString; message 'stringForKey:';
    function arrayForKey(defaultName: NSString): NSArray; message 'arrayForKey:';
    function dictionaryForKey(defaultName: NSString): NSDictionary; message 'dictionaryForKey:';
    function dataForKey(defaultName: NSString): NSData; message 'dataForKey:';
    function stringArrayForKey(defaultName: NSString): NSArray; message 'stringArrayForKey:';
    function integerForKey(defaultName: NSString): clong; message 'integerForKey:';
    function floatForKey(defaultName: NSString): single; message 'floatForKey:';
    function doubleForKey(defaultName: NSString): double; message 'doubleForKey:';
    function boolForKey(defaultName: NSString): Boolean; message 'boolForKey:';
    procedure setInteger_forKey(value: clong; defaultName: NSString); message 'setInteger:forKey:';
    procedure setFloat_forKey(value: single; defaultName: NSString); message 'setFloat:forKey:';
    procedure setDouble_forKey(value: double; defaultName: NSString); message 'setDouble:forKey:';
    procedure setBool_forKey(value: Boolean; defaultName: NSString); message 'setBool:forKey:';
    procedure registerDefaults(registrationDictionary: NSDictionary); message 'registerDefaults:';
    procedure addSuiteNamed(suiteName: NSString); message 'addSuiteNamed:';
    procedure removeSuiteNamed(suiteName: NSString); message 'removeSuiteNamed:';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';
    function volatileDomainNames: NSArray; message 'volatileDomainNames';
    function volatileDomainForName(domainName: NSString): NSDictionary; message 'volatileDomainForName:';
    procedure setVolatileDomain_forName(domain: NSDictionary; domainName: NSString); message 'setVolatileDomain:forName:';
    procedure removeVolatileDomainForName(domainName: NSString); message 'removeVolatileDomainForName:';
    function persistentDomainNames: NSArray; message 'persistentDomainNames';
    function persistentDomainForName(domainName: NSString): NSDictionary; message 'persistentDomainForName:';
    procedure setPersistentDomain_forName(domain: NSDictionary; domainName: NSString); message 'setPersistentDomain:forName:';
    procedure removePersistentDomainForName(domainName: NSString); message 'removePersistentDomainForName:';
    function synchronize: Boolean; message 'synchronize';
    function objectIsForcedForKey(key: NSString): Boolean; message 'objectIsForcedForKey:';
    function objectIsForcedForKey_inDomain(key: NSString; domain: NSString): Boolean; message 'objectIsForcedForKey:inDomain:';
  end; external;

{$endif}
{$endif}
