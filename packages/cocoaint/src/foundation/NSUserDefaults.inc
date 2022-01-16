{ Parsed from Foundation.framework NSUserDefaults.h }


{$ifdef TYPES}
type
  NSUserDefaultsPtr = ^NSUserDefaults;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSGlobalDomain: NSString; cvar; external;
  NSArgumentDomain: NSString; cvar; external;
  NSRegistrationDomain: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSUserDefaults = objcclass external (NSObject)
  private
    _unused: NSInteger;
    _reserved:array[0..3] of pointer;
  public
    class function standardUserDefaults: NSUserDefaults; message 'standardUserDefaults';
    class procedure resetStandardUserDefaults; message 'resetStandardUserDefaults';
    function init: instancetype; message 'init';
    function initWithSuiteName (suitename: NSString): instancetype; message 'initWithSuiteName:'; { available in 10_9, 7_0 }
    function initWithUser (username: NSString): id; message 'initWithUser:'; deprecated 'in 10_0, 10_9, 2_0, 7_0';
    function objectForKey (defaultName: NSString): id; message 'objectForKey:';
    procedure setObject_forKey (value: id; defaultName: NSString); message 'setObject:forKey:';
    procedure removeObjectForKey (defaultName: NSString); message 'removeObjectForKey:';
    function stringForKey (defaultName: NSString): NSString; message 'stringForKey:';
    function arrayForKey (defaultName: NSString): NSArray; message 'arrayForKey:';
    function dictionaryForKey (defaultName: NSString): NSDictionary; message 'dictionaryForKey:';
    function dataForKey (defaultName: NSString): NSData; message 'dataForKey:';
    function stringArrayForKey (defaultName: NSString): NSArray; message 'stringArrayForKey:';
    function integerForKey (defaultName: NSString): NSInteger; message 'integerForKey:';
    function floatForKey (defaultName: NSString): single; message 'floatForKey:';
    function doubleForKey (defaultName: NSString): double; message 'doubleForKey:';
    function boolForKey (defaultName: NSString): ObjCBOOL; message 'boolForKey:';
    function URLForKey (defaultName: NSString): NSURL; message 'URLForKey:'; { available in 10_6, 4_0 }
    procedure setInteger_forKey (value: NSInteger; defaultName: NSString); message 'setInteger:forKey:';
    procedure setFloat_forKey (value: single; defaultName: NSString); message 'setFloat:forKey:';
    procedure setDouble_forKey (value: double; defaultName: NSString); message 'setDouble:forKey:';
    procedure setBool_forKey (value: ObjCBOOL; defaultName: NSString); message 'setBool:forKey:';
    procedure setURL_forKey (url: NSURL; defaultName: NSString); message 'setURL:forKey:'; { available in 10_6, 4_0 }
    procedure registerDefaults (registrationDictionary: NSDictionary); message 'registerDefaults:';
    procedure addSuiteNamed (suiteName: NSString); message 'addSuiteNamed:';
    procedure removeSuiteNamed (suiteName: NSString); message 'removeSuiteNamed:';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';
    function DomainNames: NSArray; message 'DomainNames';
    function volatileDomainForName (domainName: NSString): NSDictionary; message 'volatileDomainForName:';
    procedure setVolatileDomain_forName (domain: NSDictionary; domainName: NSString); message 'setVolatileDomain:forName:';
    procedure removeVolatileDomainForName (domainName: NSString); message 'removeVolatileDomainForName:';
    function persistentDomainNames: NSArray; message 'persistentDomainNames'; deprecated 'in 10_0, 10_9, 2_0, 7_0';
    function persistentDomainForName (domainName: NSString): NSDictionary; message 'persistentDomainForName:';
    procedure setPersistentDomain_forName (domain: NSDictionary; domainName: NSString); message 'setPersistentDomain:forName:';
    procedure removePersistentDomainForName (domainName: NSString); message 'removePersistentDomainForName:';
    function synchronize: ObjCBOOL; message 'synchronize';
    function objectIsForcedForKey (key: NSString): ObjCBOOL; message 'objectIsForcedForKey:';
    function objectIsForcedForKey_inDomain (key: NSString; domain: NSString): ObjCBOOL; message 'objectIsForcedForKey:inDomain:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUserDefaultsDidChangeNotification: NSString; cvar; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE))) or defined(TARGET_OS_WIN32)}
{$ifdef EXTERNAL_SYMBOLS}
var
  NSWeekDayNameArray: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSShortWeekDayNameArray: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSMonthNameArray: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSShortMonthNameArray: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSTimeFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSDateFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSTimeDateFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSShortTimeDateFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSCurrencySymbol: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSDecimalSeparator: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSThousandsSeparator: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSDecimalDigits: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSAMPMDesignation: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSHourNameDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSYearMonthWeekDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSEarlierTimeDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSLaterTimeDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSThisDayDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSNextDayDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSNextNextDayDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSPriorDayDesignations: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSDateTimeOrdering: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSInternationalCurrencyString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSShortDateFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSPositiveCurrencyFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
  NSNegativeCurrencyFormatString: NSString deprecated 'in 10_0, 10_5, NA, NA'; cvar; external;
{$endif}

{$endif}
