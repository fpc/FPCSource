{ Parsed from AddressBook.framework ABAddressBook.h }


{$ifdef TYPES}
type
  ABAddressBookPtr = ^ABAddressBook;
{$endif}

{$ifdef TYPES}
const
  ABAddRecordsError = 1001;
  ABRemoveRecordsError = 1002;
  ABPropertyValueValidationError = 1012;
  ABPropertyUnsupportedBySourceError = 1013;
  ABPropertyReadOnlyError = 1014;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ABAddressBookErrorDomain: NSString; cvar; external;
  ABMultiValueIdentifiersErrorKey: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  ABAddressBook = objcclass external (NSObject)
  private
    _reserved8: id;
    _reserved2: pointer;
    _reserved3: pointer;
    _reserved4: id;
    _tableSchemas: NSMutableDictionary;
    _reserved5: NSMutableDictionary;
    _converterPort: pointer;
    _inactivityTimer: NSTimer;
    _reserved: id;
    _reserved1: id;
    _reserved6: pointer;
    _reserved7: pointer;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          hasUnsavedChanges_: 0..1;
          readOnly: 0..1;
          importMe: 0..1;
          needConversion: 0..1;
          cleanedUp: 0..1;
          importTips: 0..1;
          restoreFromMetaData: 0..1;
          prefsNeedSync: 0..1;
          waitingForReset: 0..1;
          enforcesConstraints: 0..1;
          tracksAllSources: 0..1;
          _reserved: 0..((1 shl 21)-1);
        );
      end;
  public
    class function sharedAddressBook: ABAddressBook; message 'sharedAddressBook';
    class function addressBook: ABAddressBook; message 'addressBook';
    function recordsMatchingSearchElement (search: ABSearchElement): NSArray; message 'recordsMatchingSearchElement:';
    function save: ObjCBOOL; message 'save';
    function saveAndReturnError (error: NSErrorPtr): ObjCBOOL; message 'saveAndReturnError:';
    function hasUnsavedChanges: ObjCBOOL; message 'hasUnsavedChanges';
    function me: ABPerson; message 'me';
    procedure setMe (moi: ABPerson); message 'setMe:';
    function recordForUniqueId (uniqueId: NSString): ABRecord; message 'recordForUniqueId:';
    function addRecord_error (record_: ABRecord; error: NSErrorPtr): ObjCBOOL; message 'addRecord:error:';
    function addRecord (record_: ABRecord): ObjCBOOL; message 'addRecord:';
    function removeRecord_error (record_: ABRecord; error: NSErrorPtr): ObjCBOOL; message 'removeRecord:error:';
    function removeRecord (record_: ABRecord): ObjCBOOL; message 'removeRecord:';
    function people: NSArray; message 'people';
    function groups: NSArray; message 'groups';
    function recordClassFromUniqueId (uniqueId: NSString): NSString; message 'recordClassFromUniqueId:';
    function formattedAddressFromDictionary (address: NSDictionary): NSAttributedString; message 'formattedAddressFromDictionary:';
    function defaultCountryCode: NSString; message 'defaultCountryCode';
    function defaultNameOrdering: NSInteger; message 'defaultNameOrdering';
  end;
{$endif}

