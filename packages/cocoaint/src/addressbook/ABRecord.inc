{ Parsed from AddressBook.framework ABRecord.h }


{$ifdef TYPES}
type
  ABRecordPtr = ^ABRecord;
{$endif}

{$ifdef CLASSES}

type
  ABRecord = objcclass external (NSObject)
  private
    _databaseImpl: id;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
  public
    function init: id; message 'init';
    function initWithAddressBook (addressBook: ABAddressBook): id; message 'initWithAddressBook:';
    function valueForProperty (property_: NSString): id; message 'valueForProperty:';
    function setValue_forProperty_error (value: id; property_: NSString; error: NSErrorPtr): ObjCBOOL; message 'setValue:forProperty:error:';
    function setValue_forProperty (value: id; property_: NSString): ObjCBOOL; message 'setValue:forProperty:';
    function removeValueForProperty (property_: NSString): ObjCBOOL; message 'removeValueForProperty:';
    function isReadOnly: ObjCBOOL; message 'isReadOnly';
  end;


type
  ABRecord_Convenience = objccategory external (ABRecord)
    function uniqueId: NSString; message 'uniqueId';
    function displayName: NSString; message 'displayName';
  end;
{$endif}

