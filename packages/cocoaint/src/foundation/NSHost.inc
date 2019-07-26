{ Parsed from Foundation.framework NSHost.h }


{$ifdef TYPES}
type
  NSHostPtr = ^NSHost;
{$endif}

{$ifdef CLASSES}

type
  NSHost = objcclass external (NSObject)
  private
    names: NSArray;
    addresses: NSArray;
    reserved: id;
  public
    class function currentHost: instancetype; message 'currentHost';
    class function hostWithName (name: NSString): instancetype; message 'hostWithName:';
    class function hostWithAddress (address: NSString): instancetype; message 'hostWithAddress:';
    function isEqualToHost (aHost: NSHost): ObjCBOOL; message 'isEqualToHost:';
    function name: NSString; message 'name';
    function names_: NSArray; message 'names';
    function address: NSString; message 'address';
    function addresses_: NSArray; message 'addresses';
    function localizedName: NSString; message 'localizedName';
    class procedure setHostCacheEnabled (flag: ObjCBOOL); message 'setHostCacheEnabled:'; deprecated 'in 10_0, 10_7';
    class function isHostCacheEnabled: ObjCBOOL; message 'isHostCacheEnabled'; deprecated 'in 10_0, 10_7';
    class procedure flushHostCache; message 'flushHostCache'; deprecated 'in 10_0, 10_7';
  end;
{$endif}

