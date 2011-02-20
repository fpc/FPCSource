{ Parsed from Foundation.framework NSHost.h }

{$ifdef TYPES}
{$ifndef NSHOST_PAS_T}
{$define NSHOST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHOST_PAS_R}
{$define NSHOST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHOST_PAS_F}
{$define NSHOST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHOST_PAS_S}
{$define NSHOST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSHost = objcclass;
  NSHostPointer = ^NSHost;
  NSHostPtr = NSHostPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSHOST_PAS_C}
{$define NSHOST_PAS_C}

{ NSHost }
  NSHost = objcclass external (NSObject)
  private
    _names: NSArray;
    _addresses: NSArray;
    reserved: Pointer;
    
  public
    class function currentHost: NSHost; message 'currentHost';
    class function hostWithName(name_: NSString): NSHost; message 'hostWithName:';
    class function hostWithAddress(address_: NSString): NSHost; message 'hostWithAddress:';
    class procedure setHostCacheEnabled(flag: Boolean); message 'setHostCacheEnabled:';
    class function isHostCacheEnabled: Boolean; message 'isHostCacheEnabled';
    class procedure flushHostCache; message 'flushHostCache';
    function isEqualToHost(aHost: NSHost): Boolean; message 'isEqualToHost:';
    function name: NSString; message 'name';
    function names: NSArray; message 'names';
    function address: NSString; message 'address';
    function addresses: NSArray; message 'addresses';
    function localizedName: NSString; message 'localizedName';
  end;

{$endif}
{$endif}
