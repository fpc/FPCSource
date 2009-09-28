{ Parsed from Foundation.framework NSHost.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSHOST_PAS_H}
{$define NSHOST_PAS_H}
type
  NSHostPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSHOST_PAS_C}
{$define NSHOST_PAS_C}

{ NSHost }
  NSHost = objcclass(NSObject)
  private
    _names: NSArray;
    _addresses: NSArray;
    _reserved: Pointer;
    
  public
    class function alloc: NSHost; message 'alloc';

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
  end; external;

{$endif}
{$endif}
