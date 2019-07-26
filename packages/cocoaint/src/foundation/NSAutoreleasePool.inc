{ Parsed from Foundation.framework NSAutoreleasePool.h }


{$ifdef TYPES}
type
  NSAutoreleasePoolPtr = ^NSAutoreleasePool;
{$endif}

{$ifdef CLASSES}

type
  NSAutoreleasePool = objcclass external (NSObject)
  private
    _token: pointer;
    _reserved3: pointer;
    _reserved2: pointer;
    _reserved: pointer;
  public
    class procedure classAddObject (anObject: id); message 'addObject:';
    procedure addObject (anObject: id); message 'addObject:';
    procedure drain; message 'drain';
  end;
{$endif}

