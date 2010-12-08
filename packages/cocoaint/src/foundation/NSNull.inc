{ Parsed from Foundation.framework NSNull.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSNULL_PAS_T}
{$define NSNULL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNULL_PAS_R}
{$define NSNULL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNULL_PAS_F}
{$define NSNULL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNULL_PAS_S}
{$define NSNULL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNull = objcclass;
  NSNullPointer = ^NSNull;
  NSNullPtr = NSNullPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNULL_PAS_C}
{$define NSNULL_PAS_C}

{ NSNull }
  NSNull = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSNull; message 'alloc';

    class function null: NSNull; message 'null';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{$endif}
{$endif}
