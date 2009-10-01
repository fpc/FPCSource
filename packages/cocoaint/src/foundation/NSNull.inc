{ Parsed from Foundation.framework NSNull.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSNULL_PAS_C}
{$define NSNULL_PAS_C}

{ NSNull }
  NSNull = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSNull; message 'alloc';

    class function null: NSNull; message 'null';
  end; external;

{$endif}
{$endif}
