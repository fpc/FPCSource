{ Parsed from Foundation.framework NSNull.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSNULL_PAS_H}
{$define NSNULL_PAS_H}
type
  NSNullPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSNULL_PAS_T}
{$define NSNULL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNull = objcclass;

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
