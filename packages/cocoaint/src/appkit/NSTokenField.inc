{ Parsed from Appkit.framework NSTokenField.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTOKENFIELD_PAS_H}
{$define NSTOKENFIELD_PAS_H}
type
  NSTokenFieldPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTOKENFIELD_PAS_T}
{$define NSTOKENFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOKENFIELD_PAS_R}
{$define NSTOKENFIELD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOKENFIELD_PAS_F}
{$define NSTOKENFIELD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOKENFIELD_PAS_T}
{$define NSTOKENFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTokenField = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOKENFIELD_PAS_C}
{$define NSTOKENFIELD_PAS_C}

{ NSTokenField }
  NSTokenField = objcclass(NSTextField)
    
  public
    class function alloc: NSTokenField; message 'alloc';

    procedure setTokenStyle(style: NSTokenStyle); message 'setTokenStyle:';
    function tokenStyle: NSTokenStyle; message 'tokenStyle';
    procedure setCompletionDelay(delay: NSTimeInterval); message 'setCompletionDelay:';
    function completionDelay: NSTimeInterval; message 'completionDelay';
    class function defaultCompletionDelay: NSTimeInterval; message 'defaultCompletionDelay';
    procedure setTokenizingCharacterSet(characterSet: NSCharacterSet); message 'setTokenizingCharacterSet:';
    function tokenizingCharacterSet: NSCharacterSet; message 'tokenizingCharacterSet';
    class function defaultTokenizingCharacterSet: NSCharacterSet; message 'defaultTokenizingCharacterSet';
  end; external;

{$endif}
{$endif}
