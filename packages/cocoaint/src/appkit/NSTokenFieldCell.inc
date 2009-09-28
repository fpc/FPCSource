{ Parsed from Appkit.framework NSTokenFieldCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTOKENFIELDCELL_PAS_H}
{$define NSTOKENFIELDCELL_PAS_H}
type
  NSTokenFieldCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTOKENFIELDCELL_PAS_T}
{$define NSTOKENFIELDCELL_PAS_T}

{ Constants }

const
  NSDefaultTokenStyle = 0;
  NSPlainTextTokenStyle = 1;
  NSRoundedTokenStyle = 2;

{ Types }
type
  NSTokenStyle = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOKENFIELDCELL_PAS_R}
{$define NSTOKENFIELDCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOKENFIELDCELL_PAS_F}
{$define NSTOKENFIELDCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOKENFIELDCELL_PAS_S}
{$define NSTOKENFIELDCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTokenFieldCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOKENFIELDCELL_PAS_C}
{$define NSTOKENFIELDCELL_PAS_C}

{ NSTokenFieldCell }
  NSTokenFieldCell = objcclass(NSTextFieldCell)
  private
    __tokenizingCharacterSet: NSCharacterSet;
    __delegate: id;
    __completionDelay: NSTimeInterval;
    __cache: id;
    __defaultTerminator: id;
    __trackingArea: id;
    __lastCell: id;
    __lastCellFrame: NSRect;
        _reserved: array[0..6] of id;
    __tfcFlags: bitpacked record
        _style: 0..((1 shl 4)-1);
        
        _invalidCache: 0..1;
        _inDidChange: 0..1;
        _validationDisabled: 0..1;
        _pendingComplete: 0..1;
        _autoCompleteMode: 0..((1 shl 2)-1);
        _inValidateEditing: 0..1;
        
        _reserved: 0..((1 shl 21)-1);
      end;
    
  public
    class function alloc: NSTokenFieldCell; message 'alloc';

    procedure setTokenStyle(style: NSTokenStyle); message 'setTokenStyle:';
    function tokenStyle: NSTokenStyle; message 'tokenStyle';
    procedure setCompletionDelay(delay: NSTimeInterval); message 'setCompletionDelay:';
    function completionDelay: NSTimeInterval; message 'completionDelay';
    class function defaultCompletionDelay: NSTimeInterval; message 'defaultCompletionDelay';
    procedure setTokenizingCharacterSet(characterSet: NSCharacterSet); message 'setTokenizingCharacterSet:';
    function tokenizingCharacterSet: NSCharacterSet; message 'tokenizingCharacterSet';
    class function defaultTokenizingCharacterSet: NSCharacterSet; message 'defaultTokenizingCharacterSet';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end; external;

{$endif}
{$endif}
