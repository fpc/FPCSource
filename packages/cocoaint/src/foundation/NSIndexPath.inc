{ Parsed from Foundation.framework NSIndexPath.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSINDEXPATH_PAS_H}
{$define NSINDEXPATH_PAS_H}
type
  NSIndexPathPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSINDEXPATH_PAS_T}
{$define NSINDEXPATH_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSINDEXPATH_PAS_R}
{$define NSINDEXPATH_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSINDEXPATH_PAS_F}
{$define NSINDEXPATH_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINDEXPATH_PAS_T}
{$define NSINDEXPATH_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSIndexPath = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSINDEXPATH_PAS_C}
{$define NSINDEXPATH_PAS_C}

{ NSIndexPath }
  NSIndexPath = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __indexes: NSUInteger;
    __hash: NSUInteger;
    __length: NSUInteger;
    __reserved: Pointer;
    
  public
    class function alloc: NSIndexPath; message 'alloc';

    class function indexPathWithIndex(index: NSUInteger): id; message 'indexPathWithIndex:';
    class function indexPathWithIndexes_length(indexes: NSUIntegerPointer; length_: NSUInteger): id; message 'indexPathWithIndexes:length:';
    function initWithIndex(index: NSUInteger): id; message 'initWithIndex:';
    function initWithIndexes_length(indexes: NSUIntegerPointer; length_: NSUInteger): id; message 'initWithIndexes:length:';
    function indexPathByAddingIndex(index: NSUInteger): NSIndexPath; message 'indexPathByAddingIndex:';
    function indexPathByRemovingLastIndex: NSIndexPath; message 'indexPathByRemovingLastIndex';
    function indexAtPosition(position: NSUInteger): NSUInteger; message 'indexAtPosition:';
    function length: NSUInteger; message 'length';
    procedure getIndexes(indexes: NSUIntegerPointer); message 'getIndexes:';
    function compare(otherObject: NSIndexPath): NSComparisonResult; message 'compare:';
  end; external;

{$endif}
{$endif}
