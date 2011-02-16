{ Parsed from Foundation.framework NSIndexPath.h }

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
{$ifndef NSINDEXPATH_PAS_S}
{$define NSINDEXPATH_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSIndexPath = objcclass;
  NSIndexPathPointer = ^NSIndexPath;
  NSIndexPathPtr = NSIndexPathPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSINDEXPATH_PAS_C}
{$define NSINDEXPATH_PAS_C}

{ NSIndexPath }
  NSIndexPath = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _indexes: NSUIntegerPtr;
    _hash: NSUInteger;
    _length: NSUInteger;
    _reserved: Pointer;
    
  public
    class function indexPathWithIndex(index: NSUInteger): id; message 'indexPathWithIndex:';
    class function indexPathWithIndexes_length(indexes: NSUIntegerPtr; length_: NSUInteger): id; message 'indexPathWithIndexes:length:';
    function initWithIndex(index: NSUInteger): id; message 'initWithIndex:';
    function initWithIndexes_length(indexes: NSUIntegerPtr; length_: NSUInteger): id; message 'initWithIndexes:length:';
    function indexPathByAddingIndex(index: NSUInteger): NSIndexPath; message 'indexPathByAddingIndex:';
    function indexPathByRemovingLastIndex: NSIndexPath; message 'indexPathByRemovingLastIndex';
    function indexAtPosition(position: NSUInteger): NSUInteger; message 'indexAtPosition:';
    function length: NSUInteger; message 'length';
    procedure getIndexes(indexes: NSUIntegerPtr); message 'getIndexes:';
    function compare(otherObject: NSIndexPath): NSComparisonResult; message 'compare:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
