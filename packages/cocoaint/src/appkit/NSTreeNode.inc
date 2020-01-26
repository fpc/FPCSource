{ Parsed from AppKit.framework NSTreeNode.h }


{$ifdef TYPES}
type
  NSTreeNodePtr = ^NSTreeNode;
{$endif}

{$ifdef CLASSES}

type
  NSTreeNode = objcclass external (NSObject)
  private
    _childNodesProxy: id;
    _representedObject: id;
    _observationInfo: pointer;
    _reserved2: id;
    _childNodes: NSMutableArray;
    _parentNode: NSTreeNode;
    __NSTreeNodeFlags: bitpacked record
      case byte of
        0: (_anonBitField___NSTreeNodeFlags0: cuint);
        1: (
          ignoreObserving: 0..1;
          reserved: 0..((1 shl 31)-1);
        );
      end;
  public
    class function treeNodeWithRepresentedObject (modelObject: id): instancetype; message 'treeNodeWithRepresentedObject:';
    function initWithRepresentedObject (modelObject: id): instancetype; message 'initWithRepresentedObject:';
    function representedObject: id; message 'representedObject';
    function indexPath: NSIndexPath; message 'indexPath';
    function isLeaf: ObjCBOOL; message 'isLeaf';
    function childNodes: NSArray; message 'childNodes';
    function mutableChildNodes: NSMutableArray; message 'mutableChildNodes';
    function descendantNodeAtIndexPath (indexPath_: NSIndexPath): NSTreeNode; message 'descendantNodeAtIndexPath:';
    function parentNode: NSTreeNode; message 'parentNode';
    procedure sortWithSortDescriptors_recursively (sortDescriptors: NSArray; recursively: ObjCBOOL); message 'sortWithSortDescriptors:recursively:';
  end;
{$endif}

