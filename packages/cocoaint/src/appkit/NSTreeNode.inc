{ Parsed from Appkit.framework NSTreeNode.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTREENODE_PAS_H}
{$define NSTREENODE_PAS_H}
type
  NSTreeNodePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTREENODE_PAS_T}
{$define NSTREENODE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTREENODE_PAS_R}
{$define NSTREENODE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTREENODE_PAS_F}
{$define NSTREENODE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTREENODE_PAS_T}
{$define NSTREENODE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTreeNode = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTREENODE_PAS_C}
{$define NSTREENODE_PAS_C}

{ NSTreeNode }
  NSTreeNode = objcclass(NSObject)
  private
    __childNodesProxy: id;
    __representedObject: id;
    __observationInfo: Pointer;
    __reserved2: id;
    __childNodes: NSMutableArray;
    __parentNode: NSTreeNode;
    __NSTreeNodeFlags: bitpacked record
        ignoreObserving: 0..1;
        reserved: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSTreeNode; message 'alloc';

    class function treeNodeWithRepresentedObject(modelObject: id): id; message 'treeNodeWithRepresentedObject:';
    function initWithRepresentedObject(modelObject: id): id; message 'initWithRepresentedObject:';
    function representedObject: id; message 'representedObject';
    function indexPath: NSIndexPath; message 'indexPath';
    function isLeaf: Boolean; message 'isLeaf';
    function childNodes: NSArray; message 'childNodes';
    function mutableChildNodes: NSMutableArray; message 'mutableChildNodes';
    function descendantNodeAtIndexPath(indexPath_: NSIndexPath): NSTreeNode; message 'descendantNodeAtIndexPath:';
    function parentNode: NSTreeNode; message 'parentNode';
    procedure sortWithSortDescriptors_recursively(sortDescriptors: NSArray; recursively: Boolean); message 'sortWithSortDescriptors:recursively:';
  end; external;

{$endif}
{$endif}
