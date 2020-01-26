{ Parsed from AppKit.framework NSObjectController.h }


{$ifdef TYPES}
type
  NSObjectControllerPtr = ^NSObjectController;
{$endif}

{$ifdef CLASSES}

type
  NSObjectController = objcclass external (NSController)
  private
    _reserved3: pointer;
    _managedProxy: id;
    __objectControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField___objectControllerFlags0: cuint);
        1: (
          _editable: 0..1;
          _automaticallyPreparesContent: 0..1;
          _hasLoadedData: 0..1;
          _explicitlyCannotAdd: 0..1;
          _explicitlyCannotRemove: 0..1;
          _isUsingManagedProxy: 0..1;
          _hasFetched: 0..1;
          _batches: 0..1;
          _reservedObjectController: 0..((1 shl 24)-1);
        );
      end;
    _objectClassName: NSString;
    _objectClass: pobjc_class;
    _contentObjectArray: NSArray;
    _content: id;
    _objectHandler: id;
  public
    function initWithContent (content: id): instancetype; message 'initWithContent:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure setContent(newValue: id); message 'setContent:';
    function content: id; message 'content';
    function selection: id; message 'selection';
    function selectedObjects: NSArray; message 'selectedObjects';
    procedure setAutomaticallyPreparesContent(newValue: ObjCBOOL); message 'setAutomaticallyPreparesContent:';
    function automaticallyPreparesContent: ObjCBOOL; message 'automaticallyPreparesContent';
    procedure prepareContent; message 'prepareContent';
    procedure setObjectClass(newValue: pobjc_class); message 'setObjectClass:';
    function objectClass: pobjc_class; message 'objectClass';
    function newObject: id; message 'newObject';
    procedure addObject (object_: id); message 'addObject:';
    procedure removeObject (object_: id); message 'removeObject:';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure add (sender: id); message 'add:';
    function canAdd: ObjCBOOL; message 'canAdd';
    procedure remove (sender: id); message 'remove:';
    function canRemove: ObjCBOOL; message 'canRemove';
    function validateUserInterfaceItem (item: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;


type
  NSManagedController = objccategory external (NSObjectController)
    procedure setManagedObjectContext(newValue: NSManagedObjectContext); message 'setManagedObjectContext:';
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    procedure setEntityName(newValue: NSString); message 'setEntityName:';
    function entityName: NSString; message 'entityName';
    procedure setFetchPredicate(newValue: NSPredicate); message 'setFetchPredicate:';
    function fetchPredicate: NSPredicate; message 'fetchPredicate';
    function fetchWithRequest_merge_error (fetchRequest: NSFetchRequest; merge: ObjCBOOL; error: NSErrorPtr): ObjCBOOL; message 'fetchWithRequest:merge:error:';
    procedure fetch (sender: id); message 'fetch:';
    procedure setUsesLazyFetching(newValue: ObjCBOOL); message 'setUsesLazyFetching:';
    function usesLazyFetching: ObjCBOOL; message 'usesLazyFetching';
    function defaultFetchRequest: NSFetchRequest; message 'defaultFetchRequest'; { available in 10_5 }
  end;
{$endif}

