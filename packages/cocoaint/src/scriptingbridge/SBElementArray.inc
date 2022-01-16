{ Parsed from ScriptingBridge.framework SBElementArray.h }


{$ifdef TYPES}
type
  SBElementArrayPtr = ^SBElementArray;
{$endif}

{$ifdef CLASSES}

type
  SBElementArray = objcclass external (NSMutableArray)
  private
    _parent: SBObject;
    _elementCode: DescType;
    _reserved: pointer;
  public
    function objectWithName (name: NSString): id; message 'objectWithName:';
    function objectWithID (identifier: id): id; message 'objectWithID:';
    function objectAtLocation (location: id): id; message 'objectAtLocation:';
    function arrayByApplyingSelector (selector: SEL): NSArray; message 'arrayByApplyingSelector:';
    function arrayByApplyingSelector_withObject (aSelector: SEL; argument: id): NSArray; message 'arrayByApplyingSelector:withObject:';
    function get: NSArray; message 'get';
  end;
{$endif}

