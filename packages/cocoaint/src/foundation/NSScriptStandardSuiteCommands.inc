{ Parsed from Foundation.framework NSScriptStandardSuiteCommands.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_H}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_H}
type
  NSCloneCommandPointer = Pointer;
  NSCloseCommandPointer = Pointer;
  NSCountCommandPointer = Pointer;
  NSCreateCommandPointer = Pointer;
  NSDeleteCommandPointer = Pointer;
  NSExistsCommandPointer = Pointer;
  NSGetCommandPointer = Pointer;
  NSMoveCommandPointer = Pointer;
  NSQuitCommandPointer = Pointer;
  NSSetCommandPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}

{ Constants }

const
  NSSaveOptionsYes = 0;
  NSSaveOptionsNo = 0;
  NSSaveOptionsAsk = 1;

{ Types }
type
  NSSaveOptions = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_R}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_F}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCloneCommand = objcclass;
  NSCloseCommand = objcclass;
  NSCountCommand = objcclass;
  NSCreateCommand = objcclass;
  NSDeleteCommand = objcclass;
  NSExistsCommand = objcclass;
  NSGetCommand = objcclass;
  NSMoveCommand = objcclass;
  NSQuitCommand = objcclass;
  NSSetCommand = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_C}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_C}

{ NSCloneCommand }
  NSCloneCommand = objcclass(NSScriptCommand)
  private
    __keySpecifier: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSCloneCommand; message 'alloc';

    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end; external;

{ NSCloseCommand }
  NSCloseCommand = objcclass(NSScriptCommand)
    
  public
    class function alloc: NSCloseCommand; message 'alloc';

    function saveOptions: NSSaveOptions; message 'saveOptions';
  end; external;

{ NSCountCommand }
  NSCountCommand = objcclass(NSScriptCommand)
    
  public
    class function alloc: NSCountCommand; message 'alloc';
  end; external;

{ NSCreateCommand }
  NSCreateCommand = objcclass(NSScriptCommand)
  private
    __moreVars2: id;
    
  public
    class function alloc: NSCreateCommand; message 'alloc';

    function createClassDescription: NSScriptClassDescription; message 'createClassDescription';
    function resolvedKeyDictionary: NSDictionary; message 'resolvedKeyDictionary';
  end; external;

{ NSDeleteCommand }
  NSDeleteCommand = objcclass(NSScriptCommand)
  private
    __keySpecifier: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSDeleteCommand; message 'alloc';

    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end; external;

{ NSExistsCommand }
  NSExistsCommand = objcclass(NSScriptCommand)
    
  public
    class function alloc: NSExistsCommand; message 'alloc';
  end; external;

{ NSGetCommand }
  NSGetCommand = objcclass(NSScriptCommand)
    
  public
    class function alloc: NSGetCommand; message 'alloc';
  end; external;

{ NSMoveCommand }
  NSMoveCommand = objcclass(NSScriptCommand)
  private
    __keySpecifier: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSMoveCommand; message 'alloc';

    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end; external;

{ NSQuitCommand }
  NSQuitCommand = objcclass(NSScriptCommand)
    
  public
    class function alloc: NSQuitCommand; message 'alloc';

    function saveOptions: NSSaveOptions; message 'saveOptions';
  end; external;

{ NSSetCommand }
  NSSetCommand = objcclass(NSScriptCommand)
  private
    __keySpecifier: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSSetCommand; message 'alloc';

    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end; external;

{$endif}
{$endif}
