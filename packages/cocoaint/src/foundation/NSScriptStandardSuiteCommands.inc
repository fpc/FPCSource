{ Parsed from Foundation.framework NSScriptStandardSuiteCommands.h }

{$ifdef TYPES}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_T}

{ Constants }

const
  NSSaveOptionsYes = 0;
  NSSaveOptionsNo = 1;
  NSSaveOptionsAsk = 2;

{ Types }
type
  NSSaveOptions = NSUInteger;
  NSSaveOptionsPtr = ^NSSaveOptions;

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
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_S}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCloneCommand = objcclass;
  NSCloneCommandPointer = ^NSCloneCommand;
  NSCloneCommandPtr = NSCloneCommandPointer;
  NSCloseCommand = objcclass;
  NSCloseCommandPointer = ^NSCloseCommand;
  NSCloseCommandPtr = NSCloseCommandPointer;
  NSCountCommand = objcclass;
  NSCountCommandPointer = ^NSCountCommand;
  NSCountCommandPtr = NSCountCommandPointer;
  NSCreateCommand = objcclass;
  NSCreateCommandPointer = ^NSCreateCommand;
  NSCreateCommandPtr = NSCreateCommandPointer;
  NSDeleteCommand = objcclass;
  NSDeleteCommandPointer = ^NSDeleteCommand;
  NSDeleteCommandPtr = NSDeleteCommandPointer;
  NSExistsCommand = objcclass;
  NSExistsCommandPointer = ^NSExistsCommand;
  NSExistsCommandPtr = NSExistsCommandPointer;
  NSGetCommand = objcclass;
  NSGetCommandPointer = ^NSGetCommand;
  NSGetCommandPtr = NSGetCommandPointer;
  NSMoveCommand = objcclass;
  NSMoveCommandPointer = ^NSMoveCommand;
  NSMoveCommandPtr = NSMoveCommandPointer;
  NSQuitCommand = objcclass;
  NSQuitCommandPointer = ^NSQuitCommand;
  NSQuitCommandPtr = NSQuitCommandPointer;
  NSSetCommand = objcclass;
  NSSetCommandPointer = ^NSSetCommand;
  NSSetCommandPtr = NSSetCommandPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTSTANDARDSUITECOMMANDS_PAS_C}
{$define NSSCRIPTSTANDARDSUITECOMMANDS_PAS_C}

{ NSCloneCommand }
  NSCloneCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
    
  public
    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

{ NSCloseCommand }
  NSCloseCommand = objcclass external (NSScriptCommand)
    
  public
    function saveOptions: NSSaveOptions; message 'saveOptions';
  end;

{ NSCountCommand }
  NSCountCommand = objcclass external (NSScriptCommand)
    
  public
  end;

{ NSCreateCommand }
  NSCreateCommand = objcclass external (NSScriptCommand)
  private
    _moreVars2: id;
    
  public
    function createClassDescription: NSScriptClassDescription; message 'createClassDescription';
    function resolvedKeyDictionary: NSDictionary; message 'resolvedKeyDictionary';
  end;

{ NSDeleteCommand }
  NSDeleteCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
    
  public
    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

{ NSExistsCommand }
  NSExistsCommand = objcclass external (NSScriptCommand)
    
  public
  end;

{ NSGetCommand }
  NSGetCommand = objcclass external (NSScriptCommand)
    
  public
  end;

{ NSMoveCommand }
  NSMoveCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
    
  public
    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

{ NSQuitCommand }
  NSQuitCommand = objcclass external (NSScriptCommand)
    
  public
    function saveOptions: NSSaveOptions; message 'saveOptions';
  end;

{ NSSetCommand }
  NSSetCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
    
  public
    procedure setReceiversSpecifier(receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

{$endif}
{$endif}
