{ Parsed from Foundation.framework NSScriptStandardSuiteCommands.h }


{$ifdef TYPES}
type
  NSCloneCommandPtr = ^NSCloneCommand;
  NSCloseCommandPtr = ^NSCloseCommand;
  NSCountCommandPtr = ^NSCountCommand;
  NSCreateCommandPtr = ^NSCreateCommand;
  NSDeleteCommandPtr = ^NSDeleteCommand;
  NSExistsCommandPtr = ^NSExistsCommand;
  NSGetCommandPtr = ^NSGetCommand;
  NSMoveCommandPtr = ^NSMoveCommand;
  NSQuitCommandPtr = ^NSQuitCommand;
  NSSetCommandPtr = ^NSSetCommand;
{$endif}

{$ifdef TYPES}
type
  NSSaveOptions = NSUInteger;
  NSSaveOptionsPtr = ^NSSaveOptions;

const
  NSSaveOptionsYes = 0;
  NSSaveOptionsNo = 1;
  NSSaveOptionsAsk = 2;
{$endif}

{$ifdef CLASSES}

type
  NSCloneCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
  public
    procedure setReceiversSpecifier (receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

type
  NSCloseCommand = objcclass external (NSScriptCommand)
  private
  public
    function saveOptions: NSSaveOptions; message 'saveOptions';
  end;

type
  NSCountCommand = objcclass external (NSScriptCommand)
  private
  end;

type
  NSCreateCommand = objcclass external (NSScriptCommand)
  private
    _moreVars2: id;
  public
    function createClassDescription: NSScriptClassDescription; message 'createClassDescription';
    function resolvedKeyDictionary: NSDictionary; message 'resolvedKeyDictionary';
  end;

type
  NSDeleteCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
  public
    procedure setReceiversSpecifier (receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

type
  NSExistsCommand = objcclass external (NSScriptCommand)
  private
  end;

type
  NSGetCommand = objcclass external (NSScriptCommand)
  private
  end;

type
  NSMoveCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
  public
    procedure setReceiversSpecifier (receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;

type
  NSQuitCommand = objcclass external (NSScriptCommand)
  private
  public
    function saveOptions: NSSaveOptions; message 'saveOptions';
  end;

type
  NSSetCommand = objcclass external (NSScriptCommand)
  private
    _keySpecifier: NSScriptObjectSpecifier;
  public
    procedure setReceiversSpecifier (receiversRef: NSScriptObjectSpecifier); message 'setReceiversSpecifier:';
    function keySpecifier: NSScriptObjectSpecifier; message 'keySpecifier';
  end;
{$endif}

