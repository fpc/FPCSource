{ Parsed from Appkit.framework NSActionCell.h }

{$ifdef TYPES}
{$ifndef NSACTIONCELL_PAS_T}
{$define NSACTIONCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSACTIONCELL_PAS_R}
{$define NSACTIONCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSACTIONCELL_PAS_F}
{$define NSACTIONCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSACTIONCELL_PAS_S}
{$define NSACTIONCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSActionCell = objcclass;
  NSActionCellPointer = ^NSActionCell;
  NSActionCellPtr = NSActionCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSACTIONCELL_PAS_C}
{$define NSACTIONCELL_PAS_C}

{ NSActionCell }
  NSActionCell = objcclass external (NSCell)
  private
    _tag: NSInteger;
    _target: id;
    _action: SEL;
    _controlView: id;
    
  public
    function target: id; message 'target';
    procedure setTarget(anObject: id); message 'setTarget:';
    function action: SEL; message 'action';
    procedure setAction(aSelector: SEL); message 'setAction:';
    function tag: NSInteger; message 'tag';
    procedure setTag(anInt: NSInteger); message 'setTag:';
  end;

{$endif}
{$endif}
