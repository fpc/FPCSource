{ Parsed from InstallerPlugins.framework InstallerPane.h }


{$ifdef TYPES}
type
  InstallerPanePtr = ^InstallerPane;
{$endif}

{$ifdef TYPES}
type
  InstallerSectionDirection = NSInteger;
  InstallerSectionDirectionPtr = ^InstallerSectionDirection;

const
  InstallerDirectionForward = 0;
  InstallerDirectionBackward = 1;
  InstallerDirectionUndefined = 2;

type
  InstallerPane_Private = OpaqueType;
  InstallerPane_PrivatePtr = ^InstallerPane_Private;
{$endif}

{$ifdef CLASSES}

type
  InstallerPane = objcclass external (NSObject)
  private
    _contentView: NSView;
    _initialKeyView: NSView;
    _firstKeyView: NSView;
    _lastKeyView: NSView;
    _nextPane: InstallerPane;
    parentSection: id;
    _private: InstallerPane_PrivatePtr;
  public
    procedure setContentView(newValue: NSView); message 'setContentView:';
    function contentView: NSView; message 'contentView';
    procedure setInitialKeyView(newValue: NSView); message 'setInitialKeyView:';
    function initialKeyView: NSView; message 'initialKeyView';
    procedure setFirstKeyView(newValue: NSView); message 'setFirstKeyView:';
    function firstKeyView: NSView; message 'firstKeyView';
    procedure setLastKeyView(newValue: NSView); message 'setLastKeyView:';
    function lastKeyView: NSView; message 'lastKeyView';
    procedure setNextPane(newValue: InstallerPane); message 'setNextPane:';
    function nextPane: InstallerPane; message 'nextPane';
    function initWithSection (parent: id): instancetype; message 'initWithSection:';
    function title: NSString; message 'title';
    function section: InstallerSection; message 'section';
    function contentView_: NSView; message 'contentView';
    function initialKeyView_: NSView; message 'initialKeyView';
    function firstKeyView_: NSView; message 'firstKeyView';
    function lastKeyView_: NSView; message 'lastKeyView';
    function nextPane_: InstallerPane; message 'nextPane';
    procedure willEnterPane (dir: InstallerSectionDirection); message 'willEnterPane:';
    procedure didEnterPane (dir: InstallerSectionDirection); message 'didEnterPane:';
    function shouldExitPane (dir: InstallerSectionDirection): ObjCBOOL; message 'shouldExitPane:';
    procedure willExitPane (dir: InstallerSectionDirection); message 'willExitPane:';
    procedure didExitPane (dir: InstallerSectionDirection); message 'didExitPane:';
    procedure setNextEnabled(newValue: ObjCBOOL); message 'setNextEnabled:';
    function nextEnabled: ObjCBOOL; message 'nextEnabled';
    procedure setPreviousEnabled(newValue: ObjCBOOL); message 'setPreviousEnabled:';
    function previousEnabled: ObjCBOOL; message 'previousEnabled';
    function gotoNextPane: ObjCBOOL; message 'gotoNextPane';
    function gotoPreviousPane: ObjCBOOL; message 'gotoPreviousPane';
  end;
{$endif}

