{ Parsed from Appkit.framework NSSavePanel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSAVEPANEL_PAS_H}
{$define NSSAVEPANEL_PAS_H}
type
  NSSavePanelPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSAVEPANEL_PAS_T}
{$define NSSAVEPANEL_PAS_T}

{ Constants }

const
  NSFileHandlingPanelCancelButton = NSCancelButton;
  NSFileHandlingPanelOKButton = NSOKButton;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSAVEPANEL_PAS_R}
{$define NSSAVEPANEL_PAS_R}

{ Records }
type
  __SPFlags = record
    saveMode: cuint;
    isExpanded: cuint;
    allowsOtherFileTypes: cuint;
    canCreateDirectories: cuint;
    canSelectedHiddenExtension: cuint;
    reserved: cuint;
  end;
_SPFlags = __SPFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSAVEPANEL_PAS_F}
{$define NSSAVEPANEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSAVEPANEL_PAS_T}
{$define NSSAVEPANEL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSavePanel = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSAVEPANEL_PAS_C}
{$define NSSAVEPANEL_PAS_C}

{ NSSavePanel }
  NSSavePanel = objcclass(NSPanel)
  private
    __navView: NSNavView;
    __accessoryView: NSView;
    __allowedFileTypes: NSArray;
    __validatedPosixName: NSString;
    __hiddenExtension: NSString;
    __messageTextField: NSTextField;
    __savePane: NSView;
    __saveNavSeparatorBox: NSBox;
    __savePaneTopPartsContainer: NSView;
    __nameField: NSTextField;
    __nameFieldLabel: NSTextField;
    __expansionButton: NSButton;
    __directoryPopUpContainer: NSView;
    __directoryPopUp: id;
    __directoryPopUpLabel: NSTextField;
    __navViewContainer: NSBox;
    __accessoryViewContainer: NSBox;
    __bottomControlsContainer: NSView;
    __hideExtensionButton: NSButton;
    __newFolderButton: NSButton;
    __cancelButton: NSButton;
    __okButton: NSButton;
    __filepathInputController: id;
    __newFolderController: id;
    __spFlags: _SPFlags;
    __spAuxiliaryStorage: NSSavePanelAuxiliary;
    __reserved: char;
    __private: Pointer;
    
  public
    class function alloc: NSSavePanel; message 'alloc';

    class function savePanel: NSSavePanel; message 'savePanel';
    function URL: NSURL; message 'URL';
    function filename: NSString; message 'filename';
    function directory: NSString; message 'directory';
    procedure setDirectory(path: NSString); message 'setDirectory:';
    function requiredFileType: NSString; message 'requiredFileType';
    procedure setRequiredFileType(type_: NSString); message 'setRequiredFileType:';
    function allowedFileTypes: NSArray; message 'allowedFileTypes';
    procedure setAllowedFileTypes(types: NSArray); message 'setAllowedFileTypes:';
    function allowsOtherFileTypes: Boolean; message 'allowsOtherFileTypes';
    procedure setAllowsOtherFileTypes(flag: Boolean); message 'setAllowsOtherFileTypes:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setAccessoryView(view: NSView); message 'setAccessoryView:';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function isExpanded: Boolean; message 'isExpanded';
    function canCreateDirectories: Boolean; message 'canCreateDirectories';
    procedure setCanCreateDirectories(flag: Boolean); message 'setCanCreateDirectories:';
    function canSelectHiddenExtension: Boolean; message 'canSelectHiddenExtension';
    procedure setCanSelectHiddenExtension(flag: Boolean); message 'setCanSelectHiddenExtension:';
    function isExtensionHidden: Boolean; message 'isExtensionHidden';
    procedure setExtensionHidden(flag: Boolean); message 'setExtensionHidden:';
    function treatsFilePackagesAsDirectories: Boolean; message 'treatsFilePackagesAsDirectories';
    procedure setTreatsFilePackagesAsDirectories(flag: Boolean); message 'setTreatsFilePackagesAsDirectories:';
    function prompt: NSString; message 'prompt';
    procedure setPrompt(prompt_: NSString); message 'setPrompt:';
    function title: NSString; message 'title';
    procedure setTitle(title_: NSString); message 'setTitle:';
    function nameFieldLabel: NSString; message 'nameFieldLabel';
    procedure setNameFieldLabel(label_: NSString); message 'setNameFieldLabel:';
    function message: NSString; message 'message';
    procedure setMessage(message_: NSString); message 'setMessage:';
    procedure validateVisibleColumns; message 'validateVisibleColumns';
    procedure selectText(sender: id); message 'selectText:';

    { Category: NSSavePanelRuntime }
    procedure ok(sender: id); message 'ok:';
    procedure cancel(sender: id); message 'cancel:';
    procedure beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo(path: NSString; name: NSString; docWindow: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: Pointer); message 'beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
    function runModalForDirectory_file(path: NSString; name: NSString): clong; message 'runModalForDirectory:file:';
    function runModal: clong; message 'runModal';
  end; external;

{$endif}
{$endif}
