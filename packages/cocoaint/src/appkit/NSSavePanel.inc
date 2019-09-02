{ Parsed from AppKit.framework NSSavePanel.h }


{$ifdef TYPES}
type
  NSSavePanelPtr = ^NSSavePanel;
  NSOpenSavePanelDelegateProtocolPtr = ^NSOpenSavePanelDelegateProtocol;
{$endif}

{$ifdef TYPES}
const
  NSFileHandlingPanelCancelButton = NSModalResponseCancel;
  NSFileHandlingPanelOKButton = NSModalResponseOK;

type
  __SPFlags = bitpacked record
    case byte of
      0: (_anonBitField___SPFlags0: cuint);
      1: (
        saveMode: 0..1;
        isExpanded: 0..1;
        allowsOtherFileTypes: 0..1;
        canCreateDirectories: 0..1;
        canSelectedHiddenExtension: 0..1;
        reserved2: 0..1;
        delegate_shouldShowFilename: 0..1;
        delegate_compareFilename: 0..1;
        delegate_shouldEnableURL: 0..1;
        delegate_validateURL: 0..1;
        delegate_didChangeToDirectoryURL: 0..1;
        changingFrameSize: 0..1;
        movingAccessoryView: 0..1;
        userAccessoryViewFrameChange: 0..1;
        canChooseDirectories: 0..1;
        canChooseFiles: 0..1;
        delegate_selectionDidChange: 0..1;
        delegate_didChangeToDirectory: 0..1;
        calledWindowOrderedIn: 0..1;
        appCentric: 0..1;
        bottomControlsDisabled: 0..1;
        okButtonDisabled: 0..1;
        accessoryViewDisclosed: 0..1;
        delegate_isValidFilename: 0..1;
        delegate_userEnteredFilename: 0..1;
        delegate_panel_willExpand: 0..1;
        canResolveUbiquitousConflicts: 0..1;
        reserved: 0..((1 shl 5)-1);
      );
    end;
type
  _SPFlags = __SPFlags;
type
  _SPFlagsPtr = ^__SPFlags;
type
  __SPFlagsPtr = ^__SPFlags;
{$endif}

{$ifdef CLASSES}

type
  NSSavePanel = objcclass external (NSPanel)
  private
    __navView: NSView;
    _accessoryView: NSView;
    _allowedFileTypes: NSArray;
    _validatedPosixName: NSString;
    _hiddenExtension: NSString;
    _messageTextField: NSTextField;
    _savePane: NSView;
    _saveNavSeparatorBox: NSBox;
    _savePaneTopPartsContainer: NSView;
    _nameField: NSTextField;
    _nameFieldLabel: NSTextField;
    _expansionButton: NSButton;
    _directoryPopUpContainer: NSView;
    _directoryPopUp: NSControl;
    _directoryPopUpLabel: NSTextField;
    _navViewContainer: NSBox;
    _accessoryViewContainer: NSBox;
    _bottomControlsContainer: NSView;
    _hideExtensionButton: NSButton;
    _newFolderButton: NSButton;
    _cancelButton: NSButton;
    _okButton: NSButton;
    _filepathInputController: id;
    _reservedID: id;
    _spFlags: _SPFlags;
    _spAuxiliaryStorage: NSSavePanelAuxiliary;
    _anonStruct_NSSavePanel0: record
      case byte of
        0: (anonBitField_NSSavePanel0: cuint);
        1: (data: bitpacked record
          _unused: 0..1;
          {$if  not defined(__LP64__)}
          _reserved:array[0..3] of char;
          {$endif}
        end;
      );
    end;
    _openProgressIndicator: NSProgressIndicator;
  public
    class function savePanel: NSSavePanel; message 'savePanel';
    function URL: NSURL; message 'URL';
    procedure setDirectoryURL(newValue: NSURL); message 'setDirectoryURL:';
    function directoryURL: NSURL; message 'directoryURL';
    procedure setAllowedFileTypes(newValue: NSArray); message 'setAllowedFileTypes:';
    function allowedFileTypes: NSArray; message 'allowedFileTypes';
    procedure setAllowsOtherFileTypes(newValue: ObjCBOOL); message 'setAllowsOtherFileTypes:';
    function allowsOtherFileTypes: ObjCBOOL; message 'allowsOtherFileTypes';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setDelegate(newValue: NSOpenSavePanelDelegateProtocol); message 'setDelegate:';
    function delegate: NSOpenSavePanelDelegateProtocol; message 'delegate';
    function isExpanded: ObjCBOOL; message 'isExpanded';
    procedure setCanCreateDirectories(newValue: ObjCBOOL); message 'setCanCreateDirectories:';
    function canCreateDirectories: ObjCBOOL; message 'canCreateDirectories';
    procedure setCanSelectHiddenExtension(newValue: ObjCBOOL); message 'setCanSelectHiddenExtension:';
    function canSelectHiddenExtension: ObjCBOOL; message 'canSelectHiddenExtension';
    procedure setExtensionHidden(newValue: ObjCBOOL); message 'setExtensionHidden:';
    function isExtensionHidden: ObjCBOOL; message 'isExtensionHidden';
    procedure setTreatsFilePackagesAsDirectories(newValue: ObjCBOOL); message 'setTreatsFilePackagesAsDirectories:';
    function treatsFilePackagesAsDirectories: ObjCBOOL; message 'treatsFilePackagesAsDirectories';
    procedure setPrompt(newValue: NSString); message 'setPrompt:';
    function prompt: NSString; message 'prompt';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setNameFieldLabel(newValue: NSString); message 'setNameFieldLabel:';
    function nameFieldLabel: NSString; message 'nameFieldLabel';
    procedure setNameFieldStringValue(newValue: NSString); message 'setNameFieldStringValue:';
    function nameFieldStringValue: NSString; message 'nameFieldStringValue';
    procedure setMessage(newValue: NSString); message 'setMessage:';
    function message_: NSString; message 'message';
    procedure validateVisibleColumns; message 'validateVisibleColumns';
    procedure setShowsHiddenFiles(newValue: ObjCBOOL); message 'setShowsHiddenFiles:';
    function showsHiddenFiles: ObjCBOOL; message 'showsHiddenFiles';
    procedure setShowsTagField(newValue: ObjCBOOL); message 'setShowsTagField:';
    function showsTagField: ObjCBOOL; message 'showsTagField';
    procedure setTagNames(newValue: NSArray); message 'setTagNames:';
    function tagNames: NSArray; message 'tagNames';
    procedure ok (sender: id); message 'ok:';
    procedure cancel (sender: id); message 'cancel:';
    procedure beginSheetModalForWindow_completionHandler (window: NSWindow; handler: OpaqueCBlock); message 'beginSheetModalForWindow:completionHandler:'; { available in 10_6 }
    procedure beginWithCompletionHandler (handler: OpaqueCBlock); message 'beginWithCompletionHandler:'; { available in 10_6 }
    function runModal: NSInteger; message 'runModal';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSOpenSavePanelDelegateProtocol = objcprotocol external name 'NSOpenSavePanelDelegate' (NSObjectProtocol)
  optional
    function panel_shouldEnableURL (sender: id; url: NSURL): ObjCBOOL; message 'panel:shouldEnableURL:'; { available in 10_6 }
    function panel_validateURL_error (sender: id; url: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'panel:validateURL:error:'; { available in 10_6 }
    procedure panel_didChangeToDirectoryURL (sender: id; url: NSURL); message 'panel:didChangeToDirectoryURL:'; { available in 10_6 }
    function panel_userEnteredFilename_confirmed (sender: id; filename: NSString; okFlag: ObjCBOOL): NSString; message 'panel:userEnteredFilename:confirmed:';
    procedure panel_willExpand (sender: id; expanding: ObjCBOOL); message 'panel:willExpand:';
    procedure panelSelectionDidChange (sender: id); message 'panelSelectionDidChange:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSSavePanelDelegateDeprecated = objccategory external (NSObject)
    function panel_isValidFilename (sender: id; filename: NSString): ObjCBOOL; message 'panel:isValidFilename:'; deprecated 'in 10_0, 10_6, "Use -panel:validateURL:error: instead"';
    procedure panel_directoryDidChange (sender: id; path: NSString); message 'panel:directoryDidChange:'; deprecated 'in 10_3, 10_6, "Use -panel:didChangeToDirectoryURL: instead"';
    function panel_compareFilename_with_caseSensitive (sender: id; name1: NSString; name2: NSString; caseSensitive: ObjCBOOL): NSComparisonResult; message 'panel:compareFilename:with:caseSensitive:'; deprecated 'in 10_0, 10_6';
    function panel_shouldShowFilename (sender: id; filename: NSString): ObjCBOOL; message 'panel:shouldShowFilename:'; deprecated 'in 10_0, 10_6, "Use -panel:shouldEnableURL: instead"';
  end;

type
  NSSavePanel_NSDeprecated = objccategory external name 'NSDeprecated' (NSSavePanel)
    function filename: NSString; message 'filename'; deprecated 'in 10_0, 10_6';
    function directory: NSString; message 'directory'; deprecated 'in 10_0, 10_6';
    procedure setDirectory (path: NSString); message 'setDirectory:'; deprecated 'in 10_0, 10_6';
    function requiredFileType: NSString; message 'requiredFileType'; deprecated 'in 10_0, 10_6';
    procedure setRequiredFileType (type_: NSString); message 'setRequiredFileType:'; deprecated 'in 10_0, 10_6';
    procedure beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo (path: NSString; name: NSString; docWindow: NSWindow; delegate_: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:'; deprecated 'in 10_0, 10_6';
    function runModalForDirectory_file (path: NSString; name: NSString): NSInteger; message 'runModalForDirectory:file:'; deprecated 'in 10_0, 10_6';
    procedure selectText (sender: id); message 'selectText:'; deprecated 'in 10_0, 10_3';
  end;
{$endif}

