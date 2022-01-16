{ Parsed from SecurityInterface.framework SFKeychainSavePanel.h }


{$ifdef TYPES}
type
  SFKeychainSavePanelPtr = ^SFKeychainSavePanel;
{$endif}

{$ifndef _SFKEYCHAINSAVEPANEL_H_}

{$ifdef CLASSES}

type
  SFKeychainSavePanel = objcclass external (NSSavePanel)
  private
    {$if (defined(__LP64__))}
    _reserved_SFKeychainSavePanel: id;
    {$else}
    _customView: NSView;
    _message: NSTextField;
    _password: NSString;
    _kcFullPathName: NSString;
    _startPath: NSString;
    _keychainFileName: NSString;
    _returnedKeychain: SecKeychainRef;
    _parentWindowForSheet: NSWindow;
    _clientDidEndSelector: SEL;
    _clientDelegate: id;
    _contextInfo: pointer;
    _result: OSStatus;
    _reserved_SFKeychainSavePanel_: id;
    {$endif}
  public
    class function sharedKeychainSavePanel: SFKeychainSavePanel; message 'sharedKeychainSavePanel';
    function runModalForDirectory_file (path: NSString; name: NSString): NSInteger; message 'runModalForDirectory:file:';
    procedure setPassword (password: NSString); message 'setPassword:';
    function keychain: SecKeychainRef; message 'keychain';
    function error: NSError; message 'error';
    procedure beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo (path: NSString; name: NSString; docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer); message 'beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
  end;
{$endif}

{$endif}
