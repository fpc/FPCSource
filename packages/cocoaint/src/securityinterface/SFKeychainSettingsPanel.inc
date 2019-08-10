{ Parsed from SecurityInterface.framework SFKeychainSettingsPanel.h }


{$ifdef TYPES}
type
  SFKeychainSettingsPanelPtr = ^SFKeychainSettingsPanel;
{$endif}

{$ifndef _SFKEYCHAINSETTINGSPANEL_H_}
{$ifdef CLASSES}

type
  SFKeychainSettingsPanel = objcclass external (NSPanel)
  private
    {$if (defined(__LP64__))}
    _reserved_SFKeychainSettingsPanel: id;
    {$else}
    _panelMessage: NSTextField;
    _autoLockValue: SFAutoLockTextValue;
    _autoLockStepper: NSStepper;
    _autoLockCtrl: NSButton;
    _lockOnSleepCtrl: NSButton;
    _cancelButton: NSButton;
    _saveButton: NSButton;
    _panel: NSWindow;
    _syncStateMessage: NSTextField;
    _keychainNameStr: NSString;
    _parentWindowForSheet: NSWindow;
    _clientDidEndSelector: SEL;
    _clientDelegate: id;
    _clientContextInfo: pointer;
    _lockIntervalVal: cuint32;
    _keychainRef: SecKeychainRef;
    _lockOnSleepVal: ObjCBOOL;
    _settings: SecKeychainSettings;
    _reserved_SFKeychainSettingsPanel_: id;
    {$endif}
  public
    class function sharedKeychainSettingsPanel: SFKeychainSettingsPanel; message 'sharedKeychainSettingsPanel';
    function runModalForSettings_keychain (settings: SecKeychainSettingsPtr; keychain: SecKeychainRef): NSInteger; message 'runModalForSettings:keychain:';
    procedure beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_settings_keychain (docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer; settings: SecKeychainSettingsPtr; keychain: SecKeychainRef); message 'beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:settings:keychain:';
  end;
{$endif}

{$endif}
