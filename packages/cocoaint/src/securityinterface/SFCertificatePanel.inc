{ Parsed from SecurityInterface.framework SFCertificatePanel.h }


{$ifdef TYPES}
type
  SFCertificatePanelPtr = ^SFCertificatePanel;
{$endif}

{$ifndef _SFCERTIFICATEPANEL_H_}
{$ifdef CLASSES}

type
  SFCertificatePanel = objcclass external (NSPanel)
  private
    {$if (defined(__LP64__))}
    _reserved_SFCertificatePanel: id;
    {$else}
    _certView: SFCertificateView;
    _panel: NSWindow;
    _okButton: NSButton;
    _certTable: NSTableView;
    _parentWindowForSheet: NSWindow;
    _clientDidEndSelector: SEL;
    _clientDelegate: id;
    _clientContextInfo: pointer;
    _certDataArray: NSMutableArray;
    _reserved_SFCertificatePanel_: id;
    {$endif}
  public
    class function sharedCertificatePanel: SFCertificatePanel; message 'sharedCertificatePanel';
    function runModalForTrust_showGroup (trust: SecTrustRef; showGroup: ObjCBOOL): NSInteger; message 'runModalForTrust:showGroup:';
    function runModalForCertificates_showGroup (certificates: NSArray; showGroup: ObjCBOOL): NSInteger; message 'runModalForCertificates:showGroup:';
    procedure beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroup (docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer; trust: SecTrustRef; showGroup: ObjCBOOL); message 'beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:';
    procedure beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroup (docWindow: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer; certificates: NSArray; showGroup: ObjCBOOL); message 'beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:';
    procedure setPolicies (policies: id); message 'setPolicies:';
    function policies: NSArray; message 'policies';
    procedure setDefaultButtonTitle (title: NSString); message 'setDefaultButtonTitle:';
    procedure setAlternateButtonTitle (title: NSString); message 'setAlternateButtonTitle:';
    procedure setShowsHelp (showsHelp: ObjCBOOL); message 'setShowsHelp:';
    function showsHelp: ObjCBOOL; message 'showsHelp';
    procedure setHelpAnchor (anchor: NSString); message 'setHelpAnchor:';
    function helpAnchor: NSString; message 'helpAnchor';
    function certificateView: SFCertificateView; message 'certificateView';
  end;


type
  SFCertificatePanelDelegate = objccategory external (NSObject)
    function certificatePanelShowHelp (sender: SFCertificatePanel): ObjCBOOL; message 'certificatePanelShowHelp:';
  end;
{$endif}

{$endif}
