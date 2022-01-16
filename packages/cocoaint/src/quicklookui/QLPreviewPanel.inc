{ Parsed from QuickLookUI.framework QLPreviewPanel.h }


{$ifdef TYPES}
type
  QLPreviewPanelPtr = ^QLPreviewPanel;
  QLPreviewPanelDataSourceProtocolPtr = ^QLPreviewPanelDataSourceProtocol;
  QLPreviewPanelDelegateProtocolPtr = ^QLPreviewPanelDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  QLPreviewPanel = objcclass external (NSPanel)
  private
    _reserved: QLPreviewPanelReserved;
  public
    class function sharedPreviewPanel: QLPreviewPanel; message 'sharedPreviewPanel';
    class function sharedPreviewPanelExists: ObjCBOOL; message 'sharedPreviewPanelExists';
    function currentController: id; message 'currentController';
    procedure updateController; message 'updateController';
    procedure setDataSource(newValue: QLPreviewPanelDataSourceProtocol); message 'setDataSource:';
    function dataSource: QLPreviewPanelDataSourceProtocol; message 'dataSource';
    procedure reloadData; message 'reloadData';
    procedure refreshCurrentPreviewItem; message 'refreshCurrentPreviewItem';
    procedure setCurrentPreviewItemIndex(newValue: NSInteger); message 'setCurrentPreviewItemIndex:';
    function currentPreviewItemIndex: NSInteger; message 'currentPreviewItemIndex';
    function currentPreviewItem: QLPreviewItemProtocol; message 'currentPreviewItem';
    procedure setDisplayState(newValue: id); message 'setDisplayState:';
    function displayState: id; message 'displayState';
    procedure setDelegate(newValue: QLPreviewPanelDelegateProtocol); message 'setDelegate:';
    function delegate: QLPreviewPanelDelegateProtocol; message 'delegate';
    function enterFullScreenMode_withOptions (screen: NSScreen; options: NSDictionary): ObjCBOOL; message 'enterFullScreenMode:withOptions:';
    procedure exitFullScreenModeWithOptions (options: NSDictionary); message 'exitFullScreenModeWithOptions:';
    function isInFullScreenMode: ObjCBOOL; message 'isInFullScreenMode';
  end;


type
  QLPreviewPanelController = objccategory external (NSObject)
    function acceptsPreviewPanelControl (panel: QLPreviewPanel): ObjCBOOL; message 'acceptsPreviewPanelControl:';
    procedure beginPreviewPanelControl (panel: QLPreviewPanel); message 'beginPreviewPanelControl:';
    procedure endPreviewPanelControl (panel: QLPreviewPanel); message 'endPreviewPanelControl:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  QLPreviewPanelDataSourceProtocol = objcprotocol external name 'QLPreviewPanelDataSource'
  required
    function numberOfPreviewItemsInPreviewPanel (panel: QLPreviewPanel): NSInteger; message 'numberOfPreviewItemsInPreviewPanel:';
    function previewPanel_previewItemAtIndex (panel: QLPreviewPanel; index: NSInteger): QLPreviewItemProtocol; message 'previewPanel:previewItemAtIndex:';
  end;

type
  QLPreviewPanelDelegateProtocol = objcprotocol external name 'QLPreviewPanelDelegate' (NSWindowDelegateProtocol)
  optional
    function previewPanel_handleEvent (panel: QLPreviewPanel; event: NSEvent): ObjCBOOL; message 'previewPanel:handleEvent:';
    function previewPanel_sourceFrameOnScreenForPreviewItem (panel: QLPreviewPanel; item: QLPreviewItemProtocol): NSRect; message 'previewPanel:sourceFrameOnScreenForPreviewItem:';
    function previewPanel_transitionImageForPreviewItem_contentRect (panel: QLPreviewPanel; item: QLPreviewItemProtocol; contentRect: NSRectPtr): id; message 'previewPanel:transitionImageForPreviewItem:contentRect:';
  end;
{$endif}

