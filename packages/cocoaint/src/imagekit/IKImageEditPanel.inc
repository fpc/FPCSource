{ Parsed from ImageKit.framework IKImageEditPanel.h }


{$ifdef TYPES}
type
  IKImageEditPanelPtr = ^IKImageEditPanel;
  IKImageEditPanelDataSourceProtocolPtr = ^IKImageEditPanelDataSourceProtocol;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef PROTOCOLS}

type
  IKImageEditPanelDataSourceProtocol = objcprotocol external name 'IKImageEditPanelDataSource'
  required
    function image: CGImageRef; message 'image';
    procedure setImage_imageProperties (image_: CGImageRef; metaData: NSDictionary); message 'setImage:imageProperties:';
  optional
    function thumbnailWithMaximumSize (size: NSSize): CGImageRef; message 'thumbnailWithMaximumSize:';
    function imageProperties: NSDictionary; message 'imageProperties';
    function hasAdjustMode: ObjCBOOL; message 'hasAdjustMode';
    function hasEffectsMode: ObjCBOOL; message 'hasEffectsMode';
    function hasDetailsMode: ObjCBOOL; message 'hasDetailsMode';
  end;
{$endif}

{$ifdef CLASSES}

type
  IKImageEditPanel = objcclass external (NSPanel)
  private
    _privateData: pointer;
  public
    procedure setDataSource(newValue: IKImageEditPanelDataSourceProtocol); message 'setDataSource:';
    function dataSource: IKImageEditPanelDataSourceProtocol; message 'dataSource';
    function filterArray: NSArray; message 'filterArray';
    class function sharedImageEditPanel: IKImageEditPanel; message 'sharedImageEditPanel';
    procedure reloadData; message 'reloadData';
  end;
{$endif}

{$endif}
