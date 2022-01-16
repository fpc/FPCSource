{ Parsed from ImageKit.framework IKFilterBrowserView.h }


{$ifdef TYPES}
type
  IKFilterBrowserViewPtr = ^IKFilterBrowserView;
{$endif}

{$ifdef CLASSES}

type
  IKFilterBrowserView = objcclass external (NSView)
  private
    _actionButton: id;
    _addCollectionButton: id;
    _browser: id;
    _descriptionField: id;
    _previewView: id;
    _removeCollectionButton: id;
    _searchField: id;
    _OKButton: id;
    _CancelButton: id;
    _foundFilters: NSMutableArray;
    _options: NSDictionary;
    _showPreviewView: ObjCBOOL;
    _useNarrowLayout: ObjCBOOL;
    _modalDelegate: id;
    _priv:array[0..7] of pointer;
  public
    procedure setPreviewState (inState: ObjCBOOL); message 'setPreviewState:';
    function filterName: NSString; message 'filterName';
  end;
{$endif}

