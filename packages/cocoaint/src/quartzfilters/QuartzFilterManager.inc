{ Parsed from QuartzFilters.framework QuartzFilterManager.h }


{$ifdef TYPES}
type
  QuartzFilterViewPtr = ^QuartzFilterView;
  QuartzFilterManagerPtr = ^QuartzFilterManager;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  globalUpdateOK: Boolean; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  QuartzFilterView = objcclass external (NSView)
  private
  public
    procedure sizeToFit; message 'sizeToFit';
  end;

type
  QuartzFilterManager = objcclass external (NSObject)
  private
    _privateData: id;
  public
    class function filterManager: QuartzFilterManager; message 'filterManager';
    class function filtersInDomains (domains: NSArray): NSArray; message 'filtersInDomains:';
    function filterPanel: NSPanel; message 'filterPanel';
    function filterView: QuartzFilterView; message 'filterView';
    function selectedFilter: QuartzFilter; message 'selectedFilter';
    function selectFilter (filter: QuartzFilter): ObjCBOOL; message 'selectFilter:';
    procedure setDelegate (aDelegate: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function importFilter (filterProperties: NSDictionary): QuartzFilter; message 'importFilter:';
  end;


type
  QuartzFilterManagerDelegate = objccategory external (NSObject)
    procedure quartzFilterManager_didAddFilter (sender: QuartzFilterManager; filter: QuartzFilter); message 'quartzFilterManager:didAddFilter:';
    procedure quartzFilterManager_didRemoveFilter (sender: QuartzFilterManager; filter: QuartzFilter); message 'quartzFilterManager:didRemoveFilter:';
    procedure quartzFilterManager_didModifyFilter (sender: QuartzFilterManager; filter: QuartzFilter); message 'quartzFilterManager:didModifyFilter:';
    procedure quartzFilterManager_didSelectFilter (sender: QuartzFilterManager; filter: QuartzFilter); message 'quartzFilterManager:didSelectFilter:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQuartzFilterApplicationDomain: NSString; cvar; external;
  kQuartzFilterPDFWorkflowDomain: NSString; cvar; external;
  kQuartzFilterPrintingDomain: NSString; cvar; external;
  kQuartzFilterManagerDidAddFilterNotification: NSString; cvar; external;
  kQuartzFilterManagerDidRemoveFilterNotification: NSString; cvar; external;
  kQuartzFilterManagerDidModifyFilterNotification: NSString; cvar; external;
  kQuartzFilterManagerDidSelectFilterNotification: NSString; cvar; external;
{$endif}

