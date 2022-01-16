{ Parsed from Foundation.framework NSProgress.h }


{ Types from NSProgress }
{$ifdef TYPES}

type
  NSProgressUnpublishingHandler = OpaqueCBlock;

type
  NSProgressPublishingHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  NSProgressPtr = ^NSProgress;
{$endif}

{$ifdef CLASSES}

type
  NSProgress = objcclass external (NSObject)
  private
    _group: id;
    _reserved4: cint64;
    _values: id;
    _reserved5: id;
    _cancellationHandler: OpaqueCBlock;
    _pausingHandler: OpaqueCBlock;
    _prioritizationHandler: OpaqueCBlock;
    _flags: cuint64;
    _userInfoProxy: id;
    _publisherID: NSString;
    _connection: NSXPCConnection;
    _unpublishingBlockageCount: NSInteger;
    _disconnectingBlockageCount: NSInteger;
    _remoteObserverCount: NSInteger;
    _acknowledgementHandlersByBundleID: NSMutableDictionary;
    _lastNotificationTimesByKey: NSMutableDictionary;
    _userInfoLastNotificationTimesByKey: NSMutableDictionary;
    _lock: NSLock;
    _childrenGroups: NSMutableSet;
  public
    class function currentProgress: NSProgress; message 'currentProgress';
    class function progressWithTotalUnitCount (unitCount: cint64): NSProgress; message 'progressWithTotalUnitCount:';
    function initWithParent_userInfo (parentProgressOrNil: NSProgress; userInfoOrNil: NSDictionary): instancetype; message 'initWithParent:userInfo:'; { NS_DESIGNATED_INITIALIZER }
    procedure becomeCurrentWithPendingUnitCount (unitCount: cint64); message 'becomeCurrentWithPendingUnitCount:';
    procedure resignCurrent; message 'resignCurrent';
    procedure setTotalUnitCount(newValue: cint64); message 'setTotalUnitCount:';
    function totalUnitCount: cint64; message 'totalUnitCount';
    procedure setCompletedUnitCount(newValue: cint64); message 'setCompletedUnitCount:';
    function completedUnitCount: cint64; message 'completedUnitCount';
    procedure setLocalizedDescription(newValue: NSString); message 'setLocalizedDescription:';
    function localizedDescription: NSString; message 'localizedDescription';
    procedure setLocalizedAdditionalDescription(newValue: NSString); message 'setLocalizedAdditionalDescription:';
    function localizedAdditionalDescription: NSString; message 'localizedAdditionalDescription';
    procedure setCancellable(newValue: ObjCBOOL); message 'setCancellable:';
    function isCancellable: ObjCBOOL; message 'isCancellable';
    procedure setPausable(newValue: ObjCBOOL); message 'setPausable:';
    function isPausable: ObjCBOOL; message 'isPausable';
    function isCancelled: ObjCBOOL; message 'isCancelled';
    function isPaused: ObjCBOOL; message 'isPaused';
    procedure setCancellationHandler(newValue: OpaqueCBlock); message 'setCancellationHandler:';
    function cancellationHandler: OpaqueCBlock; message 'cancellationHandler';
    procedure setPausingHandler(newValue: OpaqueCBlock); message 'setPausingHandler:';
    function pausingHandler: OpaqueCBlock; message 'pausingHandler';
    procedure setUserInfoObject_forKey (objectOrNil: id; key: NSString); message 'setUserInfoObject:forKey:';
    function isIndeterminate: ObjCBOOL; message 'isIndeterminate';
    function fractionCompleted: double; message 'fractionCompleted';
    procedure cancel; message 'cancel';
    procedure pause; message 'pause';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setKind(newValue: NSString); message 'setKind:';
    function kind: NSString; message 'kind';
    procedure publish; message 'publish'; { available in 10_9, NA }
    procedure unpublish; message 'unpublish'; { available in 10_9, NA }
    class function addSubscriberForFileURL_withPublishingHandler (url: NSURL; publishingHandler: NSProgressPublishingHandler): id; message 'addSubscriberForFileURL:withPublishingHandler:'; { available in 10_9, NA }
    class procedure removeSubscriber (subscriber: id); message 'removeSubscriber:'; { available in 10_9, NA }
    function isOld: ObjCBOOL; message 'isOld';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSProgressEstimatedTimeRemainingKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressThroughputKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressKindFile: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileOperationKindKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileOperationKindDownloading: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileOperationKindDecompressingAfterDownloading: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileOperationKindReceiving: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileOperationKindCopying: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileURLKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileTotalCountKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileCompletedCountKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSProgressFileAnimationImageKey: NSString { available in 10_9, NA }; cvar; external;
  NSProgressFileAnimationImageOriginalRectKey: NSString { available in 10_9, NA }; cvar; external;
  NSProgressFileIconKey: NSString { available in 10_9, NA }; cvar; external;
{$endif}

