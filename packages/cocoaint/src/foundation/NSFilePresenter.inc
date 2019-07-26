{ Parsed from Foundation.framework NSFilePresenter.h }


{$ifdef TYPES}
type
  NSFilePresenterProtocolPtr = ^NSFilePresenterProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSFilePresenterProtocol = objcprotocol external name 'NSFilePresenter' (NSObjectProtocol)
  required
    function presentedItemURL: NSURL; message 'presentedItemURL';
    function presentedItemOperationQueue: NSOperationQueue; message 'presentedItemOperationQueue';
  optional
    function primaryPresentedItemURL: NSURL; message 'primaryPresentedItemURL';
    procedure relinquishPresentedItemToReader (reader: OpaqueCBlock); message 'relinquishPresentedItemToReader:';
    procedure relinquishPresentedItemToWriter (writer: OpaqueCBlock); message 'relinquishPresentedItemToWriter:';
    procedure savePresentedItemChangesWithCompletionHandler (completionHandler: OpaqueCBlock); message 'savePresentedItemChangesWithCompletionHandler:';
    procedure accommodatePresentedItemDeletionWithCompletionHandler (completionHandler: OpaqueCBlock); message 'accommodatePresentedItemDeletionWithCompletionHandler:';
    procedure presentedItemDidMoveToURL (newURL: NSURL); message 'presentedItemDidMoveToURL:';
    procedure presentedItemDidChange; message 'presentedItemDidChange';
    procedure presentedItemDidGainVersion (version: NSFileVersion); message 'presentedItemDidGainVersion:';
    procedure presentedItemDidLoseVersion (version: NSFileVersion); message 'presentedItemDidLoseVersion:';
    procedure presentedItemDidResolveConflictVersion (version: NSFileVersion); message 'presentedItemDidResolveConflictVersion:';
    procedure accommodatePresentedSubitemDeletionAtURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock); message 'accommodatePresentedSubitemDeletionAtURL:completionHandler:';
    procedure presentedSubitemDidAppearAtURL (url: NSURL); message 'presentedSubitemDidAppearAtURL:';
    procedure presentedSubitemAtURL_didMoveToURL (oldURL: NSURL; newURL: NSURL); message 'presentedSubitemAtURL:didMoveToURL:';
    procedure presentedSubitemDidChangeAtURL (url: NSURL); message 'presentedSubitemDidChangeAtURL:';
    procedure presentedSubitemAtURL_didGainVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didGainVersion:';
    procedure presentedSubitemAtURL_didLoseVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didLoseVersion:';
    procedure presentedSubitemAtURL_didResolveConflictVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didResolveConflictVersion:';
  end;
{$endif}

