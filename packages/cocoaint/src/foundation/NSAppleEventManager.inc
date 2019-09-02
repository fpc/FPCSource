{ Parsed from Foundation.framework NSAppleEventManager.h }


{$ifdef TYPES}
type
  NSAppleEventManagerPtr = ^NSAppleEventManager;
{$endif}

{$ifdef TYPES}
type
  NSAppleEventManagerSuspensionID = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAppleEventTimeOutDefault: double; cvar; external;
  NSAppleEventTimeOutNone: double; cvar; external;
  NSAppleEventManagerWillProcessFirstEventNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAppleEventManager = objcclass external (NSObject)
  private
    _isPreparedForDispatch: ObjCBOOL;
    _padding:array[0..2] of char;
  public
    class function sharedAppleEventManager: NSAppleEventManager; message 'sharedAppleEventManager';
    procedure setEventHandler_andSelector_forEventClass_andEventID (handler: id; handleEventSelector: SEL; eventClass: AEEventClass; eventID: AEEventID); message 'setEventHandler:andSelector:forEventClass:andEventID:';
    procedure removeEventHandlerForEventClass_andEventID (eventClass: AEEventClass; eventID: AEEventID); message 'removeEventHandlerForEventClass:andEventID:';
    function dispatchRawAppleEvent_withRawReply_handlerRefCon (theAppleEvent: AppleEventPtr; theReply: AppleEventPtr; handlerRefCon: SRefCon): OSErr; message 'dispatchRawAppleEvent:withRawReply:handlerRefCon:';
    function currentAppleEvent: NSAppleEventDescriptor; message 'currentAppleEvent';
    function currentReplyAppleEvent: NSAppleEventDescriptor; message 'currentReplyAppleEvent';
    function suspendCurrentAppleEvent: NSAppleEventManagerSuspensionID; message 'suspendCurrentAppleEvent';
    function appleEventForSuspensionID (suspensionID: NSAppleEventManagerSuspensionID): NSAppleEventDescriptor; message 'appleEventForSuspensionID:';
    function replyAppleEventForSuspensionID (suspensionID: NSAppleEventManagerSuspensionID): NSAppleEventDescriptor; message 'replyAppleEventForSuspensionID:';
    procedure setCurrentAppleEventAndReplyEventWithSuspensionID (suspensionID: NSAppleEventManagerSuspensionID); message 'setCurrentAppleEventAndReplyEventWithSuspensionID:';
    procedure resumeWithSuspensionID (suspensionID: NSAppleEventManagerSuspensionID); message 'resumeWithSuspensionID:';
  end;
{$endif}

