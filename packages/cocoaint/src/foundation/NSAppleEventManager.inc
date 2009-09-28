{ Parsed from Foundation.framework NSAppleEventManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSAPPLEEVENTMANAGER_PAS_H}
{$define NSAPPLEEVENTMANAGER_PAS_H}
type
  NSAppleEventManagerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSAPPLEEVENTMANAGER_PAS_T}
{$define NSAPPLEEVENTMANAGER_PAS_T}

{ Types }
type
  NSAppleEventManagerSuspensionID = __NSAppleEventManagerSuspensionPointer;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAPPLEEVENTMANAGER_PAS_R}
{$define NSAPPLEEVENTMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAPPLEEVENTMANAGER_PAS_F}
{$define NSAPPLEEVENTMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAPPLEEVENTMANAGER_PAS_S}
{$define NSAPPLEEVENTMANAGER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAppleEventManager = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLEEVENTMANAGER_PAS_C}
{$define NSAPPLEEVENTMANAGER_PAS_C}

{ NSAppleEventManager }
  NSAppleEventManager = objcclass(NSObject)
  private
    __isPreparedForDispatch: Boolean;
        _padding: array[0..2] of char;
    
  public
    class function alloc: NSAppleEventManager; message 'alloc';

    class function sharedAppleEventManager: NSAppleEventManager; message 'sharedAppleEventManager';
    procedure setEventHandler_andSelector_forEventClass_andEventID(handler: id; handleEventSelector: SEL; eventClass: AEEventClass; eventID: AEEventID); message 'setEventHandler:andSelector:forEventClass:andEventID:';
    procedure removeEventHandlerForEventClass_andEventID(eventClass: AEEventClass; eventID: AEEventID); message 'removeEventHandlerForEventClass:andEventID:';
    function dispatchRawAppleEvent_withRawReply_handlerRefCon(theAppleEvent: AppleEventPointer; theReply: AppleEventPointer; handlerRefCon: SRefCon): OSErr; message 'dispatchRawAppleEvent:withRawReply:handlerRefCon:';
    function currentAppleEvent: NSAppleEventDescriptor; message 'currentAppleEvent';
    function currentReplyAppleEvent: NSAppleEventDescriptor; message 'currentReplyAppleEvent';
    function suspendCurrentAppleEvent: NSAppleEventManagerSuspensionID; message 'suspendCurrentAppleEvent';
    function appleEventForSuspensionID(suspensionID: NSAppleEventManagerSuspensionID): NSAppleEventDescriptor; message 'appleEventForSuspensionID:';
    function replyAppleEventForSuspensionID(suspensionID: NSAppleEventManagerSuspensionID): NSAppleEventDescriptor; message 'replyAppleEventForSuspensionID:';
    procedure setCurrentAppleEventAndReplyEventWithSuspensionID(suspensionID: NSAppleEventManagerSuspensionID); message 'setCurrentAppleEventAndReplyEventWithSuspensionID:';
    procedure resumeWithSuspensionID(suspensionID: NSAppleEventManagerSuspensionID); message 'resumeWithSuspensionID:';
  end; external;

{$endif}
{$endif}
