{ Parsed from InputMethodKit.framework IMKInputController.h }


{$ifdef TYPES}
type
  IMKInputControllerPtr = ^IMKInputController;
  IMKStateSettingProtocolPtr = ^IMKStateSettingProtocol;
  IMKMouseHandlingProtocolPtr = ^IMKMouseHandlingProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kIMKCommandMenuItemName: NSString; cvar; external;
  kIMKCommandClientName: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  IMKServerInput = objccategory external (NSObject)
    function inputText_key_modifiers_client (string_: NSString; keyCode: NSInteger; flags: NSUInteger; sender: id): ObjCBOOL; message 'inputText:key:modifiers:client:';
    function inputText_client (string_: NSString; sender: id): ObjCBOOL; message 'inputText:client:';
    function handleEvent_client (event: NSEvent; sender: id): ObjCBOOL; message 'handleEvent:client:';
    function didCommandBySelector_client (aSelector: SEL; sender: id): ObjCBOOL; message 'didCommandBySelector:client:';
    function composedString (sender: id): id; message 'composedString:';
    function originalString (sender: id): NSAttributedString; message 'originalString:';
    procedure commitComposition (sender: id); message 'commitComposition:';
    function candidates (sender: id): NSArray; message 'candidates:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  IMKStateSettingProtocol = objcprotocol external name 'IMKStateSetting'
    procedure activateServer (sender: id); message 'activateServer:';
    procedure deactivateServer (sender: id); message 'deactivateServer:';
    function valueForTag_client (tag: clong; sender: id): id; message 'valueForTag:client:';
    procedure setValue_forTag_client (value: id; tag: clong; sender: id); message 'setValue:forTag:client:';
    function modes (sender: id): NSDictionary; message 'modes:';
    function recognizedEvents (sender: id): NSUInteger; message 'recognizedEvents:';
    procedure showPreferences (sender: id); message 'showPreferences:';
  end;

type
  IMKMouseHandlingProtocol = objcprotocol external name 'IMKMouseHandling'
    function mouseDownOnCharacterIndex_coordinate_withModifier_continueTracking_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; keepTracking: pObjCBOOL; sender: id): ObjCBOOL; message 'mouseDownOnCharacterIndex:coordinate:withModifier:continueTracking:client:';
    function mouseUpOnCharacterIndex_coordinate_withModifier_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; sender: id): ObjCBOOL; message 'mouseUpOnCharacterIndex:coordinate:withModifier:client:';
    function mouseMovedOnCharacterIndex_coordinate_withModifier_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; sender: id): ObjCBOOL; message 'mouseMovedOnCharacterIndex:coordinate:withModifier:client:';
  end;
{$endif}

{$ifdef CLASSES}

type
  IMKInputController = objcclass external (NSObject, IMKStateSettingProtocol, IMKMouseHandlingProtocol)
  private
    _private: IMKInputControllerPrivate;
  public
    function initWithServer_delegate_client (server: IMKServer; delegate: id; inputClient: id): id; message 'initWithServer:delegate:client:';
    procedure updateComposition; message 'updateComposition';
    procedure cancelComposition; message 'cancelComposition';
    function compositionAttributesAtRange (range: NSRange): NSMutableDictionary; message 'compositionAttributesAtRange:';
    function selectionRange: NSRange; message 'selectionRange';
    function replacementRange: NSRange; message 'replacementRange';
    function markForStyle_atRange (style: NSInteger; range: NSRange): NSDictionary; message 'markForStyle:atRange:';
    procedure doCommandBySelector_commandDictionary (aSelector: SEL; infoDictionary: NSDictionary); message 'doCommandBySelector:commandDictionary:';
    procedure hidePalettes; message 'hidePalettes';
    function menu: NSMenu; message 'menu';
    function delegate: id; message 'delegate';
    procedure setDelegate (newDelegate: id); message 'setDelegate:';
    function server: IMKServer; message 'server';
    function client: id; message 'client';
    procedure inputControllerWillClose; message 'inputControllerWillClose';
    procedure annotationSelected_forCandidate (annotationString: NSAttributedString; candidateString: NSAttributedString); message 'annotationSelected:forCandidate:';
    procedure candidateSelectionChanged (candidateString: NSAttributedString); message 'candidateSelectionChanged:';
    procedure candidateSelected (candidateString: NSAttributedString); message 'candidateSelected:';

    { Adopted protocols }
    procedure activateServer (sender: id); message 'activateServer:';
    procedure deactivateServer (sender: id); message 'deactivateServer:';
    function modes (sender: id): NSDictionary; message 'modes:';
    function mouseDownOnCharacterIndex_coordinate_withModifier_continueTracking_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; keepTracking: pObjCBOOL; sender: id): ObjCBOOL; message 'mouseDownOnCharacterIndex:coordinate:withModifier:continueTracking:client:';
    function mouseMovedOnCharacterIndex_coordinate_withModifier_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; sender: id): ObjCBOOL; message 'mouseMovedOnCharacterIndex:coordinate:withModifier:client:';
    function mouseUpOnCharacterIndex_coordinate_withModifier_client (index: NSUInteger; point: NSPoint; flags: NSUInteger; sender: id): ObjCBOOL; message 'mouseUpOnCharacterIndex:coordinate:withModifier:client:';
    function recognizedEvents (sender: id): NSUInteger; message 'recognizedEvents:';
    procedure setValue_forTag_client (value: id; tag: clong; sender: id); message 'setValue:forTag:client:';
    procedure showPreferences (sender: id); message 'showPreferences:';
    function valueForTag_client (tag: clong; sender: id): id; message 'valueForTag:client:';
  end;
{$endif}

