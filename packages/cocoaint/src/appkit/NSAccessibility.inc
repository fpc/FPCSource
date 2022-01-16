{ Parsed from AppKit.framework NSAccessibility.h }

{$ifdef CLASSES}

type
  NSAccessibility = objccategory external (NSObject)
    function accessibilityAttributeNames: NSArray;                                                                                              message 'accessibilityAttributeNames';
    function accessibilityAttributeValue (attribute: NSString): id; message 'accessibilityAttributeValue:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    function accessibilityIsAttributeSettable (attribute: NSString): ObjCBOOL; message 'accessibilityIsAttributeSettable:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    procedure accessibilitySetValue_forAttribute (value: id; attribute: NSString); message 'accessibilitySetValue:forAttribute:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    function accessibilityParameterizedAttributeNames: NSArray; message 'accessibilityParameterizedAttributeNames';
    function accessibilityAttributeValue_forParameter (attribute: NSString; parameter: id): id; message 'accessibilityAttributeValue:forParameter:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    function accessibilityActionNames: NSArray; message 'accessibilityActionNames';
    function accessibilityActionDescription (action: NSString): NSString; message 'accessibilityActionDescription:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    procedure accessibilityPerformAction (action: NSString); message 'accessibilityPerformAction:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
    function accessibilityIsIgnored: ObjCBOOL; message 'accessibilityIsIgnored'; deprecated 'in 10_1, 10_10, "Use isAccessibilityElement instead"';
    function accessibilityHitTest (point: NSPoint): id; message 'accessibilityHitTest:';
    function accessibilityFocusedUIElement: id; message 'accessibilityFocusedUIElement';
    function accessibilityIndexOfChild (child: id): NSUInteger; message 'accessibilityIndexOfChild:';
    function accessibilityArrayAttributeCount (attribute: NSString): NSUInteger; message 'accessibilityArrayAttributeCount:';
    function accessibilityArrayAttributeValues_index_maxCount (attribute: NSString; index: NSUInteger; maxCount: NSUInteger): NSArray; message 'accessibilityArrayAttributeValues:index:maxCount:';
    function accessibilityNotifiesWhenDestroyed: ObjCBOOL; message 'accessibilityNotifiesWhenDestroyed';
  end;

type
  NSWorkspaceAccessibilityDisplay = objccategory external (NSWorkspace)
    function accessibilityDisplayShouldIncreaseContrast: ObjCBOOL; message 'accessibilityDisplayShouldIncreaseContrast';
    function accessibilityDisplayShouldDifferentiateWithoutColor: ObjCBOOL; message 'accessibilityDisplayShouldDifferentiateWithoutColor';
    function accessibilityDisplayShouldReduceTransparency: ObjCBOOL; message 'accessibilityDisplayShouldReduceTransparency';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSWorkspaceAccessibilityDisplayOptionsDidChangeNotification: NSString { available in 10_10 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSAccessibilityAdditions = objccategory external (NSObject)
    function accessibilitySetOverrideValue_forAttribute (value: id; attribute: NSString): ObjCBOOL; message 'accessibilitySetOverrideValue:forAttribute:'; deprecated 'in 10_1, 10_10, "Use the NSAccessibility protocol methods instead (see NSAccessibilityProtocols.h';
  end;
{$endif}

{$ifdef FUNCTIONS}
function NSAccessibilityFrameInView(parentView: NSView; frame: NSRect): NSRect; cdecl; external; { available in 10_10 }
function NSAccessibilityPointInView(parentView: NSView; point: NSPoint): NSPoint; cdecl; external; { available in 10_10 }
function NSAccessibilitySetMayContainProtectedContent(flag: ObjCBOOL): ObjCBOOL; cdecl; external;
function NSAccessibilityRoleDescription(role: NSString; subrole: NSString): NSString; cdecl; external;
function NSAccessibilityRoleDescriptionForUIElement(element: id): NSString; cdecl; external;
function NSAccessibilityActionDescription(action: NSString): NSString; cdecl; external;
procedure NSAccessibilityRaiseBadArgumentException(element: id; attribute: NSString; value: id); cdecl; external;
function NSAccessibilityUnignoredAncestor(element: id): id; cdecl; external;
function NSAccessibilityUnignoredDescendant(element: id): id; cdecl; external;
function NSAccessibilityUnignoredChildren(originalChildren: NSArray): NSArray; cdecl; external;
function NSAccessibilityUnignoredChildrenForOnlyChild(originalChild: id): NSArray; cdecl; external;
procedure NSAccessibilityPostNotification(element: id; notification: NSString); cdecl; external;
{$endif}

