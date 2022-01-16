{ Parsed from MapKit.framework MKAnnotationView.h }


{ Types from MKAnnotationView }
{$ifdef TYPES}





{$endif}


{$ifdef TYPES}
type
  MKAnnotationViewPtr = ^MKAnnotationView;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MKAnnotationCalloutInfoDidChangeNotification: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  MKAnnotationViewDragState = NSUInteger;
  MKAnnotationViewDragStatePtr = ^MKAnnotationViewDragState;

const
  MKAnnotationViewDragStateNone = 0;
  MKAnnotationViewDragStateStarting = 1;
  MKAnnotationViewDragStateDragging = 2;
  MKAnnotationViewDragStateCanceling = 3;
  MKAnnotationViewDragStateEnding = 4;
{$endif}

{$ifdef CLASSES}

type
  MKAnnotationView = objcclass external (NSView)
  public
    function initWithAnnotation_reuseIdentifier (annotation: MKAnnotationProtocol; reuseIdentifier: NSString): instancetype; message 'initWithAnnotation:reuseIdentifier:';
    function reuseIdentifier: NSString; message 'reuseIdentifier';
    procedure prepareForReuse; message 'prepareForReuse';
    procedure setAnnotation(newValue: MKAnnotationProtocol); message 'setAnnotation:';
    function annotation: MKAnnotationProtocol; message 'annotation';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setImage(newValue: UIImage); message 'setImage:';
    function image: UIImage; message 'image';
    {$else}
    procedure setImage_(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    {$endif}
    procedure setCenterOffset(newValue: CGPoint); message 'setCenterOffset:';
    function centerOffset: CGPoint; message 'centerOffset';
    procedure setCalloutOffset(newValue: CGPoint); message 'setCalloutOffset:';
    function calloutOffset: CGPoint; message 'calloutOffset';
    {$if  not defined(TARGET_OS_IPHONE)}
    procedure setLeftCalloutOffset(newValue: CGPoint); message 'setLeftCalloutOffset:';
    function leftCalloutOffset: CGPoint; message 'leftCalloutOffset';
    procedure setRightCalloutOffset(newValue: CGPoint); message 'setRightCalloutOffset:';
    function rightCalloutOffset: CGPoint; message 'rightCalloutOffset';
    {$endif}
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setHighlighted(newValue: ObjCBOOL); message 'setHighlighted:';
    function isHighlighted: ObjCBOOL; message 'isHighlighted';
    procedure setSelected(newValue: ObjCBOOL); message 'setSelected:';
    function isSelected: ObjCBOOL; message 'isSelected';
    procedure setSelected_animated (selected: ObjCBOOL; animated: ObjCBOOL); message 'setSelected:animated:';
    procedure setCanShowCallout(newValue: ObjCBOOL); message 'setCanShowCallout:';
    function canShowCallout: ObjCBOOL; message 'canShowCallout';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setLeftCalloutAccessoryView(newValue: UIViewPtr); message 'setLeftCalloutAccessoryView:';
    function leftCalloutAccessoryView: UIViewPtr; message 'leftCalloutAccessoryView';
    {$else}
    procedure setLeftCalloutAccessoryView_(newValue: NSView); message 'setLeftCalloutAccessoryView:';
    function leftCalloutAccessoryView_: NSView; message 'leftCalloutAccessoryView';
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    procedure setRightCalloutAccessoryView(newValue: UIViewPtr); message 'setRightCalloutAccessoryView:';
    function rightCalloutAccessoryView: UIViewPtr; message 'rightCalloutAccessoryView';
    {$else}
    procedure setRightCalloutAccessoryView_(newValue: NSView); message 'setRightCalloutAccessoryView:';
    function rightCalloutAccessoryView_: NSView; message 'rightCalloutAccessoryView';
    {$endif}
    procedure setDraggable(newValue: ObjCBOOL); message 'setDraggable:';
    function isDraggable: ObjCBOOL; message 'isDraggable';
    procedure setDragState(newValue: MKAnnotationViewDragState); message 'setDragState:';
    function dragState: MKAnnotationViewDragState; message 'dragState';
    procedure setDragState_animated (newDragState: MKAnnotationViewDragState; animated: ObjCBOOL); message 'setDragState:animated:'; { available in 10_9, 4_2 }
  end;
{$endif}

