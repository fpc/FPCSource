{ Parsed from PDFKit.framework PDFAnnotationButtonWidget.h }


{ Types from PDFAnnotationButtonWidget }
{$ifdef TYPES}





{$endif}


{$ifdef TYPES}
type
  PDFAnnotationButtonWidgetPtr = ^PDFAnnotationButtonWidget;
{$endif}

{$ifdef TYPES}
type
  PDFWidgetControlType = NSInteger;
  PDFWidgetControlTypePtr = ^PDFWidgetControlType;

const
  kPDFWidgetUnknownControl = -1;
  kPDFWidgetPushButtonControl = 0;
  kPDFWidgetRadioButtonControl = 1;
  kPDFWidgetCheckBoxControl = 2;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationButtonWidget = objcclass external (PDFAnnotation, NSCopyingProtocol)
  private
    _pdfPriv2: PDFAnnotationButtonWidgetPrivateVars;
  public
    function controlType: PDFWidgetControlType; message 'controlType';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setControlType (type__: PDFWidgetControlType); message 'setControlType:';
    {$endif}
    function state: NSInteger; message 'state';
    procedure setState (value: NSInteger); message 'setState:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function isHighlighted: ObjCBOOL; message 'isHighlighted';
    {$endif}
    procedure setHighlighted (flag: ObjCBOOL); message 'setHighlighted:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor (color_: NSColor); message 'setBackgroundColor:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function allowsToggleToOff: ObjCBOOL; message 'allowsToggleToOff';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    procedure setAllowsToggleToOff (allowOff: ObjCBOOL); message 'setAllowsToggleToOff:';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function font: NSFont; message 'font';
    procedure setFont (font_: NSFont); message 'setFont:';
    function fontColor: NSColor; message 'fontColor';
    procedure setFontColor (color_: NSColor); message 'setFontColor:';
    function caption: NSString; message 'caption';
    procedure setCaption (name: NSString); message 'setCaption:';
    function fieldName: NSString; message 'fieldName';
    procedure setFieldName (name: NSString); message 'setFieldName:';
    function onStateValue: NSString; message 'onStateValue';
    procedure setOnStateValue (name: NSString); message 'setOnStateValue:';
    {$endif}
  end;
{$endif}

