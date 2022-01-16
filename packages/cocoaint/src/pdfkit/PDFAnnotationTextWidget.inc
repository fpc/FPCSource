{ Parsed from PDFKit.framework PDFAnnotationTextWidget.h }


{ Types from PDFAnnotationTextWidget }
{$ifdef TYPES}




{$endif}


{$ifdef TYPES}
type
  PDFAnnotationTextWidgetPtr = ^PDFAnnotationTextWidget;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationTextWidget = objcclass external (PDFAnnotation, NSCopyingProtocol)
  private
    _pdfPriv2: PDFAnnotationTextWidgetPrivateVars;
  public
    function stringValue: NSString; message 'stringValue';
    procedure setStringValue (value: NSString); message 'setStringValue:';
    function attributedStringValue: NSAttributedString; message 'attributedStringValue';
    procedure setAttributedStringValue (value: NSAttributedString); message 'setAttributedStringValue:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor (color_: NSColor); message 'setBackgroundColor:';
    function rotation: cint; message 'rotation';
    procedure setRotation (rotation_: cint); message 'setRotation:';
    {$endif}
    function font: NSFont; message 'font';
    procedure setFont (font_: NSFont); message 'setFont:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function fontColor: NSColor; message 'fontColor';
    procedure setFontColor (color_: NSColor); message 'setFontColor:';
    {$endif}
    function alignment: NSTextAlignment; message 'alignment';
    procedure setAlignment (alignment_: NSTextAlignment); message 'setAlignment:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function maximumLength: NSUInteger; message 'maximumLength';
    procedure setMaximumLength (maxLen: NSUInteger); message 'setMaximumLength:';
    function fieldName: NSString; message 'fieldName';
    procedure setFieldName (name: NSString); message 'setFieldName:';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_9)}
    function isMultiline: ObjCBOOL; message 'isMultiline';
    procedure setIsMultiline (multiline: ObjCBOOL); message 'setIsMultiline:';
    {$endif}
  end;
{$endif}

