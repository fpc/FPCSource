{ Parsed from PDFKit.framework PDFAnnotationChoiceWidget.h }


{$ifdef TYPES}
type
  PDFAnnotationChoiceWidgetPtr = ^PDFAnnotationChoiceWidget;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFAnnotationChoiceWidget = objcclass external (PDFAnnotation, NSCopyingProtocol)
  private
    _pdfPriv2: PDFAnnotationChoiceWidgetPrivateVars;
  public
    function stringValue: NSString; message 'stringValue';
    procedure setStringValue (value: NSString); message 'setStringValue:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor (color_: NSColor); message 'setBackgroundColor:';
    function font: NSFont; message 'font';
    procedure setFont (font_: NSFont); message 'setFont:';
    function fontColor: NSColor; message 'fontColor';
    procedure setFontColor (color_: NSColor); message 'setFontColor:';
    function fieldName: NSString; message 'fieldName';
    procedure setFieldName (name: NSString); message 'setFieldName:';
    function isListChoice: ObjCBOOL; message 'isListChoice';
    procedure setIsListChoice (isList: ObjCBOOL); message 'setIsListChoice:';
    function choices: NSArray; message 'choices';
    procedure setChoices (options: NSArray); message 'setChoices:';
  end;
{$endif}

{$endif}
