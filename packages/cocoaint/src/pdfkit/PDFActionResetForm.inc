{ Parsed from PDFKit.framework PDFActionResetForm.h }


{$ifdef TYPES}
type
  PDFActionResetFormPtr = ^PDFActionResetForm;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFActionResetForm = objcclass external (PDFAction, NSCopyingProtocol)
  private
    _pdfPriv2: PDFActionResetFormPrivateVars;
  public
    function init: id; message 'init';
    function fields: NSArray; message 'fields';
    procedure setFields (fields_: NSArray); message 'setFields:';
    function fieldsIncludedAreCleared: ObjCBOOL; message 'fieldsIncludedAreCleared';
    procedure setFieldsIncludedAreCleared (include: ObjCBOOL); message 'setFieldsIncludedAreCleared:';
  end;
{$endif}

{$endif}
