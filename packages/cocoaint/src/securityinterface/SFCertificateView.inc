{ Parsed from SecurityInterface.framework SFCertificateView.h }


{$ifdef TYPES}
type
  SFCertificateViewPtr = ^SFCertificateView;
{$endif}

{$ifndef _SFCERTIFICATEVIEW_H_}
{$ifdef CLASSES}

type
  SFCertificateView = objcclass external (NSView)
  private
    _reserved_SFCertificateView: id;
  public
    procedure setCertificate (certificate: SecCertificateRef); message 'setCertificate:';
    function certificate: SecCertificateRef; message 'certificate';
    procedure setPolicies (policies: id); message 'setPolicies:';
    function policies: NSArray; message 'policies';
    procedure setEditableTrust (editable: ObjCBOOL); message 'setEditableTrust:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setDisplayTrust (display: ObjCBOOL); message 'setDisplayTrust:';
    function isTrustDisplayed: ObjCBOOL; message 'isTrustDisplayed';
    procedure saveTrustSettings; message 'saveTrustSettings';
    procedure setDisplayDetails (display: ObjCBOOL); message 'setDisplayDetails:';
    function detailsDisplayed: ObjCBOOL; message 'detailsDisplayed';
    procedure setDetailsDisclosed (disclosed: ObjCBOOL); message 'setDetailsDisclosed:';
    function detailsDisclosed: ObjCBOOL; message 'detailsDisclosed';
    procedure setPoliciesDisclosed (disclosed: ObjCBOOL); message 'setPoliciesDisclosed:';
    function policiesDisclosed: ObjCBOOL; message 'policiesDisclosed';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SFCertificateViewDisclosureStateDidChange: NSString; cvar; external;
{$endif}

{$endif}
