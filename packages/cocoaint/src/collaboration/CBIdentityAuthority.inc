{ Parsed from Collaboration.framework CBIdentityAuthority.h }


{$ifdef TYPES}
type
  CBIdentityAuthorityPtr = ^CBIdentityAuthority;
{$endif}

{$ifdef CLASSES}

type
  CBIdentityAuthority = objcclass external (NSObject)
  private
    _reserved:array[0..1] of id;
  public
    class function localIdentityAuthority: CBIdentityAuthority; message 'localIdentityAuthority';
    class function managedIdentityAuthority: CBIdentityAuthority; message 'managedIdentityAuthority';
    class function defaultIdentityAuthority: CBIdentityAuthority; message 'defaultIdentityAuthority';
    class function identityAuthorityWithCSIdentityAuthority (CSIdentityAuthority: CSIdentityAuthorityRef): CBIdentityAuthority; message 'identityAuthorityWithCSIdentityAuthority:';
    function CSIdentityAuthority: CSIdentityAuthorityRef; message 'CSIdentityAuthority';
    function localizedName: NSString; message 'localizedName';
  end;
{$endif}

