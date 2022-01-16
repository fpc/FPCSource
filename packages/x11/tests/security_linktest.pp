program security_linktest;
uses
  security;
begin
  halt(0);
  XSecurityQueryExtension(nil,nil,nil);
  XSecurityAllocXauth;
  XSecurityFreeXauth(nil);
  XSecurityGenerateAuthorization(nil,nil,0,nil,nil);
  XSecurityRevokeAuthorization(nil,0);
end.
