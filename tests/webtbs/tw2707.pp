{ Source provided for Free Pascal Bug Report 2707 }
{ Submitted by "Johannes Berg" on  2003-10-02 }
{ e-mail: johannes -at- sipsolutions -dot- de }
type
  PSSL_METHOD = pointer;
  PSSL_CTX = pointer;

var
  MN_SSL_CTX_new  : function(method: PSSL_METHOD): PSSL_CTX cdecl = nil;

begin
end.
