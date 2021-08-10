{ this program just links all externals, declared in the xag unit }
program xag_linktest;
uses
  xag;
begin
  halt(0);
  XagQueryVersion(nil,nil,nil);
  XagCreateEmbeddedApplicationGroup(nil,0,0,0,0,nil);
  XagCreateNonembeddedApplicationGroup(nil,nil);
  XagDestroyApplicationGroup(nil,0);
  XagGetApplicationGroupAttributes(nil,0,[]);
  XagQueryApplicationGroup(nil,0,nil);
  XagCreateAssociation(nil,nil,nil);
  XagDestroyAssociation(nil,0);
end.
