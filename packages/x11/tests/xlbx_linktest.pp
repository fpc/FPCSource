{ this program just links all externals, declared in the xlbx unit }
program xlbx_linktest;
uses
  xlbx;
begin
  halt(0);
  XLbxQueryExtension(nil,nil,nil,nil);
  XLbxQueryVersion(nil,nil,nil);
  XLbxGetEventBase(nil);
end.
