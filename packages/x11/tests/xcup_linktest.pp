{ this program just links all externals, declared in the xcup unit }
program xcup_linktest;
uses
  xcup;
begin
  halt(0);
  XcupQueryVersion(nil,nil,nil);
  XcupGetReservedColormapEntries(nil,0,nil,nil);
  XcupStoreColors(nil,0,nil,0);
end.
