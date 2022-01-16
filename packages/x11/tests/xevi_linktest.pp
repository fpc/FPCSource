{ this program just links all externals, declared in the xevi unit }
program xevi_linktest;
uses
  xevi;
begin
  halt(0);
  XeviQueryExtension(nil);
  XeviQueryVersion(nil,nil,nil);
  XeviGetVisualInfo(nil,nil,0,nil,nil);
end.
