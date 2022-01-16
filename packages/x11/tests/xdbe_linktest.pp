program xdbe_linktest;
uses
  xdbe;
begin
  halt(0);
  XdbeQueryExtension(nil,nil,nil);
  XdbeAllocateBackBufferName(nil,0,0);
  XdbeDeallocateBackBufferName(nil,0);
  XdbeSwapBuffers(nil,nil,0);
  XdbeBeginIdiom(nil);
  XdbeEndIdiom(nil);
  XdbeGetVisualInfo(nil,nil,nil);
  XdbeFreeVisualInfo(nil);
  XdbeGetBackBufferAttributes(nil,0);
end.
