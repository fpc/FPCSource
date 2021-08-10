program multibuf_linktest;
uses
  multibuf;
begin
  halt(0);
  XmbufQueryExtension(nil,nil,nil);
  XmbufGetVersion(nil,nil,nil);
  XmbufCreateBuffers(nil,0,0,0,0,nil);
  XmbufDestroyBuffers(nil,0);
  XmbufDisplayBuffers(nil,0,nil,0,0);
  XmbufGetWindowAttributes(nil,0,nil);
  XmbufChangeWindowAttributes(nil,0,0,nil);
  XmbufGetBufferAttributes(nil,0,nil);
  XmbufChangeBufferAttributes(nil,0,0,nil);
  XmbufGetScreenInfo(nil,0,nil,nil,nil,nil);
  XmbufCreateStereoWindow(nil,0,0,0,0,0,0,0,0,nil,0,nil,nil,nil);
  XmbufClearBufferArea(nil,0,0,0,0,0,0);
end.
