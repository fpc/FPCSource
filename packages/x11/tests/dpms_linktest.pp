{ this program just links all externals, declared in the dpms unit }
program dpms_linktest;
uses
  dpms;
begin
  halt(0);
  DPMSQueryExtension(nil,nil,nil);
  DPMSGetVersion(nil,nil,nil);
  DPMSCapable(nil);
  DPMSSetTimeouts(nil,0,0,0);
  DPMSGetTimeouts(nil,nil,nil,nil);
  DPMSEnable(nil);
  DPMSDisable(nil);
  DPMSForceLevel(nil,0);
  DPMSInfo(nil,nil,nil);
end.
