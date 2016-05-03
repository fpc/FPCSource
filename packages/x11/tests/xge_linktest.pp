{ this program just links all externals, declared in the xge unit }
program xge_linktest;
uses
  xge;
begin
  halt(0);
  XGEQueryExtension(nil, nil, nil);
  XGEQueryVersion(nil, nil, nil);
end.
