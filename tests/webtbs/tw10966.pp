{$r+}

const
  ctnsNeedJITParsing  = 1 shl 1;
type
  TCodeTreeNodeSubDesc = word;    
var
  SubDesc: TCodeTreeNodeSubDesc;
  l: longint;
//  c: cardinal;
begin
  SubDesc := 1;
// fails
//  SubDesc := not 2;
  l := not(2);
// fails
//  c := not(2);
  l := not ctnsNeedJITParsing;
// fails
//  c := not ctnsNeedJITParsing;
  SubDesc := SubDesc and (not 2);
  SubDesc := SubDesc and (not (1 shl 1));
  SubDesc := SubDesc and (not ctnsNeedJITParsing);
end.

