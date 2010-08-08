{$t-}
type
  TElementValidator = object
    FElementDef: pointer;
    FCurCP: pointer;
    FFailed: Boolean;
  end; 

var
  FValidator: array of TElementValidator;
  i: longint;
begin
  i:=1;
  setlength(fvalidator,5);
  writeln(ptruint(pointer(@fvalidator[1])-pointer(@fvalidator[0])));
  { aligned }
  fvalidator[0].felementdef:=@FValidator;
  fvalidator[0].fcurcp:=@FValidator;
  { unaligned }
  fvalidator[1].felementdef:=@FValidator;
  fvalidator[1].fcurcp:=@FValidator;
  { unaligned }
  fvalidator[i].felementdef:=@FValidator;
  fvalidator[i].fcurcp:=@FValidator;
end.
