var
  s : string;
begin
  s:={$ifdef fpc}'~[v]~'{$else}'~['#25']~'{$endif};
end.
