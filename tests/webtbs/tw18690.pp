Procedure AllocFreeGiB;
  var
    p: pointer;
    RequestedSize: ptruint;
  Begin
    Writeln('-----------------------------');
    RequestedSize:=high(ptruint); // n GiB - 1  
    Writeln('RequestedSize = ',RequestedSize,' bytes');  
    getmem(p,RequestedSize);    
    if (p<>nil) then
      halt(1);
  End; 
    
Begin
  ReturnNilIfGrowHeapFails:=true;
  AllocFreeGiB;
End.
