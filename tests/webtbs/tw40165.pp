{ %OPT=-O1 }

{ This test attempts to catch the incorrect optimisation that occurred
  sometimes when CMOV was allowed to use a normally-unsafe reference.
  The code in question borrows from TObject.GetInterfaceByStr, where
  the fault was first detected }

program tw40165;

{$mode objfpc} {$modeswitch advancedrecords}
type
    InterfaceEntry = record
        iid: ^pInt32;
        function GetIID: pInt32; inline;
    end;

    function InterfaceEntry.GetIID: pInt32;
    begin
        if Assigned(iid) then result := iid^ else result := nil;
    end;

var
  ieStore: InterfaceEntry = (iid: nil);
  ie: ^InterfaceEntry = @ieStore;

begin
  if Assigned(ie) and Assigned(ie^.GetIID) then
    begin
      writeln('FAIL - condition incorrect');
      halt(1);
    end;
  writeln('ok');
end.
