{$packrecords 4}
type
  tr = record
    a,b : byte;
    l :longint
  end;

  SCSI_ADAPTER_BUS_INFO = record
    NumberOfBuses: char;
    BusData : array[0..0] of tr;
  end;

var
  p1,p2 : pchar;
  r : SCSI_ADAPTER_BUS_INFO;
begin
  p1:=@r.NumberOfBuses;
  p2:=@r.BusData;
  writeln(p2-p1);
  if p2-p1<>4 then
    halt(1);
end.
