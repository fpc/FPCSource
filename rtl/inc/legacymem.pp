unit legacymem;
// temporary unit to bridge all the getfpcheapstatus/getheapstatus/memavail
// problems in tests and demoes.

interface 

function getUsedbytes  :PtrInt;
function getTotalBytes :PtrInt;

implementation

{$ifndef HASGETHEAPSTATUS}
type
  THeapStatus = record
    MaxHeapSize,
    MaxHeapUsed,
    CurrHeapSize,
    CurrHeapUsed,
    CurrHeapFree  : ptrint;
  end;

{$endif HASGETHEAPSTATUS}

{$ifndef HASGETFPCHEAPSTATUS}
    type
      TFPCHeapStatus = THeapStatus;
{$endif HASGETFPCHEAPSTATUS}


{$ifndef HASGETHEAPSTATUS}
  procedure getheapstatus(var status:THeapStatus);
  begin
    fillchar(status,sizeof(status),0);
    status.MaxHeapSize:=HeapSize;
    status.MaxHeapUsed:=HeapSize-MemAvail;
    status.CurrHeapSize:=HeapSize;
    status.CurrHeapUsed:=HeapSize-MemAvail;
    status.CurrHeapFree:=MemAvail;
  end;
{$endif HASGETHEAPSTATUS}

{$ifndef HASGETFPCHEAPSTATUS}
    function GetFPCHeapStatus:TFPCHeapStatus;
    begin
      GetHeapStatus(GetFPCHeapStatus);
    end;
{$endif HASGETFPCHEAPSTATUS}

function getTotalBytes :PtrInt;

begin
  gettotalbytes:=GetFPCHeapStatus.CurrHeapsize;
end;

function getUsedBytes  :PtrInt;

begin
  getusedbytes:=GetFPCHeapStatus.CurrHeapsize;
end;

end.