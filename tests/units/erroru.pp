{$J+}
unit erroru;
interface

{$ifdef ver1_0}
type
  ptrint=longint;
  sizeint=longint;
{$endif}


  procedure do_error(l : longint);

  procedure error;

  procedure accept_error(num : longint);

  procedure require_error(num : longint);

{$ifndef HASGETHEAPSTATUS}
type
  THeapStatus = record
    MaxHeapSize,
    MaxHeapUsed,
    CurrHeapSize,
    CurrHeapUsed,
    CurrHeapFree  : ptrint;
  end;

  procedure getheapstatus(var status:THeapStatus);
{$endif HASGETHEAPSTATUS}

Procedure DoMem (Var StartMem : sizeint);


implementation

const
  program_has_error  : boolean = false;
  accepted_error_num : longint = 0;
  required_error_num : longint = 0;

procedure do_error(l : longint);
begin
  writeln('Error near: ',l);
  halt(100);
end;


procedure error;
begin
   Writeln('Error in ',paramstr(0));
   program_has_error:=true;
end;


procedure accept_error(num : longint);
begin
   accepted_error_num:=num;
end;


procedure require_error(num : longint);
begin
   required_error_num:=num;
   accepted_error_num:=num;
end;


procedure error_unit_exit;
begin
   if exitcode<>0 then
     begin
        if (required_error_num<>0) and (exitcode<>required_error_num) then
          begin
             Write('Program ',paramstr(0));
             Write(' exited with error ',exitcode,' whereas error ');
             Writeln(required_error_num,' was expected');
             Halt(1);
          end
        else if exitcode<>accepted_error_num then
          begin
             Write('Program ',paramstr(0));
             Write(' exited with error ',exitcode,' whereas only error ');
             Writeln(accepted_error_num,' was expected');
             Halt(1);
          end;
     end
   else if required_error_num<>0 then
     begin
        Write('Program ',paramstr(0));
        Write(' exited without error whereas error ');
        Writeln(required_error_num,' was expected');
        Halt(1);
     end;
   if program_has_error then
     Halt(1)
   else
     begin
        exitcode:=0;
        erroraddr:=nil;
     end;
end;

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


Procedure DoMem (Var StartMem : sizeint);
var
  hstatus : THeapstatus;
begin
  GetHeapStatus(hstatus);
  if StartMem<>0 then
    Writeln ('Used: ',hstatus.CUrrHeapUsed shr 10,'Kb, Lost ',hstatus.CurrHeapUsed-StartMem,' Bytes.');
  StartMem:=hstatus.CurrHeapUsed;
end;


initialization
finalization
  error_unit_exit;
end.
