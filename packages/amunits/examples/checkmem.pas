program checkmem;

uses exec, amigados;

var
  chipfirst,
  chipsecond,
  fastfirst,
  fastsecond : longint;

begin

  if ParamCount <> 1 then begin
     writeln('Usage: CheckMem ProgramName');
     halt(10);
  end;

  chipfirst := AvailMem(MEMF_CHIP);
  fastfirst := AvailMem(MEMF_FAST);


  if Execute(ParamStr(1),0,0) then begin
     chipsecond := AvailMem(MEMF_CHIP);
     fastsecond := AvailMem(MEMF_FAST);

     writeln('Memory loss (Chip): ',chipsecond-chipfirst);
     writeln('Memory loss (Fast): ',fastsecond-fastfirst);
     halt;
  end else writeln('Could''t run the program');
end.
