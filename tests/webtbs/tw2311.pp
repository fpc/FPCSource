{ %CPU=i386 }
{ %OPT=-O1 -CX }
{ Source provided for Free Pascal Bug Report 2311 }
{ Submitted by "Michael Brown" on  2003-01-06 }
{ e-mail: emboss1@i4free.co.nz }
{ modified by Pierre Muller to get a good check }
{$goto on}

program Kernel;

procedure DisplayRAM;
begin
end;

const
  passes : longint = 0;
  i : longint = 0;
label
  end_label;
begin

  inc(passes);

  if passes>1 then
    goto end_label;
  DisplayRAM;

  while true do
  begin
    asm
      nop
    end;
    inc(i);
    Writeln(i);
    if i > 10 then
      break;
  end;
 end_label:
   if passes<>1 then
     begin
       writeln('Error in test 2311');
       halt(1);
     end;
end.
