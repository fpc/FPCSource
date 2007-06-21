program ts010022;

const
  EXCEPTIONCOUNT = 18;
  exception_names : array[0..EXCEPTIONCOUNT-1] of pchar = (
   'Division by Zero',
   'Debug',
   'NMI',
   'Breakpoint',
   'Overflow',
   'Bounds Check',
   'Invalid Opcode',
   'Coprocessor not available',
   'Double Fault',
   'Coprocessor overrun',
   'Invalid TSS',
   'Segment Not Present',
   'Stack Fault',
   'General Protection Fault',
   'Page fault',
   ' ',
   'Coprocessor Error',
   'Alignment Check');

    single_pchar : pchar = 'Alone test';

const filename  = 'ts010022.tmp';

var en : pchar;
    f : text;
    st : string;
begin
   assign(f,filename);
   rewrite(f);
   en:=single_pchar;
   Writeln(f,en);
   en:=exception_names[6];
   writeln(f,en);
   close(f);
   reset(f);
   readln(f,st);
   if st<>'Alone test' then halt(1);
   readln(f,st);
   if st<>'Invalid Opcode' then halt(1);
   close(f);
   erase(f);
end.
