
{$mode objfpc}

uses
  sysutils;

const
  Program_has_errors : boolean = false;
  exception_called   : boolean = false;
  TestNumber : longint = 10000;

procedure test_exception(const s : string);
  begin
    if not(exception_called) then
      begin
        Writeln('Exception not called : ',s);
        Program_has_errors := true;
      end;
  end;

var
   i,j : longint;
   e : extended;
   exception_count : longint;
begin
   j:=0;
   i:=100;
   try
   exception_called:=false;
   e:=i/j;
   except
     on e : exception do
       begin
         Writeln('exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('division by zero for reals');
   try
   exception_called:=false;
   j := i div j;
   except
     on e : exception do
       begin
         Writeln('exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('division by zero for integers');
   exception_count:=0;
   for j:=1 to TestNumber do
     begin
       try
         i:=0;
         e:=j/i;
       except
         on e : exception do
           begin
             inc(exception_count);
           end;
       end;

     end;
   if exception_count<>TestNumber then
     begin
       program_has_errors:=true;
       Writeln('Could not generate ',TestNumber,' consecutive exceptions');
       Writeln('Only ',exception_count,' exceptions were generated');
     end;
   try
   exception_called:=false;
   i := -1;
   e := ln(i);
   except
     on e : exception do
       begin
         Writeln('exception called ',e.message);
         exception_called:=true;
       end;
   end;
   test_exception('ln(-1)');
   if program_has_errors then
     Halt(1);
end.