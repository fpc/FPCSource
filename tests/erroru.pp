unit erroru;

interface

  procedure error;
  
  procedure accept_error(num : longint);

  procedure require_error(num : longint);

implementation

const program_has_error : boolean = false;

procedure error;
begin
   Writeln('Error in ',paramstr(0));
   program_has_error:=true;
end;

const
  store_exitproc : pointer = nil;
  accepted_error_num : longint = 0;
  required_error_num : longint = 0;
  

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
   exitproc:=store_exitproc;
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

begin
   store_exitproc:=exitproc;
   exitproc:=@error_unit_exit;
end.
