{$inline on }
{$mode objfpc}


var
 global_u8bit : byte;
 value_u8bit : byte;


  procedure proc_value_smallarray_const_1_inline(arr : array of const);inline;
  var
   i: integer;
  begin
    for i:=0 to high(arr) do
     begin
       case arr[i].vtype of
        vtInteger : global_u8bit := arr[i].vinteger and $ff;
        else
          RunError(255);
       end;
     end; {endfor}
  end;


begin
  value_u8bit:=133;
  proc_value_smallarray_const_1_inline([value_u8bit]);
  if global_u8bit <> 133 then
    begin
      Writeln('Error in inline code generation');
      Halt(1);
    end;
end.
