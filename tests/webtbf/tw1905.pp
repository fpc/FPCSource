{ %fail }

procedure proc_value_smallset(smallset : set of tsmallset);
   begin
     if [A_A,A_D] in smallset then
       global_u8bit := RESULT_U8BIT;
   end;

begin
end.
