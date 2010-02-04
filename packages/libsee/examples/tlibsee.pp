program tlibsee;

uses libsee;

Var
  Interp : TSEE_interpreter;
  ainput : PSEE_INPUT;
  res : TSEE_Value;
  
Const
  Program_text = 'Math.sqrt(3 + 4 * 7)+9;';
  
begin
  see_init;
  SEE_interpreter_init(@Interp);
  ainput :=SEE_input_utf8(@Interp, pchar(program_text));
  See_global_eval(@interp,ainput,@res);
  if (res._type=SEE_NUMBER) then
    Writeln('Result is : ',res.u.number)
  else
    Writeln('Result is not a number');
  see_input_close(ainput);
end.