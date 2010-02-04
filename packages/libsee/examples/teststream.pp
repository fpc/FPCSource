program teststream;

{$mode objfpc}
{$H+}

uses
  Classes, libsee, mod_stream;

Var
  interp : TSEE_interpreter;
  ainput : PSEE_INPUT;
  res    : TSEE_Value;

const
   Program_text = 's = new Stream("test.txt",Stream.fmCreate); '+
                  's.Write("something"); '+
                  's.Free(); '+
                  'delete s;';

begin
  see_init;
  RegisterStreamModule;
  SEE_interpreter_init(@interp);
  ainput :=SEE_input_utf8(@interp, pchar(program_text));
  See_global_eval(@interp,ainput,@res);
  see_input_close(ainput);
end.

