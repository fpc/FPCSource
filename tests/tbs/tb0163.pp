{ Old file: tbs0194.pp }
{ @procedure var returns value in it instead of address !! OK 0.99.11 (PM) }

{$Q+}

type
   tproc = function : longint;

var
   f : tproc;
   fa : array [0..1] of tproc;

   function dummy : longint;
     begin
        dummy:=25;
     end;
const
   prog_has_errors : boolean = false;

   procedure Wrong(const s : string);
     begin
        writeln(s);
        prog_has_errors:=True;
     end;

Begin
   f:=@dummy;
   if f()<>25 then
     Wrong('f() does not call dummy !!');
   if pointer(@f)=pointer(@dummy) then
     Wrong('@f returns value of f !');
   if longint(f)=longint(@f) then
     Wrong('longint(@f)=longint(f) !!!!');
   if f<>@dummy then
     Wrong('f does not return the address of dummy');
   if longint(@f)=longint(@dummy) then
     Wrong('longint(@f) returns address of dummy instead of address of f');
   fa[0]:=@dummy;
   if longint(@f)=longint(@fa[0]) then
     Wrong('arrays of procvar also wrong');
   if longint(f)<>longint(fa[0]) then
     Wrong('arrays of procvar and procvars are handled differently !!');
   if prog_has_errors then
     Halt(1);
End.
