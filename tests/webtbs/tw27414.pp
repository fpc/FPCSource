program fpc_procedural_type_bug;

{$mode objfpc}

type

 tproc = procedure(const aparam : string);

 tobj = class
   class procedure proc(const aparam : string); static;
 end;

var
  s: string;

class procedure tobj.proc(const aparam : string);
begin
  s:=aparam;
end;

var
 p : tproc;
begin
  p := @tobj.proc;
  p('abc');
  if s<>'abc' then
    halt(1);
end.
