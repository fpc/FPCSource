{
    $Id$

    Fake compiler unit
}
unit compiler;
interface

function Compile(const cmd:string):longint;

implementation
uses
  comphook;

function Compile(const cmd:string):longint;
begin
  do_comment(V_Info,'Fake Compiler');
  do_comment(V_Info,'Cmd = "'+cmd+'"');
end;

end.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.1  1998/12/10 23:54:28  peter
    * initial version of the FV IDE
    * initial version of a fake compiler

}
