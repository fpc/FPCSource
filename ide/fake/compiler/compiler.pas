{
    $Id$

    Fake compiler unit
}
unit compiler;
interface

function Compile(const cmd:string):longint;

const
       { do we need to link }
       IsExe : boolean = false;

implementation
uses
  comphook;

function Compile(const cmd:string):longint;
begin
  status.verbosity:=V_Default;
  status.maxerrorcount:=50;
  do_comment(V_Error,'Fake Compiler');
  do_comment(V_Error,'Cmd = "'+cmd+'"');
  Compile:=0;
end;

end.
{
  $Log$
  Revision 1.1  2000-07-13 09:48:33  michael
  + Initial import

  Revision 1.2  1999/11/18 13:38:11  pierre
   + IsExe var added

  Revision 1.1  1999/01/28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.3  1999/01/04 11:49:39  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:44:59  peter
    * use V_Error
    * set status.verbosity

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.1  1998/12/10 23:54:28  peter
    * initial version of the FV IDE
    * initial version of a fake compiler

}
