{
  $Id$

  Build file for the compiler units. This is also a unit so
  that there will be no linking
}
unit compunit;
interface
uses
  compiler,
  comphook,
  cpuinfo,
  browcol;

implementation
end.
{
  $Log$
  Revision 1.2  2001-04-25 22:38:38  peter
    * updates from fixes branch so fpcmake for Makefiles works

  Revision 1.1.2.2  2001/03/20 00:20:44  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.1  2001/01/07 22:32:33  peter
    * build all compiler units for IDE inclusion

}
