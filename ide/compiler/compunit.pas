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
  Revision 1.1  2001-08-04 11:30:27  peter
    * ide works now with both compiler versions

  Revision 1.1.2.2  2001/03/20 00:20:44  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.1  2001/01/07 22:32:33  peter
    * build all compiler units for IDE inclusion

}
