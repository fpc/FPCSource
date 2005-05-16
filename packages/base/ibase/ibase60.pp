{
  $Id: ibase60.pp,v 1.7 2005/04/24 19:55:55 joost Exp $
}
unit ibase60;

{$UNDEF LinkDynamically}

{$i ibase60.inc}

end.
{
  $Log: ibase60.pp,v $
  Revision 1.7  2005/04/24 19:55:55  joost
  - Placed dyn. loaded and stat. loaded headers in one file

  Revision 1.6  2005/02/14 17:13:19  peter
    * truncate log

  Revision 1.5  2005/02/04 18:14:22  joost
  - replaced gdsdecl by extdecl for the codetools
  - cleanup of double declarations

  Revision 1.4  2005/01/12 10:23:34  michael
  * Changes from Joost van der Sluis to enable dynamic loading of the Interbase library

}
