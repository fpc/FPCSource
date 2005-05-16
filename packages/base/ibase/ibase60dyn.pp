{
  $Id: ibase60dyn.pp,v 1.5 2005/04/24 19:55:55 joost Exp $


  Contains the Interbase/Firebird-functions calls
  In this stage only the calls needed for IBConnection are implemented
  Other calls could be simply implemented, using copy-paste from ibase60

  Call InitialiseIbase60 before using any of the calls, and call ReleaseIbase60
  when finished.
}

unit ibase60dyn;

{$DEFINE LinkDynamically}

{$i ibase60.inc}

end.

