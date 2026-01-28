{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{*
 *  version.h
 *}

const
  SND_LIB_MAJOR       = 1;        { major number of library version }
  SND_LIB_MINOR       = 1;        { minor number of library version }
  SND_LIB_SUBMINOR    = 0;       { subminor number of library version }
  SND_LIB_EXTRAVER    = 1000000;  { extra version number, used mainly for betas }
{ library version }
  SND_LIB_VERSION     = (SND_LIB_MAJOR shl 16) or (SND_LIB_MINOR shl 8) or SND_LIB_SUBMINOR;
{ library version (string) }
  SND_LIB_VERSION_STR = '1.1.0';
