{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  const
     GTK_MAJOR_VERSION_CONST = 1;
     GTK_MINOR_VERSION_CONST = 2;
     GTK_MICRO_VERSION_CONST = 5;
     GTK_BINARY_AGE_CONST = 2;
     GTK_INTERFACE_AGE_CONST = 0;

function  GTK_CHECK_VERSION(major,minor,micro:longint):boolean;

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_CHECK_VERSION(major,minor,micro:longint):boolean;
begin
  GTK_CHECK_VERSION:=(GTK_MAJOR_VERSION_CONST>major) or
     ((GTK_MAJOR_VERSION_CONST=major) and (GTK_MINOR_VERSION_CONST>minor)) or
     ((GTK_MAJOR_VERSION_CONST=major) and (GTK_MINOR_VERSION_CONST=minor) and (GTK_MICRO_VERSION_CONST>=micro));
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.4  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.3  1999/05/10 15:19:21  peter
    * cdecl fixes

  Revision 1.2  1999/05/10 09:03:11  peter
    * gtk 1.2 port working

  Revision 1.1  1999/05/07 10:40:36  peter
    * first things for 1.2

}

