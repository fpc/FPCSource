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
  Revision 1.2  2002-09-07 15:42:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:10  peter
    * splitted to base and extra

}
