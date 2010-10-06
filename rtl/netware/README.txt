
    General
    =======

    Currently generating NetWare-NLM's only work under Linux and win32. (may work under other
    unix versions also but this is not tested yet)


    Binutils with netware-support needed
    ====================================

    You need a version of binutils compiled with netware-support. As of FreePascal 1.9.5 Nov 2004
    binutils needs to be named i386-netware-* (i.e. i386-netware-ld, i386-netware-nlmconv).
    Unfortunately in the Linux distibutions this component of the binutils is not included
    so you have to compile it. So download the latest stable binutils package from your
    favourite GNU mirror, decompress it ('tar xfz binutils-x.yy.z.tar.gz' on unices
    with GNU tar), change to the binutils-x.yy.z directory and configure:

      ./configure --prefix=/usr --enable-shared --enable-target=i386-netware

    I used the prefix /usr because thats the default location on redhat (thats what I'm using)

    and use

      make
      make install

    to build and install binutils. To check that netware is supported by the version of binutils
    installed, use i386-netware-ld --version. The emulation 'i386nw' must be present. Also check that 
    i386-netware-nlmconv
    is present and can be started without specifying the complete path of i386-netware-nlmconv.

    You can find more information and a binary version of binutils with netware-support for
    linux on:
           http://home.sch.bme.hu/~keresztg/novell/howto/NLM-Linux-HOWTO.html.

    Binutils for win32 and Fedora Core 2 are available from:
           ftp://ftp.freepascal.org/pub/fpc/contrib/cross
	  

    Building the freepascal runtime-library for netware
    ===================================================

    Install the current fpc sources from ftp.freepascal.org and change to the directory
    rtl/netware under the freepascal sourcetree. Verify the path of your units in
    Makefile. The default is /usr/lib/fpc/1.9.5/units/i386-netware/*.
    Compile and install the rtl with

      make install
      
    This will install the basic rtl files. To install all (packages,fcl and nlm's) do a
    
    make OS_TARGET=netware build
    
    and
    
    make OS_TARGET=netware install
    
    at the fpc source root dir.
    

    Settings and needed files to compile for netware
    ================================================

    Edit your /etc/fpc.cfg and add the rtl source path for netware. This are my settings,
    you may paste it to your fpc.cfg:

#IFDEF Netware_clib
  -Fu/usr/lib/fpc/1.9.5/units/i386-netware/*
  -Fl/usr/lib/fpc/1.9.5/units/i386-netware/rtl
#ENDIF

#IFDEF Netware_libc
  -Fu/usr/lib/fpc/1.9.5/units/i386-netwlibc/*
  -Fl/usr/lib/fpc/1.9.5/units/i386-netwlibc/rtl
  -XPi386-netware-
#ENDIF

    This adds the search path for the rtl-units as well as for the needed import-files.
    You can use the import files from the rtl/netware directory, they are automatically
    installed. If you want to use import files from novell, be aware that you have to
    convert the files to unix format (i.e. with dos2unix).

    Building the first nlm
    ======================

    Ok, now you have installed all needed files, try the following program and compile it
    with

      ppc386 -Tnetware hello.pas

    PROGRAM Hello;
    {$Description The FreePascal HelloWorld for Netware}
    {$Version 1.0.0}
    {$Copyright Copyright (c) 2001 The FreePascal Development Team}
    {$Screenname FPC Hello World for Netware}

    BEGIN
      WriteLn ('This is open source, FreePascal for netware');
    END.

    Hints on using freepascal for nlm's
    ===================================

    - Compiler Switches for Netware
      -----------------------------
      The following compiler-swiches are supported for NetWare:
      $DESCRIPTION    : NLM-Description, will be displayed at load-time
      $M              : For Stack-Size. Heap-Size will be ignored
      $VERSION x.x.x  : Sets Major, Minor and Revision, Revision 0 is nothing, 1=a, 2=b ...
      $COPYRIGHT      : Sets Copyright, needs a patched nlmconv, patch is
                        available at the location for binutils-win32 shown
                        above.
      $SCREENNAME     : Sets the screen-name (i.e. shown in ctrl-esc screen)
                        $SCREENNAME DEFAULT : output to logger screen
			$SCREENNAME NONE    : no output at all (do not use this, writeln,
			                      even from a runtime error may crash the sever)
			$SCREENNAME MyScreen: Name the screen "MyScreen"
      $THREADNAME     : Sets the thread name (dont use names that are to long
                        for netware, that will prevent your nlm from loading)

    - Exports
      -------

      Exports will be handled like in win32:
      procedure bla; CDECL; EXPORT;
      begin
      end;

      exports bla name 'bla';

      Be aware that without Name 'bla' this will be exported in upper-case.

    - Netware import (.imp) files
      ---------------------------

      Import files are needed by nlmconv as with other netware linkers. FreePascal is
      searching import files via the specified library path (-Fl). If you plan to use
      import files from novell be aware that they have to be converted from CR/LF to
      LF only. The script 'convertimp' in rtl/netware/nwimp will do that.
      If a module name is specified in an import, the module is automatically
      declared as autoload by FreePascal.

      I.e. the following declaration needs nlmlib.imp and sets nlmlib.nlm as autoload:

        FUNCTION rmdir (path : PCHAR) : LONGINT; CDECL; EXTERNAL 'nlmlib.nlm' NAME 'rmdir';

      while the following declaration only imports the symbol without autoloading:

        FUNCTION rmdir (path : PCHAR) : LONGINT; CDECL; EXTERNAL;

      If nlmlib.nlm is not loaded while loading your nlm, you will get an error about
      unknown symbols.


    - Debugging
      ---------

      Debugging is possible with gdb on Netware 4.11, 5, 6 and 6.5.
      See http://home.arcor.de/armin.diehl/fpcnw/gdbnw.html for details
      

    - Netware SDK
      -----------

      Delphi declarations for the multiplattform api is available at
      http://developer.novell.com. You can download the sdk after registering
      as a developer.
      The files are designed for win32 so they will not work off the box.
      I think changing the dll-name to the corrosponding nlm-name will work.
      i.e. in calwin32.imp the following declaration:

        function NWAbortServicingQueueJob2;  StdCall; external 'calwin32.dll' index 231;

       has to be changed to

        function NWAbortServicingQueueJob2;  CDecl; external 'calwin32.nlm';

armin@freepascal.org
