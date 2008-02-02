{*
 *  gwin32.inc
 *
 *  depends on gtypes.inc
 *}

{$ifdef WIN32}

{ Windows emulation stubs for common Unix functions
}

    const
      MAXPATHLEN = 1024;

    type

       Ppid_t = ^Tpid_t;
       Tpid_t = longint;

    { For some POSIX functions that are not provided by the MS runtime,
       we provide emulators in glib, which are prefixed with g_win32_.
      }
    function ftruncate(f:gint; size:guint):gint;cdecl;external gliblib name 'g_win32_ftruncate';

    function g_win32_ftruncate(f:gint; size:guint):gint;cdecl;external gliblib name 'g_win32_ftruncate';

    { The MS setlocale uses locale names of the form "English_United
       States.1252" etc. We want the Unixish standard form "en", "zh_TW"
       etc. This function gets the current thread locale from Windows and
       returns it as a string of the above form for use in forming file
       names etc. The returned string should be deallocated with g_free().
      }

    function g_win32_getlocale:Pgchar;cdecl;external gliblib name 'g_win32_getlocale';

    { Translate a Win32 error code (as returned by GetLastError()) into
       the corresponding message. The returned string should be deallocated
       with g_free().
      }
    function g_win32_error_message(error:gint):Pgchar;cdecl;external gliblib name 'g_win32_error_message';

    function g_win32_get_package_installation_directory(package:Pgchar; dll_name:Pgchar):Pgchar;cdecl;external gliblib name 'g_win32_get_package_installation_directory';

    function g_win32_get_package_installation_subdirectory(package:Pgchar; dll_name:Pgchar; subdir:Pgchar):Pgchar;cdecl;external gliblib name 'g_win32_get_package_installation_subdirectory';



{$endif}
