{
    $Id: errno.inc,v 1.3 2005/02/14 17:13:21 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    BeOS POSIX compliant error codes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
{----- Error baselines ---------------------------------------}

    B_GENERAL_ERROR_BASE        =   -2147483647-1;
    B_OS_ERROR_BASE             =   B_GENERAL_ERROR_BASE + $1000;
    B_APP_ERROR_BASE            =   B_GENERAL_ERROR_BASE + $2000;
    B_INTERFACE_ERROR_BASE      =   B_GENERAL_ERROR_BASE + $3000;
    B_MEDIA_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $4000; {* - $41ff *}
    B_TRANSLATION_ERROR_BASE    =   B_GENERAL_ERROR_BASE + $4800; {* - $48ff *}
    B_MIDI_ERROR_BASE           =   B_GENERAL_ERROR_BASE + $5000;
    B_STORAGE_ERROR_BASE        =   B_GENERAL_ERROR_BASE + $6000;
    B_POSIX_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $7000;
    B_MAIL_ERROR_BASE           =   B_GENERAL_ERROR_BASE + $8000;
    B_PRINT_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $9000;
    B_DEVICE_ERROR_BASE         =   B_GENERAL_ERROR_BASE + $a000;

{--- Developer-defined errors start at (B_ERRORS_END+1)----}

    B_ERRORS_END        =       (B_GENERAL_ERROR_BASE + $ffff);

type
{----- General Errors ----------------------------------------}
tgeneralerrors=  (
        B_NO_MEMORY := B_GENERAL_ERROR_BASE,
        B_IO_ERROR,
        B_PERMISSION_DENIED,
        B_BAD_INDEX,
        B_BAD_TYPE,
        B_BAD_VALUE,
        B_MISMATCHED_VALUES,
        B_NAME_NOT_FOUND,
        B_NAME_IN_USE,
        B_TIMED_OUT,
    B_INTERRUPTED,
        B_WOULD_BLOCK,
    B_CANCELED,
        B_NO_INIT,
        B_BUSY,
        B_NOT_ALLOWED,

        B_ERROR := -1,
        B_OK := 0,
        B_NO_ERROR := 0
);

{----- Kernel Kit Errors -------------------------------------}
tkernelerror  = (
        B_BAD_SEM_ID := B_OS_ERROR_BASE,
        B_NO_MORE_SEMS,

        B_BAD_THREAD_ID := B_OS_ERROR_BASE + $100,
        B_NO_MORE_THREADS,
        B_BAD_THREAD_STATE,
        B_BAD_TEAM_ID,
        B_NO_MORE_TEAMS,

        B_BAD_PORT_ID := B_OS_ERROR_BASE + $200,
        B_NO_MORE_PORTS,

        B_BAD_IMAGE_ID := B_OS_ERROR_BASE + $300,
        B_BAD_ADDRESS,
        B_NOT_AN_EXECUTABLE,
        B_MISSING_LIBRARY,
        B_MISSING_SYMBOL,

        B_DEBUGGER_ALREADY_INSTALLED := B_OS_ERROR_BASE + $400
);


{----- Application Kit Errors --------------------------------}
tapperrors =
(
        B_BAD_REPLY := B_APP_ERROR_BASE,
        B_DUPLICATE_REPLY,
        B_MESSAGE_TO_SELF,
        B_BAD_HANDLER,
        B_ALREADY_RUNNING,
        B_LAUNCH_FAILED,
        B_AMBIGUOUS_APP_LAUNCH,
        B_UNKNOWN_MIME_TYPE,
        B_BAD_SCRIPT_SYNTAX,
        B_LAUNCH_FAILED_NO_RESOLVE_LINK,
        B_LAUNCH_FAILED_EXECUTABLE,
        B_LAUNCH_FAILED_APP_NOT_FOUND,
        B_LAUNCH_FAILED_APP_IN_TRASH,
        B_LAUNCH_FAILED_NO_PREFERRED_APP,
        B_LAUNCH_FAILED_FILES_APP_NOT_FOUND
);


{----- Storage Kit/File System Errors ------------------------}
tfserrors= (
        B_FILE_ERROR :=B_STORAGE_ERROR_BASE,
        B_FILE_NOT_FOUND,       { discouraged; use B_ENTRY_NOT_FOUND in new code }
        B_FILE_EXISTS,
        B_ENTRY_NOT_FOUND,
        B_NAME_TOO_LONG,
        B_NOT_A_DIRECTORY,
        B_DIRECTORY_NOT_EMPTY,
        B_DEVICE_FULL,
        B_READ_ONLY_DEVICE,
        B_IS_A_DIRECTORY,
        B_NO_MORE_FDS,
        B_CROSS_DEVICE_LINK,
        B_LINK_LIMIT,
        B_BUSTED_PIPE,
        B_UNSUPPORTED,
        B_PARTITION_TOO_SMALL
);


const

{***********************************************************************}
{                       POSIX ERROR DEFINITIONS                         }
{***********************************************************************}

    { The following constants are system dependent but must all exist }
    Sys_E2BIG       = (B_POSIX_ERROR_BASE + 1);
    Sys_EACCES      = ord(B_PERMISSION_DENIED);
    Sys_EAGAIN      = ord(B_WOULD_BLOCK);
    Sys_EBADF       = ord(B_FILE_ERROR);
    Sys_EBUSY       = ord(B_BUSY);
    Sys_ECHILD      = (B_POSIX_ERROR_BASE + 2);
    Sys_EDEADLK     = (B_POSIX_ERROR_BASE + 3);
    Sys_EDOM        = (B_POSIX_ERROR_BASE + 16);
    Sys_EEXIST      = ord(B_FILE_EXISTS);
    Sys_EFAULT      = ord(B_BAD_ADDRESS);
    Sys_EFBIG       = (B_POSIX_ERROR_BASE + 4);
    Sys_EINTR       = ord(B_INTERRUPTED);
    Sys_EINVAL      = ord(B_BAD_VALUE);
    Sys_EIO         = ord(B_IO_ERROR);
    Sys_EISDIR      = ord(B_IS_A_DIRECTORY);
    Sys_EMFILE      = ord(B_NO_MORE_FDS);
    Sys_EMLINK      = (B_POSIX_ERROR_BASE + 5);
    Sys_ENAMETOOLONG= ord(B_NAME_TOO_LONG);
    Sys_ENFILE      = (B_POSIX_ERROR_BASE + 6);
    Sys_ENODEV      = (B_POSIX_ERROR_BASE + 7);
    Sys_ENOENT      = ord(B_ENTRY_NOT_FOUND);
    Sys_ENOEXEC     = ord(B_NOT_AN_EXECUTABLE);
    Sys_ENOLCK      = (B_POSIX_ERROR_BASE + 8);
    Sys_ENOMEM      = ord(B_NO_MEMORY);
    Sys_ENOSPC      = ord(B_DEVICE_FULL);
    Sys_ENOSYS      = (B_POSIX_ERROR_BASE + 9);
    Sys_ENOTDIR     = ord(B_NOT_A_DIRECTORY);
    Sys_ENOTEMPTY   = ord(B_DIRECTORY_NOT_EMPTY);
    Sys_ENOTTY      = (B_POSIX_ERROR_BASE + 10);
    Sys_ENXIO       = (B_POSIX_ERROR_BASE + 11);
    Sys_EPERM       = ord(B_NOT_ALLOWED);
    Sys_EPIPE       = ord(B_BUSTED_PIPE);
    Sys_ERANGE      = (B_POSIX_ERROR_BASE + 17);
    Sys_EROFS       = ord(B_READ_ONLY_DEVICE);
    Sys_ESPIPE      = (B_POSIX_ERROR_BASE + 12);
    Sys_ESRCH       = (B_POSIX_ERROR_BASE + 13);
    Sys_ETIMEDOUT   = ord(B_TIMED_OUT);
    Sys_EXDEV       = ord(B_CROSS_DEVICE_LINK);

    {Sys_EBADMSG     =    realtime extension POSIX only   }
    {Sys_ECANCELED   =    async. I/O extension POSIX only }
    {Sys_EMSGSIZE    =    realtime extension POSIX only   }
    {Sys_EINPROGRESS =    async. I/O extension POSIX only }

{***********************************************************************}
{                   NON POSIX ERROR DEFINITIONS                         }
{***********************************************************************}
     sys_EFPOS           = (B_POSIX_ERROR_BASE + 14);
     sys_ESIGPARM        = (B_POSIX_ERROR_BASE + 15);
     sys_EPROTOTYPE      = (B_POSIX_ERROR_BASE + 18);
     sys_EPROTONOSUPPORT = (B_POSIX_ERROR_BASE + 19);
     sys_EPFNOSUPPORT    = (B_POSIX_ERROR_BASE + 20);
     sys_EAFNOSUPPORT    = (B_POSIX_ERROR_BASE + 21);
     sys_EADDRINUSE      = (B_POSIX_ERROR_BASE + 22);
     sys_EADDRNOTAVAIL   = (B_POSIX_ERROR_BASE + 23);
     sys_ENETDOWN        = (B_POSIX_ERROR_BASE + 24);
     sys_ENETUNREACH     = (B_POSIX_ERROR_BASE + 25);
     sys_ENETRESET       = (B_POSIX_ERROR_BASE + 26);
     sys_ECONNABORTED    = (B_POSIX_ERROR_BASE + 27);
     sys_ECONNRESET       = (B_POSIX_ERROR_BASE + 28);

     sys_EISCONN      = (B_POSIX_ERROR_BASE + 29);
     sys_ENOTCONN     = (B_POSIX_ERROR_BASE + 30);
     sys_ESHUTDOWN    = (B_POSIX_ERROR_BASE + 31);
     sys_ECONNREFUSED = (B_POSIX_ERROR_BASE + 32);
     sys_EHOSTUNREACH = (B_POSIX_ERROR_BASE + 33);
     sys_ENOPROTOOPT  = (B_POSIX_ERROR_BASE + 34);
     sys_ENOBUFS      = (B_POSIX_ERROR_BASE + 35);
     sys_EINPROGRESS  = (B_POSIX_ERROR_BASE + 36);
     sys_EALREADY     = (B_POSIX_ERROR_BASE + 37);

     sys_EWOULDBLOCK  = ord(B_WOULD_BLOCK);  {* BSD compatibility *}
     sys_ELOOP        = ord(B_LINK_LIMIT);


{
 $Log: errno.inc,v $
 Revision 1.3  2005/02/14 17:13:21  peter
   * truncate log

}
