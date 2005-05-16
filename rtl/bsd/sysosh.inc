{
    $Id: sysosh.inc,v 1.3 2005/04/13 20:10:50 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Platform specific information}
type
  { fd are int in C also for 64bit targets (x86_64) }
  THandle = Longint;
  TThreadID = THandle;
  
  { pthread_mutex_t }
  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = record
    __m_reserved: longint;
    __m_count: longint;
    __m_owner: pointer;
    __m_kind:  longint;
    __m_lock:  record
       __status: sizeint;
      __spinlock: longint;
    end;
  end;


{
   $Log: sysosh.inc,v $
   Revision 1.3  2005/04/13 20:10:50  florian
     + TThreadID

   Revision 1.2  2005/02/14 17:13:21  peter
     * truncate log

   Revision 1.1  2005/02/06 12:16:52  peter
     * bsd thread updates

}

