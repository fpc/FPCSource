{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    This file implements english error message strings
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  English string constants for any messages issued by the sysutils unit.
  Please have them ordered by constant name.
}

Const

   { Error messages for exceptions }
   
   SAccessDenied = 'Access denied';
   SDiskFull = 'Disk Full';
   SEndOfFile = 'Read past end of file'; 
   SInValidFileName = 'Invalid filename';
   SInvalidInput = 'Invalid input';
   SInvalidPointer = 'Invalid pointer operation';
   SOutOfMemory = 'Out of memory';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
   
   
{
  $Log$
  Revision 1.1  1998-10-01 16:04:59  michael
  + Initial implementation

}   