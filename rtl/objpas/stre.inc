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
   
   SAbstractError = 'Abstract method called';
   SAccessDenied = 'Access denied';
   SAccessViolation = 'Access violation';
   SArgumentMissing = 'Missing argument in format "%s"';
   SDiskFull = 'Disk Full';
   SDivByZero = 'Division by zero';
   SEndOfFile = 'Read past end of file'; 
   SFileNotAssigned = 'File not assigned';
   SFileNotOpen = 'File not open';
   SFileNotOpenForInput = 'File not open for input';
   SFileNotOpenForOutput = 'File not open for output';
   SInvalidArgIndex = 'Invalid argument index in format "%s"';
   SInvalidDrive = 'Invalid drive specified';
   SInvalidFileHandle = 'Invalid file handle';
   SInValidFileName = 'Invalid filename';
   SInvalidFormat = 'Invalid format specifier : "%s"';
   SInvalidInput = 'Invalid input';
   SInvalidInteger = '"%s" is an invalid integer';
   SInvalidPointer = 'Invalid pointer operation';
   SOutOfMemory = 'Out of memory';
   SRangeError = 'Range check error';
   SInvalidCast = 'Invalid type cast';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
{
  $Log$
  Revision 1.4  1998-10-03 15:08:04  florian
    * EInvalidCast added (from runerror 219)

  Revision 1.3  1998/10/02 13:57:37  michael
  Format error now causes exception

  Revision 1.2  1998/10/02 13:00:09  michael
  + More RTL error handling

  Revision 1.1  1998/10/01 16:04:59  michael
  + Initial implementation

}   
