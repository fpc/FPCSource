{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

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
   SAssertError = '%s (%s, line %d)';
   SAssertionFailed = 'Assertion failed';
   SDiskFull = 'Disk Full';
   SDivByZero = 'Division by zero';
   SEndOfFile = 'Read past end of file';
   SFileNotFound = 'File not found';
   SFileNotAssigned = 'File not assigned';
   SFileNotOpen = 'File not open';
   SFileNotOpenForInput = 'File not open for input';
   SFileNotOpenForOutput = 'File not open for output';
   SInvalidArgIndex = 'Invalid argument index in format "%s"';
   SInvalidDrive = 'Invalid drive specified';
   SInvalidFileHandle = 'Invalid file handle';
   SInValidFileName = 'Invalid filename';
   SInvalidFloat = '"%s" is an invalid float';
   SInvalidFormat = 'Invalid format specifier : "%s"';
   SInvalidInput = 'Invalid input';
   SInvalidInteger = '"%s" is an invalid integer';
   SInvalidPointer = 'Invalid pointer operation';
   SOutOfMemory = 'Out of memory';
   SRangeError = 'Range check error';
   SInvalidCast = 'Invalid type cast';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
   SOverflow = 'Floating point overflow';
   SUnderflow = 'Floating point underflow';
   SIntOverflow = 'Arithmetic overflow';
   SInvalidOp = 'Invalid floating point operation';
   SAbortError = 'Operation aborted';
   SExceptionErrorMessage = 'exception at %p';
{
  $Log$
  Revision 1.1  2000-07-13 06:31:01  michael
  + Initial import

  Revision 1.11  2000/06/22 18:05:18  michael
  + Added ExceptObject, ExceptAddr,ExceptionErrorMessage
     ShowException Abort; OutOfMemoryError; Beep;

  Revision 1.10  2000/02/09 16:59:32  peter
    * truncated log

  Revision 1.9  2000/01/07 16:41:44  daniel
    * copyright 2000

  Revision 1.8  1999/08/28 14:53:27  florian
    * bug 471 fixed: run time error 2 is now converted into a file not
      found exception

}   