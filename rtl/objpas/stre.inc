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
   
   SAbortError = 'Operation aborted';
   SAbstractError = 'Abstract method called';
   SAccessDenied = 'Access denied';
   SAccessViolation = 'Access violation';
   SArgumentMissing = 'Missing argument in format "%s"';
   SAssertError = '%s (%s, line %d)';
   SAssertionFailed = 'Assertion failed';
   SDiskFull = 'Disk Full';
   SDivByZero = 'Division by zero';
   SEndOfFile = 'Read past end of file';
   SExceptionErrorMessage = 'exception at %p';
   SFileNotAssigned = 'File not assigned';
   SFileNotFound = 'File not found';
   SFileNotOpen = 'File not open';
   SFileNotOpenForInput = 'File not open for input';
   SFileNotOpenForOutput = 'File not open for output';
   SInValidFileName = 'Invalid filename';
   SIntOverflow = 'Arithmetic overflow';
   SInvalidArgIndex = 'Invalid argument index in format "%s"';
   SInvalidBoolean = '"%s" is not a valid boolean.';
   SInvalidCast = 'Invalid type cast';
   SInvalidDateTime = '%f is not a valid datee/time value.';
   SInvalidDrive = 'Invalid drive specified';
   SInvalidFileHandle = 'Invalid file handle';
   SInvalidFloat = '"%s" is an invalid float';
   SInvalidFormat = 'Invalid format specifier : "%s"';
   SInvalidInput = 'Invalid input';
   SInvalidInteger = '"%s" is an invalid integer';
   SInvalidOp = 'Invalid floating point operation';
   SInvalidPointer = 'Invalid pointer operation';
   SOutOfMemory = 'Out of memory';
   SOverflow = 'Floating point overflow';
   SRangeError = 'Range check error';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
   SUnderflow = 'Floating point underflow';
     
{
  $Log$
  Revision 1.3  2000-08-13 17:55:38  michael
  + Added some missing functions needed for variant support

  Revision 1.2  2000/07/13 11:33:51  michael
  + removed logs
 
}
