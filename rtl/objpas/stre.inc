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
   SDispatchError = 'No variant method call dispatch';
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
   SInvalidDateTime = '%f is not a valid date/time value.';
   SInvalidDrive = 'Invalid drive specified';
   SInvalidFileHandle = 'Invalid file handle';
   SInvalidFloat = '"%s" is an invalid float';
   SInvalidFormat = 'Invalid format specifier : "%s"';
   SInvalidGUID = '"%s" is not a valid GUID value';
   SInvalidInput = 'Invalid input';
   SInvalidInteger = '"%s" is an invalid integer';
   SInvalidOp = 'Invalid floating point operation';
   SInvalidPointer = 'Invalid pointer operation';
   SInvalidVarCast = 'Invalid variant type case';
   SInvalidVarOp = 'Invalid variant operation';
   SOutOfMemory = 'Out of memory';
   SOverflow = 'Floating point overflow';
   SRangeError = 'Range check error';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
   SUnderflow = 'Floating point underflow';
   SUnknownErrorCode = 'Unknown error code: %d';
   SVarArrayBounds = 'Variant array bounds error';
   SVarArrayCreate = 'Variant array cannot be created';
   SVarNotArray = 'Variant doesn''t contain an array';

{
  $Log$
  Revision 1.6  2002-01-25 17:42:03  peter
    * interface helpers

  Revision 1.5  2001/08/19 21:02:02  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

  Revision 1.4  2000/08/30 06:50:49  michael
  + Merged changes from fixes

  Revision 1.3  2000/08/13 17:55:38  michael
  + Added some missing functions needed for variant support

  Revision 1.2  2000/07/13 11:33:51  michael
  + removed logs

  Revision 1.1.2.1  2000/08/22 19:21:48  michael
  + Implemented syserrormessage. Made dummies for go32v2 and OS/2
  * Changed linux/errors.pp so it uses pchars for storage.
}
