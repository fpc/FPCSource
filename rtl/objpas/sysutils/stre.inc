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
   SControlC = 'Control-C hit';
   SDiskFull = 'Disk Full';
   SDispatchError = 'No variant method call dispatch';
   SDivByZero = 'Division by zero';
   SEndOfFile = 'Read past end of file';
   SExceptionErrorMessage = 'exception at %p';
   SExternalException = 'External exception %x';   
   SFileNotAssigned = 'File not assigned';
   SFileNotFound = 'File not found';
   SFileNotOpen = 'File not open';
   SFileNotOpenForInput = 'File not open for input';
   SFileNotOpenForOutput = 'File not open for output';
   SInValidFileName = 'Invalid filename';
   SIntfCastError = 'Interface not supported';
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
   SPrivilege = 'Privileged instruction';
   SRangeError = 'Range check error';
   SSafecallException = 'Exception in safecall method';
   STooManyOpenFiles = 'Too many open files';
   SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
   SUnderflow = 'Floating point underflow';
   SUnknownErrorCode = 'Unknown error code: %d';
   SVarArrayBounds = 'Variant array bounds error';
   SVarArrayCreate = 'Variant array cannot be created';
   SVarNotArray = 'Variant doesn''t contain an array';

{
  $Log$
  Revision 1.1  2003-10-06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.8  2003/01/01 20:58:07  florian
    + added invalid instruction exception

  Revision 1.7  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/01/25 17:42:03  peter
    * interface helpers
}