{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit sysconst;

  interface

    resourcestring
      { from old str*.inc files }
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
      SNoThreadSupport = 'Threads not supported. Recompile program with thread driver.';
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
      SVarArrayLocked = 'Variant array locked';
      SVarNotArray = 'Variant doesn''t contain an array';
      SVarParamNotFound = 'Variant Parameter not found';
      SInvalidVarNullOp = 'Invalid NULL variant operation';
      SInvalidVarOpWithHResultWithPrefix = 'Invalid variant operation (%s%.8x)'+LineEnding+'%s';
      SVarTypeRangeCheck1 = 'Range check error for variant of type (%s)';
      SVarTypeRangeCheck2 = 'Range check error while converting variant of type (%s) into type (%s)';
      SVarTypeOutOfRangeWithPrefix = 'Custom variant type (%s%.4x) is out of range';
      SVarTypeAlreadyUsedWithPrefix = 'Custom variant type (%s%.4x) already used by %s';
      SVarTypeNotUsableWithPrefix = 'Custom variant type (%s%.4x) is not usable';
      SVarTypeTooManyCustom = 'Too many custom variant types have been registered';
      SVarTypeCouldNotConvert = 'Could not convert variant of type (%s) into type (%s)';
      SVarTypeConvertOverflow = 'Overflow while converting variant of type (%s) into type (%s)';
      SVarOverflow = 'Variant overflow';
      SVarInvalid = 'Invalid argument';
      SVarBadType = 'Invalid variant type';
      SVarNotImplemented = 'Operation not supported';
      SVarOutOfMemory = 'Variant operation ran out memory';
      SVarUnexpected = 'Unexpected variant error';
      
      SShortMonthNameJan = 'Jan';
      SShortMonthNameFeb = 'Feb';
      SShortMonthNameMar = 'Mar';
      SShortMonthNameApr = 'Apr';
      SShortMonthNameMay = 'May';
      SShortMonthNameJun = 'Jun';
      SShortMonthNameJul = 'Jul';
      SShortMonthNameAug = 'Aug';
      SShortMonthNameSep = 'Sep';
      SShortMonthNameOct = 'Oct';
      SShortMonthNameNov = 'Nov';
      SShortMonthNameDec = 'Dec';

      SLongMonthNameJan = 'January';
      SLongMonthNameFeb = 'February';
      SLongMonthNameMar = 'March';
      SLongMonthNameApr = 'April';
      SLongMonthNameMay = 'May';
      SLongMonthNameJun = 'June';
      SLongMonthNameJul = 'July';
      SLongMonthNameAug = 'August';
      SLongMonthNameSep = 'September';
      SLongMonthNameOct = 'October';
      SLongMonthNameNov = 'November';
      SLongMonthNameDec = 'December';

      SShortDayNameMon = 'Mon';
      SShortDayNameTue = 'Tue';
      SShortDayNameWed = 'Wed';
      SShortDayNameThu = 'Thu';
      SShortDayNameFri = 'Fri';
      SShortDayNameSat = 'Sat';
      SShortDayNameSun = 'Sun';

      SLongDayNameMon = 'Monday';
      SLongDayNameTue = 'Tuesday';
      SLongDayNameWed = 'Wednesday';
      SLongDayNameThu = 'Thursday';
      SLongDayNameFri = 'Friday';
      SLongDayNameSat = 'Saturday';
      SLongDayNameSun = 'Sunday';

  implementation

end.
{
  $Log$
  Revision 1.3  2003-11-26 20:34:19  michael
  + Some fixes to have everything compile again

  Revision 1.2  2003/11/26 20:00:19  florian
    * error handling for Variants improved

  Revision 1.1  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...
}