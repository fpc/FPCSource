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
      SExceptionStack = 'Exception stack error';
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
      SNoError = 'No error.';
      SNoThreadSupport = 'Threads not supported. Recompile program with thread driver.';
      SOutOfMemory = 'Out of memory';
      SOverflow = 'Floating point overflow';
      SPrivilege = 'Privileged instruction';
      SRangeError = 'Range check error';
      SSafecallException = 'Exception in safecall method';
      STooManyOpenFiles = 'Too many open files';
      SUnKnownRunTimeError = 'Unknown Run-Time error : %3.3d';
      SUnknown = 'Unknown run-time error code: ';
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

{
Resourcestring 
  RunOutOfMemory     = 'Runtime error 1';
  RunAbort           = 'Operation aborted';
  RunAbstractError   = 'Abstract method called';
  RunAccessDenied    = 'Access denied';
  RunAccessViolation = 'Access violation';
  RunAssertError     = '%s (%s, line %d)';
  RunAssertionFailed = 'Assertion failed';
  RunControlC = 'Control-C hit';
  RunDiskFull = 'Disk Full';
  RunDispatchError = 'No variant method call dispatch';
  RunDivByZero = 'Division by zero';
  RunEndOfFile = 'Read past end of file';
  RunExternalException = 'External exception.';   
  RunFileNotAssigned = 'File not assigned';
  RunFileNotFound = 'File not found';
  RunFileNotOpen = 'File not open';
  RunFileNotOpenForInput = 'File not open for input';
  RunFileNotOpenForOutput = 'File not open for output';
  RunInValidFileName = 'Invalid filename';
  RunIntfCastError = 'Interface not supported';
  RunIntOverflow = 'Arithmetic overflow';
  RunInvalidCast = 'Invalid type cast';
  RunInvalidDrive = 'Invalid drive specified';
  RunInvalidFileHandle = 'Invalid file handle';
  RunInvalidInput = 'Invalid input';
  RunInvalidOp = 'Invalid floating point operation';
  RunInvalidPointer = 'Invalid pointer operation';
  RunInvalidVarCast = 'Invalid variant type case';
  RunInvalidVarOp = 'Invalid variant operation';
  RunOverflow = 'Floating point overflow';
  RunPrivilege = 'Privileged instruction';
  RunRangeError = 'Range check error';
  RunSafecallException = 'Exception in safecall method';
  RunTooManyOpenFiles = 'Too many open files';
  RunUnderflow = 'Floating point underflow';
  RunUnknown = 'Unknown run-time error code: ';
  RunVarArrayBounds = 'Variant array bounds error';
  RunVarArrayCreate = 'Variant array cannot be created';
  RunVarNotArray = 'Variant doesn''t contain an array';
  RunExceptionStack = 'Exception stack error';
  RunThreadsNotSupported = 'Threading not supported by this binary. Recompile with thread driver.';
}
Function GetRunError(Errno : Byte) : String;

Implementation

Const 
  RunErrorArray : Array[0..255] of string = (
    { 0 } SNoError,
    { 1 } SOutOfMemory,
    { 2 } SFileNotFound,
    { 3 } SInvalidFileName,
    { 4 } STooManyOpenFiles,
    { 5 } SAccessDenied,
    { 6 } SInvalidFileHandle,
    { 7 } '',
    { 8 } '',
    { 9 } '',
    { 10 } '',
    { 11 } '',
    { 12 } '',
    { 13 } '',
    { 14 } '',
    { 15 } SInvalidDrive,
    { 16 } '',
    { 17 } '',
    { 18 } '',
    { 19 } '',
    { 20 } '',
    { 21 } '',
    { 22 } '',
    { 23 } '',
    { 24 } '',
    { 25 } '',
    { 26 } '',
    { 27 } '',
    { 28 } '',
    { 29 } '',
    { 30 } '',
    { 31 } '',
    { 32 } '',
    { 33 } '',
    { 34 } '',
    { 35 } '',
    { 36 } '',
    { 37 } '',
    { 38 } '',
    { 39 } '',
    { 40 } '',
    { 41 } '',
    { 42 } '',
    { 43 } '',
    { 44 } '',
    { 45 } '',
    { 46 } '',
    { 47 } '',
    { 48 } '',
    { 49 } '',
    { 50 } '',
    { 51 } '',
    { 52 } '',
    { 53 } '',
    { 54 } '',
    { 55 } '',
    { 56 } '',
    { 57 } '',
    { 58 } '',
    { 59 } '',
    { 60 } '',
    { 61 } '',
    { 62 } '',
    { 63 } '',
    { 64 } '',
    { 65 } '',
    { 66 } '',
    { 67 } '',
    { 68 } '',
    { 69 } '',
    { 70 } '',
    { 71 } '',
    { 72 } '',
    { 73 } '',
    { 74 } '',
    { 75 } '',
    { 76 } '',
    { 77 } '',
    { 78 } '',
    { 79 } '',
    { 80 } '',
    { 81 } '',
    { 82 } '',
    { 83 } '',
    { 84 } '',
    { 85 } '',
    { 86 } '',
    { 87 } '',
    { 88 } '',
    { 89 } '',
    { 90 } '',
    { 91 } '',
    { 92 } '',
    { 93 } '',
    { 94 } '',
    { 95 } '',
    { 96 } '',
    { 97 } '',
    { 98 } '',
    { 99 } '',
    { 100 } SEndOfFile,
    { 101 } SDiskFull,
    { 102 } SFileNotAssigned,
    { 103 } SFileNotOpen,
    { 104 } SFileNotOpenForInput,
    { 105 } SFileNotOpenForOutput,
    { 106 } SInvalidInput,
    { 107 } '',
    { 108 } '',
    { 109 } '',
    { 110 } '',
    { 111 } '',
    { 112 } '',
    { 113 } '',
    { 114 } '',
    { 115 } '',
    { 116 } '',
    { 117 } '',
    { 118 } '',
    { 119 } '',
    { 120 } '',
    { 121 } '',
    { 122 } '',
    { 123 } '',
    { 124 } '',
    { 125 } '',
    { 126 } '',
    { 127 } '',
    { 128 } '',
    { 129 } '',
    { 130 } '',
    { 131 } '',
    { 132 } '',
    { 133 } '',
    { 134 } '',
    { 135 } '',
    { 136 } '',
    { 137 } '',
    { 138 } '',
    { 139 } '',
    { 140 } '',
    { 141 } '',
    { 142 } '',
    { 143 } '',
    { 144 } '',
    { 145 } '',
    { 146 } '',
    { 147 } '',
    { 148 } '',
    { 149 } '',
    { 150 } '',
    { 151 } '',
    { 152 } '',
    { 153 } '',
    { 154 } '',
    { 155 } '',
    { 156 } '',
    { 157 } '',
    { 158 } '',
    { 159 } '',
    { 160 } '',
    { 161 } '',
    { 162 } '',
    { 163 } '',
    { 164 } '',
    { 165 } '',
    { 166 } '',
    { 167 } '',
    { 168 } '',
    { 169 } '',
    { 170 } '',
    { 171 } '',
    { 172 } '',
    { 173 } '',
    { 174 } '',
    { 175 } '',
    { 176 } '',
    { 177 } '',
    { 178 } '',
    { 179 } '',
    { 180 } '',
    { 181 } '',
    { 182 } '',
    { 183 } '',
    { 184 } '',
    { 185 } '',
    { 186 } '',
    { 187 } '',
    { 188 } '',
    { 189 } '',
    { 190 } '',
    { 191 } '',
    { 192 } '',
    { 193 } '',
    { 194 } '',
    { 195 } '',
    { 196 } '',
    { 197 } '',
    { 198 } '',
    { 199 } '',
    { 200 } SDivByZero,
    { 201 } SRangeError,
    { 202 } '',
    { 203 } SOutOfMemory,
    { 204 } SInvalidPointer,
    { 205 } SOverFlow,
    { 206 } SUnderFlow,
    { 207 } SInvalidOp,
    { 208 } '',
    { 209 } '',
    { 210 } '',
    { 211 } SAbstractError,
    { 212 } '',
    { 213 } '',
    { 214 } '',
    { 215 } SIntOverFlow,
    { 216 } SAccessViolation,
    { 217 } SPrivilege,
    { 218 } SControlC,
    { 219 } SInvalidCast,
    { 220 } SInvalidVarCast,
    { 221 } SInvalidVarOp,
    { 222 } SDispatchError,
    { 223 } SVarArrayCreate,
    { 224 } SVarNotArray,
    { 225 } SVarArrayBounds,
    { 226 } '',
    { 227 } SAssertionFailed,
    { 228 } SExternalException,
    { 229 } SIntfCastError,
    { 230 } SSafecallException,
    { 231 } SExceptionStack,
    { 232 } SNoThreadSupport,
    { 233 } '',
    { 234 } '',
    { 235 } '',
    { 236 } '',
    { 237 } '',
    { 238 } '',
    { 239 } '',
    { 240 } '',
    { 241 } '',
    { 242 } '',
    { 243 } '',
    { 244 } '',
    { 245 } '',
    { 246 } '',
    { 247 } '',
    { 248 } '',
    { 249 } '',
    { 250 } '',
    { 251 } '',
    { 252 } '',
    { 253 } '',
    { 254 } '',
    { 255 } ''
  );
  


Function GetRunError(Errno : Byte) : String;

begin
  Result:=RunErrorArray[Errno];
  If length(Result)=0 then
{$ifdef VER1_0}  
    begin
      Str(Errno:3,Result);
      Result:=SUnknown+Result;
    end;
{$else}      
    Result:=SUnknown+Str(Errno:3);
{$endif}    
end;

end.
{
  $Log$
  Revision 1.4  2003-11-27 20:39:43  michael
  + Added runerrors functionality to sysconst

  Revision 1.3  2003/11/26 20:34:19  michael
  + Some fixes to have everything compile again

  Revision 1.2  2003/11/26 20:00:19  florian
    * error handling for Variants improved

  Revision 1.1  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...
}