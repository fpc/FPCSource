{$mode objfpc}
unit runerrors;

Interface

Function GetRunError(Errno : Byte) : String;

Implementation

Resourcestring 
  RunNoError         = 'No error.';
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
  
Const 
  RunErrorArray : Array[0..255] of string = (
    { 0 } RunNoError,
    { 1 } RunOutOfMemory,
    { 2 } RunFileNotFound,
    { 3 } RunInvalidFileName,
    { 4 } RunTooManyOpenFiles,
    { 5 } RunAccessDenied,
    { 6 } RunInvalidFileHandle,
    { 7 } '',
    { 8 } '',
    { 9 } '',
    { 10 } '',
    { 11 } '',
    { 12 } '',
    { 13 } '',
    { 14 } '',
    { 15 } RunInvalidDrive,
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
    { 100 } RunEndOfFile,
    { 101 } RunDiskFull,
    { 102 } RunFileNotAssigned,
    { 103 } RunFileNotOpen,
    { 104 } RunFileNotOpenForInput,
    { 105 } RunFileNotOpenForOutput,
    { 106 } RunInvalidInput,
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
    { 200 } RunDivByZero,
    { 201 } RunRangeError,
    { 202 } '',
    { 203 } RunOutOfMemory,
    { 204 } RunInvalidPointer,
    { 205 } RunOverFlow,
    { 206 } RunUnderFlow,
    { 207 } RunInvalidOp,
    { 208 } '',
    { 209 } '',
    { 210 } '',
    { 211 } RunAbstractError,
    { 212 } '',
    { 213 } '',
    { 214 } '',
    { 215 } RunIntOverFlow,
    { 216 } RunAccessViolation,
    { 217 } RunPrivilege,
    { 218 } RunControlC,
    { 219 } RunInvalidCast,
    { 220 } RunInvalidVarCast,
    { 221 } RunInvalidVarOp,
    { 222 } RunDispatchError,
    { 223 } RunVarArrayCreate,
    { 224 } RunVarNotArray,
    { 225 } RunVarArrayBounds,
    { 226 } '',
    { 227 } RunAssertionFailed,
    { 228 } RunExternalException,
    { 229 } RunIntfCastError,
    { 230 } RunSafecallException,
    { 231 } RunExceptionStack,
    { 232 } RunThreadsNotSupported,
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
      Str(Errno,Result);
      Result:=RunUnknown+Result;
    end;
{$else}      
    Result:=RunUnknown+Str(Errno);
{$endif}    
end;

end.