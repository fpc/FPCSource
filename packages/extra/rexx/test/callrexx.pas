Uses
{$IFDEF OS2}
  DosCalls,
{$ENDIF OS2}
  RexxSAA;

Var
  Arg: RxString;                       // argument string for REXX
  RexxRetVal: RxString;                // return value from REXX
  RC: Cardinal;                        // return code from REXX
Const
  Str: PChar = 'These words will be swapped'; // text to swap
  RexxRc: Integer = 0;                // return code from function
Begin
  Write('This program will call the REXX interpreter ');
  WriteLn('to reverse the order of the');
  Write(#9'words in a string.  ');
  WriteLn('The interpreter is invoked with an initial');
  Write(#9'environment name of "FNC" ');
  WriteLn('and a file name of "backward.fnc"');

  // By setting the strlength of the output RXSTRING to zero, we
  // force the interpreter to allocate memory and return it to us.
  // We could provide a buffer for the interpreter to use instead.

  RexxRetVal.StrLength:=0;          // initialize return to empty

  MakeRxString(Arg, Str, StrLen(Str)); // create input argument

  // Here we call the interpreter.  We don't really need to use
  // all the casts in this call; they just help illustrate
  // the data types used.
  RC:=RexxStart(1,                // number of arguments
                addr(arg),        // array of arguments
                'backward.fnc',   // name of REXX file
                0,                // No INSTORE used
                'FNC',            // Command env. name
                RXSUBROUTINE,     // Code for how invoked
                0,                // No EXITs on this call
                rexxrc,           // Rexx program output
                rexxretval );     // Rexx program output

  WriteLn('Interpreter Return Code: ', RC);
  WriteLn('Function Return Code:    ', rexxrc);
  WriteLn('Original String:         ', arg.strptr);
  WriteLn('Backwards String:        ', StrPas(rexxretval.strptr));

{$IFDEF OS2}
  DosFreeMem(RexxRetVal.StrPtr);   // Release storage
{$ELSE OS2}
  {$WARNING Memory allocated for RexxRetVal.StrPtr^ should be freed using native API!}
{$ENDIF OS2}
End.
