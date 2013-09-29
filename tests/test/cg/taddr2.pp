{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondaddr()                                     }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program taddr;

{$ifdef fpc}
  {$mode tp}
{$endif}

   procedure testprocvar;
     begin
       WriteLn('Hello world!');
     end;

type
  tmyobj = object
    procedure writeit;
  end;


  procedure tmyobj.writeit;
   begin
       WriteLn('Salutations!');
   end;


const
 chararray : array[0..7] of char =
 ('A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G',
  #0
 );

var
 procptr: procedure;
 pcharptr : pchar;
 plongint : ^longint;
 long: longint;
 ptr: codepointer;
begin
 { Test procedure variable }
 procptr:=testprocvar;
 Write('Value should be ''Hello world!''...');
 procptr;
 { Test normal variable }
 pcharptr := @chararray;
 Inc(pcharptr,2);
 Write('Value should be ''CDEFG''...');
 WriteLn(pcharptr);
 long := $F0F0;
 plongint := @long;
 Write('Value should be 61680...');
 WriteLn(plongint^);
 { Test method pointers }
 { PROCVARLOAD = TRUE testing }
 { Write('Value should be ''Salutations!''...');}
 ptr:=@tmyobj.writeit;
{ ptr;}
end.
