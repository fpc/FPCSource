{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondderef()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{                 secondtypeconv()                               }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS:                                                       }
{****************************************************************}
program tderef;

type
  plongint = ^longint;
  ttestarray = array[1..64] of byte;
  ptestarray = ^ttestarray;
  pbyte = ^byte;


procedure fail;
begin
  Writeln('Failed!');
  halt(1);
end;


var
 pl : plongint;
 parray : ptestarray;
 passed : boolean;
 ptr : pbyte;
 b: byte;
Begin
 Write('secondderef() test...');
 passed := true;
 new(pl);
 new(parray);
 { left : LOC_REFERENCE }
 pl^:= $F0F0;
 if pl^ <> $F0F0 then
   passed := false;
 FillChar(parray^,sizeof(ttestarray),0);
 ptr:=pbyte((longint(parray)+32));
 ptr^ := $A0;
 if parray^[33] <> $A0 then
   passed := false;
 { left : LOC_REGISTER }
 b:=(pbyte((longint(parray)+32))^);
 if b <> $A0 then
  passed := false;
 dispose(pl);
 dispose(parray);
 if passed then
   WriteLn('Success.')
 else
   Fail;
end.

{
   $Log$
   Revision 1.4  2002-09-07 15:40:56  peter
     * old logs removed and tabs fixed

   Revision 1.3  2002/04/13 21:04:46  carl
   * fixed stupid typo

   Revision 1.2  2002/03/05 21:56:02  carl
   * Adapted for automated testing

}
