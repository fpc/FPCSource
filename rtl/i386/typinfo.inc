{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same Functionality as the TypInfo Unit }
{ of Delphi                                                     }
{ ---------------------------------------------------------------------
  This include contains cpu-specific Low-level calling of methods.
  ---------------------------------------------------------------------}

{$ASMMODE ATT}

Function CallIntegerFunc(s: Pointer; Address: Pointer; Index, IValue: LongInt): Int64; assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     je .LINoPush
     movl IValue,%eax
     pushl %eax
  .LINoPush:
     push s
     // reset EDX for routines that return only EAX
     xorl %edx,%edx
     call Address
     // now the result is in EDX:EAX
  end;

Function CallIntegerProc(s : Pointer;Address : Pointer;Value : Integer; INdex,IValue : Longint) : Integer;assembler;
  asm
     // Push value to set
     movl Value,%eax
     pushl %eax
     // ? Indexed Procedure
     movl Index,%eax
     testl %eax,%eax
     je .LIPNoPush
     movl IValue,%eax
     pushl %eax
  .LIPNoPush:
     pushl s
     call Address
  end;

Function CallSingleFunc(s : Pointer; Address : Pointer;
  Index, IValue : Longint) : Single; assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     je .LINoPush
     movl IValue,%eax
     pushl %eax
  .LINoPush:
     pushl s
     call Address
     //
  end;

Function CallDoubleFunc(s : Pointer; Address : Pointer;
  Index, IValue : Longint) : Double; assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     je .LINoPush
     movl IValue,%eax
     pushl %eax
  .LINoPush:
     pushl s
     call Address
     //
  end;

Function CallExtendedFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint) : Extended;assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     je .LINoPush
     movl IValue,%eax
     pushl %eax
  .LINoPush:
     pushl s
     call Address
     //
  end;

Function CallBooleanFunc(s : Pointer;Address : Pointer; Index,IValue : Longint) : Boolean;assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     je .LBNoPush
     movl IValue,%eax
     pushl %eax
  .LBNoPush:
     pushl s
     call Address
  end;

// Assembler Functions can't have short stringreturn values.
// So we make a Procedure with var parameter.
// That's not true (FK)

Procedure CallSStringFunc(s : Pointer;Address : Pointer; INdex,IValue : Longint;
                        Var Res: Shortstring);assembler;
  asm
     // ? Indexed Function
     movl Index,%eax
     testl %eax,%eax
     jnz .LSSNoPush
     movl IValue,%eax
     pushl %eax
     // the result is stored in an invisible parameter
     pushl Res
  .LSSNoPush:
     pushl s
     call Address
  end;

Procedure CallSStringProc(s : Pointer;Address : Pointer;Const Value : ShortString; INdex,IVAlue : Longint);assembler;
  asm
     // Push value to set
     movl Value,%eax
     pushl %eax
     // ? Indexed Procedure
     movl Index,%eax
     testl %eax,%eax
     // BUG 1 (jnz)
     je .LSSPNoPush
     movl IValue,%eax
     pushl %eax
  .LSSPNoPush:
     // BUG 2 (push)
     pushl s
     call Address
  end;

{
  $Log$
  Revision 1.6  2003-09-08 18:21:37  peter
    * save edi,esi,ebx

  Revision 1.5  2003/03/29 16:55:56  michael
  + Patch from Mattias Gaertner for single typeinfo

  Revision 1.4  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
