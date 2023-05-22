{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team

    This unit contains some routines to get informations about the
    processor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit cpu;

{$ifdef symbian}
  {$define NO_ELF_SUPPORT}
{$endif}

  interface

    function VFPv4Support : Boolean;

  implementation

     var
       has_vfpv4_support,
       gothwcaps : boolean;
       hwcaps : dword;

     const
      AT_NULL         = 0;
      AT_HWCAPS       = 16;

    type
      TAuxiliaryValue = DWord;

      TInternalUnion = record
        a_val: DWord;           //* Integer value */
          {* We use to have pointer elements added here.  We cannot do that,
             though, since it does not work when using 32-bit definitions
             on 64-bit platforms and vice versa.  *}
      end;

      Elf32_auxv_t = record
        a_type: DWord;              //* Entry type */
        a_un: TInternalUnion;
      end;
      TElf32AuxiliaryVector = Elf32_auxv_t;
      PElf32AuxiliaryVector = ^TElf32AuxiliaryVector;

    var
      psysinfo: LongWord = 0;

    procedure InitHWCaps;
      var
        ep: PPChar;
        auxv: PElf32AuxiliaryVector;
      begin
        psysinfo := 0;
{$ifndef NO_ELF_SUPPORT} 
        ep := envp;
        while ep^ <> nil do
          Inc(ep);

        Inc(ep);

        auxv := PElf32AuxiliaryVector(ep);

        while auxv^.a_type <> AT_NULL do
          begin
            if auxv^.a_type = AT_HWCAPS then
              begin
                hwcaps := auxv^.a_un.a_val;
                gothwcaps := true;
                Break;
            end;
            Inc(auxv);
          end;
{$endif ndef NO_ELF_SUPPORT} 
      end;


    function VFPv4Support : Boolean;
      begin
        Result:=has_vfpv4_support;
      end;

begin
  gothwcaps:=false;
  InitHWCaps;
  has_vfpv4_support:=gothwcaps and ((hwcaps and (1 shl 16))<>0);
end.

