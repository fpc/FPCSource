{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Carl Eric Codere

    This unit handles the temporary variables stuff for m68k

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit tgen68k;

  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm,cpubase;

    type
       tregisterset = set of tregister;
       tpushed = array[R_D0..R_A6] of boolean;

    const
       { D2 to D5 usable as scratch registers }
       usablereg32 : byte = 4;
       { A2 to A4 usable as address registers }
       usableaddress: byte = 3;
       { FP2 to FP7 usable as FPU registers   }
       usablefloatreg : byte = 6;

    function getregister32 : tregister;
    procedure ungetregister32(r : tregister);
    { return a free 32-bit address register }
    function getaddressreg: tregister;

    procedure ungetregister(r : tregister);

    procedure cleartempgen;

    function getfloatreg: tregister;
    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }

    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);


    { pushs and restores registers }
    procedure pushusedregisters(var pushed : tpushed;b : word);
    procedure popusedregisters(const pushed : tpushed);

    procedure clearregistercount;
    procedure resetusableregisters;

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       usedinproc : word;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                            }
       reg_pushes : array[R_D0..R_A6] of longint;
       is_reg_var : array[R_D0..R_A6] of boolean;

  implementation


    function getusableaddr: byte;
    { Since address registers are different then data registers }
    { we check the unused register list to determine the number }
    { of address registers which are available.                 }
    var
      i: byte;
    Begin
      i:=0;
      if R_A2 in unused then
        Inc(i);
      if R_A3 in unused then
        Inc(i);
      if R_A4 in unused then
         Inc(i);
      getusableaddr:=i;
    end;

    procedure pushusedregisters(var pushed : tpushed;b : word);

      var
         r : tregister;

      begin
         { the following registers can be pushed }
         { D0, D1, D2, D3, D4, D5, D6, D7, A0    }
         { A1, A2, A3, A4                        }
         for r:=R_D2 to R_A4 do
           begin
              pushed[r]:=false;
              { if the register is used by the calling subroutine    }
              if ((b and ($800 shr word(r)))<>0) then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        { then save it on the stack }
                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,r,R_SPPUSH)));
                        { here was a big problem  !!!!!}
                        { you cannot do that for a register that is
                        globally assigned to a var
                        this also means that you must push it much more
                        often, but there must be a better way
                        maybe by putting the value back to the stack !! }
                        if not(is_reg_var[r]) then
                          unused:=unused+[r];
                        pushed[r]:=true;
                     end;
                end;
           end;
      end;

    procedure popusedregisters(const pushed : tpushed);

      var
         r : tregister;

      begin
         for r:=R_A4 downto R_D2 do
           if pushed[r] then
             begin
                exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SPPULL,r)));
                unused:=unused-[r];
             end;
      end;

    procedure ungetregister(r : tregister);

      begin
           ungetregister32(r)
      end;


    procedure del_reference(const ref : treference);

      begin
         if ref.isintvalue then
           exit;
         ungetregister(ref.base);
         ungetregister32(ref.index);
      end;

    procedure del_locref(const location : tlocation);

      begin
         if (location.loc<>loc_mem) and (location.loc<>loc_reference) then
           exit;
         if location.reference.isintvalue then
           exit;
         ungetregister(location.reference.base);
         ungetregister32(location.reference.index);
      end;

    procedure ungetregister32(r : tregister);

      begin
         if r in [R_D2,R_D3,R_D4,R_D5,R_D7] then
          begin
             unused:=unused+[r];
             inc(usablereg32);
          end
         else
         if r in [R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7] then
         begin
              unused:=unused+[r];
              inc(usablefloatreg);
         end
         else
         if r in [R_A2,R_A3,R_A4] then
           begin
              unused:=unused+[r];
              inc(usableaddress);
           end;
        { other registers are RESERVED and should not be freed }
      end;


    function getfloatreg: tregister;
    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }
    var
     i:tregister;
    begin
      dec(usablefloatreg);
      if usablefloatreg = 0 then
       Message(cg_f_internal_error_in_getfloatreg);
      for i:=R_FP2 to R_FP7 do
      begin
         if i in unused then
         begin
           unused := unused-[i];
           getfloatreg := i;
           exit;
         end;
      end;
      { if we are here, then there was an allocation failure }
      Message(cg_f_internal_error_in_getfloatreg);
    end;


    function getaddressreg: tregister;

     begin
         dec(usableaddress);
         if R_A2 in unused then
           begin
              unused:=unused-[R_A2];
              usedinproc:=usedinproc or ($800 shr word(R_A2));
              getaddressreg:=R_A2;
           end
         else
         if R_A3 in unused then
           begin
              unused:=unused-[R_A3];
              usedinproc:=usedinproc or ($800 shr word(R_A3));
              getaddressreg:=R_A3;
           end
         else
         if R_A4 in unused then
           begin
              unused:=unused-[R_A4];
              usedinproc:=usedinproc or ($800 shr word(R_A4));
              getaddressreg:=R_A4;
           end
         else
         begin
           internalerror(10);
         end;

     end;

    function getregister32 : tregister;
      begin
         dec(usablereg32);
         if R_D2 in unused then
           begin
              unused:=unused-[R_D2];
              usedinproc:=usedinproc or ($800 shr word(R_D2));
              getregister32:=R_D2;
           end
         else if R_D3 in unused then
           begin
              unused:=unused-[R_D3];
              usedinproc:=usedinproc or ($800 shr word(R_D3));
              getregister32:=R_D3;
           end
         else if R_D4 in unused then
           begin
              unused:=unused-[R_D4];
              usedinproc:=usedinproc or ($800 shr word(R_D4));
              getregister32:=R_D4;
           end
         else if R_D5 in unused then
           begin
             unused:=unused-[R_D5];
             usedinproc:=usedinproc or ($800 shr word(R_D5));
             getregister32:=R_D5;
           end
         else if R_D7 in unused then
           begin
             unused:=unused-[R_D7];
             usedinproc:=usedinproc or ($800 shr word(R_D7));
             getregister32:=R_D7;
           end
         else
         begin
          internalerror(10);
         end;
      end;

    procedure cleartempgen;

      begin
         unused:=usableregs;
         usablereg32:=c_usableregs;
         usableaddress:=getusableaddr;
      end;


   procedure clearregistercount;
     var
       regi : tregister;
     begin
       for regi:=R_D0 to R_A6 do
         begin
           reg_pushes[regi]:=0;
           is_reg_var[regi]:=false;
         end;
     end;



   procedure resetusableregisters;
     begin
       usableregs:=[R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,R_A0,R_A1,R_A2,R_A3,R_A4,
             R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7];
       c_usableregs:=4;
       usableaddress:=3;
       usablefloatreg:=6;
     end;




begin
  resetusableregisters;
end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:52  michael
  + removed logs

}
