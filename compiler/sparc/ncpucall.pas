{
    $Id: ncpucall.pas,v 1.16 2005/02/14 17:13:10 peter Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate sparc assembler for in call nodes

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
unit ncpucall;

{$i fpcdefs.inc}

interface

    uses
      ncgcal;

    type
       tsparccallnode = class(tcgcallnode)
         procedure extra_post_call_code;override;
       end;


implementation

    uses
      cpubase,
      aasmtai,
      aasmcpu,
      paramgr,
      ncal;


    procedure tsparccallnode.extra_post_call_code;
      begin
        if paramanager.ret_in_param(procdefinition.rettype.def,procdefinition.proccalloption) then
          exprasmlist.concat(taicpu.op_const(A_UNIMP,procdefinition.rettype.def.size and $fff));
      end;


begin
  ccallnode:=TSparcCallNode;
end.
{
  $Log: ncpucall.pas,v $
  Revision 1.16  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.15  2005/01/07 16:22:54  florian
    + implemented abi compliant handling of strucutured functions results on sparc platform

}

