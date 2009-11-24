{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005-2009 by Michael Van Canneyt and David Zhang
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$define FPC_BASEUNIX_HAS_FPPIPE}
Function fppipe(var fildes : tfildes):cint;assembler;
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
// At present it is NOT IMPLEMENTED for MIPS
// simplified-work
// DavidZhang
end;
