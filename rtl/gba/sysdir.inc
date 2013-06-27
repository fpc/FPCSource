{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Free Pascal development team

    Low level directory functions
    GBA does not have any drive, so no directory handling is needed.
    Copyright (c) 2006 by Francesco Lombardi
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_MKDIR'];
begin

end;

procedure rmdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_RMDIR'];
begin

end;

procedure chdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_CHDIR'];
begin

end;

procedure do_GetDir(DriveNr: byte; var Dir: RawByteString);
begin

end;

