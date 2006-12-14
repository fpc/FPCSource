{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Free Pascal development team

    Low level directory functions
    Nintendo DS does not have any drive, so no directory handling is needed.
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
procedure mkdir(const s : string);[IOCheck];
begin
end;

procedure rmdir(const s : string);[IOCheck];
begin
end;

procedure chdir(const s : string);[IOCheck];
begin
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
begin
end;



