{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$include h68kdos.inc}

procedure Error2InOutRes(errno: longint);
begin
  case errno of
    DOSE_NOENT,
    DOSE_NODIR,
    DOSE_MFILE,
    DOSE_ISDIR,
    DOSE_BADF,
    DOSE_ILGARG,
    DOSE_ILGDRV,
    DOSE_ISCURDIR:
      InOutRes:=-errno;

    DOSE_RDONLY:
      InOutRes:=105;
    DOSE_CANTSEEK:
      InOutRes:=156;

    else
      InOutRes:=word(errno);
  end;
end;
