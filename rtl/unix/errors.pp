{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit errors;

Interface

uses strings,unixtype;

{$i errnostr.inc} // BSD or Linux ones

Function  StrError(err:cint):string;
Procedure PError(const s:string; Errno : cint);

Implementation

Function StrError(err:cint):string;
var s : string[12];
begin
  if (err<0) or (err>=sys_errn) then
   begin
     str(err,s);
     StrError:='Unknown Error ('+s+')';
   end
  else
   StrError:=StrPas(Sys_ErrList[err]);
end;


procedure PError(const s:string; Errno : cint);
begin
  WriteLn(stderr,s,': ',StrError(ErrNo));
end;

end.
