{
    This file is part of the Free Pascal test suite.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    This program makes the compilation and
    execution of individual test sources.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit bench;


  interface

    function GetMicroSTicks : int64;

  implementation

    uses
      sysutils;

    function GetMicroSTicks : int64;
      var
         h,m,s,s1000 : word;
      begin
         decodetime(time,h,m,s,s1000);
         result:=(int64(h)*3600000+int64(m)*60000+int64(s)*1000+int64(s1000))*1000;
      end;


end.

