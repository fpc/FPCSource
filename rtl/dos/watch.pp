{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit watch;

interface

uses DOS;

function TotalTime:string;
procedure StartTime;
procedure EndTime;

implementation

var
    h0,m0,s0,d0 : word;
    h1,m1,s1,d1 : word;
    h,m,s,d     : word;
function TotalTime:String;
var mm0,ss0,dd0:integer;
    st,temp:string;
begin
mm0:=m0;ss0:=s0;dd0:=d0;
if d1 < d0 then begin dd0:=dd0-100;inc(ss0);end;
d:=word(d1-dd0) ; str(d,temp);
if d<10 then st:='0'+temp else st:=temp;
st:='.'+st;

if s1 < ss0 then begin ss0:=ss0-60;inc(mm0);end;
s:=word(s1-ss0) ; str(s,temp);
if s<10 then st:='0'+temp+st else st:=temp+st;
st:=':'+st;

if m1 < mm0 then begin mm0:=mm0-60;inc(h0);end;
m:=word(m1-mm0) ; str(m,temp);
if m<10 then st:='0'+temp+st else st:=temp+st;
st:=':'+st;

h:=word(h1-h0) ; str(h,temp);
if h<10 then st:='0'+temp+st else st:=temp+st;

TotalTime:=st;
end;

procedure StartTime; begin Gettime(h0,m0,s0,d0); end;
procedure EndTime  ; begin Gettime(h1,m1,s1,d1); end;

end.
{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:41  root
  * Restored version

  Revision 1.3  1998/01/26 11:56:54  michael
  + Added log at the end


  
  Working file: rtl/dos/watch.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:15:49;  author: michael;  state: Exp;  lines: +13 -0
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
