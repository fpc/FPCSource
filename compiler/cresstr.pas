{
    $Id$
    Copyright (c) 1999 by the Free Pascal development team

    Handles resourcestrings

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
unit cresstr;

  interface

    procedure insertresourcestrings;
    procedure registerresourcestring(p : pchar;len : longint);
    function calc_resstring_hashvalue(p : pchar;len : longint) : longint;

  implementation

    uses
       aasm,verbose,files;

    const
       { we can use a static constant because we compile a program only once }
       { per compiler call                                                   }
       resstrcount : longint = 0;

    { calcs the hash value for a give resourcestring, len is }
    { necessary because the resourcestring can contain #0    }
    function calc_resstring_hashvalue(p : pchar;len : longint) : longint;

      begin
         calc_resstring_hashvalue:=12345678;
      end;

    procedure insertresourcestrings;

      begin
         if not(assigned(resourcestringlist)) then
           resourcestringlist:=new(paasmoutput,init);
         resourcestringlist^.insert(new(pai_const,init_32bit(resstrcount)));
         resourcestringlist^.insert(new(pai_symbol,initname_global('RESOURCESTRINGLIST')));
      end;

    procedure registerresourcestring(p : pchar;len : longint);

      var
         l1 : pasmlabel;
         s : pchar;

      begin
         { shall we generate a po file? }
         { !!!!!! not yet implemented   }

         { we don't need to generate consts in units }
         if current_module^.is_unit then
           exit;

         if not(assigned(resourcestringlist)) then
           resourcestringlist:=new(paasmoutput,init);

         inc(resstrcount);

         { an empty ansi string is nil! }
         if (p=nil) or (len=0) then
           resourcestringlist^.concat(new(pai_const,init_32bit(0)))
         else
           begin
              getdatalabel(l1);
              resourcestringlist^.concat(new(pai_const_symbol,init(l1)));
              { first write the maximum size }
              consts^.concat(new(pai_const,init_32bit(len)));
              { second write the real length }
              consts^.concat(new(pai_const,init_32bit(len)));
              { redondent with maxlength but who knows ... (PM) }
              { third write use count (set to -1 for safety ) }
              consts^.concat(new(pai_const,init_32bit(-1)));
              consts^.concat(new(pai_label,init(l1)));
              getmem(s,len+1);
              move(p^,s^,len);
              s[len]:=#0;
              consts^.concat(new(pai_string,init_length_pchar(s,len)));
              consts^.concat(new(pai_const,init_8bit(0)));
           end;
         resourcestringlist^.concat(new(pai_const,init_32bit(0)));
         resourcestringlist^.concat(new(pai_const,init_32bit(
           calc_resstring_hashvalue(p,len))));
      end;

end.
{
  $Log$
  Revision 1.1  1999-07-22 09:34:04  florian
    + initial revision

}