{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

 ****************************************************************************}


unit options;

interface

var
   inputfilename, outputfilename : string; { Filenames }
   LibFileName, unitname         : string; { external library name }
   UseLib,                    { Append external to implementation ?  }
   UseName,                   { Append 'libname name 'funcname ' }
   UsePPOinters,              { Use P instead of ^ for pointers    }
   EnumToConst,               { Write enumeration types as constants }
   Win32headers,              { allows dec_specifier }
   stripcomment,              { strip comments from inputfile }
   PrependTypes : Boolean;    { Print T in front of type names ?   }
   usevarparas : boolean;     { generate var parameters, when a pointer }
                              { is passed                               }
   includefile : boolean;     { creates an include file instead of a unit }
   palmpilot : boolean;       { handling of PalmOS SYS_CALLs }

Procedure ProcessOptions;

Implementation

Procedure Usage;

begin
  writeln ('Usage : ',paramstr(0),' [options]  filename');
  writeln ('        Where [options] is one or more of:');
  writeln ('        -o outputfilename  Specify the outputfilename');
  writeln ('        -l libname         Specify the library name for external.');
  writeln ('        -u unitname        Specify the name of the unit.');
  writeln ('        -t                 Prepend typedef type names with T');
  writeln ('        -p                 Use "P" instead of "^" for pointers.');
  writeln ('        -d                 Use external;');
  writeln ('        -D                 use external libname name ''func_name'';');
  writeln ('        -e                 change enum type to list of constants.');
  writeln ('        -s                 strip comments from inputfile.');
  writeln ('        -v                 replace pointer parameters by call by');
  writeln ('                           reference parameters');
  writeln ('        -w                 special for win32 headers');
  writeln ('        -i                 create include files (no unit header)');
  writeln ('        -x                 handle SYS_TRAP of PalmOS header files');
  halt (0); 
end;

Function ForceExtension(Const HStr,ext:String):String;
{
  Return a filename which certainly has the extension ext
  (no dot in ext !!)
}
var
  j : longint;
begin
  j:=length(Hstr);
  while (j>0) and (Hstr[j]<>'.') do
   dec(j);
  if j=0 then
   j:=255;
  ForceExtension:=Copy(Hstr,1,j-1)+'.'+Ext;
end;

Procedure ProcessOptions;

Var cp : string;
    I : longint;

Function GetNextParam (const Opt,Name : String) : string;

begin
 if i=paramcount then
   begin
   writeln ('Error : -',Opt,' : ',name,' expected');
   halt(1);
   end
 else
   begin
   GetNextParam:=paramstr(i+1);
   inc(i);
   end;
end;
    
begin
  if paramcount=0 then
    Usage;
  inputfilename:='';
  outputfilename:='';
  LibFileName:='';
  UnitName:='';
  UseLib:=False;
  UseName:=FAlse;
  StripComment:=False;
  UsePPointers:=False;
  EnumToCOnst:=False;
  usevarparas:=false;
  palmpilot:=false;
  includefile:=false;
  i:=1;
  while i<=paramcount do
    begin
    cp:=paramstr(i);
    if cp[1]='-' then
      case cp[2] of
      'o' : outputfilename:=GetNextParam('o','outputfilename');
      't' : PrependTypes := True;
      'p' : UsePPointers := True;
      'e' : EnumToConst  := True;
      'd' : UseLib       := True;
      'D' : begin
            UseLib       := True;
            usename      := True;
            end;
      's' : stripcomment:=true;
      'l' : LibFileName:=GetNextParam ('l','libname');
      'u' : UnitName:=GetNextParam ('u','unitname');
      'v' : usevarparas:=true;
      'i' : includefile:=true;
      'w' : begin
               Win32headers:=true;
               UseLib:=true;
               usename:=true;
               usevarparas:=true;
               LibFileName:='kernel32';
            end;
      'x' : palmpilot:=true;            
      else
        Writeln ('Illegal option : ',cp);
      end
    else
      begin { filename }
      if inputfilename<>'' then
        begin
        writeln ('Error : only one filename supported. Found also :',cp);
        halt(1);
        end;
      inputfilename:=cp;
      if outputfilename='' then
        outputfilename:=ForceExtension (inputfilename,'pp');
      end;
    inc(i);
    end;  
  If inputfilename='' then Usage;
  if UnitName='' then
    begin
    i:=pos('.',outputfilename)-1;
    if i<=0 then
      UnitName:=outputfilename
    else
      UnitName:=Copy(OutputFileName,1,i);
    end;
end;

end.

{
   $Log$
   Revision 1.2  2000-01-07 16:46:05  daniel
     * copyright 2000

   Revision 1.1  1999/05/12 16:11:39  peter
     * moved

   Revision 1.10  1999/04/08 20:47:02  florian
     * misplaced line in the help screen fixed

   Revision 1.9  1998/09/04 17:26:33  pierre
     * better packed field handling

   Revision 1.8  1998/08/05 15:50:10  florian
     * small problems with // comments fixed (invalid line counting)
     + SYS_TRAP support for PalmOS
     + switch -x for PalmOS
     + switch -i to generate include files instead of units

   Revision 1.7  1998/07/24 20:55:44  michael
   * Fixed some minor bugs in Pierres stuff

   Revision 1.6  1998/07/23 23:26:04  michael
   + added -D option instead of -d, restored old -d

   Revision 1.5  1998/06/08 08:13:47  pierre
     + merged version of h2pas
     + added -w for special win32 header directives

   Revision 1.4  1998/04/27 12:06:40  michael
   + Added GPL statement

   Revision 1.3  1998/04/24 18:23:46  florian
     + parameter -v added (replaces pointer parameters by call by reference
       parameters)
       void p(int *i) =>   procedure p(var i : longint);

}
