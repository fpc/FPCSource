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
   Revision 1.3  2000-02-09 16:44:15  peter
     * log truncated

   Revision 1.2  2000/01/07 16:46:05  daniel
     * copyright 2000

}
