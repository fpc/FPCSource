{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements some support functions for the parsing of directives
    and option strings

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

unit dirparse;

{$i fpcdefs.inc}

interface

    uses
      globtype;

    function UpdateTargetSwitchStr(s: string; var a: ttargetswitches; global: boolean): boolean;

implementation

    uses
      globals,
      cutils,
      symtable;

    function UpdateTargetSwitchStr(s: string; var a: ttargetswitches; global: boolean): boolean;
      var
        tok,
        value : string;
        setstr: string[2];
        equalspos: longint;
        doset,
        gotvalue,
        found : boolean;
        opt   : ttargetswitch;
      begin
        result:=true;
        repeat
          tok:=GetToken(s,',');
          if tok='' then
           break;
          setstr:=upper(copy(tok,length(tok),1));
          if setstr='-' then
            begin
              setlength(tok,length(tok)-1);
              doset:=false;
            end
          else
            doset:=true;
          { value specified? }
          gotvalue:=false;
          equalspos:=pos('=',tok);
          if equalspos<>0 then
            begin
              value:=copy(tok,equalspos+1,length(tok));
              delete(tok,equalspos,length(tok));
              gotvalue:=true;
            end;
          found:=false;
          uppervar(tok);
          for opt:=low(ttargetswitch) to high(ttargetswitch) do
            begin
              if TargetSwitchStr[opt].name=tok then
                begin
                  found:=true;
                  break;
                end;
            end;
          if found then
            begin
              if not global and
                 TargetSwitchStr[opt].isglobal then
                result:=false
              else if not TargetSwitchStr[opt].hasvalue then
                begin
                  if gotvalue then
                    result:=false;
                  if (TargetSwitchStr[opt].define<>'') and (doset xor (opt in a)) then
                    if doset then
                      def_system_macro(TargetSwitchStr[opt].define)
                    else
                      undef_system_macro(TargetSwitchStr[opt].define);
                  if doset then
                    include(a,opt)
                  else
                    exclude(a,opt)
                end
              else
                begin
                  if not gotvalue or
                     not doset then
                    result:=false
                  else
                    begin
                      case opt of
                        ts_auto_getter_prefix:
                          prop_auto_getter_prefix:=value;
                        ts_auto_setter_predix:
                          prop_auto_setter_prefix:=value;
                        else
                          begin
                            writeln('Internalerror 2012053001');
                            halt(1);
                          end;
                      end;
                    end;
                end;
            end
          else
            result:=false;
        until false;
      end;

end.
