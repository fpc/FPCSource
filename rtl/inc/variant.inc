{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This include file contains the implementation for variants
    support in FPC as far as it is part of the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure variant_init(var v : variant);[Public,Alias:'FPC_VARIANT_INIT'];

  begin
     variantmanager.varinit(v);
  end;

procedure variant_clear(var v : variant);[Public,Alias:'FPC_VARIANT_CLEAR'];

  begin
     variantmanager.varclear(v);
  end;

{ Integer }
operator :=(const source : byte) dest : variant;
  begin
  end;

operator :=(const source : shortint) dest : variant;
  begin
  end;

operator :=(const source : word) dest : variant;
  begin
  end;

operator :=(const source : smallint) dest : variant;
  begin
  end;

operator :=(const source : dword) dest : variant;
  begin
  end;

operator :=(const source : longint) dest : variant;
  begin
  end;

operator :=(const source : qword) dest : variant;
  begin
  end;

operator :=(const source : int64) dest : variant;
  begin
  end;

{ Boolean }
operator :=(const source : boolean) dest : variant;
  begin
  end;

operator :=(const source : wordbool) dest : variant;
  begin
  end;

operator :=(const source : longbool) dest : variant;
  begin
  end;

{ Chars }
operator :=(const source : char) dest : variant;
  begin
  end;

operator :=(const source : widechar) dest : variant;
  begin
  end;

{ Strings }
operator :=(const source : shortstring) dest : variant;
  begin
  end;

operator :=(const source : ansistring) dest : variant;
  begin
  end;

operator :=(const source : widestring) dest : variant;
  begin
  end;


{ Floats }
operator :=(const source : single) dest : variant;
  begin
  end;

operator :=(const source : double) dest : variant;
  begin
  end;

operator :=(const source : extended) dest : variant;
  begin
  end;

operator :=(const source : comp) dest : variant;
  begin
  end;

{ Misc. }
{ Fixme!!! 
operator :=(const source : currency) dest : variant;
  begin
  end;

operator :=(const source : tdatetime) dest : variant;
  begin
  end;
}
{**********************************************************************
                       from Variant assignments
 **********************************************************************}

{ Integer }
operator :=(const source : variant) dest : byte;
  begin     
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : shortint;
  begin
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : word;

  var
     l : longint;

  begin
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : smallint;

  var
     l : longint;

  begin
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : dword;
  begin
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : longint;
  begin
     dest:=variantmanager.vartoint(source);
  end;

operator :=(const source : variant) dest : qword;
  begin
     dest:=variantmanager.vartoword64(source);
  end;

operator :=(const source : variant) dest : int64;
  begin
     dest:=variantmanager.vartoint64(source);
  end;


{ Boolean }
operator :=(const source : variant) dest : boolean;
  begin
     dest:=variantmanager.vartobool(source);
  end;

operator :=(const source : variant) dest : wordbool;
  begin
     dest:=variantmanager.vartobool(source);
  end;

operator :=(const source : variant) dest : longbool;
  begin
     dest:=variantmanager.vartobool(source);
  end;

{ Chars }
operator :=(const source : variant) dest : char;
  begin
  end;

operator :=(const source : variant) dest : widechar;
  begin
  end;

{ Strings }
operator :=(const source : variant) dest : shortstring;
  begin
  end;

operator :=(const source : variant) dest : ansistring;
  begin
     variantmanager.vartolstr(dest,source);
  end;

operator :=(const source : variant) dest : widestring;
  begin
     variantmanager.vartowstr(dest,source);
  end;

{ Floats }
operator :=(const source : variant) dest : single;
  begin
     dest:=variantmanager.vartoreal(source);
  end;

operator :=(const source : variant) dest : double;
  begin
     dest:=variantmanager.vartoreal(source);
  end;

operator :=(const source : variant) dest : extended;
  begin
     dest:=variantmanager.vartoreal(source);
  end;

operator :=(const source : variant) dest : comp;
  begin
     dest:=comp(variantmanager.vartoreal(source));
  end;

{ Misc. }
{ FIXME !!!!!!!
operator :=(const source : variant) dest : currency;
  begin
     dest:=variantmanager.vartocurr(source);
  end;

operator :=(const source : variant) dest : tdatetime;
  begin
  end;
}

procedure invalidvariantop;
  begin
     Runerror(221);
  end;

procedure varclear(var v : tvardata);
begin
   if not(v.vtype in [varempty,varerror,varnull]) then
     invalidvariantop;
end;

procedure initvariantmanager;
  var
     i : longint;     
  begin
     for i:=0 to (sizeof(tvariantmanager) div sizeof(pointer))-1 do
       ppointer(@variantmanager+i*sizeof(pointer))^:=@invalidvariantop;
     pointer(variantmanager.varclear):=@varclear
  end;

{
  $Log$
  Revision 1.1  2001-08-19 21:02:01  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

}
