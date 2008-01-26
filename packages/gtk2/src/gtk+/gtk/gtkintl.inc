{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{$ifndef __GTKINTL_H__}
{$define __GTKINTL_H__}
{$include "config.h"}
{$ifdef ENABLE_NLS}
{$include<libintl.inc}
function _(_String : longint) : longint;

{$ifdef gettext_noop}
function N_(_String : longint) : longint;

{$else}
function N_(_String : longint) : longint;

{$endif}
{$else}
{ NLS is disabled  }
function _(_String : longint) : longint;
function N_(_String : longint) : longint;
function textdomain(_String : longint) : longint;
function gettext(_String : longint) : longint;
function dgettext(Domain,_String : longint) : longint;
function dcgettext(Domain,_String,_Type : longint) : longint;
function bindtextdomain(Domain,Directory : longint) : longint;

{$endif}
{$endif}
function _(_String : longint) : longint;
begin
   _:=dgettext(GETTEXT_PACKAGE,_String);
end;

function N_(_String : longint) : longint;
begin
   N_:=gettext_noop(_String);
end;

function N_(_String : longint) : longint;
begin
   N_:=_String;
end;

function _(_String : longint) : longint;
begin
   _:=_String;
end;

function N_(_String : longint) : longint;
begin
   N_:=_String;
end;

function textdomain(_String : longint) : longint;
begin
   textdomain:=_String;
end;

function gettext(_String : longint) : longint;
begin
   gettext:=_String;
end;

function dgettext(Domain,_String : longint) : longint;
begin
   dgettext:=_String;
end;

function dcgettext(Domain,_String,_Type : longint) : longint;
begin
   dcgettext:=_String;
end;

function bindtextdomain(Domain,Directory : longint) : longint;
begin
   bindtextdomain:=Domain;
end;


