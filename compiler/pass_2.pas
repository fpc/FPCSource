{
    $Id: pass_2.pas,v 1.78 2005/04/08 15:18:08 peter Exp $
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit handles the codegeneration pass

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
unit pass_2;

{$i fpcdefs.inc}

interface

uses
   node;

    type
       tenumflowcontrol = (fc_exit,fc_break,fc_continue);
       tflowcontrol = set of tenumflowcontrol;

    var
       flowcontrol : tflowcontrol;

{ produces the actual code }
function do_secondpass(var p : tnode) : boolean;
procedure secondpass(var p : tnode);


implementation

   uses
{$ifdef EXTDEBUG}
     cutils,
{$endif}
     globtype,systems,verbose,
     globals,
     paramgr,
     aasmtai,
     cgbase,
     nflw,cgobj;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

{$ifdef EXTDEBUG}
     procedure logsecond(ht:tnodetype; entry: boolean);
       const
         secondnames: array[tnodetype] of string[13] =
            ('<emptynode>',
             'add-addn',  {addn}
             'add-muln',  {muln}
             'add-subn',  {subn}
             'moddiv-divn',      {divn}
             'add-symdifn',      {symdifn}
             'moddiv-modn',      {modn}
             'assignment',  {assignn}
             'load',        {loadn}
             'nothing-range',     {range}
             'add-ltn',  {ltn}
             'add-lten',  {lten}
             'add-gtn',  {gtn}
             'add-gten',  {gten}
             'add-equaln',  {equaln}
             'add-unequaln',  {unequaln}
             'in',    {inn}
             'add-orn',  {orn}
             'add-xorn',  {xorn}
             'shlshr-shrn',      {shrn}
             'shlshr-shln',      {shln}
             'add-slashn',  {slashn}
             'add-andn',  {andn}
             'subscriptn',  {subscriptn}
             'deref',       {derefn}
             'addr',        {addrn}
             'ordconst',    {ordconstn}
             'typeconv',    {typeconvn}
             'calln',       {calln}
             'noth-callpar',{callparan}
             'realconst',   {realconstn}
             'unaryminus',  {unaryminusn}
             'asm',         {asmn}
             'vecn',        {vecn}
             'pointerconst',{pointerconstn}
             'stringconst', {stringconstn}
             'not',         {notn}
             'inline',      {inlinen}
             'niln',        {niln}
             'error',       {errorn}
             'nothing-typen',     {typen}
             'setelement',  {setelementn}
             'setconst',    {setconstn}
             'blockn',      {blockn}
             'statement',   {statementn}
             'ifn',         {ifn}
             'breakn',      {breakn}
             'continuen',   {continuen}
             'while_repeat', {whilerepeatn}
             'for',         {forn}
             'exitn',       {exitn}
             'with',        {withn}
             'case',        {casen}
             'label',       {labeln}
             'goto',        {goton}
             'tryexcept',   {tryexceptn}
             'raise',       {raisen}
             'tryfinally',  {tryfinallyn}
             'on',    {onn}
             'is',    {isn}
             'as',    {asn}
             'error-caret',       {caretn}
             'add-starstar',  {starstarn}
             'arrayconstruc', {arrayconstructn}
             'noth-arrcnstr',     {arrayconstructrangen}
             'tempcreaten',
             'temprefn',
             'tempdeleten',
             'addoptn',
             'nothing-nothg',     {nothingn}
             'loadvmt',      {loadvmtn}
             'guidconstn',
             'rttin',
             'loadparentfpn'
             );
      var
        p: pchar;
      begin
        if entry then
          p := strpnew('second '+secondnames[ht]+' (entry)')
        else
          p := strpnew('second '+secondnames[ht]+' (exit)');
        exprasmlist.concat(tai_comment.create(p));
      end;
{$endif EXTDEBUG}

     procedure secondpass(var p : tnode);
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
      begin
         if not assigned(p) then
          internalerror(200208221);
         if not(nf_error in p.flags) then
          begin
            oldcodegenerror:=codegenerror;
            oldlocalswitches:=aktlocalswitches;
            oldpos:=aktfilepos;
            if not inlining_procedure then
              aktfilepos:=p.fileinfo;
            aktlocalswitches:=p.localswitches;
            codegenerror:=false;
{$ifdef EXTDEBUG}
            if (p.expectloc=LOC_INVALID) then
              Comment(V_Warning,'ExpectLoc is not set before secondpass: '+nodetype2str[p.nodetype]);
            if (p.location.loc<>LOC_INVALID) then
              Comment(V_Warning,'Location.Loc is already set before secondpass: '+nodetype2str[p.nodetype]);
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,true);
{$endif EXTDEBUG}
            p.pass_2;
{$ifdef EXTDEBUG}
            if (cs_asm_nodes in aktglobalswitches) then
              logsecond(p.nodetype,false);
            if (not codegenerror) then
             begin
               if (p.location.loc=LOC_INVALID) then
                 Comment(V_Warning,'Location not set in secondpass: '+nodetype2str[p.nodetype])
               else if (p.location.loc<>p.expectloc) then
                 Comment(V_Warning,'Location is different in secondpass: '+nodetype2str[p.nodetype]);
             end;
{$endif EXTDEBUG}
            if codegenerror then
              include(p.flags,nf_error);
            codegenerror:=codegenerror or oldcodegenerror;
            aktlocalswitches:=oldlocalswitches;
            aktfilepos:=oldpos;
          end
         else
           codegenerror:=true;
      end;


    function do_secondpass(var p : tnode) : boolean;
      begin
         { exprasmlist must be empty }
         if not exprasmlist.empty then
           internalerror(200405201);

         { clear errors before starting }
         codegenerror:=false;
         if not(nf_error in p.flags) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;


end.
{
  $Log: pass_2.pas,v $
  Revision 1.78  2005/04/08 15:18:08  peter
  remove multiple pass2 calls. It is not supported anymore by all nodes (ttempcreatenode)

  Revision 1.77  2005/02/14 17:13:07  peter
    * truncate log

}
