{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ THTML_a }

constructor THTML_a.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eta;
end;

function THTML_a.Getshape: THTMLshape;
var r : THTMLshape;
    s : DOMString;
begin
  s := GetAttribute (atshape);
  r := high(THTMLshape);
  while (r > low(THTMLshape)) and (comparetext(s,HTMLshape[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_a.Setshape(const AValue: THTMLshape);
begin
  if AValue = shEmpty then
    RemoveAttribute(atshape)
  else
    SetAttribute (atshape, HTMLshape[AValue]);
end;

{ THTML_abbr }

constructor THTML_abbr.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etabbr;
end;

{ THTML_acronym }

constructor THTML_acronym.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etacronym;
end;

{ THTML_address }

constructor THTML_address.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etaddress;
end;

{ THTML_applet }

constructor THTML_applet.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etapplet;
end;

{ THTML_area }

constructor THTML_area.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etarea;
end;

function THTML_area.Getnohref: boolean;
begin
  result := GetAttribute (atnohref) <> '';
end;

procedure THTML_area.Setnohref(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atnohref, 'nohref')
  else
    RemoveAttribute(atnohref);
end;

function THTML_area.Getshape: THTMLshape;
var r : THTMLshape;
    s : DOMString;
begin
  s := GetAttribute (atshape);
  r := high(THTMLshape);
  while (r > low(THTMLshape)) and (comparetext(s,HTMLshape[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_area.Setshape(const AValue: THTMLshape);
begin
  if AValue = shEmpty then
    RemoveAttribute(atshape)
  else
    SetAttribute (atshape, HTMLshape[AValue]);
end;

{ THTML_b }

constructor THTML_b.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etb;
end;

{ THTML_base }

constructor THTML_base.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbase;
end;

{ THTML_basefont }

constructor THTML_basefont.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbasefont;
end;

{ THTML_bdo }

constructor THTML_bdo.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbdo;
end;

{ THTML_big }

constructor THTML_big.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbig;
end;

{ THTML_blockquote }

constructor THTML_blockquote.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etblockquote;
end;

{ THTML_body }

constructor THTML_body.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbody;
end;

{ THTML_br }

constructor THTML_br.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbr;
end;

{ THTML_button }

constructor THTML_button.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etbutton;
end;

function THTML_button.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_button.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

function THTML_button.Gettype: THTMLbuttontype;
var r : THTMLbuttontype;
    s : DOMString;
begin
  s := GetAttribute (attype);
  r := high(THTMLbuttontype);
  while (r > low(THTMLbuttontype)) and (comparetext(s,HTMLbuttontype[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_button.Settype(const AValue: THTMLbuttontype);
begin
  if AValue = btEmpty then
    RemoveAttribute(attype)
  else
    SetAttribute (attype, HTMLbuttontype[AValue]);
end;

{ THTML_caption }

constructor THTML_caption.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcaption;
end;

{ THTML_center }

constructor THTML_center.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcenter;
end;

{ THTML_cite }

constructor THTML_cite.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcite;
end;

{ THTML_code }

constructor THTML_code.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcode;
end;

{ THTML_col }

constructor THTML_col.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcol;
end;

function THTML_col.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_col.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_col.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_col.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_colgroup }

constructor THTML_colgroup.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etcolgroup;
end;

function THTML_colgroup.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_colgroup.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_colgroup.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_colgroup.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_dd }

constructor THTML_dd.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdd;
end;

{ THTML_del }

constructor THTML_del.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdel;
end;

{ THTML_dfn }

constructor THTML_dfn.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdfn;
end;

{ THTML_dir }

constructor THTML_dir.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdir;
end;

{ THTML_div }

constructor THTML_div.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdiv;
end;

{ THTML_dl }

constructor THTML_dl.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdl;
end;

{ THTML_dt }

constructor THTML_dt.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etdt;
end;

{ THTML_em }

constructor THTML_em.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etem;
end;

{ THTML_fieldset }

constructor THTML_fieldset.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etfieldset;
end;

{ THTML_font }

constructor THTML_font.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etfont;
end;

{ THTML_form }

constructor THTML_form.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etform;
end;

{ THTML_frame }

constructor THTML_frame.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etframe;
end;

function THTML_frame.Getnoresize: boolean;
begin
  result := GetAttribute (atnoresize) <> '';
end;

procedure THTML_frame.Setnoresize(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atnoresize, 'noresize')
  else
    RemoveAttribute(atnoresize);
end;

{ THTML_frameset }

constructor THTML_frameset.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etframeset;
end;

{ THTML_h1 }

constructor THTML_h1.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth1;
end;

{ THTML_h2 }

constructor THTML_h2.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth2;
end;

{ THTML_h3 }

constructor THTML_h3.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth3;
end;

{ THTML_h4 }

constructor THTML_h4.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth4;
end;

{ THTML_h5 }

constructor THTML_h5.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth5;
end;

{ THTML_h6 }

constructor THTML_h6.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eth6;
end;

{ THTML_head }

constructor THTML_head.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ethead;
end;

{ THTML_hr }

constructor THTML_hr.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ethr;
end;

{ THTML_html }

constructor THTML_html.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ethtml;
end;

{ THTML_i }

constructor THTML_i.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  eti;
end;

{ THTML_iframe }

constructor THTML_iframe.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etiframe;
end;

function THTML_iframe.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_iframe.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

{ THTML_img }

constructor THTML_img.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etimg;
end;

{ THTML_input }

constructor THTML_input.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etinput;
end;

function THTML_input.Getchecked: boolean;
begin
  result := GetAttribute (atchecked) <> '';
end;

procedure THTML_input.Setchecked(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atchecked, 'checked')
  else
    RemoveAttribute(atchecked);
end;

function THTML_input.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_input.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

function THTML_input.Getreadonly: boolean;
begin
  result := GetAttribute (atreadonly) <> '';
end;

procedure THTML_input.Setreadonly(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atreadonly, 'readonly')
  else
    RemoveAttribute(atreadonly);
end;

function THTML_input.Gettype: THTMLinputtype;
var r : THTMLinputtype;
    s : DOMString;
begin
  s := GetAttribute (attype);
  r := high(THTMLinputtype);
  while (r > low(THTMLinputtype)) and (comparetext(s,HTMLinputtype[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_input.Settype(const AValue: THTMLinputtype);
begin
  if AValue = itEmpty then
    RemoveAttribute(attype)
  else
    SetAttribute (attype, HTMLinputtype[AValue]);
end;

{ THTML_ins }

constructor THTML_ins.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etins;
end;

{ THTML_isindex }

constructor THTML_isindex.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etisindex;
end;

{ THTML_kbd }

constructor THTML_kbd.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etkbd;
end;

{ THTML_label }

constructor THTML_label.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etlabel;
end;

{ THTML_legend }

constructor THTML_legend.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etlegend;
end;

{ THTML_li }

constructor THTML_li.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etli;
end;

{ THTML_link }

constructor THTML_link.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etlink;
end;

{ THTML_map }

constructor THTML_map.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etmap;
end;

{ THTML_menu }

constructor THTML_menu.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etmenu;
end;

{ THTML_meta }

constructor THTML_meta.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etmeta;
end;

{ THTML_noframes }

constructor THTML_noframes.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etnoframes;
end;

{ THTML_noscript }

constructor THTML_noscript.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etnoscript;
end;

{ THTML_object }

constructor THTML_object.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etobject;
end;

function THTML_object.Getdeclare: boolean;
begin
  result := GetAttribute (atdeclare) <> '';
end;

procedure THTML_object.Setdeclare(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdeclare, 'declare')
  else
    RemoveAttribute(atdeclare);
end;

{ THTML_ol }

constructor THTML_ol.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etol;
end;

{ THTML_optgroup }

constructor THTML_optgroup.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etoptgroup;
end;

function THTML_optgroup.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_optgroup.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

{ THTML_option }

constructor THTML_option.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etoption;
end;

function THTML_option.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_option.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

function THTML_option.Getselected: boolean;
begin
  result := GetAttribute (atselected) <> '';
end;

procedure THTML_option.Setselected(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atselected, 'selected')
  else
    RemoveAttribute(atselected);
end;

{ THTML_p }

constructor THTML_p.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etp;
end;

{ THTML_param }

constructor THTML_param.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etparam;
end;

function THTML_param.Getvaluetype: THTMLvaluetype;
var r : THTMLvaluetype;
    s : DOMString;
begin
  s := GetAttribute (atvaluetype);
  r := high(THTMLvaluetype);
  while (r > low(THTMLvaluetype)) and (comparetext(s,HTMLvaluetype[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_param.Setvaluetype(const AValue: THTMLvaluetype);
begin
  if AValue = vtEmpty then
    RemoveAttribute(atvaluetype)
  else
    SetAttribute (atvaluetype, HTMLvaluetype[AValue]);
end;

{ THTML_pre }

constructor THTML_pre.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etpre;
end;

{ THTML_q }

constructor THTML_q.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etq;
end;

{ THTML_s }

constructor THTML_s.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ets;
end;

{ THTML_samp }

constructor THTML_samp.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etsamp;
end;

{ THTML_script }

constructor THTML_script.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etscript;
end;

function THTML_script.Getdefer: boolean;
begin
  result := GetAttribute (atdefer) <> '';
end;

procedure THTML_script.Setdefer(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdefer, 'defer')
  else
    RemoveAttribute(atdefer);
end;

{ THTML_select }

constructor THTML_select.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etselect;
end;

function THTML_select.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_select.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

function THTML_select.Getmultiple: boolean;
begin
  result := GetAttribute (atmultiple) <> '';
end;

procedure THTML_select.Setmultiple(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atmultiple, 'multiple')
  else
    RemoveAttribute(atmultiple);
end;

{ THTML_small }

constructor THTML_small.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etsmall;
end;

{ THTML_span }

constructor THTML_span.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etspan;
end;

{ THTML_strike }

constructor THTML_strike.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etstrike;
end;

{ THTML_strong }

constructor THTML_strong.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etstrong;
end;

{ THTML_style }

constructor THTML_style.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etstyle;
end;

{ THTML_sub }

constructor THTML_sub.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etsub;
end;

{ THTML_sup }

constructor THTML_sup.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etsup;
end;

{ THTML_table }

constructor THTML_table.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettable;
end;

function THTML_table.Getframe: THTMLframe;
var r : THTMLframe;
    s : DOMString;
begin
  s := GetAttribute (atframe);
  r := high(THTMLframe);
  while (r > low(THTMLframe)) and (comparetext(s,HTMLframe[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_table.Setframe(const AValue: THTMLframe);
begin
  if AValue = frEmpty then
    RemoveAttribute(atframe)
  else
    SetAttribute (atframe, HTMLframe[AValue]);
end;

function THTML_table.Getrules: THTMLrules;
var r : THTMLrules;
    s : DOMString;
begin
  s := GetAttribute (atrules);
  r := high(THTMLrules);
  while (r > low(THTMLrules)) and (comparetext(s,HTMLrules[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_table.Setrules(const AValue: THTMLrules);
begin
  if AValue = ruEmpty then
    RemoveAttribute(atrules)
  else
    SetAttribute (atrules, HTMLrules[AValue]);
end;

{ THTML_tbody }

constructor THTML_tbody.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettbody;
end;

function THTML_tbody.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tbody.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_tbody.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tbody.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_td }

constructor THTML_td.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettd;
end;

function THTML_td.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_td.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_td.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_td.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_textarea }

constructor THTML_textarea.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettextarea;
end;

function THTML_textarea.Getdisabled: boolean;
begin
  result := GetAttribute (atdisabled) <> '';
end;

procedure THTML_textarea.Setdisabled(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atdisabled, 'disabled')
  else
    RemoveAttribute(atdisabled);
end;

function THTML_textarea.Getreadonly: boolean;
begin
  result := GetAttribute (atreadonly) <> '';
end;

procedure THTML_textarea.Setreadonly(const AValue: boolean);
begin
  if AValue then
    SetAttribute (atreadonly, 'readonly')
  else
    RemoveAttribute(atreadonly);
end;

{ THTML_tfoot }

constructor THTML_tfoot.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettfoot;
end;

function THTML_tfoot.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tfoot.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_tfoot.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tfoot.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_th }

constructor THTML_th.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etth;
end;

function THTML_th.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_th.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_th.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_th.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_thead }

constructor THTML_thead.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etthead;
end;

function THTML_thead.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_thead.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_thead.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_thead.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_title }

constructor THTML_title.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettitle;
end;

{ THTML_tr }

constructor THTML_tr.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettr;
end;

function THTML_tr.Getalign: THTMLalign;
var r : THTMLalign;
    s : DOMString;
begin
  s := GetAttribute (atalign);
  r := high(THTMLalign);
  while (r > low(THTMLalign)) and (comparetext(s,HTMLalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tr.Setalign(const AValue: THTMLalign);
begin
  if AValue = alEmpty then
    RemoveAttribute(atalign)
  else
    SetAttribute (atalign, HTMLalign[AValue]);
end;

function THTML_tr.Getvalign: THTMLvalign;
var r : THTMLvalign;
    s : DOMString;
begin
  s := GetAttribute (atvalign);
  r := high(THTMLvalign);
  while (r > low(THTMLvalign)) and (comparetext(s,HTMLvalign[r]) <> 0) do
    dec (r);
  result := r;
end;

procedure THTML_tr.Setvalign(const AValue: THTMLvalign);
begin
  if AValue = vaEmpty then
    RemoveAttribute(atvalign)
  else
    SetAttribute (atvalign, HTMLvalign[AValue]);
end;

{ THTML_tt }

constructor THTML_tt.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  ettt;
end;

{ THTML_u }

constructor THTML_u.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etu;
end;

{ THTML_ul }

constructor THTML_ul.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etul;
end;

{ THTML_var }

constructor THTML_var.create (AOwner: TDOMDocument);
begin
  inherited create (AOwner);
  ElementTag :=  etvar;
end;

