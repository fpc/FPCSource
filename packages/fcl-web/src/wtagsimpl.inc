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
{ tag "a", alias "anchor" }

function THTMLWriter.taga (s : string) : THTML_a;
begin
  result := THTML_a (CreateElement (THTML_a, s));
end;

function THTMLWriter.taga (sub : THTMLCustomElement) : THTML_a;
begin
  result := THTML_a (CreateElement (THTML_a, sub));
end;

function THTMLWriter.taga (subs : Array of THTMLCustomElement) : THTML_a;
begin
  result := THTML_a (CreateElement (THTML_a, subs));
end;

function THTMLWriter.taga (subs : TDOMNodelist) : THTML_a;
begin
  result := THTML_a (CreateElement (THTML_a, subs));
end;

function THTMLWriter.anchor (s : string) : THTML_a;
begin
  result := taga (s);
end;

function THTMLWriter.anchor (sub : THTMLCustomElement) : THTML_a;
begin
  result := taga (sub);
end;

function THTMLWriter.anchor (subs : Array of THTMLCustomElement) : THTML_a;
begin
  result := taga (subs);
end;

function THTMLWriter.anchor (subs : TDOMNodelist) : THTML_a;
begin
  result := taga (subs);
end;

function THTMLWriter.Starta : THTML_a;
begin
  result := THTML_a (StartElement (THTML_a));
end;

function THTMLWriter.Enda : THTML_a;
begin
  result := THTML_a (EndElement (THTML_a));
end;

function THTMLWriter.Startanchor : THTML_a;
begin
  result := Starta;
end;

function THTMLWriter.Endanchor : THTML_a;
begin
  result := Enda;
end;

{ tag "abbr", alias "abbr" }

function THTMLWriter.tagabbr (s : string) : THTML_abbr;
begin
  result := THTML_abbr (CreateElement (THTML_abbr, s));
end;

function THTMLWriter.tagabbr (sub : THTMLCustomElement) : THTML_abbr;
begin
  result := THTML_abbr (CreateElement (THTML_abbr, sub));
end;

function THTMLWriter.tagabbr (subs : Array of THTMLCustomElement) : THTML_abbr;
begin
  result := THTML_abbr (CreateElement (THTML_abbr, subs));
end;

function THTMLWriter.tagabbr (subs : TDOMNodelist) : THTML_abbr;
begin
  result := THTML_abbr (CreateElement (THTML_abbr, subs));
end;

function THTMLWriter.abbr (s : string) : THTML_abbr;
begin
  result := tagabbr (s);
end;

function THTMLWriter.abbr (sub : THTMLCustomElement) : THTML_abbr;
begin
  result := tagabbr (sub);
end;

function THTMLWriter.abbr (subs : Array of THTMLCustomElement) : THTML_abbr;
begin
  result := tagabbr (subs);
end;

function THTMLWriter.abbr (subs : TDOMNodelist) : THTML_abbr;
begin
  result := tagabbr (subs);
end;

function THTMLWriter.Startabbr : THTML_abbr;
begin
  result := THTML_abbr (StartElement (THTML_abbr));
end;

function THTMLWriter.Endabbr : THTML_abbr;
begin
  result := THTML_abbr (EndElement (THTML_abbr));
end;

{ tag "acronym", alias "acronym" }

function THTMLWriter.tagacronym (s : string) : THTML_acronym;
begin
  result := THTML_acronym (CreateElement (THTML_acronym, s));
end;

function THTMLWriter.tagacronym (sub : THTMLCustomElement) : THTML_acronym;
begin
  result := THTML_acronym (CreateElement (THTML_acronym, sub));
end;

function THTMLWriter.tagacronym (subs : Array of THTMLCustomElement) : THTML_acronym;
begin
  result := THTML_acronym (CreateElement (THTML_acronym, subs));
end;

function THTMLWriter.tagacronym (subs : TDOMNodelist) : THTML_acronym;
begin
  result := THTML_acronym (CreateElement (THTML_acronym, subs));
end;

function THTMLWriter.acronym (s : string) : THTML_acronym;
begin
  result := tagacronym (s);
end;

function THTMLWriter.acronym (sub : THTMLCustomElement) : THTML_acronym;
begin
  result := tagacronym (sub);
end;

function THTMLWriter.acronym (subs : Array of THTMLCustomElement) : THTML_acronym;
begin
  result := tagacronym (subs);
end;

function THTMLWriter.acronym (subs : TDOMNodelist) : THTML_acronym;
begin
  result := tagacronym (subs);
end;

function THTMLWriter.Startacronym : THTML_acronym;
begin
  result := THTML_acronym (StartElement (THTML_acronym));
end;

function THTMLWriter.Endacronym : THTML_acronym;
begin
  result := THTML_acronym (EndElement (THTML_acronym));
end;

{ tag "address", alias "address" }

function THTMLWriter.tagaddress (s : string) : THTML_address;
begin
  result := THTML_address (CreateElement (THTML_address, s));
end;

function THTMLWriter.tagaddress (sub : THTMLCustomElement) : THTML_address;
begin
  result := THTML_address (CreateElement (THTML_address, sub));
end;

function THTMLWriter.tagaddress (subs : Array of THTMLCustomElement) : THTML_address;
begin
  result := THTML_address (CreateElement (THTML_address, subs));
end;

function THTMLWriter.tagaddress (subs : TDOMNodelist) : THTML_address;
begin
  result := THTML_address (CreateElement (THTML_address, subs));
end;

function THTMLWriter.address (s : string) : THTML_address;
begin
  result := tagaddress (s);
end;

function THTMLWriter.address (sub : THTMLCustomElement) : THTML_address;
begin
  result := tagaddress (sub);
end;

function THTMLWriter.address (subs : Array of THTMLCustomElement) : THTML_address;
begin
  result := tagaddress (subs);
end;

function THTMLWriter.address (subs : TDOMNodelist) : THTML_address;
begin
  result := tagaddress (subs);
end;

function THTMLWriter.Startaddress : THTML_address;
begin
  result := THTML_address (StartElement (THTML_address));
end;

function THTMLWriter.Endaddress : THTML_address;
begin
  result := THTML_address (EndElement (THTML_address));
end;

{ tag "applet", alias "applet" }

function THTMLWriter.tagapplet (s : string) : THTML_applet;
begin
  result := THTML_applet (CreateElement (THTML_applet, s));
end;

function THTMLWriter.tagapplet (sub : THTMLCustomElement) : THTML_applet;
begin
  result := THTML_applet (CreateElement (THTML_applet, sub));
end;

function THTMLWriter.tagapplet (subs : Array of THTMLCustomElement) : THTML_applet;
begin
  result := THTML_applet (CreateElement (THTML_applet, subs));
end;

function THTMLWriter.tagapplet (subs : TDOMNodelist) : THTML_applet;
begin
  result := THTML_applet (CreateElement (THTML_applet, subs));
end;

function THTMLWriter.applet (s : string) : THTML_applet;
begin
  result := tagapplet (s);
end;

function THTMLWriter.applet (sub : THTMLCustomElement) : THTML_applet;
begin
  result := tagapplet (sub);
end;

function THTMLWriter.applet (subs : Array of THTMLCustomElement) : THTML_applet;
begin
  result := tagapplet (subs);
end;

function THTMLWriter.applet (subs : TDOMNodelist) : THTML_applet;
begin
  result := tagapplet (subs);
end;

function THTMLWriter.Startapplet : THTML_applet;
begin
  result := THTML_applet (StartElement (THTML_applet));
end;

function THTMLWriter.Endapplet : THTML_applet;
begin
  result := THTML_applet (EndElement (THTML_applet));
end;

{ tag "area", alias "area" }

function THTMLWriter.tagarea : THTML_area;
begin
  result := THTML_area (AddElement (THTML_area));
end;

function THTMLWriter.area : THTML_area;
begin
  result := tagarea;
end;

{ tag "b", alias "bold" }

function THTMLWriter.tagb (s : string) : THTML_b;
begin
  result := THTML_b (CreateElement (THTML_b, s));
end;

function THTMLWriter.tagb (sub : THTMLCustomElement) : THTML_b;
begin
  result := THTML_b (CreateElement (THTML_b, sub));
end;

function THTMLWriter.tagb (subs : Array of THTMLCustomElement) : THTML_b;
begin
  result := THTML_b (CreateElement (THTML_b, subs));
end;

function THTMLWriter.tagb (subs : TDOMNodelist) : THTML_b;
begin
  result := THTML_b (CreateElement (THTML_b, subs));
end;

function THTMLWriter.bold (s : string) : THTML_b;
begin
  result := tagb (s);
end;

function THTMLWriter.bold (sub : THTMLCustomElement) : THTML_b;
begin
  result := tagb (sub);
end;

function THTMLWriter.bold (subs : Array of THTMLCustomElement) : THTML_b;
begin
  result := tagb (subs);
end;

function THTMLWriter.bold (subs : TDOMNodelist) : THTML_b;
begin
  result := tagb (subs);
end;

function THTMLWriter.Startb : THTML_b;
begin
  result := THTML_b (StartElement (THTML_b));
end;

function THTMLWriter.Endb : THTML_b;
begin
  result := THTML_b (EndElement (THTML_b));
end;

function THTMLWriter.Startbold : THTML_b;
begin
  result := Startb;
end;

function THTMLWriter.Endbold : THTML_b;
begin
  result := Endb;
end;

{ tag "base", alias "base" }

function THTMLWriter.tagbase : THTML_base;
begin
  result := THTML_base (AddElement (THTML_base));
end;

function THTMLWriter.base : THTML_base;
begin
  result := tagbase;
end;

{ tag "basefont", alias "basefont" }

function THTMLWriter.tagbasefont : THTML_basefont;
begin
  result := THTML_basefont (AddElement (THTML_basefont));
end;

function THTMLWriter.basefont : THTML_basefont;
begin
  result := tagbasefont;
end;

{ tag "bdo", alias "bdo" }

function THTMLWriter.tagbdo (s : string) : THTML_bdo;
begin
  result := THTML_bdo (CreateElement (THTML_bdo, s));
end;

function THTMLWriter.tagbdo (sub : THTMLCustomElement) : THTML_bdo;
begin
  result := THTML_bdo (CreateElement (THTML_bdo, sub));
end;

function THTMLWriter.tagbdo (subs : Array of THTMLCustomElement) : THTML_bdo;
begin
  result := THTML_bdo (CreateElement (THTML_bdo, subs));
end;

function THTMLWriter.tagbdo (subs : TDOMNodelist) : THTML_bdo;
begin
  result := THTML_bdo (CreateElement (THTML_bdo, subs));
end;

function THTMLWriter.bdo (s : string) : THTML_bdo;
begin
  result := tagbdo (s);
end;

function THTMLWriter.bdo (sub : THTMLCustomElement) : THTML_bdo;
begin
  result := tagbdo (sub);
end;

function THTMLWriter.bdo (subs : Array of THTMLCustomElement) : THTML_bdo;
begin
  result := tagbdo (subs);
end;

function THTMLWriter.bdo (subs : TDOMNodelist) : THTML_bdo;
begin
  result := tagbdo (subs);
end;

function THTMLWriter.Startbdo : THTML_bdo;
begin
  result := THTML_bdo (StartElement (THTML_bdo));
end;

function THTMLWriter.Endbdo : THTML_bdo;
begin
  result := THTML_bdo (EndElement (THTML_bdo));
end;

{ tag "big", alias "big" }

function THTMLWriter.tagbig (s : string) : THTML_big;
begin
  result := THTML_big (CreateElement (THTML_big, s));
end;

function THTMLWriter.tagbig (sub : THTMLCustomElement) : THTML_big;
begin
  result := THTML_big (CreateElement (THTML_big, sub));
end;

function THTMLWriter.tagbig (subs : Array of THTMLCustomElement) : THTML_big;
begin
  result := THTML_big (CreateElement (THTML_big, subs));
end;

function THTMLWriter.tagbig (subs : TDOMNodelist) : THTML_big;
begin
  result := THTML_big (CreateElement (THTML_big, subs));
end;

function THTMLWriter.big (s : string) : THTML_big;
begin
  result := tagbig (s);
end;

function THTMLWriter.big (sub : THTMLCustomElement) : THTML_big;
begin
  result := tagbig (sub);
end;

function THTMLWriter.big (subs : Array of THTMLCustomElement) : THTML_big;
begin
  result := tagbig (subs);
end;

function THTMLWriter.big (subs : TDOMNodelist) : THTML_big;
begin
  result := tagbig (subs);
end;

function THTMLWriter.Startbig : THTML_big;
begin
  result := THTML_big (StartElement (THTML_big));
end;

function THTMLWriter.Endbig : THTML_big;
begin
  result := THTML_big (EndElement (THTML_big));
end;

{ tag "blockquote", alias "blockquote" }

function THTMLWriter.tagblockquote (s : string) : THTML_blockquote;
begin
  result := THTML_blockquote (CreateElement (THTML_blockquote, s));
end;

function THTMLWriter.tagblockquote (sub : THTMLCustomElement) : THTML_blockquote;
begin
  result := THTML_blockquote (CreateElement (THTML_blockquote, sub));
end;

function THTMLWriter.tagblockquote (subs : Array of THTMLCustomElement) : THTML_blockquote;
begin
  result := THTML_blockquote (CreateElement (THTML_blockquote, subs));
end;

function THTMLWriter.tagblockquote (subs : TDOMNodelist) : THTML_blockquote;
begin
  result := THTML_blockquote (CreateElement (THTML_blockquote, subs));
end;

function THTMLWriter.blockquote (s : string) : THTML_blockquote;
begin
  result := tagblockquote (s);
end;

function THTMLWriter.blockquote (sub : THTMLCustomElement) : THTML_blockquote;
begin
  result := tagblockquote (sub);
end;

function THTMLWriter.blockquote (subs : Array of THTMLCustomElement) : THTML_blockquote;
begin
  result := tagblockquote (subs);
end;

function THTMLWriter.blockquote (subs : TDOMNodelist) : THTML_blockquote;
begin
  result := tagblockquote (subs);
end;

function THTMLWriter.Startblockquote : THTML_blockquote;
begin
  result := THTML_blockquote (StartElement (THTML_blockquote));
end;

function THTMLWriter.Endblockquote : THTML_blockquote;
begin
  result := THTML_blockquote (EndElement (THTML_blockquote));
end;

{ tag "body", alias "body" }

function THTMLWriter.tagbody (s : string) : THTML_body;
begin
  result := THTML_body (CreateElement (THTML_body, s));
end;

function THTMLWriter.tagbody (sub : THTMLCustomElement) : THTML_body;
begin
  result := THTML_body (CreateElement (THTML_body, sub));
end;

function THTMLWriter.tagbody (subs : Array of THTMLCustomElement) : THTML_body;
begin
  result := THTML_body (CreateElement (THTML_body, subs));
end;

function THTMLWriter.tagbody (subs : TDOMNodelist) : THTML_body;
begin
  result := THTML_body (CreateElement (THTML_body, subs));
end;

function THTMLWriter.body (s : string) : THTML_body;
begin
  result := tagbody (s);
end;

function THTMLWriter.body (sub : THTMLCustomElement) : THTML_body;
begin
  result := tagbody (sub);
end;

function THTMLWriter.body (subs : Array of THTMLCustomElement) : THTML_body;
begin
  result := tagbody (subs);
end;

function THTMLWriter.body (subs : TDOMNodelist) : THTML_body;
begin
  result := tagbody (subs);
end;

function THTMLWriter.Startbody : THTML_body;
begin
  result := THTML_body (StartElement (THTML_body));
end;

function THTMLWriter.Endbody : THTML_body;
begin
  result := THTML_body (EndElement (THTML_body));
end;

{ tag "br", alias "linebreak" }

function THTMLWriter.tagbr : THTML_br;
begin
  result := THTML_br (AddElement (THTML_br));
end;

function THTMLWriter.linebreak : THTML_br;
begin
  result := tagbr;
end;

{ tag "button", alias "button" }

function THTMLWriter.tagbutton (s : string) : THTML_button;
begin
  result := THTML_button (CreateElement (THTML_button, s));
end;

function THTMLWriter.tagbutton (sub : THTMLCustomElement) : THTML_button;
begin
  result := THTML_button (CreateElement (THTML_button, sub));
end;

function THTMLWriter.tagbutton (subs : Array of THTMLCustomElement) : THTML_button;
begin
  result := THTML_button (CreateElement (THTML_button, subs));
end;

function THTMLWriter.tagbutton (subs : TDOMNodelist) : THTML_button;
begin
  result := THTML_button (CreateElement (THTML_button, subs));
end;

function THTMLWriter.button (s : string) : THTML_button;
begin
  result := tagbutton (s);
end;

function THTMLWriter.button (sub : THTMLCustomElement) : THTML_button;
begin
  result := tagbutton (sub);
end;

function THTMLWriter.button (subs : Array of THTMLCustomElement) : THTML_button;
begin
  result := tagbutton (subs);
end;

function THTMLWriter.button (subs : TDOMNodelist) : THTML_button;
begin
  result := tagbutton (subs);
end;

function THTMLWriter.Startbutton : THTML_button;
begin
  result := THTML_button (StartElement (THTML_button));
end;

function THTMLWriter.Endbutton : THTML_button;
begin
  result := THTML_button (EndElement (THTML_button));
end;

{ tag "caption", alias "caption" }

function THTMLWriter.tagcaption (s : string) : THTML_caption;
begin
  result := THTML_caption (CreateElement (THTML_caption, s));
end;

function THTMLWriter.tagcaption (sub : THTMLCustomElement) : THTML_caption;
begin
  result := THTML_caption (CreateElement (THTML_caption, sub));
end;

function THTMLWriter.tagcaption (subs : Array of THTMLCustomElement) : THTML_caption;
begin
  result := THTML_caption (CreateElement (THTML_caption, subs));
end;

function THTMLWriter.tagcaption (subs : TDOMNodelist) : THTML_caption;
begin
  result := THTML_caption (CreateElement (THTML_caption, subs));
end;

function THTMLWriter.caption (s : string) : THTML_caption;
begin
  result := tagcaption (s);
end;

function THTMLWriter.caption (sub : THTMLCustomElement) : THTML_caption;
begin
  result := tagcaption (sub);
end;

function THTMLWriter.caption (subs : Array of THTMLCustomElement) : THTML_caption;
begin
  result := tagcaption (subs);
end;

function THTMLWriter.caption (subs : TDOMNodelist) : THTML_caption;
begin
  result := tagcaption (subs);
end;

function THTMLWriter.Startcaption : THTML_caption;
begin
  result := THTML_caption (StartElement (THTML_caption));
end;

function THTMLWriter.Endcaption : THTML_caption;
begin
  result := THTML_caption (EndElement (THTML_caption));
end;

{ tag "center", alias "center" }

function THTMLWriter.tagcenter (s : string) : THTML_center;
begin
  result := THTML_center (CreateElement (THTML_center, s));
end;

function THTMLWriter.tagcenter (sub : THTMLCustomElement) : THTML_center;
begin
  result := THTML_center (CreateElement (THTML_center, sub));
end;

function THTMLWriter.tagcenter (subs : Array of THTMLCustomElement) : THTML_center;
begin
  result := THTML_center (CreateElement (THTML_center, subs));
end;

function THTMLWriter.tagcenter (subs : TDOMNodelist) : THTML_center;
begin
  result := THTML_center (CreateElement (THTML_center, subs));
end;

function THTMLWriter.center (s : string) : THTML_center;
begin
  result := tagcenter (s);
end;

function THTMLWriter.center (sub : THTMLCustomElement) : THTML_center;
begin
  result := tagcenter (sub);
end;

function THTMLWriter.center (subs : Array of THTMLCustomElement) : THTML_center;
begin
  result := tagcenter (subs);
end;

function THTMLWriter.center (subs : TDOMNodelist) : THTML_center;
begin
  result := tagcenter (subs);
end;

function THTMLWriter.Startcenter : THTML_center;
begin
  result := THTML_center (StartElement (THTML_center));
end;

function THTMLWriter.Endcenter : THTML_center;
begin
  result := THTML_center (EndElement (THTML_center));
end;

{ tag "cite", alias "cite" }

function THTMLWriter.tagcite (s : string) : THTML_cite;
begin
  result := THTML_cite (CreateElement (THTML_cite, s));
end;

function THTMLWriter.tagcite (sub : THTMLCustomElement) : THTML_cite;
begin
  result := THTML_cite (CreateElement (THTML_cite, sub));
end;

function THTMLWriter.tagcite (subs : Array of THTMLCustomElement) : THTML_cite;
begin
  result := THTML_cite (CreateElement (THTML_cite, subs));
end;

function THTMLWriter.tagcite (subs : TDOMNodelist) : THTML_cite;
begin
  result := THTML_cite (CreateElement (THTML_cite, subs));
end;

function THTMLWriter.cite (s : string) : THTML_cite;
begin
  result := tagcite (s);
end;

function THTMLWriter.cite (sub : THTMLCustomElement) : THTML_cite;
begin
  result := tagcite (sub);
end;

function THTMLWriter.cite (subs : Array of THTMLCustomElement) : THTML_cite;
begin
  result := tagcite (subs);
end;

function THTMLWriter.cite (subs : TDOMNodelist) : THTML_cite;
begin
  result := tagcite (subs);
end;

function THTMLWriter.Startcite : THTML_cite;
begin
  result := THTML_cite (StartElement (THTML_cite));
end;

function THTMLWriter.Endcite : THTML_cite;
begin
  result := THTML_cite (EndElement (THTML_cite));
end;

{ tag "code", alias "code" }

function THTMLWriter.tagcode (s : string) : THTML_code;
begin
  result := THTML_code (CreateElement (THTML_code, s));
end;

function THTMLWriter.tagcode (sub : THTMLCustomElement) : THTML_code;
begin
  result := THTML_code (CreateElement (THTML_code, sub));
end;

function THTMLWriter.tagcode (subs : Array of THTMLCustomElement) : THTML_code;
begin
  result := THTML_code (CreateElement (THTML_code, subs));
end;

function THTMLWriter.tagcode (subs : TDOMNodelist) : THTML_code;
begin
  result := THTML_code (CreateElement (THTML_code, subs));
end;

function THTMLWriter.code (s : string) : THTML_code;
begin
  result := tagcode (s);
end;

function THTMLWriter.code (sub : THTMLCustomElement) : THTML_code;
begin
  result := tagcode (sub);
end;

function THTMLWriter.code (subs : Array of THTMLCustomElement) : THTML_code;
begin
  result := tagcode (subs);
end;

function THTMLWriter.code (subs : TDOMNodelist) : THTML_code;
begin
  result := tagcode (subs);
end;

function THTMLWriter.Startcode : THTML_code;
begin
  result := THTML_code (StartElement (THTML_code));
end;

function THTMLWriter.Endcode : THTML_code;
begin
  result := THTML_code (EndElement (THTML_code));
end;

{ tag "col", alias "column" }

function THTMLWriter.tagcol : THTML_col;
begin
  result := THTML_col (AddElement (THTML_col));
end;

function THTMLWriter.column : THTML_col;
begin
  result := tagcol;
end;

{ tag "colgroup", alias "colgroup" }

function THTMLWriter.tagcolgroup (s : string) : THTML_colgroup;
begin
  result := THTML_colgroup (CreateElement (THTML_colgroup, s));
end;

function THTMLWriter.tagcolgroup (sub : THTMLCustomElement) : THTML_colgroup;
begin
  result := THTML_colgroup (CreateElement (THTML_colgroup, sub));
end;

function THTMLWriter.tagcolgroup (subs : Array of THTMLCustomElement) : THTML_colgroup;
begin
  result := THTML_colgroup (CreateElement (THTML_colgroup, subs));
end;

function THTMLWriter.tagcolgroup (subs : TDOMNodelist) : THTML_colgroup;
begin
  result := THTML_colgroup (CreateElement (THTML_colgroup, subs));
end;

function THTMLWriter.colgroup (s : string) : THTML_colgroup;
begin
  result := tagcolgroup (s);
end;

function THTMLWriter.colgroup (sub : THTMLCustomElement) : THTML_colgroup;
begin
  result := tagcolgroup (sub);
end;

function THTMLWriter.colgroup (subs : Array of THTMLCustomElement) : THTML_colgroup;
begin
  result := tagcolgroup (subs);
end;

function THTMLWriter.colgroup (subs : TDOMNodelist) : THTML_colgroup;
begin
  result := tagcolgroup (subs);
end;

function THTMLWriter.Startcolgroup : THTML_colgroup;
begin
  result := THTML_colgroup (StartElement (THTML_colgroup));
end;

function THTMLWriter.Endcolgroup : THTML_colgroup;
begin
  result := THTML_colgroup (EndElement (THTML_colgroup));
end;

{ tag "dd", alias "defdescr" }

function THTMLWriter.tagdd (s : string) : THTML_dd;
begin
  result := THTML_dd (CreateElement (THTML_dd, s));
end;

function THTMLWriter.tagdd (sub : THTMLCustomElement) : THTML_dd;
begin
  result := THTML_dd (CreateElement (THTML_dd, sub));
end;

function THTMLWriter.tagdd (subs : Array of THTMLCustomElement) : THTML_dd;
begin
  result := THTML_dd (CreateElement (THTML_dd, subs));
end;

function THTMLWriter.tagdd (subs : TDOMNodelist) : THTML_dd;
begin
  result := THTML_dd (CreateElement (THTML_dd, subs));
end;

function THTMLWriter.defdescr (s : string) : THTML_dd;
begin
  result := tagdd (s);
end;

function THTMLWriter.defdescr (sub : THTMLCustomElement) : THTML_dd;
begin
  result := tagdd (sub);
end;

function THTMLWriter.defdescr (subs : Array of THTMLCustomElement) : THTML_dd;
begin
  result := tagdd (subs);
end;

function THTMLWriter.defdescr (subs : TDOMNodelist) : THTML_dd;
begin
  result := tagdd (subs);
end;

function THTMLWriter.Startdd : THTML_dd;
begin
  result := THTML_dd (StartElement (THTML_dd));
end;

function THTMLWriter.Enddd : THTML_dd;
begin
  result := THTML_dd (EndElement (THTML_dd));
end;

function THTMLWriter.Startdefdescr : THTML_dd;
begin
  result := Startdd;
end;

function THTMLWriter.Enddefdescr : THTML_dd;
begin
  result := Enddd;
end;

{ tag "del", alias "deleted" }

function THTMLWriter.tagdel (s : string) : THTML_del;
begin
  result := THTML_del (CreateElement (THTML_del, s));
end;

function THTMLWriter.tagdel (sub : THTMLCustomElement) : THTML_del;
begin
  result := THTML_del (CreateElement (THTML_del, sub));
end;

function THTMLWriter.tagdel (subs : Array of THTMLCustomElement) : THTML_del;
begin
  result := THTML_del (CreateElement (THTML_del, subs));
end;

function THTMLWriter.tagdel (subs : TDOMNodelist) : THTML_del;
begin
  result := THTML_del (CreateElement (THTML_del, subs));
end;

function THTMLWriter.deleted (s : string) : THTML_del;
begin
  result := tagdel (s);
end;

function THTMLWriter.deleted (sub : THTMLCustomElement) : THTML_del;
begin
  result := tagdel (sub);
end;

function THTMLWriter.deleted (subs : Array of THTMLCustomElement) : THTML_del;
begin
  result := tagdel (subs);
end;

function THTMLWriter.deleted (subs : TDOMNodelist) : THTML_del;
begin
  result := tagdel (subs);
end;

function THTMLWriter.Startdel : THTML_del;
begin
  result := THTML_del (StartElement (THTML_del));
end;

function THTMLWriter.Enddel : THTML_del;
begin
  result := THTML_del (EndElement (THTML_del));
end;

function THTMLWriter.Startdeleted : THTML_del;
begin
  result := Startdel;
end;

function THTMLWriter.Enddeleted : THTML_del;
begin
  result := Enddel;
end;

{ tag "dfn", alias "definition" }

function THTMLWriter.tagdfn (s : string) : THTML_dfn;
begin
  result := THTML_dfn (CreateElement (THTML_dfn, s));
end;

function THTMLWriter.tagdfn (sub : THTMLCustomElement) : THTML_dfn;
begin
  result := THTML_dfn (CreateElement (THTML_dfn, sub));
end;

function THTMLWriter.tagdfn (subs : Array of THTMLCustomElement) : THTML_dfn;
begin
  result := THTML_dfn (CreateElement (THTML_dfn, subs));
end;

function THTMLWriter.tagdfn (subs : TDOMNodelist) : THTML_dfn;
begin
  result := THTML_dfn (CreateElement (THTML_dfn, subs));
end;

function THTMLWriter.definition (s : string) : THTML_dfn;
begin
  result := tagdfn (s);
end;

function THTMLWriter.definition (sub : THTMLCustomElement) : THTML_dfn;
begin
  result := tagdfn (sub);
end;

function THTMLWriter.definition (subs : Array of THTMLCustomElement) : THTML_dfn;
begin
  result := tagdfn (subs);
end;

function THTMLWriter.definition (subs : TDOMNodelist) : THTML_dfn;
begin
  result := tagdfn (subs);
end;

function THTMLWriter.Startdfn : THTML_dfn;
begin
  result := THTML_dfn (StartElement (THTML_dfn));
end;

function THTMLWriter.Enddfn : THTML_dfn;
begin
  result := THTML_dfn (EndElement (THTML_dfn));
end;

function THTMLWriter.Startdefinition : THTML_dfn;
begin
  result := Startdfn;
end;

function THTMLWriter.Enddefinition : THTML_dfn;
begin
  result := Enddfn;
end;

{ tag "dir", alias "dir" }

function THTMLWriter.tagdir (s : string) : THTML_dir;
begin
  result := THTML_dir (CreateElement (THTML_dir, s));
end;

function THTMLWriter.tagdir (sub : THTMLCustomElement) : THTML_dir;
begin
  result := THTML_dir (CreateElement (THTML_dir, sub));
end;

function THTMLWriter.tagdir (subs : Array of THTMLCustomElement) : THTML_dir;
begin
  result := THTML_dir (CreateElement (THTML_dir, subs));
end;

function THTMLWriter.tagdir (subs : TDOMNodelist) : THTML_dir;
begin
  result := THTML_dir (CreateElement (THTML_dir, subs));
end;

function THTMLWriter.dir (s : string) : THTML_dir;
begin
  result := tagdir (s);
end;

function THTMLWriter.dir (sub : THTMLCustomElement) : THTML_dir;
begin
  result := tagdir (sub);
end;

function THTMLWriter.dir (subs : Array of THTMLCustomElement) : THTML_dir;
begin
  result := tagdir (subs);
end;

function THTMLWriter.dir (subs : TDOMNodelist) : THTML_dir;
begin
  result := tagdir (subs);
end;

function THTMLWriter.Startdir : THTML_dir;
begin
  result := THTML_dir (StartElement (THTML_dir));
end;

function THTMLWriter.Enddir : THTML_dir;
begin
  result := THTML_dir (EndElement (THTML_dir));
end;

{ tag "div", alias "divider" }

function THTMLWriter.tagdiv (s : string) : THTML_div;
begin
  result := THTML_div (CreateElement (THTML_div, s));
end;

function THTMLWriter.tagdiv (sub : THTMLCustomElement) : THTML_div;
begin
  result := THTML_div (CreateElement (THTML_div, sub));
end;

function THTMLWriter.tagdiv (subs : Array of THTMLCustomElement) : THTML_div;
begin
  result := THTML_div (CreateElement (THTML_div, subs));
end;

function THTMLWriter.tagdiv (subs : TDOMNodelist) : THTML_div;
begin
  result := THTML_div (CreateElement (THTML_div, subs));
end;

function THTMLWriter.divider (s : string) : THTML_div;
begin
  result := tagdiv (s);
end;

function THTMLWriter.divider (sub : THTMLCustomElement) : THTML_div;
begin
  result := tagdiv (sub);
end;

function THTMLWriter.divider (subs : Array of THTMLCustomElement) : THTML_div;
begin
  result := tagdiv (subs);
end;

function THTMLWriter.divider (subs : TDOMNodelist) : THTML_div;
begin
  result := tagdiv (subs);
end;

function THTMLWriter.Startdiv : THTML_div;
begin
  result := THTML_div (StartElement (THTML_div));
end;

function THTMLWriter.Enddiv : THTML_div;
begin
  result := THTML_div (EndElement (THTML_div));
end;

function THTMLWriter.Startdivider : THTML_div;
begin
  result := Startdiv;
end;

function THTMLWriter.Enddivider : THTML_div;
begin
  result := Enddiv;
end;

{ tag "dl", alias "deflist" }

function THTMLWriter.tagdl (s : string) : THTML_dl;
begin
  result := THTML_dl (CreateElement (THTML_dl, s));
end;

function THTMLWriter.tagdl (sub : THTMLCustomElement) : THTML_dl;
begin
  result := THTML_dl (CreateElement (THTML_dl, sub));
end;

function THTMLWriter.tagdl (subs : Array of THTMLCustomElement) : THTML_dl;
begin
  result := THTML_dl (CreateElement (THTML_dl, subs));
end;

function THTMLWriter.tagdl (subs : TDOMNodelist) : THTML_dl;
begin
  result := THTML_dl (CreateElement (THTML_dl, subs));
end;

function THTMLWriter.deflist (s : string) : THTML_dl;
begin
  result := tagdl (s);
end;

function THTMLWriter.deflist (sub : THTMLCustomElement) : THTML_dl;
begin
  result := tagdl (sub);
end;

function THTMLWriter.deflist (subs : Array of THTMLCustomElement) : THTML_dl;
begin
  result := tagdl (subs);
end;

function THTMLWriter.deflist (subs : TDOMNodelist) : THTML_dl;
begin
  result := tagdl (subs);
end;

function THTMLWriter.Startdl : THTML_dl;
begin
  result := THTML_dl (StartElement (THTML_dl));
end;

function THTMLWriter.Enddl : THTML_dl;
begin
  result := THTML_dl (EndElement (THTML_dl));
end;

function THTMLWriter.Startdeflist : THTML_dl;
begin
  result := Startdl;
end;

function THTMLWriter.Enddeflist : THTML_dl;
begin
  result := Enddl;
end;

{ tag "dt", alias "defterm" }

function THTMLWriter.tagdt (s : string) : THTML_dt;
begin
  result := THTML_dt (CreateElement (THTML_dt, s));
end;

function THTMLWriter.tagdt (sub : THTMLCustomElement) : THTML_dt;
begin
  result := THTML_dt (CreateElement (THTML_dt, sub));
end;

function THTMLWriter.tagdt (subs : Array of THTMLCustomElement) : THTML_dt;
begin
  result := THTML_dt (CreateElement (THTML_dt, subs));
end;

function THTMLWriter.tagdt (subs : TDOMNodelist) : THTML_dt;
begin
  result := THTML_dt (CreateElement (THTML_dt, subs));
end;

function THTMLWriter.defterm (s : string) : THTML_dt;
begin
  result := tagdt (s);
end;

function THTMLWriter.defterm (sub : THTMLCustomElement) : THTML_dt;
begin
  result := tagdt (sub);
end;

function THTMLWriter.defterm (subs : Array of THTMLCustomElement) : THTML_dt;
begin
  result := tagdt (subs);
end;

function THTMLWriter.defterm (subs : TDOMNodelist) : THTML_dt;
begin
  result := tagdt (subs);
end;

function THTMLWriter.Startdt : THTML_dt;
begin
  result := THTML_dt (StartElement (THTML_dt));
end;

function THTMLWriter.Enddt : THTML_dt;
begin
  result := THTML_dt (EndElement (THTML_dt));
end;

function THTMLWriter.Startdefterm : THTML_dt;
begin
  result := Startdt;
end;

function THTMLWriter.Enddefterm : THTML_dt;
begin
  result := Enddt;
end;

{ tag "em", alias "emphasis" }

function THTMLWriter.tagem (s : string) : THTML_em;
begin
  result := THTML_em (CreateElement (THTML_em, s));
end;

function THTMLWriter.tagem (sub : THTMLCustomElement) : THTML_em;
begin
  result := THTML_em (CreateElement (THTML_em, sub));
end;

function THTMLWriter.tagem (subs : Array of THTMLCustomElement) : THTML_em;
begin
  result := THTML_em (CreateElement (THTML_em, subs));
end;

function THTMLWriter.tagem (subs : TDOMNodelist) : THTML_em;
begin
  result := THTML_em (CreateElement (THTML_em, subs));
end;

function THTMLWriter.emphasis (s : string) : THTML_em;
begin
  result := tagem (s);
end;

function THTMLWriter.emphasis (sub : THTMLCustomElement) : THTML_em;
begin
  result := tagem (sub);
end;

function THTMLWriter.emphasis (subs : Array of THTMLCustomElement) : THTML_em;
begin
  result := tagem (subs);
end;

function THTMLWriter.emphasis (subs : TDOMNodelist) : THTML_em;
begin
  result := tagem (subs);
end;

function THTMLWriter.Startem : THTML_em;
begin
  result := THTML_em (StartElement (THTML_em));
end;

function THTMLWriter.Endem : THTML_em;
begin
  result := THTML_em (EndElement (THTML_em));
end;

function THTMLWriter.Startemphasis : THTML_em;
begin
  result := Startem;
end;

function THTMLWriter.Endemphasis : THTML_em;
begin
  result := Endem;
end;

{ tag "fieldset", alias "fieldset" }

function THTMLWriter.tagfieldset (s : string) : THTML_fieldset;
begin
  result := THTML_fieldset (CreateElement (THTML_fieldset, s));
end;

function THTMLWriter.tagfieldset (sub : THTMLCustomElement) : THTML_fieldset;
begin
  result := THTML_fieldset (CreateElement (THTML_fieldset, sub));
end;

function THTMLWriter.tagfieldset (subs : Array of THTMLCustomElement) : THTML_fieldset;
begin
  result := THTML_fieldset (CreateElement (THTML_fieldset, subs));
end;

function THTMLWriter.tagfieldset (subs : TDOMNodelist) : THTML_fieldset;
begin
  result := THTML_fieldset (CreateElement (THTML_fieldset, subs));
end;

function THTMLWriter.fieldset (s : string) : THTML_fieldset;
begin
  result := tagfieldset (s);
end;

function THTMLWriter.fieldset (sub : THTMLCustomElement) : THTML_fieldset;
begin
  result := tagfieldset (sub);
end;

function THTMLWriter.fieldset (subs : Array of THTMLCustomElement) : THTML_fieldset;
begin
  result := tagfieldset (subs);
end;

function THTMLWriter.fieldset (subs : TDOMNodelist) : THTML_fieldset;
begin
  result := tagfieldset (subs);
end;

function THTMLWriter.Startfieldset : THTML_fieldset;
begin
  result := THTML_fieldset (StartElement (THTML_fieldset));
end;

function THTMLWriter.Endfieldset : THTML_fieldset;
begin
  result := THTML_fieldset (EndElement (THTML_fieldset));
end;

{ tag "font", alias "font" }

function THTMLWriter.tagfont (s : string) : THTML_font;
begin
  result := THTML_font (CreateElement (THTML_font, s));
end;

function THTMLWriter.tagfont (sub : THTMLCustomElement) : THTML_font;
begin
  result := THTML_font (CreateElement (THTML_font, sub));
end;

function THTMLWriter.tagfont (subs : Array of THTMLCustomElement) : THTML_font;
begin
  result := THTML_font (CreateElement (THTML_font, subs));
end;

function THTMLWriter.tagfont (subs : TDOMNodelist) : THTML_font;
begin
  result := THTML_font (CreateElement (THTML_font, subs));
end;

function THTMLWriter.font (s : string) : THTML_font;
begin
  result := tagfont (s);
end;

function THTMLWriter.font (sub : THTMLCustomElement) : THTML_font;
begin
  result := tagfont (sub);
end;

function THTMLWriter.font (subs : Array of THTMLCustomElement) : THTML_font;
begin
  result := tagfont (subs);
end;

function THTMLWriter.font (subs : TDOMNodelist) : THTML_font;
begin
  result := tagfont (subs);
end;

function THTMLWriter.Startfont : THTML_font;
begin
  result := THTML_font (StartElement (THTML_font));
end;

function THTMLWriter.Endfont : THTML_font;
begin
  result := THTML_font (EndElement (THTML_font));
end;

{ tag "form", alias "form" }

function THTMLWriter.tagform (s : string) : THTML_form;
begin
  result := THTML_form (CreateElement (THTML_form, s));
end;

function THTMLWriter.tagform (sub : THTMLCustomElement) : THTML_form;
begin
  result := THTML_form (CreateElement (THTML_form, sub));
end;

function THTMLWriter.tagform (subs : Array of THTMLCustomElement) : THTML_form;
begin
  result := THTML_form (CreateElement (THTML_form, subs));
end;

function THTMLWriter.tagform (subs : TDOMNodelist) : THTML_form;
begin
  result := THTML_form (CreateElement (THTML_form, subs));
end;

function THTMLWriter.form (s : string) : THTML_form;
begin
  result := tagform (s);
end;

function THTMLWriter.form (sub : THTMLCustomElement) : THTML_form;
begin
  result := tagform (sub);
end;

function THTMLWriter.form (subs : Array of THTMLCustomElement) : THTML_form;
begin
  result := tagform (subs);
end;

function THTMLWriter.form (subs : TDOMNodelist) : THTML_form;
begin
  result := tagform (subs);
end;

function THTMLWriter.Startform : THTML_form;
begin
  result := THTML_form (StartElement (THTML_form));
end;

function THTMLWriter.Endform : THTML_form;
begin
  result := THTML_form (EndElement (THTML_form));
end;

{ tag "frame", alias "frame" }

function THTMLWriter.tagframe : THTML_frame;
begin
  result := THTML_frame (AddElement (THTML_frame));
end;

function THTMLWriter.frame : THTML_frame;
begin
  result := tagframe;
end;

{ tag "frameset", alias "frameset" }

function THTMLWriter.tagframeset (s : string) : THTML_frameset;
begin
  result := THTML_frameset (CreateElement (THTML_frameset, s));
end;

function THTMLWriter.tagframeset (sub : THTMLCustomElement) : THTML_frameset;
begin
  result := THTML_frameset (CreateElement (THTML_frameset, sub));
end;

function THTMLWriter.tagframeset (subs : Array of THTMLCustomElement) : THTML_frameset;
begin
  result := THTML_frameset (CreateElement (THTML_frameset, subs));
end;

function THTMLWriter.tagframeset (subs : TDOMNodelist) : THTML_frameset;
begin
  result := THTML_frameset (CreateElement (THTML_frameset, subs));
end;

function THTMLWriter.frameset (s : string) : THTML_frameset;
begin
  result := tagframeset (s);
end;

function THTMLWriter.frameset (sub : THTMLCustomElement) : THTML_frameset;
begin
  result := tagframeset (sub);
end;

function THTMLWriter.frameset (subs : Array of THTMLCustomElement) : THTML_frameset;
begin
  result := tagframeset (subs);
end;

function THTMLWriter.frameset (subs : TDOMNodelist) : THTML_frameset;
begin
  result := tagframeset (subs);
end;

function THTMLWriter.Startframeset : THTML_frameset;
begin
  result := THTML_frameset (StartElement (THTML_frameset));
end;

function THTMLWriter.Endframeset : THTML_frameset;
begin
  result := THTML_frameset (EndElement (THTML_frameset));
end;

{ tag "h1", alias "heading1" }

function THTMLWriter.tagh1 (s : string) : THTML_h1;
begin
  result := THTML_h1 (CreateElement (THTML_h1, s));
end;

function THTMLWriter.tagh1 (sub : THTMLCustomElement) : THTML_h1;
begin
  result := THTML_h1 (CreateElement (THTML_h1, sub));
end;

function THTMLWriter.tagh1 (subs : Array of THTMLCustomElement) : THTML_h1;
begin
  result := THTML_h1 (CreateElement (THTML_h1, subs));
end;

function THTMLWriter.tagh1 (subs : TDOMNodelist) : THTML_h1;
begin
  result := THTML_h1 (CreateElement (THTML_h1, subs));
end;

function THTMLWriter.heading1 (s : string) : THTML_h1;
begin
  result := tagh1 (s);
end;

function THTMLWriter.heading1 (sub : THTMLCustomElement) : THTML_h1;
begin
  result := tagh1 (sub);
end;

function THTMLWriter.heading1 (subs : Array of THTMLCustomElement) : THTML_h1;
begin
  result := tagh1 (subs);
end;

function THTMLWriter.heading1 (subs : TDOMNodelist) : THTML_h1;
begin
  result := tagh1 (subs);
end;

function THTMLWriter.Starth1 : THTML_h1;
begin
  result := THTML_h1 (StartElement (THTML_h1));
end;

function THTMLWriter.Endh1 : THTML_h1;
begin
  result := THTML_h1 (EndElement (THTML_h1));
end;

function THTMLWriter.Startheading1 : THTML_h1;
begin
  result := Starth1;
end;

function THTMLWriter.Endheading1 : THTML_h1;
begin
  result := Endh1;
end;

{ tag "h2", alias "heading2" }

function THTMLWriter.tagh2 (s : string) : THTML_h2;
begin
  result := THTML_h2 (CreateElement (THTML_h2, s));
end;

function THTMLWriter.tagh2 (sub : THTMLCustomElement) : THTML_h2;
begin
  result := THTML_h2 (CreateElement (THTML_h2, sub));
end;

function THTMLWriter.tagh2 (subs : Array of THTMLCustomElement) : THTML_h2;
begin
  result := THTML_h2 (CreateElement (THTML_h2, subs));
end;

function THTMLWriter.tagh2 (subs : TDOMNodelist) : THTML_h2;
begin
  result := THTML_h2 (CreateElement (THTML_h2, subs));
end;

function THTMLWriter.heading2 (s : string) : THTML_h2;
begin
  result := tagh2 (s);
end;

function THTMLWriter.heading2 (sub : THTMLCustomElement) : THTML_h2;
begin
  result := tagh2 (sub);
end;

function THTMLWriter.heading2 (subs : Array of THTMLCustomElement) : THTML_h2;
begin
  result := tagh2 (subs);
end;

function THTMLWriter.heading2 (subs : TDOMNodelist) : THTML_h2;
begin
  result := tagh2 (subs);
end;

function THTMLWriter.Starth2 : THTML_h2;
begin
  result := THTML_h2 (StartElement (THTML_h2));
end;

function THTMLWriter.Endh2 : THTML_h2;
begin
  result := THTML_h2 (EndElement (THTML_h2));
end;

function THTMLWriter.Startheading2 : THTML_h2;
begin
  result := Starth2;
end;

function THTMLWriter.Endheading2 : THTML_h2;
begin
  result := Endh2;
end;

{ tag "h3", alias "heading3" }

function THTMLWriter.tagh3 (s : string) : THTML_h3;
begin
  result := THTML_h3 (CreateElement (THTML_h3, s));
end;

function THTMLWriter.tagh3 (sub : THTMLCustomElement) : THTML_h3;
begin
  result := THTML_h3 (CreateElement (THTML_h3, sub));
end;

function THTMLWriter.tagh3 (subs : Array of THTMLCustomElement) : THTML_h3;
begin
  result := THTML_h3 (CreateElement (THTML_h3, subs));
end;

function THTMLWriter.tagh3 (subs : TDOMNodelist) : THTML_h3;
begin
  result := THTML_h3 (CreateElement (THTML_h3, subs));
end;

function THTMLWriter.heading3 (s : string) : THTML_h3;
begin
  result := tagh3 (s);
end;

function THTMLWriter.heading3 (sub : THTMLCustomElement) : THTML_h3;
begin
  result := tagh3 (sub);
end;

function THTMLWriter.heading3 (subs : Array of THTMLCustomElement) : THTML_h3;
begin
  result := tagh3 (subs);
end;

function THTMLWriter.heading3 (subs : TDOMNodelist) : THTML_h3;
begin
  result := tagh3 (subs);
end;

function THTMLWriter.Starth3 : THTML_h3;
begin
  result := THTML_h3 (StartElement (THTML_h3));
end;

function THTMLWriter.Endh3 : THTML_h3;
begin
  result := THTML_h3 (EndElement (THTML_h3));
end;

function THTMLWriter.Startheading3 : THTML_h3;
begin
  result := Starth3;
end;

function THTMLWriter.Endheading3 : THTML_h3;
begin
  result := Endh3;
end;

{ tag "h4", alias "heading4" }

function THTMLWriter.tagh4 (s : string) : THTML_h4;
begin
  result := THTML_h4 (CreateElement (THTML_h4, s));
end;

function THTMLWriter.tagh4 (sub : THTMLCustomElement) : THTML_h4;
begin
  result := THTML_h4 (CreateElement (THTML_h4, sub));
end;

function THTMLWriter.tagh4 (subs : Array of THTMLCustomElement) : THTML_h4;
begin
  result := THTML_h4 (CreateElement (THTML_h4, subs));
end;

function THTMLWriter.tagh4 (subs : TDOMNodelist) : THTML_h4;
begin
  result := THTML_h4 (CreateElement (THTML_h4, subs));
end;

function THTMLWriter.heading4 (s : string) : THTML_h4;
begin
  result := tagh4 (s);
end;

function THTMLWriter.heading4 (sub : THTMLCustomElement) : THTML_h4;
begin
  result := tagh4 (sub);
end;

function THTMLWriter.heading4 (subs : Array of THTMLCustomElement) : THTML_h4;
begin
  result := tagh4 (subs);
end;

function THTMLWriter.heading4 (subs : TDOMNodelist) : THTML_h4;
begin
  result := tagh4 (subs);
end;

function THTMLWriter.Starth4 : THTML_h4;
begin
  result := THTML_h4 (StartElement (THTML_h4));
end;

function THTMLWriter.Endh4 : THTML_h4;
begin
  result := THTML_h4 (EndElement (THTML_h4));
end;

function THTMLWriter.Startheading4 : THTML_h4;
begin
  result := Starth4;
end;

function THTMLWriter.Endheading4 : THTML_h4;
begin
  result := Endh4;
end;

{ tag "h5", alias "heading5" }

function THTMLWriter.tagh5 (s : string) : THTML_h5;
begin
  result := THTML_h5 (CreateElement (THTML_h5, s));
end;

function THTMLWriter.tagh5 (sub : THTMLCustomElement) : THTML_h5;
begin
  result := THTML_h5 (CreateElement (THTML_h5, sub));
end;

function THTMLWriter.tagh5 (subs : Array of THTMLCustomElement) : THTML_h5;
begin
  result := THTML_h5 (CreateElement (THTML_h5, subs));
end;

function THTMLWriter.tagh5 (subs : TDOMNodelist) : THTML_h5;
begin
  result := THTML_h5 (CreateElement (THTML_h5, subs));
end;

function THTMLWriter.heading5 (s : string) : THTML_h5;
begin
  result := tagh5 (s);
end;

function THTMLWriter.heading5 (sub : THTMLCustomElement) : THTML_h5;
begin
  result := tagh5 (sub);
end;

function THTMLWriter.heading5 (subs : Array of THTMLCustomElement) : THTML_h5;
begin
  result := tagh5 (subs);
end;

function THTMLWriter.heading5 (subs : TDOMNodelist) : THTML_h5;
begin
  result := tagh5 (subs);
end;

function THTMLWriter.Starth5 : THTML_h5;
begin
  result := THTML_h5 (StartElement (THTML_h5));
end;

function THTMLWriter.Endh5 : THTML_h5;
begin
  result := THTML_h5 (EndElement (THTML_h5));
end;

function THTMLWriter.Startheading5 : THTML_h5;
begin
  result := Starth5;
end;

function THTMLWriter.Endheading5 : THTML_h5;
begin
  result := Endh5;
end;

{ tag "h6", alias "heading6" }

function THTMLWriter.tagh6 (s : string) : THTML_h6;
begin
  result := THTML_h6 (CreateElement (THTML_h6, s));
end;

function THTMLWriter.tagh6 (sub : THTMLCustomElement) : THTML_h6;
begin
  result := THTML_h6 (CreateElement (THTML_h6, sub));
end;

function THTMLWriter.tagh6 (subs : Array of THTMLCustomElement) : THTML_h6;
begin
  result := THTML_h6 (CreateElement (THTML_h6, subs));
end;

function THTMLWriter.tagh6 (subs : TDOMNodelist) : THTML_h6;
begin
  result := THTML_h6 (CreateElement (THTML_h6, subs));
end;

function THTMLWriter.heading6 (s : string) : THTML_h6;
begin
  result := tagh6 (s);
end;

function THTMLWriter.heading6 (sub : THTMLCustomElement) : THTML_h6;
begin
  result := tagh6 (sub);
end;

function THTMLWriter.heading6 (subs : Array of THTMLCustomElement) : THTML_h6;
begin
  result := tagh6 (subs);
end;

function THTMLWriter.heading6 (subs : TDOMNodelist) : THTML_h6;
begin
  result := tagh6 (subs);
end;

function THTMLWriter.Starth6 : THTML_h6;
begin
  result := THTML_h6 (StartElement (THTML_h6));
end;

function THTMLWriter.Endh6 : THTML_h6;
begin
  result := THTML_h6 (EndElement (THTML_h6));
end;

function THTMLWriter.Startheading6 : THTML_h6;
begin
  result := Starth6;
end;

function THTMLWriter.Endheading6 : THTML_h6;
begin
  result := Endh6;
end;

{ tag "head", alias "header" }

function THTMLWriter.taghead (s : string) : THTML_head;
begin
  result := THTML_head (CreateElement (THTML_head, s));
end;

function THTMLWriter.taghead (sub : THTMLCustomElement) : THTML_head;
begin
  result := THTML_head (CreateElement (THTML_head, sub));
end;

function THTMLWriter.taghead (subs : Array of THTMLCustomElement) : THTML_head;
begin
  result := THTML_head (CreateElement (THTML_head, subs));
end;

function THTMLWriter.taghead (subs : TDOMNodelist) : THTML_head;
begin
  result := THTML_head (CreateElement (THTML_head, subs));
end;

function THTMLWriter.header (s : string) : THTML_head;
begin
  result := taghead (s);
end;

function THTMLWriter.header (sub : THTMLCustomElement) : THTML_head;
begin
  result := taghead (sub);
end;

function THTMLWriter.header (subs : Array of THTMLCustomElement) : THTML_head;
begin
  result := taghead (subs);
end;

function THTMLWriter.header (subs : TDOMNodelist) : THTML_head;
begin
  result := taghead (subs);
end;

function THTMLWriter.Starthead : THTML_head;
begin
  result := THTML_head (StartElement (THTML_head));
end;

function THTMLWriter.Endhead : THTML_head;
begin
  result := THTML_head (EndElement (THTML_head));
end;

function THTMLWriter.Startheader : THTML_head;
begin
  result := Starthead;
end;

function THTMLWriter.Endheader : THTML_head;
begin
  result := Endhead;
end;

{ tag "hr", alias "horrule" }

function THTMLWriter.taghr : THTML_hr;
begin
  result := THTML_hr (AddElement (THTML_hr));
end;

function THTMLWriter.horrule : THTML_hr;
begin
  result := taghr;
end;

{ tag "html", alias "html" }

function THTMLWriter.taghtml (s : string) : THTML_html;
begin
  result := THTML_html (CreateElement (THTML_html, s));
end;

function THTMLWriter.taghtml (sub : THTMLCustomElement) : THTML_html;
begin
  result := THTML_html (CreateElement (THTML_html, sub));
end;

function THTMLWriter.taghtml (subs : Array of THTMLCustomElement) : THTML_html;
begin
  result := THTML_html (CreateElement (THTML_html, subs));
end;

function THTMLWriter.taghtml (subs : TDOMNodelist) : THTML_html;
begin
  result := THTML_html (CreateElement (THTML_html, subs));
end;

function THTMLWriter.html (s : string) : THTML_html;
begin
  result := taghtml (s);
end;

function THTMLWriter.html (sub : THTMLCustomElement) : THTML_html;
begin
  result := taghtml (sub);
end;

function THTMLWriter.html (subs : Array of THTMLCustomElement) : THTML_html;
begin
  result := taghtml (subs);
end;

function THTMLWriter.html (subs : TDOMNodelist) : THTML_html;
begin
  result := taghtml (subs);
end;

function THTMLWriter.Starthtml : THTML_html;
begin
  result := THTML_html (StartElement (THTML_html));
end;

function THTMLWriter.Endhtml : THTML_html;
begin
  result := THTML_html (EndElement (THTML_html));
end;

{ tag "i", alias "italic" }

function THTMLWriter.tagi (s : string) : THTML_i;
begin
  result := THTML_i (CreateElement (THTML_i, s));
end;

function THTMLWriter.tagi (sub : THTMLCustomElement) : THTML_i;
begin
  result := THTML_i (CreateElement (THTML_i, sub));
end;

function THTMLWriter.tagi (subs : Array of THTMLCustomElement) : THTML_i;
begin
  result := THTML_i (CreateElement (THTML_i, subs));
end;

function THTMLWriter.tagi (subs : TDOMNodelist) : THTML_i;
begin
  result := THTML_i (CreateElement (THTML_i, subs));
end;

function THTMLWriter.italic (s : string) : THTML_i;
begin
  result := tagi (s);
end;

function THTMLWriter.italic (sub : THTMLCustomElement) : THTML_i;
begin
  result := tagi (sub);
end;

function THTMLWriter.italic (subs : Array of THTMLCustomElement) : THTML_i;
begin
  result := tagi (subs);
end;

function THTMLWriter.italic (subs : TDOMNodelist) : THTML_i;
begin
  result := tagi (subs);
end;

function THTMLWriter.Starti : THTML_i;
begin
  result := THTML_i (StartElement (THTML_i));
end;

function THTMLWriter.Endi : THTML_i;
begin
  result := THTML_i (EndElement (THTML_i));
end;

function THTMLWriter.Startitalic : THTML_i;
begin
  result := Starti;
end;

function THTMLWriter.Enditalic : THTML_i;
begin
  result := Endi;
end;

{ tag "iframe", alias "inlineframe" }

function THTMLWriter.tagiframe (s : string) : THTML_iframe;
begin
  result := THTML_iframe (CreateElement (THTML_iframe, s));
end;

function THTMLWriter.tagiframe (sub : THTMLCustomElement) : THTML_iframe;
begin
  result := THTML_iframe (CreateElement (THTML_iframe, sub));
end;

function THTMLWriter.tagiframe (subs : Array of THTMLCustomElement) : THTML_iframe;
begin
  result := THTML_iframe (CreateElement (THTML_iframe, subs));
end;

function THTMLWriter.tagiframe (subs : TDOMNodelist) : THTML_iframe;
begin
  result := THTML_iframe (CreateElement (THTML_iframe, subs));
end;

function THTMLWriter.inlineframe (s : string) : THTML_iframe;
begin
  result := tagiframe (s);
end;

function THTMLWriter.inlineframe (sub : THTMLCustomElement) : THTML_iframe;
begin
  result := tagiframe (sub);
end;

function THTMLWriter.inlineframe (subs : Array of THTMLCustomElement) : THTML_iframe;
begin
  result := tagiframe (subs);
end;

function THTMLWriter.inlineframe (subs : TDOMNodelist) : THTML_iframe;
begin
  result := tagiframe (subs);
end;

function THTMLWriter.Startiframe : THTML_iframe;
begin
  result := THTML_iframe (StartElement (THTML_iframe));
end;

function THTMLWriter.Endiframe : THTML_iframe;
begin
  result := THTML_iframe (EndElement (THTML_iframe));
end;

function THTMLWriter.Startinlineframe : THTML_iframe;
begin
  result := Startiframe;
end;

function THTMLWriter.Endinlineframe : THTML_iframe;
begin
  result := Endiframe;
end;

{ tag "img", alias "image" }

function THTMLWriter.tagimg : THTML_img;
begin
  result := THTML_img (AddElement (THTML_img));
end;

function THTMLWriter.image : THTML_img;
begin
  result := tagimg;
end;

{ tag "input", alias "input" }

function THTMLWriter.taginput : THTML_input;
begin
  result := THTML_input (AddElement (THTML_input));
end;

function THTMLWriter.input : THTML_input;
begin
  result := taginput;
end;

{ tag "ins", alias "inserted" }

function THTMLWriter.tagins (s : string) : THTML_ins;
begin
  result := THTML_ins (CreateElement (THTML_ins, s));
end;

function THTMLWriter.tagins (sub : THTMLCustomElement) : THTML_ins;
begin
  result := THTML_ins (CreateElement (THTML_ins, sub));
end;

function THTMLWriter.tagins (subs : Array of THTMLCustomElement) : THTML_ins;
begin
  result := THTML_ins (CreateElement (THTML_ins, subs));
end;

function THTMLWriter.tagins (subs : TDOMNodelist) : THTML_ins;
begin
  result := THTML_ins (CreateElement (THTML_ins, subs));
end;

function THTMLWriter.inserted (s : string) : THTML_ins;
begin
  result := tagins (s);
end;

function THTMLWriter.inserted (sub : THTMLCustomElement) : THTML_ins;
begin
  result := tagins (sub);
end;

function THTMLWriter.inserted (subs : Array of THTMLCustomElement) : THTML_ins;
begin
  result := tagins (subs);
end;

function THTMLWriter.inserted (subs : TDOMNodelist) : THTML_ins;
begin
  result := tagins (subs);
end;

function THTMLWriter.Startins : THTML_ins;
begin
  result := THTML_ins (StartElement (THTML_ins));
end;

function THTMLWriter.Endins : THTML_ins;
begin
  result := THTML_ins (EndElement (THTML_ins));
end;

function THTMLWriter.Startinserted : THTML_ins;
begin
  result := Startins;
end;

function THTMLWriter.Endinserted : THTML_ins;
begin
  result := Endins;
end;

{ tag "isindex", alias "isindex" }

function THTMLWriter.tagisindex : THTML_isindex;
begin
  result := THTML_isindex (AddElement (THTML_isindex));
end;

function THTMLWriter.isindex : THTML_isindex;
begin
  result := tagisindex;
end;

{ tag "kbd", alias "enteredtext" }

function THTMLWriter.tagkbd (s : string) : THTML_kbd;
begin
  result := THTML_kbd (CreateElement (THTML_kbd, s));
end;

function THTMLWriter.tagkbd (sub : THTMLCustomElement) : THTML_kbd;
begin
  result := THTML_kbd (CreateElement (THTML_kbd, sub));
end;

function THTMLWriter.tagkbd (subs : Array of THTMLCustomElement) : THTML_kbd;
begin
  result := THTML_kbd (CreateElement (THTML_kbd, subs));
end;

function THTMLWriter.tagkbd (subs : TDOMNodelist) : THTML_kbd;
begin
  result := THTML_kbd (CreateElement (THTML_kbd, subs));
end;

function THTMLWriter.enteredtext (s : string) : THTML_kbd;
begin
  result := tagkbd (s);
end;

function THTMLWriter.enteredtext (sub : THTMLCustomElement) : THTML_kbd;
begin
  result := tagkbd (sub);
end;

function THTMLWriter.enteredtext (subs : Array of THTMLCustomElement) : THTML_kbd;
begin
  result := tagkbd (subs);
end;

function THTMLWriter.enteredtext (subs : TDOMNodelist) : THTML_kbd;
begin
  result := tagkbd (subs);
end;

function THTMLWriter.Startkbd : THTML_kbd;
begin
  result := THTML_kbd (StartElement (THTML_kbd));
end;

function THTMLWriter.Endkbd : THTML_kbd;
begin
  result := THTML_kbd (EndElement (THTML_kbd));
end;

function THTMLWriter.Startenteredtext : THTML_kbd;
begin
  result := Startkbd;
end;

function THTMLWriter.Endenteredtext : THTML_kbd;
begin
  result := Endkbd;
end;

{ tag "label", alias "alabel" }

function THTMLWriter.taglabel (s : string) : THTML_label;
begin
  result := THTML_label (CreateElement (THTML_label, s));
end;

function THTMLWriter.taglabel (sub : THTMLCustomElement) : THTML_label;
begin
  result := THTML_label (CreateElement (THTML_label, sub));
end;

function THTMLWriter.taglabel (subs : Array of THTMLCustomElement) : THTML_label;
begin
  result := THTML_label (CreateElement (THTML_label, subs));
end;

function THTMLWriter.taglabel (subs : TDOMNodelist) : THTML_label;
begin
  result := THTML_label (CreateElement (THTML_label, subs));
end;

function THTMLWriter.alabel (s : string) : THTML_label;
begin
  result := taglabel (s);
end;

function THTMLWriter.alabel (sub : THTMLCustomElement) : THTML_label;
begin
  result := taglabel (sub);
end;

function THTMLWriter.alabel (subs : Array of THTMLCustomElement) : THTML_label;
begin
  result := taglabel (subs);
end;

function THTMLWriter.alabel (subs : TDOMNodelist) : THTML_label;
begin
  result := taglabel (subs);
end;

function THTMLWriter.Startlabel : THTML_label;
begin
  result := THTML_label (StartElement (THTML_label));
end;

function THTMLWriter.Endlabel : THTML_label;
begin
  result := THTML_label (EndElement (THTML_label));
end;

function THTMLWriter.Startalabel : THTML_label;
begin
  result := Startlabel;
end;

function THTMLWriter.Endalabel : THTML_label;
begin
  result := Endlabel;
end;

{ tag "legend", alias "legend" }

function THTMLWriter.taglegend (s : string) : THTML_legend;
begin
  result := THTML_legend (CreateElement (THTML_legend, s));
end;

function THTMLWriter.taglegend (sub : THTMLCustomElement) : THTML_legend;
begin
  result := THTML_legend (CreateElement (THTML_legend, sub));
end;

function THTMLWriter.taglegend (subs : Array of THTMLCustomElement) : THTML_legend;
begin
  result := THTML_legend (CreateElement (THTML_legend, subs));
end;

function THTMLWriter.taglegend (subs : TDOMNodelist) : THTML_legend;
begin
  result := THTML_legend (CreateElement (THTML_legend, subs));
end;

function THTMLWriter.legend (s : string) : THTML_legend;
begin
  result := taglegend (s);
end;

function THTMLWriter.legend (sub : THTMLCustomElement) : THTML_legend;
begin
  result := taglegend (sub);
end;

function THTMLWriter.legend (subs : Array of THTMLCustomElement) : THTML_legend;
begin
  result := taglegend (subs);
end;

function THTMLWriter.legend (subs : TDOMNodelist) : THTML_legend;
begin
  result := taglegend (subs);
end;

function THTMLWriter.Startlegend : THTML_legend;
begin
  result := THTML_legend (StartElement (THTML_legend));
end;

function THTMLWriter.Endlegend : THTML_legend;
begin
  result := THTML_legend (EndElement (THTML_legend));
end;

{ tag "li", alias "listitem" }

function THTMLWriter.tagli (s : string) : THTML_li;
begin
  result := THTML_li (CreateElement (THTML_li, s));
end;

function THTMLWriter.tagli (sub : THTMLCustomElement) : THTML_li;
begin
  result := THTML_li (CreateElement (THTML_li, sub));
end;

function THTMLWriter.tagli (subs : Array of THTMLCustomElement) : THTML_li;
begin
  result := THTML_li (CreateElement (THTML_li, subs));
end;

function THTMLWriter.tagli (subs : TDOMNodelist) : THTML_li;
begin
  result := THTML_li (CreateElement (THTML_li, subs));
end;

function THTMLWriter.listitem (s : string) : THTML_li;
begin
  result := tagli (s);
end;

function THTMLWriter.listitem (sub : THTMLCustomElement) : THTML_li;
begin
  result := tagli (sub);
end;

function THTMLWriter.listitem (subs : Array of THTMLCustomElement) : THTML_li;
begin
  result := tagli (subs);
end;

function THTMLWriter.listitem (subs : TDOMNodelist) : THTML_li;
begin
  result := tagli (subs);
end;

function THTMLWriter.Startli : THTML_li;
begin
  result := THTML_li (StartElement (THTML_li));
end;

function THTMLWriter.Endli : THTML_li;
begin
  result := THTML_li (EndElement (THTML_li));
end;

function THTMLWriter.Startlistitem : THTML_li;
begin
  result := Startli;
end;

function THTMLWriter.Endlistitem : THTML_li;
begin
  result := Endli;
end;

{ tag "link", alias "link" }

function THTMLWriter.taglink : THTML_link;
begin
  result := THTML_link (AddElement (THTML_link));
end;

function THTMLWriter.link : THTML_link;
begin
  result := taglink;
end;

{ tag "map", alias "map" }

function THTMLWriter.tagmap (s : string) : THTML_map;
begin
  result := THTML_map (CreateElement (THTML_map, s));
end;

function THTMLWriter.tagmap (sub : THTMLCustomElement) : THTML_map;
begin
  result := THTML_map (CreateElement (THTML_map, sub));
end;

function THTMLWriter.tagmap (subs : Array of THTMLCustomElement) : THTML_map;
begin
  result := THTML_map (CreateElement (THTML_map, subs));
end;

function THTMLWriter.tagmap (subs : TDOMNodelist) : THTML_map;
begin
  result := THTML_map (CreateElement (THTML_map, subs));
end;

function THTMLWriter.map (s : string) : THTML_map;
begin
  result := tagmap (s);
end;

function THTMLWriter.map (sub : THTMLCustomElement) : THTML_map;
begin
  result := tagmap (sub);
end;

function THTMLWriter.map (subs : Array of THTMLCustomElement) : THTML_map;
begin
  result := tagmap (subs);
end;

function THTMLWriter.map (subs : TDOMNodelist) : THTML_map;
begin
  result := tagmap (subs);
end;

function THTMLWriter.Startmap : THTML_map;
begin
  result := THTML_map (StartElement (THTML_map));
end;

function THTMLWriter.Endmap : THTML_map;
begin
  result := THTML_map (EndElement (THTML_map));
end;

{ tag "menu", alias "menu" }

function THTMLWriter.tagmenu (s : string) : THTML_menu;
begin
  result := THTML_menu (CreateElement (THTML_menu, s));
end;

function THTMLWriter.tagmenu (sub : THTMLCustomElement) : THTML_menu;
begin
  result := THTML_menu (CreateElement (THTML_menu, sub));
end;

function THTMLWriter.tagmenu (subs : Array of THTMLCustomElement) : THTML_menu;
begin
  result := THTML_menu (CreateElement (THTML_menu, subs));
end;

function THTMLWriter.tagmenu (subs : TDOMNodelist) : THTML_menu;
begin
  result := THTML_menu (CreateElement (THTML_menu, subs));
end;

function THTMLWriter.menu (s : string) : THTML_menu;
begin
  result := tagmenu (s);
end;

function THTMLWriter.menu (sub : THTMLCustomElement) : THTML_menu;
begin
  result := tagmenu (sub);
end;

function THTMLWriter.menu (subs : Array of THTMLCustomElement) : THTML_menu;
begin
  result := tagmenu (subs);
end;

function THTMLWriter.menu (subs : TDOMNodelist) : THTML_menu;
begin
  result := tagmenu (subs);
end;

function THTMLWriter.Startmenu : THTML_menu;
begin
  result := THTML_menu (StartElement (THTML_menu));
end;

function THTMLWriter.Endmenu : THTML_menu;
begin
  result := THTML_menu (EndElement (THTML_menu));
end;

{ tag "meta", alias "meta" }

function THTMLWriter.tagmeta : THTML_meta;
begin
  result := THTML_meta (AddElement (THTML_meta));
end;

function THTMLWriter.meta : THTML_meta;
begin
  result := tagmeta;
end;

{ tag "noframes", alias "noframes" }

function THTMLWriter.tagnoframes (s : string) : THTML_noframes;
begin
  result := THTML_noframes (CreateElement (THTML_noframes, s));
end;

function THTMLWriter.tagnoframes (sub : THTMLCustomElement) : THTML_noframes;
begin
  result := THTML_noframes (CreateElement (THTML_noframes, sub));
end;

function THTMLWriter.tagnoframes (subs : Array of THTMLCustomElement) : THTML_noframes;
begin
  result := THTML_noframes (CreateElement (THTML_noframes, subs));
end;

function THTMLWriter.tagnoframes (subs : TDOMNodelist) : THTML_noframes;
begin
  result := THTML_noframes (CreateElement (THTML_noframes, subs));
end;

function THTMLWriter.noframes (s : string) : THTML_noframes;
begin
  result := tagnoframes (s);
end;

function THTMLWriter.noframes (sub : THTMLCustomElement) : THTML_noframes;
begin
  result := tagnoframes (sub);
end;

function THTMLWriter.noframes (subs : Array of THTMLCustomElement) : THTML_noframes;
begin
  result := tagnoframes (subs);
end;

function THTMLWriter.noframes (subs : TDOMNodelist) : THTML_noframes;
begin
  result := tagnoframes (subs);
end;

function THTMLWriter.Startnoframes : THTML_noframes;
begin
  result := THTML_noframes (StartElement (THTML_noframes));
end;

function THTMLWriter.Endnoframes : THTML_noframes;
begin
  result := THTML_noframes (EndElement (THTML_noframes));
end;

{ tag "noscript", alias "noscript" }

function THTMLWriter.tagnoscript (s : string) : THTML_noscript;
begin
  result := THTML_noscript (CreateElement (THTML_noscript, s));
end;

function THTMLWriter.tagnoscript (sub : THTMLCustomElement) : THTML_noscript;
begin
  result := THTML_noscript (CreateElement (THTML_noscript, sub));
end;

function THTMLWriter.tagnoscript (subs : Array of THTMLCustomElement) : THTML_noscript;
begin
  result := THTML_noscript (CreateElement (THTML_noscript, subs));
end;

function THTMLWriter.tagnoscript (subs : TDOMNodelist) : THTML_noscript;
begin
  result := THTML_noscript (CreateElement (THTML_noscript, subs));
end;

function THTMLWriter.noscript (s : string) : THTML_noscript;
begin
  result := tagnoscript (s);
end;

function THTMLWriter.noscript (sub : THTMLCustomElement) : THTML_noscript;
begin
  result := tagnoscript (sub);
end;

function THTMLWriter.noscript (subs : Array of THTMLCustomElement) : THTML_noscript;
begin
  result := tagnoscript (subs);
end;

function THTMLWriter.noscript (subs : TDOMNodelist) : THTML_noscript;
begin
  result := tagnoscript (subs);
end;

function THTMLWriter.Startnoscript : THTML_noscript;
begin
  result := THTML_noscript (StartElement (THTML_noscript));
end;

function THTMLWriter.Endnoscript : THTML_noscript;
begin
  result := THTML_noscript (EndElement (THTML_noscript));
end;

{ tag "object", alias "anobject" }

function THTMLWriter.tagobject (s : string) : THTML_object;
begin
  result := THTML_object (CreateElement (THTML_object, s));
end;

function THTMLWriter.tagobject (sub : THTMLCustomElement) : THTML_object;
begin
  result := THTML_object (CreateElement (THTML_object, sub));
end;

function THTMLWriter.tagobject (subs : Array of THTMLCustomElement) : THTML_object;
begin
  result := THTML_object (CreateElement (THTML_object, subs));
end;

function THTMLWriter.tagobject (subs : TDOMNodelist) : THTML_object;
begin
  result := THTML_object (CreateElement (THTML_object, subs));
end;

function THTMLWriter.anobject (s : string) : THTML_object;
begin
  result := tagobject (s);
end;

function THTMLWriter.anobject (sub : THTMLCustomElement) : THTML_object;
begin
  result := tagobject (sub);
end;

function THTMLWriter.anobject (subs : Array of THTMLCustomElement) : THTML_object;
begin
  result := tagobject (subs);
end;

function THTMLWriter.anobject (subs : TDOMNodelist) : THTML_object;
begin
  result := tagobject (subs);
end;

function THTMLWriter.Startobject : THTML_object;
begin
  result := THTML_object (StartElement (THTML_object));
end;

function THTMLWriter.Endobject : THTML_object;
begin
  result := THTML_object (EndElement (THTML_object));
end;

function THTMLWriter.Startanobject : THTML_object;
begin
  result := Startobject;
end;

function THTMLWriter.Endanobject : THTML_object;
begin
  result := Endobject;
end;

{ tag "ol", alias "orderedlist" }

function THTMLWriter.tagol (s : string) : THTML_ol;
begin
  result := THTML_ol (CreateElement (THTML_ol, s));
end;

function THTMLWriter.tagol (sub : THTMLCustomElement) : THTML_ol;
begin
  result := THTML_ol (CreateElement (THTML_ol, sub));
end;

function THTMLWriter.tagol (subs : Array of THTMLCustomElement) : THTML_ol;
begin
  result := THTML_ol (CreateElement (THTML_ol, subs));
end;

function THTMLWriter.tagol (subs : TDOMNodelist) : THTML_ol;
begin
  result := THTML_ol (CreateElement (THTML_ol, subs));
end;

function THTMLWriter.orderedlist (s : string) : THTML_ol;
begin
  result := tagol (s);
end;

function THTMLWriter.orderedlist (sub : THTMLCustomElement) : THTML_ol;
begin
  result := tagol (sub);
end;

function THTMLWriter.orderedlist (subs : Array of THTMLCustomElement) : THTML_ol;
begin
  result := tagol (subs);
end;

function THTMLWriter.orderedlist (subs : TDOMNodelist) : THTML_ol;
begin
  result := tagol (subs);
end;

function THTMLWriter.Startol : THTML_ol;
begin
  result := THTML_ol (StartElement (THTML_ol));
end;

function THTMLWriter.Endol : THTML_ol;
begin
  result := THTML_ol (EndElement (THTML_ol));
end;

function THTMLWriter.Startorderedlist : THTML_ol;
begin
  result := Startol;
end;

function THTMLWriter.Endorderedlist : THTML_ol;
begin
  result := Endol;
end;

{ tag "optgroup", alias "optiongroup" }

function THTMLWriter.tagoptgroup (s : string) : THTML_optgroup;
begin
  result := THTML_optgroup (CreateElement (THTML_optgroup, s));
end;

function THTMLWriter.tagoptgroup (sub : THTMLCustomElement) : THTML_optgroup;
begin
  result := THTML_optgroup (CreateElement (THTML_optgroup, sub));
end;

function THTMLWriter.tagoptgroup (subs : Array of THTMLCustomElement) : THTML_optgroup;
begin
  result := THTML_optgroup (CreateElement (THTML_optgroup, subs));
end;

function THTMLWriter.tagoptgroup (subs : TDOMNodelist) : THTML_optgroup;
begin
  result := THTML_optgroup (CreateElement (THTML_optgroup, subs));
end;

function THTMLWriter.optiongroup (s : string) : THTML_optgroup;
begin
  result := tagoptgroup (s);
end;

function THTMLWriter.optiongroup (sub : THTMLCustomElement) : THTML_optgroup;
begin
  result := tagoptgroup (sub);
end;

function THTMLWriter.optiongroup (subs : Array of THTMLCustomElement) : THTML_optgroup;
begin
  result := tagoptgroup (subs);
end;

function THTMLWriter.optiongroup (subs : TDOMNodelist) : THTML_optgroup;
begin
  result := tagoptgroup (subs);
end;

function THTMLWriter.Startoptgroup : THTML_optgroup;
begin
  result := THTML_optgroup (StartElement (THTML_optgroup));
end;

function THTMLWriter.Endoptgroup : THTML_optgroup;
begin
  result := THTML_optgroup (EndElement (THTML_optgroup));
end;

function THTMLWriter.Startoptiongroup : THTML_optgroup;
begin
  result := Startoptgroup;
end;

function THTMLWriter.Endoptiongroup : THTML_optgroup;
begin
  result := Endoptgroup;
end;

{ tag "option", alias "option" }

function THTMLWriter.tagoption (s : string) : THTML_option;
begin
  result := THTML_option (CreateElement (THTML_option, s));
end;

function THTMLWriter.tagoption (sub : THTMLCustomElement) : THTML_option;
begin
  result := THTML_option (CreateElement (THTML_option, sub));
end;

function THTMLWriter.tagoption (subs : Array of THTMLCustomElement) : THTML_option;
begin
  result := THTML_option (CreateElement (THTML_option, subs));
end;

function THTMLWriter.tagoption (subs : TDOMNodelist) : THTML_option;
begin
  result := THTML_option (CreateElement (THTML_option, subs));
end;

function THTMLWriter.option (s : string) : THTML_option;
begin
  result := tagoption (s);
end;

function THTMLWriter.option (sub : THTMLCustomElement) : THTML_option;
begin
  result := tagoption (sub);
end;

function THTMLWriter.option (subs : Array of THTMLCustomElement) : THTML_option;
begin
  result := tagoption (subs);
end;

function THTMLWriter.option (subs : TDOMNodelist) : THTML_option;
begin
  result := tagoption (subs);
end;

function THTMLWriter.Startoption : THTML_option;
begin
  result := THTML_option (StartElement (THTML_option));
end;

function THTMLWriter.Endoption : THTML_option;
begin
  result := THTML_option (EndElement (THTML_option));
end;

{ tag "p", alias "paragraph" }

function THTMLWriter.tagp (s : string) : THTML_p;
begin
  result := THTML_p (CreateElement (THTML_p, s));
end;

function THTMLWriter.tagp (sub : THTMLCustomElement) : THTML_p;
begin
  result := THTML_p (CreateElement (THTML_p, sub));
end;

function THTMLWriter.tagp (subs : Array of THTMLCustomElement) : THTML_p;
begin
  result := THTML_p (CreateElement (THTML_p, subs));
end;

function THTMLWriter.tagp (subs : TDOMNodelist) : THTML_p;
begin
  result := THTML_p (CreateElement (THTML_p, subs));
end;

function THTMLWriter.paragraph (s : string) : THTML_p;
begin
  result := tagp (s);
end;

function THTMLWriter.paragraph (sub : THTMLCustomElement) : THTML_p;
begin
  result := tagp (sub);
end;

function THTMLWriter.paragraph (subs : Array of THTMLCustomElement) : THTML_p;
begin
  result := tagp (subs);
end;

function THTMLWriter.paragraph (subs : TDOMNodelist) : THTML_p;
begin
  result := tagp (subs);
end;

function THTMLWriter.Startp : THTML_p;
begin
  result := THTML_p (StartElement (THTML_p));
end;

function THTMLWriter.Endp : THTML_p;
begin
  result := THTML_p (EndElement (THTML_p));
end;

function THTMLWriter.Startparagraph : THTML_p;
begin
  result := Startp;
end;

function THTMLWriter.Endparagraph : THTML_p;
begin
  result := Endp;
end;

{ tag "param", alias "parameter" }

function THTMLWriter.tagparam : THTML_param;
begin
  result := THTML_param (AddElement (THTML_param));
end;

function THTMLWriter.parameter : THTML_param;
begin
  result := tagparam;
end;

{ tag "pre", alias "preformatted" }

function THTMLWriter.tagpre (s : string) : THTML_pre;
begin
  result := THTML_pre (CreateElement (THTML_pre, s));
end;

function THTMLWriter.tagpre (sub : THTMLCustomElement) : THTML_pre;
begin
  result := THTML_pre (CreateElement (THTML_pre, sub));
end;

function THTMLWriter.tagpre (subs : Array of THTMLCustomElement) : THTML_pre;
begin
  result := THTML_pre (CreateElement (THTML_pre, subs));
end;

function THTMLWriter.tagpre (subs : TDOMNodelist) : THTML_pre;
begin
  result := THTML_pre (CreateElement (THTML_pre, subs));
end;

function THTMLWriter.preformatted (s : string) : THTML_pre;
begin
  result := tagpre (s);
end;

function THTMLWriter.preformatted (sub : THTMLCustomElement) : THTML_pre;
begin
  result := tagpre (sub);
end;

function THTMLWriter.preformatted (subs : Array of THTMLCustomElement) : THTML_pre;
begin
  result := tagpre (subs);
end;

function THTMLWriter.preformatted (subs : TDOMNodelist) : THTML_pre;
begin
  result := tagpre (subs);
end;

function THTMLWriter.Startpre : THTML_pre;
begin
  result := THTML_pre (StartElement (THTML_pre));
end;

function THTMLWriter.Endpre : THTML_pre;
begin
  result := THTML_pre (EndElement (THTML_pre));
end;

function THTMLWriter.Startpreformatted : THTML_pre;
begin
  result := Startpre;
end;

function THTMLWriter.Endpreformatted : THTML_pre;
begin
  result := Endpre;
end;

{ tag "q", alias "quotation" }

function THTMLWriter.tagq (s : string) : THTML_q;
begin
  result := THTML_q (CreateElement (THTML_q, s));
end;

function THTMLWriter.tagq (sub : THTMLCustomElement) : THTML_q;
begin
  result := THTML_q (CreateElement (THTML_q, sub));
end;

function THTMLWriter.tagq (subs : Array of THTMLCustomElement) : THTML_q;
begin
  result := THTML_q (CreateElement (THTML_q, subs));
end;

function THTMLWriter.tagq (subs : TDOMNodelist) : THTML_q;
begin
  result := THTML_q (CreateElement (THTML_q, subs));
end;

function THTMLWriter.quotation (s : string) : THTML_q;
begin
  result := tagq (s);
end;

function THTMLWriter.quotation (sub : THTMLCustomElement) : THTML_q;
begin
  result := tagq (sub);
end;

function THTMLWriter.quotation (subs : Array of THTMLCustomElement) : THTML_q;
begin
  result := tagq (subs);
end;

function THTMLWriter.quotation (subs : TDOMNodelist) : THTML_q;
begin
  result := tagq (subs);
end;

function THTMLWriter.Startq : THTML_q;
begin
  result := THTML_q (StartElement (THTML_q));
end;

function THTMLWriter.Endq : THTML_q;
begin
  result := THTML_q (EndElement (THTML_q));
end;

function THTMLWriter.Startquotation : THTML_q;
begin
  result := Startq;
end;

function THTMLWriter.Endquotation : THTML_q;
begin
  result := Endq;
end;

{ tag "s", alias "strikethrough" }

function THTMLWriter.tags (s : string) : THTML_s;
begin
  result := THTML_s (CreateElement (THTML_s, s));
end;

function THTMLWriter.tags (sub : THTMLCustomElement) : THTML_s;
begin
  result := THTML_s (CreateElement (THTML_s, sub));
end;

function THTMLWriter.tags (subs : Array of THTMLCustomElement) : THTML_s;
begin
  result := THTML_s (CreateElement (THTML_s, subs));
end;

function THTMLWriter.tags (subs : TDOMNodelist) : THTML_s;
begin
  result := THTML_s (CreateElement (THTML_s, subs));
end;

function THTMLWriter.strikethrough (s : string) : THTML_s;
begin
  result := tags (s);
end;

function THTMLWriter.strikethrough (sub : THTMLCustomElement) : THTML_s;
begin
  result := tags (sub);
end;

function THTMLWriter.strikethrough (subs : Array of THTMLCustomElement) : THTML_s;
begin
  result := tags (subs);
end;

function THTMLWriter.strikethrough (subs : TDOMNodelist) : THTML_s;
begin
  result := tags (subs);
end;

function THTMLWriter.Starts : THTML_s;
begin
  result := THTML_s (StartElement (THTML_s));
end;

function THTMLWriter.Ends : THTML_s;
begin
  result := THTML_s (EndElement (THTML_s));
end;

function THTMLWriter.Startstrikethrough : THTML_s;
begin
  result := Starts;
end;

function THTMLWriter.Endstrikethrough : THTML_s;
begin
  result := Ends;
end;

{ tag "samp", alias "sample" }

function THTMLWriter.tagsamp (s : string) : THTML_samp;
begin
  result := THTML_samp (CreateElement (THTML_samp, s));
end;

function THTMLWriter.tagsamp (sub : THTMLCustomElement) : THTML_samp;
begin
  result := THTML_samp (CreateElement (THTML_samp, sub));
end;

function THTMLWriter.tagsamp (subs : Array of THTMLCustomElement) : THTML_samp;
begin
  result := THTML_samp (CreateElement (THTML_samp, subs));
end;

function THTMLWriter.tagsamp (subs : TDOMNodelist) : THTML_samp;
begin
  result := THTML_samp (CreateElement (THTML_samp, subs));
end;

function THTMLWriter.sample (s : string) : THTML_samp;
begin
  result := tagsamp (s);
end;

function THTMLWriter.sample (sub : THTMLCustomElement) : THTML_samp;
begin
  result := tagsamp (sub);
end;

function THTMLWriter.sample (subs : Array of THTMLCustomElement) : THTML_samp;
begin
  result := tagsamp (subs);
end;

function THTMLWriter.sample (subs : TDOMNodelist) : THTML_samp;
begin
  result := tagsamp (subs);
end;

function THTMLWriter.Startsamp : THTML_samp;
begin
  result := THTML_samp (StartElement (THTML_samp));
end;

function THTMLWriter.Endsamp : THTML_samp;
begin
  result := THTML_samp (EndElement (THTML_samp));
end;

function THTMLWriter.Startsample : THTML_samp;
begin
  result := Startsamp;
end;

function THTMLWriter.Endsample : THTML_samp;
begin
  result := Endsamp;
end;

{ tag "script", alias "script" }

function THTMLWriter.tagscript (s : string) : THTML_script;
begin
  result := THTML_script (CreateElement (THTML_script, s));
end;

function THTMLWriter.tagscript (sub : THTMLCustomElement) : THTML_script;
begin
  result := THTML_script (CreateElement (THTML_script, sub));
end;

function THTMLWriter.tagscript (subs : Array of THTMLCustomElement) : THTML_script;
begin
  result := THTML_script (CreateElement (THTML_script, subs));
end;

function THTMLWriter.tagscript (subs : TDOMNodelist) : THTML_script;
begin
  result := THTML_script (CreateElement (THTML_script, subs));
end;

function THTMLWriter.script (s : string) : THTML_script;
begin
  result := tagscript (s);
end;

function THTMLWriter.script (sub : THTMLCustomElement) : THTML_script;
begin
  result := tagscript (sub);
end;

function THTMLWriter.script (subs : Array of THTMLCustomElement) : THTML_script;
begin
  result := tagscript (subs);
end;

function THTMLWriter.script (subs : TDOMNodelist) : THTML_script;
begin
  result := tagscript (subs);
end;

function THTMLWriter.Startscript : THTML_script;
begin
  result := THTML_script (StartElement (THTML_script));
end;

function THTMLWriter.Endscript : THTML_script;
begin
  result := THTML_script (EndElement (THTML_script));
end;

{ tag "select", alias "select" }

function THTMLWriter.tagselect (s : string) : THTML_select;
begin
  result := THTML_select (CreateElement (THTML_select, s));
end;

function THTMLWriter.tagselect (sub : THTMLCustomElement) : THTML_select;
begin
  result := THTML_select (CreateElement (THTML_select, sub));
end;

function THTMLWriter.tagselect (subs : Array of THTMLCustomElement) : THTML_select;
begin
  result := THTML_select (CreateElement (THTML_select, subs));
end;

function THTMLWriter.tagselect (subs : TDOMNodelist) : THTML_select;
begin
  result := THTML_select (CreateElement (THTML_select, subs));
end;

function THTMLWriter.select (s : string) : THTML_select;
begin
  result := tagselect (s);
end;

function THTMLWriter.select (sub : THTMLCustomElement) : THTML_select;
begin
  result := tagselect (sub);
end;

function THTMLWriter.select (subs : Array of THTMLCustomElement) : THTML_select;
begin
  result := tagselect (subs);
end;

function THTMLWriter.select (subs : TDOMNodelist) : THTML_select;
begin
  result := tagselect (subs);
end;

function THTMLWriter.Startselect : THTML_select;
begin
  result := THTML_select (StartElement (THTML_select));
end;

function THTMLWriter.Endselect : THTML_select;
begin
  result := THTML_select (EndElement (THTML_select));
end;

{ tag "small", alias "small" }

function THTMLWriter.tagsmall (s : string) : THTML_small;
begin
  result := THTML_small (CreateElement (THTML_small, s));
end;

function THTMLWriter.tagsmall (sub : THTMLCustomElement) : THTML_small;
begin
  result := THTML_small (CreateElement (THTML_small, sub));
end;

function THTMLWriter.tagsmall (subs : Array of THTMLCustomElement) : THTML_small;
begin
  result := THTML_small (CreateElement (THTML_small, subs));
end;

function THTMLWriter.tagsmall (subs : TDOMNodelist) : THTML_small;
begin
  result := THTML_small (CreateElement (THTML_small, subs));
end;

function THTMLWriter.small (s : string) : THTML_small;
begin
  result := tagsmall (s);
end;

function THTMLWriter.small (sub : THTMLCustomElement) : THTML_small;
begin
  result := tagsmall (sub);
end;

function THTMLWriter.small (subs : Array of THTMLCustomElement) : THTML_small;
begin
  result := tagsmall (subs);
end;

function THTMLWriter.small (subs : TDOMNodelist) : THTML_small;
begin
  result := tagsmall (subs);
end;

function THTMLWriter.Startsmall : THTML_small;
begin
  result := THTML_small (StartElement (THTML_small));
end;

function THTMLWriter.Endsmall : THTML_small;
begin
  result := THTML_small (EndElement (THTML_small));
end;

{ tag "span", alias "span" }

function THTMLWriter.tagspan (s : string) : THTML_span;
begin
  result := THTML_span (CreateElement (THTML_span, s));
end;

function THTMLWriter.tagspan (sub : THTMLCustomElement) : THTML_span;
begin
  result := THTML_span (CreateElement (THTML_span, sub));
end;

function THTMLWriter.tagspan (subs : Array of THTMLCustomElement) : THTML_span;
begin
  result := THTML_span (CreateElement (THTML_span, subs));
end;

function THTMLWriter.tagspan (subs : TDOMNodelist) : THTML_span;
begin
  result := THTML_span (CreateElement (THTML_span, subs));
end;

function THTMLWriter.span (s : string) : THTML_span;
begin
  result := tagspan (s);
end;

function THTMLWriter.span (sub : THTMLCustomElement) : THTML_span;
begin
  result := tagspan (sub);
end;

function THTMLWriter.span (subs : Array of THTMLCustomElement) : THTML_span;
begin
  result := tagspan (subs);
end;

function THTMLWriter.span (subs : TDOMNodelist) : THTML_span;
begin
  result := tagspan (subs);
end;

function THTMLWriter.Startspan : THTML_span;
begin
  result := THTML_span (StartElement (THTML_span));
end;

function THTMLWriter.Endspan : THTML_span;
begin
  result := THTML_span (EndElement (THTML_span));
end;

{ tag "strike", alias "strike" }

function THTMLWriter.tagstrike (s : string) : THTML_strike;
begin
  result := THTML_strike (CreateElement (THTML_strike, s));
end;

function THTMLWriter.tagstrike (sub : THTMLCustomElement) : THTML_strike;
begin
  result := THTML_strike (CreateElement (THTML_strike, sub));
end;

function THTMLWriter.tagstrike (subs : Array of THTMLCustomElement) : THTML_strike;
begin
  result := THTML_strike (CreateElement (THTML_strike, subs));
end;

function THTMLWriter.tagstrike (subs : TDOMNodelist) : THTML_strike;
begin
  result := THTML_strike (CreateElement (THTML_strike, subs));
end;

function THTMLWriter.strike (s : string) : THTML_strike;
begin
  result := tagstrike (s);
end;

function THTMLWriter.strike (sub : THTMLCustomElement) : THTML_strike;
begin
  result := tagstrike (sub);
end;

function THTMLWriter.strike (subs : Array of THTMLCustomElement) : THTML_strike;
begin
  result := tagstrike (subs);
end;

function THTMLWriter.strike (subs : TDOMNodelist) : THTML_strike;
begin
  result := tagstrike (subs);
end;

function THTMLWriter.Startstrike : THTML_strike;
begin
  result := THTML_strike (StartElement (THTML_strike));
end;

function THTMLWriter.Endstrike : THTML_strike;
begin
  result := THTML_strike (EndElement (THTML_strike));
end;

{ tag "strong", alias "strong" }

function THTMLWriter.tagstrong (s : string) : THTML_strong;
begin
  result := THTML_strong (CreateElement (THTML_strong, s));
end;

function THTMLWriter.tagstrong (sub : THTMLCustomElement) : THTML_strong;
begin
  result := THTML_strong (CreateElement (THTML_strong, sub));
end;

function THTMLWriter.tagstrong (subs : Array of THTMLCustomElement) : THTML_strong;
begin
  result := THTML_strong (CreateElement (THTML_strong, subs));
end;

function THTMLWriter.tagstrong (subs : TDOMNodelist) : THTML_strong;
begin
  result := THTML_strong (CreateElement (THTML_strong, subs));
end;

function THTMLWriter.strong (s : string) : THTML_strong;
begin
  result := tagstrong (s);
end;

function THTMLWriter.strong (sub : THTMLCustomElement) : THTML_strong;
begin
  result := tagstrong (sub);
end;

function THTMLWriter.strong (subs : Array of THTMLCustomElement) : THTML_strong;
begin
  result := tagstrong (subs);
end;

function THTMLWriter.strong (subs : TDOMNodelist) : THTML_strong;
begin
  result := tagstrong (subs);
end;

function THTMLWriter.Startstrong : THTML_strong;
begin
  result := THTML_strong (StartElement (THTML_strong));
end;

function THTMLWriter.Endstrong : THTML_strong;
begin
  result := THTML_strong (EndElement (THTML_strong));
end;

{ tag "style", alias "style" }

function THTMLWriter.tagstyle (s : string) : THTML_style;
begin
  result := THTML_style (CreateElement (THTML_style, s));
end;

function THTMLWriter.tagstyle (sub : THTMLCustomElement) : THTML_style;
begin
  result := THTML_style (CreateElement (THTML_style, sub));
end;

function THTMLWriter.tagstyle (subs : Array of THTMLCustomElement) : THTML_style;
begin
  result := THTML_style (CreateElement (THTML_style, subs));
end;

function THTMLWriter.tagstyle (subs : TDOMNodelist) : THTML_style;
begin
  result := THTML_style (CreateElement (THTML_style, subs));
end;

function THTMLWriter.style (s : string) : THTML_style;
begin
  result := tagstyle (s);
end;

function THTMLWriter.style (sub : THTMLCustomElement) : THTML_style;
begin
  result := tagstyle (sub);
end;

function THTMLWriter.style (subs : Array of THTMLCustomElement) : THTML_style;
begin
  result := tagstyle (subs);
end;

function THTMLWriter.style (subs : TDOMNodelist) : THTML_style;
begin
  result := tagstyle (subs);
end;

function THTMLWriter.Startstyle : THTML_style;
begin
  result := THTML_style (StartElement (THTML_style));
end;

function THTMLWriter.Endstyle : THTML_style;
begin
  result := THTML_style (EndElement (THTML_style));
end;

{ tag "sub", alias "subscript" }

function THTMLWriter.tagsub (s : string) : THTML_sub;
begin
  result := THTML_sub (CreateElement (THTML_sub, s));
end;

function THTMLWriter.tagsub (sub : THTMLCustomElement) : THTML_sub;
begin
  result := THTML_sub (CreateElement (THTML_sub, sub));
end;

function THTMLWriter.tagsub (subs : Array of THTMLCustomElement) : THTML_sub;
begin
  result := THTML_sub (CreateElement (THTML_sub, subs));
end;

function THTMLWriter.tagsub (subs : TDOMNodelist) : THTML_sub;
begin
  result := THTML_sub (CreateElement (THTML_sub, subs));
end;

function THTMLWriter.subscript (s : string) : THTML_sub;
begin
  result := tagsub (s);
end;

function THTMLWriter.subscript (sub : THTMLCustomElement) : THTML_sub;
begin
  result := tagsub (sub);
end;

function THTMLWriter.subscript (subs : Array of THTMLCustomElement) : THTML_sub;
begin
  result := tagsub (subs);
end;

function THTMLWriter.subscript (subs : TDOMNodelist) : THTML_sub;
begin
  result := tagsub (subs);
end;

function THTMLWriter.Startsub : THTML_sub;
begin
  result := THTML_sub (StartElement (THTML_sub));
end;

function THTMLWriter.Endsub : THTML_sub;
begin
  result := THTML_sub (EndElement (THTML_sub));
end;

function THTMLWriter.Startsubscript : THTML_sub;
begin
  result := Startsub;
end;

function THTMLWriter.Endsubscript : THTML_sub;
begin
  result := Endsub;
end;

{ tag "sup", alias "superscript" }

function THTMLWriter.tagsup (s : string) : THTML_sup;
begin
  result := THTML_sup (CreateElement (THTML_sup, s));
end;

function THTMLWriter.tagsup (sub : THTMLCustomElement) : THTML_sup;
begin
  result := THTML_sup (CreateElement (THTML_sup, sub));
end;

function THTMLWriter.tagsup (subs : Array of THTMLCustomElement) : THTML_sup;
begin
  result := THTML_sup (CreateElement (THTML_sup, subs));
end;

function THTMLWriter.tagsup (subs : TDOMNodelist) : THTML_sup;
begin
  result := THTML_sup (CreateElement (THTML_sup, subs));
end;

function THTMLWriter.superscript (s : string) : THTML_sup;
begin
  result := tagsup (s);
end;

function THTMLWriter.superscript (sub : THTMLCustomElement) : THTML_sup;
begin
  result := tagsup (sub);
end;

function THTMLWriter.superscript (subs : Array of THTMLCustomElement) : THTML_sup;
begin
  result := tagsup (subs);
end;

function THTMLWriter.superscript (subs : TDOMNodelist) : THTML_sup;
begin
  result := tagsup (subs);
end;

function THTMLWriter.Startsup : THTML_sup;
begin
  result := THTML_sup (StartElement (THTML_sup));
end;

function THTMLWriter.Endsup : THTML_sup;
begin
  result := THTML_sup (EndElement (THTML_sup));
end;

function THTMLWriter.Startsuperscript : THTML_sup;
begin
  result := Startsup;
end;

function THTMLWriter.Endsuperscript : THTML_sup;
begin
  result := Endsup;
end;

{ tag "table", alias "table" }

function THTMLWriter.tagtable (s : string) : THTML_table;
begin
  result := THTML_table (CreateElement (THTML_table, s));
end;

function THTMLWriter.tagtable (sub : THTMLCustomElement) : THTML_table;
begin
  result := THTML_table (CreateElement (THTML_table, sub));
end;

function THTMLWriter.tagtable (subs : Array of THTMLCustomElement) : THTML_table;
begin
  result := THTML_table (CreateElement (THTML_table, subs));
end;

function THTMLWriter.tagtable (subs : TDOMNodelist) : THTML_table;
begin
  result := THTML_table (CreateElement (THTML_table, subs));
end;

function THTMLWriter.table (s : string) : THTML_table;
begin
  result := tagtable (s);
end;

function THTMLWriter.table (sub : THTMLCustomElement) : THTML_table;
begin
  result := tagtable (sub);
end;

function THTMLWriter.table (subs : Array of THTMLCustomElement) : THTML_table;
begin
  result := tagtable (subs);
end;

function THTMLWriter.table (subs : TDOMNodelist) : THTML_table;
begin
  result := tagtable (subs);
end;

function THTMLWriter.Starttable : THTML_table;
begin
  result := THTML_table (StartElement (THTML_table));
end;

function THTMLWriter.Endtable : THTML_table;
begin
  result := THTML_table (EndElement (THTML_table));
end;

{ tag "tbody", alias "tablebody" }

function THTMLWriter.tagtbody (s : string) : THTML_tbody;
begin
  result := THTML_tbody (CreateElement (THTML_tbody, s));
end;

function THTMLWriter.tagtbody (sub : THTMLCustomElement) : THTML_tbody;
begin
  result := THTML_tbody (CreateElement (THTML_tbody, sub));
end;

function THTMLWriter.tagtbody (subs : Array of THTMLCustomElement) : THTML_tbody;
begin
  result := THTML_tbody (CreateElement (THTML_tbody, subs));
end;

function THTMLWriter.tagtbody (subs : TDOMNodelist) : THTML_tbody;
begin
  result := THTML_tbody (CreateElement (THTML_tbody, subs));
end;

function THTMLWriter.tablebody (s : string) : THTML_tbody;
begin
  result := tagtbody (s);
end;

function THTMLWriter.tablebody (sub : THTMLCustomElement) : THTML_tbody;
begin
  result := tagtbody (sub);
end;

function THTMLWriter.tablebody (subs : Array of THTMLCustomElement) : THTML_tbody;
begin
  result := tagtbody (subs);
end;

function THTMLWriter.tablebody (subs : TDOMNodelist) : THTML_tbody;
begin
  result := tagtbody (subs);
end;

function THTMLWriter.Starttbody : THTML_tbody;
begin
  result := THTML_tbody (StartElement (THTML_tbody));
end;

function THTMLWriter.Endtbody : THTML_tbody;
begin
  result := THTML_tbody (EndElement (THTML_tbody));
end;

function THTMLWriter.Starttablebody : THTML_tbody;
begin
  result := Starttbody;
end;

function THTMLWriter.Endtablebody : THTML_tbody;
begin
  result := Endtbody;
end;

{ tag "td", alias "tablecell" }

function THTMLWriter.tagtd (s : string) : THTML_td;
begin
  result := THTML_td (CreateElement (THTML_td, s));
end;

function THTMLWriter.tagtd (sub : THTMLCustomElement) : THTML_td;
begin
  result := THTML_td (CreateElement (THTML_td, sub));
end;

function THTMLWriter.tagtd (subs : Array of THTMLCustomElement) : THTML_td;
begin
  result := THTML_td (CreateElement (THTML_td, subs));
end;

function THTMLWriter.tagtd (subs : TDOMNodelist) : THTML_td;
begin
  result := THTML_td (CreateElement (THTML_td, subs));
end;

function THTMLWriter.tablecell (s : string) : THTML_td;
begin
  result := tagtd (s);
end;

function THTMLWriter.tablecell (sub : THTMLCustomElement) : THTML_td;
begin
  result := tagtd (sub);
end;

function THTMLWriter.tablecell (subs : Array of THTMLCustomElement) : THTML_td;
begin
  result := tagtd (subs);
end;

function THTMLWriter.tablecell (subs : TDOMNodelist) : THTML_td;
begin
  result := tagtd (subs);
end;

function THTMLWriter.Starttd : THTML_td;
begin
  result := THTML_td (StartElement (THTML_td));
end;

function THTMLWriter.Endtd : THTML_td;
begin
  result := THTML_td (EndElement (THTML_td));
end;

function THTMLWriter.Starttablecell : THTML_td;
begin
  result := Starttd;
end;

function THTMLWriter.Endtablecell : THTML_td;
begin
  result := Endtd;
end;

{ tag "textarea", alias "textarea" }

function THTMLWriter.tagtextarea (s : string) : THTML_textarea;
begin
  result := THTML_textarea (CreateElement (THTML_textarea, s));
end;

function THTMLWriter.tagtextarea (sub : THTMLCustomElement) : THTML_textarea;
begin
  result := THTML_textarea (CreateElement (THTML_textarea, sub));
end;

function THTMLWriter.tagtextarea (subs : Array of THTMLCustomElement) : THTML_textarea;
begin
  result := THTML_textarea (CreateElement (THTML_textarea, subs));
end;

function THTMLWriter.tagtextarea (subs : TDOMNodelist) : THTML_textarea;
begin
  result := THTML_textarea (CreateElement (THTML_textarea, subs));
end;

function THTMLWriter.textarea (s : string) : THTML_textarea;
begin
  result := tagtextarea (s);
end;

function THTMLWriter.textarea (sub : THTMLCustomElement) : THTML_textarea;
begin
  result := tagtextarea (sub);
end;

function THTMLWriter.textarea (subs : Array of THTMLCustomElement) : THTML_textarea;
begin
  result := tagtextarea (subs);
end;

function THTMLWriter.textarea (subs : TDOMNodelist) : THTML_textarea;
begin
  result := tagtextarea (subs);
end;

function THTMLWriter.Starttextarea : THTML_textarea;
begin
  result := THTML_textarea (StartElement (THTML_textarea));
end;

function THTMLWriter.Endtextarea : THTML_textarea;
begin
  result := THTML_textarea (EndElement (THTML_textarea));
end;

{ tag "tfoot", alias "tablefoot" }

function THTMLWriter.tagtfoot (s : string) : THTML_tfoot;
begin
  result := THTML_tfoot (CreateElement (THTML_tfoot, s));
end;

function THTMLWriter.tagtfoot (sub : THTMLCustomElement) : THTML_tfoot;
begin
  result := THTML_tfoot (CreateElement (THTML_tfoot, sub));
end;

function THTMLWriter.tagtfoot (subs : Array of THTMLCustomElement) : THTML_tfoot;
begin
  result := THTML_tfoot (CreateElement (THTML_tfoot, subs));
end;

function THTMLWriter.tagtfoot (subs : TDOMNodelist) : THTML_tfoot;
begin
  result := THTML_tfoot (CreateElement (THTML_tfoot, subs));
end;

function THTMLWriter.tablefoot (s : string) : THTML_tfoot;
begin
  result := tagtfoot (s);
end;

function THTMLWriter.tablefoot (sub : THTMLCustomElement) : THTML_tfoot;
begin
  result := tagtfoot (sub);
end;

function THTMLWriter.tablefoot (subs : Array of THTMLCustomElement) : THTML_tfoot;
begin
  result := tagtfoot (subs);
end;

function THTMLWriter.tablefoot (subs : TDOMNodelist) : THTML_tfoot;
begin
  result := tagtfoot (subs);
end;

function THTMLWriter.Starttfoot : THTML_tfoot;
begin
  result := THTML_tfoot (StartElement (THTML_tfoot));
end;

function THTMLWriter.Endtfoot : THTML_tfoot;
begin
  result := THTML_tfoot (EndElement (THTML_tfoot));
end;

function THTMLWriter.Starttablefoot : THTML_tfoot;
begin
  result := Starttfoot;
end;

function THTMLWriter.Endtablefoot : THTML_tfoot;
begin
  result := Endtfoot;
end;

{ tag "th", alias "tableheadcell" }

function THTMLWriter.tagth (s : string) : THTML_th;
begin
  result := THTML_th (CreateElement (THTML_th, s));
end;

function THTMLWriter.tagth (sub : THTMLCustomElement) : THTML_th;
begin
  result := THTML_th (CreateElement (THTML_th, sub));
end;

function THTMLWriter.tagth (subs : Array of THTMLCustomElement) : THTML_th;
begin
  result := THTML_th (CreateElement (THTML_th, subs));
end;

function THTMLWriter.tagth (subs : TDOMNodelist) : THTML_th;
begin
  result := THTML_th (CreateElement (THTML_th, subs));
end;

function THTMLWriter.tableheadcell (s : string) : THTML_th;
begin
  result := tagth (s);
end;

function THTMLWriter.tableheadcell (sub : THTMLCustomElement) : THTML_th;
begin
  result := tagth (sub);
end;

function THTMLWriter.tableheadcell (subs : Array of THTMLCustomElement) : THTML_th;
begin
  result := tagth (subs);
end;

function THTMLWriter.tableheadcell (subs : TDOMNodelist) : THTML_th;
begin
  result := tagth (subs);
end;

function THTMLWriter.Startth : THTML_th;
begin
  result := THTML_th (StartElement (THTML_th));
end;

function THTMLWriter.Endth : THTML_th;
begin
  result := THTML_th (EndElement (THTML_th));
end;

function THTMLWriter.Starttableheadcell : THTML_th;
begin
  result := Startth;
end;

function THTMLWriter.Endtableheadcell : THTML_th;
begin
  result := Endth;
end;

{ tag "thead", alias "tablehead" }

function THTMLWriter.tagthead (s : string) : THTML_thead;
begin
  result := THTML_thead (CreateElement (THTML_thead, s));
end;

function THTMLWriter.tagthead (sub : THTMLCustomElement) : THTML_thead;
begin
  result := THTML_thead (CreateElement (THTML_thead, sub));
end;

function THTMLWriter.tagthead (subs : Array of THTMLCustomElement) : THTML_thead;
begin
  result := THTML_thead (CreateElement (THTML_thead, subs));
end;

function THTMLWriter.tagthead (subs : TDOMNodelist) : THTML_thead;
begin
  result := THTML_thead (CreateElement (THTML_thead, subs));
end;

function THTMLWriter.tablehead (s : string) : THTML_thead;
begin
  result := tagthead (s);
end;

function THTMLWriter.tablehead (sub : THTMLCustomElement) : THTML_thead;
begin
  result := tagthead (sub);
end;

function THTMLWriter.tablehead (subs : Array of THTMLCustomElement) : THTML_thead;
begin
  result := tagthead (subs);
end;

function THTMLWriter.tablehead (subs : TDOMNodelist) : THTML_thead;
begin
  result := tagthead (subs);
end;

function THTMLWriter.Startthead : THTML_thead;
begin
  result := THTML_thead (StartElement (THTML_thead));
end;

function THTMLWriter.Endthead : THTML_thead;
begin
  result := THTML_thead (EndElement (THTML_thead));
end;

function THTMLWriter.Starttablehead : THTML_thead;
begin
  result := Startthead;
end;

function THTMLWriter.Endtablehead : THTML_thead;
begin
  result := Endthead;
end;

{ tag "title", alias "title" }

function THTMLWriter.tagtitle (s : string) : THTML_title;
begin
  result := THTML_title (CreateElement (THTML_title, s));
end;

function THTMLWriter.tagtitle (sub : THTMLCustomElement) : THTML_title;
begin
  result := THTML_title (CreateElement (THTML_title, sub));
end;

function THTMLWriter.tagtitle (subs : Array of THTMLCustomElement) : THTML_title;
begin
  result := THTML_title (CreateElement (THTML_title, subs));
end;

function THTMLWriter.tagtitle (subs : TDOMNodelist) : THTML_title;
begin
  result := THTML_title (CreateElement (THTML_title, subs));
end;

function THTMLWriter.title (s : string) : THTML_title;
begin
  result := tagtitle (s);
end;

function THTMLWriter.title (sub : THTMLCustomElement) : THTML_title;
begin
  result := tagtitle (sub);
end;

function THTMLWriter.title (subs : Array of THTMLCustomElement) : THTML_title;
begin
  result := tagtitle (subs);
end;

function THTMLWriter.title (subs : TDOMNodelist) : THTML_title;
begin
  result := tagtitle (subs);
end;

function THTMLWriter.Starttitle : THTML_title;
begin
  result := THTML_title (StartElement (THTML_title));
end;

function THTMLWriter.Endtitle : THTML_title;
begin
  result := THTML_title (EndElement (THTML_title));
end;

{ tag "tr", alias "tablerow" }

function THTMLWriter.tagtr (s : string) : THTML_tr;
begin
  result := THTML_tr (CreateElement (THTML_tr, s));
end;

function THTMLWriter.tagtr (sub : THTMLCustomElement) : THTML_tr;
begin
  result := THTML_tr (CreateElement (THTML_tr, sub));
end;

function THTMLWriter.tagtr (subs : Array of THTMLCustomElement) : THTML_tr;
begin
  result := THTML_tr (CreateElement (THTML_tr, subs));
end;

function THTMLWriter.tagtr (subs : TDOMNodelist) : THTML_tr;
begin
  result := THTML_tr (CreateElement (THTML_tr, subs));
end;

function THTMLWriter.tablerow (s : string) : THTML_tr;
begin
  result := tagtr (s);
end;

function THTMLWriter.tablerow (sub : THTMLCustomElement) : THTML_tr;
begin
  result := tagtr (sub);
end;

function THTMLWriter.tablerow (subs : Array of THTMLCustomElement) : THTML_tr;
begin
  result := tagtr (subs);
end;

function THTMLWriter.tablerow (subs : TDOMNodelist) : THTML_tr;
begin
  result := tagtr (subs);
end;

function THTMLWriter.Starttr : THTML_tr;
begin
  result := THTML_tr (StartElement (THTML_tr));
end;

function THTMLWriter.Endtr : THTML_tr;
begin
  result := THTML_tr (EndElement (THTML_tr));
end;

function THTMLWriter.Starttablerow : THTML_tr;
begin
  result := Starttr;
end;

function THTMLWriter.Endtablerow : THTML_tr;
begin
  result := Endtr;
end;

{ tag "tt", alias "teletype" }

function THTMLWriter.tagtt (s : string) : THTML_tt;
begin
  result := THTML_tt (CreateElement (THTML_tt, s));
end;

function THTMLWriter.tagtt (sub : THTMLCustomElement) : THTML_tt;
begin
  result := THTML_tt (CreateElement (THTML_tt, sub));
end;

function THTMLWriter.tagtt (subs : Array of THTMLCustomElement) : THTML_tt;
begin
  result := THTML_tt (CreateElement (THTML_tt, subs));
end;

function THTMLWriter.tagtt (subs : TDOMNodelist) : THTML_tt;
begin
  result := THTML_tt (CreateElement (THTML_tt, subs));
end;

function THTMLWriter.teletype (s : string) : THTML_tt;
begin
  result := tagtt (s);
end;

function THTMLWriter.teletype (sub : THTMLCustomElement) : THTML_tt;
begin
  result := tagtt (sub);
end;

function THTMLWriter.teletype (subs : Array of THTMLCustomElement) : THTML_tt;
begin
  result := tagtt (subs);
end;

function THTMLWriter.teletype (subs : TDOMNodelist) : THTML_tt;
begin
  result := tagtt (subs);
end;

function THTMLWriter.Starttt : THTML_tt;
begin
  result := THTML_tt (StartElement (THTML_tt));
end;

function THTMLWriter.Endtt : THTML_tt;
begin
  result := THTML_tt (EndElement (THTML_tt));
end;

function THTMLWriter.Startteletype : THTML_tt;
begin
  result := Starttt;
end;

function THTMLWriter.Endteletype : THTML_tt;
begin
  result := Endtt;
end;

{ tag "u", alias "underlined" }

function THTMLWriter.tagu (s : string) : THTML_u;
begin
  result := THTML_u (CreateElement (THTML_u, s));
end;

function THTMLWriter.tagu (sub : THTMLCustomElement) : THTML_u;
begin
  result := THTML_u (CreateElement (THTML_u, sub));
end;

function THTMLWriter.tagu (subs : Array of THTMLCustomElement) : THTML_u;
begin
  result := THTML_u (CreateElement (THTML_u, subs));
end;

function THTMLWriter.tagu (subs : TDOMNodelist) : THTML_u;
begin
  result := THTML_u (CreateElement (THTML_u, subs));
end;

function THTMLWriter.underlined (s : string) : THTML_u;
begin
  result := tagu (s);
end;

function THTMLWriter.underlined (sub : THTMLCustomElement) : THTML_u;
begin
  result := tagu (sub);
end;

function THTMLWriter.underlined (subs : Array of THTMLCustomElement) : THTML_u;
begin
  result := tagu (subs);
end;

function THTMLWriter.underlined (subs : TDOMNodelist) : THTML_u;
begin
  result := tagu (subs);
end;

function THTMLWriter.Startu : THTML_u;
begin
  result := THTML_u (StartElement (THTML_u));
end;

function THTMLWriter.Endu : THTML_u;
begin
  result := THTML_u (EndElement (THTML_u));
end;

function THTMLWriter.Startunderlined : THTML_u;
begin
  result := Startu;
end;

function THTMLWriter.Endunderlined : THTML_u;
begin
  result := Endu;
end;

{ tag "ul", alias "unorederedlist" }

function THTMLWriter.tagul (s : string) : THTML_ul;
begin
  result := THTML_ul (CreateElement (THTML_ul, s));
end;

function THTMLWriter.tagul (sub : THTMLCustomElement) : THTML_ul;
begin
  result := THTML_ul (CreateElement (THTML_ul, sub));
end;

function THTMLWriter.tagul (subs : Array of THTMLCustomElement) : THTML_ul;
begin
  result := THTML_ul (CreateElement (THTML_ul, subs));
end;

function THTMLWriter.tagul (subs : TDOMNodelist) : THTML_ul;
begin
  result := THTML_ul (CreateElement (THTML_ul, subs));
end;

function THTMLWriter.unorederedlist (s : string) : THTML_ul;
begin
  result := tagul (s);
end;

function THTMLWriter.unorederedlist (sub : THTMLCustomElement) : THTML_ul;
begin
  result := tagul (sub);
end;

function THTMLWriter.unorederedlist (subs : Array of THTMLCustomElement) : THTML_ul;
begin
  result := tagul (subs);
end;

function THTMLWriter.unorederedlist (subs : TDOMNodelist) : THTML_ul;
begin
  result := tagul (subs);
end;

function THTMLWriter.Startul : THTML_ul;
begin
  result := THTML_ul (StartElement (THTML_ul));
end;

function THTMLWriter.Endul : THTML_ul;
begin
  result := THTML_ul (EndElement (THTML_ul));
end;

function THTMLWriter.Startunorederedlist : THTML_ul;
begin
  result := Startul;
end;

function THTMLWriter.Endunorederedlist : THTML_ul;
begin
  result := Endul;
end;

{ tag "var", alias "variable" }

function THTMLWriter.tagvar (s : string) : THTML_var;
begin
  result := THTML_var (CreateElement (THTML_var, s));
end;

function THTMLWriter.tagvar (sub : THTMLCustomElement) : THTML_var;
begin
  result := THTML_var (CreateElement (THTML_var, sub));
end;

function THTMLWriter.tagvar (subs : Array of THTMLCustomElement) : THTML_var;
begin
  result := THTML_var (CreateElement (THTML_var, subs));
end;

function THTMLWriter.tagvar (subs : TDOMNodelist) : THTML_var;
begin
  result := THTML_var (CreateElement (THTML_var, subs));
end;

function THTMLWriter.variable (s : string) : THTML_var;
begin
  result := tagvar (s);
end;

function THTMLWriter.variable (sub : THTMLCustomElement) : THTML_var;
begin
  result := tagvar (sub);
end;

function THTMLWriter.variable (subs : Array of THTMLCustomElement) : THTML_var;
begin
  result := tagvar (subs);
end;

function THTMLWriter.variable (subs : TDOMNodelist) : THTML_var;
begin
  result := tagvar (subs);
end;

function THTMLWriter.Startvar : THTML_var;
begin
  result := THTML_var (StartElement (THTML_var));
end;

function THTMLWriter.Endvar : THTML_var;
begin
  result := THTML_var (EndElement (THTML_var));
end;

function THTMLWriter.Startvariable : THTML_var;
begin
  result := Startvar;
end;

function THTMLWriter.Endvariable : THTML_var;
begin
  result := Endvar;
end;

