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

    // tag "a", alias "anchor"
    function taga (s : string) : THTML_a;
    function taga (sub : THTMLCustomElement) : THTML_a;
    function taga (subs : Array of THTMLCustomElement) : THTML_a;
    function taga (subs : TDOMNodelist) : THTML_a;
    function anchor (s : string) : THTML_a;
    function anchor (sub : THTMLCustomElement) : THTML_a;
    function anchor (subs : Array of THTMLCustomElement) : THTML_a;
    function anchor (subs : TDOMNodelist) : THTML_a;
    function Starta : THTML_a;
    function Enda : THTML_a;
    function Startanchor : THTML_a;
    function Endanchor : THTML_a;

    // tag "abbr", alias "abbr"
    function tagabbr (s : string) : THTML_abbr;
    function tagabbr (sub : THTMLCustomElement) : THTML_abbr;
    function tagabbr (subs : Array of THTMLCustomElement) : THTML_abbr;
    function tagabbr (subs : TDOMNodelist) : THTML_abbr;
    function abbr (s : string) : THTML_abbr;
    function abbr (sub : THTMLCustomElement) : THTML_abbr;
    function abbr (subs : Array of THTMLCustomElement) : THTML_abbr;
    function abbr (subs : TDOMNodelist) : THTML_abbr;
    function Startabbr : THTML_abbr;
    function Endabbr : THTML_abbr;

    // tag "acronym", alias "acronym"
    function tagacronym (s : string) : THTML_acronym;
    function tagacronym (sub : THTMLCustomElement) : THTML_acronym;
    function tagacronym (subs : Array of THTMLCustomElement) : THTML_acronym;
    function tagacronym (subs : TDOMNodelist) : THTML_acronym;
    function acronym (s : string) : THTML_acronym;
    function acronym (sub : THTMLCustomElement) : THTML_acronym;
    function acronym (subs : Array of THTMLCustomElement) : THTML_acronym;
    function acronym (subs : TDOMNodelist) : THTML_acronym;
    function Startacronym : THTML_acronym;
    function Endacronym : THTML_acronym;

    // tag "address", alias "address"
    function tagaddress (s : string) : THTML_address;
    function tagaddress (sub : THTMLCustomElement) : THTML_address;
    function tagaddress (subs : Array of THTMLCustomElement) : THTML_address;
    function tagaddress (subs : TDOMNodelist) : THTML_address;
    function address (s : string) : THTML_address;
    function address (sub : THTMLCustomElement) : THTML_address;
    function address (subs : Array of THTMLCustomElement) : THTML_address;
    function address (subs : TDOMNodelist) : THTML_address;
    function Startaddress : THTML_address;
    function Endaddress : THTML_address;

    // tag "applet", alias "applet"
    function tagapplet (s : string) : THTML_applet;
    function tagapplet (sub : THTMLCustomElement) : THTML_applet;
    function tagapplet (subs : Array of THTMLCustomElement) : THTML_applet;
    function tagapplet (subs : TDOMNodelist) : THTML_applet;
    function applet (s : string) : THTML_applet;
    function applet (sub : THTMLCustomElement) : THTML_applet;
    function applet (subs : Array of THTMLCustomElement) : THTML_applet;
    function applet (subs : TDOMNodelist) : THTML_applet;
    function Startapplet : THTML_applet;
    function Endapplet : THTML_applet;

    // tag "area", alias "area"
    function tagarea : THTML_area;
    function area : THTML_area;

    // tag "b", alias "bold"
    function tagb (s : string) : THTML_b;
    function tagb (sub : THTMLCustomElement) : THTML_b;
    function tagb (subs : Array of THTMLCustomElement) : THTML_b;
    function tagb (subs : TDOMNodelist) : THTML_b;
    function bold (s : string) : THTML_b;
    function bold (sub : THTMLCustomElement) : THTML_b;
    function bold (subs : Array of THTMLCustomElement) : THTML_b;
    function bold (subs : TDOMNodelist) : THTML_b;
    function Startb : THTML_b;
    function Endb : THTML_b;
    function Startbold : THTML_b;
    function Endbold : THTML_b;

    // tag "base", alias "base"
    function tagbase : THTML_base;
    function base : THTML_base;

    // tag "basefont", alias "basefont"
    function tagbasefont : THTML_basefont;
    function basefont : THTML_basefont;

    // tag "bdo", alias "bdo"
    function tagbdo (s : string) : THTML_bdo;
    function tagbdo (sub : THTMLCustomElement) : THTML_bdo;
    function tagbdo (subs : Array of THTMLCustomElement) : THTML_bdo;
    function tagbdo (subs : TDOMNodelist) : THTML_bdo;
    function bdo (s : string) : THTML_bdo;
    function bdo (sub : THTMLCustomElement) : THTML_bdo;
    function bdo (subs : Array of THTMLCustomElement) : THTML_bdo;
    function bdo (subs : TDOMNodelist) : THTML_bdo;
    function Startbdo : THTML_bdo;
    function Endbdo : THTML_bdo;

    // tag "big", alias "big"
    function tagbig (s : string) : THTML_big;
    function tagbig (sub : THTMLCustomElement) : THTML_big;
    function tagbig (subs : Array of THTMLCustomElement) : THTML_big;
    function tagbig (subs : TDOMNodelist) : THTML_big;
    function big (s : string) : THTML_big;
    function big (sub : THTMLCustomElement) : THTML_big;
    function big (subs : Array of THTMLCustomElement) : THTML_big;
    function big (subs : TDOMNodelist) : THTML_big;
    function Startbig : THTML_big;
    function Endbig : THTML_big;

    // tag "blockquote", alias "blockquote"
    function tagblockquote (s : string) : THTML_blockquote;
    function tagblockquote (sub : THTMLCustomElement) : THTML_blockquote;
    function tagblockquote (subs : Array of THTMLCustomElement) : THTML_blockquote;
    function tagblockquote (subs : TDOMNodelist) : THTML_blockquote;
    function blockquote (s : string) : THTML_blockquote;
    function blockquote (sub : THTMLCustomElement) : THTML_blockquote;
    function blockquote (subs : Array of THTMLCustomElement) : THTML_blockquote;
    function blockquote (subs : TDOMNodelist) : THTML_blockquote;
    function Startblockquote : THTML_blockquote;
    function Endblockquote : THTML_blockquote;

    // tag "body", alias "body"
    function tagbody (s : string) : THTML_body;
    function tagbody (sub : THTMLCustomElement) : THTML_body;
    function tagbody (subs : Array of THTMLCustomElement) : THTML_body;
    function tagbody (subs : TDOMNodelist) : THTML_body;
    function body (s : string) : THTML_body;
    function body (sub : THTMLCustomElement) : THTML_body;
    function body (subs : Array of THTMLCustomElement) : THTML_body;
    function body (subs : TDOMNodelist) : THTML_body;
    function Startbody : THTML_body;
    function Endbody : THTML_body;

    // tag "br", alias "linebreak"
    function tagbr : THTML_br;
    function linebreak : THTML_br;

    // tag "button", alias "button"
    function tagbutton (s : string) : THTML_button;
    function tagbutton (sub : THTMLCustomElement) : THTML_button;
    function tagbutton (subs : Array of THTMLCustomElement) : THTML_button;
    function tagbutton (subs : TDOMNodelist) : THTML_button;
    function button (s : string) : THTML_button;
    function button (sub : THTMLCustomElement) : THTML_button;
    function button (subs : Array of THTMLCustomElement) : THTML_button;
    function button (subs : TDOMNodelist) : THTML_button;
    function Startbutton : THTML_button;
    function Endbutton : THTML_button;

    // tag "caption", alias "caption"
    function tagcaption (s : string) : THTML_caption;
    function tagcaption (sub : THTMLCustomElement) : THTML_caption;
    function tagcaption (subs : Array of THTMLCustomElement) : THTML_caption;
    function tagcaption (subs : TDOMNodelist) : THTML_caption;
    function caption (s : string) : THTML_caption;
    function caption (sub : THTMLCustomElement) : THTML_caption;
    function caption (subs : Array of THTMLCustomElement) : THTML_caption;
    function caption (subs : TDOMNodelist) : THTML_caption;
    function Startcaption : THTML_caption;
    function Endcaption : THTML_caption;

    // tag "center", alias "center"
    function tagcenter (s : string) : THTML_center;
    function tagcenter (sub : THTMLCustomElement) : THTML_center;
    function tagcenter (subs : Array of THTMLCustomElement) : THTML_center;
    function tagcenter (subs : TDOMNodelist) : THTML_center;
    function center (s : string) : THTML_center;
    function center (sub : THTMLCustomElement) : THTML_center;
    function center (subs : Array of THTMLCustomElement) : THTML_center;
    function center (subs : TDOMNodelist) : THTML_center;
    function Startcenter : THTML_center;
    function Endcenter : THTML_center;

    // tag "cite", alias "cite"
    function tagcite (s : string) : THTML_cite;
    function tagcite (sub : THTMLCustomElement) : THTML_cite;
    function tagcite (subs : Array of THTMLCustomElement) : THTML_cite;
    function tagcite (subs : TDOMNodelist) : THTML_cite;
    function cite (s : string) : THTML_cite;
    function cite (sub : THTMLCustomElement) : THTML_cite;
    function cite (subs : Array of THTMLCustomElement) : THTML_cite;
    function cite (subs : TDOMNodelist) : THTML_cite;
    function Startcite : THTML_cite;
    function Endcite : THTML_cite;

    // tag "code", alias "code"
    function tagcode (s : string) : THTML_code;
    function tagcode (sub : THTMLCustomElement) : THTML_code;
    function tagcode (subs : Array of THTMLCustomElement) : THTML_code;
    function tagcode (subs : TDOMNodelist) : THTML_code;
    function code (s : string) : THTML_code;
    function code (sub : THTMLCustomElement) : THTML_code;
    function code (subs : Array of THTMLCustomElement) : THTML_code;
    function code (subs : TDOMNodelist) : THTML_code;
    function Startcode : THTML_code;
    function Endcode : THTML_code;

    // tag "col", alias "column"
    function tagcol : THTML_col;
    function column : THTML_col;

    // tag "colgroup", alias "colgroup"
    function tagcolgroup (s : string) : THTML_colgroup;
    function tagcolgroup (sub : THTMLCustomElement) : THTML_colgroup;
    function tagcolgroup (subs : Array of THTMLCustomElement) : THTML_colgroup;
    function tagcolgroup (subs : TDOMNodelist) : THTML_colgroup;
    function colgroup (s : string) : THTML_colgroup;
    function colgroup (sub : THTMLCustomElement) : THTML_colgroup;
    function colgroup (subs : Array of THTMLCustomElement) : THTML_colgroup;
    function colgroup (subs : TDOMNodelist) : THTML_colgroup;
    function Startcolgroup : THTML_colgroup;
    function Endcolgroup : THTML_colgroup;

    // tag "dd", alias "defdescr"
    function tagdd (s : string) : THTML_dd;
    function tagdd (sub : THTMLCustomElement) : THTML_dd;
    function tagdd (subs : Array of THTMLCustomElement) : THTML_dd;
    function tagdd (subs : TDOMNodelist) : THTML_dd;
    function defdescr (s : string) : THTML_dd;
    function defdescr (sub : THTMLCustomElement) : THTML_dd;
    function defdescr (subs : Array of THTMLCustomElement) : THTML_dd;
    function defdescr (subs : TDOMNodelist) : THTML_dd;
    function Startdd : THTML_dd;
    function Enddd : THTML_dd;
    function Startdefdescr : THTML_dd;
    function Enddefdescr : THTML_dd;

    // tag "del", alias "deleted"
    function tagdel (s : string) : THTML_del;
    function tagdel (sub : THTMLCustomElement) : THTML_del;
    function tagdel (subs : Array of THTMLCustomElement) : THTML_del;
    function tagdel (subs : TDOMNodelist) : THTML_del;
    function deleted (s : string) : THTML_del;
    function deleted (sub : THTMLCustomElement) : THTML_del;
    function deleted (subs : Array of THTMLCustomElement) : THTML_del;
    function deleted (subs : TDOMNodelist) : THTML_del;
    function Startdel : THTML_del;
    function Enddel : THTML_del;
    function Startdeleted : THTML_del;
    function Enddeleted : THTML_del;

    // tag "dfn", alias "definition"
    function tagdfn (s : string) : THTML_dfn;
    function tagdfn (sub : THTMLCustomElement) : THTML_dfn;
    function tagdfn (subs : Array of THTMLCustomElement) : THTML_dfn;
    function tagdfn (subs : TDOMNodelist) : THTML_dfn;
    function definition (s : string) : THTML_dfn;
    function definition (sub : THTMLCustomElement) : THTML_dfn;
    function definition (subs : Array of THTMLCustomElement) : THTML_dfn;
    function definition (subs : TDOMNodelist) : THTML_dfn;
    function Startdfn : THTML_dfn;
    function Enddfn : THTML_dfn;
    function Startdefinition : THTML_dfn;
    function Enddefinition : THTML_dfn;

    // tag "dir", alias "dir"
    function tagdir (s : string) : THTML_dir;
    function tagdir (sub : THTMLCustomElement) : THTML_dir;
    function tagdir (subs : Array of THTMLCustomElement) : THTML_dir;
    function tagdir (subs : TDOMNodelist) : THTML_dir;
    function dir (s : string) : THTML_dir;
    function dir (sub : THTMLCustomElement) : THTML_dir;
    function dir (subs : Array of THTMLCustomElement) : THTML_dir;
    function dir (subs : TDOMNodelist) : THTML_dir;
    function Startdir : THTML_dir;
    function Enddir : THTML_dir;

    // tag "div", alias "divider"
    function tagdiv (s : string) : THTML_div;
    function tagdiv (sub : THTMLCustomElement) : THTML_div;
    function tagdiv (subs : Array of THTMLCustomElement) : THTML_div;
    function tagdiv (subs : TDOMNodelist) : THTML_div;
    function divider (s : string) : THTML_div;
    function divider (sub : THTMLCustomElement) : THTML_div;
    function divider (subs : Array of THTMLCustomElement) : THTML_div;
    function divider (subs : TDOMNodelist) : THTML_div;
    function Startdiv : THTML_div;
    function Enddiv : THTML_div;
    function Startdivider : THTML_div;
    function Enddivider : THTML_div;

    // tag "dl", alias "deflist"
    function tagdl (s : string) : THTML_dl;
    function tagdl (sub : THTMLCustomElement) : THTML_dl;
    function tagdl (subs : Array of THTMLCustomElement) : THTML_dl;
    function tagdl (subs : TDOMNodelist) : THTML_dl;
    function deflist (s : string) : THTML_dl;
    function deflist (sub : THTMLCustomElement) : THTML_dl;
    function deflist (subs : Array of THTMLCustomElement) : THTML_dl;
    function deflist (subs : TDOMNodelist) : THTML_dl;
    function Startdl : THTML_dl;
    function Enddl : THTML_dl;
    function Startdeflist : THTML_dl;
    function Enddeflist : THTML_dl;

    // tag "dt", alias "defterm"
    function tagdt (s : string) : THTML_dt;
    function tagdt (sub : THTMLCustomElement) : THTML_dt;
    function tagdt (subs : Array of THTMLCustomElement) : THTML_dt;
    function tagdt (subs : TDOMNodelist) : THTML_dt;
    function defterm (s : string) : THTML_dt;
    function defterm (sub : THTMLCustomElement) : THTML_dt;
    function defterm (subs : Array of THTMLCustomElement) : THTML_dt;
    function defterm (subs : TDOMNodelist) : THTML_dt;
    function Startdt : THTML_dt;
    function Enddt : THTML_dt;
    function Startdefterm : THTML_dt;
    function Enddefterm : THTML_dt;

    // tag "em", alias "emphasis"
    function tagem (s : string) : THTML_em;
    function tagem (sub : THTMLCustomElement) : THTML_em;
    function tagem (subs : Array of THTMLCustomElement) : THTML_em;
    function tagem (subs : TDOMNodelist) : THTML_em;
    function emphasis (s : string) : THTML_em;
    function emphasis (sub : THTMLCustomElement) : THTML_em;
    function emphasis (subs : Array of THTMLCustomElement) : THTML_em;
    function emphasis (subs : TDOMNodelist) : THTML_em;
    function Startem : THTML_em;
    function Endem : THTML_em;
    function Startemphasis : THTML_em;
    function Endemphasis : THTML_em;

    // tag "fieldset", alias "fieldset"
    function tagfieldset (s : string) : THTML_fieldset;
    function tagfieldset (sub : THTMLCustomElement) : THTML_fieldset;
    function tagfieldset (subs : Array of THTMLCustomElement) : THTML_fieldset;
    function tagfieldset (subs : TDOMNodelist) : THTML_fieldset;
    function fieldset (s : string) : THTML_fieldset;
    function fieldset (sub : THTMLCustomElement) : THTML_fieldset;
    function fieldset (subs : Array of THTMLCustomElement) : THTML_fieldset;
    function fieldset (subs : TDOMNodelist) : THTML_fieldset;
    function Startfieldset : THTML_fieldset;
    function Endfieldset : THTML_fieldset;

    // tag "font", alias "font"
    function tagfont (s : string) : THTML_font;
    function tagfont (sub : THTMLCustomElement) : THTML_font;
    function tagfont (subs : Array of THTMLCustomElement) : THTML_font;
    function tagfont (subs : TDOMNodelist) : THTML_font;
    function font (s : string) : THTML_font;
    function font (sub : THTMLCustomElement) : THTML_font;
    function font (subs : Array of THTMLCustomElement) : THTML_font;
    function font (subs : TDOMNodelist) : THTML_font;
    function Startfont : THTML_font;
    function Endfont : THTML_font;

    // tag "form", alias "form"
    function tagform (s : string) : THTML_form;
    function tagform (sub : THTMLCustomElement) : THTML_form;
    function tagform (subs : Array of THTMLCustomElement) : THTML_form;
    function tagform (subs : TDOMNodelist) : THTML_form;
    function form (s : string) : THTML_form;
    function form (sub : THTMLCustomElement) : THTML_form;
    function form (subs : Array of THTMLCustomElement) : THTML_form;
    function form (subs : TDOMNodelist) : THTML_form;
    function Startform : THTML_form;
    function Endform : THTML_form;

    // tag "frame", alias "frame"
    function tagframe : THTML_frame;
    function frame : THTML_frame;

    // tag "frameset", alias "frameset"
    function tagframeset (s : string) : THTML_frameset;
    function tagframeset (sub : THTMLCustomElement) : THTML_frameset;
    function tagframeset (subs : Array of THTMLCustomElement) : THTML_frameset;
    function tagframeset (subs : TDOMNodelist) : THTML_frameset;
    function frameset (s : string) : THTML_frameset;
    function frameset (sub : THTMLCustomElement) : THTML_frameset;
    function frameset (subs : Array of THTMLCustomElement) : THTML_frameset;
    function frameset (subs : TDOMNodelist) : THTML_frameset;
    function Startframeset : THTML_frameset;
    function Endframeset : THTML_frameset;

    // tag "h1", alias "heading1"
    function tagh1 (s : string) : THTML_h1;
    function tagh1 (sub : THTMLCustomElement) : THTML_h1;
    function tagh1 (subs : Array of THTMLCustomElement) : THTML_h1;
    function tagh1 (subs : TDOMNodelist) : THTML_h1;
    function heading1 (s : string) : THTML_h1;
    function heading1 (sub : THTMLCustomElement) : THTML_h1;
    function heading1 (subs : Array of THTMLCustomElement) : THTML_h1;
    function heading1 (subs : TDOMNodelist) : THTML_h1;
    function Starth1 : THTML_h1;
    function Endh1 : THTML_h1;
    function Startheading1 : THTML_h1;
    function Endheading1 : THTML_h1;

    // tag "h2", alias "heading2"
    function tagh2 (s : string) : THTML_h2;
    function tagh2 (sub : THTMLCustomElement) : THTML_h2;
    function tagh2 (subs : Array of THTMLCustomElement) : THTML_h2;
    function tagh2 (subs : TDOMNodelist) : THTML_h2;
    function heading2 (s : string) : THTML_h2;
    function heading2 (sub : THTMLCustomElement) : THTML_h2;
    function heading2 (subs : Array of THTMLCustomElement) : THTML_h2;
    function heading2 (subs : TDOMNodelist) : THTML_h2;
    function Starth2 : THTML_h2;
    function Endh2 : THTML_h2;
    function Startheading2 : THTML_h2;
    function Endheading2 : THTML_h2;

    // tag "h3", alias "heading3"
    function tagh3 (s : string) : THTML_h3;
    function tagh3 (sub : THTMLCustomElement) : THTML_h3;
    function tagh3 (subs : Array of THTMLCustomElement) : THTML_h3;
    function tagh3 (subs : TDOMNodelist) : THTML_h3;
    function heading3 (s : string) : THTML_h3;
    function heading3 (sub : THTMLCustomElement) : THTML_h3;
    function heading3 (subs : Array of THTMLCustomElement) : THTML_h3;
    function heading3 (subs : TDOMNodelist) : THTML_h3;
    function Starth3 : THTML_h3;
    function Endh3 : THTML_h3;
    function Startheading3 : THTML_h3;
    function Endheading3 : THTML_h3;

    // tag "h4", alias "heading4"
    function tagh4 (s : string) : THTML_h4;
    function tagh4 (sub : THTMLCustomElement) : THTML_h4;
    function tagh4 (subs : Array of THTMLCustomElement) : THTML_h4;
    function tagh4 (subs : TDOMNodelist) : THTML_h4;
    function heading4 (s : string) : THTML_h4;
    function heading4 (sub : THTMLCustomElement) : THTML_h4;
    function heading4 (subs : Array of THTMLCustomElement) : THTML_h4;
    function heading4 (subs : TDOMNodelist) : THTML_h4;
    function Starth4 : THTML_h4;
    function Endh4 : THTML_h4;
    function Startheading4 : THTML_h4;
    function Endheading4 : THTML_h4;

    // tag "h5", alias "heading5"
    function tagh5 (s : string) : THTML_h5;
    function tagh5 (sub : THTMLCustomElement) : THTML_h5;
    function tagh5 (subs : Array of THTMLCustomElement) : THTML_h5;
    function tagh5 (subs : TDOMNodelist) : THTML_h5;
    function heading5 (s : string) : THTML_h5;
    function heading5 (sub : THTMLCustomElement) : THTML_h5;
    function heading5 (subs : Array of THTMLCustomElement) : THTML_h5;
    function heading5 (subs : TDOMNodelist) : THTML_h5;
    function Starth5 : THTML_h5;
    function Endh5 : THTML_h5;
    function Startheading5 : THTML_h5;
    function Endheading5 : THTML_h5;

    // tag "h6", alias "heading6"
    function tagh6 (s : string) : THTML_h6;
    function tagh6 (sub : THTMLCustomElement) : THTML_h6;
    function tagh6 (subs : Array of THTMLCustomElement) : THTML_h6;
    function tagh6 (subs : TDOMNodelist) : THTML_h6;
    function heading6 (s : string) : THTML_h6;
    function heading6 (sub : THTMLCustomElement) : THTML_h6;
    function heading6 (subs : Array of THTMLCustomElement) : THTML_h6;
    function heading6 (subs : TDOMNodelist) : THTML_h6;
    function Starth6 : THTML_h6;
    function Endh6 : THTML_h6;
    function Startheading6 : THTML_h6;
    function Endheading6 : THTML_h6;

    // tag "head", alias "header"
    function taghead (s : string) : THTML_head;
    function taghead (sub : THTMLCustomElement) : THTML_head;
    function taghead (subs : Array of THTMLCustomElement) : THTML_head;
    function taghead (subs : TDOMNodelist) : THTML_head;
    function header (s : string) : THTML_head;
    function header (sub : THTMLCustomElement) : THTML_head;
    function header (subs : Array of THTMLCustomElement) : THTML_head;
    function header (subs : TDOMNodelist) : THTML_head;
    function Starthead : THTML_head;
    function Endhead : THTML_head;
    function Startheader : THTML_head;
    function Endheader : THTML_head;

    // tag "hr", alias "horrule"
    function taghr : THTML_hr;
    function horrule : THTML_hr;

    // tag "html", alias "html"
    function taghtml (s : string) : THTML_html;
    function taghtml (sub : THTMLCustomElement) : THTML_html;
    function taghtml (subs : Array of THTMLCustomElement) : THTML_html;
    function taghtml (subs : TDOMNodelist) : THTML_html;
    function html (s : string) : THTML_html;
    function html (sub : THTMLCustomElement) : THTML_html;
    function html (subs : Array of THTMLCustomElement) : THTML_html;
    function html (subs : TDOMNodelist) : THTML_html;
    function Starthtml : THTML_html;
    function Endhtml : THTML_html;

    // tag "i", alias "italic"
    function tagi (s : string) : THTML_i;
    function tagi (sub : THTMLCustomElement) : THTML_i;
    function tagi (subs : Array of THTMLCustomElement) : THTML_i;
    function tagi (subs : TDOMNodelist) : THTML_i;
    function italic (s : string) : THTML_i;
    function italic (sub : THTMLCustomElement) : THTML_i;
    function italic (subs : Array of THTMLCustomElement) : THTML_i;
    function italic (subs : TDOMNodelist) : THTML_i;
    function Starti : THTML_i;
    function Endi : THTML_i;
    function Startitalic : THTML_i;
    function Enditalic : THTML_i;

    // tag "iframe", alias "inlineframe"
    function tagiframe (s : string) : THTML_iframe;
    function tagiframe (sub : THTMLCustomElement) : THTML_iframe;
    function tagiframe (subs : Array of THTMLCustomElement) : THTML_iframe;
    function tagiframe (subs : TDOMNodelist) : THTML_iframe;
    function inlineframe (s : string) : THTML_iframe;
    function inlineframe (sub : THTMLCustomElement) : THTML_iframe;
    function inlineframe (subs : Array of THTMLCustomElement) : THTML_iframe;
    function inlineframe (subs : TDOMNodelist) : THTML_iframe;
    function Startiframe : THTML_iframe;
    function Endiframe : THTML_iframe;
    function Startinlineframe : THTML_iframe;
    function Endinlineframe : THTML_iframe;

    // tag "img", alias "image"
    function tagimg : THTML_img;
    function image : THTML_img;

    // tag "input", alias "input"
    function taginput : THTML_input;
    function input : THTML_input;

    // tag "ins", alias "inserted"
    function tagins (s : string) : THTML_ins;
    function tagins (sub : THTMLCustomElement) : THTML_ins;
    function tagins (subs : Array of THTMLCustomElement) : THTML_ins;
    function tagins (subs : TDOMNodelist) : THTML_ins;
    function inserted (s : string) : THTML_ins;
    function inserted (sub : THTMLCustomElement) : THTML_ins;
    function inserted (subs : Array of THTMLCustomElement) : THTML_ins;
    function inserted (subs : TDOMNodelist) : THTML_ins;
    function Startins : THTML_ins;
    function Endins : THTML_ins;
    function Startinserted : THTML_ins;
    function Endinserted : THTML_ins;

    // tag "isindex", alias "isindex"
    function tagisindex : THTML_isindex;
    function isindex : THTML_isindex;

    // tag "kbd", alias "enteredtext"
    function tagkbd (s : string) : THTML_kbd;
    function tagkbd (sub : THTMLCustomElement) : THTML_kbd;
    function tagkbd (subs : Array of THTMLCustomElement) : THTML_kbd;
    function tagkbd (subs : TDOMNodelist) : THTML_kbd;
    function enteredtext (s : string) : THTML_kbd;
    function enteredtext (sub : THTMLCustomElement) : THTML_kbd;
    function enteredtext (subs : Array of THTMLCustomElement) : THTML_kbd;
    function enteredtext (subs : TDOMNodelist) : THTML_kbd;
    function Startkbd : THTML_kbd;
    function Endkbd : THTML_kbd;
    function Startenteredtext : THTML_kbd;
    function Endenteredtext : THTML_kbd;

    // tag "label", alias "alabel"
    function taglabel (s : string) : THTML_label;
    function taglabel (sub : THTMLCustomElement) : THTML_label;
    function taglabel (subs : Array of THTMLCustomElement) : THTML_label;
    function taglabel (subs : TDOMNodelist) : THTML_label;
    function alabel (s : string) : THTML_label;
    function alabel (sub : THTMLCustomElement) : THTML_label;
    function alabel (subs : Array of THTMLCustomElement) : THTML_label;
    function alabel (subs : TDOMNodelist) : THTML_label;
    function Startlabel : THTML_label;
    function Endlabel : THTML_label;
    function Startalabel : THTML_label;
    function Endalabel : THTML_label;

    // tag "legend", alias "legend"
    function taglegend (s : string) : THTML_legend;
    function taglegend (sub : THTMLCustomElement) : THTML_legend;
    function taglegend (subs : Array of THTMLCustomElement) : THTML_legend;
    function taglegend (subs : TDOMNodelist) : THTML_legend;
    function legend (s : string) : THTML_legend;
    function legend (sub : THTMLCustomElement) : THTML_legend;
    function legend (subs : Array of THTMLCustomElement) : THTML_legend;
    function legend (subs : TDOMNodelist) : THTML_legend;
    function Startlegend : THTML_legend;
    function Endlegend : THTML_legend;

    // tag "li", alias "listitem"
    function tagli (s : string) : THTML_li;
    function tagli (sub : THTMLCustomElement) : THTML_li;
    function tagli (subs : Array of THTMLCustomElement) : THTML_li;
    function tagli (subs : TDOMNodelist) : THTML_li;
    function listitem (s : string) : THTML_li;
    function listitem (sub : THTMLCustomElement) : THTML_li;
    function listitem (subs : Array of THTMLCustomElement) : THTML_li;
    function listitem (subs : TDOMNodelist) : THTML_li;
    function Startli : THTML_li;
    function Endli : THTML_li;
    function Startlistitem : THTML_li;
    function Endlistitem : THTML_li;

    // tag "link", alias "link"
    function taglink : THTML_link;
    function link : THTML_link;

    // tag "map", alias "map"
    function tagmap (s : string) : THTML_map;
    function tagmap (sub : THTMLCustomElement) : THTML_map;
    function tagmap (subs : Array of THTMLCustomElement) : THTML_map;
    function tagmap (subs : TDOMNodelist) : THTML_map;
    function map (s : string) : THTML_map;
    function map (sub : THTMLCustomElement) : THTML_map;
    function map (subs : Array of THTMLCustomElement) : THTML_map;
    function map (subs : TDOMNodelist) : THTML_map;
    function Startmap : THTML_map;
    function Endmap : THTML_map;

    // tag "menu", alias "menu"
    function tagmenu (s : string) : THTML_menu;
    function tagmenu (sub : THTMLCustomElement) : THTML_menu;
    function tagmenu (subs : Array of THTMLCustomElement) : THTML_menu;
    function tagmenu (subs : TDOMNodelist) : THTML_menu;
    function menu (s : string) : THTML_menu;
    function menu (sub : THTMLCustomElement) : THTML_menu;
    function menu (subs : Array of THTMLCustomElement) : THTML_menu;
    function menu (subs : TDOMNodelist) : THTML_menu;
    function Startmenu : THTML_menu;
    function Endmenu : THTML_menu;

    // tag "meta", alias "meta"
    function tagmeta : THTML_meta;
    function meta : THTML_meta;

    // tag "noframes", alias "noframes"
    function tagnoframes (s : string) : THTML_noframes;
    function tagnoframes (sub : THTMLCustomElement) : THTML_noframes;
    function tagnoframes (subs : Array of THTMLCustomElement) : THTML_noframes;
    function tagnoframes (subs : TDOMNodelist) : THTML_noframes;
    function noframes (s : string) : THTML_noframes;
    function noframes (sub : THTMLCustomElement) : THTML_noframes;
    function noframes (subs : Array of THTMLCustomElement) : THTML_noframes;
    function noframes (subs : TDOMNodelist) : THTML_noframes;
    function Startnoframes : THTML_noframes;
    function Endnoframes : THTML_noframes;

    // tag "noscript", alias "noscript"
    function tagnoscript (s : string) : THTML_noscript;
    function tagnoscript (sub : THTMLCustomElement) : THTML_noscript;
    function tagnoscript (subs : Array of THTMLCustomElement) : THTML_noscript;
    function tagnoscript (subs : TDOMNodelist) : THTML_noscript;
    function noscript (s : string) : THTML_noscript;
    function noscript (sub : THTMLCustomElement) : THTML_noscript;
    function noscript (subs : Array of THTMLCustomElement) : THTML_noscript;
    function noscript (subs : TDOMNodelist) : THTML_noscript;
    function Startnoscript : THTML_noscript;
    function Endnoscript : THTML_noscript;

    // tag "object", alias "anobject"
    function tagobject (s : string) : THTML_object;
    function tagobject (sub : THTMLCustomElement) : THTML_object;
    function tagobject (subs : Array of THTMLCustomElement) : THTML_object;
    function tagobject (subs : TDOMNodelist) : THTML_object;
    function anobject (s : string) : THTML_object;
    function anobject (sub : THTMLCustomElement) : THTML_object;
    function anobject (subs : Array of THTMLCustomElement) : THTML_object;
    function anobject (subs : TDOMNodelist) : THTML_object;
    function Startobject : THTML_object;
    function Endobject : THTML_object;
    function Startanobject : THTML_object;
    function Endanobject : THTML_object;

    // tag "ol", alias "orderedlist"
    function tagol (s : string) : THTML_ol;
    function tagol (sub : THTMLCustomElement) : THTML_ol;
    function tagol (subs : Array of THTMLCustomElement) : THTML_ol;
    function tagol (subs : TDOMNodelist) : THTML_ol;
    function orderedlist (s : string) : THTML_ol;
    function orderedlist (sub : THTMLCustomElement) : THTML_ol;
    function orderedlist (subs : Array of THTMLCustomElement) : THTML_ol;
    function orderedlist (subs : TDOMNodelist) : THTML_ol;
    function Startol : THTML_ol;
    function Endol : THTML_ol;
    function Startorderedlist : THTML_ol;
    function Endorderedlist : THTML_ol;

    // tag "optgroup", alias "optiongroup"
    function tagoptgroup (s : string) : THTML_optgroup;
    function tagoptgroup (sub : THTMLCustomElement) : THTML_optgroup;
    function tagoptgroup (subs : Array of THTMLCustomElement) : THTML_optgroup;
    function tagoptgroup (subs : TDOMNodelist) : THTML_optgroup;
    function optiongroup (s : string) : THTML_optgroup;
    function optiongroup (sub : THTMLCustomElement) : THTML_optgroup;
    function optiongroup (subs : Array of THTMLCustomElement) : THTML_optgroup;
    function optiongroup (subs : TDOMNodelist) : THTML_optgroup;
    function Startoptgroup : THTML_optgroup;
    function Endoptgroup : THTML_optgroup;
    function Startoptiongroup : THTML_optgroup;
    function Endoptiongroup : THTML_optgroup;

    // tag "option", alias "option"
    function tagoption (s : string) : THTML_option;
    function tagoption (sub : THTMLCustomElement) : THTML_option;
    function tagoption (subs : Array of THTMLCustomElement) : THTML_option;
    function tagoption (subs : TDOMNodelist) : THTML_option;
    function option (s : string) : THTML_option;
    function option (sub : THTMLCustomElement) : THTML_option;
    function option (subs : Array of THTMLCustomElement) : THTML_option;
    function option (subs : TDOMNodelist) : THTML_option;
    function Startoption : THTML_option;
    function Endoption : THTML_option;

    // tag "p", alias "paragraph"
    function tagp (s : string) : THTML_p;
    function tagp (sub : THTMLCustomElement) : THTML_p;
    function tagp (subs : Array of THTMLCustomElement) : THTML_p;
    function tagp (subs : TDOMNodelist) : THTML_p;
    function paragraph (s : string) : THTML_p;
    function paragraph (sub : THTMLCustomElement) : THTML_p;
    function paragraph (subs : Array of THTMLCustomElement) : THTML_p;
    function paragraph (subs : TDOMNodelist) : THTML_p;
    function Startp : THTML_p;
    function Endp : THTML_p;
    function Startparagraph : THTML_p;
    function Endparagraph : THTML_p;

    // tag "param", alias "parameter"
    function tagparam : THTML_param;
    function parameter : THTML_param;

    // tag "pre", alias "preformatted"
    function tagpre (s : string) : THTML_pre;
    function tagpre (sub : THTMLCustomElement) : THTML_pre;
    function tagpre (subs : Array of THTMLCustomElement) : THTML_pre;
    function tagpre (subs : TDOMNodelist) : THTML_pre;
    function preformatted (s : string) : THTML_pre;
    function preformatted (sub : THTMLCustomElement) : THTML_pre;
    function preformatted (subs : Array of THTMLCustomElement) : THTML_pre;
    function preformatted (subs : TDOMNodelist) : THTML_pre;
    function Startpre : THTML_pre;
    function Endpre : THTML_pre;
    function Startpreformatted : THTML_pre;
    function Endpreformatted : THTML_pre;

    // tag "q", alias "quotation"
    function tagq (s : string) : THTML_q;
    function tagq (sub : THTMLCustomElement) : THTML_q;
    function tagq (subs : Array of THTMLCustomElement) : THTML_q;
    function tagq (subs : TDOMNodelist) : THTML_q;
    function quotation (s : string) : THTML_q;
    function quotation (sub : THTMLCustomElement) : THTML_q;
    function quotation (subs : Array of THTMLCustomElement) : THTML_q;
    function quotation (subs : TDOMNodelist) : THTML_q;
    function Startq : THTML_q;
    function Endq : THTML_q;
    function Startquotation : THTML_q;
    function Endquotation : THTML_q;

    // tag "s", alias "strikethrough"
    function tags (s : string) : THTML_s;
    function tags (sub : THTMLCustomElement) : THTML_s;
    function tags (subs : Array of THTMLCustomElement) : THTML_s;
    function tags (subs : TDOMNodelist) : THTML_s;
    function strikethrough (s : string) : THTML_s;
    function strikethrough (sub : THTMLCustomElement) : THTML_s;
    function strikethrough (subs : Array of THTMLCustomElement) : THTML_s;
    function strikethrough (subs : TDOMNodelist) : THTML_s;
    function Starts : THTML_s;
    function Ends : THTML_s;
    function Startstrikethrough : THTML_s;
    function Endstrikethrough : THTML_s;

    // tag "samp", alias "sample"
    function tagsamp (s : string) : THTML_samp;
    function tagsamp (sub : THTMLCustomElement) : THTML_samp;
    function tagsamp (subs : Array of THTMLCustomElement) : THTML_samp;
    function tagsamp (subs : TDOMNodelist) : THTML_samp;
    function sample (s : string) : THTML_samp;
    function sample (sub : THTMLCustomElement) : THTML_samp;
    function sample (subs : Array of THTMLCustomElement) : THTML_samp;
    function sample (subs : TDOMNodelist) : THTML_samp;
    function Startsamp : THTML_samp;
    function Endsamp : THTML_samp;
    function Startsample : THTML_samp;
    function Endsample : THTML_samp;

    // tag "script", alias "script"
    function tagscript (s : string) : THTML_script;
    function tagscript (sub : THTMLCustomElement) : THTML_script;
    function tagscript (subs : Array of THTMLCustomElement) : THTML_script;
    function tagscript (subs : TDOMNodelist) : THTML_script;
    function script (s : string) : THTML_script;
    function script (sub : THTMLCustomElement) : THTML_script;
    function script (subs : Array of THTMLCustomElement) : THTML_script;
    function script (subs : TDOMNodelist) : THTML_script;
    function Startscript : THTML_script;
    function Endscript : THTML_script;

    // tag "select", alias "select"
    function tagselect (s : string) : THTML_select;
    function tagselect (sub : THTMLCustomElement) : THTML_select;
    function tagselect (subs : Array of THTMLCustomElement) : THTML_select;
    function tagselect (subs : TDOMNodelist) : THTML_select;
    function select (s : string) : THTML_select;
    function select (sub : THTMLCustomElement) : THTML_select;
    function select (subs : Array of THTMLCustomElement) : THTML_select;
    function select (subs : TDOMNodelist) : THTML_select;
    function Startselect : THTML_select;
    function Endselect : THTML_select;

    // tag "small", alias "small"
    function tagsmall (s : string) : THTML_small;
    function tagsmall (sub : THTMLCustomElement) : THTML_small;
    function tagsmall (subs : Array of THTMLCustomElement) : THTML_small;
    function tagsmall (subs : TDOMNodelist) : THTML_small;
    function small (s : string) : THTML_small;
    function small (sub : THTMLCustomElement) : THTML_small;
    function small (subs : Array of THTMLCustomElement) : THTML_small;
    function small (subs : TDOMNodelist) : THTML_small;
    function Startsmall : THTML_small;
    function Endsmall : THTML_small;

    // tag "span", alias "span"
    function tagspan (s : string) : THTML_span;
    function tagspan (sub : THTMLCustomElement) : THTML_span;
    function tagspan (subs : Array of THTMLCustomElement) : THTML_span;
    function tagspan (subs : TDOMNodelist) : THTML_span;
    function span (s : string) : THTML_span;
    function span (sub : THTMLCustomElement) : THTML_span;
    function span (subs : Array of THTMLCustomElement) : THTML_span;
    function span (subs : TDOMNodelist) : THTML_span;
    function Startspan : THTML_span;
    function Endspan : THTML_span;

    // tag "strike", alias "strike"
    function tagstrike (s : string) : THTML_strike;
    function tagstrike (sub : THTMLCustomElement) : THTML_strike;
    function tagstrike (subs : Array of THTMLCustomElement) : THTML_strike;
    function tagstrike (subs : TDOMNodelist) : THTML_strike;
    function strike (s : string) : THTML_strike;
    function strike (sub : THTMLCustomElement) : THTML_strike;
    function strike (subs : Array of THTMLCustomElement) : THTML_strike;
    function strike (subs : TDOMNodelist) : THTML_strike;
    function Startstrike : THTML_strike;
    function Endstrike : THTML_strike;

    // tag "strong", alias "strong"
    function tagstrong (s : string) : THTML_strong;
    function tagstrong (sub : THTMLCustomElement) : THTML_strong;
    function tagstrong (subs : Array of THTMLCustomElement) : THTML_strong;
    function tagstrong (subs : TDOMNodelist) : THTML_strong;
    function strong (s : string) : THTML_strong;
    function strong (sub : THTMLCustomElement) : THTML_strong;
    function strong (subs : Array of THTMLCustomElement) : THTML_strong;
    function strong (subs : TDOMNodelist) : THTML_strong;
    function Startstrong : THTML_strong;
    function Endstrong : THTML_strong;

    // tag "style", alias "style"
    function tagstyle (s : string) : THTML_style;
    function tagstyle (sub : THTMLCustomElement) : THTML_style;
    function tagstyle (subs : Array of THTMLCustomElement) : THTML_style;
    function tagstyle (subs : TDOMNodelist) : THTML_style;
    function style (s : string) : THTML_style;
    function style (sub : THTMLCustomElement) : THTML_style;
    function style (subs : Array of THTMLCustomElement) : THTML_style;
    function style (subs : TDOMNodelist) : THTML_style;
    function Startstyle : THTML_style;
    function Endstyle : THTML_style;

    // tag "sub", alias "subscript"
    function tagsub (s : string) : THTML_sub;
    function tagsub (sub : THTMLCustomElement) : THTML_sub;
    function tagsub (subs : Array of THTMLCustomElement) : THTML_sub;
    function tagsub (subs : TDOMNodelist) : THTML_sub;
    function subscript (s : string) : THTML_sub;
    function subscript (sub : THTMLCustomElement) : THTML_sub;
    function subscript (subs : Array of THTMLCustomElement) : THTML_sub;
    function subscript (subs : TDOMNodelist) : THTML_sub;
    function Startsub : THTML_sub;
    function Endsub : THTML_sub;
    function Startsubscript : THTML_sub;
    function Endsubscript : THTML_sub;

    // tag "sup", alias "superscript"
    function tagsup (s : string) : THTML_sup;
    function tagsup (sub : THTMLCustomElement) : THTML_sup;
    function tagsup (subs : Array of THTMLCustomElement) : THTML_sup;
    function tagsup (subs : TDOMNodelist) : THTML_sup;
    function superscript (s : string) : THTML_sup;
    function superscript (sub : THTMLCustomElement) : THTML_sup;
    function superscript (subs : Array of THTMLCustomElement) : THTML_sup;
    function superscript (subs : TDOMNodelist) : THTML_sup;
    function Startsup : THTML_sup;
    function Endsup : THTML_sup;
    function Startsuperscript : THTML_sup;
    function Endsuperscript : THTML_sup;

    // tag "table", alias "table"
    function tagtable (s : string) : THTML_table;
    function tagtable (sub : THTMLCustomElement) : THTML_table;
    function tagtable (subs : Array of THTMLCustomElement) : THTML_table;
    function tagtable (subs : TDOMNodelist) : THTML_table;
    function table (s : string) : THTML_table;
    function table (sub : THTMLCustomElement) : THTML_table;
    function table (subs : Array of THTMLCustomElement) : THTML_table;
    function table (subs : TDOMNodelist) : THTML_table;
    function Starttable : THTML_table;
    function Endtable : THTML_table;

    // tag "tbody", alias "tablebody"
    function tagtbody (s : string) : THTML_tbody;
    function tagtbody (sub : THTMLCustomElement) : THTML_tbody;
    function tagtbody (subs : Array of THTMLCustomElement) : THTML_tbody;
    function tagtbody (subs : TDOMNodelist) : THTML_tbody;
    function tablebody (s : string) : THTML_tbody;
    function tablebody (sub : THTMLCustomElement) : THTML_tbody;
    function tablebody (subs : Array of THTMLCustomElement) : THTML_tbody;
    function tablebody (subs : TDOMNodelist) : THTML_tbody;
    function Starttbody : THTML_tbody;
    function Endtbody : THTML_tbody;
    function Starttablebody : THTML_tbody;
    function Endtablebody : THTML_tbody;

    // tag "td", alias "tablecell"
    function tagtd (s : string) : THTML_td;
    function tagtd (sub : THTMLCustomElement) : THTML_td;
    function tagtd (subs : Array of THTMLCustomElement) : THTML_td;
    function tagtd (subs : TDOMNodelist) : THTML_td;
    function tablecell (s : string) : THTML_td;
    function tablecell (sub : THTMLCustomElement) : THTML_td;
    function tablecell (subs : Array of THTMLCustomElement) : THTML_td;
    function tablecell (subs : TDOMNodelist) : THTML_td;
    function Starttd : THTML_td;
    function Endtd : THTML_td;
    function Starttablecell : THTML_td;
    function Endtablecell : THTML_td;

    // tag "textarea", alias "textarea"
    function tagtextarea (s : string) : THTML_textarea;
    function tagtextarea (sub : THTMLCustomElement) : THTML_textarea;
    function tagtextarea (subs : Array of THTMLCustomElement) : THTML_textarea;
    function tagtextarea (subs : TDOMNodelist) : THTML_textarea;
    function textarea (s : string) : THTML_textarea;
    function textarea (sub : THTMLCustomElement) : THTML_textarea;
    function textarea (subs : Array of THTMLCustomElement) : THTML_textarea;
    function textarea (subs : TDOMNodelist) : THTML_textarea;
    function Starttextarea : THTML_textarea;
    function Endtextarea : THTML_textarea;

    // tag "tfoot", alias "tablefoot"
    function tagtfoot (s : string) : THTML_tfoot;
    function tagtfoot (sub : THTMLCustomElement) : THTML_tfoot;
    function tagtfoot (subs : Array of THTMLCustomElement) : THTML_tfoot;
    function tagtfoot (subs : TDOMNodelist) : THTML_tfoot;
    function tablefoot (s : string) : THTML_tfoot;
    function tablefoot (sub : THTMLCustomElement) : THTML_tfoot;
    function tablefoot (subs : Array of THTMLCustomElement) : THTML_tfoot;
    function tablefoot (subs : TDOMNodelist) : THTML_tfoot;
    function Starttfoot : THTML_tfoot;
    function Endtfoot : THTML_tfoot;
    function Starttablefoot : THTML_tfoot;
    function Endtablefoot : THTML_tfoot;

    // tag "th", alias "tableheadcell"
    function tagth (s : string) : THTML_th;
    function tagth (sub : THTMLCustomElement) : THTML_th;
    function tagth (subs : Array of THTMLCustomElement) : THTML_th;
    function tagth (subs : TDOMNodelist) : THTML_th;
    function tableheadcell (s : string) : THTML_th;
    function tableheadcell (sub : THTMLCustomElement) : THTML_th;
    function tableheadcell (subs : Array of THTMLCustomElement) : THTML_th;
    function tableheadcell (subs : TDOMNodelist) : THTML_th;
    function Startth : THTML_th;
    function Endth : THTML_th;
    function Starttableheadcell : THTML_th;
    function Endtableheadcell : THTML_th;

    // tag "thead", alias "tablehead"
    function tagthead (s : string) : THTML_thead;
    function tagthead (sub : THTMLCustomElement) : THTML_thead;
    function tagthead (subs : Array of THTMLCustomElement) : THTML_thead;
    function tagthead (subs : TDOMNodelist) : THTML_thead;
    function tablehead (s : string) : THTML_thead;
    function tablehead (sub : THTMLCustomElement) : THTML_thead;
    function tablehead (subs : Array of THTMLCustomElement) : THTML_thead;
    function tablehead (subs : TDOMNodelist) : THTML_thead;
    function Startthead : THTML_thead;
    function Endthead : THTML_thead;
    function Starttablehead : THTML_thead;
    function Endtablehead : THTML_thead;

    // tag "title", alias "title"
    function tagtitle (s : string) : THTML_title;
    function tagtitle (sub : THTMLCustomElement) : THTML_title;
    function tagtitle (subs : Array of THTMLCustomElement) : THTML_title;
    function tagtitle (subs : TDOMNodelist) : THTML_title;
    function title (s : string) : THTML_title;
    function title (sub : THTMLCustomElement) : THTML_title;
    function title (subs : Array of THTMLCustomElement) : THTML_title;
    function title (subs : TDOMNodelist) : THTML_title;
    function Starttitle : THTML_title;
    function Endtitle : THTML_title;

    // tag "tr", alias "tablerow"
    function tagtr (s : string) : THTML_tr;
    function tagtr (sub : THTMLCustomElement) : THTML_tr;
    function tagtr (subs : Array of THTMLCustomElement) : THTML_tr;
    function tagtr (subs : TDOMNodelist) : THTML_tr;
    function tablerow (s : string) : THTML_tr;
    function tablerow (sub : THTMLCustomElement) : THTML_tr;
    function tablerow (subs : Array of THTMLCustomElement) : THTML_tr;
    function tablerow (subs : TDOMNodelist) : THTML_tr;
    function Starttr : THTML_tr;
    function Endtr : THTML_tr;
    function Starttablerow : THTML_tr;
    function Endtablerow : THTML_tr;

    // tag "tt", alias "teletype"
    function tagtt (s : string) : THTML_tt;
    function tagtt (sub : THTMLCustomElement) : THTML_tt;
    function tagtt (subs : Array of THTMLCustomElement) : THTML_tt;
    function tagtt (subs : TDOMNodelist) : THTML_tt;
    function teletype (s : string) : THTML_tt;
    function teletype (sub : THTMLCustomElement) : THTML_tt;
    function teletype (subs : Array of THTMLCustomElement) : THTML_tt;
    function teletype (subs : TDOMNodelist) : THTML_tt;
    function Starttt : THTML_tt;
    function Endtt : THTML_tt;
    function Startteletype : THTML_tt;
    function Endteletype : THTML_tt;

    // tag "u", alias "underlined"
    function tagu (s : string) : THTML_u;
    function tagu (sub : THTMLCustomElement) : THTML_u;
    function tagu (subs : Array of THTMLCustomElement) : THTML_u;
    function tagu (subs : TDOMNodelist) : THTML_u;
    function underlined (s : string) : THTML_u;
    function underlined (sub : THTMLCustomElement) : THTML_u;
    function underlined (subs : Array of THTMLCustomElement) : THTML_u;
    function underlined (subs : TDOMNodelist) : THTML_u;
    function Startu : THTML_u;
    function Endu : THTML_u;
    function Startunderlined : THTML_u;
    function Endunderlined : THTML_u;

    // tag "ul", alias "unorederedlist"
    function tagul (s : string) : THTML_ul;
    function tagul (sub : THTMLCustomElement) : THTML_ul;
    function tagul (subs : Array of THTMLCustomElement) : THTML_ul;
    function tagul (subs : TDOMNodelist) : THTML_ul;
    function unorederedlist (s : string) : THTML_ul;
    function unorederedlist (sub : THTMLCustomElement) : THTML_ul;
    function unorederedlist (subs : Array of THTMLCustomElement) : THTML_ul;
    function unorederedlist (subs : TDOMNodelist) : THTML_ul;
    function Startul : THTML_ul;
    function Endul : THTML_ul;
    function Startunorederedlist : THTML_ul;
    function Endunorederedlist : THTML_ul;

    // tag "var", alias "variable"
    function tagvar (s : string) : THTML_var;
    function tagvar (sub : THTMLCustomElement) : THTML_var;
    function tagvar (subs : Array of THTMLCustomElement) : THTML_var;
    function tagvar (subs : TDOMNodelist) : THTML_var;
    function variable (s : string) : THTML_var;
    function variable (sub : THTMLCustomElement) : THTML_var;
    function variable (subs : Array of THTMLCustomElement) : THTML_var;
    function variable (subs : TDOMNodelist) : THTML_var;
    function Startvar : THTML_var;
    function Endvar : THTML_var;
    function Startvariable : THTML_var;
    function Endvariable : THTML_var;
