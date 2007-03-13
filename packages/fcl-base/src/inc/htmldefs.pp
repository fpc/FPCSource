{
    $Id: htmldefs.pp,v 1.2 2006/01/03 23:33:23 lukvdl Exp $
    This file is part of the Free Component Library

    HTML definitions and utility functions
    Copyright (c) 2000-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit HTMLDefs;

{$MODE objfpc}
{$H+}

interface

type

  THTMLCData = string;
  THTMLID = string;
  THTMLName = string;
  THTMLIDRef = string;
  THTMLIDRefs = string;
  THTMLNumber = longint;

  THTMLText = THTMLCData;
  THTMLCharsets = THTMLCData;
  THTMLContentTypes = THTMLCData;
  THTMLURI = string;
  THTMLCharacter = char;

  THTMLDir = (dirEmpty,dirLeftToRight,dirRightToLeft);
  THTMLalign = (alEmpty,alleft,alcenter,alright,aljustify,alchar);
  THTMLvalign = (vaEmpty,vatop,vamiddle,vabottom,vabaseline);
  THTMLframe = (frEmpty,frvoid,frabove,frbelow,frhsides,frvsides,frlefthandsise,frrighthandside,frbox,frborder);
  THTMLrules = (ruEmpty,runone,rugroups,rurows,rucols,ruall);
  THTMLvaluetype = (vtEmpty,vtdata,vtref,vtobject);
  THTMLshape = (shEmpty,shdefault,shrect,shcircle,shpoly);
  THTMLinputtype = (itEmpty,ittext,itpassword,itcheckbox,itradio,itsubmit,itreset,itfile,ithidden,itimage,itbutton);
  THTMLbuttontype = (btEmpty,btsubmit,btreset,btbutton);

  THTMLColor = (
      clHTMLBlack, clHTMLSilver, clHTMLGray,   clHTMLWhite, clHTMLMaroon,
    //  #000000      #C0C0C0       #808080       #FFFFFF      #800000
      clHTMLRed,   clHTMLPurple, clHTMLFuchsia,clHTMLGreen, clHTMLLime, clHTMLOlive,
    //  #FF0000      #800080       #FF00FF       #008000      #00FF00     #808000
      clHTMLYellow,clHTMLNavy,   clHTMLBlue,   clHTMLTeal,  clHTMLAqua
    //  #FFFF00      #000080       #0000FF       #008080      #00FFFF
      );

  THTMLAttributeTag = (
      atabbr, atacceptcharset, ataccept, ataccesskey, ataction, atalign, atalt, atarchive,
      ataxis, atborder, atcellpadding, atcellspacing, atchar, atcharoff, atcharset,
      atchecked, atcite, atclass, atclassid, atcodebase, atcodetype, atcols,
      atcolspan, atcontent, atcoords, atdata, atdatetime, atdeclare,atdefer,
      atdir, atdisabled, atenctype, atfor, atframe, atframeborder, atheaders,
      atheight, athref, athreflang, athttpequiv, atid, atismap, atlabel, atlang,
      atlongdesc, atmarginheight, atmarginwidth, atmaxlength, atmedia, atmethod,
      atmultiple, atname, atnohref, atnoresize, atonblur, atonchange, atonclick,
      atondblclick, atonfocus, atonkeydown, atonkeypress, atonkeyup, atonload,
      atonmousedown, atonmousemove, atonmouseout, atonmouseover, atonmouseup,
      atonreset, atonselect, atonsubmit, atonunload, atprofile, atreadonly,
      atrel, atrev, atrows, atrowspan, atrules, atscheme, atscope, atscrolling,
      atselected, atshape, atsize, atspan, atsrc, atstandby, atstyle, atsummary,
      attabindex, attarget, attitle, attype, atusemap, atvalign, atvalue,
      atvaluetype, atwidth
      );
  THTMLAttributeSet = set of THTMLAttributeTag;

  THTMLElementTag = (
      eta, etabbr, etacronym, etaddress, etapplet, etarea, etb, etbase,
      etbasefont, etbdo, etbig, etblockquote, etbody, etbr, etbutton,
      etcaption, etcenter, etcite, etcode, etcol, etcolgroup, etdd, etdel,
      etdfn, etdir, etdiv, etdl, etdt, etem, etfieldset, etfont, etform,
      etframe, etframeset, eth1, eth2, eth3, eth4, eth5, eth6, ethead, ethr,
      ethtml, eti, etiframe, etimg, etinput, etins, etisindex, etkbd, etlabel,
      etlegend, etli, etlink, etmap, etmenu, etmeta, etnoframes, etnoscript,
      etobject, etol, etoptgroup, etoption, etp, etparam, etpre, etq, ets,
      etsamp, etscript, etselect, etsmall, etspan, etstrike, etstrong,
      etstyle, etsub, etsup, ettable, ettbody, ettd, ettextarea, ettfoot,
      etth, etthead, ettitle, ettr, ettt, etu, etul, etvar,
      etText, etUnknown
      );
  THTMLElementTagSet = set of THTMLElementTag;

  THTMLElementFlag = (
    efSubelementContent,                // may have subelements
    efPCDATAContent,                    // may have PCDATA content
    efPreserveWhitespace,               // preserve all whitespace
    efDeprecated,                       // can be dropped in future versions
    efNoChecks                          // Checks (attributes,subtags,...) can only be implemented in descendants
    );
  THTMLElementFlags = set of THTMLElementFlag;

  PHTMLElementProps = ^THTMLElementProps;
  THTMLElementProps = record
    Name: String;
    Flags: THTMLElementFlags;
    Attributes: THTMLAttributeSet;
  end;


const

  BooleanAttributes = [atchecked,atdeclare,atdefer,atdisabled,atnohref,atnoresize,
                    atmultiple,atreadonly,atselected];

  efSubcontent = [efSubelementContent, efPCDATAContent];

  atsi18n = [atlang, atdir];
  atscoreattrs = [atid,atclass,atstyle,attitle];
  atsevents = [atonclick,atondblclick,atonmousedown,atonmouseup,atonmouseover,
               atonmousemove,atonmouseout,atonkeypress,atonkeydown,atonkeyup];
  atsattrs = atsevents + atscoreattrs + atsi18n;
  atscellhalign = [atalign, atchar, atcharoff];

{  etsStructured := [];
  etsDivisions := [];
  etsLists := [];
  etsLinks := [];
  etsObjects := [etImg, etObject, etApplet, etMap, etArea];
  etsForms := [etForm];

  etsText = etsStructured + etsDivisions + etsLists + etsLinks + etsObjects +
          etsForms +
          etTable + etText + etScript + ;      }

  HTMLElementProps: array[THTMLElementTag] of THTMLElementProps = (
    (Name: 'a';         Flags: efSubcontent;
     Attributes: atsattrs+[atcharset,attype,atname,athref,athreflang,atrel,atrev,
                 ataccesskey,atshape,atcoords,attabindex,atonfocus,atonblur]),

    (Name: 'abbr';      Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'acronym';   Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'address';   Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'applet';    Flags: efSubcontent+[efDeprecated];
     Attributes: atscoreattrs+[atcodebase,atarchive,atalt,atname,atwidth,atheight]),

    (Name: 'area';      Flags: [];
     Attributes: atsattrs+[atshape,atcoords,athref,atnohref,atalt,attabindex,
     ataccesskey,atonfocus,atonblur]),

    (Name: 'b';         Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'base';      Flags: []; Attributes: [athref]),

    (Name: 'basefont';  Flags: [efDeprecated]; Attributes: [atid]),

    (Name: 'bdo';       Flags: efSubcontent; Attributes: atscoreattrs+[atlang,atdir]),

    (Name: 'big';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'blockquote';Flags: [efSubelementContent]; Attributes: atsattrs+[atcite]),

    (Name: 'body';      Flags: [efSubelementContent];
     Attributes: atsAttrs+[atonload, atonunload]),

    (Name: 'br';        Flags: []; Attributes: atscoreattrs),

    (Name: 'button';    Flags: efSubcontent;
     Attributes: atsattrs+[atname,atvalue,attype,atdisabled,attabindex,
                 ataccesskey,atonfocus,atonblur]),

    (Name: 'caption';   Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'center';    Flags: [efSubelementContent,efDeprecated]; Attributes: []),

    (Name: 'cite';      Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'code';      Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'col';       Flags: [];
     Attributes: atsattrs+atscellhalign+[atvalign,atspan,atwidth]),

    (Name: 'colgroup';  Flags: [efSubelementContent];
     Attributes: atsattrs+atscellhalign+[atvalign,atspan,atwidth]),

    (Name: 'dd';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'del';       Flags: [efSubelementContent]; Attributes: atsattrs+[atcite,atdatetime]),

    (Name: 'dfn';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'dir';       Flags: [efSubelementContent,efDeprecated]; Attributes: atsattrs),

    (Name: 'div';       Flags: [efSubelementContent]; Attributes: atsattrs),

    (Name: 'dl';        Flags: [efSubelementContent]; Attributes: atsattrs),

    (Name: 'dt';        Flags: [efPCDataContent]; Attributes: atsattrs),

    (Name: 'em';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'fieldset';  Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'font';      Flags: efSubcontent+[efDeprecated]; Attributes: atscoreattrs+atsi18n),

    (Name: 'form';      Flags: [efSubelementContent];
     Attributes: atsattrs+[ataction,atmethod,atenctype,atonsubmit,atonreset,atacceptcharset]),

    (Name: 'frame';     Flags: [];
     Attributes: atscoreattrs+[atlongdesc,atname,atsrc,atframeborder,
                 atmarginwidth,atmarginheight,atnoresize,atscrolling]),

    (Name: 'frameset';  Flags: efSubcontent;
     Attributes: atsCoreattrs+[atrows,atcols,atonload,atonunload]),

    (Name: 'h1';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'h2';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'h3';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'h4';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'h5';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'h6';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'head';      Flags: [efSubelementContent]; Attributes: atsi18n+[atprofile]),

    (Name: 'hr';        Flags: []; Attributes: atscoreattrs+atsevents),

    (Name: 'html';      Flags: [efSubelementContent]; Attributes: atsi18n),

    (Name: 'i';         Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'iframe';    Flags: [efSubelementContent];
     Attributes: atscoreattrs+[atlongdesc,atname,atsrc,atframeborder,atmarginwidth,
                 atmarginheight,atscrolling,atalign,atheight,atwidth]),

    (Name: 'img';       Flags: [];
     Attributes: atsattrs+[atsrc,atalt,atlongdesc,atheight,atwidth,atusemap,atismap]),

    (Name: 'input';     Flags: [];
     Attributes: atsattrs+[attype,atname,atvalue,atchecked,atdisabled,
                 atreadonly,atsize,atmaxlength,atsrc,atalt,atusemap,attabindex,
                 ataccesskey,atonfocus,atonblur,atonselect,atonchange,ataccept]),

    (Name: 'ins';       Flags: [efSubelementContent]; Attributes: atsattrs+[atcite,atdatetime]),

    (Name: 'isindex';   Flags: [efDeprecated]; Attributes: atscoreattrs+atsi18n),

    (Name: 'kbd';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'label';     Flags: efSubcontent;
     Attributes: atsattrs+[atfor,ataccesskey,atonfocus,atonblur]),

    (Name: 'legend';    Flags: efSubcontent; Attributes: atsattrs+[ataccesskey]),

    (Name: 'li';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'link';      Flags: [];
     Attributes: atsattrs+[atcharset,athref,athreflang,attype,atrel,atrev,atmedia]),

    (Name: 'map';       Flags: [efSubelementContent]; Attributes: atsattrs+[atname]),

    (Name: 'menu';      Flags: [efSubelementContent,efDeprecated]; Attributes: atsattrs),

    (Name: 'meta';      Flags: []; Attributes: atsi18n+[athttpequiv,atname,atcontent,atscheme]),

    (Name: 'noframes';  Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'noscript';  Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'object';    Flags: efSubcontent;
     Attributes: atsattrs+[atdeclare,atclassid,atcodebase,atdata,attype,atcodetype,
                 atarchive,atstandby,atheight,atwidth,atusemap,atname,attabindex]),

    (Name: 'ol';        Flags: [efSubelementContent]; Attributes: atsattrs),

    (Name: 'optgroup';  Flags: efSubcontent; Attributes: atsattrs+[atdisabled,atlabel]),

    (Name: 'option';    Flags: efSubcontent;
     Attributes: atsattrs+[atselected,atdisabled,atlabel,atvalue]),

    (Name: 'p';         Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'param';     Flags: []; Attributes: [atid,atname,atvalue,atvaluetype,attype]),

    (Name: 'pre';       Flags: efSubcontent + [efPreserveWhitespace]; Attributes: atsattrs),

    (Name: 'q';         Flags: efSubcontent; Attributes: atsattrs+[atcite]),

    (Name: 's';         Flags: efSubcontent+[efDeprecated]; Attributes: atsattrs),

    (Name: 'samp';      Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'script';    Flags: [efPCDATAContent]; Attributes: [atcharset,attype,atsrc,atdefer]),

    (Name: 'select';    Flags: [efSubelementContent];
     Attributes: atsattrs+[atname,atsize,atmultiple,atdisabled,attabindex,atonfocus,
                 atonblur,atonchange]),

    (Name: 'small';     Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'span';      Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'strike';    Flags: efSubcontent+[efDeprecated]; Attributes: atsattrs),

    (Name: 'strong';    Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'style';     Flags: [efPCDATAContent];
     Attributes: atsi18n+[attype,atmedia,attitle]),

    (Name: 'sub';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'sup';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'table';     Flags: [efSubelementContent];
     Attributes: atsattrs+[atsummary,atwidth,atborder,atframe,atrules,atcellspacing,atcellpadding]),

    (Name: 'tbody';     Flags: [efSubelementContent]; Attributes: atsattrs+atscellhalign+[atvalign]),

    (Name: 'td';        Flags: efSubcontent;
     Attributes: atsattrs+atscellhalign+[atvalign,atabbr,ataxis,atheaders,atscope,atrowspan,atcolspan]),

    (Name: 'textarea';  Flags: [efPCDATAContent];
     Attributes: atsattrs+[atname,atrows,atcols,atdisabled,atreadonly,attabindex,
                 ataccesskey,atonfocus,atonblur,atonselect,atonchange]),

    (Name: 'tfoot';     Flags: [efSubelementContent]; Attributes: atsattrs+atscellhalign+[atvalign]),

    (Name: 'th';        Flags: efSubcontent;
     Attributes: atsattrs+atscellhalign+[atvalign,atabbr,ataxis,atheaders,atscope,atrowspan,atcolspan]),

    (Name: 'thead';     Flags: [efSubelementContent]; Attributes: atsattrs+atscellhalign+[atvalign]),

    (Name: 'title';     Flags: efSubcontent; Attributes: atsi18n),

    (Name: 'tr';        Flags: [efSubelementContent];
     Attributes: atsattrs+atscellhalign+[atvalign]),

    (Name: 'tt';        Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'u';         Flags: efSubcontent+[efDeprecated]; Attributes: atsattrs),

    (Name: 'ul';        Flags: [efSubelementContent]; Attributes: atsattrs),

    (Name: 'var';       Flags: efSubcontent; Attributes: atsattrs),

    (Name: 'text';      Flags: efSubcontent; Attributes: []),

    (Name: 'unknown';   Flags: efSubcontent+[efNoChecks]; Attributes: [])

    );

  HTMLAttributeTag : array [THTMLAttributeTag] of string = (
      'abbr', 'accept-charset', 'accept', 'accesskey', 'action', 'align', 'alt', 'archive',
      'axis', 'border', 'cellpadding', 'cellspacing', 'char', 'charoff', 'charset',
      'checked', 'cite', 'class', 'classid', 'codebase', 'codetype', 'cols',
      'colspan', 'content', 'coords', 'data', 'datetime', 'declare', 'defer',
      'dir', 'disabled', 'enctype', 'for', 'frame', 'frameborder', 'headers',
      'height', 'href', 'hreflang', 'http-equiv', 'id', 'ismap', 'label', 'lang',
      'longdesc', 'marginheight', 'marginwidth', 'maxlength', 'media', 'method',
      'multiple', 'name', 'nohref', 'noresize', 'onblur', 'onchange', 'onclick',
      'ondblclick', 'onfocus', 'onkeydown', 'onkeypress', 'onkeyup', 'onload',
      'onmousedown', 'onmousemove', 'onmouseout', 'onmouseover', 'onmouseup',
      'onreset', 'onselect', 'onsubmit', 'onunload', 'profile', 'readonly',
      'rel', 'rev', 'rows', 'rowspan', 'rules', 'scheme', 'scope', 'scrolling',
      'selected', 'shape', 'size', 'span', 'src', 'standby', 'style', 'summary',
      'tabindex', 'target', 'title', 'type', 'usemap', 'valign', 'value',
      'valuetype', 'width');

  HTMLColor : array [THTMLColor] of string =
    ('Black', 'Silver', 'Gray', 'White', 'Maroon', 'Red', 'Purple', 'Fuchsia',
     'Green', 'Lime', 'Olive', 'Yellow', 'Navy', 'Blue', 'Teal', 'Aqua');
  HTMLDir : array [THTMLDir] of string = ('','LTR','RTL');
  HTMLAlign : array [THTMLalign] of string = ('','left','center','right','justify','char');
  HTMLvalign : array [THTMLvalign] of string = ('','top','middle','bottom','baseline');
  HTMLframe : array [THTMLframe] of string =
      ('','void','above','below','hsides','vsides','lhs','rhs','box','border');
  HTMLrules : array [THTMLrules] of string = ('','none','groups','rows','cols','all');
  HTMLvaluetype : array [THTMLvaluetype] of string = ('','data','ref','object');
  HTMLshape : array [THTMLshape] of string = ('','default','rect','circle','poly');
  HTMLinputtype : array [THTMLinputtype] of string = ('','text','password','checkbox',
      'radio','submit','reset','file','hidden','image','button');
  HTMLbuttontype : array [THTMLbuttontype] of string = ('','submit','reset','button');


  // ISO8859-1 mapping:
  HTMLEntities: array[#160..#255] of String = (
    // 160-191
    'nbsp', 'iexcl', 'cent', 'pound', 'curren', 'yen', 'brvbar', 'sect',
    'uml', 'copy', 'ordf', 'laquo', 'not', 'shy', 'reg', 'macr',
    'deg', 'plusmn', 'sup2', 'sup3', 'acute', 'micro', 'para', 'middot',
    'cedil', 'sup1', 'ordm', 'raquo', 'frac14', 'frac12', 'frac34', 'iquest',
    // 192-223
    'Agrave', 'Aacute', 'Acirc', 'Atilde', 'Auml', 'Aring', 'AElig', 'Ccedil',
    'Egrave', 'Eacute', 'Ecirc', 'Euml', 'Igrave', 'Iacute', 'Icirc', 'Iuml',
    'ETH', 'Ntilde', 'Ograve', 'Oacute', 'Ocirc', 'Otilde', 'Ouml', 'times',
    'Oslash', 'Ugrave', 'Uacute', 'Ucirc', 'Uuml', 'Yacute', 'THORN', 'szlig',
    // 224-255
    'agrave', 'aacute', 'acirc', 'atilde', 'auml', 'aring', 'aelig', 'ccedil',
    'egrave', 'eacute', 'ecirc', 'euml', 'igrave', 'iacute', 'icirc', 'iuml',
    'eth', 'ntilde', 'ograve', 'oacute', 'ocirc', 'otilde', 'ouml', 'divide',
    'oslash', 'ugrave', 'uacute', 'ucirc', 'uuml', 'yacute', 'thorn', 'yuml');


  UnicodeHTMLEntities: array[0..141] of String = (
    'Alpha',    // #913
    'Beta',     // #914
    'Gamma',    // #915
    'Delta',    // #916
    'Epsilon',  // #917
    'Zeta',     // #918
    'Eta',      // #919
    'Theta',    // #920
    'Iota',     // #921
    'Kappa',    // #922
    'Lambda',   // #923
    'Mu',       // #924
    'Nu',       // #925
    'Xi',       // #926
    'Omicron',  // #927
    'Pi',       // #928
    'Rho',      // #929
    'Sigma',    // #931
    'Tau',      // #932
    'Upsilon',  // #933
    'Phi',      // #934
    'Chi',      // #935
    'Psi',      // #936
    'Omega',    // #937
    'alpha',    // #945
    'beta',     // #946
    'gamma',    // #947
    'delta',    // #948
    'epsilon',  // #949
    'zeta',     // #950
    'eta',      // #951
    'theta',    // #952
    'iota',     // #953
    'kappa',    // #954
    'lambda',   // #955
    'mu',       // #956
    'nu',       // #957
    'xi',       // #958
    'omicron',  // #959
    'pi',       // #960
    'rho',      // #961
    'sigmaf',   // #962
    'sigma',    // #963
    'tau',      // #964
    'upsilon',  // #965
    'phi',      // #966
    'chi',      // #967
    'psi',      // #968
    'omega',    // #969
    'thetasym', // #977
    'upsih',    // #978
    'piv',      // #982
    'ensp',     // #8194
    'emsp',     // #8195
    'thinsp',   // #8201
    'zwnj',     // #8204
    'zwj',      // #8205
    'lrm',      // #8206
    'rlm',      // #8207
    'ndash',    // #8211
    'mdash',    // #8212
    'lsquo',    // #8216
    'rsquo',    // #8217
    'sbquo',    // #8218
    'ldquo',    // #8220
    'rdquo',    // #8221
    'bdquo',    // #8222
    'dagger',   // #8224
    'Dagger',   // #8225
    'bull',     // #8226
    'hellip',   // #8230
    'permil',   // #8240
    'prime',    // #8242
    'lsaquo',   // #8249
    'rsaquo',   // #8250
    'oline',    // #8254
    'frasl',    // #8260
    'image',    // #8465
    'weierp',   // #8472
    'real',     // #8476
    'trade',    // #8482
    'alefsym',  // #8501
    'larr',     // #8592
    'uarr',     // #8593
    'rarr',     // #8594
    'darr',     // #8595
    'harr',     // #8596
    'crarr',    // #8629
    'lArr',     // #8656
    'uArr',     // #8657
    'rArr',     // #8658
    'dArr',     // #8659
    'hArr',     // #8660
    'forall',   // #8704
    'part',     // #8706
    'exist',    // #8707
    'empty',    // #8709
    'nabla',    // #8711
    'isin',     // #8712
    'notin',    // #8713
    'ni',       // #8715
    'prod',     // #8719
    'sum',      // #8721
    'minus',    // #8722
    'lowast',   // #8727
    'radic',    // #8730
    'prop',     // #8733
    'infin',    // #8734
    'ang',      // #8736
    'and',      // #8743
    'or',       // #8744
    'cap',      // #8745
    'cup',      // #8746
    'int',      // #8747
    'there4',   // #8756
    'sim',      // #8764
    'cong',     // #8773
    'asymp',    // #8776
    'ne',       // #8800
    'equiv',    // #8801
    'le',       // #8804
    'ge',       // #8805
    'sub',      // #8834
    'sup',      // #8835
    'nsub',     // #8836
    'sube',     // #8838
    'supe',     // #8839
    'oplus',    // #8853
    'otimes',   // #8855
    'perp',     // #8869
    'sdot',     // #8901
    'lceil',    // #8968
    'rceil',    // #8969
    'lfloor',   // #8970
    'rfloor',   // #8971
    'lang',     // #9001
    'rang',     // #9002
    'loz',      // #9674
    'spades',   // #9824
    'clubs',    // #9827
    'hearts',   // #9829
    'diams'     // #9830
  );



function ResolveHTMLEntityReference(const Name: String;
  var Entity: Char): Boolean;



implementation

uses SysUtils;

function ResolveHTMLEntityReference(const Name: String;
  var Entity: Char): Boolean;
var
  Ent: Char;
  i: Integer;
begin
  if Name = 'quot' then
  begin
    Entity := '"';
    Result := True;
  end else if Name = 'apos' then
  begin
    Entity := '''';
    Result := True;
  end else if Name = 'amp' then
  begin
    Entity := '&';
    Result := True;
  end else if Name = 'lt' then
  begin
    Entity := '<';
    Result := True;
  end else if Name = 'gt' then
  begin
    Entity := '>';
    Result := True;
  end else if (Length(Name) > 0) and (Name[1] = '#') then
  begin
    for i := 2 to Length(Name) do
      if (Name[i] < '0') or (Name[i] > '9') then
        break;
    if i > 2 then
    begin
      Entity := Chr(StrToInt(Copy(Name, 2, i - 1)));
      Result := True;
    end else
      Result := False;
  end else
  begin
    for Ent := Low(HTMLEntities) to High(HTMLEntities) do
      if HTMLEntities[Ent] = Name then
      begin
        Entity := Ent;
        Result := True;
        exit;
      end;
    Result := False;
  end;
end;

end.
