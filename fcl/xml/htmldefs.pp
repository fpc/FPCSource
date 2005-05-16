{
    $Id: htmldefs.pp,v 1.4 2005/02/14 17:13:18 peter Exp $
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

  THTMLElementFlags = set of (
    efSubelementContent,                // may have subelements
    efPCDATAContent,                    // may have PCDATA content
    efPreserveWhitespace);              // preserve all whitespace

  PHTMLElementProps = ^THTMLElementProps;
  THTMLElementProps = record
    Name: String;
    Flags: THTMLElementFlags;
  end;


const

  HTMLElProps: array[0..78] of THTMLElementProps = (
    (Name: 'a';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'abbr';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'acronym';   Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'address';   Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'applet';    Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'b';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'basefont';  Flags: []),
    (Name: 'bdo';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'big';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'blockquote';Flags: [efSubelementContent]),
    (Name: 'body';      Flags: [efSubelementContent]),
    (Name: 'br';        Flags: []),
    (Name: 'button';    Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'caption';   Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'center';    Flags: [efSubelementContent]),
    (Name: 'cite';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'code';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'col';       Flags: []),
    (Name: 'colgroup';  Flags: [efSubelementContent]),
    (Name: 'del';       Flags: [efSubelementContent]),
    (Name: 'dfn';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'dir';       Flags: [efSubelementContent]),
    (Name: 'div';       Flags: [efSubelementContent]),
    (Name: 'dl';        Flags: [efSubelementContent]),
    (Name: 'em';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'fieldset';  Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'font';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'form';      Flags: [efSubelementContent]),
    (Name: 'h1';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'h2';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'h3';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'h4';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'h5';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'h6';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'head';      Flags: [efSubelementContent]),
    (Name: 'hr';        Flags: []),
    (Name: 'html';      Flags: [efSubelementContent]),
    (Name: 'i';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'iframe';    Flags: [efSubelementContent]),
    (Name: 'img';       Flags: []),
    (Name: 'input';     Flags: []),
    (Name: 'ins';       Flags: [efSubelementContent]),
    (Name: 'isindex';   Flags: []),
    (Name: 'kbd';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'label';     Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'link';      Flags: []),
    (Name: 'map';       Flags: [efSubelementContent]),
    (Name: 'menu';      Flags: [efSubelementContent]),
    (Name: 'meta';      Flags: []),
    (Name: 'noframes';  Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'noscript';  Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'object';    Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'ol';        Flags: [efSubelementContent]),
    (Name: 'p';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'pre';       Flags: [efSubelementContent, efPCDATAContent, efPreserveWhitespace]),
    (Name: 'q';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 's';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'samp';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'script';    Flags: [efPCDATAContent]),
    (Name: 'select';    Flags: [efSubelementContent]),
    (Name: 'small';     Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'span';      Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'strike';    Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'strong';    Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'style';     Flags: [efPCDATAContent]),
    (Name: 'sub';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'sup';       Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'table';     Flags: [efSubelementContent]),
    (Name: 'textarea';  Flags: [efPCDATAContent]),
    (Name: 'tbody';     Flags: [efSubelementContent]),
    (Name: 'td';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'tfoot';     Flags: [efSubelementContent]),
    (Name: 'th';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'thead';     Flags: [efSubelementContent]),
    (Name: 'tr';        Flags: [efSubelementContent]),
    (Name: 'tt';        Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'u';         Flags: [efSubelementContent, efPCDATAContent]),
    (Name: 'ul';        Flags: [efSubelementContent]),
    (Name: 'var';       Flags: [efSubelementContent, efPCDATAContent]));


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


{
  $Log: htmldefs.pp,v $
  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
