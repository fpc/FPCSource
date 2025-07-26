
(*
 * macros from xmlversion.inc
 *)

procedure LIBXML_TEST_VERSION;
begin
  xmlCheckVersion(LIBXML_VERSION);
end;


(*
 * macros from xmlversion.inc
 *)


(*
 * macros from chvalid.inc
 *)

function xmlIsBaseChar_ch(c: cint): cbool;
begin
  Result :=
    ((c >= $41) and (c <= $5A)) or
    ((c >= $61) and (c <= $7A)) or
    ((c >= $C0) and (c <= $D6)) or
    ((c >= $D8) and (c <= $F6)) or
     (c >= $F8);
end;

function xmlIsBaseCharQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsBaseChar_ch(c)
  else
    Result := xmlCharInRange(c, __xmlIsBaseCharGroup);
end;

function xmlIsBlank_ch(c: cint): cbool;
begin
  Result := (c = $20) or ((c >= $9) and (c <= $A)) or (c = $D);
end;

function xmlIsBlankQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsBaseChar_ch(c)
  else
    Result := false;
end;

function xmlIsChar_ch(c: cint): cbool;
begin
  Result := ((c >= $9) and (c <= $A)) or (c = $D) or (c >= $20);
end;

function xmlIsCharQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsChar_ch(c)
  else
    Result :=
          ((c >= $000100) and (c <= $00D7FF)) or
          ((c >= $00E000) and (c <= $00FFFD)) or
          ((c >= $010000) and (c <= $10FFFF));
end;

function xmlIsCombiningQ(c: cint): cbool;
begin
    if c < $100 then
    Result := false
  else
    Result := xmlCharInRange(c, __xmlIsCombiningGroup);
end;

function xmlIsDigit_ch(c: cint): cbool;
begin
  Result := (c >= $30) and (c <= $39);
end;

function xmlIsDigitQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsDigit_ch(c)
  else
    Result := xmlCharInRange(c, __xmlIsDigitGroup);
end;

function xmlIsExtender_ch(c: cint): cbool;
begin
  Result := c = $B7;
end;

function xmlIsExtenderQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsExtender_ch(c)
  else
    Result := xmlCharInRange(c, __xmlIsExtenderGroup);
end;

function xmlIsIdeographicQ(c: cint): cbool;
begin
  if c < $100 then
    Result := false
  else
    Result :=
      ((c >= $4E00) and (c <= $9FA5)) or
       (c  = $3007) or
      ((c >= $3021) and (c <= $3029));
end;

function xmlIsPubidChar_ch(c: cint): cbool;
begin
  if (c >= 0) and (c <= 255) then
    Result := __xmlIsPubidChar_tab^[c]
  else
    Result := false;
end;

function xmlIsPubidCharQ(c: cint): cbool;
begin
  if c < $100 then
    Result := xmlIsPubidChar_ch(c)
  else
    Result := false;
end;


(*
 * macros from HTMLparser.inc
 *)

function htmlDefaultSubelement(elt: htmlElemDescPtr): PAnsiChar;
begin
  Result := elt^.defaultsubelt;
end;

function htmlElementAllowedHereDesc(parent: htmlElemDescPtr; elt: htmlElemDescPtr): cint;
begin
  Result := htmlElementAllowedHere(parent, xmlCharPtr(elt^.name));
end;

function htmlRequiredAttrs(elt: htmlElemDescPtr): PPAnsiChar;
begin
  Result := elt^.attrs_req;
end;


(*
 * macros from tree.inc
 *)

function XML_GET_CONTENT(n: pointer): xmlCharPtr;
begin
  if xmlNodePtr(n)^._type = XML_ELEMENT_NODE then
    Result := nil
  else
    Result := xmlNodePtr(n)^.content;
end;

function XML_GET_LINE(n: Pointer): clong; inline;
begin
  Result := xmlGetLineNo(xmlNodePtr(n));
end;

(*
 * macros from xpath.inc
 *)

function xmlXPathNodeSetGetLength(ns: xmlNodeSetPtr): cint;
begin
  if assigned(ns) then
    Result := ns^.nodeNr
  else
    Result := 0;
end;

function xmlXPathNodeSetItem(ns: xmlNodeSetPtr; index: cint): xmlNodePtr;
begin
  if assigned(ns) and (index >= 0) and (index < ns^.nodeNr) then
    Result := ns^.nodeTab[index]
  else
    Result := nil;
end;

function xmlXPathNodeSetIsEmpty(ns: xmlNodeSetPtr): boolean;
begin
  Result := not assigned(ns) or (ns^.nodeNr = 0) or (ns^.nodeTab = nil);
end;

