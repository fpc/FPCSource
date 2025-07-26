(**
 * section: 	XPath
 * synopsis: 	Evaluate XPath expression and prints result node set.
 * purpose: 	Shows how to evaluate XPath expression and register
 *          	known namespaces in XPath context.
 * usage:	xpath1 <xml-file> <xpath-expr> [<known-ns-list>]
 * test:	xpath1 test3.xml '//child2' > xpath1.tmp && diff xpath1.tmp $(srcdir)/xpath1.res
 * author: 	Aleksey Sanin
 * copy: 	see Copyright for the status of this software.
 *)

program xpath1;

{$mode objfpc}

uses
  ctypes,
  xml2,
  exutils,
  SysUtils;

(**
 * usage:
 * @name:		the program name.
 *
 * Prints usage information.
 *)
procedure usage(const name: String);
begin
  //assert(name <> '');

  printfn('Usage: %s <xml-file> <xpath-expr> [<known-ns-list>]', [name]);
  printfn('where <known-ns-list> is a list of known namespaces');
  printfn('in "<prefix1>=<href1> <prefix2>=href2> ..." format');
end;

(**
 * print_xpath_nodes:
 * @nodes:		the nodes set.
 * @output:		the output file handle.
 *
 * Prints the @nodes content to @output.
 *)
procedure print_xpath_nodes(nodes: xmlNodeSetPtr; var output: TextFile);
var
  cur: xmlNodePtr;
  size: cint;
  i: cint;
  ns: xmlNsPtr;
begin
  //assert(output);
  if nodes <> Nil then
    size := nodes^.nodeNr
  else
    size := 0;

  WriteLn(output, Format('Result (%d nodes):', [size]));
  for i := 0 to size - 1 do
  begin
    assert(nodes^.nodeTab[i] <> Nil);

    if nodes^.nodeTab[i]^._type = XML_NAMESPACE_DECL then
    begin
      ns := xmlNsPtr(nodes^.nodeTab[i]);
      cur := xmlNodePtr(ns^.next);
      if cur^.ns <> nil then
        WriteLn(output, Format('= namespace "%s"="%s" for node %s:%s',
          [ns^.prefix, ns^.href, cur^.ns^.href, cur^.name]))
      else
        WriteLn(output, Format('= namespace "%s"="%s" for node %s',
          [ns^.prefix, ns^.href, cur^.name]));
    end
    else if nodes^.nodeTab[i]^._type = XML_ELEMENT_NODE then
    begin
      cur := nodes^.nodeTab[i];
      if cur^.ns <> Nil then
        WriteLn(output, Format('= element node "%s:%s"',
          [cur^.ns^.href, cur^.name]))
      else
    	WriteLn(output, Format('= element node "%s"', [cur^.name]));
    end
    else
    begin
      cur := nodes^.nodeTab[i];
      WriteLn(output, Format('= node "%s": type %d', [cur^.name, cur^._type]));
    end;
  end;
end;

(**
 * register_namespaces:
 * @xpathCtx:		the pointer to an XPath context.
 * @nsList:		the list of known namespaces in
 *			"<prefix1>=<href1> <prefix2>=href2> ..." format.
 *
 * Registers namespaces from @nsList in @xpathCtx.
 *
 * Returns 0 on success and a negative value otherwise.
 *)
function register_namespaces(xpathCtx: xmlXPathContextPtr; const nsList: xmlCharPtr): cint;
var
  nsListDup: xmlCharPtr;
  prefix: xmlCharPtr;
  href: xmlCharPtr;
  next: xmlCharPtr;
begin
  assert(xpathCtx <> Nil);
  assert(nsList <> Nil);

  nsListDup := xmlStrdup(nsList);
  if nsListDup = Nil then
  begin
    printfn('Error: unable to strdup namespaces list');
    Exit(-1);
  end;

  next := nsListDup;
  while next <> Nil do
  begin
    (* skip spaces *)
    while next^ = ' ' do
      Inc(next);
    if next^ = #0 then
      break;

    (* find prefix *)
    prefix := next;
    next := xmlCharPtr(xmlStrchr(next, '='));
    if next = Nil then
    begin
      printfn('Error: invalid namespaces list format');
      xmlFree(nsListDup);
      Exit(-1);
    end;
    next^ := #0;
    Inc(next);

    (* find href *)
    href := next;
    next := xmlCharPtr(xmlStrchr(next, ' '));
    if next <> Nil then
    begin
      next^ := #0;
      Inc(next);
    end;

    (* do register namespace *)
    if xmlXPathRegisterNs(xpathCtx, prefix, href) <> 0 then
    begin
      printfn('Error: unable to register NS with prefix="%s" and href="%s"', [prefix, href]);
      xmlFree(nsListDup);
      Exit(-1);
    end;
  end;

  xmlFree(nsListDup);
  Result := 0;
end;

(**
 * execute_xpath_expression:
 * @filename:		the input XML filename.
 * @xpathExpr:		the xpath expression for evaluation.
 * @nsList:		the optional list of known namespaces in
 *			"<prefix1>=<href1> <prefix2>=href2> ..." format.
 *
 * Parses input XML file, evaluates XPath expression and prints results.
 *
 * Returns 0 on success and a negative value otherwise.
 *)
function execute_xpath_expression(const filename: PAnsiChar; const xpathExpr, nsList: xmlCharPtr): cint;
var
  doc: xmlDocPtr;
  xpathCtx: xmlXPathContextPtr;
  xpathObj: xmlXPathObjectPtr;
begin

  assert(filename <> Nil);
  assert(xpathExpr <> Nil);

  (* Load XML document *)
  doc := xmlParseFile(filename);
  if doc = Nil then
  begin
    printfn('Error: unable to parse file "%s"', [filename]);
    Exit(-1);
  end;

  (* Create xpath evaluation context *)
  xpathCtx := xmlXPathNewContext(doc);
  if xpathCtx = Nil then
  begin
    printfn('Error: unable to create new XPath context');
    xmlFreeDoc(doc);
    Exit(-1);
  end;

  (* Register namespaces from list (if any) *)
  if (nsList <> Nil) and (register_namespaces(xpathCtx, nsList) < 0) then
  begin
    printfn('Error: failed to register namespaces list "%s"', [nsList]);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    Exit(-1);
  end;

  (* Evaluate xpath expression *)
  xpathObj := xmlXPathEvalExpression(xpathExpr, xpathCtx);
  if xpathObj = Nil then
  begin
    printfn('Error: unable to evaluate xpath expression "%s"', [xpathExpr]);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    Exit(-1);
  end;

  (* Print results *)
  print_xpath_nodes(xpathObj^.nodesetval, StdOut);

  (* Cleanup *)
  xmlXPathFreeObject(xpathObj);
  xmlXPathFreeContext(xpathCtx);
  xmlFreeDoc(doc);

  Result := 0;
end;

begin
  (* Parse command line and process file *)
  if (ParamCount < 2) or (ParamCount > 3) then
  begin
    printfn('Error: wrong number of arguments.');
    usage(ParamStr(0));
    Halt(-1);
  end;

  (* Init libxml *)
  xmlInitParser();
  LIBXML_TEST_VERSION;

  (* Do the main job *)
  if execute_xpath_expression(PAnsiChar(ParamStr(1)), PAnsiChar(ParamStr(2)),
    specialize IfThen<PAnsiChar>(argc > 2, PAnsiChar(ParamStr(3)), Nil)) < 0 then
  begin
    usage(ParamStr(0));
    Halt(-1);
  end;

  (* Shutdown libxml *)
  xmlCleanupParser();

  (*
   * this is to debug memory for regression tests
   *)
  xmlMemoryDump();
end.

