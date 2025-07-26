(**
 * section: 	XPath
 * synopsis: 	Load a document, locate subelements with XPath, modify
 *              said elements and save the resulting document.
 * purpose: 	Shows how to make a full round-trip from a load/edit/save
 * usage:	xpath2 <xml-file> <xpath-expr> <new-value>
 * test:	xpath2 test3.xml '//discarded' discarded > xpath2.tmp && diff xpath2.tmp $(srcdir)/xpath2.res
 * author: 	Aleksey Sanin and Daniel Veillard
 * copy: 	see Copyright for the status of this software.
 *)

program xpath2;

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
  //assert(name);

  printfn('Usage: %s <xml-file> <xpath-expr> <value>', [name]);
end;

(**
 * update_xpath_nodes:
 * @nodes:		the nodes set.
 * @value:		the new value for the node(s)
 *
 * Prints the @nodes content to @output.
 *)
procedure update_xpath_nodes(nodes: xmlNodeSetPtr; const value: xmlCharPtr);
var
  size: cint;
  i: cint;
begin
  assert(value <> Nil);
  if nodes <> nil then
    size := nodes^.nodeNr
  else
    size := 0;

  (*
   * NOTE: the nodes are processed in reverse order, i.e. reverse document
   *       order because xmlNodeSetContent can actually free up descendant
   *       of the node and such nodes may have been selected too ! Handling
   *       in reverse order ensure that descendant are accessed first, before
   *       they get removed. Mixing XPath and modifications on a tree must be
   *       done carefully !
   *)
  for i := size - 1 downto 0 do
  begin
    assert(nodes^.nodeTab[i] <> Nil);

    xmlNodeSetContent(nodes^.nodeTab[i], value);
    (*
     * All the elements returned by an XPath query are pointers to
     * elements from the tree *except* namespace nodes where the XPath
     * semantic is different from the implementation in libxml2 tree.
     * As a result when a returned node set is freed when
     * xmlXPathFreeObject() is called, that routine must check the
     * element type. But node from the returned set may have been removed
     * by xmlNodeSetContent() resulting in access to freed data.
     * This can be exercised by running
     *       valgrind xpath2 test3.xml '//discarded' discarded
     * There is 2 ways around it:
     *   - make a copy of the pointers to the nodes from the result set
     *     then call xmlXPathFreeObject() and then modify the nodes
     * or
     *   - remove the reference to the modified nodes from the node set
     *     as they are processed, if they are not namespace nodes.
     *)
    if nodes^.nodeTab[i]^._type <> XML_NAMESPACE_DECL then
      nodes^.nodeTab[i] := Nil;
  end;
end;

(**
 * example4:
 * @filename:		the input XML filename.
 * @xpathExpr:		the xpath expression for evaluation.
 * @value:		the new node content.
 *
 * Parses input XML file, evaluates XPath expression and update the nodes
 * then print the result.
 *
 * Returns 0 on success and a negative value otherwise.
 *)
function example4(const filename: PAnsiChar; const xpathExpr, value: xmlCharPtr): cint;
var
  doc: xmlDocPtr;
  xpathCtx: xmlXPathContextPtr;
  xpathObj: xmlXPathObjectPtr;
  mem: xmlCharPtr;
  memLen: Integer;
begin
  assert(filename <> Nil);
  assert(xpathExpr <> Nil);
  assert(value <> Nil);

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

  (* Evaluate xpath expression *)
  xpathObj := xmlXPathEvalExpression(xpathExpr, xpathCtx);
  if xpathObj = Nil then
  begin
    printfn('Error: unable to evaluate xpath expression "%s"', [xpathExpr]);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    Exit(-1);
  end;

  (* update selected nodes *)
  update_xpath_nodes(xpathObj^.nodesetval, value);


  (* Cleanup of XPath data *)
  xmlXPathFreeObject(xpathObj);
  xmlXPathFreeContext(xpathCtx);

  (* dump the resulting document *)
  xmlDocDumpMemory(doc, mem, memLen);
  WriteLn(mem);
  xmlFree(mem);

  (* free the document *)
  xmlFreeDoc(doc);

  Result := 0;
end;

begin
  (* Parse command line and process file *)
  if ParamCount <> 3 then
  begin
    printfn('Error: wrong number of arguments.');
    usage(ParamStr(0));
    Halt(-1);
  end;

  (* Init libxml *)
  xmlInitParser();
  LIBXML_TEST_VERSION;

  (* Do the main job *)
  if example4(PAnsiChar(ParamStr(1)), PAnsiChar(ParamStr(2)), PAnsiChar(ParamStr(3))) <> 0 then
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

