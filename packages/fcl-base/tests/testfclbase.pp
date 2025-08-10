program testfclbase;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$ENDIf}
  punit,
  utcbasenenc,
  utcExprParsOps,
  utcExprParsAggr,
  utcExprBuiltin,
  utcExprParsParser,
  utcExprParsScanner,
  utcFPHashObjectList,
  utcFPTemplate,
  utcIniFile,
  utclzw,
  utcObjectList,
  utcMaskUtils,
  utcObjectStack,
  utcQueue,
  utcOrderedList,
  utcObjectQueue,
  utcStack,
  utcCSVDocument,
  utcCSVReadWrite,
  utcBufferedFileStream,
  utcClassList,
  utcComponentList,
  utcFPObjectList,
  utcFPStringHashTable,
  utcFPObjectHashTable,
  utcExprParsNodes,
  utcInterlocked,
  utcDirwatch
  ;

var
  lSuite : PSuite;

begin
  utcFPObjectList.RegisterTests;
  utcObjectList.RegisterTests;
  utcComponentList.RegisterTests;
  utcClassList.RegisterTests;
  utcOrderedList.RegisterTests;
  utcStack.RegisterTests;
  utcObjectStack.RegisterTests;
  utcQueue.RegisterTests;
  utcObjectQueue.RegisterTests;
  utcFPHashObjectList.RegisterTests;
  utcFPStringHashTable.RegisterTests;
  utcFPObjectHashTable.RegisterTests;
  utcBufferedFileStream.RegisterTests;
  utcIniFile.RegisterTests;
  utcCSVReadWrite.RegisterTests;
  utcMaskUtils.RegisterTests;
  utcCSVDocument.RegisterTests;
  utcfptemplate.RegisterTests;
  utcbasenenc.RegisterTests;
  utclzw.RegisterTests;
  lSuite:=AddSuite('ExpressionParser');
  utcExprParsScanner.RegisterTests(lSuite);
  utcExprParsNodes.RegisterTests(lSuite);
  utcExprParsOps.RegisterTests(lSuite);
  utcExprParsAggr.RegisterTests(lSuite);
  utcExprBuiltin.RegisterTests(lSuite);
  utcInterlocked.RegisterTests;
  utcDirwatch.RegisterTests;
  RunAllSysTests;
end.

