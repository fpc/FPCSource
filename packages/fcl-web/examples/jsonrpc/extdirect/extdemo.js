/* 
  Define namespace FPC 
*/

Ext.ns('FPC');

/* 
  Include our server-side Ext.Direct API; The default name 'FPWeb' was used
*/

Ext.Direct.addProvider(FPWeb);

/* 
  Callback used to process result. It will show the actual page
*/

FPC.ShowResult = function (Provider,Response) {
  var panel = new Ext.Panel({
    renderTo: Ext.getBody(),
    frame: true,
    title: "Adding 1.2 and 3.4 = " + Response.result,
    height: 50,
    width: 200,
    html: "The result is : " + Response.result
   });
  panel.show();
}

/* 
  onReady callback function 
*/

FPC.ShowPage = function () {
  /* 
    Call our API, using FPC.ShowResult as the callback to process the result 
  */
  DemoClass.Add(1.2, 3.4, FPC.ShowResult);
}

/* 
  Start Extjs 
*/

Ext.onReady(FPC.ShowPage);
