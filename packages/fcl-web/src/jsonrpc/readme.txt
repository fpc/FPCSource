
The JSON-RPC support for FPC consists out of 2 units:

fpjsonrpc: a set of components suitable for executing requests.
webjsonrpc: Handles all HTTP transport and content handling for the JSON-RPC requests.
fpextdirect: implements a Ext.Direct variant of JSON-RPC; 
             It includes HTTP transport an content handling.

The fpjsonrpc unit implements support for JSON-RPC: it implements versions
1.0 and 2.0 of the protocol, and has provisions for Ext.Direct-style RPC.

It introduces the following classes:

TCustomJSONRPCHandler = Class(TComponent)
  The main method of this class is 
    Function Execute(Const Params : TJSONData; AContext : TJSONRPCCallContext = Nil) : TJSONData;
       it executes a JSON-RPC call. The parameters to the call are in the Params
       parameter, the AContext parameter can be used by dispatching mechanisms to
       provide information about the context of the call (HTTP session etc.)
    Property ParamDefs : TJSONParamDefs Read FParamDefs Write SetParamDefs;
       Describes the parameters to the call. If 'Options' contains jroCheckParams, 
       then the types (and names, if appropriate) of the parameters will be checked.

TJSONRPCHandler = Class(TCustomJSONRPCHandler)
  A descendent which delegates the execution of the RPC call to an Event Handler.
  Typically, one will drop an instance of TJSONRPCHandler on a webmodule.
  Note that it does not provide context information in the event handler.

TCustomJSONRPCDispatcher = Class(TComponent)
  A request dispatching mechanism. It receives a JSON-encoded RPC request or a batch of
  requests: 
    Function Execute(Requests : TJSONData; AContext : TJSONRPCCallContext = Nil) : TJSONData;
  For each request in the batch, it will examine the 'method' and 'class' 
  of the request and will search for a TCustomJSONRPCHandler instance that
  can execute the call, and pass the call on to that instance. It also
  passes the context to the handler.

  If jsoSearchOwner is in OPtions, then it will look on the Owner (usually a
  TDatamodule descendent) for a TCustomJSONRPCHandler instance whose name
  matches 'method' in the request, and will invoke its 'Execute' method.

  if jdoSearchRegistry is in Options, then the JSONRPCHandlerManager factory
  is queryied for an instance of TCustomJSONRPCHandler that can handle the
  call. This instance is freed after the call ends.

  The dispatcher can check the validity of the request and responses 
  based on the Options property:
    jdoJSONRPC1, // Allow JSON RPC-1
    jdoJSONRPC2, // Allow JSON RPC-2
    jdoRequireClass, // Require class name (as in  Ext.Direct)
    jdoNotifications, // Allow JSON Notifications
    jdoStrictNotifications // Error if notification returned result. Default is to discard result.

TJSONRPCDispatcher = Class(TCustomJSONRPCDispatcher)
  A descendent of TCustomJSONRPCDispatcher that publishes some events and  
  properties:

  OnStartBatch : Called before a batch is started
  OnFindHandler : Called when no handler is found, can be used to construct custom method handlers.
  OnDispatchRequest : Called before the request is dispatched to the handler.
  OnEndBatch : called after the batch has been processed.

  Most methods of this component can be overridden to provide customized
  behaviour.


TCustomJSONRPCHandlerManager = Class(TComponent)

  A class that implements a TCustomJSONRPCHandler factory: definitions of TCustomJSONRPCHandler classes are kept.
  classes must be registered using one of the calls:
    Function RegisterHandler(Const AMethodName : TJSONStringType; AClass : TCustomJSONRPCHandlerClass; AArgumentCount : Integer = 0) : TJSONRPCHandlerDef; 
    Function RegisterHandler(Const AClassName,AMethodName : TJSONStringType; AClass : TCustomJSONRPCHandlerClass; AArgumentCount : Integer = 0) :TJSONRPCHandlerDef; 

  There is also support for registering a datamodule with TCustomJSONRPCHandler instances:

    Procedure RegisterDatamodule(Const AClass : TDatamoduleClass; Const AHandlerClassName : TJSONStringType);

  This will create an instance of the datamodule, register all TCustomJSONRPCHandler instances on the datamodule
  with the datamodule name as the classname is AHandlerClassName is empty, and the TCustomJSONRPCHandler instance 
  name as the method name. After registration, the module is freed. 

A global instance of TCustomJSONRPCHandlerManager is available through the

  Function JSONRPCHandlerManager : TCustomJSONRPCHandlerManager;

function. By default, an instance of TJSONRPCHandlerManager is created.

This instance is used by the dispatcher classes to search for a TCustomJSONRPCHandler instance.

The webjsonrpc unit implements HTTP transport and content handling for the json-rpc mechanism. 
It introduces the following classes:

TCustomJSONRPCContentProducer = Class(THTTPContentProducer)    

  Handles a HTTP request, extracts the JSON-RPC request from it, and returns the result in the HTTP response.
  It needs a dispatcher instance, which must be provided by a descendent.

TJSONRPCContentProducer = Class(TCustomJSONRPCContentProducer)
  Publishes a Dispatcher property, which must be set by the programmer. 
  All requests will be dispatched to this instance

TJSONRPCSessionContext = Class(TJSONRPCCallContext) 
  JSON-RPC call context which gives access to the HTTP Session object.

TSessionJSONRPCDispatcher = Class(TCustomJSONRPCDispatcher)
  JSON-RPC dispatcher which initializes the call context if the handler is
  located on a websession module.

TJSONRPCDispatchModule = Class(TSessionHTTPModule)
  Webmodule which knows how to dispatch a request and create session information for it.

TCustomJSONRPCModule = Class(TJSONRPCDispatchModule)
  It completely handles a JSON-RPC request over HTTP. 
  It creates a dispatcher if needed to handle the request.
  The dispatcher is by default of type TSessionJSONRPCDispatcher

TJSONRPCModule= Class(TCustomJSONRPCModule)  
  A webmodule which can be registered in the fcl-web module registry. 
  Publishes a property Dispatcher to dispatch a request, if set, this
  dispatcher will be used.
  Publishes a property DispatchOptions; If the module creates a dispatcher,  
  it will be passed these properties.

The fpextdirect unit contains the following classes:

TCustomExtDirectDispatcher = Class(TCustomJSONRPCDispatcher)
  A JSON-RPC dispatcher that conforms to Ext.Direct specifications.
  It sets the default Options to require a class name.
  It handles HTTP sessions just as TSessionJSONRPCDispatcher does.

TExtDirectDispatcher = Class(TCustomExtDirectDispatcher)
  Simply publishes all properties.

TCustomExtDirectContentProducer = Class(TCustomJSONRPCContentProducer)
  A content producer that knows how to handle the 'API' request. It
  assumes the path is something like /myapp/extdirect/api. All other
  paths are treated as actual method calls.

TCustomExtDirectModule =  Class(TJSONRPCDispatchModule)
  Handles JSON-RPC requests. It has 2 properties APIPath and RouterPath
  which determine how the request is handled:
  /modulepath/APIPath will create the API response.
  /modulepath/RouterPath will route the JSON-RPC request.

TExtDirectModule = Class(TCustomExtDirectModule)
  Publishes the properties introduced in TCustomExtDirectModule.

There are 4 ways to handle JSON-RPC in fcl-web:
1. Manual:
  A. Drop one or more TJSONRPCHandler components on a webmodule.
  B. Implement the OnExecute handlers of the TJSONRPCHandler components
  C. Handle the request in a OnRequest handler of an webaction and/or webmodule.
  This means 
  1. converting the request to JSONData.
  2. Extracting the method(s) and (their) parameters from the data.
  3. locating The TJSONRPCHandler.
  4. Passing the parameters to the Execute method of the handler.
  5. Convert the result to JSON and send it back.

2. Using a TJSONRPCDispatcher:
  A. Drop one or more TJSONRPCHandler components on a webmodule.
  B. Implement the OnExecute handlers of the TJSONRPCHandler components
  C. Handle the request in a OnRequest handler of an webaction and/or webmodule.
  This means 
  1. converting the request to JSONData.
  4. Passing the parameters to the Execute method of the handler
  5. Convert the result to JSON and send it back.

3. Using a TJSONRPCContentProducer:
   A. Drop one or more TJSONRPCHandler components on a webmodule.
   B. Implement the OnExecute handlers of the TJSONRPCHandler components.
   C. Handle the request in a OnRequest handler of an webaction and/or webmodule.
   1. Return the content of the TJSONRPCContentProducer. 

4. Using a TJSONRPCModule
   A. Drop one or more TJSONRPCHandler on a TJSONRPCModule.
   B. Implement the OnExecute handlers of the TJSONRPCHandler components.

For large applicaions, TJSONRPCHandler instances can be dropped on
datamodules or TJSONRPCModules, and the datamodule and/or TJSONRPCModules
must be registered using
  JSONRpcHandlerManager.RegisterModule(TMyModule,'myclassname');
if TJSONRPCModules are used, only 1 must be registered as a webmodule
to handle the request. It will dispatch the request to the other instances
if needed.

Note: if a request contains no class name, then the handler is searched
using the first matching method name. If 2 modules implement the same
method name, then the first match is used.
