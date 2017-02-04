/*
    This file is part of the Free Pascal pas2js tool.
    Copyright (c) 2017 Mattias Gaertner

    Basic RTL for pas2js programs.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

var pas = {};

var rtl = {

  quiet: false,
  debug_load_units: true,

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  debug: function(){
    if (!window.console || rtl.quiet) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  isArray: function isArray(a) {
    return a instanceof Array;
  },

  isNumber: function isNumber(n){
    return typeof(n)=="number";
  },

  isInteger: function isInteger(A){
    return Math.floor(A)===A;
  },

  isBoolean: function isBoolean(b){
    return typeof(b)=="boolean";
  },

  isString: function isString(s){
    return typeof(s)=="string";
  },

  isObject: function isObject(o){
    return typeof(o)=="object";
  },

  isFunction: function isFunction(f){
    return typeof(f)=="function";
  },

  isNull: function isNull(o){
    return (o==null && typeof(o)=='object') || o==undefined;
  },

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  module: function(module_name, intfuseslist, code, impluseslist){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist);
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(code)) rtl.error('invalid module code of "'+module_name+'"');
    if ((impluseslist!=undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" already registered');

    var module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $code: code
    };
  },

  run: function(module_name){
    if (module_name==undefined) module_name='program';
    var module = pas[module_name];
    rtl.loadintf(module);
    rtl.loadimpl(module);
    if (module_name=='program'){
      rtl.debug('running $main');
      pas.program.$main();
    }
    return pas.System.ExitCode;
  },

  loadintf: function(module){
    if (module.state>rtl.m_loading_intf) return; // already finished
    rtl.debug('loadintf: '+module.$name);
    if (module.$state==rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    rtl.debug('loadintf: run intf of '+module.$name);
    module.$code(module.$intfuseslist,module);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    for (var i in useslist){
      var unitname=useslist[i];
      //rtl.debug('loaduseslist of "'+module.name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_loading_intf) rtl.loadintf(module);
    rtl.debug('loadimpl: '+module.$name+' load uses');
    module.$state=rtl.m_loading_impl;
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    // there are implementation uses cycles some used units might not yet be
    // initialized. This is by design.

    // run initialization
    rtl.debug('loadimpl: '+module.$name+' run init');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init))
      module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var wrapper = function(){
      return fn.apply(scope,arguments);
    };
    wrapper.fn = fn;
    return wrapper;
  },

  createClass: function(owner,name,ancestor,initfn){
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor; // c.$ancestor == Object.getPrototypeOf(c)
    } else {
      c = {};
      c.$create = function(fnname,args){
        var o = Object.create(this);
        o.$class = this; // Note: o.$class == Object.getPrototypeOf(o)
        if (args == undefined) args = [];
        o[fnname].apply(o,args);
        o.AfterConstruction();
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        this[fnname].apply(obj,[]);
      };
    };
    c.$classname = name;
    c.$name = owner.$name+'.'+name;
    c.$unitname = rtl.isString(owner.$unitname) ? owner.$unitname : owner.$name;
    owner[name] = c;
    initfn.call(c);
  },

  as: function(instance,typ){
    if(typ.isPrototypeOf(instance)) return instance;
    throw pas.System.EInvalidCast.$create("create");
  },

  setArrayLength: function(arr,newlength,defaultvalue){
    var oldlen = arr.length;
    if (oldlen==newlength) return;
    arr.length = newlength;
    for (var i=oldlen; i<newlength; i++) arr[i]=defaultvalue;
  },

  length: function(a){
    if (a==null){
      return 0;
    } else {
      return a.length;
    }
  },
}
