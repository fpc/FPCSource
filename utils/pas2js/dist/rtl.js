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
  debug_load_units: false,

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  isArray: function(a) {
    return a instanceof Array;
  },

  isNumber: function(n){
    return typeof(n)=="number";
  },

  isInteger: function(A){
    return Math.floor(A)===A;
  },

  isBoolean: function(b){
    return typeof(b)=="boolean";
  },

  isString: function(s){
    return typeof(s)=="string";
  },

  isObject: function(o){
    return typeof(o)=="object";
  },

  isFunction: function(f){
    return typeof(f)=="function";
  },

  isNull: function(o){
    return (o==null && typeof(o)=='object') || o==undefined;
  },

  isRecord: function(r){
    return (typeof(r)=="function") && (typeof(r.$create) == "function");
  },

  isClass: function(c){
    return (typeof(o)=="object") && (o.$class == o);
  },

  isClassInstance: function(c){
    return (typeof(o)=="object") && (o.$class == Object.getPrototypeOf(o));
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
    if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
    var module = pas[module_name];
    rtl.loadintf(module);
    rtl.loadimpl(module);
    if (module_name=='program'){
      if (rtl.debug_load_units) rtl.debug('running $main');
      pas.program.$main();
    }
    return pas.System.ExitCode;
  },

  loadintf: function(module){
    if (module.state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: '+module.$name);
    if (module.$state==rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of '+module.$name);
    module.$code(module.$intfuseslist,module);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    for (var i in useslist){
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_loading_intf) rtl.loadintf(module);
    if (rtl.debug_load_units) rtl.debug('loadimpl: '+module.$name+' load uses');
    module.$state=rtl.m_loading_impl;
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    // there are implementation uses cycles some used units might not yet be
    // initialized. This is by design.

    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: '+module.$name+' run init');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init))
      module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fnname){
    var cb = function(){
      return scope[fnname].apply(scope,arguments);
    };
    cb.scope = scope;
    cb.fnname = fnname;
    return cb;
  },

  cloneCallback: function(cb){
    return rtl.createCallback(cb.scope,cb.fnname);
  },

  eqCallback: function(a,b){
    if (a==null){
      return (b==null);
    } else {
      return (b!=null) && (a.scope==b.scope) && (a.fnname==b.fnname);
    }
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
        o.$init();
        o[fnname].apply(o,args);
        o.AfterConstruction();
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        this[fnname]();
        this.$final;
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

  arraySetLength: function(arr,newlength,defaultvalue){
    var oldlen = arr.length;
    if (oldlen==newlength) return;
    arr.length = newlength;
    if (rtl.isArray(defaultvalue)){
      for (var i=oldlen; i<newlength; i++) arr[i]=[]; // new array
    } else if (rtl.isFunction(defaultvalue)){
      for (var i=oldlen; i<newlength; i++) arr[i]=new defaultvalue(); // new record
    } else {
      for (var i=oldlen; i<newlength; i++) arr[i]=defaultvalue;
    }
    return arr;
  },

  arrayNewMultiDim: function(dims,defaultvalue){
    function create(dim){
      if (dim == dims.length-1){
        return rtl.arraySetLength([],dims[dim],defaultvalue);
      }
      var a = [];
      var count = dims[dim];
      a.length = count;
      for(var i=0; i<count; i++) a[i] = create(dim+1);
      return a;
    };
    return create(0);
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key)) r[key]=true;
    return r;
  },

  refSet: function(s){
    s.$shared = true;
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && !t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key)) r[key]=true;
    for (var key in t) if (t.hasOwnProperty(key)) r[key]=true;
    delete r.$shared;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && t[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (s.hasOwnProperty(key) && !t[key]) r[key]=true;
    for (var key in t) if (t.hasOwnProperty(key) && !s[key]) r[key]=true;
    delete r.$shared;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (s.hasOwnProperty(key) && !t[key] && (key!='$shared')) return false;
    for (var key in t) if (t.hasOwnProperty(key) && !s[key] && (key!='$shared')) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (s.hasOwnProperty(key) && !t[key] && (key!='$shared')) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (t.hasOwnProperty(key) && !s[key] && (key!='$shared')) return false;
    return true;
  },
}
