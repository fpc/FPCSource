
{
  Automatically converted by H2Pas 1.0.0 from oro.h
  The following command line parameters were used:
    -p
    -D
    -l
    jojo.so
    oro.h
}

{$PACKRECORDS C}

  { Copyright (c) 1994, 2003, Oracle Corporation.  All rights reserved.   }
  {
    NAME
      OCI - Oracle Object Interface for External/Internal/Kernel Clients
  
    DESCRIPTION
      This header file contains Oracle object interface definitions which
      can be included by external user applications, tools, as well as
      the kernel.  It defines types and constants that are common to all
      object interface which is being defined in several other header files
      (e.g., ori.h, ort.h, and orl.h).
  
    RELATED DOCUMENTS
      TBD
  
    INSPECTION STATUS [[ deletable if no inspection ]]
      Inspection date:    [[ date of the last logging meeting ]]
      Inspection status:  [[ exited, not exited, or N/A if exit is not a goal ]]
      Estimated increasing cost defects per page:
      Rule sets:          [[ rule sets inspected against or planned to be 
                             inspected against ]]
  
    ACCEPTANCE REVIEW STATUS [[ deletable if no approval review ]]
      Review date:    [[ date of the meeting where issues were logged and the 
                         approval status was decided ]] 
      Review status:  [[ current status:  accepted, conditionally accepted, 
                         major revision required, rejected ]]
      Reviewers:      [[ names of the members on the review team ]]
  
    PUBLIC FUNCTIONS
  
    EXAMPLES
      Examples are given in the description of each function or macro where
      relevant.
  
    MODIFIED
      mnanal     06/09/03 - backout of fix 2836388
      mnanal     05/14/03 - bug-2836388
      srseshad   11/27/02 - Change OCI_TYPECODE_BFLOAT/BDOUBLE codes
      rxgovind   10/09/02 - add OCI_TYPECODE_UROWID
      mxyang     09/17/02 - grabtrans 'mmorsi_obj_float'
      srseshad   09/11/02 - 
      srseshad   09/01/02 - 
      aahluwal   06/03/02 - bug 2360115
      celsbern   10/19/01 - merge LOG to MAIN
      rxgovind   10/16/01 - update typecodes
      rxgovind   09/19/01 - add typecodes
      rkasamse   08/15/01 - add OCI_DURATION_USER_CALLBACK
      jchai      09/24/01 - add type code for PLS_INTEGER
      porangas   08/22/01 - Fix bug#1776434
      schatter   04/09/01 - merge 1456235: define OCI_DURATION_INVALID
      rdani      10/12/00 - 1449943 NOCOPY and PIPELINE
      ciyer      05/26/00 - short names for abstract, overriding
      rkasamse   05/25/00 - OCCI enhancements
      smuralid   05/11/00 - OCITypeMethodFlags - add NOT INSTANTIABLE, OVERRIDING
      rxgovind   05/09/00 - add OCI_TYPECODE_NONE
      tnbui      07/28/99 - Remove OCI_TYPECODE_TIMESTAMP_ITZ                    
      tnbui      07/21/99 - TS LOCAL TZ                                          
      thoang     06/21/99 - Add OCI_TYPECODE_TIMESTAMP_ITZ
      thoang     03/04/99 - Add datetime datatypes
      rkasamse   10/20/98 - add OCI_ATTR_CACHE_ARRAYFLUSH
      rkasamse   10/29/98 - add OCI_DURATION_CALLOUT
      rkasamse   04/28/98 - OCI_OBJECT_DETECTCHANGE -> OCI_ATTR_OBJECT_DETECTCHAN
      rkasamse   04/28/98 - OCI_OBJECT_NEWNOTNULL -> OCI_ATTR_OBJECT_NEWNOTNULL  
      rkasamse   04/23/98 - add OCI_OBJECT_DETECTCHANGE
      rkasamse   04/03/98 - add OCI_OBJECT_NEWNOTNULL                            
      pmitra     04/01/98 - OCI_LOCK_X_NOWAIT added                              
      rxgovind   02/18/98 - add OCI_TYPECODE_OPAQUE
      rkasamse   02/13/98 - Add OCI_DURATION_PROCESS
      cxcheng    07/28/97 - fix compile with SLSHORTNAME
      skrishna   07/14/97 - add OCIObjectGetProperty
      cxcheng    04/30/97 - make OCITypeParamMode values consistent with PL/SQL
      skrishna   04/28/97 - undocument OCIObjectProperty & OCIObjectEvent
      cxcheng    03/29/97 - remove all remaining short names
      sthakur    03/20/97 - add casts to constants
      cxcheng    02/21/97 - temporarily put SLSHORTNAME for PL/SQL
      cxcheng    02/06/97 - take out short name support except with SLSHORTNAME
      lchidamb   01/20/97 - update OCIRef comments
      sgollapu   11/19/96 - Add OCI type codes for BOOL,REC,and TAB
      cxcheng    11/19/96 - more typecode changes
      cxcheng    11/13/96 - add #include for ocidfn.h
      cxcheng    11/13/96 - add OCI_TYPECODE_ADT for compatibility
      cxcheng    11/12/96 - add SQLT_NCO for named collection
      cxcheng    11/11/96 - more changes to typecodes
      cxcheng    11/07/96 - #define OCI_TYPECODE_MLSLABEL to SQLT_LAB
      cxcheng    11/06/96 - fix #define omission for OROTCNAT
      cxcheng    10/30/96 - move OCI_TYPECODE_* to ocidfn.h as SQLT_*
      cxcheng    10/28/96 - more beautification changes
      jboonleu   10/29/96 - add flags for freeing object
      dchatter   10/26/96 - delete redef of OCISvcCtx, OCIError, OCIEnv
      cxcheng    10/15/96 - more changes
      cxcheng    10/14/96 - more final fixes to constants
      mluong     10/11/96 -
      mluong     10/11/96 -  KOCON and KONSP are in lowercase
      mluong     10/11/96 -  add some define per Calvin
      cxcheng    10/09/96 -  add #define for OROOCOSFN to OCI_COPY_NOREF
      jboonleu   10/08/96 -  change OROOCOSFN to OCICopyFlag
      jboonleu   10/07/96 -  use new OCI names for cache options
      cxcheng    10/07/96 -  add OROTCS02 for KOTTCBRI and OROTCS03 as spare
      cxcheng    10/07/96 -  more lint fixes
      cxcheng    10/02/96 -  move oronsp to ko.h as konsp
      cxcheng    10/01/96 -  add long names for readability
      cxcheng    10/01/96 -  remove orotty and orotal
      rjenkins   09/28/96 -  2k char 4k varchar2
      jboonleu   09/27/96 -  add macro used only in beta2
      cxcheng    09/27/96 -  move oroenv to oroenv.h
      cxcheng    09/24/96 -  remove unnecessary orotyp
      cxcheng    09/25/96 -  add typecode OROTCS01 as placeholder for lob pointer
      cxcheng    09/20/96 -  add TDO load option orotgo
      jboonleu   09/18/96 -  add OROOPOREC
      jboonleu   09/10/96 -  add OROOPODFL
      jweisz     08/27/96 -  add SQL internal typecode OROTCS00
      cxcheng    08/02/96 -  add PLSQL internal typecodes OROTCP..
      cxcheng    08/01/96 -  add OROTCFAR to fill up space left by OROTCCAR
      jboonleu   07/16/96 -  new pin option
      cxcheng    06/18/96 -  add casts to OROTNOPRE and OROTNOSCL
      cxcheng    05/29/96 -  change OROTCNPT back to OROTCDOM
      vkrishna   05/27/96 -  add OROTCCAR
      cxcheng    05/17/96 -  replace OROTCFAR with OROTCCAR
      cxcheng    05/08/96 -  change orotmf from ub1 to ub2
      cxcheng    05/07/96 -  fix public defines for method types
      cxcheng    04/30/96 -  change OROTCDOM to OROTCNPT
      cxcheng    04/15/96 -  remove obsolete OROTTYICT
      jboonleu   04/12/96 -  add new pin option
      sthakur    04/12/96 -  add indicator type and indicator status
      cxcheng    04/10/96 -  add function parameter codes for ORT/KOT
      cxcheng    04/03/96 -  replace OROTCFAR as OROTCCAR
      jwijaya    03/29/96 -  add OROTTCCAR
      jwijaya    03/27/96 -  better comments for orotc
      cxcheng    02/23/96 -  add typecodes for SMALLINT and VARCHAR2
      skrishna   02/22/96 -  add oroind - null indicator type
      cxcheng    02/21/96 -  change lob character codes to OROTCCLB, OROTCBLB...
      jboonleu   02/06/96 -  new value for predefined duration
      cxcheng    01/12/96 -  add OROTCCLO, OROTCBLO, OROTCFIL to orotc
      cxcheng    12/05/95 -  add OROTCDOM and OROTCAAT to orotc
      skotsovo   10/30/95 -  reserve space for internal 'oid' type
      jwijaya    10/20/95 -  support variable-length ref
      cxcheng    10/03/95 -  add OROTMFOR for ordering function to orotmf
      cxcheng    10/03/95 -  Adding the ordering function type to orotmf
      jboonleu   09/28/95 -  set OROODTPRE 
      jboonleu   09/25/95 -  add oroodt
      skotsovo   03/10/95 -  update to only include release 1 
      jboonleu   02/15/95 -  add OROOPOREC, remove orocro, oroolo
      skotsovo   01/30/95 -  add default max lengths for varrays and vstrings 
      skotsovo   01/24/95 -  categorize sint32, double, and real as number types 
                             (with precision and scale) instead of scalar types.
      skotsovo   12/20/94 -  add release 1 types 
      skotsovo   12/12/94 -  update according to new ots doc
      skotsovo   12/01/94 -  add default precision and scale 
      jwijaya    11/15/94 -  rename ORONSPTAB to ORONSPEXT 
      jwijaya    10/25/94 -  tint 
      jwijaya    10/06/94 -  add namespace 
      jwijaya    10/02/94 -  connection handle -> connection number 
      skotsovo   09/12/94 -  keep 0 as uninitialized value for ORT consts 
      skotsovo   08/24/94 -  fix orotec 
      skotsovo   08/17/94 -  modify type code names 
      skotsovo   08/12/94 -  fix 141 lint errors 
      skotsovo   07/25/94 -  modify categorization of complex types (orotc) 
      skotsovo   07/07/94 -  change typecode enum values & add decimal type
      skotsovo   07/01/94 -  change order of typecodes 
      jwijaya    06/15/94 -  review 
      jboonleu   06/13/94 -  add comments for the object cache options
      jwijaya    06/13/94 -  adhere to the header file template 
      skotsovo   06/09/94 -  make ots scalar type names consistent with the ots 
                             document 
      jwijaya    06/07/94 -  include oratypes.h instead of s.h 
      skotsovo   05/24/94 -  change typecodes 
      jwijaya    05/23/94 -  fix comments of ororef 
      skotsovo   05/19/94 -  remove type composition 
      skotsovo   05/09/94 -  modified orotc according to new OTS document
      jwijaya    05/03/94 -  oroid and ororef
      jwijaya    01/26/94 -  Creation
   }

  {--------------------------------------------------------------------------- }
  {                         SHORT NAMES SUPPORT SECTION                        }
  {--------------------------------------------------------------------------- }
{$ifdef SLSHORTNAME}
  { the following are short names that are only supported on IBM mainframes
     with the SLSHORTNAME defined.
     With this all subsequent long names will actually be substituted with
     the short names here  }

  const
     OCIDuration = oroodt;     
     OCIInd = oroind;     
     OCILockOpt = oroolm;     
     OCIMarkOpt = oroomo;     
     OCIObjectEvent = orocev;     
     OCIObjectProperty = oroopr;     
     OCIPinOpt = oroopo;     
     OCIRef = ororef;     
     OCIRefreshOpt = orooro;     
     OCITypeCode = orotc;     
     OCITypeEncap = orotec;     
     OCITypeGetOpt = orotgo;     
     OCITypeMethodFlag = orotmf;     
     OCITypeParamMode = orotpm;     
     OCIObjectPropId = oroopi;     
     OCIObjectLifetime = oroolft;     
     OCIObjectMarkstatus = oroomst;     
     OCI_LOCK_NONE = OROOLMNUL;     
     OCI_LOCK_X = OROOLMX;     
     OCI_LOCK_X_NOWAIT = OROOLMXNW;     
     OCI_MARK_DEFAULT = OROOMODFL;     
     OCI_MARK_NONE = OROOMONON;     
     OCI_MARK_UPDATE = OROOMOUPD;     
     OCI_OBJECTEVENT_AFTER_FLUSH = OROCEVAFL;     
     OCI_OBJECTEVENT_AFTER_REFRESH = OROCEVARF;     
     OCI_OBJECTEVENT_BEFORE_FLUSH = OROCEVBFL;     
     OCI_OBJECTEVENT_BEFORE_REFRESH = OROCEVBRF;     
     OCI_OBJECTEVENT_WHEN_LOCK = OROCEVWLK;     
     OCI_OBJECTEVENT_WHEN_MARK_DELETED = OROCEVWDL;     
     OCI_OBJECTEVENT_WHEN_MARK_UPDATED = OROCEVWUP;     
     OCI_OBJECTEVENT_WHEN_UNMARK = OROCEVWUM;     
     OCI_OBJECTPROP_DIRTIED = OROOPRDRT;     
     OCI_OBJECTPROP_LOADED = OROOPRLOD;     
     OCI_OBJECTPROP_LOCKED = OROOPRLCK;     
     OCI_PIN_ANY = OROOPOANY;     
     OCI_PIN_DEFAULT = OROOPODFL;     
     OCI_PIN_LATEST = OROOPOLST;     
     OCI_PIN_RECENT = OROOPOREC;     
     OCI_REFRESH_LOADED = OROOROLOD;     
     OCI_TYPEENCAP_PRIVATE = OROTECPVT;     
     OCI_TYPEENCAP_PUBLIC = OROTECPUB;     
     OCI_TYPEGET_ALL = OROTGOALL;     
     OCI_TYPEGET_HEADER = OROTGOHDR;     
     OCI_TYPEMETHOD_CONSTANT = OROTMCON;     
     OCI_TYPEMETHOD_CONSTRUCTOR = OROTMCSTR;     
     OCI_TYPEMETHOD_DESTRUCTOR = OROTMDSTR;     
     OCI_TYPEMETHOD_INLINE = OROTMINL;     
     OCI_TYPEMETHOD_MAP = OROTMMAP;     
     OCI_TYPEMETHOD_OPERATOR = OROTMOP;     
     OCI_TYPEMETHOD_ORDER = OROTMOR;     
     OCI_TYPEMETHOD_RNDS = OROTMRDS;     
     OCI_TYPEMETHOD_RNPS = OROTMRPS;     
     OCI_TYPEMETHOD_SELFISH = OROTMSLF;     
     OCI_TYPEMETHOD_VIRTUAL = OROTMVRT;     
     OCI_TYPEMETHOD_WNDS = OROTMWDS;     
     OCI_TYPEMETHOD_WNPS = OROTMWPS;     
     OCI_TYPEMETHOD_ABSTRACT = OROTMABSTRACT;     
     OCI_TYPEMETHOD_OVERRIDING = OROTMOVERRIDING;     
     OCI_TYPEMETHOD_PIPELINED = OROTMPIPELINED;     
     OCI_TYPEPARAM_BYREF = OROTPMREF;     
     OCI_TYPEPARAM_IN = OROTPMIN;     
     OCI_TYPEPARAM_INOUT = OROTPMIO;     
     OCI_TYPEPARAM_OUT = OROTPMOUT;     
     OCI_TYPEPARAM_OUTNCPY = OROTPMOUTNCPY;     
     OCI_TYPEPARAM_INOUTNCPY = OROTPMIONCPY;     
{$endif}
  { SLSHORTNAME  }
  {--------------------------------------------------------------------------- }
  {                    PUBLIC TYPES, CONSTANTS AND MACROS                      }
  {--------------------------------------------------------------------------- }
  {--------------------------------------------------------------------------- }
  {                  GENERAL OBJECT TYPES, CONSTANTS, MACROS                   }
  {--------------------------------------------------------------------------- }
  {------------------------- OBJECT REFERENCE (REF) -------------------------- }

  type
    POCIRef = pointer;
  {
   * OCIRef - OCI object REFerence
   *
   * In the Oracle object runtime environment, an object is identified by an 
   * object reference (ref) which contains the object identifier plus other 
   * runtime information.  The contents of a ref is opaque to clients.  Use
   * OCIObjectNew() to construct a ref.
    }
  {--------------------------- OBJECT INDICATOR ------------------------------ }

     POCIInd = ^OCIInd;
     OCIInd = sb2;
  {
   * OCIInd -- a variable of this type contains (null) indicator information
    }
  { not NULL  }
  { was #define dname def_expr }
  function OCI_IND_NOTNULL : OCIInd;    

  { NULL  }
  { was #define dname def_expr }
  function OCI_IND_NULL : OCIInd;    

  { BAD NULL  }
  { was #define dname def_expr }
  function OCI_IND_BADNULL : OCIInd;    

  { not NULLable  }
  { was #define dname def_expr }
  function OCI_IND_NOTNULLABLE : OCIInd;    

  {--------------------------------------------------------------------------- }
  {                               OBJECT CACHE                                 }
  {--------------------------------------------------------------------------- }
  { To enable object change detection mode, set this to TRUE  }

  const
     OCI_ATTR_OBJECT_DETECTCHANGE = $00000020;     
  { To enable object creation with  non-NULL attributes by default, set the
     following to TRUE.
     By default, object is created with NULL attributes
   }
     OCI_ATTR_OBJECT_NEWNOTNULL = $00000010;     
  { To enable sorting of the objects that belong to the same table
     before being flushed through OCICacheFlush.
     Please note that by enabling this object cache will not be flushing
     the objects in the same order they were dirtied  }
     OCI_ATTR_CACHE_ARRAYFLUSH = $00000040;     
  {--------------------------- OBJECT PIN OPTION ----------------------------- }
  { 0 = uninitialized  }
  { default pin option  }
  { pin any copy of the object  }
  { pin recent copy of the object  }
  { pin latest copy of the object  }

  type
     OCIPinOpt = (OCI_PIN_DEFAULT := 1,OCI_PIN_ANY := 3,
       OCI_PIN_RECENT := 4,OCI_PIN_LATEST := 5
       );


     POCIPinOpt = ^OCIPinOpt;
  {
   * OCIPinOpt - OCI object Pin Option 
   *
   * In the Oracle object runtime environment, the program has the option to
   * specify which copy of the object to pin.  
   *
   * OCI_PINOPT_DEFAULT pins an object using the default pin option.  The default
   * pin option can be set as an attribute of the OCI environment handle 
   * (OCI_ATTR_PINTOPTION).  The value of the default pin option can be
   * OCI_PINOPT_ANY, OCI_PINOPT_RECENT, or OCI_PIN_LATEST. The default option
   * is initialized to OCI_PINOPT_ANY.
   *
   * OCI_PIN_ANY pins any copy of the object.  The object is pinned 
   * using the following criteria:
   *   If the object copy is not loaded, load it from the persistent store.
   *   Otherwise, the loaded object copy is returned to the program. 
   * 
   * OCI_PIN_RECENT pins the latest copy of an object.  The object is 
   * pinned using the following criteria:
   *   If the object is not loaded, load the object from the persistent store
   *       from the latest version.
   *   If the object is not loaded in the current transaction and it is not 
   *       dirtied, the object is refreshed from the latest version.
   *   Otherwise, the loaded object copy is returned to the program. 
   *
   * OCI_PINOPT_LATEST pins the latest copy of an object.  The object copy is 
   * pinned using the following criteria:
   *   If the object copy is not loaded, load it from the persistent store.
   *   If the object copy is loaded and dirtied, it is returned to the program.
   *   Otherwise, the loaded object copy is refreshed from the persistent store.
    }
  {--------------------------- OBJECT LOCK OPTION ---------------------------- }
  { 0 = uninitialized  }
  { null (same as no lock)  }
  { exclusive lock  }
  { exclusive lock, do not wait   }
     OCILockOpt = (OCI_LOCK_NONE := 1,OCI_LOCK_X := 2,
       OCI_LOCK_X_NOWAIT := 3);


     POCILockOpt = ^OCILockOpt;
  {
   * OCILockOpt - OCI object LOCK Option 
   *
   * This option is used to specify the locking preferences when an object is
   * loaded from the server.
    }
  {------------------------- OBJECT MODIFYING OPTION ------------------------- }
  { 0 = uninitialized  }
  { default (the same as OCI_MARK_NONE)  }
  { object has not been modified  }
  { object is to be updated  }
{     OCIMarkOpt = (OCI_MARK_DEFAULT := 1,OCI_MARK_NONE := OCI_MARK_DEFAULT,
       OCI_MARK_UPDATE);


     POCIMarkOpt = ^OCIMarkOpt;}
  {
   * OCIMarkOpt - OCI object Mark option
   *
   * When the object is marked updated, the client has to specify how the
   * object is intended to be changed.  
    }
  {-------------------------- OBJECT Duration -------------------------------- }

     POCIDuration = ^OCIDuration;
     OCIDuration = ub2;
  { Invalid duration  }

  const
     OCI_DURATION_INVALID = $FFFF;     
  { was #define dname def_expr }
  function OCI_DURATION_BEGIN : OCIDuration;    

  { beginning sequence of duration  }
  { was #define dname def_expr }
  function OCI_DURATION_NULL : OCIDuration;    

  { null duration  }
  { default  }
  { was #define dname def_expr }
  function OCI_DURATION_DEFAULT : OCIDuration;    

  { was #define dname def_expr }
  function OCI_DURATION_USER_CALLBACK : OCIDuration;    

  { was #define dname def_expr }
  function OCI_DURATION_NEXT : OCIDuration;    

  { next special duration  }
  { was #define dname def_expr }
  function OCI_DURATION_SESSION : OCIDuration;    

  { the end of user session  }
  { was #define dname def_expr }
  function OCI_DURATION_TRANS : OCIDuration;    

  { the end of user transaction  }
  {*****************************************************************************
  **  DO NOT USE OCI_DURATION_CALL. IT  IS UNSUPPORTED                         **
  **  WILL BE REMOVED/CHANGED IN A FUTURE RELEASE                              **
  ***************************************************************************** }
  { was #define dname def_expr }
  function OCI_DURATION_CALL : OCIDuration;    

  { the end of user client/server call  }
  { was #define dname def_expr }
  function OCI_DURATION_STATEMENT : OCIDuration;    

  { This is to be used only during callouts.  It is similar to that 
  of OCI_DURATION_CALL, but lasts only for the duration of a callout.
  Its heap is from PGA  }
  { was #define dname def_expr }
  function OCI_DURATION_CALLOUT : OCIDuration;    


{  const
     OCI_DURATION_LAST = OCI_DURATION_CALLOUT;     }
  { last of predefined durations  }
  { This is not being treated as other predefined durations such as 
     SESSION, CALL etc, because this would not have an entry in the duration
     table and its functionality is primitive such that only allocate, free,
     resize memory are allowed, but one cannot create subduration out of this
   }  { was #define dname def_expr }
  function OCI_DURATION_PROCESS : OCIDuration;    

  {
   * OCIDuration - OCI object duration
   *
   * A client can specify the duration of which an object is pinned (pin
   * duration) and the duration of which the object is in memory (allocation
   * duration).  If the objects are still pinned at the end of the pin duration,
   * the object cache manager will automatically unpin the objects for the
   * client. If the objects still exist at the end of the allocation duration,
   * the object cache manager will automatically free the objects for the client.
   *
   * Objects that are pinned with the option OCI_DURATION_TRANS will get unpinned
   * automatically at the end of the current transaction.
   *
   * Objects that are pinned with the option OCI_DURATION_SESSION will get
   * unpinned automatically at the end of the current session (connection).
   *
   * The option OCI_DURATION_NULL is used when the client does not want to set
   * the pin duration.  If the object is already loaded into the cache, then the
   * pin duration will remain the same.  If the object is not yet loaded, the
   * pin duration of the object will be set to OCI_DURATION_DEFAULT.
    }
  {----------------------------- OBJECT PROPERTY ----------------------------- }
  {*****************************************************************************
  **  DO NOT USE OCIObjectProperty. IT IS UNSUPPORTED                          **
  **  WILL BE REMOVED/CHANGED IN A FUTURE RELEASE                              **
  ***************************************************************************** }
  { 0 = uninitialized  }
  { dirty objects  }
  { objects loaded in the transaction  }
  { locked objects  }

  type
     OCIObjectProperty = (OCI_OBJECTPROP_DIRTIED := 1,OCI_OBJECTPROP_LOADED,
       OCI_OBJECTPROP_LOCKED);


     POCIObjectProperty = ^OCIObjectProperty;
  {
   * OCIObjectProperty -- OCI Object Property
   * This specifies the properties of objects in the object cache.
    }
  {------------------------- CACHE REFRESH OPTION --------------------------- }
  { 0 = uninitialized  }
  { refresh objects loaded in the transaction  }
     OCIRefreshOpt = (OCI_REFRESH_LOADED := 1);


     POCIRefreshOpt = ^OCIRefreshOpt;
  {
   * OCIRefreshOpt - OCI cache Refresh Option
   * This option is used to specify the set of objects to be refreshed. 
   *
   * OCI_REFRESH_LOAD refreshes the objects that are loaded in the current  
   * transaction. 
    }
  {-------------------------------- OBJECT EVENT ----------------------------- }
  {*****************************************************************************
  **  DO NOT USE OCIObjectEvent. IT IS UNSUPPORTED                             **
  **  WILL BE REMOVED/CHANGED IN A FUTURE RELEASE                              **
  ***************************************************************************** }
  { 0 = uninitialized  }
  { before flushing the cache  }
  { after flushing the cache  }
  { before refreshing the cache  }
  { after refreshing the cache  }
  { when an object is marked updated  }
  { when an object is marked deleted  }
  { when an object is being unmarked  }
  { when an object is being locked  }
     OCIObjectEvent = (OCI_OBJECTEVENT_BEFORE_FLUSH := 1,
       OCI_OBJECTEVENT_AFTER_FLUSH,OCI_OBJECTEVENT_BEFORE_REFRESH,
       OCI_OBJECTEVENT_AFTER_REFRESH,OCI_OBJECTEVENT_WHEN_MARK_UPDATED,
       OCI_OBJECTEVENT_WHEN_MARK_DELETED,OCI_OBJECTEVENT_WHEN_UNMARK,
       OCI_OBJECTEVENT_WHEN_LOCK);


     POCIObjectEvent = ^OCIObjectEvent;
  {
   * OCIObjectEvent -- OCI Object Event 
   * This specifies the kind of event that is supported by the object
   * cache.  The program can register a callback that is invoked when the  
   * specified event occurs.
    }
  {----------------------------- OBJECT COPY OPTION -------------------------- }
  { was #define dname def_expr }
  function OCI_OBJECTCOPY_NOREF : ub1;    

  {
   * OCIObjectCopyFlag - Object copy flag
   *
   * If OCI_OBJECTCOPY_NOREF is specified when copying an instance, the 
   * reference and lob will not be copied to the target instance.
    }
  {----------------------------- OBJECT FREE OPTION -------------------------- }
  { was #define dname def_expr }
  function OCI_OBJECTFREE_FORCE : ub2;    

  { was #define dname def_expr }
  function OCI_OBJECTFREE_NONULL : ub2;    

  { was #define dname def_expr }
  function OCI_OBJECTFREE_HEADER : ub2;    

  {
   * OCIObjectFreeFlag - Object free flag
   *
   * If OCI_OBJECTCOPY_FORCE is specified when freeing an instance, the instance
   * is freed regardless it is pinned or diritied.
   * If OCI_OBJECTCOPY_NONULL is specified when freeing an instance, the null 
   * structure is not freed.
    }
  {----------------------- OBJECT PROPERTY ID ------------------------------- }

  type

     POCIObjectPropId = ^OCIObjectPropId;
     OCIObjectPropId = ub1;
  { persistent or transient or value  }

  const
     OCI_OBJECTPROP_LIFETIME = 1;     
  { schema name of table containing object  }
     OCI_OBJECTPROP_SCHEMA = 2;     
  { table name of table containing object  }
     OCI_OBJECTPROP_TABLE = 3;     
  { pin duartion of object  }
     OCI_OBJECTPROP_PIN_DURATION = 4;     
  { alloc duartion of object  }
     OCI_OBJECTPROP_ALLOC_DURATION = 5;     
  { lock status of object  }
     OCI_OBJECTPROP_LOCK = 6;     
  { mark status of object  }
     OCI_OBJECTPROP_MARKSTATUS = 7;     
  { is object a view object or not?  }
     OCI_OBJECTPROP_VIEW = 8;     
  {
   * OCIObjectPropId - OCI Object Property Id
   * Identifies the different properties of objects.
    }
  {----------------------- OBJECT LIFETIME ---------------------------------- }
  { 0 = uninitialized  }
  { persistent object  }
  { transient object  }
  { value object  }

  type
     OCIObjectLifetime = (OCI_OBJECT_PERSISTENT := 1,OCI_OBJECT_TRANSIENT,
       OCI_OBJECT_VALUE);


     POCIObjectLifetime = ^OCIObjectLifetime;
  {
   * OCIObjectLifetime - OCI Object Lifetime
   * Classifies objects depending upon the lifetime and referenceability 
   * of the object.
    }
  {----------------------- OBJECT MARK STATUS ------------------------------- }

     POCIObjectMarkStatus = ^OCIObjectMarkStatus;
     OCIObjectMarkStatus = uword;
  { new object  }

  const
     OCI_OBJECT_NEW = $0001;     
  { object marked deleted  }
     OCI_OBJECT_DELETED = $0002;     
  { object marked updated  }
     OCI_OBJECT_UPDATED = $0004;     
  {
   * OCIObjectMarkStatus - OCI Object Mark Status
   * Status of the object - new or updated or deleted
    }
  { macros to test the object mark status  }  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_UPDATED(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_DELETED(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_NEW(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_OBJECT_IS_DIRTY(flag : longint) : longint;  

  {--------------------------------------------------------------------------- }
  {                               TYPE MANAGER                                 }
  {--------------------------------------------------------------------------- }
  {------------------------------ TYPE CODE ---------------------------------- }
  {
   * Type manager typecodes
   *
   * These are typecodes designed to be used with the type manager;
   * they also include longer, more readable versions of existing SQLT names.
   * Those types that are directly related to existing SQLT types are #define'd
   * to their SQLT equivalents.
   *
   * The type manager typecodes are designed to be useable for all OCI calls.
   * They are in the range from 192 to 320 for typecodes, so as not to conflict
   * with existing OCI SQLT typecodes (see ocidfn.h).
    }
  { SQL/OTS OBJECT REFERENCE  }

  const
     OCI_TYPECODE_REF = SQLT_REF;
  { SQL DATE  OTS DATE  }
     OCI_TYPECODE_DATE = SQLT_DAT;
  { SQL SIGNED INTEGER(8)  OTS SINT8  }
     OCI_TYPECODE_SIGNED8 = 27;
  { SQL SIGNED INTEGER(16)  OTS SINT16  }
     OCI_TYPECODE_SIGNED16 = 28;
  { SQL SIGNED INTEGER(32)  OTS SINT32  }
     OCI_TYPECODE_SIGNED32 = 29;
  { SQL REAL  OTS SQL_REAL  }
     OCI_TYPECODE_REAL = 21;
  { SQL DOUBLE PRECISION  OTS SQL_DOUBLE  }
     OCI_TYPECODE_DOUBLE = 22;
  { Binary float  }
     OCI_TYPECODE_BFLOAT = SQLT_IBFLOAT;
  { Binary double  }
     OCI_TYPECODE_BDOUBLE = SQLT_IBDOUBLE;
  { SQL FLOAT(P)  OTS FLOAT(P)  }
     OCI_TYPECODE_FLOAT = SQLT_FLT;
  { SQL NUMBER(P S)  OTS NUMBER(P S)  }
    OCI_TYPECODE_NUMBER = SQLT_NUM;
    OCI_TYPECODE_DECIMAL = SQLT_PDN;
  { SQL DECIMAL(P S)  OTS DECIMAL(P S)  }
     OCI_TYPECODE_UNSIGNED8 = SQLT_BIN;
  { SQL UNSIGNED INTEGER(8)  OTS UINT8  }
  { SQL UNSIGNED INTEGER(16)  OTS UINT16  }
     OCI_TYPECODE_UNSIGNED16 = 25;
  { SQL UNSIGNED INTEGER(32)  OTS UINT32  }
     OCI_TYPECODE_UNSIGNED32 = 26;
  { SQL ???  OTS OCTET  }
//     OCI_TYPECODE_OCTET = 245;
  { SQL SMALLINT  OTS SMALLINT  }
     OCI_TYPECODE_SMALLINT = 246;     
  { SQL INTEGER  OTS INTEGER  }
     OCI_TYPECODE_INTEGER = SQLT_INT;
  { SQL RAW(N)  OTS RAW(N)  }
     OCI_TYPECODE_RAW = SQLT_LVB;
  { SQL POINTER  OTS POINTER  }
     OCI_TYPECODE_PTR = 32;
     OCI_TYPECODE_VARCHAR2 = SQLT_VCS;
  { SQL VARCHAR2(N)  OTS SQL_VARCHAR2(N)  }
  { SQL CHAR(N)  OTS SQL_CHAR(N)  }
     OCI_TYPECODE_CHAR = SQLT_AFC;
     OCI_TYPECODE_VARCHAR = SQLT_CHR;
  { SQL VARCHAR(N)  OTS SQL_VARCHAR(N)  }
  { OTS MLSLABEL  }
//     OCI_TYPECODE_MLSLABEL = SQLT_LAB;
  { SQL VARRAY  OTS PAGED VARRAY  }
//     OCI_TYPECODE_VARRAY = 247;
  { SQL TABLE  OTS MULTISET  }
//     OCI_TYPECODE_TABLE = 248;
  { SQL/OTS NAMED OBJECT TYPE  }
//     OCI_TYPECODE_OBJECT = SQLT_NTY;
  {  SQL/OTS Opaque Types  }
//     OCI_TYPECODE_OPAQUE = 58;
//     OCI_TYPECODE_NAMEDCOLLECTION = SQLT_NCO;
  { SQL/OTS NAMED COLLECTION TYPE  }
  { SQL/OTS BINARY LARGE OBJECT  }
//     OCI_TYPECODE_BLOB = SQLT_BLOB;
  { SQL/OTS BINARY FILE OBJECT  }
//     OCI_TYPECODE_BFILE = SQLT_BFILE;
  { SQL/OTS CHARACTER LARGE OBJECT  }
//     OCI_TYPECODE_CLOB = SQLT_CLOB;
  { SQL/OTS CHARACTER FILE OBJECT  }
//     OCI_TYPECODE_CFILE = SQLT_CFILE;
  { the following are ANSI datetime datatypes added in 8.1  }
  { SQL/OTS TIME  }
     OCI_TYPECODE_TIME = SQLT_TIME;
  { SQL/OTS TIME_TZ  }
     OCI_TYPECODE_TIME_TZ = SQLT_TIME_TZ;
  { SQL/OTS TIMESTAMP  }
     OCI_TYPECODE_TIMESTAMP = SQLT_TIMESTAMP;
  { SQL/OTS TIMESTAMP_TZ  }
     OCI_TYPECODE_TIMESTAMP_TZ = SQLT_TIMESTAMP_TZ;
  { TIMESTAMP_LTZ  }
     OCI_TYPECODE_TIMESTAMP_LTZ = SQLT_TIMESTAMP_LTZ;
  { SQL/OTS INTRVL YR-MON  }
     OCI_TYPECODE_INTERVAL_YM = SQLT_INTERVAL_YM;
  { SQL/OTS INTRVL DAY-SEC  }
     OCI_TYPECODE_INTERVAL_DS = SQLT_INTERVAL_DS;
  { Urowid type  }
//     OCI_TYPECODE_UROWID = SQLT_RDD;
  { first Open Type Manager typecode  }
//     OCI_TYPECODE_OTMFIRST = 228;
  { last OTM typecode  }
//     OCI_TYPECODE_OTMLAST = 320;
  { first OTM system type (internal)  }
//     OCI_TYPECODE_SYSFIRST = 228;
  { last OTM system type (internal)  }
//     OCI_TYPECODE_SYSLAST = 235;
  { type code for PLS_INTEGER  }
//     OCI_TYPECODE_PLS_INTEGER = 266;
  { the following are PL/SQL-only internal. They should not be used  }
  { PLSQL indexed table  }
//     OCI_TYPECODE_ITABLE = SQLT_TAB;
  { PLSQL record  }
//     OCI_TYPECODE_RECORD = SQLT_REC;
  { PLSQL boolean  }
//     OCI_TYPECODE_BOOLEAN = SQLT_BOL;
  { NOTE : The following NCHAR related codes are just short forms for saying
     OCI_TYPECODE_VARCHAR2 with a charset form of SQLCS_NCHAR. These codes are
     intended for use in the OCIAnyData API only and nowhere else.  }
     OCI_TYPECODE_NCHAR = 286;     
     OCI_TYPECODE_NVARCHAR2 = 287;     
     OCI_TYPECODE_NCLOB = 288;     
  { To indicate absence of typecode being specified  }
     OCI_TYPECODE_NONE = 0;     
  { To indicate error has to be taken from error handle - reserved for
     sqlplus use  }
     OCI_TYPECODE_ERRHP = 283;
  { The OCITypeCode type is interchangeable with the existing SQLT type
     which is a ub2  }

  type

     POCITypeCode = ^OCITypeCode;
     OCITypeCode = ub2;
  {----------------------- GET OPTIONS FOR TDO  ------------------------------ }
  { load only the header portion of the TDO when getting type  }
  { load all attribute and method descriptors as well  }
     OCITypeGetOpt = (OCI_TYPEGET_HEADER,OCI_TYPEGET_ALL);


     POCITypeGetOpt = ^OCITypeGetOpt;
  {
   * OCITypeGetOpt
   *
   * This is the flag passed to OCIGetTypeArray() to indicate how the TDO is
   * going to be loaded into the object cache.
   * OCI_TYPEGET_HEADER implies that only the header portion is to be loaded
   * initially, with the rest loaded in on a 'lazy' basis. Only the header is
   * needed for PL/SQL and OCI operations. OCI_TYPEGET_ALL implies that ALL
   * the attributes and methods belonging to a TDO will be loaded into the
   * object cache in one round trip. Hence it will take much longer to execute,
   * but will ensure that no more loading needs to be done when pinning ADOs
   * etc. This is only needed if your code needs to examine and manipulate
   * attribute and method information.
   *
   * The default is OCI_TYPEGET_HEADER.
    }
  {------------------------ TYPE ENCAPSULTATION LEVEL ------------------------ }
  { 0 = uninitialized  }
  { private: only internally visible  }
  { public: visible to both internally and externally  }
     OCITypeEncap = (OCI_TYPEENCAP_PRIVATE,OCI_TYPEENCAP_PUBLIC
       );


     POCITypeEncap = ^OCITypeEncap;
  {
   * OCITypeEncap - OCI Encapsulation Level
    }
  {---------------------------- TYPE METHOD FLAGS ---------------------------- }
  { inline  }
  { constant  }
  { virtual  }
  { constructor  }
  { destructor  }
  { operator  }
  { selfish method (generic otherwise)  }
  { map (relative ordering)  }
  { order (relative ordering)  }
  { OCI_TYPEMETHOD_MAP and OCI_TYPEMETHOD_ORDER are mutually exclusive  }
  { Read no Data State (default)  }
  { Write no Data State  }
  { Read no Process State  }
  { Write no Process State  }
  { abstract (not instantiable) method  }
  { overriding method  }
  { method is pipelined  }
     OCITypeMethodFlag = (OCI_TYPEMETHOD_INLINE := $0001,OCI_TYPEMETHOD_CONSTANT := $0002,
       OCI_TYPEMETHOD_VIRTUAL := $0004,OCI_TYPEMETHOD_CONSTRUCTOR := $0008,
       OCI_TYPEMETHOD_DESTRUCTOR := $0010,OCI_TYPEMETHOD_OPERATOR := $0020,
       OCI_TYPEMETHOD_SELFISH := $0040,OCI_TYPEMETHOD_MAP := $0080,
       OCI_TYPEMETHOD_ORDER := $0100,OCI_TYPEMETHOD_RNDS := $0200,
       OCI_TYPEMETHOD_WNDS := $0400,OCI_TYPEMETHOD_RNPS := $0800,
       OCI_TYPEMETHOD_WNPS := $1000,OCI_TYPEMETHOD_ABSTRACT := $2000,
       OCI_TYPEMETHOD_OVERRIDING := $4000,OCI_TYPEMETHOD_PIPELINED := $8000
       );


     POCITypeMethodFlag = ^OCITypeMethodFlag;
  { macros to test the type method flags  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_INLINE(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_CONSTANT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_VIRTUAL(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_CONSTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_DESTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_OPERATOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_SELFISH(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_MAP(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_ORDER(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_RNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_WNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_RNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_WNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_ABSTRACT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_OVERRIDING(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_METHOD_IS_PIPELINED(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_INLINE(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_CONSTANT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_VIRTUAL(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_CONSTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_DESTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_OPERATOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_SELFISH(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_MAP(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_ORDER(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_RNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_WNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_RNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_WNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_ABSTRACT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_OVERRIDING(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_IS_PIPELINED(flag : longint) : longint;  

  { macros to set the type method flags  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_INLINE(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_CONSTANT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_VIRTUAL(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_CONSTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_DESTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_OPERATOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_SELFISH(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_MAP(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_ORDER(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_RNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_WNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_RNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_SET_WNPS(flag : longint) : longint;  

  { macros to clear the type method flags  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_INLINE(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_CONSTANT(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_VIRTUAL(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_CONSTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_DESTRUCTOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_OPERATOR(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_SELFISH(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_MAP(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_ORDER(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_RNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_WNDS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_RNPS(flag : longint) : longint;  

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function OCI_TYPEMETHOD_CLEAR_WNPS(flag : longint) : longint;  

  {--------------------------- TYPE PARAMETER MODE --------------------------- }
  { PL/SQL starts this from 0  }
  { in  }
  { out  }
  { in-out  }
  { call by reference (implicitly in-out)  }
  { OUT with NOCOPY modifier  }
  { IN OUT with NOCOPY modifier  }

  type
     OCITypeParamMode = (OCI_TYPEPARAM_IN := 0,OCI_TYPEPARAM_OUT,
       OCI_TYPEPARAM_INOUT,OCI_TYPEPARAM_BYREF,
       OCI_TYPEPARAM_OUTNCPY,OCI_TYPEPARAM_INOUTNCPY
       );


     POCITypeParamMode = ^OCITypeParamMode;
  {-------------------------------- DEFAULTS --------------------------------- }
  { default binary and decimal precision and scale  }
  { no precision specified  }
  { was #define dname def_expr }
  function OCI_NUMBER_DEFAULTPREC : ub1;    

  { was #define dname def_expr }
  function OCI_NUMBER_DEFAULTSCALE : sb1;    

  { no binary/decimal scale specified  }
  { default maximum length for varrays and vstrings (used in sql.bsq)  }

  const
     OCI_VARRAY_MAXSIZE = 4000;     
  { default maximum number of elements for a varray  }
  { default maximum length of a vstring  }
     OCI_STRING_MAXLEN = 4000;     
  {--------------------------------------------------------------------------- }
  { This set of macro is used only in beta2. They should be removed as soon as
   * PLSQL has made the changes of not using these macros.
    }
  { Special duration for allocating memory only. No instance can be allocated
   * given these durations.
    }
//     OCICoherency = OCIRefreshOpt;
  { was #define dname def_expr }
  function OCI_COHERENCY_NONE : OCIRefreshOpt;    

  { was #define dname def_expr }
  function OCI_COHERENCY_NULL : OCIRefreshOpt;    

  { was #define dname def_expr }
  function OCI_COHERENCY_ALWAYS : OCIRefreshOpt;    

//implementation

