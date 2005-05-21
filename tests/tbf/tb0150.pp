{ %fail }
{ This should give out an error since some of the set elements are duplicated
  actually its m_hintdirective which is duplicated.
 }

type
       { Switches which can be changed by a mode (fpc,tp7,delphi) }
       tmodeswitch = (m_none,m_all, { needed for keyword }
         { generic }
         m_fpc,m_objfpc,m_delphi,m_tp7,m_gpc,
         { more specific }
         m_class,               { delphi class model }
         m_objpas,              { load objpas unit }
         m_result,              { result in functions }
         m_string_pchar,        { pchar 2 string conversion }
         m_cvar_support,        { cvar variable directive }
         m_nested_comment,      { nested comments }
         m_tp_procvar,          { tp style procvars (no @ needed) }
         m_repeat_forward,      { repeating forward declarations is needed }
         m_pointer_2_procedure, { allows the assignement of pointers to
                                  procedure variables                     }
         m_autoderef,           { does auto dereferencing of struct. vars }
         m_initfinal,           { initialization/finalization for units }
         m_add_pointer,         { allow pointer add/sub operations }
         m_default_ansistring,  { ansistring turned on by default }
         m_out,                 { support the calling convention OUT }
         m_default_para,        { support default parameters }
         m_hintdirective,       { support hint directives }
         m_duplicate_names      { allow locals/paras to have duplicate names of globals }
       );
       tmodeswitches = set of tmodeswitch;


const
       delphimodeswitches : tmodeswitches=
         [m_delphi,m_all,m_class,m_objpas,m_result,m_string_pchar,
          m_pointer_2_procedure,m_autoderef,m_tp_procvar,m_initfinal,m_default_ansistring,
          m_out,m_default_para,m_hintdirective,m_duplicate_names,m_hintdirective];

Begin
end.
