{$SMARTLINK OFF}

{$if declared(indirect_bootstrap)}
{$define FPC_BOOTSTRAP_INDIRECT_ENTRY}
{$endif declared(indirect_bootstrap)}

{$if not defined(FPC_BOOTSTRAP_INDIRECT_ENTRY) and not defined(FPC_HAS_INDIRECT_ENTRY_INFORMATION)}
var
  initialstkptr: pointer; public name '__stkptr';
  operatingsystem_parameter_envp: ppchar; public name 'operatingsystem_parameter_envp';
  operatingsystem_parameter_argc: longint; public name 'operatingsystem_parameter_argc';
  operatingsystem_parameter_argv: ppchar; public name 'operatingsystem_parameter_argv';
{$elseif defined(FPC_BOOTSTRAP_INDIRECT_ENTRY)}
var
  initialstkptr: pointer; external name '__stkptr';
  operatingsystem_parameter_envp: ppchar; external name 'operatingsystem_parameter_envp';
  operatingsystem_parameter_argc: longint; external name 'operatingsystem_parameter_argc';
  operatingsystem_parameter_argv: ppchar; external name 'operatingsystem_parameter_argv';
{$endif not FPC_BOOTSTRAP_INDIRECT_ENTRY and not FPC_HAS_INDIRECT_ENTRY_INFORMATION}
