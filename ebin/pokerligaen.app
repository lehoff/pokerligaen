%%% -*- mode:erlang -*-
{application, pokerligaen,
 [
  {description, "Set of tools to run the PokerLigaen point system."},
  {vsn, "0.0.1"},
  {modules,
   [ pot_ds
   ]},

  {registered, []},

  {applications,
   [ kernel,
     stdlib,
     sasl]},

  {included_applications, []},
  
  {env, []}
 ]
}.
