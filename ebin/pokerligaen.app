%%% -*- mode:erlang -*-
{application, pokerligaen,
 [
  {description, "Set of tools to run the PokerLigaen point system."},
  {vsn, "0.0.1"},
  {modules,
   [ pot_ds,
     pl_round,
     pl_night,
     pl_results,
     pl_scores,
     pl_print,
     pl_init,
     pl_tally
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
