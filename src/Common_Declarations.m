(* ::Package:: *)

(* ::Subsection:: *)
(*Directory handling*)


EnsureDirectoryExists::usage = 
 "EnsureDirectoryExists[dir] creates dir if it does not exist.";


SetDataDirectories::usage = "SetDataDirectories[dir] set the various data directories \
(using SetJournalDir etc.) to default names with dir as root (so GetJournalDir[] will \
return dir/Journals/ etc. afterwards).";


(* ::Subsection:: *)
(*Data structure functions*)


HasKeysQ::usage = 
 "HasKeysQ[assoc, keys] returns True if every key in keys exists among the keys of\[NonBreakingSpace]\
assoc. Also works if assoc is a list of rules.";
