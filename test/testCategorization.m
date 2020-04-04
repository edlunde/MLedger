(* ::Package:: *)

AddSuite[MLedgerTests, categorizationTests];


(* ::Subsection:: *)
(*Test journal objects*)


AddSuite[categorizationTests, categorizationFormTests];


(* ::Subsubsection:: *)
(*Journal*)


AddTest[categorizationFormTests, "testCategorizationForm",
 1
];


(* ::Subsubsection:: *)
(*Internal tests*)


(*Begin["MLedger`Private`"];
AddSuite[categorizationFormTests, categorizationFormTestsInternal]
AddTest[categorizationFormTestsInternal, "test",
 1
];
End[]; (* End "MLedger`Private`" *)*)
