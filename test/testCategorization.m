(* ::Package:: *)

AddSuite[MLedgerTests, categorizationTests];


(* ::Subsection:: *)
(*Test categorization form*)


AddSuite[categorizationTests, categorizationFormTests];


AddTest[categorizationFormTests, "Set Up", 
 exampleJournal = CreateJournal[CreateJournalEntry@@@exampleJournalData];
];

AddTest[categorizationFormTests, "Tear Down", 
 ClearAll@exampleJournal
];


AddTest[categorizationFormTests, "testCategorizationForm",
 AssertTrue[IsJournal@exampleJournal];
 AssertEquals[17, Length@exampleJournal];
 With[{form = CategorizationForm@exampleJournal},
  AssertEquals[Length@exampleJournal, Length@ExtractSelectedCategories@form];
  AssertMatch[
   Labeled[Grid[
    {{__, PopupMenu[Dynamic[_], __],  InputField[Dynamic[_], String, __]}..}], ___],
   form
  ];
 ];
];


AddTest[categorizationFormTests, "testExtractSelectedCategories",
 AssertTrue[IsJournal@exampleJournal];
 AssertEquals[17, Length@exampleJournal];
 With[{form = CategorizationForm@exampleJournal},
  (* Test for names of variables extracted, as the dynamic part is too hard to test for *)
  AssertEquals[Length@exampleJournal, Length@ExtractSelectedCategories@form];
  AssertTrue[
   And@@(StringMatchQ[ToString@#, "category" ~~ (DigitCharacter..)] & /@ 
    ExtractSelectedCategories@form)
  ];
 ];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[categorizationFormTests, categorizationFormTestsInternal]
AddTest[categorizationFormTestsInternal, "testCategorizationFormRow",
 With[{exampleEntry = Normal@First@exampleJournal},
  AssertTrue[IsJournalEntry@exampleEntry];
  AssertEquals[categorizationFormRow, 
   Head@categorizationFormRow[Drop[exampleEntry, 1]]];
  AssertMatch[ 
   {exampleEntry[["date"]], exampleEntry[["description"]], 
    exampleEntry[["amount"]], 
    PopupMenu[Dynamic[_], __],  InputField[Dynamic[_], String, __]
   },
   categorizationFormRow@exampleEntry];
]];
End[]; (* End "MLedger`Private`" *)
