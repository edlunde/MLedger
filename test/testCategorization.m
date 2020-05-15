(* ::Package:: *)

AddSuite[MLedgerTests, categorizationTests];


(* ::Subsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsection:: *)
(*Test categorization prediction*)


AddSuite[categorizationTests, categorizationPredictionTests];


AddTest[categorizationPredictionTests, "Set Up", 
 exampleJournal = SetCategories[
  CreateJournal[CreateJournalEntry@@@exampleJournalData], 
  categoriesForExampleJournal];
];

AddTest[categorizationFormTests, "Tear Down", 
 ClearAll@exampleJournal
];


AddTest[categorizationPredictionTests, "testTrainCategoryClassifier",
 AssertTrue@IsJournal@exampleJournal;
 With[{categories = Union@DeleteCases[categoriesForExampleJournal, ""],
   classifierFunction = TrainCategoryClassifier[exampleJournal]},
  AssertEquals[ClassifierFunction, Head@classifierFunction];
  AssertEquals[categories, 
   Sort@Information[classifierFunction, "Classes"]
   ];
 ];
];


(*Cases[FullForm@classifierFunction,
"Index" \[Rule]  {<|"Banking fees" -> 1, "Electronics" -> 2, "Groceries" -> 3, 
   "Insurance" -> 4, "Internal" -> 5, "Mortgage" -> 6, "Salary" -> 7|>},
All]*)


(* ::Subsubsection::Closed:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[categorizationPredictionTests, categorizationPredictionTestsInternal]

AddTest[categorizationPredictionTestsInternal, "testFeatureKeys",
 AssertEquals[{"account", "amount", "description"}, Sort@featureKeys[]];
];

AddTest[categorizationPredictionTestsInternal, "testFormatTrainingData",
 AssertTrue@IsJournal@exampleJournal;
 AssertMatch[{({__} -> _)..}, formatTrainingData[TakeCategorized@exampleJournal]];
 AssertEquals[-710.49, formatTrainingData[TakeCategorized@exampleJournal][[3, 1, 2]]];
];
End[]; (* End "MLedger`Private`" *)
