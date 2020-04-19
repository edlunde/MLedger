(* ::Package:: *)

AddSuite[MLedgerTests, commonTests];


(* ::Subsection:: *)
(*Test dates*)


AddSuite[commonTests, datesTests];


Begin["MLedger`Private`"];
AddTest[datesTests, "testToDateString",
 AssertEquals["2004-01-01", toDateString[{2004, 1, 1, 0, 0, 0}]];
 AssertEquals["2004-01-01", toDateString["2004-01-01"]];
];

AddTest[datesTests, "testSortByDateDescending",
 AssertEquals[sortByDateDescending, 
  Head@sortByDateDescending@{<|"date" -> "2004-01-01"|>, <|"notdate" -> "2004-14-99"|>}];
 With[{list = {
   <|"date" -> "2004-01-01", "value" -> 1|>, 
   <|"date" -> "2003-11-01", "value" -> 2|>, 
   <|"date" -> "2004-01-01", "value" -> 3|>
   }},
  (* Check order maintained for entries with identical date *)
  AssertEquals[{1, 3, 2}, sortByDateDescending[list][[All, "value"]]];
  AssertEquals[{1, 3, 2}, Normal@sortByDateDescending[Dataset@list][[All, "value"]]];
  AssertTrue[Dataset, Head@sortByDateDescending[Dataset@list]]
 ];
];

End[];(* MLedger`Private` *)


(* ::Subsection:: *)
(*Test data structure functions*)


AddSuite[commonTests, dataStructTests];


AddTest[dataStructTests, "testHasKeysQ",
 Module[{keys = {"a", "b"}, assoc, list},
  assoc = <| "a" -> 1, "b" -> 3, "c" -> 2 |>;
  AssertTrue@HasKeysQ[assoc, keys];
  assoc = <| "a" -> 1, "c" -> 2 |>;
  AssertTrue@Not@HasKeysQ[assoc, keys];

  list = {"a" -> 1,  "b" -> 3};
  AssertTrue@HasKeysQ[list, keys];
  list = {"a" -> 1, {"b", 3}};
  AssertTrue@Not@HasKeysQ[list, keys];
 ];
];


(* ::Subsection:: *)
(*Test directory handling*)


AddSuite[commonTests, directoryHandlingTests];


AddTest[directoryHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp123/";
 EnsureDirectoryExists[testDirTemp];
];

AddTest[directoryHandlingTests, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];

AddTest[directoryHandlingTests, "testEnsureDirectoryExists",
 With[{dir = testDirTemp <> "tempDir/"},
  AssertTrue[Not@FileExistsQ[dir]];
  EnsureDirectoryExists[dir];
  AssertTrue[FileExistsQ[dir]];
 ];
];
