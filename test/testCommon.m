(* ::Package:: *)

AddSuite[MLedgerTests, commonTests];


(* ::Subsection:: *)
(*Test dates*)


AddSuite[commonTests, datesTests];


AddTest[datesTests, "testToDateString",
 AssertEquals["2004-01-01", MLedger`Private`toDateString[{2004, 1, 1, 0, 0, 0}]];
 AssertEquals["2004-01-01", MLedger`Private`toDateString["2004-01-01"]];
];


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
