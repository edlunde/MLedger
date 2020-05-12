(* ::Package:: *)

AddSuite[MLedgerTests, commonTests];


(* ::Subsection::Closed:: *)
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

AddTest[datesTests, "testSplitByMonthYear",
 With[{table = {
  <|"a" -> 6, "b" -> 2, "date" -> "20140201"|>, 
  <|"a" -> 3, "date" -> "20130101", "c" -> 4|>,
  <|"date" -> "20140101", "d" -> "20140201", "b" -> 5|>}},
  AssertTrue[And@@(KeyExistsQ[#, "date"]& /@ table)];
  
  AssertEquals[<|2013 -> table[[{2}]], 2014 -> table[[{1,3}]]|>, 
   splitByYear@table];
   
  AssertEquals[<|2013 -> Dataset, 2014 -> Dataset|>, 
   Head /@ splitByYear@Dataset@table];
  AssertEquals[<|2013 -> table[[2;;2]], 2014 -> table[[{1,3}]]|>, 
   Normal /@ splitByYear@Dataset@table];
   
  AssertEquals[
   <|{2013, 1} -> table[[{2}]], {2014, 1} -> table[[{3}]], {2014, 2} -> table[[{1}]]|>, 
   splitByMonthAndYear@table];
   
  AssertEquals[<|{2013, 1} -> Dataset, {2014, 1} -> Dataset, {2014, 2} -> Dataset|>, 
   Head /@ splitByMonthAndYear@Dataset@table];
  AssertEquals[
   <|{2013, 1} -> table[[{2}]], {2014, 1} -> table[[{3}]], {2014, 2} -> table[[{1}]]|>, 
   Normal /@ splitByMonthAndYear@Dataset@table];
 ];
];

End[];(* MLedger`Private` *)


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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

AddTest[directoryHandlingTests, "testSetDataDirectories",
 With[{dir = testDirTemp <> "tempDir/"},
  AssertEquals[{"Balances", "Journals", "Ledger"}, 
   Sort[FileNameTake /@ SetDataDirectories[dir]]];
  AssertEquals[dir <> "Journals/", GetJournalDir[]];
  AssertEquals[dir <> "Ledger/", GetLedgerDir[]];
  AssertEquals[dir <> "Balances/", GetBalancesDir[]];
 ];
];
