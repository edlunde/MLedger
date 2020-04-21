(* ::Package:: *)

AddSuite[MLedgerTests, balancesTests];


(* ::Subsection::Closed:: *)
(*Test balances objects*)


AddSuite[balancesTests, testBalancesObject];


AddTest[testBalancesObject, "testIsAccountBalances",
 AssertTrue[Not@IsAccountBalances[1]];
 AssertTrue[Not@IsAccountBalances[{1}]];
 AssertTrue[Not@IsAccountBalances[{<|"a" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"account" -> 1, "balance" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"account" -> 1, "currency" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{
  <|"account" -> 1, "balance" -> 1|>,
  <|"account" -> 3, "balance" -> 2, "currency" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{}]];
 
 AssertTrue[IsAccountBalances[{<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[IsAccountBalances[{
  <|"account" -> 1, "balance" -> 1, "currency" -> 1, "extra" -> {}|>,
  <|"account" -> 3, "balance" -> 2, "currency" -> 1|>}]];
];

AddTest[testBalancesObject, "testIsBalances",
 AssertTrue[Not@IsBalances[1]];
 AssertTrue[Not@IsBalances[<|"a" -> 3|>]];
 AssertTrue[Not@IsBalances[<|"date" -> 3, "accountBalances" -> 2|>]];
 AssertTrue[Not@IsBalances[<|"date" -> 3, "accountBalances" -> {}|>]];
 
 AssertTrue[IsBalances[
  <|"date" -> 3, 
    "accountBalances" -> {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}|>]];
];


AddTest[testBalancesObject, "testCreateBalancesObject",
 AssertTrue[Not@IsBalances@CreateBalancesObject[1]];
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", 2]];
 AssertTrue[KeyExistsQ[#, "date"] & @ 
  CreateBalancesObject["1", {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[KeyExistsQ[#, "accountBalances"] & @ 
  CreateBalancesObject["1", {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> 1|>]];
 SetBankAccounts[{}];
 AddBoAAccount@"acc1"; AddNordeaAccount@"acc2"; 
 AssertEquals[{"acc1", "acc2"}, ListBankAccounts[]];
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> 1, "acc3" -> 2|>]];

 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> "1", "acc2" -> 2|>]];
 
 AssertTrue[KeyExistsQ[#, "date"] & @ CreateBalancesObject["1", <|"acc1" -> 1, "acc2" -> 3|>]];
 AssertTrue[KeyExistsQ[#, "accountBalances"] & @ CreateBalancesObject["1", <|"acc1" -> 1|>]];
];


(* ::Subsection:: *)
(*Test balances file handling*)


AddSuite[balancesTests, balancesFileHandlingTests];


AddTest[balancesFileHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp642/";
 SetBalancesDir[testDirTemp];
 
 exampleBalances = CreateBalancesObject["2003-10-09", {<|"account"->"Example BoA account" ,"balance"->0.55,"currency"->"USD"|>,<|"account"->"Example Nordea account" ,"balance"->0.55,"currency"->"SEK" |>}];
];

AddTest[balancesFileHandlingTests, "Tear Down", 
 ClearAll[testDirTemp, exampleBalances];
 SetBalancesDir[""];
];


AddTest[balancesFileHandlingTests, "testGetSetBalancesDir",
 AssertEquals[testDirTemp, GetBalancesDir[]];
 SetBalancesDir["dir"];
 AssertEquals["dir", GetBalancesDir[]];
];


(* ::Subsubsection::Closed:: *)
(*Read/WriteToBalances*)


AddSuite[balancesFileHandlingTests, readWriteBalancesTests];


AddTest[readWriteBalancesTests, "Set Up", 
 EnsureDirectoryExists[testDirTemp];
];

AddTest[readWriteBalancesTests, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];


AddTest[readWriteBalancesTests, "testReadWriteBalances",
 AssertTrue[FileExistsQ@GetBalancesDir[]];
 AssertTrue[IsBalances@exampleBalances];
 
 AssertMessage[Import::nffil, ReadBalances[exampleBalances["date"]]];
 
 AssertTrue[Not@FileExistsQ[GetBalancesDir[] <> exampleBalances["date"] <> ".csv"]];
 WriteToBalances[exampleBalances];
 AssertTrue[FileExistsQ[GetBalancesDir[] <> exampleBalances["date"] <> ".csv"]];
 
 With[{balancesRead = ReadBalances[exampleBalances["date"]]},
   AssertTrue[IsBalances@balancesRead];
   AssertEquals[2, Length@balancesRead["accountBalances"]];
   AssertEquals["Example BoA account", balancesRead[["accountBalances", 1, "account"]]];
   AssertEquals[0.55, balancesRead[["accountBalances", 2, "balance"]]];
  ];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[balancesFileHandlingTests, balancesFileHandlingTestsInternal];

AddTest[balancesFileHandlingTestsInternal, "Set Up", 
 EnsureDirectoryExists[testDirTemp];
];

AddTest[balancesFileHandlingTestsInternal, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];

AddTest[balancesFileHandlingTestsInternal, "testGetPrecedingBalances",
 AssertEquals[{}, getPrecedingBalances@"2003-12-31"];
 
 AssertTrue[FileExistsQ@GetBalancesDir[]];
 AssertTrue[Not@FileExistsQ@formatBalancesFilename@exampleBalances["date"]];
 WriteToBalances[exampleBalances];
 AssertTrue[FileExistsQ@formatBalancesFilename@exampleBalances["date"]];
 
 AssertEquals[exampleBalances[["date"]], getPrecedingBalancesDate@"2003-12-31"];
 AssertEquals[exampleBalances, getPrecedingBalances@"2003-12-31"];
 
 (* Check right date chosen *)
 With[{balances2 = MapAt["2003-12-10"&, exampleBalances, "date"]},
  AssertEquals["2003-12-10", balances2[["date"]]];
  AssertTrue[Not@FileExistsQ@formatBalancesFilename@balances2["date"]];
  WriteToBalances[balances2];
  AssertTrue[FileExistsQ@formatBalancesFilename@balances2["date"]];
 
  AssertEquals[balances2[["date"]], getPrecedingBalancesDate@"2003-12-31"];
  AssertEquals[balances2, getPrecedingBalances@"2003-12-31"];
 ];
];

End[]; (* End "MLedger`Private`" *)
