(* ::Package:: *)

AddSuite[MLedgerTests, journalsTests];


(* ::Subsection:: *)
(*Test journal objects*)


AddSuite[journalsTests, journalObjectsTests];


(* ::Subsubsection::Closed:: *)
(*Journal*)


AddTest[journalObjectsTests, "testIsJournal",
 AssertTrue@Not@IsJournal[1];
 AssertTrue@Not@IsJournal[{}];
 AssertTrue@Not@IsJournal[{1, 2}];
 AssertTrue[IsJournal[Dataset@{}]];
 With[{journalKeys = Keys@CreateJournalEntry[]},
  With[{assoc := <|Thread[journalKeys -> RandomInteger[10, Length@journalKeys]]|>},
   AssertTrue@Not@IsJournal[{assoc}];
   AssertTrue@Not@IsJournal[<|assoc|>]; 

   AssertTrue@IsJournal[Dataset@{assoc}];
   AssertTrue@IsJournal[Dataset@{assoc, assoc}];
   AssertTrue@Not@IsJournal[Dataset@{assoc, Drop[assoc, 3]}];
  ];
 ];
];


AddTest[journalObjectsTests, "testCreateJournal",
 AssertTrue[IsJournal@CreateJournal[]];
 AssertTrue[IsJournal@CreateJournal[{}]];
 AssertTrue[IsJournal@CreateJournal[{CreateJournalEntry[]}]];
 AssertTrue[Not@IsJournal@CreateJournal[{Drop[#, 3] & @ CreateJournalEntry[]}]];
 
 With[{journal = CreateJournal@{<|
   "date" -> "121212", "description" -> "1234",
   "amount" -> 12, "balance" -> 13, "account" -> "test", 
   "currency" -> "SEK", "category" -> ""
   |>}},
  AssertTrue[IsJournal@journal];
  AssertEquals[Dataset, Head@journal];
  AssertEquals["1234", journal[1, "description"]];
 ];
 
 (* Check ids are added *)
 AssertTrue[And@@(KeyExistsQ[#, "id"] & /@ CreateJournal[{CreateJournalEntry[]}])];
];


(* ::Subsubsection::Closed:: *)
(*JournalEntry*)


AddTest[journalObjectsTests, "testCreateJournalEntry",
 AssertEquals[
  <|"date" -> "1-01-01", "description" -> "", "amount" -> 0.`, "balance" -> 0.`,
    "account" -> "", "currency" -> "", "category" -> ""|>,
  CreateJournalEntry[]];
 AssertEquals[
  <|"date" -> "1-01-01", "description" -> "", "amount" -> 0.`, "balance" -> 0.`,
    "account" -> "", "currency" -> "", "category" -> ""|>,
  CreateJournalEntry[{}]];
 
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "Payroll Deposit - HOTEL", 
    "amount" -> 694.81, "balance" -> 695.36,
    "account" -> "testAccount", "currency" -> "USD", "category" -> "Hotel"|>,
  CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
    "testAccount", "USD", "Hotel"]];
    
 (* With extra info, here FITID from BoA to be used in calculating id *) 
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "Payroll Deposit - HOTEL", 
    "amount" -> 694.81, "balance" -> 695.36, "account" -> "testAccount", 
    "currency" -> "USD", "category" -> "Hotel", "FITID"  ->  1234|>,
  CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
    "testAccount", "USD", "Hotel", "FITID"  ->  1234]];
 
 (* Check we can use CreateJournalEntry on JournalEntries *)
 AssertEquals[CreateJournalEntry[], CreateJournalEntry@CreateJournalEntry[]];
 (* Check extra info handled in this case *)
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "Payroll Deposit - HOTEL", 
    "amount" -> 694.81, "balance" -> 695.36, "account" -> "testAccount", 
    "currency" -> "USD", "category" -> "Hotel", 
    "FITID"  ->  1234, "otherExtra" -> "4321"|>,
  CreateJournalEntry[
   <|"date" -> "2003-10-14", "description" -> "Payroll Deposit - HOTEL", 
    "amount" -> 694.81, "balance" -> 695.36, "account" -> "testAccount", 
    "currency" -> "USD", "category" -> "Hotel", 
    "FITID"  ->  1234, "otherExtra" -> "4321"|>]];
];

AddTest[journalObjectsTests, "testCreateJournalEntryTrimAndToString",
 (* Check leading/trailing whitespace removed on strings *)
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "Payroll Deposit - HOTEL", 
    "amount" -> 694.81, "balance" -> 695.36,
    "account" -> "testAccount", "currency" -> "USD", "category" -> "Hotel"|>, 
  CreateJournalEntry[
   {2003, 10, 14}, " Payroll Deposit - HOTEL ", 694.81, 695.36, 
    "testAccount", "USD", " Hotel "]];
 (* Check numerics turned into strings for fields where strings are expected *)
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "51324", 
    "amount" -> 694.81, "balance" -> 695.36,
    "account" -> "43", "currency" -> "USD", "category" -> "12"|>, 
  CreateJournalEntry[
   {2003, 10, 14}, 51324, 694.81, 695.36, 
    43, "USD", 12]];
(* Check this works for associations already on journalEntry-form
    (important when loading data from journal files that contain numeric descriptions) *)
 AssertEquals[
  <|"date" -> "2003-10-14", "description" -> "51324", 
    "amount" -> 694.81, "balance" -> 695.36,
    "account" -> "43", "currency" -> "USD", "category" -> "12"|>, 
  CreateJournalEntry[
   <|"date" -> "2003-10-14", "description" -> 51324, 
    "amount" -> 694.81, "balance" -> 695.36,
    "account" -> 43, "currency" -> "USD", "category" -> 12|>]];
];


AddTest[journalObjectsTests, "testIsJournalEntry",
 AssertTrue[Not@IsJournalEntry[1]];
 AssertTrue[Not@IsJournalEntry[{1}]];
 AssertTrue[Not@IsJournalEntry[<|"a" -> 2|>]];
 AssertTrue[Not@IsJournalEntry@Drop[CreateJournalEntry[],3]];
 AssertTrue[IsJournalEntry@CreateJournalEntry[]];
 AssertTrue[IsJournalEntry@Reverse@CreateJournalEntry[]];
 
 (* Extra values should be allowed *)
 AssertTrue[IsJournalEntry@<|CreateJournalEntry[], "extra key" -> "extra value"|>];
];


(* ::Subsubsection::Closed:: *)
(*SetCategories*)


AddTest[journalObjectsTests, "testSetCategories",
 With[{
  journal = CreateJournal[ 
   (* create blank entries but different amounts to avoid warning addIDs::duplicate *)
    <|CreateJournalEntry[], "amount" -> RandomReal[]|> & /@ {{}, {},\[NonBreakingSpace]{}}],
   categories = {"category1", "category2", "category3"}
  },
  AssertTrue[IsJournal@journal];
  AssertEquals[Length@journal, Length@categories];
  
  AssertTrue[IsJournal@SetCategories[journal, categories]];
  AssertEquals[categories, SetCategories[journal, categories][All, "category"] // Normal];
  
  (* Check equal length required *)
  AssertMessage[SetCategories::length, SetCategories[journal, categories[[;;-2]]]];
  Off[SetCategories::length];
  AssertEquals[SetCategories, Head@SetCategories[journal, categories[[;;-2]]]];
  AssertEquals[SetCategories, Head@SetCategories[journal[[;;-2]], categories]];
  On[SetCategories::length];
 ];
];


(* ::Subsubsection::Closed:: *)
(*ResetIDs*)


AddTest[journalObjectsTests, "testResetIDs",
 With[{journal = CreateJournal[CreateJournalEntry@@@exampleJournalData]},
  Module[{journal2 = Dataset@MapAt[321 &, Normal@journal, {3, "amount"}]},
   AssertTrue@IsJournal@journal2;
   AssertEquals[321, journal2[3, "amount"]];
   (* Check this will be a sharp test *)
   AssertEquals[journal[3, "id"], journal2[3, "id"]];
   AssertTrue@IsJournal@ResetIDs@journal2;
   AssertTrue[journal[3, "id"] != ResetIDs[journal2][3, "id"]];
  ];
 ];
];


(* ::Subsubsection::Closed:: *)
(*AddCalculatedBalances*)


AddTest[journalObjectsTests, "testAddCalculatedBalances",
 With[{journal = CreateJournal[CreateJournalEntry@@@exampleJournalData]},
  With[{withBalances = AddCalculatedBalances[journal, 0.55]},
   AssertTrue[IsJournal@withBalances];
   AssertTrue[And @@ (KeyExistsQ[#, "calcBalance"] & /@ withBalances)];
   
   (* Check balance and calcBalance adjacent *)
   AssertTrue[And @@ (
    Function[{row}, {{-1}} === 
     Subtract@@(Position[Keys@row, #] & /@ {"balance", "calcBalance"})] /@ 
      withBalances)
   ];
   
   (* Check calculation *)
   AssertEqualsN[
    Normal@withBalances[All, "balance"], 
    Normal@withBalances[All, "calcBalance"]
   ];
   
   (* Check calcBalance recalculated *)
   With[{withBalances2 = AddCalculatedBalances[withBalances, 1 + 0.55]},
    AssertTrue[IsJournal@withBalances2];
    AssertEqualsN[
     1 + Normal@withBalances2[All, "balance"], 
     Normal@withBalances2[All, "calcBalance"]
    ];
   ];
  ];
 ];
];


AddTest[journalObjectsTests, "testAddCalculatedBalancesRounding",
 Module[{amounts, jEntries, journal},
  amounts = 
   {3003.1, -13.68, -1161.04, -1126., 7150.15, -25., -1212., -106.82, -80., 
    -5000., -16., 7248., -1200., -1200., 2400.};
  jEntries = CreateJournalEntry@@@exampleJournalData[[;; Length@amounts]];
  jEntries[[All, "amount"]] = amounts;
  journal = AddCalculatedBalances[CreateJournal@jEntries, 0];
  AssertEquals[
   "8660.71,5657.61,5671.29,6832.33,7958.33,808.18,833.18,2045.18,2152.," <> 
     "2232.,7232.,7248.,0.,1200.,2400.\n",
   ExportString[journal[[All,"calcBalance"]], "CSV"]
  ];
 ];
];


(* ::Subsubsection::Closed:: *)
(*TakeCategorized*)


AddTest[categorizationPredictionTestsInternal, "testTakeCategorized",
 With[{exampleJournal = CreateJournal[CreateJournalEntry@@@exampleJournalData]},
  AssertEquals[TakeCategorized, Head@TakeCategorized@{1}];
  AssertEquals[{}, TakeCategorized@{}];
  
  AssertTrue@IsJournal@exampleJournal;
  AssertEquals[Length@DeleteCases[categoriesForExampleJournal, ""], 
   Length@TakeCategorized@exampleJournal];
 ];
];

AddTest[categorizationPredictionTestsInternal, "testTakeUncategorized",
 With[{exampleJournal = CreateJournal[CreateJournalEntry@@@exampleJournalData]},
  AssertEquals[TakeUncategorized, Head@TakeUncategorized@{1}];
  AssertEquals[{}, TakeUncategorized@{}];

  AssertTrue@IsJournal@exampleJournal;
  AssertEquals[Count[categoriesForExampleJournal, ""], 
   Length@TakeUncategorized@exampleJournal];
 ];
];


(* ::Subsubsection::Closed:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[journalObjectsTests, journalObjectsTestsInternal]
AddTest[journalObjectsTestsInternal, "testAddIDs",
 AssertEquals[<|CreateJournalEntry[], "id"->119425892664861902967123551446002477177|>,
  addID@CreateJournalEntry[]];
 AssertEquals[<|CreateJournalEntry[], "id"->119425892664861902967123551446002477177|>,
  addID@addID@CreateJournalEntry[]];
  
 AssertMessage[
  addIDs::duplicate,
  addIDs@Dataset@{CreateJournalEntry[], CreateJournalEntry[]}];
  
 AssertTrue[
  And@@(KeyExistsQ[#, "id"] & /@ addIDs@Dataset[{CreateJournalEntry[]}])];
  
 (* Check FITID is used when present *)
 With[{entry = CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
    "testAccount", "USD", "Hotel", "FITID" -> 1234]},
  (* Without FITID *)
  AssertEquals[209301251017031479487551913485260210694, 
   addID[KeyDrop[entry, "FITID"]]["id"]];
  (* With FITID *)
  AssertEquals[57290321703453810724647628122176071905, 
   addID[entry]["id"]];
 ];
 
 (* Check correctly considers equal entries differing in keys not supposed to go 
     into hash calculation *)
 Module[{entry1, entry2},
  entry1 = CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
   "testAccount", "USD", "Hotel"];
  entry2 = entry1; entry2[["balance"]] = 0.;
  AssertTrue@IsJournalEntry@entry2;
  AssertEquals[addID[entry2]["id"], addID[entry1]["id"]];
  entry2 = entry1; entry2[["category"]] = "not Hotel";
  AssertTrue@IsJournalEntry@entry2;
  AssertEquals[addID[entry2]["id"], addID[entry1]["id"]];
 ];
];
End[]; (* End "MLedger`Private`" *)


(* ::Subsection:: *)
(*Test journal file handling*)


AddSuite[journalsTests, journalFileHandlingTests];


(* ::Subsubsection::Closed:: *)
(*Setup/Teardown*)


AddTest[journalFileHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp642/";
 SetJournalDir[testDirTemp];
 
 exampleJournal = CreateJournal[CreateJournalEntry@@@exampleJournalData];
 (* Create a journal with two different years and accounts interleaved *)
 exampleJournal2 = CreateJournal[CreateJournalEntry@@@exampleJournalData2];
];

AddTest[journalFileHandlingTests, "Tear Down", 
 ClearAll[testDirTemp, exampleJournal, exampleJournal2];
 SetJournalDir[""];
];


(* ::Subsubsection::Closed:: *)
(*Get/SetJournalDir*)


AddTest[journalFileHandlingTests, "testGetSetJournalDir",
 AssertEquals[testDirTemp, GetJournalDir[]];
 SetJournalDir["dir"];
 AssertEquals["dir", GetJournalDir[]];
];


(* ::Subsubsection::Closed:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[journalFileHandlingTests, journalFileHandlingTestsInternal];

AddTest[journalFileHandlingTestsInternal, "testGetJournalYear",
 AssertEquals[2003, getJournalYear@exampleJournal];
 AssertEquals[False, getJournalYear@exampleJournal2];
];

AddTest[journalFileHandlingTestsInternal, "testGetJournalAccount",
 AssertEquals["BoA Checking", getJournalAccount@exampleJournal];
 AssertEquals[False, getJournalAccount@exampleJournal2];
];

AddTest[journalFileHandlingTestsInternal, "testFormatJournalFilename",
 AssertEquals[GetJournalDir[] <> "BoA Checking/2003.csv", 
  formatJournalFilename@exampleJournal];
 AssertEquals[False, formatJournalFilename@exampleJournal2];
];

AddTest[journalFileHandlingTestsInternal, "testSplitJournalByAccount",
 With[{journals = splitJournalByAccount@exampleJournal2},
  AssertEquals[2, Length@journals];
  AssertTrue[And @@ (IsJournal /@ journals)];
  AssertEquals[{"BoA Checking", "BoA Savings"}, getJournalAccount /@ journals];
 ];
];
AddTest[journalFileHandlingTestsInternal, "testSplitJournalByYear",
 With[{journals = splitJournalByYear@exampleJournal2},
  AssertEquals[2, Length@journals];
  AssertTrue[And @@ (IsJournal /@ journals)];
  AssertEquals[{2003, 2004}, getJournalYear /@ journals];
 ];
];

AddTest[journalFileHandlingTestsInternal, "testMergeJournals",
 With[{mergedJournal = mergeJournals[exampleJournal, exampleJournal2]},
  AssertTrue[IsJournal@mergedJournal];
  AssertEquals[30, Length@mergedJournal];
  AssertTrue[DuplicateFreeQ@mergedJournal];
  
  (* Check sorted by date *)
  AssertEquals[Normal@mergedJournal[All, "date"], 
   Reverse@SortBy[DateList[#]&]@Normal@mergedJournal[All, "date"]]
 ];
];

End[]; (* End "MLedger`Private`" *)


(* ::Subsubsection::Closed:: *)
(*Read/WriteToJournal*)


(* ::Text:: *)
(*Separate subsuite for tests needing setup/teardown of directories to avoid interacting with file system with rest of tests.*)


AddSuite[journalFileHandlingTests, readWriteJournalTests];


AddTest[readWriteJournalTests, "Set Up", 
 EnsureDirectoryExists[testDirTemp];
];

AddTest[readWriteJournalTests, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];


AddTest[readWriteJournalTests, "testReadWriteJournal",
 AssertTrue[FileExistsQ@testDirTemp];
 AssertTrue[IsJournal@exampleJournal2];
 
 AssertEquals[{}, Normal@ReadJournal["BoA Savings", 2004]];
 
 AssertTrue[Not@FileExistsQ[GetJournalDir[] <> "BoA Checking/2003.csv"]];
 WriteToJournal[exampleJournal2];
 (* Check properly split by account and year *)
 AssertTrue[FileExistsQ[GetJournalDir[] <> "BoA Checking/2003.csv"]];
 AssertTrue[FileExistsQ[GetJournalDir[] <> "BoA Checking/2004.csv"]];
 AssertTrue[FileExistsQ[GetJournalDir[] <> "BoA Savings/2003.csv"]];
 AssertTrue[FileExistsQ[GetJournalDir[] <> "BoA Savings/2004.csv"]];
 
 (* ReadJournal[account, year] *)
 With[{journalRead = ReadJournal["BoA Savings", 2004]},
   AssertTrue[IsJournal@journalRead];
   AssertEquals[3, Length@journalRead];
   AssertEquals["BoA Savings", Normal@journalRead[[1, "account"]]];
   AssertEquals["2004-10-20", Normal@journalRead[[3, "date"]]];
  ];
 (* ReadJournal[journal] *)
 With[{journalRead = ReadJournal[exampleJournal2[[2;;2]]]},
   AssertTrue[IsJournal@journalRead];
   AssertEquals[5, Length@journalRead];
   AssertEquals["BoA Checking", Normal@journalRead[[1, "account"]]];
   AssertEquals["2003-10-27", Normal@journalRead[[3, "date"]]];
  ];
 (* ReadJournal[account] *)
 With[{journalRead = ReadJournal["BoA Checking"]},
   AssertTrue[IsJournal@journalRead];
   AssertEquals[11, Length@journalRead];
   AssertEquals["BoA Checking", Normal@journalRead[[1, "account"]]];
   AssertEquals["2004-10-24", Normal@journalRead[[3, "date"]]];
   AssertEquals["2003-10-27", Normal@journalRead[[-3, "date"]]];
  ];
 
 (* ReadJournal[year], requires setting up accounts *)
 SetBankAccounts[{<|"name" -> "BoA Checking"|>, <|"name" -> "BoA Savings"|>}];
 With[{journalRead = ReadJournal[2003]},
   AssertTrue[IsJournal@journalRead];
   AssertEquals[8, Length@journalRead];
   AssertEquals["BoA Checking", Normal@journalRead[[1, "account"]]];
   AssertEquals["BoA Savings", Normal@journalRead[[2, "account"]]];
   AssertEquals["2003-10-30", Normal@journalRead[[3, "date"]]];
   AssertEquals["2003-10-21", Normal@journalRead[[-3, "date"]]];
  ];
  
 (* Check writing into journal, not overwriting *)
 WriteToJournal[exampleJournal];
 With[{journalRead = ReadJournal["BoA Checking", 2003]},
  AssertTrue[IsJournal@journalRead];
  AssertEquals[18, Length@journalRead];
  AssertEquals["BoA Checking", Normal@journalRead[[1, "account"]]];
  AssertEquals[{-5., -5., 123}, Sort@Normal@journalRead[[;;3, "amount"]]];
 ];
];


(* ::Subsubsection::Closed:: *)
(*ListAccountsWithJournals*)


AddTest[readWriteJournalTests, "testListAccountsWithJournals",
 SetBankAccounts[{}];
 AssertEquals[{}, ListAccountsWithJournals[]];
 
 AssertTrue[FileExistsQ@testDirTemp];
 AssertTrue[IsJournal@exampleJournal2];
 WriteToJournal[exampleJournal2];
 
 (* Check warning before accounts set up *)
 AssertMessage[ListAccountsWithJournals::extraFiles, ListAccountsWithJournals[]];
 AssertEquals[{}, 
  Quiet[ListAccountsWithJournals[], ListAccountsWithJournals::extraFiles]];
 
 SetBankAccounts[{<|"name" -> "BoA Checking"|>, <|"name" -> "BoA Savings"|>}];
 AssertEquals[{"BoA Checking", "BoA Savings"}, Sort@ListAccountsWithJournals[]];
 
 (* Check no warning on hidden files *)
 With[{hiddenFile = GetJournalDir[] <> ".hidden"},
  CreateFile@hiddenFile;
  AssertTrue@FileExistsQ@hiddenFile;
  AssertNoMessage[ListAccountsWithJournals[]];
 ];
];
