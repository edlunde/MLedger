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
  <|"date"->"1-01-01", "description"->"", "amount"->0.`, "balance"->0.`,
    "account"->"", "currency"->"", "category"->""|>,
  CreateJournalEntry[]];
 AssertEquals[
  <|"date"->"1-01-01", "description"->"", "amount"->0.`, "balance"->0.`,
    "account"->"", "currency"->"", "category"->""|>,
  CreateJournalEntry[{}]];
 
 AssertEquals[
  <|"date"->"2003-10-14", "description"->"Payroll Deposit - HOTEL", 
    "amount"->694.81, "balance"->695.36,
    "account"->"testAccount", "currency"->"USD", "category"->"Hotel"|>,
  CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
    "testAccount", "USD", "Hotel"]];
    
 (* With extra info, here FITID from BoA to be used in calculating id *) 
 AssertEquals[
  <|"date"->"2003-10-14", "description"->"Payroll Deposit - HOTEL", 
    "amount"->694.81, "balance"->695.36,
    "account"->"testAccount", "currency"->"USD", "category"->"Hotel", "FITID" -> 1234|>,
  CreateJournalEntry[
   {2003, 10, 14}, "Payroll Deposit - HOTEL", 694.81, 695.36, 
    "testAccount", "USD", "Hotel", "FITID" -> 1234]];
 
 (* Check we can use CreateJournalEntry on JournalEntries *)
 AssertEquals[CreateJournalEntry[], CreateJournalEntry@CreateJournalEntry[]];
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
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[journalObjectsTests, journalObjectsTestsInternal]
AddTest[journalObjectsTestsInternal, "testAddIDs",
 AssertEquals[<|CreateJournalEntry[], "id"->28496656361855733621011000650886999167|>,
  addID@CreateJournalEntry[]];
 AssertEquals[<|CreateJournalEntry[], "id"->28496656361855733621011000650886999167|>,
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
  AssertEquals[43520287855825489889128932694485169284, 
   addID[KeyDrop[entry, "FITID"]]["id"]]; (* Without FITID *)
  AssertEquals[189748608173027501958982806065933508219, 
   addID[entry]["id"]]; (* With FITID *)
 ];
];
End[]; (* End "MLedger`Private`" *)


(* ::Subsection:: *)
(*Test journal file handling*)


AddSuite[journalsTests, journalFileHandlingTests];


AddTest[journalFileHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp642/";
 SetJournalDir[testDirTemp];
 
 exampleJournal = CreateJournal@{<|"date"->"2003-11-08","description"->"Fees - Monthly","amount"->-5.`,"balance"->-72.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->274356663192601122998088388993363735798|>,<|"date"->"2003-11-07","description"->"Fees - Overdraft","amount"->-5.`,"balance"->-62.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84979730638829994080353479076932943230|>,<|"date"->"2003-11-06","description"->"Mortgage Payment","amount"->-710.49`,"balance"->648.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->88868563507080916278564744628206662059|>,<|"date"->"2003-11-03","description"->"Pre-Auth. Payment - INSURANCE","amount"->-33.55`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->91664361405917894742960028992867906147|>,<|"date"->"2003-11-03","description"->"Cheque No. - 409","amount"->-100.`,"balance"->748.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->178122922470972405198691130574520500688|>,<|"date"->"2003-10-30","description"->"Web Funds Transfer - From SAVINGS","amount"->50.`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->241118077855012299614093250593762684684|>,<|"date"->"2003-10-28","description"->"Payroll Deposit - HOTEL","amount"->731.57`,"balance"->731.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->249827740959480498780368837530154740381|>,<|"date"->"2003-10-27","description"->"Telephone Bill Payment - VISA","amount"->-6.77`,"balance"->36.76`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->93485259797755573237552240030516042590|>,<|"date"->"2003-10-24","description"->"Interac Refund - ELECTRONICS","amount"->2.99`,"balance"->43.53`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->177283377122706640434729207919340396612|>,<|"date"->"2003-10-23","description"->"Interac Purchase - SUPERMARKET","amount"->-29.08`,"balance"->40.54`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->306543770689891693646728945877615865925|>,<|"date"->"2003-10-22","description"->"ATM Withdrawal - FIRST BANK","amount"->-100.`,"balance"->69.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->62939956412697090788141371309312500050|>,<|"date"->"2003-10-21","description"->"Web Bill Payment - AMEX","amount"->-300.`,"balance"->169.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->277055912645424638284676796424185571315|>,<|"date"->"2003-10-20","description"->"Interac Purchase - ELECTRONICS","amount"->-2.99`,"balance"->469.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->22901395375731000305798399609347607189|>,<|"date"->"2003-10-16","description"->"ATM Withdrawal - ITERAC","amount"->-21.25`,"balance"->474.11`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->187594759589763931531150202577699536803|>,<|"date"->"2003-10-16","description"->"Fees - Interac","amount"->-1.5`,"balance"->472.61`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->220337474021917067281791751934634596384|>,<|"date"->"2003-10-14","description"->"Payroll Deposit - HOTEL","amount"->694.81`,"balance"->695.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->299816565148943758289173312921977928600|>,<|"date"->"2003-10-14","description"->"Web Bill Payment - MASTERCARD","amount"->-200.`,"balance"->495.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84570175040844436262852571911033643807|>};
 (* Create a journal with two different years and accounts interleaved *)
 exampleJournal2 = Normal@exampleJournal;
 exampleJournal2[[;; ;; 2, "date"]] = 
  StringReplace[#, "2003" -> "2004"]& /@ exampleJournal2[[;; ;; 2, "date"]];
 exampleJournal2[[;; ;; 3, "account"]] = "BoA Savings";
 (* add a few more changes in amounts *)
 exampleJournal2[[;; 5, "amount"]] = 123;
 exampleJournal2 = CreateJournal[KeyDrop["id"] /@ exampleJournal2];
];

AddTest[journalFileHandlingTests, "Tear Down", 
 ClearAll[testDirTemp, exampleJournal, exampleJournal2];
 SetJournalDir[""];
];


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
  AssertEquals[{2004, 2003}, getJournalYear /@ journals];
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


(* ::Subsubsection:: *)
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
  
 (* Check writing into journal, not overwriting *)
 WriteToJournal[exampleJournal];
 With[{journalRead = ReadJournal["BoA Checking", 2003]},
  AssertTrue[IsJournal@journalRead];
  AssertEquals[18, Length@journalRead];
  AssertEquals["BoA Checking", Normal@journalRead[[1, "account"]]];
  AssertEquals[{-5., 123}, Sort@Normal@journalRead[[;;2, "amount"]]];
 ];
];


AddTest[readWriteJournalTests, "testListAccountsWithJournals",
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
];
