(* ::Package:: *)

AddSuite[MLedgerTests, ledgerTests];


(* ::Subsubsection:: *)
(*ledgerTests setup/teardown*)


AddTest[ledgerTests, "Set Up", 
 exampleJournal = CreateJournal[CreateJournalEntry@@@exampleJournalData];
];

AddTest[ledgerTests, "Tear Down", 
 ClearAll@exampleJournal
];


(* ::Subsection:: *)
(*Test ledger object*)


AddSuite[ledgerTests, ledgerObjectTests];


(* ::Subsubsection::Closed:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[ledgerObjectTests, ledgerObjectTestsInternal]

AddTest[ledgerObjectTestsInternal, "testCreateLedgerLine",
 AssertEquals[
   <|"date"->"2001-01-01", "account"->"", "debit"->"", "credit"->0,
     "currency"->"", "description"->"", "id"->0|>,
   createLedgerLine[]];

 With[{e = Normal@First@exampleJournal},
  AssertTrue[IsJournalEntry@e];
  AssertMatch[
   <|"date" -> "2003-11-08", "account" -> "BoA Checking", "debit"-> -5., "credit" -> "", 
     "currency" -> "USD", "description" -> "Fees - Monthly", "id" -> _|>, 
   createLedgerLine[e[["date"]], e[["account"]], e[["amount"]], "",
    e[["currency"]], e[["description"]], e[["id"]]]
   ];
 ];
];

AddTest[ledgerObjectTestsInternal, "testIsLedgerLine",
 AssertTrue[Not@isLedgerLine[1]];
 AssertTrue[Not@isLedgerLine[{1}]];
 AssertTrue[Not@isLedgerLine[<|"a" -> 2|>]];
 AssertTrue[Not@isLedgerLine@Drop[createLedgerLine[],3]];
 AssertTrue[isLedgerLine@createLedgerLine[]];
 With[{e = Normal@First@exampleJournal},
  AssertTrue@IsJournalEntry@e;
  AssertTrue[isLedgerLine@
   createLedgerLine[e[["date"]], e[["account"]], e[["amount"]], "",
    e[["currency"]], e[["description"]], e[["id"]]]];
 ];
];

AddTest[ledgerObjectTestsInternal, "testJournalEntryToLedgerLines",
 (* Normal entries gives one ledger line for the account and one for the category *)
 With[{journalEntry = MapAt["someCategory" &, Normal@First@exampleJournal, "category"]},
  AssertTrue@IsJournalEntry@journalEntry;
  With[{ledgerLines = journalEntryToLedgerLines@journalEntry},
   AssertMatch[{__?isLedgerLine}, ledgerLines];
   AssertEquals[2, Length@ledgerLines];
   AssertEquals[5., ledgerLines[[1, "credit"]]];
   AssertEquals["", ledgerLines[[2, "credit"]]];
   AssertEquals[journalEntry["account"], ledgerLines[[1, "account"]]];
   AssertEquals["someCategory", ledgerLines[[2, "account"]]];
  ];
 ];
 
 (* Entries with category "Internal" get only one ledger line, the matching line
    should come from the other account involved. *)
 With[{journalEntry = MapAt["Internal" &, Normal@First@exampleJournal, "category"]},
  AssertTrue@IsJournalEntry@journalEntry;
  With[{ledgerLines = journalEntryToLedgerLines@journalEntry},
   AssertMatch[{__?isLedgerLine}, ledgerLines];
   AssertEquals[1, Length@ledgerLines];
   AssertEquals["", ledgerLines[[1, "debit"]]];
   AssertEquals[5., ledgerLines[[1, "credit"]]];
   AssertEquals[journalEntry["account"], ledgerLines[[1, "account"]]];
  ];
 ];
];

End[]; (* End "MLedger`Private`" *)


(* ::Subsubsection::Closed:: *)
(*object tests*)


AddTest[ledgerObjectTests, "testIsLedger",
 AssertTrue[Not@IsLedger[1]];
 AssertTrue[Not@IsLedger[{}]];
 AssertTrue[Not@IsLedger[x]];
 AssertTrue[IsLedger[Dataset@{}]];
 AssertTrue[IsLedger[Dataset@{MLedger`Private`createLedgerLine[]}]];
 AssertTrue[Not@IsLedger[Dataset@{Drop[#,1]&@MLedger`Private`createLedgerLine[]}]];
];


AddTest[ledgerObjectTests, "testCreateLedger",
 AssertEquals[{}, Normal@CreateLedger[]];
 AssertTrue[IsJournal@exampleJournal];
 With[{ledger = CreateLedger@exampleJournal},
  AssertTrue[IsLedger@ledger];
  AssertEquals[34, Length@ledger];
  AssertEquals[5., ledger[3, "credit"]];
 ];
 With[{ledgerWithInternalEntry = 
          CreateLedger@MapAt["Internal" &, exampleJournal, {1, "category"}]},
  AssertTrue[IsLedger@ledgerWithInternalEntry];
  (* Check only one ledger entry from "Internal" journal entry *)
  AssertEquals[33, Length@ledgerWithInternalEntry]; 
  AssertEquals["", ledgerWithInternalEntry[3, "credit"]];
 ];
];

AddTest[ledgerObjectTests, "testCreateLedgerFromLedgerLines",
 With[{ledgerLines = Normal@CreateLedger@exampleJournal},
  AssertMatch[{__?MLedger`Private`isLedgerLine}, ledgerLines];
  AssertTrue[IsLedger@CreateLedger@ledgerLines];
  AssertEquals[34, Length@CreateLedger@ledgerLines];
 ];
];


(* ::Subsection:: *)
(*Test ledger file handling*)


AddSuite[ledgerTests, ledgerFileHandlingTests];


AddTest[ledgerFileHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp642/";
 SetLedgerDir[testDirTemp];
 exampleLedger = CreateLedger@exampleJournal;
 
 exampleJournal2 = CreateJournal[CreateJournalEntry@@@exampleJournalData2];
 exampleLedger2 = CreateLedger@exampleJournal2;
];
AddTest[ledgerFileHandlingTests, "Tear Down", 
 ClearAll[testDirTemp, exampleLedger, exampleJournal2, exampleLedger2];
 SetLedgerDir[""];
];


AddTest[ledgerFileHandlingTests, "testGetSetLedgerDir",
 AssertEquals[testDirTemp, GetLedgerDir[]];
 SetLedgerDir["dir"];
 AssertEquals["dir", GetLedgerDir[]];
];


(* ::Subsubsection::Closed:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[ledgerFileHandlingTests, ledgerFileHandlingTestsInternal];

AddTest[ledgerFileHandlingTestsInternal, "testSplitLedgerByMonthAndYear",
 AssertTrue[IsLedger@exampleLedger];
 With[{ledgers = splitLedgerByMonthAndYear@exampleLedger},
  AssertMatch[{__?IsLedger}, ledgers];
  AssertEquals[2, Length@ledgers]; (* single year, two different months *)
  (*AssertEquals[{"BoA Checking", "BoA Savings"}, getJournalAccount /@ ledgers];*)
 ];
 AssertTrue[IsLedger@exampleLedger2];
 With[{ledgers = splitLedgerByMonthAndYear@exampleLedger2},
  AssertMatch[{__?IsLedger}, ledgers];
  AssertEquals[4, Length@ledgers]; (* two year, two months each *)
  (*AssertEquals[{"BoA Checking", "BoA Savings"}, getJournalAccount /@ ledgers];*)
 ];
];

AddTest[ledgerFileHandlingTestsInternal, "testFormatLedgerFilename",
 AssertEquals[GetLedgerDir[] <> "2003/November.csv",
  formatLedgerFilename[2003, 11]];
 AssertTrue[IsLedger@exampleLedger];
 AssertEquals[GetLedgerDir[] <> "2003/November.csv", 
  formatLedgerFilename@exampleLedger[[;;5]]];
 AssertEquals[False, formatLedgerFilename@exampleLedger2];
];

End[]; (* End "MLedger`Private`" *)


(* ::Subsubsection::Closed:: *)
(*Read/WriteToLedger*)


(* ::Text:: *)
(*Separate subsuite for tests needing setup/teardown of directories to avoid interacting with file system with rest of tests.*)


AddSuite[ledgerFileHandlingTests, readWriteLedgerTests];


AddTest[readWriteLedgerTests, "Set Up", 
 EnsureDirectoryExists[testDirTemp];
];

AddTest[readWriteLedgerTests, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];


AddTest[readWriteLedgerTests, "testReadWriteLedger",
 AssertTrue[FileExistsQ@GetLedgerDir[]];
 AssertEquals[{}, Normal@ReadLedger[2003, 11]];
 
 AssertTrue[Not@FileExistsQ[GetLedgerDir[] <> "2003/October.csv"]];
 AssertTrue[IsLedger@exampleLedger2];
 WriteToLedger[exampleLedger2];
 (* Check properly split by account and year *)
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2003/October.csv"]];
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2003/November.csv"]];
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2004/October.csv"]];
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2004/November.csv"]];
 
 With[{ledgerRead = ReadLedger[2003, 11]},
   AssertTrue[IsLedger@ledgerRead];
   AssertEquals[4, Length@ledgerRead];
   AssertEquals["BoA Checking", Normal@ledgerRead[[1, "account"]]];
   AssertEquals["2003-11-03", Normal@ledgerRead[[3, "date"]]];
   AssertEquals[123, Normal@ledgerRead[[4, "credit"]]];
  ];
];


AddTest[readWriteLedgerTests, "WriteLedgerFromJournalFiles",
 SetJournalDir[testDirTemp <> "Journal/"];
 EnsureDirectoryExists[GetJournalDir[]];
 AssertTrue[FileExistsQ@GetJournalDir[]];
 AssertTrue[FileExistsQ@GetLedgerDir[]];
 
 WriteToJournal[exampleJournal2];
 AssertTrue[FileExistsQ@MLedger`Private`formatJournalFilename@exampleJournal2[[;;1]]];
 With[{journal = ReadJournal[MLedger`Private`getJournalAccount@exampleJournal2[[;;1]]]},
  AssertTrue[IsJournal@journal];
  AssertEquals[6, Length@journal];
 ];
 
 AssertEquals[{}, Normal@ReadLedger[2003, 11]];
 WriteLedgerFromJournalFiles[2003];
 AssertEquals[{}, Normal@ReadLedger[2004, 11]];
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2003/October.csv"]];
 AssertTrue[FileExistsQ[GetLedgerDir[] <> "2003/November.csv"]];
 
 With[{ledgerRead = ReadLedger[2003, 11]},
  AssertTrue[IsLedger@ledgerRead];
  AssertEquals[4, Length@ledgerRead];
  AssertEquals["BoA Checking", Normal@ledgerRead[[1, "account"]]];
  AssertEquals["2003-11-03", Normal@ledgerRead[[3, "date"]]];
  AssertEquals[123, Normal@ledgerRead[[4, "credit"]]];
 ];
];
