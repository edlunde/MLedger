(* ::Package:: *)

AddSuite[MLedgerTests, journalsTests];


(*(* Make sure we get a clean list of bank accounts each test, 
     and that any old accounts are restored*)
Module[{accounts},
 AddTest[bankAccountsTests, "Set Up", 
  accounts = GetBankAccounts[];
  SetBankAccounts[{}];
 ];
 AddTest[bankAccountsTests, "Tear Down",
  SetBankAccounts[accounts];
 ];
]*)


(* ::Subsection:: *)
(*Test journal*)


AddSuite[journalsTests, journalTests];


AddTest[journalTests, "testIsJournal",
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


AddTest[journalTests, "testCreateJournal",
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


AddTest[journalTests, "testCreateJournalEntry",
 AssertEquals[
  <|"date"->"1-01-01", "description"->"", "amount"->0.`, "balance"->0.`,
    "account"->"", "currency"->"", "category"->""|>,
  CreateJournalEntry[]];
  
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



AddTest[journalTests, "testIsJournalEntry",
 AssertTrue[Not@IsJournalEntry[1]];
 AssertTrue[Not@IsJournalEntry[{1}]];
 AssertTrue[Not@IsJournalEntry[<|"a" -> 2|>]];
 AssertTrue[Not@IsJournalEntry@Drop[CreateJournalEntry[],3]];
 AssertTrue[IsJournalEntry@CreateJournalEntry[]];
 AssertTrue[IsJournalEntry@Reverse@CreateJournalEntry[]];
 
 (* Extra values should be allowed *)
 AssertTrue[IsJournalEntry@<|CreateJournalEntry[], "extra key" -> "extra value"|>];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[journalTests, journalTestsInternal]
AddTest[journalTestsInternal, "testAddIDs",
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
End[];
