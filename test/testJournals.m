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
];



AddTest[journalTests, "testIsJournalEntry",
 AssertTrue[Not@IsJournalEntry[1]];
 AssertTrue[Not@IsJournalEntry[{1}]];
 AssertTrue[Not@IsJournalEntry[<|"a" -> 2|>]];
 AssertTrue[Not@IsJournalEntry@Drop[CreateJournalEntry[],3]];
 AssertTrue[IsJournalEntry@CreateJournalEntry[]];
 AssertTrue[IsJournalEntry@Reverse@CreateJournalEntry[]];
];
