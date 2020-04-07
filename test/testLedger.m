(* ::Package:: *)

AddSuite[MLedgerTests, ledgerTests];


(* ::Subsection:: *)
(*Test Ledger object*)


AddSuite[ledgerTests, ledgerObjectTests];


AddTest[ledgerObjectTests, "Set Up", 
 exampleJournal = CreateJournal@{<|"date"->"2003-11-08","description"->"Fees - Monthly","amount"->-5.`,"balance"->-72.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->274356663192601122998088388993363735798|>,<|"date"->"2003-11-07","description"->"Fees - Overdraft","amount"->-5.`,"balance"->-62.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84979730638829994080353479076932943230|>,<|"date"->"2003-11-06","description"->"Mortgage Payment","amount"->-710.49`,"balance"->648.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->88868563507080916278564744628206662059|>,<|"date"->"2003-11-03","description"->"Pre-Auth. Payment - INSURANCE","amount"->-33.55`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->91664361405917894742960028992867906147|>,<|"date"->"2003-11-03","description"->"Cheque No. - 409","amount"->-100.`,"balance"->748.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->178122922470972405198691130574520500688|>,<|"date"->"2003-10-30","description"->"Web Funds Transfer - From SAVINGS","amount"->50.`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->241118077855012299614093250593762684684|>,<|"date"->"2003-10-28","description"->"Payroll Deposit - HOTEL","amount"->731.57`,"balance"->731.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->249827740959480498780368837530154740381|>,<|"date"->"2003-10-27","description"->"Telephone Bill Payment - VISA","amount"->-6.77`,"balance"->36.76`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->93485259797755573237552240030516042590|>,<|"date"->"2003-10-24","description"->"Interac Refund - ELECTRONICS","amount"->2.99`,"balance"->43.53`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->177283377122706640434729207919340396612|>,<|"date"->"2003-10-23","description"->"Interac Purchase - SUPERMARKET","amount"->-29.08`,"balance"->40.54`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->306543770689891693646728945877615865925|>,<|"date"->"2003-10-22","description"->"ATM Withdrawal - FIRST BANK","amount"->-100.`,"balance"->69.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->62939956412697090788141371309312500050|>,<|"date"->"2003-10-21","description"->"Web Bill Payment - AMEX","amount"->-300.`,"balance"->169.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->277055912645424638284676796424185571315|>,<|"date"->"2003-10-20","description"->"Interac Purchase - ELECTRONICS","amount"->-2.99`,"balance"->469.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->22901395375731000305798399609347607189|>,<|"date"->"2003-10-16","description"->"ATM Withdrawal - ITERAC","amount"->-21.25`,"balance"->474.11`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->187594759589763931531150202577699536803|>,<|"date"->"2003-10-16","description"->"Fees - Interac","amount"->-1.5`,"balance"->472.61`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->220337474021917067281791751934634596384|>,<|"date"->"2003-10-14","description"->"Payroll Deposit - HOTEL","amount"->694.81`,"balance"->695.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->299816565148943758289173312921977928600|>,<|"date"->"2003-10-14","description"->"Web Bill Payment - MASTERCARD","amount"->-200.`,"balance"->495.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84570175040844436262852571911033643807|>};
];

AddTest[ledgerObjectTests, "Tear Down", 
 ClearAll@exampleJournal
];


AddTest[ledgerObjectTests, "testIsLedger",
 AssertTrue[Not@IsLedger[1]];
 AssertTrue[Not@IsLedger[{}]];
 AssertTrue[Not@IsLedger[x]];
 AssertTrue[IsLedger[Dataset@{}]];
 AssertTrue[IsLedger[Dataset@{MLedger`Private`createLedgerLine[]}]];
 AssertTrue[Not@IsLedger[Dataset@{Drop[#,1]&@MLedger`Private`createLedgerLine[]}]];
];


AddTest[ledgerObjectTests, "testCreateLedger",
 AssertTrue[IsJournal@exampleJournal];
 With[{ledger = CreateLedger@exampleJournal},
  AssertTrue[IsLedger@ledger];
 ];
];


(* ::Subsubsection:: *)
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
   AssertEquals[1, Length@ledgerLines];
   AssertEquals["", ledgerLines[[1, "debit"]]];
   AssertEquals[5., ledgerLines[[1, "credit"]]];
   AssertEquals[journalEntry["account"], ledgerLines[[1, "account"]]];
  ];
 ];
];

End[]; (* End "MLedger`Private`" *)
