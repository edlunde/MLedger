(* ::Package:: *)

AddSuite[MLedgerTests, categorizationTests];


(* ::Subsection:: *)
(*Test categorization form*)


AddSuite[categorizationTests, categorizationFormTests];


AddTest[categorizationFormTests, "testCategorizationForm",
 1
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[categorizationFormTests, categorizationFormTestsInternal]
AddTest[categorizationFormTestsInternal, "testCategorizationFormRow",
 With[{exampleEntry = <|"date"->"2003-11-03","description"->"Pre-Auth. Payment - INSURANCE","amount"->-33.55`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"Insurance","id"->91664361405917894742960028992867906147|>},
  AssertTrue[IsJournalEntry@exampleEntry];
  AssertEquals[categorizationFormRow, 
   Head@categorizationFormRow[Drop[exampleEntry, 1]]];
  AssertMatch[ 
   {exampleEntry[["date"]], exampleEntry[["description"]], 
    exampleEntry[["amount"]], 
    PopupMenu[Dynamic[_], __],  InputField[Dynamic[_], String, __]
   },
   categorizationFormRow@exampleEntry];
]];
End[]; (* End "MLedger`Private`" *)


exampleJournal = CreateJournal@{<|"date"->"2003-11-08","description"->"Fees - Monthly","amount"->-5.`,"balance"->-72.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->274356663192601122998088388993363735798|>,<|"date"->"2003-11-07","description"->"Fees - Overdraft","amount"->-5.`,"balance"->-62.47`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84979730638829994080353479076932943230|>,<|"date"->"2003-11-06","description"->"Mortgage Payment","amount"->-710.49`,"balance"->648.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->88868563507080916278564744628206662059|>,<|"date"->"2003-11-03","description"->"Pre-Auth. Payment - INSURANCE","amount"->-33.55`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->91664361405917894742960028992867906147|>,<|"date"->"2003-11-03","description"->"Cheque No. - 409","amount"->-100.`,"balance"->748.02`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->178122922470972405198691130574520500688|>,<|"date"->"2003-10-30","description"->"Web Funds Transfer - From SAVINGS","amount"->50.`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->241118077855012299614093250593762684684|>,<|"date"->"2003-10-28","description"->"Payroll Deposit - HOTEL","amount"->731.57`,"balance"->731.57`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->249827740959480498780368837530154740381|>,<|"date"->"2003-10-27","description"->"Telephone Bill Payment - VISA","amount"->-6.77`,"balance"->36.76`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->93485259797755573237552240030516042590|>,<|"date"->"2003-10-24","description"->"Interac Refund - ELECTRONICS","amount"->2.99`,"balance"->43.53`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->177283377122706640434729207919340396612|>,<|"date"->"2003-10-23","description"->"Interac Purchase - SUPERMARKET","amount"->-29.08`,"balance"->40.54`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->306543770689891693646728945877615865925|>,<|"date"->"2003-10-22","description"->"ATM Withdrawal - FIRST BANK","amount"->-100.`,"balance"->69.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->62939956412697090788141371309312500050|>,<|"date"->"2003-10-21","description"->"Web Bill Payment - AMEX","amount"->-300.`,"balance"->169.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->277055912645424638284676796424185571315|>,<|"date"->"2003-10-20","description"->"Interac Purchase - ELECTRONICS","amount"->-2.99`,"balance"->469.62`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->22901395375731000305798399609347607189|>,<|"date"->"2003-10-16","description"->"ATM Withdrawal - ITERAC","amount"->-21.25`,"balance"->474.11`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->187594759589763931531150202577699536803|>,<|"date"->"2003-10-16","description"->"Fees - Interac","amount"->-1.5`,"balance"->472.61`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->220337474021917067281791751934634596384|>,<|"date"->"2003-10-14","description"->"Payroll Deposit - HOTEL","amount"->694.81`,"balance"->695.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->299816565148943758289173312921977928600|>,<|"date"->"2003-10-14","description"->"Web Bill Payment - MASTERCARD","amount"->-200.`,"balance"->495.36`,"account"->"BoA Checking","currency"->"USD","category"->"","id"->84570175040844436262852571911033643807|>};


Normal@exampleJournal[[4]]


exampleEntry = <|"date"->"2003-11-03","description"->"Pre-Auth. Payment - INSURANCE","amount"->-33.55`,"balance"->781.57`,"account"->"BoA Checking","currency"->"USD","category"->"Insurance","id"->91664361405917894742960028992867906147|>


Keys@exampleEntry
