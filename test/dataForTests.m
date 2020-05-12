(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Journal*)


exampleJournalData = {
 {"2003-11-08","Fees - Monthly", -5., -72.47, "BoA Checking", "USD", ""}, 
 {"2003-11-07", "Fees - Overdraft", -5., -67.47, "BoA Checking", "USD", ""}, 
 {"2003-11-06", "Mortgage Payment", -710.49, -62.47, "BoA Checking", "USD", ""}, 
 {"2003-11-03", "Cheque No. - 409", -100., 648.02, "BoA Checking", "USD", ""}, 
 {"2003-11-03", "Pre-Auth. Payment - INSURANCE", -33.55, 748.02, "BoA Checking", "USD", ""}, 
 {"2003-10-30", "Web Funds Transfer - From SAVINGS", 50., 781.57, "BoA Checking", "USD", ""}, 
 {"2003-10-28", "Payroll Deposit - HOTEL", 694.81, 731.57, "BoA Checking", "USD", ""}, 
 {"2003-10-27", "Telephone Bill Payment - VISA", -6.77, 36.76, "BoA Checking", "USD", ""}, 
 {"2003-10-24", "Interac Refund - ELECTRONICS", 2.99, 43.53, "BoA Checking", "USD", ""}, 
 {"2003-10-23", "Interac Purchase - SUPERMARKET", -29.08, 40.54, "BoA Checking", "USD", ""}, 
 {"2003-10-22", "ATM Withdrawal - FIRST BANK", -100., 69.62, "BoA Checking", "USD", ""}, 
 {"2003-10-21", "Web Bill Payment - AMEX", -300., 169.62, "BoA Checking", "USD", ""}, 
 {"2003-10-20", "Interac Purchase - ELECTRONICS", -2.99, 469.62, "BoA Checking", "USD", ""}, 
 {"2003-10-16", "Fees - Interac", -1.5, 472.61, "BoA Checking", "USD", ""}, 
 {"2003-10-16", "ATM Withdrawal - ITERAC", -21.25, 474.11, "BoA Checking", "USD", ""}, 
 {"2003-10-14", "Web Bill Payment - MASTERCARD", -200., 495.36, "BoA Checking", "USD", ""}, 
 {"2003-10-14", "Payroll Deposit - HOTEL", 694.81, 695.36, "BoA Checking", "USD", ""}};

(* CreateJournal[CreateJournalEntry@@@exampleJournalData] *)


(* Create a journal with two different years and accounts interleaved *)
exampleJournalData2 = exampleJournalData;
exampleJournalData2[[;; ;; 2, 1]] = 
 StringReplace[#, "2003" -> "2004"]& /@ exampleJournalData2[[;; ;; 2, 1]];
exampleJournalData2[[;; ;; 3, 5]] = "BoA Savings";
(* add a few more changes in amounts *)
exampleJournalData2[[;; 5, 3]] = 123;
 
(* CreateJournal[CreateJournalEntry@@@exampleJournalData2] *)


(* Some categories for the exampleJournal *)
categoriesForExampleJournal =
 {"Banking fees","Banking fees","Mortgage","","Insurance","Internal","Salary",
  "","Electronics","Groceries","","","","","","",""};


(* ::Subsection::Closed:: *)
(*Balances*)


exampleBalanceData = {"2003-10-09", 
 {<|"account"->"Example BoA account" ,"balance"->0.55,"currency"->"USD"|>,
  <|"account"->"Example Nordea account" ,"balance"->0.55,"currency"->"SEK" |>}
  };


(* ::Subsection::Closed:: *)
(*Budget*)


exampleCategoryGroups = <|
 "Expenses" -> <|
  "Mortgage" -> {}, "Groceries" -> {}, (* Leaving Electronics uncategorized*)
  "Insurance" -> {}, "Misc." -> {"Banking fees"}|>,
 "Income" -> <|"Salary" -> {}|>,
 "Savings" -> <|"Investments" -> {}, "Rainy day fund" -> {}|>
 |>;
 
exampleBudget = <|
 "Mortgage" -> 711, "Groceries" -> 30,
 "Insurance" -> 34, "Misc." -> 10
 |>;
