(* ::Package:: *)

AddSuite[MLedgerTests, bankAccountsTests];


(* Make sure we get a clean list of bank accounts each test, 
     and that any old accounts are restored*)
Module[{accounts},
 AddTest[bankAccountsTests, "Set Up", 
  accounts = GetBankAccounts[];
  SetBankAccounts[{}];
 ];
 AddTest[bankAccountsTests, "Tear Down",
  SetBankAccounts[accounts];
 ];
]


(* ::Subsection::Closed:: *)
(*Test bank account objects*)


AddSuite[bankAccountsTests, bankAccountObjectsTests];


AddTest[bankAccountObjectsTests, "testGetSetBankAccounts",
 AssertEquals[{}, GetBankAccounts[]];
 SetBankAccounts[{""}];
 AssertEquals[{""}, GetBankAccounts[]];
];

AddTest[bankAccountObjectsTests, "testAddBankAccount",
 AssertEquals[{}, GetBankAccounts[]];
 AddBankAccount["testAccount", "SEK", "", ""];
 AssertEquals[1, Length@GetBankAccounts[]];
 AssertEquals[{"name", "currency", "filePattern", "importFunction"}, 
  Keys@GetBankAccounts[][[1]]];
 AssertEquals["testAccount", GetBankAccounts[][[1]]["name"]];
 AssertEquals["SEK", GetBankAccounts[][[1]]["currency"]];
 AssertEquals["", GetBankAccounts[][[1]]["filePattern"]];
 AssertEquals["", GetBankAccounts[][[1]]["importFunction"]];
];

AddTest[bankAccountObjectsTests, "testBankAccountNameQ",
 AddBankAccount[#, "USD", "", Null] & /@ {"test1", "test2"};
 AssertTrue[Not@BankAccountNameQ@1];
 AssertTrue[Not@BankAccountNameQ@"test"];
 AssertTrue[BankAccountNameQ@"test1"];
 AssertTrue[BankAccountNameQ@"test2"];
];

AddTest[bankAccountObjectsTests, "testListBankAccounts",
 AddBankAccount[#, "USD", "", Null] & /@ {"test1", "test2"};
 AssertEquals[{"test1", "test2"}, ListBankAccounts[]];
];


(* ::Subsection::Closed:: *)
(*Test importing bank statements*)


AddSuite[bankAccountsTests, importBankStatementsTests];

AddTest[importBankStatementsTests, "Set Up", 
 AddBankAccount["Example account USD", "USD", "export"~~___~~".csv", importF1];
 AddBankAccount["Example account USD 2", "USD", "stmt"~~___~~".txt", importF2];
];


AddTest[importBankStatementsTests, "testListImportableFiles", 
 AssertTrue[Length@FileNames@testFilesDir > 0];
 AssertEquals[{"export0.csv", "stmt.txt"}, 
  FileNameTake/@ListImportableFiles[testFilesDir]];
];


AddTest[importBankStatementsTests, "testSelectAccountsForm", 
 AssertTrue[Length@FileNames@testFilesDir > 0];
 With[{form = SelectAccountsForm@ListImportableFiles[testFilesDir]},
 AssertMatch[Grid[___], form];
 AssertMatch[Grid[{___,
                   {"export0.csv", PopupMenu[Dynamic[_], {"Example account USD"}, ___]},
                   ___}], 
             form];
 AssertMatch[Grid[{___,
                   {"stmt.txt", PopupMenu[Dynamic[_], {"Example account USD 2"}, ___]},
                   ___}], 
             form];
 ]
];


AddTest[importBankStatementsTests, "testImportAccountFiles", 
 With[{files = FileNames[#, testFilesDir]& /@ {"export0.csv", "stmt.txt"}},
  AssertTrue[Length@files > 0];
  With[{accounts = MLedger`Private`getMatchingAccounts /@ First /@ files},
   AssertTrue[And @@ (# > 0 & /@ Length /@ accounts)];
   With[{imported = 
     ImportAccountFiles[First /@ files, (First /@ accounts)[[All, "name"]]]},
    AssertEquals[
     {importF1[First@files[[1]], "Example account USD"], 
      importF2[First@files[[2]], "Example account USD 2"]}, 
     imported];
   ]]];
];


(* ::Subsubsection:: *)
(*Internal tests*)


Begin["MLedger`Private`"];
AddSuite[importBankStatementsTests, importBankStatementsTestsInternal]
AddTest[importBankStatementsTestsInternal, "testImportableFileQ",
 AddBankAccount["Example account USD 2", "USD", "stmt"~~___~~".txt", Null];

 AssertTrue[!importableFileQ[1]];
 AssertTrue[!importableFileQ[{}]];
 AssertTrue[!importableFileQ[Symbol]];
 
 AssertTrue[importableFileQ["stmt.txt"]];
 AssertTrue[importableFileQ["someFolder/stmt .txt"]];
 AssertTrue[!importableFileQ["st.txt"]];
];
End[];


(* ::Subsection:: *)
(*Test bank specific functions*)


AddSuite[bankAccountsTests, bankSpecificTests];


(* ::Subsubsection:: *)
(*Bank of America*)


AddSuite[bankSpecificTests, BoATests];


AddTest[BoATests, "testAddBoAAccount",
 AddBoAAccount["BoATestAcc"];
 AssertTrue[MemberQ[ListBankAccounts[], "BoATestAcc"]];
 With[{account = First@Select[GetBankAccounts[], #[["name"]] == "BoATestAcc" &]},
  AssertEquals["USD", account[["currency"]]];
  AssertEquals[MLedger`Private`importBoA, account[["importFunction"]]];
  AssertTrue[StringMatchQ["stmt.txt", account[["filePattern"]]]];
 ];
];


Begin["MLedger`Private`"];
AddSuite[BoATests, BoATestsInternal];

AddTest[BoATestsInternal, "testfilePatternBoA",
 AssertTrue[StringMatchQ["stmt.txt", filePatternBoA]];
 AssertTrue[StringMatchQ["stmt0.txt", filePatternBoA]];
 AssertTrue[StringMatchQ["stmt (copy).txt", filePatternBoA]];
 AssertTrue[StringMatchQ["stmt.qfx", filePatternBoA]];
 AssertTrue[StringMatchQ["stmt0.qfx", filePatternBoA]];
 AssertTrue[StringMatchQ["stmt (copy).qfx", filePatternBoA]];
];

AddTest[BoATestsInternal, "testImportBoAtxt",
 With[{filename = testFilesDir <> "stmt.txt"},
  AssertTrue[Length@FileNames[filename] > 0];
  With[{imported = importBoA[filename, "BoATestAcc"]},
   AssertTrue[IsJournal@imported];
   AssertEquals[17, Length@imported];
   
   (* Check one of each field *)
   AssertEquals["2003-10-14", imported[[-1, "date"]]];
   AssertEquals["ATM Withdrawal - ITERAC", imported[[-3, "description"]]];
   AssertEquals[-33.55`, imported[[5, "amount"]]];
   AssertEquals[-72.47, imported[[1, "balance"]]];
   AssertEquals["BoATestAcc", imported[[1, "account"]]];
   AssertEquals["USD", imported[[1, "currency"]]];
   
   (* Check sorted by date descending and order correct for balances *)
   AssertEquals[
    {<|"date" -> "2003-11-06", "amount" -> -710.49, "balance" -> -62.47|>,
     <|"date" -> "2003-11-03", "amount" -> -100.,   "balance" -> 648.02|>,
     <|"date" -> "2003-11-03", "amount" -> -33.55,  "balance" -> 748.02 |>},
    Normal@imported[[3;;5, {"date", "amount", "balance"}]]];
  ];
 ];
];

AddTest[BoATestsInternal, "testImportBoAqfx",
 With[{filename = testFilesDir <> "bank_medium.qfx"},
  AssertTrue[Length@FileNames[filename] > 0];
  With[{imported = importBoA[filename, "BoATestAcc"]},
   AssertTrue[IsJournal@imported];
   AssertEquals[3, Length@imported];
   
   (* Check one of each field *)
   AssertEquals["2009-04-01", imported[[1, "date"]]];
   AssertEquals["Joe's Bald Hairstyles", imported[[2, "description"]]];
   AssertEquals[-22., imported[[3, "amount"]]];
   AssertEquals[0., imported[[1, "balance"]]];
   AssertEquals["BoATestAcc", imported[[1, "account"]]];
   AssertEquals["USD", imported[[1, "currency"]]];
   AssertEquals["0000123456782009040100001", imported[[1, "FITID"]]];
   
   (*(* Check sorted by date descending and order correct for balances *)
   AssertEquals[
    {<|"date" -> "2003-11-06", "amount" -> -710.49, "balance" -> -62.47|>,
     <|"date" -> "2003-11-03", "amount" -> -100.,   "balance" -> 648.02|>,
     <|"date" -> "2003-11-03", "amount" -> -33.55,  "balance" -> 748.02 |>},
    Normal@imported[[3;;5, {"date", "amount", "balance"}]]];*)
  ];
 ];
];

End[];


(* ::Subsubsection::Closed:: *)
(*Nordea*)


AddSuite[bankSpecificTests, nordeaTests];


AddTest[nordeaTests, "testAddNordeaAccount",
 AddNordeaAccount["nordeaTestAcc"];
 AssertTrue[MemberQ[ListBankAccounts[], "nordeaTestAcc"]];
 With[{account = First@Select[GetBankAccounts[], #[["name"]] == "nordeaTestAcc" &]},
  AssertEquals["SEK", account[["currency"]]];
  AssertEquals[MLedger`Private`importNordea, account[["importFunction"]]];
  AssertTrue[StringMatchQ["export.csv", account[["filePattern"]]]];
 ];
];


Begin["MLedger`Private`"];
AddSuite[nordeaTests, nordeaTestsInternal];

AddTest[nordeaTestsInternal, "testNordeaFilePattern",
 AssertTrue[StringMatchQ["export.csv", nordeaFilePattern]];
 AssertTrue[StringMatchQ["export0.csv", nordeaFilePattern]];
 AssertTrue[StringMatchQ["export (copy).csv", nordeaFilePattern]];
];

AddTest[nordeaTestsInternal, "testImportNordea",
 With[{filename = testFilesDir <> "export0.csv"},
  AssertTrue[Length@FileNames[filename] > 0];
  With[{imported = importNordea[filename, "nordeaTestAcc"]},
   AssertTrue[IsJournal@imported];
   AssertEquals[17, Length@imported];
   
   AssertEquals["2003-11-08", imported[[1, "date"]]];
   AssertEquals["ATM Withdrawal - ITERAC", imported[[-3, "description"]]];
   AssertEquals[-33.55`, imported[[5, "amount"]]];
   AssertEquals[-72.47, imported[[1, "balance"]]];
   AssertEquals["nordeaTestAcc", imported[[1, "account"]]];
   AssertEquals["SEK", imported[[1, "currency"]]];
  ];
 ];
];

End[];
