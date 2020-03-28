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


(* ::Subsection:: *)
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
(*Nordea*)


AddSuite[bankSpecificTests, nordeaTests];


Begin["MLedger`Private`"];
AddSuite[nordeaTests, nordeaTestsInternal];
AddTest[nordeaTests, "testNordeaFilePattern",
 AssertTrue[StringMatchQ["export.csv", nordeaFilePattern]];
 AssertTrue[StringMatchQ["export0.csv", nordeaFilePattern]];
 AssertTrue[StringMatchQ["export (copy).csv", nordeaFilePattern]];
]
End[];
