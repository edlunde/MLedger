(* ::Package:: *)

AddSuite[MLedgerTests, bankAccountsTests];


(* ::Subsection:: *)
(*Test bank account objects*)


AddSuite[bankAccountsTests, bankAccountObjectsTests];

(* Make sure we get a clean list of bank accounts each test, 
     and that any old accounts are restored*)
Module[{accounts},
 AddTest[bankAccountObjectsTests, "Set Up", 
  accounts = getBankAccounts[];
  setBankAccounts[{}];
 ];
 AddTest[bankAccountObjectsTests, "Tear Down",
  setBankAccounts[accounts];
 ];
]


AddTest[bankAccountObjectsTests, "testGetSetBankAccounts",
 AssertEquals[{}, getBankAccounts[]];
 setBankAccounts[{""}];
 AssertEquals[{""}, getBankAccounts[]];
];

AddTest[bankAccountObjectsTests, "testAddBankAccount",
 AssertEquals[{}, getBankAccounts[]];
 addBankAccount["testAccount", "SEK", "", ""];
 AssertEquals[1, Length@getBankAccounts[]];
 AssertEquals[{"name", "currency", "filePattern", "importFunction"}, 
  Keys@getBankAccounts[][[1]]];
 AssertEquals["testAccount", getBankAccounts[][[1]]["name"]];
 AssertEquals["SEK", getBankAccounts[][[1]]["currency"]];
 AssertEquals["", getBankAccounts[][[1]]["filePattern"]];
 AssertEquals["", getBankAccounts[][[1]]["importFunction"]];
];
