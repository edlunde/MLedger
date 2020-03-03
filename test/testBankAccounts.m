(* ::Package:: *)

AddSuite[MLedgerTests, bankAccountsTests];


(* ::Subsection:: *)
(*Test bank account objects*)


AddSuite[bankAccountsTests, bankAccountObjectsTests];

(* Make sure we get a clean list of bank accounts each test, 
     and that any old accounts are restored*)
Module[{accounts},
 AddTest[bankAccountObjectsTests, "Set Up", 
  accounts = GetBankAccounts[];
  SetBankAccounts[{}];
 ];
 AddTest[bankAccountObjectsTests, "Tear Down",
  SetBankAccounts[accounts];
 ];
]


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
