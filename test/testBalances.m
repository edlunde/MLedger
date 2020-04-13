(* ::Package:: *)

AddSuite[MLedgerTests, balancesTests];


(* ::Subsection:: *)
(*Test balances object*)


AddSuite[balancesTests, testBalancesObject];


AddTest[testBalancesObject, "testIsAccountBalances",
 AssertTrue[Not@IsAccountBalances[1]];
 AssertTrue[Not@IsAccountBalances[{1}]];
 AssertTrue[Not@IsAccountBalances[{<|"a" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"account" -> 1, "balance" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"account" -> 1, "currency" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{<|"balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[Not@IsAccountBalances[{
  <|"account" -> 1, "balance" -> 1|>,
  <|"account" -> 3, "balance" -> 2, "currency" -> 1|>}]];

 AssertTrue[IsAccountBalances[{<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[IsAccountBalances[{
  <|"account" -> 1, "balance" -> 1, "currency" -> 1, "extra" -> {}|>,
  <|"account" -> 3, "balance" -> 2, "currency" -> 1|>}]];
];

AddTest[testBalancesObject, "testIsBalances",
 AssertTrue[Not@IsBalances[1]];
 AssertTrue[Not@IsBalances[<|"a" -> 3|>]];
 AssertTrue[Not@IsBalances[<|"date" -> 3, "accountBalances" -> 2|>]];

 AssertTrue[IsBalances[
  <|"date" -> 3, 
    "accountBalances" -> {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}|>]];
];


AddTest[testBalancesObject, "testCreateBalancesObject",
 AssertTrue[Not@IsBalances@CreateBalancesObject[1]];
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", 2]];
 AssertTrue[KeyExistsQ[#, "date"] & @ 
  CreateBalancesObject["1", {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 AssertTrue[KeyExistsQ[#, "accountBalances"] & @ 
  CreateBalancesObject["1", {<|"account" -> 1, "balance" -> 1, "currency" -> 1|>}]];
 
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> 1|>]];
 AddBoAAccount@"acc1"; AddNordeaAccount@"acc2"; 
 AssertEquals[{"acc1", "acc2"}, ListBankAccounts[]];
 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> 1, "acc3" -> 2|>]];

 AssertTrue[Not@IsBalances@CreateBalancesObject["1", <|"acc1" -> "1", "acc2" -> 2|>]];
 
 AssertTrue[KeyExistsQ[#, "date"] & @ CreateBalancesObject["1", <|"acc1" -> 1, "acc2" -> 3|>]];
 AssertTrue[KeyExistsQ[#, "accountBalances"] & @ CreateBalancesObject["1", <|"acc1" -> 1|>]];
];
