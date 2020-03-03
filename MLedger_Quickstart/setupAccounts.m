(* ::Package:: *)

setupBankAccounts[] := (
 SetBankAccounts[{}];
 AddBankAccount["Example account USD", "USD", Null, Null];
 AddBankAccount["Example account SEK", "SEK", Null, Null];
 ListBankAccounts[]
)
