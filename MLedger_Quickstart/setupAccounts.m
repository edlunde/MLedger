(* ::Package:: *)

setupBankAccounts[] := (
 SetBankAccounts[{}];
 AddBankAccount["Example account USD", "USD", "stmt"~~___~~".txt", Null];
 AddBankAccount["Example account SEK", "SEK", "export"~~___~~".csv", Null];
 ListBankAccounts[]
)
