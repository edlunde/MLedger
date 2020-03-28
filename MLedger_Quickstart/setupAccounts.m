(* ::Package:: *)

setupBankAccounts[] := (
 SetBankAccounts[{}];
 AddBoAAccount["Example BoA account"];
 AddNordeaAccount["Example Nordea account"];
 ListBankAccounts[]
)
