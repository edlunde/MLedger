(* ::Package:: *)

(* ::Subsection:: *)
(*Balances objects*)


CreateBalancesObject::usage = "CreateBalancesObject[date, accountBalances] creates \
a balances object with the given date and accountBalances.

CreateBalancesObject[date, <|accountName -> balance, ...|>] creates a balances object \
with balances for given accounts.";

IsBalances::usage = "IsBalances[obj] returns True if obj is recognized as a balances \
object, false otherwise.";
IsAccountBalances::usage = "IsAccountBalances[obj] returns True if obj is a list \
of account balances, false otherwise.";


(* ::Subsection:: *)
(*Balances file handling*)


GetBalancesDir::usage = "GetBalancesDir[] returns the directory used for balances.";
SetBalancesDir::usage = "SetBalancesDir[directory] sets the directory used for balances.";


ReadBalances::usage = "ReadBalances[date] reads the balance file for the given date.";

WriteToBalances::usage = "WriteToBalances[balances] writes the given balances object \
to GetBalancesDir[]/date.csv";
