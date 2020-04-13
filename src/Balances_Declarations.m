(* ::Package:: *)

(* ::Subsection:: *)
(*Balances object*)


CreateBalancesObject::usage = "CreateBalancesObject[date, accountBalances] creates \
a balances object with the given date and accountBalances.

CreateBalancesObject[date, <|accountName -> balance, ...|>] creates a balances object \
with balances for given accounts.";

IsBalances::usage = "IsBalances[obj] returns True if obj is recognized as a balances \
object, false otherwise.";
IsAccountBalances::usage = "IsAccountBalances[obj] returns True if obj is a list \
of account balances, false otherwise.";
