(* ::Package:: *)

(* ::Subsection:: *)
(*Balances objects*)


CreateBalancesObject::usage = "CreateBalancesObject[date, accountBalances] creates \
a balances object with the given date and accountBalances.

CreateBalancesObject[date, <|accountName -> balance, ...|>] creates a balances object \
with balances for given accounts. \

CreateBalancesObject[date, ledger, incomingBalances] creates a balances object for date \
under the assumption that ledger contains all events between the date of \
incomingBalances and date.";

IsBalances::usage = "IsBalances[obj] returns True if obj is recognized as a balances \
object, false otherwise.";
IsAccountBalances::usage = "IsAccountBalances[obj] returns True if obj is a list \
of account balances, false otherwise.";


(* ::Subsection:: *)
(*Balances input form*)


BalancesInputForm::usage = "form = BalancesInputForm[date] gives an input form \
for recording balances for a given date, prefilled with balances calculated from \
existing data. Use ExtractBalances[form] to get result when done filling out.";
ExtractBalances::usage = "ExtractBalances[form] returns a Balances object created \
from the given form. Expects a form from BalancesInputForm.";


(* ::Subsection:: *)
(*Balances file handling*)


GetBalancesDir::usage = "GetBalancesDir[] returns the directory used for balances.";
SetBalancesDir::usage = "SetBalancesDir[directory] sets the directory used for balances.";


ReadBalances::usage = "ReadBalances[date] reads the balance file for the given date \
(if it exists).";

WriteToBalances::usage = "WriteToBalances[balances] writes the given balances object \
to GetBalancesDir[]/date.csv";
