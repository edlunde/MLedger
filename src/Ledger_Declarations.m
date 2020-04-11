(* ::Package:: *)

(* ::Subsection:: *)
(*Ledger object*)


IsLedger::usage = "IsLedger[obj_] returns True if obj is recognized \
as a Ledger, False otherwise."
CreateLedger::usage = "CreateLedger[journal] creates a ledger from a journal.";


(* ::Subsection:: *)
(*Ledger file handling*)


GetLedgerDir::usage = "GetLedgerDir[] returns the directory used for ledgers.";
SetLedgerDir::usage = "SetLedgerDir[directory] sets the directory used for ledgers.";


ReadLedger::usage = "ReadLedger[year, month] reads the ledger for given year \
and month.";

WriteToLedger::usage = "WriteToLedger[ledger] splits the ledger by year \
and month and adds the entries to the existing ledger file in \
GetLedgerDir[] <> /year/month.csv.";
