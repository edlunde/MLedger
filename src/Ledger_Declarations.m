(* ::Package:: *)

(* ::Subsection:: *)
(*Ledger object*)


IsLedger::usage = "IsLedger[obj_] returns True if obj is recognized \
as a Ledger, False otherwise."
CreateLedger::usage = "CreateLedger[journal] creates a ledger from a journal.";


GetBalancesFromLedger::usage = "GetBalancesFromLedger[ledger] gives debit - credit \
for each account and category of expense in ledger.";


(* ::Subsection:: *)
(*Ledger file handling*)


GetLedgerDir::usage = "GetLedgerDir[] returns the directory used for ledgers.";
SetLedgerDir::usage = "SetLedgerDir[directory] sets the directory used for ledgers.";


ReadLedger::usage = "ReadLedger[year, month] reads the ledger for given year \
and month.";

WriteToLedger::usage = "WriteToLedger[ledger] splits the ledger by year \
and month and adds the entries to the existing ledger file in \
GetLedgerDir[] <> /year/month.csv.";
WriteLedgerFromJournalFiles::usage = "WriteLedgerFromJournalFiles[year] reads \
entries from all journals for the given year, creates corresponding ledger, and \
writes to file.";
