(* ::Package:: *)

(* ::Subsection:: *)
(*Journal*)


IsJournal::usage = "IsJournal[obj_] returns True if obj is recognized \
as a Journal, False otherwise.";
CreateJournal::usage = "CreateJournal[listOfJournalEntries] creates a Journal.";


IsJournalEntry::usage = "IsJournalEntry[obj_] returns True if obj is recognized \
as a JournalEntry, False otherwise.";
CreateJournalEntry::usage = "CreateJournalEntry[date, description, amount, \
balance, account, currency, category] creates a journal entry from data.
CreateJournalEntry[..., extra] appends extra information. Has to be given as\
 key/value-pairs.";


SetCategories::usage = "SetCategories[journal, categories] takes a journal and a list \
of categories with equal length and sets the \"category\"-field of the journal's \
entries to the given categories.";


AddCalculatedBalances::usage = "";


(* ::Subsection:: *)
(*Journal file handling*)


GetJournalDir::usage = "GetJournalDir[] returns the directory used for journals.";
SetJournalDir::usage = "SetJournalDir[directory] sets the directory used for journals.";


ReadJournal::usage = "ReadJournal[account, year] reads the journal for given year \
and account.
ReadJournal[journal] reads the saved journal with account and year corresponding to\[NonBreakingSpace]\
those of the argument journal. If the latter is with mixed years/accounts, \
will instead give ReadJournal[___, False, ___]. 

ReadJournal[account] reads and merges all journals for all years for the given account.
ReadJournal[year] reads and merges all journals for all accounts for the given year.";

WriteToJournal::usage = "WriteToJournal[journal] splits the journal by account \
and year and adds the entries to the existing journal file in \
Journals/accountName/year.csv.";


ListAccountsWithJournals::usage = "ListAccountsWithJournals[] lists the names of \
accounts that have journal files. Gives a warning if it finds journals whose accounts \
are not found by GetAccounts[].";
