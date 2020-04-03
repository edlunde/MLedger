(* ::Package:: *)

(* ::Subsection:: *)
(*Journal*)


IsJournal::usage = "IsJournalEntry[obj_] returns True if obj is recognized \
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


(* ::Subsection:: *)
(*Journal file handling*)


GetJournalDir::usage = "GetJournalDir[] returns the directory used for journals.";
SetJournalDir::usage = "SetJournalDir[directory] sets the directory used for journals.";


ReadJournal::usage = "";

WriteToJournal::usage = "WriteToJournal[journal] adds the entries from the journal \
to the existing journal file in Journals/accountName/year.csv.";
