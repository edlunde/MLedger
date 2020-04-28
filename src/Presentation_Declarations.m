(* ::Package:: *)

(* ::Subsection:: *)
(*Presentation common*)


FormattedGrid::usage = "Formats a two dimensional table of numbers into a Grid. \
Numbers are rounded to nearest integer. Handles list of lists, association of list \
(keys added as row names, list of associations (keys added as column names), \
and association of associations (interpreted as having both row and column names).";


(* ::Subsection:: *)
(*Budget sheet*)


CreateBudgetSheet::usage = "CreateBudgetSheet[ledger, budget, budgetCategories] \
groups the expenses in ledger according to budgetCategories and compares with the \
given budget in a budget sheet.";


(* ::Subsection:: *)
(*Year balance sheet*)


CreateYearBalanceSheet::usage = "CreateYearBalanceSheet[ledger, incomingBalances] \
presents the balance for all bank accounts (taken from ListBankAccounts) together \
with their changes by month and counting from incomingBalances, given the entries in\[NonBreakingSpace]\
ledger. Uses GetAccountCategories[].";

SetAccountCategories::usage = "SetAccountCategories[<|\"cat1\" -> {__accounts}, \
\"cat2\" -> {__accounts}, ...|>] 
 sets the current account categories explicitly, used for CreateYearBalanceSheet.
SetAccountCategories[{checkingAccounts, savingsAccounts}] sets the current accounts to \
<|\"Checking Accounts\" -> checkingAccounts, \"Savings Accounts\" -> savingsAccounts|>.";
GetAccountCategories::usage = "GetAccountCategories[] returns the current account \
categories, used for CreateYearBalanceSheet.
If categories are not set, gives a warning and returns a split of ListBankAccounts[] \
with placeholder category names.";
