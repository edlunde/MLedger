(* ::Package:: *)

getBankAccounts::usage = "getBankAccounts[] lists current bank accounts.";
setBankAccounts::usage = "setBankAccounts[accounts] sets current bank accounts.\
	Should not typically be used directly.";
	
addBankAccount::usage = "addBankAccount[name, currency, filePattern, importFunction] \
	adds a new account to the list under name with given currency. filePattern determines \
	files to be recognized as belonging to this account and importFunction is used to \
	parse the files."
