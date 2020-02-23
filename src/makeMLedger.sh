#!/bin/bash

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# We place the combined file one folder level up
targetFile=$(dirname "${scriptDir}")/MLedger.m

# The source files
declarationsFiles=${scriptDir}"/BankAccounts_Declarations.m"
implementationsFiles=${scriptDir}"/BankAccounts_Implementations.m"


# Glue everything together

printf 'BeginPackage["MLedger`"];\n' > "${targetFile}"


printf '(* ::Section:: *)\n(*Declarations*)\n' >> "${targetFile}"
cat "${declarationsFiles}" >> "${targetFile}"

printf '(* ::Section:: *)\n(*Implementations*)\n' >> "${targetFile}"
printf 'Begin["`Private`"];\n' >> "${targetFile}"
cat "${implementationsFiles}" >> "${targetFile}"
printf '(* ::Subsection::Closed:: *)\n' >> "${targetFile}"
printf '(*Tail*)\n' >> "${targetFile}"
printf 'End[];\n' >> "${targetFile}"

printf '(* ::Section::Closed:: *)\n' >> "${targetFile}"
printf '(*Tail*)\n' >> "${targetFile}"
printf 'EndPackage[]\n' >> "${targetFile}"


# Cleaning up by closing all subsections and below
# Escaping * as it has special meaning in regexps
sed -i '' -e 's/(\* ::Subsection:: \*)/(\* ::Subsection::Closed:: \*)/g' "${targetFile}"
sed -i '' -e 's/(\* ::Subsubsection:: \*)/(\* ::Subsubsection::Closed:: \*)/g' "${targetFile}"
