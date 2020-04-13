#!/bin/bash

# Merges all functions into a single Mathematica-package MLedger.m

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# We place the combined file one folder level up
targetFile=$(dirname "${scriptDir}")/MLedger.m

mathematicaVersion=$(/Applications/Mathematica.app/Contents/MacOS/MathematicaScript --version 2>&1)

# Array with name stems for source files in desired order
srcNames=("Common" "BankAccounts" "Journals" "Categorization" "Ledger" "Balances")


##### Glue everything together
# Overwrite with >
printf '(* ::Package:: *)\n\n' > "${targetFile}"

# Add text cell with version tested on
printf "(* ::Text:: *)\n(*Built and tested with ${mathematicaVersion}*)\n" >> "${targetFile}"

printf '\nBeginPackage["MLedger`"];\n' >> "${targetFile}"


printf '(* ::Chapter:: *)\n(*Declarations*)\n' >> "${targetFile}"
for f in ${srcNames[@]}; do
    printf "(* ::Section:: *)\n(*${f}*)\n" >> "${targetFile}"
    cat "${scriptDir}/${f}_Declarations.m" >> "${targetFile}"
done

printf '(* ::Chapter:: *)\n(*Implementations*)\n' >> "${targetFile}"
printf 'Begin["`Private`"];\n' >> "${targetFile}"
for f in ${srcNames[@]}; do
    printf "(* ::Section:: *)\n(*${f}*)\n" >> "${targetFile}"
    cat "${scriptDir}/${f}_Implementations.m" >> "${targetFile}"
done
printf '(* ::Section::Closed:: *)\n' >> "${targetFile}"
printf '(*Tail*)\n' >> "${targetFile}"
printf 'End[];\n' >> "${targetFile}"

printf '(* ::Chapter::Closed:: *)\n' >> "${targetFile}"
printf '(*Tail*)\n' >> "${targetFile}"
printf 'EndPackage[]\n' >> "${targetFile}"


# Cleaning up by closing all subsections and below
# Escaping * as it has special meaning in regexps
sed -i '' -e 's/(\* ::Subsection:: \*)/(\* ::Subsection::Closed:: \*)/g' "${targetFile}"
sed -i '' -e 's/(\* ::Subsubsection:: \*)/(\* ::Subsubsection::Closed:: \*)/g' "${targetFile}"
