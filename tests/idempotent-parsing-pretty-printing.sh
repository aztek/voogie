#!/bin/bash

# Check that the parsing-pprinting loop is an idemponent function, meaning that
# `pploop . pploop = pploop`, where `pploop = pprint . parse`

# Voogie does not preserve comments and whitespace while parsing Boogie,
# therefore it does not guarantee that `pploop = id`

echo_success () {
  # echo -e "\\033[92m${1}\\033[0m"
  echo "${1}"
}

echo_error () {
  # echo -e "\\033[91m${1}\\033[0m"
  echo "${1}"
}

echo_bold () {
  # echo -e "\\033[1m${1}\\033[0m"
  echo "${1}"
}

VOOGIE="voogie"

OK=$(echo_success "OK")
FAIL=$(echo_error "FAIL")

EXAMPLES_DIR="$(pwd)/$(dirname "${BASH_SOURCE[@]}")/../examples"
for e in "${EXAMPLES_DIR}"/*;
do
  FILENAME=$(basename "${e}")
  echo -en "Checking $(echo_bold "${FILENAME}")... "

  if ! EXAMPLE=$(${VOOGIE} --action check "${e}" 2>&1)
  then
    echo "${FAIL}"
    echo_error "Unable to parse the file"
    echo "${EXAMPLE}"
    exit 1
  fi

  if ! EXAMPLE2=$(echo "${EXAMPLE}" | ${VOOGIE} --action check --stdin 2>&1)
  then
    echo "${FAIL}"
    echo_error "Unable to parse the output of voogie"
    echo "${EXAMPLE2}"
    exit 1
  fi

  if ! DIFF=$(diff <(echo "${EXAMPLE}") <(echo "${EXAMPLE2}"))
  then
    echo "${FAIL}"
    echo_error "Parsing and pretty-printing do not coincide"
    echo "${DIFF}"
    exit 1
  fi

  echo "${OK}"
done
