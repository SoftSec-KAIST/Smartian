#!/bin/bash

function test_motiv() {
  ./run.sh 10 motiv || exit 1
  python check.py log/motiv.txt AssertionFailure
}

function test_constructor() {
  ./run.sh 10 constructor || exit 1
  python check.py log/constructor.txt IntegerBug
}

function test_AF() {
  ./run.sh 30 AF || exit 1
  python check.py log/AF.txt AssertionFailure
}

function test_AW() {
  ./run.sh 30 AW || exit 1
  python check.py log/AW.txt ArbitraryWrite
}

function test_BD_true1() {
  ./run.sh 10 BD_true1 || exit 1
  python check.py log/BD_true1.txt \
    BlockstateDependency,BlockstateDependencyILF,BlockstateDependencySFuzz,BlockstateDependencyMythril,BlockstateDependencyManticore
}

function test_BD_true2() {
  ./run.sh 180 BD_true2 || exit 1
  python check.py log/BD_true2.txt \
    BlockstateDependency,BlockstateDependencyILF,BlockstateDependencySFuzz,BlockstateDependencyMythril,BlockstateDependencyManticore
}

function test_BD_false() {
  ./run.sh 60 BD_false || exit 1
  python check.py log/BD_false.txt \
    -BlockstateDependency,BlockstateDependencyILF,BlockstateDependencySFuzz,BlockstateDependencyMythril,BlockstateDependencyManticore
}

function test_CH_true1() {
  ./run.sh 30 CH_true1 || exit 1
  python check.py log/CH_true1.txt ControlHijack
}

function test_CH_true2() {
  ./run.sh 30 CH_true2 || exit 1
  python check.py log/CH_true2.txt ControlHijack
}

function test_CH_false() {
  ./run.sh 30 CH_false || exit 1
  python check.py log/CH_false.txt -ControlHijack
}

function test_IB() {
  ./run.sh 30 IB || exit 1
  python check.py log/IB.txt \
  IntegerBug,IntegerBugSFuzz,IntegerBugMythril,IntegerBugManticore
}

function test_ME_true() {
  ./run.sh 30 ME_true || exit 1
  python check.py log/ME_true.txt \
    MishandledException,MishandledExceptionILF,MishandledExceptionSFuzz,MishandledExceptionMythril,MishandledExceptionManticore,EtherLeak
}

function test_MS_true() {
  ./run.sh 10 MS_true || exit 1
  python check.py log/MS_true.txt MultipleSend
}

function test_MS_false() {
  ./run.sh 10 MS_false || exit 1
  python check.py log/MS_false.txt -MultipleSend
}

function test_RE() {
  ./run.sh 120 RE || exit 1
  python check.py log/RE.txt \
    Reentrancy,ReentrancyILF,ReentrancySFuzz,ReentrancyMythril,ReentrancyManticore
}

function test_SC() {
  ./run.sh 10 SC || exit 1
  python check.py log/SC.txt SuicidalContract
}

function test_TO() {
  ./run.sh 10 TO || exit 1
  python check.py log/TO.txt TransactionOriginUse
}

if [ $# -ne 1 ]; then
    echo "Usage: $0 <target (all/motiv/AF/AW/...)>"
    exit 1
fi

case $1 in
  all)
    test_motiv
    test_constructor
    test_AF
    test_AW
    test_BD_true1
    test_BD_true2
    test_BD_false
    test_CH_true1
    test_CH_true2
    test_CH_false
    test_IB
    test_ME_true
    test_MS_true
    test_MS_false
    test_RE
    test_SC
    test_TO
    ;;
  *)
    test_$1
esac
